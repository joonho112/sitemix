# sm_vcov class -------------------------------------------------------------

#' Construct or inspect a within-site covariance object
#'
#' @encoding UTF-8
#'
#' @description
#' `sm_vcov()` is the constructor and validator for the
#' \code{sm_vcov} S3 class — the per-site-year within-indicator
#' covariance carrier that populates the optional \code{V}
#' list-column of a \code{sitemix_estimates} tibble. Every
#' \code{sitemix_estimates} row produced with \code{vjt = TRUE}
#' carries one \code{sm_vcov} object; this function is the
#' canonical home for the scale and method lexicons every downstream
#' consumer reads.
#'
#' @details
#' An \code{sm_vcov} object is a structured list carrying the
#' covariance matrix and structured metadata documenting how it was
#' constructed, on which scale, and over which site-year. The class
#' enforces PSD-ness up to a numerical tolerance and validates the
#' lexicon values listed below.
#'
#' \strong{Scale and method lexicons.} The \code{vcov_scale} and
#' \code{vcov_method} fields are the canonical home for sitemix's
#' covariance vocabulary; every other function that touches a
#' covariance matrix reads these fields rather than re-deriving the
#' lexicon. See the named sections below for the locked vocabulary.
#' `sm_vcov()` is the only exported constructor. Low-level construction and
#' validation helpers remain internal implementation details; the registered
#' `format()`, `print()`, and `as.matrix()` methods revalidate an object before
#' reading it so direct post-construction tampering fails with a classed
#' covariance error.
#' Matrix validity uses separate scale-aware tolerances for symmetry,
#' positive-semidefiniteness, the multinomial simplex identity, and numerical
#' rank. Each tolerance combines a matrix-scale-relative term with a
#' machine-range absolute floor; numerical rank does not reuse the more
#' permissive PSD-validity threshold. Multinomial rank remains the analytic
#' support rank described below.
#'
#' For the SUR derivation see
#' \code{vignette("m3-multivariate-sur-covariance")};
#' for the multinomial simplex covariance see
#' \code{vignette("m4-multinomial-simplex")};
#' for the D1 working-independence rationale see
#' \code{vignette("m5-aggregate-engines")}.
#'
#' @param matrix Symmetric PSD numeric matrix of dimension
#'   \code{K x K} with \eqn{K \ge 1}{K >= 1}. Rows and columns are
#'   indexed by \code{indicator_order}.
#' @param site_id Site identifier metadata; character or integer. Defaults to
#'   \code{NA_character_}.
#' @param year Integer scalar. Year identifier stored as metadata.
#'   Defaults to \code{NA_integer_}.
#' @param indicator_order Character row and column order for \code{matrix};
#'   defaults to \code{colnames(matrix)}.
#' @param family Character scalar. Estimation family that produced
#'   the matrix. One of \code{"binomial"}, \code{"multivariate"}, or
#'   \code{"multinomial"}. Required.
#' @param vcov_method Construction method; see \emph{vcov_method convention}.
#'   Character scalar; defaults to \code{NA_character_}, which is valid only
#'   for \code{family = "binomial"}. Multivariate and multinomial callers
#'   must supply a compatible recorded method.
#' @param estimate_scale Character scalar. The row-level
#'   \code{estimate_scale} of the producing \code{sitemix_estimates} tibble.
#'   One of \code{"none"}, \code{"arcsine"}, \code{"arcsine_anscombe"}, or
#'   \code{"logit"}; required.
#' @param vcov_scale Character scalar. The scale of \code{matrix}
#'   entries; one of \code{"raw"}, \code{"arcsine_delta"},
#'   \code{"logit_delta"}, or \code{"reference_raw"}. Required.
#'   \strong{May differ from estimate_scale} under Scenarios B and C;
#'   see \emph{vcov_scale convention} below.
#' @param matrix_boundary_rule Character scalar. Metadata recording
#'   how boundary cells were handled during matrix construction.
#'   Defaults to \code{"none"}.
#' @param scalar_correction_rule Character vector of length
#'   \code{length(indicator_order)}. Per-indicator scalar SE correction rules;
#'   supported values are listed under \emph{Scalar correction rules}. Defaults to
#'   \code{rep("none", length(indicator_order))}.
#' @param psd_repair Character scalar. Metadata recording the PSD
#'   repair (if any) applied during matrix construction. Defaults to
#'   \code{"none"}.
#' @param matrix_rank Integer scalar or \code{NULL} (default
#'   \code{NULL}). Rank metadata. For Scenario C this is the analytic
#'   simplex rank \code{positive_support - 1}, including when a census makes
#'   the realized sampling covariance exactly zero. For other families it is
#'   the numerical matrix rank. When \code{NULL}, the constructor computes the
#'   family-appropriate value.
#' @param positive_support Integer scalar. Number of positive
#'   categories for Scenario C multinomial output and the basis of its
#'   analytic rank; \code{NA_integer_} otherwise. Defaults to
#'   \code{NA_integer_}.
#' @param n_jt Integer scalar. Cell size metadata. Defaults to
#'   \code{NA_integer_}.
#' @param n_eff Numeric scalar. Effective sample size metadata.
#'   Defaults to \code{NA_real_}.
#' @param population_size Numeric scalar. Fixed site-year population size
#'   under SRSWOR, or \code{NA_real_} when no finite-population design was
#'   supplied.
#' @param sampling_fraction Sampling fraction \code{n / population_size};
#'   numeric scalar or aligned vector. Missing without an FPC design.
#' @param fpc_variance_multiplier Numeric scalar or coordinate-aligned vector.
#'   Conventional SRSWOR variance multiplier
#'   \code{(N - n) / (N - 1)}, with census value zero.
#' @param fpc_se_multiplier Numeric scalar or coordinate-aligned vector. Square
#'   root of the conventional FPC variance multiplier documented above.
#' @param variance_multiplier_applied Numeric scalar or coordinate-aligned
#'   vector recording the multiplier actually applied: the conventional FPC
#'   for plug-in variance or \code{(N - n) / N} for the design-corrected rule.
#' @param se_multiplier_applied Numeric scalar or coordinate-aligned vector.
#'   Square root of the applied variance multiplier documented above.
#' @param sampling_design Character scalar, one of \code{"not_specified"} or
#'   \code{"SRSWOR"}.
#' @param variance_rule Character scalar or aligned vector:
#'   \code{"plugin"} or \code{"design_corrected"}.
#' @param diag_contract Character scalar documenting which row companion the
#'   covariance diagonal matches. See \emph{Diagonal contract}.
#'
#' @return An S3 object of class \code{sm_vcov} with fields
#'   \code{matrix}, \code{site_id}, \code{year},
#'   \code{indicator_order}, \code{family}, \code{vcov_method},
#'   \code{estimate_scale}, \code{vcov_scale},
#'   \code{matrix_boundary_rule}, \code{scalar_correction_rule},
#'   \code{psd_repair}, \code{matrix_rank}, \code{positive_support},
#'   \code{n_jt}, \code{n_eff}, the finite-population provenance fields
#'   (\code{population_size}, \code{sampling_fraction},
#'   \code{fpc_variance_multiplier}, \code{fpc_se_multiplier},
#'   \code{variance_multiplier_applied}, \code{se_multiplier_applied},
#'   \code{sampling_design}, \code{variance_rule}), and \code{diag_contract}.
#'
#' @section Scalar correction rules:
#' \itemize{
#'   \item \code{"none"}: no scalar correction.
#'   \item \code{"binomial_bc"}: the binomial n-1 correction.
#'   \item \code{"wilson_boundary_surrogate"}: Wilson boundary uncertainty.
#'   \item \code{"agresti_coull_boundary_surrogate"}: Agresti--Coull boundary
#'     uncertainty.
#' }
#'
#' @section vcov_scale convention:
#' \code{vcov_scale} reports the scale on which the stored
#' covariance is computed; it \strong{may differ from a row's
#' estimate_scale} under Scenarios B and C, where the row-level
#' \code{theta_hat} is reported on the requested \code{vst} (e.g.,
#' arcsine) but the joint covariance is computed in raw-proportion
#' space for numerical stability. Always read \code{vcov_scale} from
#' this slot before consuming \code{matrix}.
#'
#' @section Scenario B whole-matrix contract:
#' Let \eqn{Q = \sum_i (y_i - \bar y)(y_i - \bar y)^\top} and
#' \eqn{q = (N-n)/(N-1)}. Scenario B stores the raw-scale plug-in matrix
#' \eqn{Q/n^2}; with a fixed SRSWOR population it stores \eqn{qQ/n^2}.
#' When \code{bias_correction = "binomial_bc"}, the entire matrix—not only
#' its diagonal—is replaced by \eqn{Q/[n(n-1)]}, or by
#' \eqn{(N-n)Q/[Nn(n-1)]} under SRSWOR. Uniform whole-matrix scaling
#' preserves correlations, symmetry, and positive semidefiniteness.
#' A constant indicator has zero off-diagonals. Wilson boundary handling is
#' an explicit positive scalar/diagonal surrogate; Agresti--Coull boundary
#' handling is not legal when a Scenario B matrix is requested. When an
#' \code{sm_vcov} is attached to package-produced Scenario B rows,
#' \code{validate.sitemix_estimates()} cross-checks the SUR method, raw matrix
#' scale, row-raw-SE diagonal contract, denominators, scalar rules, and boundary
#' rule against those rows; direct stand-alone \code{sm_vcov()} construction
#' remains available for explicitly user-defined covariance objects.
#'
#' @section Scenario C whole-matrix contract:
#' For \eqn{M = \mathrm{diag}(\hat\pi)-\hat\pi\hat\pi^\top}, Scenario C
#' stores \eqn{M/n} without FPC and \eqn{qM/n} under SRSWOR. With
#' \code{bias_correction = "binomial_bc"}, the whole matrix is
#' \eqn{M/(n-1)} without FPC and
#' \eqn{(N-n)M/[N(n-1)]} under SRSWOR. These rules preserve PSD and
#' \eqn{V\mathbf 1=0}. If \eqn{S} categories have positive observed count,
#' \code{matrix_rank} records the analytic simplex support rank \eqn{S-1};
#' this remains the metadata rank for a census even though its realized
#' sampling covariance is the zero matrix. The matrix-level
#' \code{variance_rule} and applied multiplier are uniform over all
#' coordinates. A zero-support coordinate keeps an exact zero matrix row and
#' column, so its scalar plug-in or Wilson provenance may intentionally differ
#' from the global design-corrected matrix rule. Package-output validation
#' permits that mismatch only at an exact structural zero and retains the
#' explicit Wilson boundary diagonal exception.
#'
#' @section Diagonal contract:
#' \code{diag_contract = "row_se_squared"} means the matrix is on the row
#' estimate scale and its diagonal equals \code{se^2}; this is used by A, D0,
#' and D1. \code{"row_se_raw_squared"} means a raw-scale B/C matrix agrees
#' with \code{se_raw^2}. For multinomial boundary surrogates,
#' the value formed by concatenating \code{"row_se_raw_squared_"} and
#' \code{"except_boundary_surrogates"} records that the
#' simplex-preserving matrix deliberately keeps a zero boundary diagonal while
#' the scalar Wilson surrogate remains positive. \code{"not_checked"} is
#' reserved for user-constructed objects without a row companion.
#'
#' @section vcov_method convention:
#' \describe{
#'   \item{\code{NA_character_}}{No recorded method (e.g.,
#'     user-constructed matrix).}
#'   \item{\code{"sur"}}{Scenario B; multivariate SUR-style
#'     covariance with off-diagonal \eqn{\sigma_{kk'}}{sigma_kk'}
#'     from joint proportions.}
#'   \item{\code{"multinomial"}}{Scenario C; simplex covariance
#'     \eqn{(\mathrm{diag}(\pi) - \pi \pi^\top) / n}{(diag(pi) - pi pi') / n}.}
#'   \item{\code{"working_independence"}}{Scenario D1;
#'     working-independence diagonal covariance when cross-marginal
#'     joints are unidentified.}
#' }
#'
#' @section var_method convention (row-level SE provenance):
#' Although \code{var_method} is a column of
#' \code{sitemix_estimates} rather than a field of \code{sm_vcov},
#' its locked lexicon is canonically defined here so all
#' SE-provenance documentation has a single home. The implemented
#' base values are grouped as follows:
#' \itemize{
#'   \item Arcsine values: \code{"arcsine_vst"} and
#'     \code{"arcsine_anscombe"}.
#'   \item Bias-corrected arcsine: \code{"arcsine_delta_binomial_bc"}.
#'   \item Logit values: \code{"logit_delta"} and
#'     \code{"logit_delta_binomial_bc"}.
#'   \item Raw-scale values: \code{"binomial"} and \code{"binomial_bc"}.
#'   \item Boundary values: \code{"wilson_boundary_surrogate"} and
#'     \code{"agresti_coull_boundary_surrogate"}.
#'   \item Suppression values: \code{"suppressed_drop"} and
#'     \code{"suppression_sensitivity"}.
#' }
#' Experimental GVF/log-variance smoothing records
#' \code{" + gvf_smooth_loglinear"} or
#' \code{" + gvf_smooth_gam"} in the alternative
#' \code{var_method_smoothed} column; an allowed overwrite may copy
#' that provenance to canonical \code{var_method}. Legacy
#' \code{" + fh_smooth_*"} labels remain readable. Derivations are documented
#' in:
#' \itemize{
#'   \item \code{vignette("m2-scalar-se-binomial")} for scalar binomial SEs.
#'   \item \code{vignette("m6-variance-smoothing-theory")} for smoothing.
#' }
#'
#' @references
#' Zellner, A. (1962). An efficient method of estimating seemingly
#' unrelated regressions and tests for aggregation bias.
#' \emph{Journal of the American Statistical Association},
#' \bold{57}(298), 348--368.
#' \doi{10.1080/01621459.1962.10480664}
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=sm_estimate]{sm_estimate()}} for the main dispatcher
#'     that produces \code{V} list-columns of \code{sm_vcov} objects.
#'   \item \code{\link[=sm_frechet_envelope]{sm_frechet_envelope()}} for
#'     Scenario-D1 pairwise intervals and projected stress.
#'   \item \code{\link[=sm_diagnose]{sm_diagnose()}} for the
#'     \code{level = "vcov"} diagnostic.
#'   \item \code{vignette("m3-multivariate-sur-covariance")}.
#'   \item \code{vignette("m4-multinomial-simplex")}.
#'   \item \code{vignette("m7-frechet-envelope-theory")}.
#' }
#'
#' @examples
#' \dontshow{set.seed(1L)}
#' data(alprek_subset, package = "sitemix")
#' est <- sm_estimate(
#'   subset(alprek_subset, year == 2024),
#'   family     = "multivariate",
#'   indicators = c("frpm", "snap"),
#'   vjt        = TRUE
#' )
#' v1 <- est$V[[1L]]
#' class(v1)
#' v1$vcov_scale
#' v1$vcov_method
#' dim(v1$matrix)
#'
#' @family covariance
#' @export
sm_vcov <- function(
  matrix,
  site_id = NA_character_,
  year = NA_integer_,
  indicator_order = colnames(matrix),
  family,
  vcov_method = NA_character_,
  estimate_scale,
  vcov_scale,
  matrix_boundary_rule = "none",
  scalar_correction_rule = rep("none", length(indicator_order)),
  psd_repair = "none",
  matrix_rank = NULL,
  positive_support = NA_integer_,
  n_jt = NA_integer_,
  n_eff = NA_real_,
  population_size = NA_real_,
  sampling_fraction = NA_real_,
  fpc_variance_multiplier = 1,
  fpc_se_multiplier = 1,
  variance_multiplier_applied = 1,
  se_multiplier_applied = 1,
  sampling_design = "not_specified",
  variance_rule = "plugin",
  diag_contract = "not_checked"
) {
  if (missing(family)) {
    .sm_abort_required_vcov_argument(
      "family",
      c("binomial", "multivariate", "multinomial")
    )
  }
  if (missing(estimate_scale)) {
    .sm_abort_required_vcov_argument(
      "estimate_scale",
      c("none", "arcsine", "arcsine_anscombe", "logit")
    )
  }
  if (missing(vcov_scale)) {
    .sm_abort_required_vcov_argument(
      "vcov_scale",
      c("raw", "arcsine_delta", "logit_delta", "reference_raw")
    )
  }

  x <- .sm_new_sm_vcov(
    matrix = matrix,
    site_id = site_id,
    year = year,
    indicator_order = indicator_order,
    family = family,
    vcov_method = vcov_method,
    estimate_scale = estimate_scale,
    vcov_scale = vcov_scale,
    matrix_boundary_rule = matrix_boundary_rule,
    scalar_correction_rule = scalar_correction_rule,
    psd_repair = psd_repair,
    matrix_rank = matrix_rank,
    positive_support = positive_support,
    n_jt = n_jt,
    n_eff = n_eff,
    population_size = population_size,
    sampling_fraction = sampling_fraction,
    fpc_variance_multiplier = fpc_variance_multiplier,
    fpc_se_multiplier = fpc_se_multiplier,
    variance_multiplier_applied = variance_multiplier_applied,
    se_multiplier_applied = se_multiplier_applied,
    sampling_design = sampling_design,
    variance_rule = variance_rule,
    diag_contract = diag_contract
  )
  .sm_validate_sm_vcov(x)
  x
}

.sm_new_sm_vcov <- function(
  matrix,
  site_id = NA_character_,
  year = NA_integer_,
  indicator_order,
  family,
  vcov_method = NA_character_,
  estimate_scale,
  vcov_scale,
  matrix_boundary_rule = "none",
  scalar_correction_rule,
  psd_repair = "none",
  matrix_rank = NULL,
  positive_support = NA_integer_,
  n_jt = NA_integer_,
  n_eff = NA_real_,
  population_size = NA_real_,
  sampling_fraction = NA_real_,
  fpc_variance_multiplier = 1,
  fpc_se_multiplier = 1,
  variance_multiplier_applied = 1,
  se_multiplier_applied = 1,
  sampling_design = "not_specified",
  variance_rule = "plugin",
  diag_contract = "not_checked"
) {
  if (!is.matrix(matrix) || !is.numeric(matrix)) {
    .sm_abort_vcov(
      "`matrix` must be a numeric matrix.",
      class = "sitemix_error_vcov_invariant",
      expected = "numeric matrix",
      actual = paste(class(matrix), collapse = "/"),
      fix = "Pass a square numeric covariance matrix."
    )
  }
  if (nrow(matrix) < 1L || ncol(matrix) < 1L) {
    .sm_abort_vcov(
      "`matrix` must have at least one covariance coordinate.",
      class = "sitemix_error_vcov_invariant",
      expected = "K x K numeric matrix with K >= 1",
      actual = paste(dim(matrix), collapse = " x "),
      fix = "Pass a non-empty square covariance matrix."
    )
  }
  if (!is.character(indicator_order) || !is.null(dim(indicator_order))) {
    .sm_abort_vcov(
      "`indicator_order` must be a character vector.",
      class = "sitemix_error_vcov_dimnames",
      expected = "character vector aligned to matrix coordinates",
      actual = paste(class(indicator_order), collapse = "/"),
      fix = "Pass indicator labels as an explicit character vector."
    )
  }

  year <- .sm_vcov_as_integerish(year, "year", allow_na = TRUE)
  positive_support <- .sm_vcov_as_integerish(
    positive_support,
    "positive_support",
    allow_na = TRUE
  )
  n_jt <- .sm_vcov_as_integerish(n_jt, "n_jt", allow_na = TRUE)
  if (!is.null(matrix_rank)) {
    matrix_rank <- .sm_vcov_as_integerish(matrix_rank, "matrix_rank")
  }

  if (is.null(dimnames(matrix)) || all(vapply(dimnames(matrix), is.null, logical(1)))) {
    if (length(indicator_order) == nrow(matrix) && nrow(matrix) == ncol(matrix)) {
      dimnames(matrix) <- list(indicator_order, indicator_order)
    }
  }

  if (is.null(matrix_rank)) {
    matrix_rank <- if (identical(family, "multinomial") &&
        is.integer(positive_support) && length(positive_support) == 1L &&
        !is.na(positive_support)) {
      as.integer(max(0L, positive_support - 1L))
    } else {
      .sm_matrix_rank(matrix)
    }
  }

  structure(
    list(
      matrix = matrix,
      site_id = as.character(site_id),
      year = year,
      indicator_order = indicator_order,
      family = family,
      vcov_method = vcov_method,
      estimate_scale = estimate_scale,
      vcov_scale = vcov_scale,
      matrix_boundary_rule = matrix_boundary_rule,
      scalar_correction_rule = as.character(scalar_correction_rule),
      psd_repair = psd_repair,
      matrix_rank = matrix_rank,
      positive_support = positive_support,
      n_jt = n_jt,
      n_eff = as.numeric(n_eff),
      population_size = as.numeric(population_size),
      sampling_fraction = as.numeric(sampling_fraction),
      fpc_variance_multiplier = as.numeric(fpc_variance_multiplier),
      fpc_se_multiplier = as.numeric(fpc_se_multiplier),
      variance_multiplier_applied = as.numeric(variance_multiplier_applied),
      se_multiplier_applied = as.numeric(se_multiplier_applied),
      sampling_design = as.character(sampling_design),
      variance_rule = as.character(variance_rule),
      diag_contract = as.character(diag_contract)
    ),
    class = "sm_vcov"
  )
}

# Internal constructor compatibility alias. It is intentionally not exported.
new_sm_vcov <- .sm_new_sm_vcov

.sm_validate_sm_vcov <- function(x) {
  if (!inherits(x, "sm_vcov")) {
    .sm_abort_vcov(
      "`x` must be an `sm_vcov` object.",
      class = "sitemix_error_vcov_invariant",
      expected = "sm_vcov",
      actual = paste(class(x), collapse = "/"),
      fix = "Construct covariance objects with `sm_vcov()`."
    )
  }

  mat <- x$matrix
  k <- .sm_validate_vcov_matrix(mat)
  .sm_validate_indicator_order(x$indicator_order, k)
  .sm_validate_vcov_dimnames(mat, x$indicator_order)
  .sm_validate_vcov_metadata(x, k)
  .sm_validate_vcov_symmetry(mat)
  .sm_validate_vcov_psd(mat)
  .sm_validate_vcov_rank(x)
  if (identical(x$family, "multinomial")) {
    .sm_validate_vcov_simplex(mat)
  }

  invisible(TRUE)
}

# Internal pseudo-S3 compatibility alias. It is neither exported nor registered
# as an S3 method; package code uses the dotted validator.
validate.sm_vcov <- .sm_validate_sm_vcov

#' @noRd
#' @export
format.sm_vcov <- function(x, ...) {
  .sm_validate_sm_vcov(x)
  method <- if (is.na(x$vcov_method)) "NA" else x$vcov_method
  paste0(
    "<sm_vcov[",
    nrow(x$matrix),
    "x",
    ncol(x$matrix),
    "] ",
    x$family,
    "/",
    method,
    " ",
    x$vcov_scale,
    " rank=",
    x$matrix_rank,
    ">"
  )
}

#' @noRd
#' @export
print.sm_vcov <- function(x, ...) {
  .sm_validate_sm_vcov(x)
  method <- if (is.na(x$vcov_method)) "NA" else x$vcov_method
  cat(
    "sm_vcov[",
    nrow(x$matrix),
    "x",
    ncol(x$matrix),
    "] site_id=",
    x$site_id,
    " year=",
    x$year,
    " family=",
    x$family,
    " vcov_method=",
    method,
    " vcov_scale=",
    x$vcov_scale,
    "\n",
    sep = ""
  )
  print(signif(x$matrix, 4))
  cat(
    "matrix_rank=",
    x$matrix_rank,
    " psd_repair=",
    x$psd_repair,
    " estimate_scale=",
    x$estimate_scale,
    " matrix_boundary_rule=",
    x$matrix_boundary_rule,
    " indicators=",
    paste(x$indicator_order, collapse = ","),
    "\n",
    sep = ""
  )
  invisible(x)
}

#' @noRd
#' @export
as.matrix.sm_vcov <- function(x, ...) {
  .sm_validate_sm_vcov(x)
  unclass(x$matrix)
}

.sm_abort_required_vcov_argument <- function(field, expected) {
  .sm_abort_vcov(
    paste0("`", field, "` is required."),
    class = "sitemix_error_vcov_invariant",
    expected = expected,
    actual = "missing",
    fix = paste0("Supply `", field, "` explicitly when constructing `sm_vcov`."),
    call = rlang::caller_env(2L)
  )
}

.sm_vcov_as_integerish <- function(x, field, allow_na = FALSE) {
  untyped_na <- allow_na && is.logical(x) && length(x) == 1L && is.na(x)
  numeric_scalar <- is.numeric(x) && length(x) == 1L
  numeric_na <- numeric_scalar && allow_na && is.na(x) && !is.nan(x)
  valid_value <- numeric_scalar && (
    numeric_na ||
      (!is.na(x) && is.finite(x) && x == floor(x) &&
        abs(x) <= .Machine$integer.max)
  )

  if (!untyped_na && !valid_value) {
    .sm_abort_vcov(
      paste0("`", field, "` must be a whole-number scalar before coercion."),
      class = "sitemix_error_vcov_invariant",
      expected = if (allow_na) {
        "whole-number numeric scalar within integer range, or NA"
      } else {
        "whole-number numeric scalar within integer range"
      },
      actual = if (length(x)) as.character(x) else paste0("<", typeof(x), "(0)>"),
      fix = paste0("Pass `", field, "` as an integer or whole-number numeric scalar.")
    )
  }

  as.integer(x)
}

.sm_validate_vcov_matrix <- function(mat) {
  if (!is.matrix(mat) || !is.numeric(mat)) {
    .sm_abort_vcov(
      "`matrix` must be a numeric matrix.",
      class = "sitemix_error_vcov_invariant",
      expected = "numeric matrix",
      actual = paste(class(mat), collapse = "/"),
      fix = "Pass a square numeric covariance matrix."
    )
  }
  if (length(dim(mat)) != 2L || nrow(mat) != ncol(mat)) {
    .sm_abort_vcov(
      "`matrix` must be square.",
      class = "sitemix_error_vcov_invariant",
      expected = "K x K matrix",
      actual = paste(dim(mat), collapse = " x "),
      fix = "Check covariance helper output before constructing `sm_vcov`."
    )
  }
  if (anyNA(mat) || any(!is.finite(mat))) {
    .sm_abort_vcov(
      "`matrix` must contain finite values.",
      class = "sitemix_error_vcov_invariant",
      expected = "finite numeric matrix",
      actual = "NA, NaN, or Inf present",
      fix = "Check covariance helper output before constructing `sm_vcov`."
    )
  }

  nrow(mat)
}

.sm_validate_indicator_order <- function(indicator_order, k) {
  if (!is.character(indicator_order) || length(indicator_order) != k || anyNA(indicator_order) || any(indicator_order == "")) {
    .sm_abort_vcov(
      "`indicator_order` must be a non-missing character vector of length K.",
      class = "sitemix_error_vcov_dimnames",
      expected = paste0("character vector of length ", k),
      actual = paste0("length ", length(indicator_order)),
      fix = "Pass indicator labels in the same order as matrix rows and columns."
    )
  }
  if (anyDuplicated(indicator_order)) {
    .sm_abort_vcov(
      "`indicator_order` must not contain duplicates.",
      class = "sitemix_error_vcov_dimnames",
      expected = "unique indicator labels",
      actual = paste(indicator_order[duplicated(indicator_order)], collapse = ", "),
      fix = "Use one covariance coordinate per indicator/category."
    )
  }

  invisible(TRUE)
}

.sm_validate_vcov_dimnames <- function(mat, indicator_order) {
  dn <- dimnames(mat)
  if (is.null(dn) || is.null(dn[[1]]) || is.null(dn[[2]])) {
    .sm_abort_vcov(
      "`matrix` must have row and column dimnames.",
      class = "sitemix_error_vcov_dimnames",
      expected = "rownames and colnames matching `indicator_order`",
      actual = "missing dimnames",
      fix = "Set dimnames before validating `sm_vcov`."
    )
  }
  if (!identical(rownames(mat), indicator_order) || !identical(colnames(mat), indicator_order)) {
    .sm_abort_vcov(
      "`matrix` dimnames must match `indicator_order`.",
      class = "sitemix_error_vcov_dimnames",
      expected = indicator_order,
      actual = unique(c(rownames(mat), colnames(mat))),
      fix = "Use the same indicator order in row metadata and covariance matrices."
    )
  }

  invisible(TRUE)
}

.sm_validate_vcov_metadata <- function(x, k) {
  .sm_vcov_scalar_chr(x$site_id, "site_id", allow_na = TRUE)
  .sm_vcov_scalar_int(x$year, "year", allow_na = TRUE)
  .sm_vcov_scalar_chr(x$family, "family", allowed = c("binomial", "multivariate", "multinomial"))
  .sm_vcov_scalar_chr(
    x$vcov_method,
    "vcov_method",
    allowed = c("sur", "multinomial", "working_independence"),
    allow_na = TRUE
  )
  .sm_vcov_scalar_chr(x$estimate_scale, "estimate_scale", allowed = c("none", "arcsine", "arcsine_anscombe", "logit"))
  .sm_vcov_scalar_chr(x$vcov_scale, "vcov_scale", allowed = c("raw", "arcsine_delta", "logit_delta", "reference_raw"))
  .sm_vcov_scalar_chr(x$matrix_boundary_rule, "matrix_boundary_rule", allowed = c("none", "diagonal_boundary_floor", "simplex_preserve"))
  .sm_vcov_scalar_chr(x$psd_repair, "psd_repair", allowed = c("none", "eigen_clip_tol"))
  .sm_vcov_scalar_int(x$matrix_rank, "matrix_rank")
  .sm_vcov_scalar_int(x$positive_support, "positive_support", allow_na = TRUE)
  .sm_vcov_scalar_int(x$n_jt, "n_jt", allow_na = TRUE, positive = TRUE)
  .sm_vcov_scalar_num(x$n_eff, "n_eff", allow_na = TRUE, positive = TRUE)
  .sm_validate_vcov_design_metadata(x, k)

  valid_scalar_rules <- c(
    "none",
    "wilson_boundary_surrogate",
    "agresti_coull_boundary_surrogate",
    "binomial_bc"
  )
  if (!is.character(x$scalar_correction_rule) || length(x$scalar_correction_rule) != k || anyNA(x$scalar_correction_rule) || any(!x$scalar_correction_rule %in% valid_scalar_rules)) {
    .sm_abort_vcov(
      "`scalar_correction_rule` must align to `indicator_order`.",
      class = "sitemix_error_vcov_invariant",
      expected = valid_scalar_rules,
      actual = unique(as.character(x$scalar_correction_rule)),
      fix = "Use one scalar correction label per covariance coordinate."
    )
  }

  if (identical(x$family, "binomial") && (!is.na(x$vcov_method))) {
    .sm_abort_vcov(
      "Scenario A `sm_vcov` objects must use `vcov_method = NA`.",
      class = "sitemix_error_vcov_invariant",
      expected = NA_character_,
      actual = x$vcov_method,
      fix = "Store matrix construction labels only for multivariate matrices."
    )
  }
  if (identical(x$family, "multinomial") && !identical(x$vcov_method, "multinomial")) {
    .sm_abort_vcov(
      "Multinomial `sm_vcov` objects require `vcov_method = \"multinomial\"`.",
      class = "sitemix_error_vcov_invariant",
      expected = "multinomial",
      actual = x$vcov_method,
      fix = "Use the multinomial covariance constructor for Scenario C."
    )
  }
  if (identical(x$family, "multivariate") && !x$vcov_method %in% c("sur", "working_independence")) {
    .sm_abort_vcov(
      "Multivariate `sm_vcov` objects require a multivariate covariance method.",
      class = "sitemix_error_vcov_invariant",
      expected = c("sur", "working_independence"),
      actual = x$vcov_method,
      fix = "Use `sur` for Scenario B or `working_independence` for D1 aggregate rows."
    )
  }

  invisible(TRUE)
}

.sm_validate_vcov_design_metadata <- function(x, k, tol = 1e-10) {
  .sm_vcov_scalar_num(x$population_size, "population_size", allow_na = TRUE, positive = TRUE)
  .sm_vcov_scalar_chr(
    x$sampling_design,
    "sampling_design",
    allowed = c("not_specified", "SRSWOR")
  )
  .sm_vcov_scalar_chr(
    x$diag_contract,
    "diag_contract",
    allowed = c(
      "not_checked",
      "row_se_squared",
      "row_se_raw_squared",
      "row_se_raw_squared_except_boundary_surrogates"
    )
  )
  .sm_vcov_num_vector(x$sampling_fraction, "sampling_fraction", k, allow_na = TRUE)
  .sm_vcov_num_vector(x$fpc_variance_multiplier, "fpc_variance_multiplier", k)
  .sm_vcov_num_vector(x$fpc_se_multiplier, "fpc_se_multiplier", k)
  .sm_vcov_num_vector(x$variance_multiplier_applied, "variance_multiplier_applied", k)
  .sm_vcov_num_vector(x$se_multiplier_applied, "se_multiplier_applied", k)
  .sm_vcov_chr_vector(
    x$variance_rule,
    "variance_rule",
    k,
    allowed = c("plugin", "design_corrected")
  )

  fraction <- rep_len(x$sampling_fraction, k)
  q <- rep_len(x$fpc_variance_multiplier, k)
  q_se <- rep_len(x$fpc_se_multiplier, k)
  applied <- rep_len(x$variance_multiplier_applied, k)
  applied_se <- rep_len(x$se_multiplier_applied, k)
  rule <- rep_len(x$variance_rule, k)

  if (identical(x$sampling_design, "not_specified")) {
    if (!is.na(x$population_size) || any(!is.na(fraction)) ||
        any(abs(q - 1) > tol) || any(abs(q_se - 1) > tol) ||
        any(abs(applied - 1) > tol) || any(abs(applied_se - 1) > tol)) {
      .sm_abort_vcov(
        "Unspecified sampling designs must use neutral finite-population metadata.",
        class = "sitemix_error_vcov_invariant",
        expected = "population/fraction missing and all multipliers equal to 1",
        actual = "non-neutral finite-population metadata",
        fix = "Use the covariance FPC metadata helper when constructing package objects."
      )
    }
    return(invisible(TRUE))
  }

  N <- x$population_size
  if (is.na(N) || N != floor(N) || anyNA(fraction) ||
      any(fraction <= 0 | fraction > 1)) {
    .sm_abort_vcov(
      "SRSWOR covariance metadata requires a whole population size and valid sampling fractions.",
      class = "sitemix_error_vcov_invariant",
      expected = "whole population_size and 0 < sampling_fraction <= 1",
      actual = c(population_size = N, sampling_fraction = fraction),
      fix = "Normalize the fixed population size by site-year before matrix construction."
    )
  }
  n_from_fraction <- fraction * N
  if (any(abs(n_from_fraction - round(n_from_fraction)) > tol * pmax(1, N))) {
    .sm_abort_vcov(
      "SRSWOR sampling fractions must imply whole-number sample sizes.",
      class = "sitemix_error_vcov_invariant",
      expected = "sampling_fraction * population_size is whole",
      actual = n_from_fraction,
      fix = "Record fractions from the observed integer denominators."
    )
  }
  n <- round(n_from_fraction)
  expected_q <- ifelse(N == n, 0, (N - n) / (N - 1))
  expected_applied <- expected_q
  corrected <- rule == "design_corrected"
  expected_applied[corrected] <- (N - n[corrected]) / N
  if (any(abs(q - expected_q) > tol) ||
      any(abs(q_se^2 - q) > tol) ||
      any(abs(applied - expected_applied) > tol) ||
      any(abs(applied_se^2 - applied) > tol)) {
    .sm_abort_vcov(
      "SRSWOR covariance multipliers are inconsistent with the recorded rule.",
      class = "sitemix_error_vcov_invariant",
      expected = "q for plug-in and (N-n)/N for design-corrected coordinates",
      actual = "inconsistent finite-population multiplier metadata",
      fix = "Construct covariance provenance with the covariance FPC metadata helper."
    )
  }
  invisible(TRUE)
}

.sm_vcov_scalar_chr <- function(x, field, allowed = NULL, allow_na = FALSE) {
  ok <- is.character(x) && length(x) == 1L
  if (!ok || (!allow_na && is.na(x)) || (!is.null(allowed) && !is.na(x) && !x %in% allowed)) {
    .sm_abort_vcov(
      paste0("`", field, "` has an invalid value."),
      class = "sitemix_error_vcov_invariant",
      expected = allowed %||% "single character value",
      actual = as.character(x),
      fix = "Use the locked `sm_vcov` metadata lexicon."
    )
  }
  invisible(TRUE)
}

.sm_vcov_scalar_int <- function(x, field, allow_na = FALSE, positive = FALSE) {
  ok <- is.integer(x) && length(x) == 1L
  if (!ok || (!allow_na && is.na(x)) || (!is.na(x) && positive && x <= 0L)) {
    .sm_abort_vcov(
      paste0("`", field, "` has an invalid value."),
      class = "sitemix_error_vcov_invariant",
      expected = if (positive) "positive integer scalar" else "integer scalar",
      actual = as.character(x),
      fix = "Use scalar metadata aligned to the site-year group."
    )
  }
  invisible(TRUE)
}

.sm_vcov_scalar_num <- function(x, field, allow_na = FALSE, positive = FALSE) {
  ok <- is.numeric(x) && length(x) == 1L
  if (!ok || (!allow_na && is.na(x)) || (!is.na(x) && (!is.finite(x) || (positive && x <= 0)))) {
    .sm_abort_vcov(
      paste0("`", field, "` has an invalid value."),
      class = "sitemix_error_vcov_invariant",
      expected = if (positive) "positive numeric scalar" else "numeric scalar",
      actual = as.character(x),
      fix = "Use scalar metadata aligned to the site-year group."
    )
  }
  invisible(TRUE)
}

.sm_vcov_num_vector <- function(x, field, k, allow_na = FALSE) {
  ok <- is.numeric(x) && length(x) %in% c(1L, k)
  if (!ok || (!allow_na && anyNA(x)) ||
      any(!is.na(x) & !is.finite(x)) ||
      any(!is.na(x) & x < 0)) {
    .sm_abort_vcov(
      paste0(field, " has invalid coordinate metadata."),
      class = "sitemix_error_vcov_invariant",
      expected = paste0("non-negative numeric length 1 or K = ", k),
      actual = as.character(x),
      fix = "Use scalar metadata or one value per covariance coordinate."
    )
  }
  invisible(TRUE)
}

.sm_vcov_chr_vector <- function(x, field, k, allowed) {
  ok <- is.character(x) && length(x) %in% c(1L, k) &&
    !anyNA(x) && all(x %in% allowed)
  if (!ok) {
    .sm_abort_vcov(
      paste0(field, " has invalid coordinate metadata."),
      class = "sitemix_error_vcov_invariant",
      expected = allowed,
      actual = as.character(x),
      fix = "Use one locked rule or one rule per covariance coordinate."
    )
  }
  invisible(TRUE)
}

.sm_validate_vcov_symmetry <- function(mat) {
  tol <- .sm_symmetry_tolerance(mat)
  asymmetry <- max(abs(mat - t(mat)))
  if (asymmetry > tol) {
    .sm_abort_vcov(
      "`matrix` must be symmetric.",
      class = "sitemix_error_vcov_invariant",
      expected = paste0("max asymmetry <= ", tol),
      actual = asymmetry,
      fix = "Symmetrize or repair covariance construction before `sm_vcov()`."
    )
  }
  invisible(TRUE)
}

.sm_validate_vcov_psd <- function(mat) {
  values <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
  tol <- .sm_psd_tolerance(mat)
  if (min(values) < -tol) {
    .sm_abort_vcov(
      "`matrix` must be positive semi-definite within tolerance.",
      class = "sitemix_error_vcov_invariant",
      expected = paste0("min eigenvalue >= ", signif(-tol, 4)),
      actual = signif(min(values), 6),
      fix = "Check covariance construction or record an allowed PSD repair."
    )
  }
  invisible(TRUE)
}

.sm_validate_vcov_rank <- function(x) {
  if (identical(x$family, "multinomial")) {
    k <- length(x$indicator_order)
    if (is.na(x$positive_support) || x$positive_support < 1L ||
        x$positive_support > k) {
      .sm_abort_vcov(
        "Multinomial `positive_support` must be between one and K.",
        class = "sitemix_error_vcov_invariant",
        expected = paste0("integer in [1, ", k, "]"),
        actual = x$positive_support,
        fix = "Record the number of categories with positive observed count."
      )
    }
    expected <- as.integer(x$positive_support - 1L)
    if (!identical(as.integer(x$matrix_rank), expected)) {
      .sm_abort_vcov(
        "Multinomial `matrix_rank` must equal `positive_support - 1`.",
        class = "sitemix_error_vcov_invariant",
        expected = expected,
        actual = x$matrix_rank,
        fix = "Use the analytic simplex support rank, including for a zero-variance census."
      )
    }
    return(invisible(TRUE))
  }

  rank <- .sm_matrix_rank(x$matrix)
  if (!identical(as.integer(rank), as.integer(x$matrix_rank))) {
    .sm_abort_vcov(
      "`matrix_rank` does not match the numerical matrix rank.",
      class = "sitemix_error_vcov_invariant",
      expected = rank,
      actual = x$matrix_rank,
      fix = "Let `sm_vcov()` compute `matrix_rank` or pass the matching value."
    )
  }
  invisible(TRUE)
}

.sm_validate_vcov_simplex <- function(mat) {
  tol <- .sm_simplex_tolerance(mat)
  residual <- as.vector(mat %*% rep(1, ncol(mat)))
  if (max(abs(residual)) > tol) {
    .sm_abort_vcov(
      "Multinomial covariance matrices must satisfy the simplex row-sum-zero identity.",
      class = "sitemix_error_vcov_invariant",
      expected = "V %*% 1 = 0",
      actual = paste(signif(residual, 6), collapse = ", "),
      fix = "Use the full-simplex multinomial covariance formula."
    )
  }
  invisible(TRUE)
}

.sm_matrix_rank <- function(mat) {
  if (!is.matrix(mat) || !is.numeric(mat) || nrow(mat) != ncol(mat) || anyNA(mat) || any(!is.finite(mat))) {
    return(NA_integer_)
  }
  if (max(abs(mat - t(mat))) > .sm_symmetry_tolerance(mat)) {
    return(NA_integer_)
  }

  values <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
  as.integer(sum(values > .sm_rank_tolerance(mat, values = values)))
}

.sm_psd_tolerance <- function(mat) {
  values <- eigen((mat + t(mat)) / 2, symmetric = TRUE, only.values = TRUE)$values
  .sm_scaled_matrix_tolerance(
    scale = max(abs(values)),
    dimension = nrow(mat),
    multiplier = 64
  )
}

.sm_rank_tolerance <- function(mat, values = NULL) {
  if (is.null(values)) {
    values <- eigen((mat + t(mat)) / 2, symmetric = TRUE, only.values = TRUE)$values
  }
  .sm_scaled_matrix_tolerance(
    scale = max(abs(values)),
    dimension = nrow(mat),
    multiplier = 8
  )
}

.sm_symmetry_tolerance <- function(mat) {
  .sm_scaled_matrix_tolerance(
    scale = norm(mat, type = "I"),
    dimension = nrow(mat),
    multiplier = 64
  )
}

.sm_simplex_tolerance <- function(mat) {
  .sm_scaled_matrix_tolerance(
    scale = norm(mat, type = "I"),
    dimension = nrow(mat),
    multiplier = 64
  )
}

.sm_scaled_matrix_tolerance <- function(scale, dimension, multiplier) {
  relative <- multiplier * max(1, dimension) * .Machine$double.eps * scale
  absolute <- multiplier * max(1, dimension) * .Machine$double.xmin
  relative + absolute
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
