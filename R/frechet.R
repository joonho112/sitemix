# Frechet-envelope diagnostics ---------------------------------------------

#' Compute D1 pairwise Fréchet intervals and projected stress scenarios
#'
#' @encoding UTF-8
#'
#' @description
#' `sm_frechet_envelope()` preserves the raw pairwise Fréchet intervals
#' for D1 marginal aggregate estimates as the formal result. It also assembles
#' the pairwise negative- and positive-dependence corners and, for more than
#' two indicators, PSD-projects those corners while preserving the raw-scale
#' plug-in diagonal. The projected matrices are stress scenarios, not
#' multivariate lower or upper bounds, and are not validated \code{sm_vcov}
#' objects. Projection can change a pair's sign, move it outside its raw
#' pairwise interval, or reverse the elementwise order of the two scenarios;
#' those events are reported explicitly in \code{projection_diagnostics}.
#'
#' @details
#' \strong{Two interpretations: D1a and D1b.} The function supports
#' two population regimes that the caller must explicitly choose
#' between:
#'
#' \describe{
#'   \item{\strong{D1a -- formal common-population}}{The published
#'     marginals refer to the same sampled units, use one finite denominator
#'     per site-year, and use the IID plug-in raw-probability variance rule
#'     without FPC or \code{binomial_bc}. These conditions must already be
#'     recorded by the D1 estimator. Under them, each raw pairwise covariance
#'     interval is formal. A matching denominator alone is not provenance.}
#'   \item{\strong{D1b -- subgroup-conditional heuristic}}{The
#'     published marginals are subgroup-conditional and the envelope
#'     is a \strong{heuristic stress test}, not a formal bound.
#'     Requires explicit acknowledgement via
#'     \code{subgroup_conditional_action = "allow"} (or proceeds
#'     with a warning under \code{"warn"}); \code{"error"} aborts
#'     the call.}
#' }
#'
#' \strong{PSD stress scenarios.} The assembled corner matrices are not
#' guaranteed to be PSD at the corners of the Fréchet box; the
#' function applies PSD projection via \code{psd_method = "higham"}
#' (Higham 2002 nearest-PSD algorithm via \code{Matrix::nearPD()})
#' or \code{"shrink"} (line search for the largest shrinkage weight
#' that yields a PSD matrix). The working-independence diagonal is
#' preserved through repair.
#' For \eqn{K \le 2}{K <= 2}, both projected fields are exact identities of
#' their source corners and no iterative method is attempted. For
#' \eqn{K > 2}{K > 2}, a fixed \code{shrink_alpha} is applied exactly even
#' when the source corner is already PSD; an infeasible fixed value errors.
#' Higham and automatic-shrink nonconvergence also error rather than returning
#' a partially repaired matrix.
#'
#' For the formal derivation see
#' \code{vignette("m7-frechet-envelope-theory", package = "sitemix")}.
#' Suppressed-missing and suppression-sensitivity rows are rejected at
#' entry: formal pairwise intervals require complete identified marginals.
#'
#' @param x A D1 aggregate-path \code{sitemix_estimates} object. Both
#'   \code{vjt = FALSE} and
#'   \code{vjt = TRUE} outputs are supported; formal D1a validation uses
#'   row and matrix provenance when each is available.
#' @param indicator Character vector or \code{NULL} (default
#'   \code{NULL}). Optional indicators to retain before envelope
#'   construction. \code{NULL} retains all indicators present in
#'   \code{x}.
#' @param population_regime Character scalar. Required regime label.
#'   One of \code{"d1a"} (formal common-population) or \code{"d1b"}
#'   (subgroup-conditional heuristic). No default; omission raises the stable
#'   required-regime condition documented under \emph{Errors}.
#' @param subgroup_conditional_action Character scalar. Action for
#'   \code{population_regime = "d1b"}. One of \code{"warn"} (default;
#'   proceed with a warning), \code{"allow"} (proceed silently), or
#'   \code{"error"} (abort). Ignored when
#'   \code{population_regime = "d1a"}.
#' @param return_correlations Logical scalar. If \code{TRUE}, return
#'   the raw pairwise Fréchet correlation endpoints as matrices in
#'   \code{pairwise_correlation_lower} and
#'   \code{pairwise_correlation_upper}. Defaults to \code{FALSE}; invalid
#'   values raise the stable condition documented under \emph{Errors}.
#' @param psd_method Character scalar. PSD repair method: \code{"higham"}
#'   (default; \code{Matrix::nearPD()}) or \code{"shrink"} (line search toward
#'   the working-independence diagonal).
#' @param psd_tol Positive numeric scalar. Relative PSD tolerance for the
#'   smallest eigenvalue and projection convergence. Eigenvalue checks scale
#'   this value by the matrix eigenvalue scale, with a machine-range absolute
#'   floor. Must not exceed \code{sqrt(.Machine$double.eps)} so that this
#'   numerical allowance cannot relabel materially indefinite matrices as PSD.
#'   Defaults to \code{1e-8}.
#' @param psd_max_iter Positive integer scalar. Maximum iterations
#'   for the PSD repair algorithm. Defaults to \code{100L}.
#' @param shrink_alpha Positive numeric scalar in \eqn{(0, 1]}{(0, 1]}
#'   or \code{NULL} (default \code{NULL}). Fixed shrinkage weight
#'   used exactly for \eqn{K > 2}{K > 2} when
#'   \code{psd_method = "shrink"}. \code{NULL} triggers a line search for
#'   the largest PSD-feasible weight. Ignored only by the exact
#'   \eqn{K \le 2}{K <= 2} identity policy.
#' @param ... Supported deterministic arguments forwarded to
#'   \code{Matrix::nearPD()} when \code{psd_method = "higham"}:
#'   \code{base.matrix}, \code{doSym}, \code{doDykstra},
#'   \code{posd.tol}, and \code{conv.norm.type}. Arguments that conflict with
#'   the package's diagonal, tolerance, iteration, symmetry, or output
#'   contract are rejected.
#'
#' @return An \code{sm_frechet_envelope} list object with the
#'   following fields:
#'   \describe{
#'     \item{\code{raw_pairwise_intervals}}{One row per site-year and
#'       indicator pair, with raw pairwise joint-probability, correlation,
#'       and covariance interval endpoints. The covariance scale is raw
#'       probability. The row-level \code{interval_scope} distinguishes
#'       formal D1a intervals from D1b heuristic ranges.}
#'     \item{\code{V_independence}}{List of per-site-year
#'       raw-scale covariance matrices. For formal D1a this is the IID
#'       plug-in diagonal, not the transformed row \code{V}.}
#'     \item{\code{unprojected_negative_dependence_corner},
#'       \code{unprojected_positive_dependence_corner}}{Lists of matrices
#'       assembled from the raw pairwise covariance endpoints before PSD
#'       projection. They are corners, not globally ordered covariance bounds.}
#'     \item{\code{projected_negative_dependence_stress},
#'       \code{projected_positive_dependence_stress}}{Lists of PSD-projected
#'       corner stress scenarios. They are not lower/upper bounds.}
#'     \item{\code{pairwise_correlation_lower},
#'       \code{pairwise_correlation_upper}}{Lists of raw pairwise correlation
#'       endpoint matrices when \code{return_correlations = TRUE}. Both slots
#'       are always present on the object; they are \code{NULL} otherwise.}
#'     \item{\code{projection_diagnostics}}{Canonical long diagnostics with
#'       one row per site-year and stress scenario. Rows record estimate and
#'       covariance scales; method, status, attempted/converged state and
#'       iterations; relative and realized absolute tolerances; before/after
#'       eigenvalue scales and minima; requested/applied shrinkage; raw and
#'       projected norms and distances; and diagonal, symmetry, PSD,
#'       sign-change, projected-order, and raw-interval invariants.}
#'     \item{\code{site_keys}}{Site-year identifier mapping for the
#'       list slots.}
#'     \item{\code{call}}{The function call as captured by
#'       \code{match.call()}.}
#'     \item{\code{population_regime}}{Character scalar; the regime
#'       label passed to \code{population_regime}.}
#'     \item{\code{frechet_scope}}{Character scalar; either formal raw
#'       pairwise intervals or a heuristic D1b stress range.}
#'     \item{\code{covariance_scale}, \code{projected_scenario_role}}{
#'       Canonical scale and role metadata.}
#'     \item{\code{psd_method}, \code{psd_tol}, \code{psd_max_iter},
#'       \code{shrink_alpha}, \code{projection_config}}{
#'       PSD repair settings actually applied.}
#'     \item{\code{V_lower_raw}, \code{V_upper_raw},
#'       \code{V_lower_psd}, \code{V_upper_psd}, \code{R_lower},
#'       \code{R_upper}}{Deprecated compatibility aliases for the canonical
#'       matrix fields above. New code should not use these names because
#'       projected matrices are not bounds.}
#'     \item{\code{psd_diagnostics}}{Deprecated one-row-per-site-year wide
#'       compatibility table. New code should consume the canonical long
#'       \code{projection_diagnostics}.}
#'   }
#'
#' @section Errors:
#' Omitting \code{population_regime} raises
#' \code{sitemix_error_population_regime_required}. Other invalid regime
#' values raise the corresponding stable invalid-regime condition.
#' Invalid \code{return_correlations} values raise
#' \code{sitemix_error_invalid_return_correlations}.
#'
#' @references
#' Fréchet, M. (1951). Sur les tableaux de corrélation dont les
#' marges sont données. \emph{Annales de l'Université de Lyon, 3e série,
#' Section A: Sciences Mathématiques et Astronomie}, \bold{14}, 53--77.
#'
#' Hoeffding, W. (1994). Scale-Invariant Correlation Theory. In
#' N. I. Fisher and P. K. Sen (Eds.), \emph{The Collected Works of
#' Wassily Hoeffding} (pp. 57--107). Springer.
#' doi:10.1007/978-1-4612-0865-5_4. (English reprint of the 1940 original.)
#'
#' Nelsen, R. B. (2006). \emph{An Introduction to Copulas} (2nd
#' ed.). Springer.
#'
#' Higham, N. J. (2002). Computing the nearest correlation matrix---a
#' problem from finance. \emph{IMA Journal of Numerical Analysis},
#' \bold{22}(3), 329--343. doi:10.1093/imanum/22.3.329.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}}
#'     for the D1 estimator producing the working-independence \code{V}.
#'   \item \code{\link[=sm_vcov]{sm_vcov()}} for the matrix class spec and
#'     \code{vcov_scale} convention.
#'   \item \code{vignette("a7-variance-smoothing-and-frechet")} for the
#'     applied walkthrough.
#'   \item \code{vignette("m7-frechet-envelope-theory")} for the formal
#'     derivation and D1a / D1b distinction.
#' }
#'
#' @examples
#' \dontshow{set.seed(1L)}
#' # D1 example using bundled counts as a synthetic aggregate slice
#' counts_path <- system.file(
#'   "extdata", "alprek_subset_counts.rds",
#'   package = "sitemix", mustWork = TRUE
#' )
#' counts <- readRDS(counts_path)
#'
#' # Build a tiny D1 input: two marginals (frpm, snap), one year
#' d1 <- counts[counts$year == 2024, ]
#' d1_long <- rbind(
#'   data.frame(
#'     site_id   = d1$site_id, year = d1$year,
#'     indicator = "frpm",
#'     c_jt      = d1$c_jt_frpm, n_jt = d1$n_jt
#'   ),
#'   data.frame(
#'     site_id   = d1$site_id, year = d1$year,
#'     indicator = "snap",
#'     c_jt      = d1$c_jt_snap, n_jt = d1$n_jt
#'   )
#' )
#'
#' est <- sm_estimate_from_aggregates(
#'   d1_long, family = "multivariate",
#'   indicator_col = "indicator",
#'   sampling_relation = "same_units",
#'   vjt = TRUE
#' )
#' env <- sm_frechet_envelope(est, population_regime = "d1a")
#' class(env)
#' env$psd_method
#'
#' @family covariance
#' @export
sm_frechet_envelope <- function(
  x,
  indicator = NULL,
  population_regime = NULL,
  subgroup_conditional_action = c("warn", "allow", "error"),
  return_correlations = FALSE,
  psd_method = c("higham", "shrink"),
  psd_tol = 1e-8,
  psd_max_iter = 100L,
  shrink_alpha = NULL,
  ...
) {
  .sm_validate_frechet_x(x)
  indicators <- .sm_resolve_frechet_indicators(x, indicator)
  population_regime <- .sm_validate_frechet_population_regime(population_regime)
  subgroup_conditional_action <- .sm_public_choice(
    subgroup_conditional_action,
    c("warn", "allow", "error"),
    "subgroup_conditional_action",
    "sitemix_error_invalid_population_regime"
  )
  return_correlations <- .sm_validate_frechet_return_correlations(return_correlations)
  psd_method <- .sm_public_choice(
    psd_method,
    c("higham", "shrink"),
    "psd_method",
    "sitemix_error_invalid_psd_method"
  )
  .sm_validate_frechet_psd_tol(psd_tol)
  .sm_validate_frechet_psd_max_iter(psd_max_iter)
  .sm_validate_frechet_shrink_alpha(shrink_alpha)
  if (!identical(psd_method, "shrink") && !is.null(shrink_alpha)) {
    .sm_abort_argument(
      "`shrink_alpha` is only meaningful when `psd_method = \"shrink\"`.",
      class = "sitemix_error_invalid_shrink_alpha",
      expected = "NULL for the Higham method",
      actual = as.character(shrink_alpha),
      fix = "Set `shrink_alpha = NULL` or use `psd_method = \"shrink\"`."
    )
  }
  .sm_validate_frechet_regime_provenance(
    x = x,
    population_regime = population_regime,
    indicators = indicators
  )
  .sm_handle_frechet_population_regime(
    population_regime = population_regime,
    subgroup_conditional_action = subgroup_conditional_action
  )

  work <- x[x$indicator %in% indicators, , drop = FALSE]
  work <- work[!.sm_is_suppressed_drop_row(work), , drop = FALSE]
  work <- work[is.finite(work$theta_raw) & is.finite(work$se_raw) & work$se_raw >= 0, , drop = FALSE]
  if (nrow(work) == 0L) {
    .sm_abort_argument(
      "`sm_frechet_envelope()` requires at least one retained finite D1 row.",
      class = "sitemix_error_invalid_indicators",
      expected = "finite D1 marginal rows",
      actual = "zero retained rows",
      fix = "Drop suppressed rows or select indicators with finite estimates."
    )
  }

  groups <- split(seq_len(nrow(work)), paste(work$site_id, work$year, sep = "\r"))
  site_keys <- vector("list", length(groups))
  site_names <- character(length(groups))
  site_results <- vector("list", length(groups))

  dots <- list(...)
  if (!identical(psd_method, "higham") && length(dots) > 0L) {
    .sm_abort_argument(
      "Additional `nearPD()` arguments are only available for the Higham method.",
      class = "sitemix_error_invalid_psd_method",
      expected = "no `...` arguments when `psd_method = \"shrink\"`",
      actual = names(dots) %||% "unnamed arguments",
      fix = "Remove `...` or use `psd_method = \"higham\"`."
    )
  }
  nearpd_settings <- .sm_frechet_sanitize_nearpd_args(
    dots,
    psd_tol = psd_tol,
    psd_max_iter = psd_max_iter
  )
  projection_config <- .sm_frechet_projection_config(
    psd_method = psd_method,
    psd_tol = psd_tol,
    psd_max_iter = psd_max_iter,
    shrink_alpha = shrink_alpha,
    nearpd_settings = nearpd_settings
  )
  for (i in seq_along(groups)) {
    idx <- groups[[i]]
    group <- work[idx, , drop = FALSE]
    if ("V" %in% names(group)) {
      .sm_warn_frechet_non_diagonal_v(group, psd_tol = psd_tol)
    }
    site_key <- paste(group$site_id[[1]], group$year[[1]], sep = "::")
    site_keys[[i]] <- data.frame(
      site_id = group$site_id[[1]],
      year = group$year[[1]],
      site_key = site_key,
      stringsAsFactors = FALSE
    )
    site_names[[i]] <- site_key
    stress_se <- group$se_raw
    if (identical(population_regime, "d1a")) {
      common_n <- unique(group$n)
      stress_se <- sqrt(group$theta_raw * (1 - group$theta_raw) / common_n)
    }
    site_results[[i]] <- .sm_frechet_from_vectors(
      p = group$theta_raw,
      s = stress_se,
      indicators = group$indicator,
      site_id = group$site_id[[1]],
      year = group$year[[1]],
      psd_method = psd_method,
      psd_tol = psd_tol,
      psd_max_iter = psd_max_iter,
      shrink_alpha = shrink_alpha,
      return_correlations = return_correlations,
      nearpd_args = list(),
      nearpd_settings = nearpd_settings,
      n_common = if (identical(population_regime, "d1a")) common_n else NA_real_
    )
  }

  names(site_results) <- site_names
  raw_pairwise_intervals <- vctrs::vec_rbind(
    !!!lapply(site_results, `[[`, "raw_pairwise_intervals")
  )
  raw_pairwise_intervals$interval_scope <- rep(
    if (identical(population_regime, "d1a")) {
      "formal_raw_pairwise_interval"
    } else {
      "heuristic_pairwise_stress_range"
    },
    nrow(raw_pairwise_intervals)
  )
  new_sm_frechet_envelope(
    V_independence = lapply(site_results, `[[`, "V_independence"),
    raw_pairwise_intervals = raw_pairwise_intervals,
    unprojected_negative_dependence_corner = lapply(site_results, `[[`, "unprojected_negative_dependence_corner"),
    unprojected_positive_dependence_corner = lapply(site_results, `[[`, "unprojected_positive_dependence_corner"),
    projected_negative_dependence_stress = lapply(site_results, `[[`, "projected_negative_dependence_stress"),
    projected_positive_dependence_stress = lapply(site_results, `[[`, "projected_positive_dependence_stress"),
    pairwise_correlation_lower = if (isTRUE(return_correlations)) lapply(site_results, `[[`, "pairwise_correlation_lower") else NULL,
    pairwise_correlation_upper = if (isTRUE(return_correlations)) lapply(site_results, `[[`, "pairwise_correlation_upper") else NULL,
    projection_diagnostics = vctrs::vec_rbind(!!!lapply(site_results, `[[`, "projection_diagnostics")),
    psd_diagnostics = vctrs::vec_rbind(!!!lapply(site_results, `[[`, "psd_diagnostics")),
    site_keys = vctrs::vec_rbind(!!!site_keys),
    call = match.call(),
    population_regime = population_regime,
    frechet_scope = if (identical(population_regime, "d1a")) "formal" else "heuristic_stress_test",
    covariance_scale = "raw",
    projected_scenario_role = "stress_scenario_not_bound",
    psd_method = psd_method,
    psd_tol = psd_tol,
    psd_max_iter = as.integer(psd_max_iter),
    shrink_alpha = shrink_alpha,
    projection_config = projection_config
  )
}

new_sm_frechet_envelope <- function(
  V_independence,
  raw_pairwise_intervals,
  unprojected_negative_dependence_corner,
  unprojected_positive_dependence_corner,
  projected_negative_dependence_stress,
  projected_positive_dependence_stress,
  pairwise_correlation_lower,
  pairwise_correlation_upper,
  projection_diagnostics,
  psd_diagnostics,
  site_keys,
  call,
  population_regime,
  frechet_scope,
  covariance_scale,
  projected_scenario_role,
  psd_method,
  psd_tol,
  psd_max_iter,
  shrink_alpha,
  projection_config
) {
  # The legacy names remain exact aliases during the v0.2 migration. They are
  # deliberately absent from print/summary because projected scenarios are not
  # lower or upper bounds.
  structure(
    list(
      V_independence = V_independence,
      raw_pairwise_intervals = raw_pairwise_intervals,
      unprojected_negative_dependence_corner = unprojected_negative_dependence_corner,
      unprojected_positive_dependence_corner = unprojected_positive_dependence_corner,
      projected_negative_dependence_stress = projected_negative_dependence_stress,
      projected_positive_dependence_stress = projected_positive_dependence_stress,
      pairwise_correlation_lower = pairwise_correlation_lower,
      pairwise_correlation_upper = pairwise_correlation_upper,
      projection_diagnostics = projection_diagnostics,
      V_lower_raw = unprojected_negative_dependence_corner,
      V_upper_raw = unprojected_positive_dependence_corner,
      V_lower_psd = projected_negative_dependence_stress,
      V_upper_psd = projected_positive_dependence_stress,
      R_lower = pairwise_correlation_lower,
      R_upper = pairwise_correlation_upper,
      psd_diagnostics = psd_diagnostics,
      site_keys = site_keys,
      call = call,
      population_regime = population_regime,
      frechet_scope = frechet_scope,
      covariance_scale = covariance_scale,
      projected_scenario_role = projected_scenario_role,
      psd_method = psd_method,
      psd_tol = psd_tol,
      psd_max_iter = psd_max_iter,
      shrink_alpha = shrink_alpha,
      projection_config = projection_config
    ),
    class = c("sm_frechet_envelope", "list")
  )
}

#' @noRd
#' @export
format.sm_frechet_envelope <- function(x, n = 6L, verbose = FALSE, ...) {
  .sm_validate_frechet_envelope_object(x)
  diagnostics <- summary(x)
  requested_n <- if (length(n) == 1L && is.finite(n)) as.integer(n) else nrow(diagnostics)
  n_show <- if (isTRUE(verbose)) nrow(diagnostics) else min(requested_n, nrow(diagnostics))
  n_show <- max(0L, n_show)
  shown <- if (n_show > 0L) diagnostics[seq_len(n_show), , drop = FALSE] else diagnostics[0L, , drop = FALSE]
  k_values <- unique(diagnostics$K)
  k_label <- if (length(k_values) == 1L) as.character(k_values) else paste(range(k_values), collapse = "-")
  scope_label <- if (identical(x$frechet_scope, "formal")) "formal" else "heuristic_stress_test"
  lines <- c(
    "Frechet pairwise intervals and projected stress scenarios - sm_frechet_envelope",
    paste0("  Sites:             ", length(unique(diagnostics$site_key))),
    paste0("  Indicators (K):    ", k_label),
    paste0("  Population regime: ", x$population_regime),
    paste0("  Frechet scope:     ", scope_label),
    paste0("  Covariance scale:  ", x$covariance_scale),
    paste0("  Projected role:    ", x$projected_scenario_role),
    paste0("  PSD method:        ", x$psd_method),
    paste0("  Tolerance:         ", x$psd_tol)
  )
  if (n_show > 0L) {
    rows <- vapply(seq_len(nrow(shown)), function(i) {
      paste0(
        "  ",
        shown$site_key[[i]],
        " | K=", shown$K[[i]],
        " | scenario=", shown$scenario[[i]],
        " | status=", shown$projection_status[[i]],
        " | raw norm=", signif(shown$raw_frobenius_norm[[i]], 4),
        " | projected norm=", signif(shown$projected_frobenius_norm[[i]], 4),
        " | relative distance=", signif(shown$projection_distance_relative[[i]], 4),
        " | order reversals=", shown$projected_order_reversals[[i]],
        " | interval violations=", shown$raw_interval_violations[[i]]
      )
    }, character(1))
    lines <- c(
      lines,
      paste0("  Scenario diagnostics: first ", n_show, " of ", nrow(diagnostics)),
      rows
    )
  }
  if (nrow(diagnostics) > 0L) {
    lines <- c(
      lines,
      paste0(
        "  Median relative projection distance (positive stress): ",
        signif(stats::median(
          diagnostics$projection_distance_relative[
            diagnostics$scenario == "positive_dependence_stress"
          ],
          na.rm = TRUE
        ), 4)
      ),
      paste0(
        "  Unprojected negative corner not PSD: ",
        sum(
          !diagnostics$raw_was_psd &
            diagnostics$scenario == "negative_dependence_stress"
        ),
        " site-years"
      ),
      paste0(
        "  Non-converged projections: ",
        sum(diagnostics$converged %in% FALSE),
        " scenario rows"
      ),
      paste0(
        "  Projected pair-order reversals: ",
        sum(
          diagnostics$projected_order_reversals[
            diagnostics$scenario == "negative_dependence_stress"
          ]
        ),
        " pairs"
      ),
      paste0(
        "  Projected raw-interval violations: ",
        sum(diagnostics$raw_interval_violations),
        " scenario-pairs"
      )
    )
  }
  lines
}

#' @noRd
#' @export
print.sm_frechet_envelope <- function(x, n = 6L, verbose = FALSE, ...) {
  cat(paste(format(x, n = n, verbose = verbose, ...), collapse = "\n"), "\n", sep = "")
  invisible(x)
}

#' @noRd
#' @export
summary.sm_frechet_envelope <- function(object, ...) {
  .sm_validate_frechet_envelope_object(object)
  out <- tibble::as_tibble(object$projection_diagnostics)
  out$population_regime <- object$population_regime
  out$frechet_scope <- object$frechet_scope
  out <- out[
    c(
      "site_key", "site_id", "year", "K", "scenario",
      "population_regime", "frechet_scope",
      "estimate_scale", "vcov_scale", "projection_method", "projection_status",
      "relative_tolerance", "absolute_tolerance_before", "absolute_tolerance_after",
      "eigen_scale_before", "eigen_scale_after",
      "min_eigen_before", "min_eigen_after", "raw_was_psd",
      "projection_attempted", "converged", "iterations", "max_iterations",
      "shrink_alpha_requested", "shrink_alpha_applied",
      "frobenius_independence", "raw_frobenius_norm", "projected_frobenius_norm",
      "projection_distance_absolute", "projection_distance_relative",
      "diagonal_max_abs_change", "diagonal_preserved",
      "symmetry_max_abs_residual", "symmetry_preserved", "psd_preserved",
      "sign_changes", "raw_interval_violations", "max_raw_interval_violation",
      "projected_order_reversals", "projected_order_reversal_max"
    )
  ]
  class(out) <- c("summary.sm_frechet_envelope", class(out))
  out
}

.sm_frechet_spread_pct <- function(reference, value) {
  ifelse(reference > 0, 100 * (value - reference) / reference, NA_real_)
}

.sm_validate_frechet_envelope_object <- function(x) {
  if (!inherits(x, "sm_frechet_envelope")) {
    .sm_abort_input(
      "`envelope` must be an `sm_frechet_envelope` object.",
      class = "sitemix_error_frechet_envelope_missing",
      expected = "sm_frechet_envelope",
      actual = paste(class(x), collapse = "/"),
      fix = "Create the object with `sm_frechet_envelope()`."
    )
  }
  required <- c(
    "V_independence", "raw_pairwise_intervals",
    "unprojected_negative_dependence_corner",
    "unprojected_positive_dependence_corner",
    "projected_negative_dependence_stress",
    "projected_positive_dependence_stress",
    "pairwise_correlation_lower", "pairwise_correlation_upper",
    "projection_diagnostics", "site_keys", "population_regime",
    "frechet_scope", "covariance_scale", "projected_scenario_role",
    "psd_method", "psd_tol", "psd_max_iter", "shrink_alpha",
    "projection_config", "V_lower_raw", "V_upper_raw",
    "V_lower_psd", "V_upper_psd", "R_lower", "R_upper", "psd_diagnostics"
  )
  missing <- setdiff(required, names(x))
  if (length(missing) > 0L) {
    .sm_abort_input(
      "`sm_frechet_envelope` object is missing required fields.",
      class = "sitemix_error_frechet_envelope_missing",
      expected = required,
      actual = names(x),
      fix = paste0("Missing: ", .sm_cli_collapse(missing, quote = TRUE), ".")
    )
  }
  .sm_validate_frechet_object_semantics(x)
  invisible(TRUE)
}

.sm_abort_frechet_object <- function(message, expected, actual, fix) {
  .sm_abort_input(
    message,
    class = "sitemix_error_frechet_envelope_missing",
    expected = expected,
    actual = actual,
    fix = fix
  )
}

.sm_validate_frechet_object_semantics <- function(x) {
  aliases <- list(
    V_lower_raw = "unprojected_negative_dependence_corner",
    V_upper_raw = "unprojected_positive_dependence_corner",
    V_lower_psd = "projected_negative_dependence_stress",
    V_upper_psd = "projected_positive_dependence_stress",
    R_lower = "pairwise_correlation_lower",
    R_upper = "pairwise_correlation_upper"
  )
  bad_alias <- names(aliases)[!vapply(names(aliases), function(alias) {
    identical(x[[alias]], x[[aliases[[alias]]]])
  }, logical(1))]
  if (length(bad_alias) > 0L) {
    .sm_abort_frechet_object(
      "Deprecated Frechet fields must remain exact aliases of canonical fields.",
      "alias identity",
      bad_alias,
      "Recreate the object with `sm_frechet_envelope()`; do not edit compatibility aliases independently."
    )
  }
  valid_regime_scope <- (
    identical(x$population_regime, "d1a") && identical(x$frechet_scope, "formal")
  ) || (
    identical(x$population_regime, "d1b") &&
      identical(x$frechet_scope, "heuristic_stress_test")
  )
  if (!valid_regime_scope ||
      !identical(x$covariance_scale, "raw") ||
      !identical(x$projected_scenario_role, "stress_scenario_not_bound")) {
    .sm_abort_frechet_object(
      "Frechet regime, scope, scale, or projected-scenario role metadata was altered.",
      "d1a/formal or d1b/heuristic_stress_test; raw scale; stress_scenario_not_bound role",
      paste(
        x$population_regime,
        x$frechet_scope,
        x$covariance_scale,
        x$projected_scenario_role,
        sep = " / "
      ),
      "Recreate the object from validated D1 estimates."
    )
  }
  .sm_validate_frechet_projection_config(x)

  if (!is.data.frame(x$site_keys) ||
      !all(c("site_id", "year", "site_key") %in% names(x$site_keys)) ||
      anyDuplicated(x$site_keys$site_key) ||
      any(x$site_keys$site_key != paste(x$site_keys$site_id, x$site_keys$year, sep = "::"))) {
    .sm_abort_frechet_object(
      "Frechet site-key metadata is invalid.",
      "unique site_id/year/site_key rows",
      names(x$site_keys),
      "Recreate the object from validated D1 estimates."
    )
  }
  keys <- x$site_keys$site_key
  matrix_fields <- c(
    "V_independence",
    "unprojected_negative_dependence_corner",
    "unprojected_positive_dependence_corner",
    "projected_negative_dependence_stress",
    "projected_positive_dependence_stress"
  )
  bad_list <- matrix_fields[!vapply(matrix_fields, function(field) {
    value <- x[[field]]
    is.list(value) && length(value) == length(keys) && identical(names(value), keys)
  }, logical(1))]
  if (length(bad_list) > 0L) {
    .sm_abort_frechet_object(
      "Frechet matrix lists are not aligned with site keys.",
      keys,
      bad_list,
      "Keep every canonical matrix list named and ordered by `site_keys$site_key`."
    )
  }
  expected_diagnostic_keys <- rep(keys, each = 2L)
  expected_scenarios <- rep(
    c("negative_dependence_stress", "positive_dependence_stress"),
    times = length(keys)
  )
  if (!is.data.frame(x$projection_diagnostics) ||
      !identical(x$projection_diagnostics$site_key, expected_diagnostic_keys) ||
      !identical(x$projection_diagnostics$scenario, expected_scenarios)) {
    .sm_abort_frechet_object(
      "Projection diagnostics are not aligned with site keys.",
      paste(expected_diagnostic_keys, expected_scenarios, sep = "/"),
      x$projection_diagnostics$site_key %||% "<missing>",
      "Recreate the object rather than reordering diagnostics."
    )
  }
  if (!is.data.frame(x$psd_diagnostics) ||
      !identical(x$psd_diagnostics$site_key, keys)) {
    .sm_abort_frechet_object(
      "Deprecated wide PSD diagnostics are not aligned with site keys.",
      keys,
      x$psd_diagnostics$site_key %||% "<missing>",
      "Recreate the object rather than editing compatibility diagnostics."
    )
  }

  intervals <- x$raw_pairwise_intervals
  required_intervals <- c(
    "site_id", "year", "site_key", "indicator_1", "indicator_2",
    "p_1", "p_2", "n_common", "joint_probability_lower",
    "joint_probability_upper", "pairwise_correlation_lower",
    "pairwise_correlation_upper", "pairwise_covariance_lower",
    "pairwise_covariance_upper", "interval_scale",
    "covariance_construction", "interval_scope"
  )
  if (!is.data.frame(intervals) || !all(required_intervals %in% names(intervals))) {
    .sm_abort_frechet_object(
      "Raw pairwise interval schema is incomplete.",
      required_intervals,
      names(intervals),
      "Recreate the object with the current `sm_frechet_envelope()`."
    )
  }
  expected_interval_rows <- sum(vapply(
    x$V_independence,
    function(mat) if (nrow(mat) < 2L) 0L else choose(nrow(mat), 2L),
    numeric(1)
  ))
  pair_keys <- keys[vapply(x$V_independence, nrow, integer(1)) >= 2L]
  if (!setequal(unique(intervals$site_key), pair_keys) ||
      nrow(intervals) != expected_interval_rows ||
      any(!intervals$site_key %in% pair_keys)) {
    .sm_abort_frechet_object(
      "Raw pairwise intervals contain a missing, duplicate, or unknown site-key scope.",
      paste0(expected_interval_rows, " rows over pair-bearing keys: ", paste(pair_keys, collapse = ", ")),
      paste0(nrow(intervals), " rows over: ", paste(unique(intervals$site_key), collapse = ", ")),
      "Recreate the object rather than adding or removing pairwise interval rows."
    )
  }

  for (i in seq_along(keys)) {
    key <- keys[[i]]
    matrices <- lapply(matrix_fields, function(field) x[[field]][[i]])
    names(matrices) <- matrix_fields
    base <- matrices$V_independence
    indicators <- rownames(base)
    K <- length(indicators)
    valid_matrix <- vapply(matrices, function(mat) {
      is.matrix(mat) && is.numeric(mat) && all(is.finite(mat)) &&
        identical(dim(mat), c(K, K)) &&
        identical(rownames(mat), indicators) &&
        identical(colnames(mat), indicators) &&
        isTRUE(all.equal(mat, t(mat), tolerance = 0))
    }, logical(1))
    if (!all(valid_matrix) || anyDuplicated(indicators) || anyNA(indicators)) {
      .sm_abort_frechet_object(
        "A Frechet matrix failed dimension, name, finiteness, or symmetry validation.",
        paste0(K, "x", K, " symmetric matrices with identical indicator names"),
        matrix_fields[!valid_matrix],
        paste0("Recreate site `", key, "` from validated D1 estimates.")
      )
    }
    scale <- max(abs(unlist(matrices, use.names = FALSE)))
    tol <- .sm_scaled_matrix_tolerance(scale, K, multiplier = 128)
    diagonal_ok <- vapply(matrices[-1L], function(mat) {
      max(abs(diag(mat) - diag(base))) <= tol
    }, logical(1))
    if (!all(diagonal_ok)) {
      .sm_abort_frechet_object(
        "A projected or unprojected Frechet matrix changed the canonical diagonal.",
        "all diagonals equal V_independence",
        matrix_fields[-1L][!diagonal_ok],
        paste0("Recreate site `", key, "`; projected scenarios preserve the diagonal.")
      )
    }
    if (K <= 2L && (
      max(abs(
        matrices$projected_negative_dependence_stress -
          matrices$unprojected_negative_dependence_corner
      )) > tol ||
      max(abs(
        matrices$projected_positive_dependence_stress -
          matrices$unprojected_positive_dependence_corner
      )) > tol
    )) {
      .sm_abort_frechet_object(
        "K <= 2 projected stress matrices must equal their unprojected corners.",
        "no projection change for K <= 2",
        key,
        "Recreate the object; pairwise Frechet corners are already PSD."
      )
    }
    if (!.sm_frechet_is_psd(matrices$projected_negative_dependence_stress, x$psd_tol) ||
        !.sm_frechet_is_psd(matrices$projected_positive_dependence_stress, x$psd_tol)) {
      .sm_abort_frechet_object(
        "A projected Frechet stress scenario is not PSD.",
        "both projected stress matrices satisfy the stored relative PSD tolerance",
        key,
        "Recreate the object; do not replace projected stress matrices after construction."
      )
    }

    site_intervals <- intervals[intervals$site_key == key, , drop = FALSE]
    expected_pairs <- if (K < 2L) 0L else choose(K, 2L)
    actual_pair_labels <- paste(site_intervals$indicator_1, site_intervals$indicator_2, sep = "\r")
    expected_pair_labels <- if (K < 2L) {
      character()
    } else {
      pair_index <- utils::combn(seq_len(K), 2L)
      paste(indicators[pair_index[1L, ]], indicators[pair_index[2L, ]], sep = "\r")
    }
    interval_order_ok <- all(is.finite(site_intervals$p_1)) &&
      all(is.finite(site_intervals$p_2)) &&
      all(site_intervals$p_1 >= 0 & site_intervals$p_1 <= 1) &&
      all(site_intervals$p_2 >= 0 & site_intervals$p_2 <= 1) &&
      all(site_intervals$joint_probability_lower <= site_intervals$joint_probability_upper) &&
      all(site_intervals$pairwise_covariance_lower <= site_intervals$pairwise_covariance_upper) &&
      all(is.na(site_intervals$pairwise_correlation_lower) == is.na(site_intervals$pairwise_correlation_upper)) &&
      all(
        is.na(site_intervals$pairwise_correlation_lower) |
          site_intervals$pairwise_correlation_lower <= site_intervals$pairwise_correlation_upper
      )
    if (nrow(site_intervals) != expected_pairs ||
        !identical(actual_pair_labels, expected_pair_labels) || !interval_order_ok ||
        any(site_intervals$interval_scale != "raw_probability") ||
        any(as.character(site_intervals$site_id) != as.character(x$site_keys$site_id[[i]])) ||
        any(as.integer(site_intervals$year) != as.integer(x$site_keys$year[[i]]))) {
      .sm_abort_frechet_object(
        "Raw pairwise intervals failed pair alignment, ordering, or scale validation.",
        paste0(expected_pairs, " ordered raw-probability pairs for ", key),
        paste0(nrow(site_intervals), " pair rows"),
        "Recreate the object rather than editing raw pairwise intervals."
      )
    }
    if (expected_pairs > 0L) {
      marginal_map <- data.frame(
        indicator = c(site_intervals$indicator_1, site_intervals$indicator_2),
        p = c(site_intervals$p_1, site_intervals$p_2),
        stringsAsFactors = FALSE
      )
      marginal_consistent <- setequal(unique(marginal_map$indicator), indicators) &&
        all(vapply(split(marginal_map$p, marginal_map$indicator), function(value) {
          all(is.finite(value)) && max(value) - min(value) <= tol
        }, logical(1)))
      formal_common_n <- !identical(x$population_regime, "d1a") ||
        (
          all(is.finite(site_intervals$n_common)) &&
            length(unique(site_intervals$n_common)) == 1L
        )
      if (!marginal_consistent || !formal_common_n) {
        .sm_abort_frechet_object(
          "Pairwise rows do not share one internally consistent marginal map and formal denominator.",
          "one p per indicator and, for D1a, one n_common per site-year",
          key,
          "Recreate the object; pair-specific edits cannot redefine site-level D1a provenance."
        )
      }
      if (identical(x$population_regime, "d1a")) {
        n_common <- unique(site_intervals$n_common)
        p_by_indicator <- vapply(
          split(marginal_map$p, marginal_map$indicator),
          function(value) value[[1L]],
          numeric(1)
        )
        diagonal_oracle <- p_by_indicator[indicators] *
          (1 - p_by_indicator[indicators]) / n_common
        if (max(abs(diag(base) - diagonal_oracle)) > tol) {
          .sm_abort_frechet_object(
            "Formal D1a independence diagonal does not match its marginal `/n` oracle.",
            "diag = p * (1 - p) / n_common",
            key,
            "Recreate the object from the original same-unit D1 estimates."
          )
        }
      }
      negative <- matrices$unprojected_negative_dependence_corner
      positive <- matrices$unprojected_positive_dependence_corner
      for (j in seq_len(expected_pairs)) {
        a <- site_intervals$indicator_1[[j]]
        b <- site_intervals$indicator_2[[j]]
        p_1 <- site_intervals$p_1[[j]]
        p_2 <- site_intervals$p_2[[j]]
        q_lower <- max(0, p_1 + p_2 - 1)
        q_upper <- min(p_1, p_2)
        correlation_denom <- sqrt(p_1 * (1 - p_1) * p_2 * (1 - p_2))
        correlation_lower <- if (correlation_denom < .Machine$double.eps) {
          NA_real_
        } else {
          (q_lower - p_1 * p_2) / correlation_denom
        }
        correlation_upper <- if (correlation_denom < .Machine$double.eps) {
          NA_real_
        } else {
          (q_upper - p_1 * p_2) / correlation_denom
        }
        oracle_ok <- abs(site_intervals$joint_probability_lower[[j]] - q_lower) <= tol &&
          abs(site_intervals$joint_probability_upper[[j]] - q_upper) <= tol &&
          isTRUE(all.equal(
            site_intervals$pairwise_correlation_lower[[j]],
            correlation_lower,
            tolerance = tol
          )) &&
          isTRUE(all.equal(
            site_intervals$pairwise_correlation_upper[[j]],
            correlation_upper,
            tolerance = tol
          ))
        if (identical(x$population_regime, "d1a")) {
          n_common <- site_intervals$n_common[[j]]
          oracle_ok <- oracle_ok && is.finite(n_common) && n_common > 0 &&
            identical(site_intervals$covariance_construction[[j]], "formal_iid_pairwise_covariance_over_n") &&
            identical(site_intervals$interval_scope[[j]], "formal_raw_pairwise_interval") &&
            abs(site_intervals$pairwise_covariance_lower[[j]] -
                  (q_lower - p_1 * p_2) / n_common) <= tol &&
            abs(site_intervals$pairwise_covariance_upper[[j]] -
                  (q_upper - p_1 * p_2) / n_common) <= tol
        } else {
          heuristic_covariance_lower <- if (is.na(correlation_lower)) {
            0
          } else {
            correlation_lower * sqrt(base[a, a]) * sqrt(base[b, b])
          }
          heuristic_covariance_upper <- if (is.na(correlation_upper)) {
            0
          } else {
            correlation_upper * sqrt(base[a, a]) * sqrt(base[b, b])
          }
          oracle_ok <- oracle_ok && is.na(site_intervals$n_common[[j]]) &&
            identical(site_intervals$covariance_construction[[j]], "heuristic_se_scaled_pairwise_correlation") &&
            identical(site_intervals$interval_scope[[j]], "heuristic_pairwise_stress_range") &&
            abs(site_intervals$pairwise_covariance_lower[[j]] - heuristic_covariance_lower) <= tol &&
            abs(site_intervals$pairwise_covariance_upper[[j]] - heuristic_covariance_upper) <= tol
        }
        if (!oracle_ok) {
          .sm_abort_frechet_object(
            "A raw pairwise interval no longer matches its Frechet oracle or scope.",
            "q endpoints, correlations, and formal `/n` covariance reconstruct exactly",
            paste(a, b, sep = " / "),
            paste0("Recreate site `", key, "` from the original D1 estimates.")
          )
        }
        if (abs(negative[a, b] - site_intervals$pairwise_covariance_lower[[j]]) > tol ||
            abs(positive[a, b] - site_intervals$pairwise_covariance_upper[[j]]) > tol) {
          .sm_abort_frechet_object(
            "Raw pairwise interval endpoints do not match the unprojected corners.",
            "corner off-diagonals equal pairwise covariance endpoints",
            paste(a, b, sep = " / "),
            paste0("Recreate site `", key, "` from the original D1 estimates.")
          )
        }
      }
    }

    correlations_present <- !is.null(x$pairwise_correlation_lower) ||
      !is.null(x$pairwise_correlation_upper)
    if (correlations_present) {
      if (is.null(x$pairwise_correlation_lower) || is.null(x$pairwise_correlation_upper) ||
          length(x$pairwise_correlation_lower) != length(keys) ||
          length(x$pairwise_correlation_upper) != length(keys)) {
        .sm_abort_frechet_object(
          "Pairwise correlation endpoint matrices are incomplete.",
          "both correlation lists or neither",
          key,
          "Recreate the object with a consistent `return_correlations` setting."
        )
      }
      lower_correlation <- x$pairwise_correlation_lower[[i]]
      upper_correlation <- x$pairwise_correlation_upper[[i]]
      if (!identical(dimnames(lower_correlation), dimnames(base)) ||
          !identical(dimnames(upper_correlation), dimnames(base))) {
        .sm_abort_frechet_object(
          "Pairwise correlation endpoints are not indicator-aligned.",
          indicators,
          key,
          "Recreate the object rather than editing correlation matrices."
        )
      }
      for (j in seq_len(expected_pairs)) {
        a <- site_intervals$indicator_1[[j]]
        b <- site_intervals$indicator_2[[j]]
        if (!isTRUE(all.equal(lower_correlation[a, b], site_intervals$pairwise_correlation_lower[[j]], tolerance = tol)) ||
            !isTRUE(all.equal(upper_correlation[a, b], site_intervals$pairwise_correlation_upper[[j]], tolerance = tol))) {
          .sm_abort_frechet_object(
            "Pairwise correlation matrices do not match raw interval rows.",
            "correlation matrix endpoints equal raw pairwise interval endpoints",
            paste(a, b, sep = " / "),
            paste0("Recreate site `", key, "` from the original D1 estimates.")
          )
        }
      }
    }

    recomputed <- .sm_frechet_projection_semantics(
      unprojected_negative = matrices$unprojected_negative_dependence_corner,
      unprojected_positive = matrices$unprojected_positive_dependence_corner,
      projected_negative = matrices$projected_negative_dependence_stress,
      projected_positive = matrices$projected_positive_dependence_stress
    )
    site_key_row <- x$site_keys[i, , drop = FALSE]
    replay_negative <- if (K <= 2L) {
      .sm_frechet_identity_projection(
        matrices$unprojected_negative_dependence_corner,
        method = x$psd_method,
        psd_tol = x$psd_tol
      )
    } else {
      .sm_frechet_psd_project(
        matrices$unprojected_negative_dependence_corner,
        method = x$psd_method,
        psd_tol = x$psd_tol,
        psd_max_iter = x$psd_max_iter,
        shrink_alpha = x$shrink_alpha,
        nearpd_settings = x$projection_config$nearpd_settings
      )
    }
    replay_positive <- if (K <= 2L) {
      .sm_frechet_identity_projection(
        matrices$unprojected_positive_dependence_corner,
        method = x$psd_method,
        psd_tol = x$psd_tol
      )
    } else {
      .sm_frechet_psd_project(
        matrices$unprojected_positive_dependence_corner,
        method = x$psd_method,
        psd_tol = x$psd_tol,
        psd_max_iter = x$psd_max_iter,
        shrink_alpha = x$shrink_alpha,
        nearpd_settings = x$projection_config$nearpd_settings
      )
    }
    replay_ok <- max(abs(
      replay_negative$mat - matrices$projected_negative_dependence_stress
    )) <= tol && max(abs(
      replay_positive$mat - matrices$projected_positive_dependence_stress
    )) <= tol

    expected_long <- .sm_frechet_projection_diagnostics(
      site_id = site_key_row$site_id[[1L]],
      year = site_key_row$year[[1L]],
      V_independence = matrices$V_independence,
      unprojected_negative = matrices$unprojected_negative_dependence_corner,
      unprojected_positive = matrices$unprojected_positive_dependence_corner,
      negative_projection = replay_negative,
      positive_projection = replay_positive,
      semantic_diagnostics = recomputed,
      psd_method = x$psd_method,
      psd_tol = x$psd_tol,
      psd_max_iter = x$psd_max_iter,
      shrink_alpha = x$shrink_alpha
    )
    actual_long <- x$projection_diagnostics[
      x$projection_diagnostics$site_key == key,
      ,
      drop = FALSE
    ]
    expected_wide <- .sm_frechet_legacy_psd_diagnostics(
      long = expected_long,
      semantic_diagnostics = recomputed,
      boundary_marginal_indicators = if (expected_pairs > 0L) {
        p_by_indicator <- vapply(
          split(marginal_map$p, marginal_map$indicator),
          function(value) value[[1L]],
          numeric(1)
        )
        indicators[p_by_indicator[indicators] == 0 | p_by_indicator[indicators] == 1]
      } else {
        x$psd_diagnostics$boundary_marginal_indicators[[i]]
      }
    )
    actual_wide <- x$psd_diagnostics[i, , drop = FALSE]
    diagnostics_ok <- .sm_frechet_diagnostics_equal(actual_long, expected_long, tol) &&
      .sm_frechet_diagnostics_equal(actual_wide, expected_wide, tol)
    if (!replay_ok || !diagnostics_ok) {
      .sm_abort_frechet_object(
        "Projected matrices or projection diagnostics do not match deterministic replay.",
        "K <= 2 identity or K > 2 replay from raw corners and stored projection settings",
        key,
        "Recreate the object; do not edit projected matrices or diagnostics independently."
      )
    }
  }
  invisible(TRUE)
}

.sm_frechet_from_vectors <- function(
  p,
  s,
  indicators,
  site_id = NA_character_,
  year = NA_integer_,
  psd_method = "higham",
  psd_tol = 1e-8,
  psd_max_iter = 100L,
  shrink_alpha = NULL,
  return_correlations = FALSE,
  nearpd_args = list(),
  nearpd_settings = NULL,
  n_common = NA_real_
) {
  p <- as.numeric(p)
  s <- as.numeric(s)
  indicators <- as.character(indicators)
  .sm_validate_frechet_vector_inputs(p, s, indicators)
  .sm_validate_frechet_psd_tol(psd_tol)
  .sm_validate_frechet_psd_max_iter(psd_max_iter)
  .sm_validate_frechet_shrink_alpha(shrink_alpha)
  if (is.null(nearpd_settings)) {
    nearpd_settings <- .sm_frechet_sanitize_nearpd_args(
      nearpd_args,
      psd_tol = psd_tol,
      psd_max_iter = psd_max_iter
    )
  }
  K <- length(indicators)

  R_lower <- diag(1, K)
  R_upper <- diag(1, K)
  dimnames(R_lower) <- list(indicators, indicators)
  dimnames(R_upper) <- list(indicators, indicators)
  boundary <- p <= .Machine$double.eps | p >= 1 - .Machine$double.eps

  if (K > 1L) {
    for (k in seq_len(K - 1L)) {
      for (ell in (k + 1L):K) {
        denom <- sqrt(p[[k]] * (1 - p[[k]]) * p[[ell]] * (1 - p[[ell]]))
        if (denom < .Machine$double.eps) {
          R_lower[k, ell] <- NA_real_
          R_upper[k, ell] <- NA_real_
        } else {
          R_lower[k, ell] <- (max(0, p[[k]] + p[[ell]] - 1) - p[[k]] * p[[ell]]) / denom
          R_upper[k, ell] <- (min(p[[k]], p[[ell]]) - p[[k]] * p[[ell]]) / denom
        }
        R_lower[ell, k] <- R_lower[k, ell]
        R_upper[ell, k] <- R_upper[k, ell]
      }
    }
  }

  raw_pairwise_intervals <- .sm_frechet_pairwise_interval_table(
    p = p,
    s = s,
    indicators = indicators,
    R_lower = R_lower,
    R_upper = R_upper,
    site_id = site_id,
    year = year,
    n_common = n_common
  )

  S <- tcrossprod(s)
  unprojected_negative_dependence_corner <- R_lower * S
  unprojected_positive_dependence_corner <- R_upper * S
  unprojected_negative_dependence_corner[is.na(unprojected_negative_dependence_corner)] <- 0
  unprojected_positive_dependence_corner[is.na(unprojected_positive_dependence_corner)] <- 0
  diag(unprojected_negative_dependence_corner) <- s^2
  diag(unprojected_positive_dependence_corner) <- s^2
  dimnames(unprojected_negative_dependence_corner) <- list(indicators, indicators)
  dimnames(unprojected_positive_dependence_corner) <- list(indicators, indicators)
  if (nrow(raw_pairwise_intervals) > 0L) {
    for (i in seq_len(nrow(raw_pairwise_intervals))) {
      a <- raw_pairwise_intervals$indicator_1[[i]]
      b <- raw_pairwise_intervals$indicator_2[[i]]
      unprojected_negative_dependence_corner[a, b] <- raw_pairwise_intervals$pairwise_covariance_lower[[i]]
      unprojected_negative_dependence_corner[b, a] <- raw_pairwise_intervals$pairwise_covariance_lower[[i]]
      unprojected_positive_dependence_corner[a, b] <- raw_pairwise_intervals$pairwise_covariance_upper[[i]]
      unprojected_positive_dependence_corner[b, a] <- raw_pairwise_intervals$pairwise_covariance_upper[[i]]
    }
  }

  V_independence <- diag(s^2, nrow = K, ncol = K)
  dimnames(V_independence) <- list(indicators, indicators)

  if (K <= 2L) {
    lower_psd <- .sm_frechet_identity_projection(
      unprojected_negative_dependence_corner,
      method = psd_method,
      psd_tol = psd_tol
    )
    upper_psd <- .sm_frechet_identity_projection(
      unprojected_positive_dependence_corner,
      method = psd_method,
      psd_tol = psd_tol
    )
  } else {
    lower_psd <- .sm_frechet_psd_project(
      unprojected_negative_dependence_corner,
      method = psd_method,
      psd_tol = psd_tol,
      psd_max_iter = psd_max_iter,
      shrink_alpha = shrink_alpha,
      nearpd_settings = nearpd_settings
    )
    upper_psd <- .sm_frechet_psd_project(
      unprojected_positive_dependence_corner,
      method = psd_method,
      psd_tol = psd_tol,
      psd_max_iter = psd_max_iter,
      shrink_alpha = shrink_alpha,
      nearpd_settings = nearpd_settings
    )
  }

  .sm_frechet_validate_psd_result(lower_psd$mat, psd_tol)
  .sm_frechet_validate_psd_result(upper_psd$mat, psd_tol)

  semantic_diagnostics <- .sm_frechet_projection_semantics(
    unprojected_negative = unprojected_negative_dependence_corner,
    unprojected_positive = unprojected_positive_dependence_corner,
    projected_negative = lower_psd$mat,
    projected_positive = upper_psd$mat
  )

  diagnostics <- .sm_frechet_projection_diagnostics(
    site_id = site_id,
    year = year,
    V_independence = V_independence,
    unprojected_negative = unprojected_negative_dependence_corner,
    unprojected_positive = unprojected_positive_dependence_corner,
    negative_projection = lower_psd,
    positive_projection = upper_psd,
    semantic_diagnostics = semantic_diagnostics,
    psd_method = psd_method,
    psd_tol = psd_tol,
    psd_max_iter = psd_max_iter,
    shrink_alpha = shrink_alpha
  )
  legacy_diagnostics <- .sm_frechet_legacy_psd_diagnostics(
    long = diagnostics,
    semantic_diagnostics = semantic_diagnostics,
    boundary_marginal_indicators = indicators[boundary]
  )

  list(
    V_independence = V_independence,
    raw_pairwise_intervals = raw_pairwise_intervals,
    unprojected_negative_dependence_corner = unprojected_negative_dependence_corner,
    unprojected_positive_dependence_corner = unprojected_positive_dependence_corner,
    projected_negative_dependence_stress = lower_psd$mat,
    projected_positive_dependence_stress = upper_psd$mat,
    pairwise_correlation_lower = if (isTRUE(return_correlations)) R_lower else NULL,
    pairwise_correlation_upper = if (isTRUE(return_correlations)) R_upper else NULL,
    projection_diagnostics = diagnostics,
    V_lower_raw = unprojected_negative_dependence_corner,
    V_upper_raw = unprojected_positive_dependence_corner,
    V_lower_psd = lower_psd$mat,
    V_upper_psd = upper_psd$mat,
    R_lower = if (isTRUE(return_correlations)) R_lower else NULL,
    R_upper = if (isTRUE(return_correlations)) R_upper else NULL,
    psd_diagnostics = legacy_diagnostics,
    projection_config = .sm_frechet_projection_config(
      psd_method = psd_method,
      psd_tol = psd_tol,
      psd_max_iter = psd_max_iter,
      shrink_alpha = shrink_alpha,
      nearpd_settings = nearpd_settings
    )
  )
}

.sm_frechet_pairwise_interval_table <- function(
  p,
  s,
  indicators,
  R_lower,
  R_upper,
  site_id,
  year,
  n_common = NA_real_
) {
  K <- length(indicators)
  if (K < 2L) {
    return(tibble::tibble(
      site_id = character(),
      year = integer(),
      site_key = character(),
      indicator_1 = character(),
      indicator_2 = character(),
      p_1 = double(),
      p_2 = double(),
      n_common = double(),
      joint_probability_lower = double(),
      joint_probability_upper = double(),
      pairwise_correlation_lower = double(),
      pairwise_correlation_upper = double(),
      pairwise_covariance_lower = double(),
      pairwise_covariance_upper = double(),
      interval_scale = character(),
      covariance_construction = character()
    ))
  }

  pairs <- utils::combn(seq_len(K), 2L)
  rows <- lapply(seq_len(ncol(pairs)), function(i) {
    k <- pairs[1L, i]
    ell <- pairs[2L, i]
    q_lower <- max(0, p[[k]] + p[[ell]] - 1)
    q_upper <- min(p[[k]], p[[ell]])
    has_common_n <- length(n_common) == 1L && is.finite(n_common) && n_common > 0
    covariance_scale <- s[[k]] * s[[ell]]
    covariance_lower <- if (has_common_n) {
      (q_lower - p[[k]] * p[[ell]]) / n_common
    } else if (is.finite(R_lower[k, ell])) {
      R_lower[k, ell] * covariance_scale
    } else {
      0
    }
    covariance_upper <- if (has_common_n) {
      (q_upper - p[[k]] * p[[ell]]) / n_common
    } else if (is.finite(R_upper[k, ell])) {
      R_upper[k, ell] * covariance_scale
    } else {
      0
    }
    tibble::tibble(
      site_id = as.character(site_id),
      year = as.integer(year),
      site_key = paste(as.character(site_id), as.integer(year), sep = "::"),
      indicator_1 = indicators[[k]],
      indicator_2 = indicators[[ell]],
      p_1 = p[[k]],
      p_2 = p[[ell]],
      n_common = if (has_common_n) as.numeric(n_common) else NA_real_,
      joint_probability_lower = q_lower,
      joint_probability_upper = q_upper,
      pairwise_correlation_lower = R_lower[k, ell],
      pairwise_correlation_upper = R_upper[k, ell],
      pairwise_covariance_lower = covariance_lower,
      pairwise_covariance_upper = covariance_upper,
      interval_scale = "raw_probability",
      covariance_construction = if (has_common_n) {
        "formal_iid_pairwise_covariance_over_n"
      } else {
        "heuristic_se_scaled_pairwise_correlation"
      }
    )
  })
  vctrs::vec_rbind(!!!rows)
}

.sm_frechet_projection_semantics <- function(
  unprojected_negative,
  unprojected_positive,
  projected_negative,
  projected_positive
) {
  K <- nrow(unprojected_negative)
  pair <- upper.tri(unprojected_negative)
  scale <- max(abs(c(
    unprojected_negative,
    unprojected_positive,
    projected_negative,
    projected_positive
  )))
  tolerance <- .sm_scaled_matrix_tolerance(scale, K, multiplier = 128)
  if (!any(pair)) {
    return(list(
      negative_sign_changes = 0L,
      positive_sign_changes = 0L,
      projected_order_reversals = 0L,
      projected_order_reversal_max = 0,
      projected_negative_raw_interval_violations = 0L,
      projected_positive_raw_interval_violations = 0L,
      projected_raw_interval_violations = 0L,
      projected_negative_max_raw_interval_violation = 0,
      projected_positive_max_raw_interval_violation = 0,
      diagonal_max_abs_change_negative = max(abs(diag(projected_negative) - diag(unprojected_negative))),
      diagonal_max_abs_change_positive = max(abs(diag(projected_positive) - diag(unprojected_positive)))
    ))
  }

  lower <- unprojected_negative[pair]
  upper <- unprojected_positive[pair]
  negative <- projected_negative[pair]
  positive <- projected_positive[pair]
  negative_violation <- pmax(lower - negative, negative - upper, 0)
  positive_violation <- pmax(lower - positive, positive - upper, 0)
  order_reversal <- pmax(negative - positive, 0)
  negative_sign_change <- abs(lower) > tolerance &
    abs(negative) > tolerance & sign(lower) != sign(negative)
  positive_sign_change <- abs(upper) > tolerance &
    abs(positive) > tolerance & sign(upper) != sign(positive)

  list(
    negative_sign_changes = as.integer(sum(negative_sign_change)),
    positive_sign_changes = as.integer(sum(positive_sign_change)),
    projected_order_reversals = as.integer(sum(order_reversal > tolerance)),
    projected_order_reversal_max = max(order_reversal),
    projected_negative_raw_interval_violations = as.integer(sum(negative_violation > tolerance)),
    projected_positive_raw_interval_violations = as.integer(sum(positive_violation > tolerance)),
    projected_raw_interval_violations = as.integer(sum(
      pmax(negative_violation, positive_violation) > tolerance
    )),
    projected_negative_max_raw_interval_violation = max(negative_violation),
    projected_positive_max_raw_interval_violation = max(positive_violation),
    diagonal_max_abs_change_negative = max(abs(diag(projected_negative) - diag(unprojected_negative))),
    diagonal_max_abs_change_positive = max(abs(diag(projected_positive) - diag(unprojected_positive)))
  )
}

.sm_validate_frechet_x <- function(x) {
  if (!inherits(x, "sitemix_estimates")) {
    .sm_abort_input(
      "`x` must be a `sitemix_estimates` object.",
      class = "sitemix_error_input_class",
      expected = "sitemix_estimates",
      actual = paste(class(x), collapse = "/"),
      fix = "Call `sm_estimate_from_aggregates(..., family = \"multivariate\")` first."
    )
  }
  validate.sitemix_estimates(x)
  sensitivity <- if ("estimate_status" %in% names(x)) {
    x$estimate_status == "suppression_sensitivity"
  } else {
    .sm_is_suppression_sensitivity_row(x)
  }
  if (any(sensitivity)) {
    .sm_abort_argument(
      "Suppression-sensitivity rows are excluded from formal Frechet inputs.",
      class = "sitemix_error_suppression_sensitivity_excluded",
      expected = "identified D1 marginals only",
      actual = paste0(sum(sensitivity), " suppression-sensitivity row(s)"),
      fix = "Use `suppression = \"drop\"` and a complete identified indicator set; consume separated sensitivity fields outside the Frechet API."
    )
  }
  suppressed_missing <- if ("estimate_status" %in% names(x)) {
    x$estimate_status == "suppressed_missing"
  } else {
    .sm_is_suppressed_drop_row(x)
  }
  if (any(suppressed_missing)) {
    .sm_abort_argument(
      "Suppressed-missing rows leave the formal Frechet indicator set incomplete.",
      class = "sitemix_error_invalid_indicators",
      expected = "complete identified D1 marginals in every site-year group",
      actual = paste0(sum(suppressed_missing), " suppressed-missing row(s)"),
      fix = "Provide identified marginals for the full indicator set before computing a formal Frechet envelope."
    )
  }
  if (!identical(attr(x, "family", exact = TRUE), "multivariate") ||
      !identical(attr(x, "aggregate_case", exact = TRUE), "D1")) {
    .sm_abort_argument(
      "`sm_frechet_envelope()` currently supports D1 aggregate multivariate estimates only.",
      class = "sitemix_error_invalid_aggregate_case",
      expected = "family = \"multivariate\" and aggregate_case = \"D1\"",
      actual = paste0(
        "family = ", attr(x, "family", exact = TRUE) %||% "<missing>",
        ", aggregate_case = ", attr(x, "aggregate_case", exact = TRUE) %||% "<missing>"
      ),
      fix = "Use D1 aggregate estimates; Scenario B full-information outputs already carry identified `V`."
    )
  }
  invisible(TRUE)
}

.sm_resolve_frechet_indicators <- function(x, indicator) {
  observed <- unique(x$indicator)
  if (is.null(indicator)) {
    return(observed)
  }
  if (!is.character(indicator) || length(indicator) < 1L || anyNA(indicator) || any(indicator == "") || anyDuplicated(indicator)) {
    .sm_abort_argument(
      "`indicator` must be NULL or one or more distinct D1 indicators.",
      class = "sitemix_error_invalid_indicator",
      expected = "NULL or distinct indicator labels",
      actual = as.character(indicator),
      fix = "Pass labels present in `x$indicator`."
    )
  }
  missing <- setdiff(indicator, observed)
  if (length(missing) > 0L) {
    .sm_abort_argument(
      "`indicator` contains labels not present in `x`.",
      class = "sitemix_error_invalid_indicator",
      expected = observed,
      actual = indicator,
      fix = paste0("Missing: ", .sm_cli_collapse(missing, quote = TRUE), ".")
    )
  }
  indicator
}

.sm_validate_frechet_population_regime <- function(population_regime) {
  if (is.null(population_regime)) {
    .sm_abort_argument(
      "`population_regime` is required for `sm_frechet_envelope()`.",
      class = "sitemix_error_population_regime_required",
      expected = c("d1a", "d1b"),
      actual = "NULL",
      fix = "Pass `population_regime = \"d1a\"` for formal common-population marginals or `\"d1b\"` for subgroup-conditional stress tests."
    )
  }
  .sm_public_choice(
    population_regime,
    c("d1a", "d1b"),
    "population_regime",
    "sitemix_error_invalid_population_regime"
  )
}

.sm_validate_frechet_regime_provenance <- function(x, population_regime, indicators) {
  sampling_relation <- attr(x, "sampling_relation", exact = TRUE)
  d1_regime <- attr(x, "d1_regime", exact = TRUE)
  by_group <- attr(x, "d1_regime_by_group", exact = TRUE)

  abort_provenance <- function(message, expected, actual, fix) {
    .sm_abort_argument(
      message,
      class = "sitemix_error_invalid_population_regime",
      expected = expected,
      actual = actual,
      fix = fix
    )
  }

  if (identical(population_regime, "d1b")) {
    if (identical(sampling_relation, "same_units") || identical(d1_regime, "D1a")) {
      abort_provenance(
        "`population_regime = \"d1b\"` conflicts with the D1 sampling provenance.",
        "sampling_relation = different_units or unknown",
        paste0("sampling_relation = ", sampling_relation %||% "<missing>",
               ", d1_regime = ", d1_regime %||% "<missing>"),
        "Use `population_regime = \"d1a\"` for same-unit marginals or rebuild the D1 input with truthful sampling provenance."
      )
    }
    return(invisible(TRUE))
  }

  if (!identical(sampling_relation, "same_units") || !identical(d1_regime, "D1a")) {
    abort_provenance(
      "Formal D1a pairwise intervals require explicit same-unit provenance.",
      "sampling_relation = same_units and d1_regime = D1a",
      paste0("sampling_relation = ", sampling_relation %||% "<missing>",
             ", d1_regime = ", d1_regime %||% "<missing>"),
      "Rebuild the estimates with `sampling_relation = \"same_units\"` only when publisher metadata establishes common sampled units."
    )
  }

  required_group_fields <- c(
    "site_id", "year", "sampling_relation", "denominator_pattern", "d1_regime"
  )
  if (!is.data.frame(by_group) || !all(required_group_fields %in% names(by_group)) ||
      any(by_group$sampling_relation != "same_units") ||
      any(by_group$denominator_pattern != "common") ||
      any(by_group$d1_regime != "D1a")) {
    abort_provenance(
      "Formal D1a pairwise intervals require compatible per-group provenance.",
      "every group marked same_units / common / D1a",
      if (is.data.frame(by_group)) paste(names(by_group), collapse = ", ") else "missing d1_regime_by_group",
      "Rebuild the complete D1 estimates rather than editing regime attributes after construction."
    )
  }

  work <- x[x$indicator %in% indicators, , drop = FALSE]
  group_id <- paste(work$site_id, work$year, sep = "\r")
  common_finite_n <- vapply(
    split(work$n, group_id),
    function(n) length(unique(n)) == 1L && all(is.finite(n)) && all(n > 0),
    logical(1)
  )
  if (!all(common_finite_n) || any(!is.finite(work$n_eff)) || any(work$n_eff != work$n)) {
    abort_provenance(
      "Formal D1a pairwise intervals require one finite IID denominator per site-year.",
      "common finite n with n_eff = n in every selected group",
      paste0("compatible groups = ", sum(common_finite_n), "/", length(common_finite_n)),
      "Use `population_regime = \"d1b\"` only for an explicitly heuristic analysis, or supply compatible D1a denominators."
    )
  }

  has_fpc <- ("population_size" %in% names(work) && any(!is.na(work$population_size))) ||
    ("sampling_design" %in% names(work) && any(work$sampling_design != "not_specified"))
  has_bc <- any(grepl("binomial_bc", work$var_method, fixed = TRUE))
  incompatible_v <- FALSE
  if ("V" %in% names(work)) {
    unique_groups <- split(seq_len(nrow(work)), group_id)
    incompatible_v <- any(vapply(unique_groups, function(idx) {
      V <- work$V[[idx[[1L]]]]
      !identical(V$sampling_design, "not_specified") ||
        any(V$variance_rule != "plugin") ||
        any(V$scalar_correction_rule == "binomial_bc") ||
        any(!is.na(V$population_size))
    }, logical(1)))
  }
  if (has_fpc || has_bc || incompatible_v) {
    abort_provenance(
      "Formal D1a covariance intervals are limited to the approved IID plug-in `/n` oracle.",
      "sampling_design = not_specified, variance_rule = plugin, no FPC, no binomial_bc",
      paste0("FPC = ", has_fpc, ", binomial_bc = ", has_bc,
             ", incompatible V provenance = ", incompatible_v),
      "Remove the design/bias correction or treat the result as a non-formal D1b heuristic; corrected formal Frechet covariance is deferred."
    )
  }
  invisible(TRUE)
}

.sm_handle_frechet_population_regime <- function(
  population_regime,
  subgroup_conditional_action
) {
  if (!identical(population_regime, "d1b")) {
    return(invisible(TRUE))
  }
  if (identical(subgroup_conditional_action, "error")) {
    .sm_abort_argument(
      "D1b Frechet envelopes are heuristic stress tests, not formal covariance bounds.",
      class = "sitemix_error_frechet_d1b_disallowed",
      expected = "formal D1a envelope",
      actual = "population_regime = \"d1b\"",
      fix = "Use `subgroup_conditional_action = \"warn\"` or `\"allow\"` to compute the heuristic stress test."
    )
  }
  if (identical(subgroup_conditional_action, "warn")) {
    .sm_warn(
      "D1b Frechet envelope is a heuristic stress test.",
      class = "sitemix_warning_frechet_d1b_heuristic",
      expected = "formal common-population multivariate Bernoulli marginals",
      actual = "subgroup-conditional aggregate rates",
      fix = "Use `population_regime = \"d1a\"` for formal bounds or `subgroup_conditional_action = \"allow\"` to silence this warning."
    )
  }
  invisible(TRUE)
}

.sm_validate_frechet_return_correlations <- function(return_correlations) {
  if (!is.logical(return_correlations) || length(return_correlations) != 1L || is.na(return_correlations)) {
    .sm_abort_argument(
      "`return_correlations` must be TRUE or FALSE.",
      class = "sitemix_error_invalid_return_correlations",
      expected = c("TRUE", "FALSE"),
      actual = paste(class(return_correlations), collapse = "/"),
      location = list(argument = "return_correlations"),
      fix = "Pass a scalar logical value."
    )
  }
  return_correlations
}

.sm_validate_frechet_psd_tol <- function(psd_tol) {
  max_tol <- sqrt(.Machine$double.eps)
  if (!is.numeric(psd_tol) || length(psd_tol) != 1L || is.na(psd_tol) ||
      !is.finite(psd_tol) || psd_tol <= 0 || psd_tol > max_tol) {
    .sm_abort_argument(
      "`psd_tol` must stay within the numerical-tolerance regime.",
      class = "sitemix_error_invalid_psd_tol",
      expected = paste0("0 < psd_tol <= sqrt(.Machine$double.eps) = ", max_tol),
      actual = as.character(psd_tol),
      fix = "Use the default `1e-8` or another tolerance no larger than `sqrt(.Machine$double.eps)`."
    )
  }
  invisible(TRUE)
}

.sm_validate_frechet_psd_max_iter <- function(psd_max_iter) {
  if (!is.numeric(psd_max_iter) ||
      length(psd_max_iter) != 1L ||
      is.na(psd_max_iter) ||
      !is.finite(psd_max_iter) ||
      psd_max_iter <= 0 ||
      psd_max_iter != as.integer(psd_max_iter)) {
    .sm_abort_argument(
      "`psd_max_iter` must be a positive integer scalar.",
      class = "sitemix_error_invalid_psd_max_iter",
      expected = "positive integer scalar",
      actual = as.character(psd_max_iter),
      fix = "Use an iteration cap such as `100L`."
    )
  }
  invisible(TRUE)
}

.sm_validate_frechet_shrink_alpha <- function(shrink_alpha) {
  if (is.null(shrink_alpha)) {
    return(invisible(TRUE))
  }
  if (!is.numeric(shrink_alpha) ||
      length(shrink_alpha) != 1L ||
      is.na(shrink_alpha) ||
      !is.finite(shrink_alpha) ||
      shrink_alpha <= 0 ||
      shrink_alpha > 1) {
    .sm_abort_argument(
      "`shrink_alpha` must be NULL or a finite scalar in (0, 1].",
      class = "sitemix_error_invalid_shrink_alpha",
      expected = "NULL or 0 < shrink_alpha <= 1",
      actual = as.character(shrink_alpha),
      fix = "Use `NULL` for line search or a smaller fixed shrinkage weight."
    )
  }
  invisible(TRUE)
}

.sm_validate_frechet_vector_inputs <- function(p, s, indicators) {
  if (length(p) != length(s) || length(p) != length(indicators) || length(p) < 1L) {
    .sm_abort_argument(
      "Frechet vector inputs must have equal positive lengths.",
      class = "sitemix_error_invalid_indicators",
      expected = "equal-length p, s, and indicator vectors",
      actual = paste(length(p), length(s), length(indicators), sep = " / "),
      fix = "Check D1 group extraction before computing the envelope."
    )
  }
  if (anyNA(indicators) || any(indicators == "") || anyDuplicated(indicators)) {
    .sm_abort_argument(
      "Frechet indicator labels must be unique and non-missing.",
      class = "sitemix_error_invalid_indicators",
      expected = "unique labels",
      actual = indicators,
      fix = "Use one row per indicator within each D1 site-year group."
    )
  }
  if (anyNA(p) || any(!is.finite(p)) || any(p < 0) || any(p > 1)) {
    .sm_abort_argument(
      "Frechet marginal probabilities must lie in [0, 1].",
      class = "sitemix_error_estimate_var_method",
      expected = "finite probabilities in [0, 1]",
      actual = paste(range(p, na.rm = TRUE), collapse = " to "),
      fix = "Use finite D1 estimates on the raw probability scale."
    )
  }
  if (anyNA(s) || any(!is.finite(s)) || any(s < 0)) {
    .sm_abort_argument(
      "Frechet standard errors must be finite and non-negative.",
      class = "sitemix_error_estimate_var_method",
      expected = "finite non-negative SEs",
      actual = paste(range(s, na.rm = TRUE), collapse = " to "),
      fix = "Use D1 estimates with finite SE columns."
    )
  }
  invisible(TRUE)
}

.sm_warn_frechet_non_diagonal_v <- function(group, psd_tol) {
  V <- group$V[[1]]
  mat <- as.matrix(V)
  offdiag <- mat
  diag(offdiag) <- 0
  tol <- .sm_frechet_psd_tolerance(mat, psd_tol)
  if (max(abs(offdiag)) > tol) {
    .sm_warn(
      "`sm_frechet_envelope()` ignores non-diagonal entries in input `V`.",
      class = "sitemix_warning_frechet_non_diagonal_v",
      expected = "working-independence D1 diagonal `V`",
      actual = paste0("max off-diagonal = ", signif(max(abs(offdiag)), 6)),
      fix = "Use this diagnostic only for D1 marginal aggregate outputs."
    )
  }
  invisible(TRUE)
}

.sm_frechet_sanitize_nearpd_args <- function(nearpd_args, psd_tol, psd_max_iter) {
  if (!is.list(nearpd_args)) {
    .sm_abort_argument(
      "Additional `nearPD()` settings must be supplied as named `...` arguments.",
      class = "sitemix_error_invalid_psd_method",
      expected = "a named list of supported nearPD settings",
      actual = class(nearpd_args),
      fix = "Pass supported named arguments through `...`."
    )
  }
  if (length(nearpd_args) > 0L &&
      (is.null(names(nearpd_args)) || anyNA(names(nearpd_args)) || any(names(nearpd_args) == ""))) {
    .sm_abort_argument(
      "All additional `nearPD()` arguments must be named.",
      class = "sitemix_error_invalid_psd_method",
      expected = "named arguments",
      actual = "one or more unnamed arguments",
      fix = "Name each supported `nearPD()` setting explicitly."
    )
  }
  if (anyDuplicated(names(nearpd_args))) {
    .sm_abort_argument(
      "Additional `nearPD()` argument names must be unique.",
      class = "sitemix_error_invalid_psd_method",
      expected = "unique argument names",
      actual = names(nearpd_args)[duplicated(names(nearpd_args))],
      fix = "Supply each `nearPD()` setting once."
    )
  }
  reserved <- c(
    "x", "corr", "keepDiag", "do2eigen", "only.values",
    "eig.tol", "conv.tol", "maxit", "ensureSymmetry", "trace"
  )
  conflicts <- intersect(names(nearpd_args), reserved)
  if (length(conflicts) > 0L) {
    .sm_abort_argument(
      "A reserved `nearPD()` setting conflicts with the Frechet projection contract.",
      class = "sitemix_error_invalid_psd_method",
      expected = "no overrides of x/corr/keepDiag/do2eigen/only.values/eig.tol/conv.tol/maxit/ensureSymmetry/trace",
      actual = conflicts,
      fix = "Use `psd_tol` and `psd_max_iter` for tolerances and iteration limits; remove reserved `...` arguments."
    )
  }
  allowed <- c("base.matrix", "doSym", "doDykstra", "posd.tol", "conv.norm.type")
  unsupported <- setdiff(names(nearpd_args), allowed)
  if (length(unsupported) > 0L) {
    .sm_abort_argument(
      "Unsupported `nearPD()` settings were supplied.",
      class = "sitemix_error_invalid_psd_method",
      expected = allowed,
      actual = unsupported,
      fix = "Use only deterministic settings recorded by `projection_config`."
    )
  }
  settings <- list(
    corr = FALSE,
    keepDiag = TRUE,
    base.matrix = FALSE,
    do2eigen = TRUE,
    doSym = FALSE,
    doDykstra = TRUE,
    only.values = FALSE,
    ensureSymmetry = TRUE,
    eig.tol = as.numeric(psd_tol),
    conv.tol = as.numeric(psd_tol),
    posd.tol = as.numeric(psd_tol),
    maxit = as.integer(psd_max_iter),
    conv.norm.type = "I",
    trace = FALSE
  )
  for (name in names(nearpd_args)) {
    settings[[name]] <- nearpd_args[[name]]
  }
  logical_settings <- c("base.matrix", "doSym", "doDykstra")
  logical_ok <- vapply(logical_settings, function(name) {
    value <- settings[[name]]
    is.logical(value) && length(value) == 1L && !is.na(value)
  }, logical(1))
  if (!all(logical_ok) ||
      !is.numeric(settings$posd.tol) || length(settings$posd.tol) != 1L ||
      is.na(settings$posd.tol) || !is.finite(settings$posd.tol) ||
      settings$posd.tol <= 0 || settings$posd.tol > sqrt(.Machine$double.eps) ||
      !is.character(settings$conv.norm.type) || length(settings$conv.norm.type) != 1L ||
      !settings$conv.norm.type %in% c("I", "F", "M")) {
    .sm_abort_argument(
      "A supported `nearPD()` setting has an invalid value.",
      class = "sitemix_error_invalid_psd_method",
      expected = "scalar logical controls, 0 < posd.tol <= sqrt(.Machine$double.eps), and conv.norm.type I/F/M",
      actual = nearpd_args,
      fix = "Correct the nearPD setting values or omit them to use canonical defaults."
    )
  }
  settings
}

.sm_frechet_projection_config <- function(
  psd_method,
  psd_tol,
  psd_max_iter,
  shrink_alpha,
  nearpd_settings
) {
  list(
    schema_version = 1L,
    estimate_scale = "raw_probability",
    vcov_scale = "raw",
    psd_method = psd_method,
    relative_tolerance = as.numeric(psd_tol),
    max_iterations = as.integer(psd_max_iter),
    shrink_alpha_requested = if (is.null(shrink_alpha)) NA_real_ else as.numeric(shrink_alpha),
    shrink_auto_stopping_rule = "alpha_interval_width_le_relative_tolerance",
    fixed_alpha_policy = "apply_exactly_for_k_gt_2_even_if_raw_psd",
    k_le_2_policy = "exact_identity_no_projection",
    finalization_policy = "reset_raw_diagonal_then_exact_symmetrization",
    nearpd_settings = nearpd_settings
  )
}

.sm_validate_frechet_projection_config <- function(x) {
  config <- x$projection_config
  required <- c(
    "schema_version", "estimate_scale", "vcov_scale", "psd_method",
    "relative_tolerance", "max_iterations", "shrink_alpha_requested",
    "shrink_auto_stopping_rule", "fixed_alpha_policy", "k_le_2_policy",
    "finalization_policy", "nearpd_settings"
  )
  if (!is.list(config)) {
    .sm_abort_frechet_object(
      "Frechet projection configuration is missing or malformed.",
      required,
      class(config),
      "Recreate the object; do not edit projection settings after construction."
    )
  }
  tolerance_ok <- is.numeric(x$psd_tol) && length(x$psd_tol) == 1L &&
    !is.na(x$psd_tol) && is.finite(x$psd_tol) && x$psd_tol > 0 &&
    x$psd_tol <= sqrt(.Machine$double.eps)
  method_ok <- is.character(x$psd_method) && length(x$psd_method) == 1L &&
    x$psd_method %in% c("higham", "shrink")
  max_iter_ok <- is.numeric(x$psd_max_iter) && length(x$psd_max_iter) == 1L &&
    !is.na(x$psd_max_iter) && is.finite(x$psd_max_iter) &&
    x$psd_max_iter > 0 && x$psd_max_iter == as.integer(x$psd_max_iter)
  alpha_ok <- is.null(x$shrink_alpha) || (
    identical(x$psd_method, "shrink") && is.numeric(x$shrink_alpha) &&
      length(x$shrink_alpha) == 1L && !is.na(x$shrink_alpha) &&
      is.finite(x$shrink_alpha) && x$shrink_alpha > 0 && x$shrink_alpha <= 1
  )
  expected_alpha <- if (isTRUE(alpha_ok) && !is.null(x$shrink_alpha)) {
    as.numeric(x$shrink_alpha)
  } else {
    NA_real_
  }
  config_ok <- tolerance_ok && method_ok && max_iter_ok && alpha_ok &&
    is.list(config) && identical(names(config), required) &&
    identical(config$schema_version, 1L) &&
    identical(config$estimate_scale, "raw_probability") &&
    identical(config$vcov_scale, "raw") &&
    identical(config$psd_method, x$psd_method) &&
    identical(config$relative_tolerance, as.numeric(x$psd_tol)) &&
    identical(config$max_iterations, as.integer(x$psd_max_iter)) &&
    identical(config$shrink_alpha_requested, expected_alpha) &&
    identical(config$shrink_auto_stopping_rule, "alpha_interval_width_le_relative_tolerance") &&
    identical(config$fixed_alpha_policy, "apply_exactly_for_k_gt_2_even_if_raw_psd") &&
    identical(config$k_le_2_policy, "exact_identity_no_projection") &&
    identical(config$finalization_policy, "reset_raw_diagonal_then_exact_symmetrization")
  settings <- config$nearpd_settings
  expected_setting_names <- c(
    "corr", "keepDiag", "base.matrix", "do2eigen", "doSym", "doDykstra",
    "only.values", "ensureSymmetry", "eig.tol", "conv.tol", "posd.tol",
    "maxit", "conv.norm.type", "trace"
  )
  settings_ok <- is.list(settings) && identical(names(settings), expected_setting_names) &&
    identical(settings$corr, FALSE) && identical(settings$keepDiag, TRUE) &&
    identical(settings$do2eigen, TRUE) && identical(settings$only.values, FALSE) &&
    identical(settings$ensureSymmetry, TRUE) && identical(settings$trace, FALSE) &&
    identical(settings$eig.tol, as.numeric(x$psd_tol)) &&
    identical(settings$conv.tol, as.numeric(x$psd_tol)) &&
    identical(settings$maxit, as.integer(x$psd_max_iter)) &&
    is.logical(settings$base.matrix) && length(settings$base.matrix) == 1L && !is.na(settings$base.matrix) &&
    is.logical(settings$doSym) && length(settings$doSym) == 1L && !is.na(settings$doSym) &&
    is.logical(settings$doDykstra) && length(settings$doDykstra) == 1L && !is.na(settings$doDykstra) &&
    is.numeric(settings$posd.tol) && length(settings$posd.tol) == 1L &&
    is.finite(settings$posd.tol) && settings$posd.tol > 0 &&
    settings$posd.tol <= sqrt(.Machine$double.eps) &&
    is.character(settings$conv.norm.type) && length(settings$conv.norm.type) == 1L &&
    settings$conv.norm.type %in% c("I", "F", "M")
  if (!isTRUE(config_ok) || !isTRUE(settings_ok)) {
    .sm_abort_frechet_object(
      "Frechet projection configuration is incomplete or conflicts with top-level provenance.",
      "the canonical deterministic projection configuration",
      names(config) %||% "<missing>",
      "Recreate the object; do not edit projection settings after construction."
    )
  }
  invisible(TRUE)
}

.sm_frechet_identity_projection <- function(mat, method, psd_tol) {
  list(
    mat = mat,
    iters = 0L,
    was_psd = .sm_frechet_is_psd(mat, psd_tol),
    attempted = FALSE,
    converged = NA,
    status = "identity_k_le_2",
    method = method,
    shrink_alpha_applied = NA_real_
  )
}

.sm_frechet_psd_project <- function(
  mat,
  method,
  psd_tol,
  psd_max_iter,
  shrink_alpha,
  nearpd_settings = NULL,
  nearpd_args = list()
) {
  if (is.null(nearpd_settings)) {
    nearpd_settings <- .sm_frechet_sanitize_nearpd_args(
      nearpd_args,
      psd_tol = psd_tol,
      psd_max_iter = psd_max_iter
    )
  }
  was_psd <- .sm_frechet_is_psd(mat, psd_tol)
  fixed_shrink <- identical(method, "shrink") && !is.null(shrink_alpha)
  if (isTRUE(was_psd) && !fixed_shrink) {
    return(list(
      mat = mat,
      iters = 0L,
      was_psd = TRUE,
      attempted = FALSE,
      converged = NA,
      status = "already_psd",
      method = method,
      shrink_alpha_applied = if (identical(method, "shrink")) 1 else NA_real_
    ))
  }
  if (identical(method, "higham")) {
    return(.sm_frechet_higham_project(mat, psd_tol, nearpd_settings))
  }
  .sm_frechet_shrink_project(mat, psd_tol, psd_max_iter, shrink_alpha)
}

.sm_frechet_higham_project <- function(mat, psd_tol, nearpd_settings) {
  out <- suppressWarnings(do.call(Matrix::nearPD, c(list(x = mat), nearpd_settings)))
  if (!isTRUE(out$converged)) {
    .sm_abort_vcov(
      "Higham Frechet projection did not converge within `psd_max_iter`.",
      class = "sitemix_error_vcov_projection_nonconvergence",
      expected = "Matrix::nearPD() converged = TRUE",
      actual = paste0("converged = ", out$converged %||% "<missing>",
        "; iterations = ", out$iterations %||% "<missing>"),
      fix = "Increase `psd_max_iter`, adjust supported `nearPD()` settings, or use `psd_method = \"shrink\"`."
    )
  }
  repaired <- as.matrix(out$mat)
  dimnames(repaired) <- dimnames(mat)
  diag(repaired) <- diag(mat)
  repaired <- (repaired + t(repaired)) / 2
  .sm_frechet_validate_psd_result(repaired, psd_tol)
  list(
    mat = repaired,
    iters = as.integer(out$iterations %||% 0L),
    was_psd = .sm_frechet_is_psd(mat, psd_tol),
    attempted = TRUE,
    converged = TRUE,
    status = "projected",
    method = "higham",
    shrink_alpha_applied = NA_real_
  )
}

.sm_frechet_shrink_project <- function(mat, psd_tol, psd_max_iter, shrink_alpha) {
  was_psd <- .sm_frechet_is_psd(mat, psd_tol)
  diag_mat <- diag(diag(mat), nrow = nrow(mat), ncol = ncol(mat))
  dimnames(diag_mat) <- dimnames(mat)
  shrink <- function(alpha) alpha * mat + (1 - alpha) * diag_mat

  if (!is.null(shrink_alpha)) {
    candidate <- shrink(shrink_alpha)
    candidate_tol <- .sm_frechet_psd_tolerance(candidate, psd_tol)
    if (.sm_frechet_min_eigen(candidate) < -candidate_tol) {
      .sm_abort_vcov(
        "Fixed `shrink_alpha` did not produce a PSD Frechet matrix.",
        class = "sitemix_error_vcov_invariant",
        expected = paste0("min eigenvalue >= ", -candidate_tol),
        actual = signif(.sm_frechet_min_eigen(candidate), 6),
        fix = "Lower `shrink_alpha`, use `shrink_alpha = NULL`, or switch to `psd_method = \"higham\"`."
      )
    }
    candidate <- (candidate + t(candidate)) / 2
    diag(candidate) <- diag(mat)
    candidate <- (candidate + t(candidate)) / 2
    .sm_frechet_validate_psd_result(candidate, psd_tol)
    return(list(
      mat = candidate,
      iters = 0L,
      was_psd = was_psd,
      attempted = TRUE,
      converged = TRUE,
      status = "fixed_alpha_applied",
      method = "shrink",
      shrink_alpha_applied = as.numeric(shrink_alpha)
    ))
  }

  if (isTRUE(was_psd)) {
    return(list(
      mat = mat,
      iters = 0L,
      was_psd = TRUE,
      attempted = FALSE,
      converged = NA,
      status = "already_psd",
      method = "shrink",
      shrink_alpha_applied = 1
    ))
  }

  lo <- 0
  hi <- 1
  best <- diag_mat
  iters <- 0L
  for (i in seq_len(as.integer(psd_max_iter))) {
    iters <- i
    mid <- (lo + hi) / 2
    candidate <- shrink(mid)
    if (.sm_frechet_is_psd(candidate, psd_tol)) {
      best <- candidate
      lo <- mid
    } else {
      hi <- mid
    }
    if ((hi - lo) <= psd_tol) {
      break
    }
  }
  converged <- (hi - lo) <= psd_tol
  if (!isTRUE(converged)) {
    .sm_abort_vcov(
      "Automatic shrink Frechet projection did not converge within `psd_max_iter`.",
      class = "sitemix_error_vcov_projection_nonconvergence",
      expected = paste0("alpha interval width <= ", psd_tol),
      actual = paste0("width = ", signif(hi - lo, 8), "; iterations = ", iters),
      fix = "Increase `psd_max_iter`, loosen `psd_tol`, or use a PSD-feasible fixed `shrink_alpha`."
    )
  }
  best <- (best + t(best)) / 2
  diag(best) <- diag(mat)
  best <- (best + t(best)) / 2
  .sm_frechet_validate_psd_result(best, psd_tol)
  list(
    mat = best,
    iters = as.integer(iters),
    was_psd = FALSE,
    attempted = TRUE,
    converged = TRUE,
    status = "projected",
    method = "shrink",
    shrink_alpha_applied = as.numeric(lo)
  )
}

.sm_frechet_projection_diagnostics <- function(
  site_id,
  year,
  V_independence,
  unprojected_negative,
  unprojected_positive,
  negative_projection,
  positive_projection,
  semantic_diagnostics,
  psd_method,
  psd_tol,
  psd_max_iter,
  shrink_alpha
) {
  make_row <- function(scenario, raw, projection, sign_changes, violations, max_violation) {
    projected <- projection$mat
    raw_values <- .sm_frechet_eigenvalues(raw)
    projected_values <- .sm_frechet_eigenvalues(projected)
    raw_scale <- max(abs(raw_values))
    projected_scale <- max(abs(projected_values))
    distance <- norm(projected - raw, type = "F")
    raw_norm <- norm(raw, type = "F")
    relative_distance <- if (raw_norm > 0) {
      distance / raw_norm
    } else if (distance == 0) {
      0
    } else {
      Inf
    }
    matrix_scale <- max(abs(c(raw, projected)))
    invariant_tol <- .sm_scaled_matrix_tolerance(
      matrix_scale,
      nrow(raw),
      multiplier = 128
    )
    diagonal_change <- max(abs(diag(projected) - diag(raw)))
    symmetry_residual <- max(abs(projected - t(projected)))
    tibble::tibble(
      site_id = as.character(site_id),
      year = as.integer(year),
      site_key = paste(as.character(site_id), as.integer(year), sep = "::"),
      K = as.integer(nrow(raw)),
      scenario = scenario,
      estimate_scale = "raw_probability",
      vcov_scale = "raw",
      projection_method = psd_method,
      projection_status = projection$status,
      relative_tolerance = as.numeric(psd_tol),
      absolute_tolerance_before = psd_tol * raw_scale +
        max(1, nrow(raw)) * .Machine$double.xmin,
      absolute_tolerance_after = psd_tol * projected_scale +
        max(1, nrow(projected)) * .Machine$double.xmin,
      eigen_scale_before = raw_scale,
      eigen_scale_after = projected_scale,
      min_eigen_before = min(raw_values),
      min_eigen_after = min(projected_values),
      raw_was_psd = isTRUE(projection$was_psd),
      projection_attempted = isTRUE(projection$attempted),
      converged = as.logical(projection$converged),
      iterations = as.integer(projection$iters),
      max_iterations = as.integer(psd_max_iter),
      shrink_alpha_requested = if (is.null(shrink_alpha)) NA_real_ else as.numeric(shrink_alpha),
      shrink_alpha_applied = as.numeric(projection$shrink_alpha_applied),
      frobenius_independence = norm(V_independence, type = "F"),
      raw_frobenius_norm = raw_norm,
      projected_frobenius_norm = norm(projected, type = "F"),
      projection_distance_absolute = distance,
      projection_distance_relative = relative_distance,
      diagonal_max_abs_change = diagonal_change,
      diagonal_preserved = diagonal_change <= invariant_tol,
      symmetry_max_abs_residual = symmetry_residual,
      symmetry_preserved = symmetry_residual <= invariant_tol,
      psd_preserved = min(projected_values) >= -(
        psd_tol * projected_scale + max(1, nrow(projected)) * .Machine$double.xmin
      ),
      sign_changes = as.integer(sign_changes),
      raw_interval_violations = as.integer(violations),
      max_raw_interval_violation = as.numeric(max_violation),
      projected_order_reversals = as.integer(semantic_diagnostics$projected_order_reversals),
      projected_order_reversal_max = as.numeric(semantic_diagnostics$projected_order_reversal_max)
    )
  }
  vctrs::vec_rbind(
    make_row(
      "negative_dependence_stress",
      unprojected_negative,
      negative_projection,
      semantic_diagnostics$negative_sign_changes,
      semantic_diagnostics$projected_negative_raw_interval_violations,
      semantic_diagnostics$projected_negative_max_raw_interval_violation
    ),
    make_row(
      "positive_dependence_stress",
      unprojected_positive,
      positive_projection,
      semantic_diagnostics$positive_sign_changes,
      semantic_diagnostics$projected_positive_raw_interval_violations,
      semantic_diagnostics$projected_positive_max_raw_interval_violation
    )
  )
}

.sm_frechet_legacy_psd_diagnostics <- function(
  long,
  semantic_diagnostics,
  boundary_marginal_indicators
) {
  negative <- long[long$scenario == "negative_dependence_stress", , drop = FALSE]
  positive <- long[long$scenario == "positive_dependence_stress", , drop = FALSE]
  tibble::tibble(
    site_id = negative$site_id,
    year = negative$year,
    site_key = negative$site_key,
    K = negative$K,
    negative_min_eigen_before = negative$min_eigen_before,
    negative_min_eigen_after = negative$min_eigen_after,
    negative_iterations = negative$iterations,
    negative_was_psd = negative$raw_was_psd,
    positive_min_eigen_before = positive$min_eigen_before,
    positive_min_eigen_after = positive$min_eigen_after,
    positive_iterations = positive$iterations,
    positive_was_psd = positive$raw_was_psd,
    frobenius_independence = negative$frobenius_independence,
    frobenius_unprojected_negative = negative$raw_frobenius_norm,
    frobenius_projected_negative = negative$projected_frobenius_norm,
    frobenius_unprojected_positive = positive$raw_frobenius_norm,
    frobenius_projected_positive = positive$projected_frobenius_norm,
    projection_distance_negative = negative$projection_distance_absolute,
    projection_distance_positive = positive$projection_distance_absolute,
    negative_sign_changes = semantic_diagnostics$negative_sign_changes,
    positive_sign_changes = semantic_diagnostics$positive_sign_changes,
    projected_order_reversals = semantic_diagnostics$projected_order_reversals,
    projected_order_reversal_max = semantic_diagnostics$projected_order_reversal_max,
    projected_negative_raw_interval_violations = semantic_diagnostics$projected_negative_raw_interval_violations,
    projected_positive_raw_interval_violations = semantic_diagnostics$projected_positive_raw_interval_violations,
    projected_raw_interval_violations = semantic_diagnostics$projected_raw_interval_violations,
    projected_negative_max_raw_interval_violation = semantic_diagnostics$projected_negative_max_raw_interval_violation,
    projected_positive_max_raw_interval_violation = semantic_diagnostics$projected_positive_max_raw_interval_violation,
    diagonal_max_abs_change_negative = semantic_diagnostics$diagonal_max_abs_change_negative,
    diagonal_max_abs_change_positive = semantic_diagnostics$diagonal_max_abs_change_positive,
    L_min_eig_before = negative$min_eigen_before,
    L_min_eig_after = negative$min_eigen_after,
    L_iters = negative$iterations,
    L_was_PSD = negative$raw_was_psd,
    U_min_eig_before = positive$min_eigen_before,
    U_min_eig_after = positive$min_eigen_after,
    U_iters = positive$iterations,
    U_was_PSD = positive$raw_was_psd,
    frob_WI = negative$frobenius_independence,
    frob_L_raw = negative$raw_frobenius_norm,
    frob_L_PSD = negative$projected_frobenius_norm,
    frob_U_raw = positive$raw_frobenius_norm,
    frob_U_PSD = positive$projected_frobenius_norm,
    boundary_marginal_indicators = list(boundary_marginal_indicators)
  )
}

.sm_frechet_diagnostics_equal <- function(actual, expected, tolerance) {
  if (!is.data.frame(actual) || !identical(names(actual), names(expected)) ||
      nrow(actual) != nrow(expected)) {
    return(FALSE)
  }
  all(vapply(names(expected), function(field) {
    left <- actual[[field]]
    right <- expected[[field]]
    if (is.numeric(right)) {
      same_na <- identical(is.na(left), is.na(right))
      same_inf <- identical(is.infinite(left), is.infinite(right)) &&
        all(left[is.infinite(left)] == right[is.infinite(right)])
      finite <- is.finite(left) & is.finite(right)
      numeric_scale <- pmax(abs(left[finite]), abs(right[finite]))
      numeric_tol <- 256 * .Machine$double.eps * numeric_scale +
        .Machine$double.xmin
      return(same_na && same_inf &&
        all(abs(left[finite] - right[finite]) <= numeric_tol))
    }
    identical(left, right)
  }, logical(1)))
}

.sm_frechet_validate_psd_result <- function(mat, psd_tol) {
  tol <- .sm_frechet_psd_tolerance(mat, psd_tol)
  if (.sm_frechet_min_eigen(mat) < -tol) {
    .sm_abort_vcov(
      "Frechet PSD projection failed to produce a PSD matrix.",
      class = "sitemix_error_vcov_invariant",
      expected = paste0("min eigenvalue >= ", -tol),
      actual = signif(.sm_frechet_min_eigen(mat), 6),
      fix = "Increase `psd_max_iter`, loosen `psd_tol`, or use `psd_method = \"higham\"`."
    )
  }
  invisible(TRUE)
}

.sm_frechet_min_eigen <- function(mat) {
  min(.sm_frechet_eigenvalues(mat))
}

.sm_frechet_eigenvalues <- function(mat) {
  eigen((mat + t(mat)) / 2, symmetric = TRUE, only.values = TRUE)$values
}

.sm_frechet_is_psd <- function(mat, psd_tol) {
  .sm_frechet_min_eigen(mat) >= -.sm_frechet_psd_tolerance(mat, psd_tol)
}

.sm_frechet_psd_tolerance <- function(mat, psd_tol) {
  values <- eigen((mat + t(mat)) / 2, symmetric = TRUE, only.values = TRUE)$values
  scale <- max(abs(values))
  relative <- psd_tol * scale
  absolute <- max(1, nrow(mat)) * .Machine$double.xmin
  relative + absolute
}
