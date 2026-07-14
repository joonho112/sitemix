# Aggregate-input wrapper ---------------------------------------------------

#' Estimate site-year rates from published aggregate rows
#'
#' @encoding UTF-8
#'
#' @description
#' `sm_estimate_from_aggregates()` is the published-aggregates wrapper
#' around [sm_estimate()] for analysts working from publisher CSVs
#' rather than student rows. It locks `from_aggregates = TRUE` in the
#' underlying dispatch and refuses `from_counts` (raises
#' \code{sitemix_error_input_path_conflict}). The aggregate path
#' supports two scenarios: \strong{D0} single-indicator binomial rows
#' (one numerator and one denominator per site-year) and \strong{D1}
#' marginal multivariate rows (multiple aggregate marginals per
#' site-year) with optional working-independence covariance and
#' raw pairwise Fréchet intervals and projected stress scenarios via
#' [sm_frechet_envelope()]. Aggregate multinomial composition is not a
#' D1 mode and is rejected with
#' \code{sitemix_error_ambiguous_dispatch}; use
#' [sm_estimate_from_counts()] with complete category counts for Scenario C.
#'
#' This wrapper is the recommended public entry point for published aggregate
#' rows. Its v0.2 formals are frozen, and no argument is deprecated. Direct
#' \code{sm_estimate(..., from_aggregates = TRUE)} calls remain supported for
#' compatibility and produce the same result.
#'
#' @details
#' \strong{D0 vs D1.} The aggregate-input dispatch follows the same
#' Scenario taxonomy as [sm_estimate()]:
#'
#' \describe{
#'   \item{\strong{D0}}{Use when \code{family = "binomial"} and the
#'     input has one row per site-year with explicit
#'     \code{numerator_col} and \code{denominator_col}.}
#'   \item{\strong{D1}}{Use when \code{family = "multivariate"}
#'     and the input has multiple aggregate
#'     marginals per site-year. The cross-indicator covariance is not
#'     identified from marginals alone; the engine emits a diagonal
#'     working-independence \code{V} (when \code{vjt = TRUE}). For a
#'     pairwise interval and projected stress analysis of unidentified joints, see
#'     [sm_frechet_envelope()].}
#' }
#'
#' Set \code{aggregate_case = "auto"} (default) to resolve one unique
#' indicator as D0 and two or more as D1; pass \code{"D0"} or
#' \code{"D1"} to assert the case. D1 requires the same ordered
#' indicator set in every site-year group. Set
#' \code{sampling_relation = "same_units"} only when the marginal
#' rows are known to describe the same observational units, or
#' \code{"different_units"} when they are known to differ. The default
#' \code{"unknown"} makes no such claim. Common denominators are
#' recorded separately and never imply \code{"same_units"}.
#'
#' \strong{Subgroup framings.} For publisher files where each site
#' carries multiple subgroup rows, pivot the file first via
#' [sm_pivot_subgroups_to_sites()] (Framing X) or
#' [sm_pivot_subgroups_to_indicators()] (Framing Y), then pass the
#' pivoted table here. See
#' \code{vignette("a5-published-aggregates", package = "sitemix")}.
#'
#' \strong{Suppression.} The wrapper exposes these publisher-side controls:
#' \itemize{
#'   \item Detection: \code{suppression_col}, \code{suppression_flag_value},
#'     and \code{suppression_when}.
#'   \item Policy mode: \code{suppression}.
#'   \item Sensitivity point: \code{suppressed_theta_hat}.
#'   \item Required acknowledgement:
#'     \code{suppression_sensitivity_acknowledge}.
#'   \item Hidden denominators: \code{suppressed_n_strategy} and
#'     \code{suppressed_n_bound}.
#' }
#' For an
#' audit pass before estimation, use [sm_suppression_report()].
#' \code{suppression = "drop"} retains an unavailable audit row with
#' canonical estimate and SE columns missing. The legacy
#' \code{"upper_bound"} label now means a separated worst-case variance
#' sensitivity.
#'
#' \code{suppression_sensitivity_acknowledge = TRUE} is required for that
#' sensitivity; it never populates canonical estimates or ordinary covariance.
#'
#' @inheritParams sm_estimate
#' @param ... Additional arguments forwarded to [sm_estimate()].
#'
#' @return A \code{sitemix_estimates} tibble with the same column
#'   structure as [sm_estimate()]; see that function's \emph{Return}
#'   section for the canonical column glossary and object metadata.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=sm_estimate]{sm_estimate()}} for the main dispatcher
#'     and canonical column glossary.
#'   \item \code{\link[=sm_estimate_from_counts]{sm_estimate_from_counts()}}
#'     for the sufficient-counts sister wrapper.
#'   \item \code{\link[=sm_pivot_subgroups_to_sites]{sm_pivot_subgroups_to_sites()}}
#'     and \code{\link[=sm_pivot_subgroups_to_indicators]{sm_pivot_subgroups_to_indicators()}}
#'     for the Framing X and Framing Y pivots.
#'   \item \code{\link[=sm_suppression_report]{sm_suppression_report()}} for
#'     publisher-side suppression auditing.
#'   \item \code{\link[=sm_frechet_envelope]{sm_frechet_envelope()}} for D1
#'     aggregate sensitivity.
#'   \item \code{vignette("a5-published-aggregates")} for the applied
#'     walkthrough.
#'   \item \code{vignette("m5-aggregate-engines")} for formal D0 / D1
#'     specifications.
#' }
#'
#' @examples
#' \dontshow{set.seed(1L)}
#' # Build a D0 aggregate slice from the bundled count artifact:
#' counts_path <- system.file(
#'   "extdata", "alprek_subset_counts.rds",
#'   package = "sitemix", mustWork = TRUE
#' )
#' counts <- readRDS(counts_path)
#'
#' d0_frpm <- counts[counts$year == 2024, c("site_id", "year", "n_jt", "c_jt_frpm")]
#' d0_frpm$indicator <- "frpm"
#' d0_frpm$c_jt <- d0_frpm$c_jt_frpm
#' d0_frpm <- d0_frpm[c("site_id", "year", "indicator", "c_jt", "n_jt")]
#'
#' est <- sm_estimate_from_aggregates(
#'   d0_frpm,
#'   family    = "binomial",
#'   indicator = "frpm"
#' )
#' head(est, 5)
#' unique(est$estimate_scale)
#'
#' @family estimation
#' @export
sm_estimate_from_aggregates <- function(
  data,
  family,
  indicator = NULL,
  indicators = NULL,
  id_cols = c("site_id", "year"),
  numerator_col = NULL,
  denominator_col = NULL,
  indicator_col = NULL,
  subgroup_col = NULL,
  aggregate_case = c("auto", "D0", "D1"),
  framing = NA_character_,
  sampling_relation = c("unknown", "same_units", "different_units"),
  accountability_n = 30L,
  suppression = c("drop", "upper_bound"),
  suppression_col = NULL,
  suppression_flag_value = "",
  suppression_when = NULL,
  suppressed_theta_hat = 0.5,
  suppression_sensitivity_acknowledge = FALSE,
  suppressed_n_strategy = c("observed_n", "worst_case_bound"),
  suppressed_n_bound = NULL,
  ...
) {
  if (missing(family)) {
    .sm_abort_missing_family()
  }
  dots <- list(...)
  if ("from_aggregates" %in% names(dots)) {
    .sm_abort_argument(
      "`from_aggregates` is controlled by `sm_estimate_from_aggregates()`.",
      class = "sitemix_error_invalid_from_aggregates",
      expected = "omit `from_aggregates`",
      actual = dots$from_aggregates,
      fix = "Call `sm_estimate()` directly if you need to set `from_aggregates`."
    )
  }
  if ("from_counts" %in% names(dots)) {
    .sm_abort_aggregate(
      "`from_counts` cannot be used through `sm_estimate_from_aggregates()`.",
      class = "sitemix_error_input_path_conflict",
      expected = "aggregate input path only",
      actual = dots$from_counts,
      fix = "Use `sm_estimate_from_counts()` for full sufficient-count inputs."
    )
  }

  do.call(
    sm_estimate,
    c(
      list(
        data = data,
        family = family,
        indicator = indicator,
        indicators = indicators,
        id_cols = id_cols,
        numerator_col = numerator_col,
        denominator_col = denominator_col,
        indicator_col = indicator_col,
        subgroup_col = subgroup_col,
        aggregate_case = aggregate_case,
        framing = framing,
        sampling_relation = sampling_relation,
        accountability_n = accountability_n,
        suppression = suppression,
        suppression_col = suppression_col,
        suppression_flag_value = suppression_flag_value,
        suppression_when = suppression_when,
        suppressed_theta_hat = suppressed_theta_hat,
        suppression_sensitivity_acknowledge = suppression_sensitivity_acknowledge,
        suppressed_n_strategy = suppressed_n_strategy,
        suppressed_n_bound = suppressed_n_bound,
        from_aggregates = TRUE
      ),
      dots
    )
  )
}
