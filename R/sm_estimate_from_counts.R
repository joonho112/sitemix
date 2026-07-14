# Count-input wrapper -------------------------------------------------------

#' Estimate site-year rates from sufficient counts
#'
#' @encoding UTF-8
#'
#' @description
#' `sm_estimate_from_counts()` is the sufficient-counts wrapper around
#' [sm_estimate()] for sites that have already aggregated their student
#' rows into complete per-site-year sufficient statistics. It locks
#' `from_counts = TRUE` in the underlying dispatch; otherwise the
#' contract -- arguments, output schema, scale conventions -- is
#' identical to [sm_estimate()]. The sufficient-counts identity
#' guarantees agreement with [sm_estimate()] applied to the original
#' student rows to within `1e-10`; see
#' \code{vignette("m2-scalar-se-binomial", package = "sitemix")} for
#' the T2.5 invariant.
#'
#' This wrapper is the recommended public entry point for sufficient counts.
#' Its v0.2 formals are frozen, and no argument is deprecated. Direct
#' \code{sm_estimate(..., from_counts = TRUE)} calls remain supported for
#' compatibility and produce the same result.
#'
#' @details
#' The input `data` is one row per site-year with an `n_jt` denominator
#' and family-specific \verb{c_jt_*} columns. Scenario A requires one
#' named marginal count. Scenario B requires two or more ordered marginal
#' counts plus every ordered pairwise co-occurrence count; joint feasibility
#' is verified for \eqn{K = 2}{K = 2} and \eqn{K = 3}{K = 3}, while
#' \eqn{K \ge 4}{K >= 4} count input fails closed. Scenario C requires at
#' least two category counts whose row sum equals `n_jt`; the category order
#' can be set explicitly by `indicators`.
#'
#' This wrapper raises \code{sitemix_error_invalid_from_counts} if
#' the caller passes `from_counts` explicitly; call [sm_estimate()]
#' directly if you need to override the wrapper's lock. See the
#' Scenario dispatch table in [sm_estimate()] for which family /
#' indicator combinations apply.
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
#'   \item \code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}}
#'     for the published-aggregates sister wrapper.
#'   \item \code{\link[=sm_diagnose]{sm_diagnose()}} for output uncertainty
#'     auditing.
#'   \item \code{vignette("a2-input-formats")} for the input-mode decision
#'     tree.
#'   \item \code{vignette("a3-scenario-binomial")} for the Scenario A counts
#'     pathway.
#'   \item \code{vignette("m2-scalar-se-binomial")} for the T2.5
#'     sufficient-counts identity.
#' }
#'
#' @examples
#' \dontshow{set.seed(1L)}
#' counts_path <- system.file(
#'   "extdata", "alprek_subset_counts.rds",
#'   package = "sitemix", mustWork = TRUE
#' )
#' counts <- readRDS(counts_path)
#'
#' # Build a one-indicator sufficient-counts slice for Scenario A:
#' snap_counts <- counts[
#'   counts$year == 2024,
#'   c("site_id", "year", "n_jt", "c_jt_snap")
#' ]
#' est <- sm_estimate_from_counts(
#'   snap_counts,
#'   family    = "binomial",
#'   indicator = "snap"
#' )
#' head(est, 5)
#' unique(est$estimate_scale)
#'
#' @family estimation
#' @export
sm_estimate_from_counts <- function(
  data,
  family,
  indicator = NULL,
  indicators = NULL,
  id_cols = c("site_id", "year"),
  accountability_n = 30L,
  ...
) {
  if (missing(family)) {
    .sm_abort_missing_family()
  }
  dots <- list(...)
  if ("from_counts" %in% names(dots)) {
    .sm_abort_argument(
      "`from_counts` is controlled by `sm_estimate_from_counts()`.",
      class = "sitemix_error_invalid_from_counts",
      expected = "omit `from_counts`",
      actual = dots$from_counts,
      fix = "Call `sm_estimate()` directly if you need to set `from_counts`."
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
        accountability_n = accountability_n,
        from_counts = TRUE
      ),
      dots
    )
  )
}
