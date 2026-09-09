# Aggregate-input wrapper ---------------------------------------------------

#' Estimate site-year rates from published aggregate rows
#'
#' @encoding UTF-8
#'
#' @description
#' `sm_estimate_from_aggregates()` estimates site-year proportions and
#' standard errors from published numerator and denominator counts. Use
#' \code{family = "binomial"} for one indicator (D0), or
#' \code{family = "multivariate"} for several marginal indicators (D1).
#' D1 can return a covariance matrix based on working independence;
#' marginal counts alone do not identify cross-indicator dependence.
#'
#' This function calls [sm_estimate()] with \code{from_aggregates = TRUE}.
#' Calling \code{sm_estimate(..., from_aggregates = TRUE)} directly
#' produces the same result. Do not pass \code{from_aggregates} or
#' \code{from_counts} through \code{...}; the latter raises
#' \code{sitemix_error_input_path_conflict}. Published aggregates do not
#' support \code{family = "multinomial"}; this raises
#' \code{sitemix_error_ambiguous_dispatch}. Use
#' [sm_estimate_from_counts()] with complete category counts for Scenario C.
#'
#' @details
#' \strong{Input columns.} Each row needs the site and year columns named
#' by \code{id_cols}. Counts can be arranged in either of two forms:
#' \itemize{
#'   \item Long form: one row per site-year-indicator, with
#'     \code{indicator}, \code{c_jt}, and \code{n_jt}. For D0, the
#'     \code{indicator} argument can supply the label when the column
#'     is absent.
#'   \item Wide form: one row per site-year, with numerator columns named
#'     \verb{c_jt_<indicator>}. Supply either a common \code{n_jt}
#'     denominator or a matching \verb{n_jt_<indicator>} column for
#'     every indicator. Do not mix these denominator forms. Use
#'     \code{indicators} to specify the marginal indicator names.
#' }
#' Use \code{numerator_col}, \code{denominator_col},
#' \code{indicator_col}, \code{subgroup_col}, and \code{suppression_col}
#' when the corresponding columns have other names. With \code{NULL}
#' (the default), the function uses existing \code{c_jt}, \code{n_jt},
#' \code{indicator}, \code{subgroup}, and \code{suppression_flag}
#' columns, respectively. Wide-form numerator and per-indicator denominator
#' columns use the naming patterns above. Subgroup and suppression-flag
#' columns are optional.
#'
#' \strong{One or several indicators.} Choose the family to match the
#' available counts:
#'
#' \describe{
#'   \item{\strong{D0}}{Use when \code{family = "binomial"} and the
#'     input has one numerator and one denominator per site-year.}
#'   \item{\strong{D1}}{Use when \code{family = "multivariate"}
#'     and the input has multiple aggregate
#'     marginals per site-year. With \code{vjt = TRUE}, the returned
#'     \code{V} is diagonal: off-diagonal zeros express a working
#'     independence assumption, not observed independence. For raw pairwise
#'     Fréchet intervals and separately labeled projected stress scenarios,
#'     see [sm_frechet_envelope()].}
#' }
#'
#' Set \code{aggregate_case = "auto"} (default) to resolve one unique
#' indicator as D0 and two or more as D1; pass \code{"D0"} or
#' \code{"D1"} to require that case. D1 requires the same ordered
#' indicator set in every site-year group. Set
#' \code{sampling_relation = "same_units"} only when the marginal
#' rows are known to describe the same observational units, or
#' \code{"different_units"} when they are known to differ. The default
#' \code{"unknown"} makes no such claim. Common denominators are
#' recorded separately and never imply \code{"same_units"}.
#'
#' \strong{Subgroups.} If each site has several subgroup rows, decide
#' whether to estimate each subgroup as a separate site (Framing X) or
#' as an indicator within the original site (Framing Y). Reshape the file
#' first with [sm_pivot_subgroups_to_sites()] or
#' [sm_pivot_subgroups_to_indicators()], respectively, then pass the
#' resulting table here. See
#' \code{vignette("a5-published-aggregates", package = "sitemix")}.
#'
#' \strong{Suppression.} Use [sm_suppression_report()] to count
#' publisher-suppressed rows before estimation. Detection uses the
#' suppression flag and missing numerators unless \code{suppression_when}
#' supplies a custom rule; see the suppression arguments below.
#' \code{suppression = "drop"} retains each suppressed row with missing
#' estimates and standard errors.
#'
#' \code{suppression = "upper_bound"} requires
#' \code{suppression_sensitivity_acknowledge = TRUE} when suppressed rows
#' are present. It stores a worst-case Bernoulli variance scenario in
#' separate \code{sensitivity_*} fields, leaving the estimate and SE
#' columns missing. These rows cannot supply ordinary covariance or formal
#' Fréchet inputs. A hidden denominator cannot support a numeric sensitivity
#' variance; \code{suppressed_n_strategy} and \code{suppressed_n_bound}
#' record the available denominator information without estimating that
#' variance.
#'
#' @inheritParams sm_estimate
#' @param ... Additional arguments forwarded to [sm_estimate()].
#'
#' @return A \code{sitemix_estimates} tibble with one row per
#'   site-year-indicator. It contains proportions, standard errors,
#'   their scales and calculation methods, and suppression flags.
#'   With \code{vjt = TRUE}, it also includes a \code{V} list-column.
#'   See [sm_estimate()] for the returned columns and attributes.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=sm_estimate]{sm_estimate()}} for estimation options
#'     and returned columns.
#'   \item \code{\link[=sm_estimate_from_counts]{sm_estimate_from_counts()}}
#'     for estimates from sufficient counts.
#'   \item \code{\link[=sm_pivot_subgroups_to_sites]{sm_pivot_subgroups_to_sites()}}
#'     and \code{\link[=sm_pivot_subgroups_to_indicators]{sm_pivot_subgroups_to_indicators()}}
#'     for the Framing X and Framing Y pivots.
#'   \item \code{\link[=sm_suppression_report]{sm_suppression_report()}} for
#'     publisher-side suppression auditing.
#'   \item \code{\link[=sm_frechet_envelope]{sm_frechet_envelope()}} for D1
#'     aggregate sensitivity.
#'   \item \code{vignette("a5-published-aggregates")} for the applied
#'     walkthrough.
#'   \item \code{vignette("m5-aggregate-engines")} for D0 / D1
#'     estimation methods and assumptions.
#' }
#'
#' @examples
#' \dontshow{set.seed(1L)}
#' # Select one indicator and year from the bundled simulated counts:
#' counts_path <- system.file(
#'   "extdata", "prek_sim_counts.rds",
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
#' # theta_hat and se use this scale; theta_raw remains a proportion.
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
