# Variance smoothing helper -------------------------------------------------

#' Smooth standard errors with an experimental variance model
#'
#' @encoding UTF-8
#'
#' @description
#' `sm_smooth_variance()` models log variances across rows and adds smoothed
#' standard errors without changing point estimates. By default it preserves
#' the original standard errors and adds either \code{se_smoothed} or
#' \code{se_raw_smoothed}, according to \code{scale}.
#'
#' This optional generalized variance-function (GVF) model is experimental.
#' Use its output as a sensitivity alternative; small sample size alone is
#' not a reason to smooth, and improved precision or downstream performance
#' is not guaranteed. The function does not fit a Fay--Herriot model or
#' estimate small-area posterior means.
#'
#' @details
#' \strong{Eligible rows.} The input is validated before fitting. A row is
#' eligible only when it is not suppressed and both its selected standard
#' error and \code{n} are finite and strictly positive. Thus an exact-census
#' row with zero SE is excluded. \code{scope = "tier2"} and \code{min_n}
#' further restrict the eligible rows. The model is fit and predictions are
#' applied only to those rows; the added SE column retains input values
#' elsewhere.
#'
#' If fewer than \code{min_rows} rows qualify, fitting is skipped with
#' \code{sitemix_warning_smoother_skipped}. The added SE column copies the
#' selected input column, and \code{var_method_smoothed} copies
#' \code{var_method}. No overwrite is attempted, even when
#' \code{overwrite = TRUE}; no new pre-smoothing snapshots, fit residuals,
#' or fitted model are added. Both summary attributes record the skipped fit.
#'
#' \strong{Model and grouping.} With \code{method = "loglinear"},
#' \code{stats::lm()} fits \code{log_var ~ log_n} for \code{scale = "se"},
#' or \code{log_var ~ log_n + offset(p_offset)} for \code{scale = "se_raw"}.
#' Here \code{log_var} is the log of the selected SE squared. The raw-scale
#' offset accounts for the rate-dependent factor \eqn{p(1-p)}{p * (1 - p)}.
#' With \code{method = "gam"}, \code{mgcv::gam()} uses \code{s(log_n)} in
#' place of \code{log_n}; the raw-scale offset is retained.
#'
#' By default, eligible rows from all years enter one model. With
#' \code{by = "year"}, the default formula adds a year fixed effect to that
#' model. To fit a separate model for each year, call this function on each
#' year's subset.
#'
#' \strong{Warnings.} In addition to the skipped-fit warning above:
#' \describe{
#'   \item{\code{sitemix_warning_smoother_multi_year_default}}{Eligible
#'     rows span multiple years and \code{by = NULL}.}
#'   \item{\code{sitemix_warning_unexpected_slope}}{A loglinear fit to
#'     \code{scale = "se"} on arcsine or Anscombe-arcsine rows has a
#'     finite \code{log_n} coefficient more than 0.15 away from -1.}
#'   \item{\code{sitemix_warning_raw_scale_smoothing}}{The first request
#'     for \code{scale = "se_raw"} after loading the package. A custom
#'     formula should include \code{offset(p_offset)} or another
#'     rate-dependent term.}
#' }
#' Invalid logical controls raise \code{sitemix_error_invalid_smoothing_flag}.
#'
#' @param x A \code{sitemix_estimates} object from [sm_estimate()] or one
#'   of its wrappers. Suppressed-missing rows may be retained but are not
#'   smoothed. An object containing non-identified suppression-sensitivity
#'   rows is rejected; smooth identified estimates before conducting a
#'   separate sensitivity analysis.
#' @param method A single string: \code{"loglinear"} (default, using
#'   \code{stats::lm()}) or \code{"gam"} (requires the optional
#'   \code{mgcv} package).
#' @param scale The standard-error column to smooth: \code{"se"}
#'   (default, on each row's \code{estimate_scale}) or \code{"se_raw"}
#'   (on the raw probability scale). \code{se} is also raw when
#'   \code{estimate_scale = "none"}.
#' @param scope A single string: \code{"all"} (default, all eligible
#'   rows) or \code{"tier2"} (eligible rows with \eqn{11 \le n \le 29}{11 <= n <= 29}).
#'   This fixed interval does not follow \code{accountability_n} or
#'   \code{flag_below_accountability}. In [sm_suppression_report()], Tier 2
#'   instead depends on the report's \code{accountability_n} threshold.
#' @param by Column names, or \code{NULL} (default). Each column is
#'   converted to a factor named \code{by_1}, \code{by_2}, and so on in
#'   the model data. The default formula includes these fixed effects in
#'   one model. A custom \code{formula} must include the desired terms
#'   explicitly. See \emph{Details} for multiple years.
#' @param formula A model formula, or \code{NULL} for the default described
#'   in \emph{Details}. The response must be \code{log_var}. Available
#'   variables are \code{log_var}, \code{log_n}, \code{n},
#'   \code{theta_raw}, \code{p_offset}, and the generated \code{by_*}
#'   factors. Use ordinary model terms and \code{offset()}; GAM formulas
#'   also allow \code{s()}, \code{te()}, \code{ti()}, and \code{t2()}.
#'   The offset variable is \code{log(p_star * (1 - p_star))}, where
#'   \code{p_star} uses \code{theta_raw} in the interior and the Wilson
#'   center at 0/1 boundaries, then is clipped away from 0 and 1 for
#'   numerical stability.
#' @param bias_correct A single logical value. If \code{TRUE} (default),
#'   add half the fitted residual variance to the predicted log variance
#'   before applying the exponential function to return to the variance scale.
#' @param min_n A finite numeric threshold of at least 1, or \code{NULL}
#'   (default). Eligible rows must also have \code{n >= min_n}.
#' @param min_rows Minimum number of eligible rows required to fit.
#'   Supply a finite number of at least 2; it is converted to an integer.
#'   Defaults to \code{50L}. See \emph{Details} for skipped fits.
#' @param overwrite A single logical value. Defaults to \code{FALSE},
#'   which preserves the original SE columns. If \code{TRUE} and a model
#'   is fit, also replace the selected SEs for eligible rows, keeping
#'   snapshots of overwritten columns. With \code{scale = "se_raw"},
#'   rows on \code{estimate_scale = "none"} also have \code{se} updated.
#'   Overwrite is rejected if any eligible row has a \code{V} matrix on
#'   the selected scale, because its unchanged diagonal could then disagree
#'   with the new SE. A \code{V} on a different scale is retained unchanged;
#'   its relation to the selected SE is recorded in \code{smoothing}.
#' @param return_diagnostics A single logical value. If \code{TRUE} and
#'   a model is fit, add \code{residual_log_var} and attach the fitted
#'   model as \code{smoother_fit}. Defaults to \code{FALSE}. The
#'   \code{smoother_fit_summary} and \code{smoothing} attributes are
#'   recorded regardless of this setting.
#' @param ... Additional arguments passed to \code{stats::lm()} or
#'   \code{mgcv::gam()}, according to \code{method}.
#'
#' @return A \code{sitemix_estimates} tibble with the same rows and point
#'   estimates as \code{x}. The function adds or updates these columns:
#'   \describe{
#'     \item{\code{se_smoothed}, \code{se_raw_smoothed}}{The alternative
#'       SE: \code{se_smoothed} for \code{scale = "se"}, or
#'       \code{se_raw_smoothed} for \code{scale = "se_raw"}. That column
#'       is added or updated; other SE columns are preserved by default.
#'       Ineligible rows keep their input values.}
#'     \item{\code{var_method_smoothed}}{The calculation label for the
#'       alternative SE. Fitted rows receive a \code{" + gvf_smooth_loglinear"}
#'       or \code{" + gvf_smooth_gam"} suffix; other rows retain their
#'       input \code{var_method}.}
#'     \item{\code{residual_log_var}}{Fit residuals for eligible rows,
#'       with \code{NA} elsewhere. Added only when a model is fit and
#'       \code{return_diagnostics = TRUE}.}
#'     \item{\code{se_pre_smoothing}, \code{se_raw_pre_smoothing}}{
#'       Copies of the input columns that are overwritten. Added only
#'       when a model is fit with \code{overwrite = TRUE}. Raw-scale
#'       overwrite also saves \code{se_pre_smoothing} when it updates
#'       \code{se} for rows with \code{estimate_scale = "none"}.}
#'   }
#'   With \code{overwrite = TRUE}, \code{var_method} is updated wherever
#'   \code{se} is replaced. Overwriting \code{se_raw} on transformed rows
#'   leaves their \code{se} and \code{var_method} unchanged. Covariance
#'   matrices and finite-population sampling information are preserved.
#'
#'   The \code{smoother_fit_summary} attribute records fit status, method,
#'   formula, number of eligible rows, and covariance-scale relation. A
#'   completed fit also records rank, convergence, prediction checks, and
#'   model summaries. The \code{smoothing} attribute records the settings,
#'   selected output column, eligible row indices, and \code{v} information.
#'   Its \code{v$relation} is \code{"absent"}, \code{"matching"},
#'   \code{"incompatible"}, or \code{"mixed"}; \code{v$matrix_effect}
#'   records whether matrices are absent or retained unchanged.
#'   Both attributes are recorded for skipped fits. With
#'   \code{return_diagnostics = TRUE}, a completed fit is additionally
#'   attached as \code{smoother_fit}; a skipped call adds no fitted model.
#'
#' @references
#' Wood, S. N. (2017). \emph{Generalized Additive Models: An
#' Introduction with R} (2nd ed.). Chapman and Hall/CRC.
#'
#' @seealso
#' [sm_estimate()] for estimates and their original standard errors;
#' [sm_diagnose()] for checking uncertainty before smoothing;
#' \code{mgcv::gam()} for fitting the GAM;
#' \code{vignette("a7-variance-smoothing-and-frechet", package = "sitemix")}
#'   for a worked example;
#' \code{vignette("m6-variance-smoothing-theory", package = "sitemix")}
#'   for the model, assumptions, and simulation findings.
#'
#' @examples
#' \dontshow{set.seed(1L)}
#' data(prek_sim, package = "sitemix")
#' est <- sm_estimate(
#'   subset(prek_sim, year == 2024),
#'   family    = "binomial",
#'   indicator = "frpm"
#' )
#'
#' # Keep the experimental alternative alongside the original SE:
#' est_s <- sm_smooth_variance(est, method = "loglinear")
#' head(est_s[, c("site_id", "n", "se", "se_smoothed")], 5)
#'
#' @family smoothing
#' @export
sm_smooth_variance <- function(
  x,
  method = c("loglinear", "gam"),
  scale = c("se", "se_raw"),
  scope = c("all", "tier2"),
  by = NULL,
  formula = NULL,
  bias_correct = TRUE,
  min_n = NULL,
  min_rows = 50L,
  overwrite = FALSE,
  return_diagnostics = FALSE,
  ...
) {
  .sm_validate_smooth_x(x)
  if (any(.sm_is_suppression_sensitivity_row(x))) {
    .sm_abort_argument(
      "Suppression-sensitivity rows are excluded from variance smoothing.",
      class = "sitemix_error_suppression_sensitivity_excluded",
      expected = "identified or suppressed-missing rows only",
      actual = "non-identified variance sensitivity present",
      fix = "Smooth an identified estimates object; consume separated sensitivity fields only in a dedicated sensitivity analysis."
    )
  }
  method <- .sm_public_choice(method, c("loglinear", "gam"), "method", "sitemix_error_invalid_smoothing_method")
  scale <- .sm_public_choice(scale, c("se", "se_raw"), "scale", "sitemix_error_invalid_smoothing_scale")
  scope <- .sm_public_choice(scope, c("all", "tier2"), "scope", "sitemix_error_invalid_smoothing_scope")
  by <- .sm_validate_smoothing_by(by, x)
  bias_correct <- .sm_validate_smoothing_flag(bias_correct, "bias_correct")
  overwrite <- .sm_validate_smoothing_flag(overwrite, "overwrite")
  return_diagnostics <- .sm_validate_smoothing_flag(return_diagnostics, "return_diagnostics")
  min_rows <- .sm_validate_smoothing_min_rows(min_rows)
  min_n <- .sm_validate_smoothing_min_n(min_n)
  .sm_validate_smoothing_formula(formula)

  model_formula <- formula %||% .sm_smoothing_default_formula(method, scale, by)
  .sm_validate_smoothing_model_formula(model_formula, by = by, method = method)

  if (identical(method, "gam") && !requireNamespace("mgcv", quietly = TRUE)) {
    .sm_abort_argument(
      "`method = \"gam\"` requires the optional `mgcv` package.",
      class = "sitemix_error_smoother_gam_unavailable",
      expected = "installed `mgcv` package",
      actual = "package not available",
      fix = "Install `mgcv` or use `method = \"loglinear\"`."
    )
  }
  if (identical(scale, "se_raw")) {
    .sm_warn_raw_scale_smoothing()
  }

  out <- x
  selected <- out[[scale]]
  eligible <- .sm_smoothing_eligible_rows(out, selected, scope = scope, min_n = min_n)
  v_fact <- .sm_smoothing_v_fact(out, scale = scale, eligible = eligible)
  .sm_warn_smoother_multi_year_default(out, eligible = eligible, by = by)
  if (sum(eligible) < min_rows) {
    target_column <- .sm_smoothing_target_column(scale)
    out[[target_column]] <- selected
    out$var_method_smoothed <- out$var_method
    attr(out, "smoother_fit_summary") <- .sm_smoother_summary_skipped(
      method = method,
      scale = scale,
      scope = scope,
      n_fit = sum(eligible),
      min_rows = min_rows,
      model_formula = model_formula,
      target_column = target_column,
      v_fact = v_fact
    )
    attr(out, "smoothing") <- .sm_smoothing_provenance(
      method = method,
      scale = scale,
      scope = scope,
      by = by,
      min_n = min_n,
      min_rows = min_rows,
      bias_correct = bias_correct,
      overwrite = overwrite,
      model_formula = model_formula,
      eligible = eligible,
      target_column = target_column,
      v_fact = v_fact,
      status = "skipped"
    )
    .sm_warn(
      "Variance smoothing skipped because too few eligible rows were available.",
      class = "sitemix_warning_smoother_skipped",
      expected = paste0("at least ", min_rows, " eligible rows"),
      actual = paste0(sum(eligible), " eligible rows"),
      fix = "Lower `min_rows`, widen `scope`, or use unsmoothed SEs."
    )
    validate.sitemix_estimates(out)
    return(out)
  }

  .sm_abort_matching_v_overwrite(
    x = out,
    scale = scale,
    overwrite = overwrite,
    v_fact = v_fact
  )

  fit_data <- .sm_smoothing_fit_data(out, selected, eligible = eligible, by = by)
  fit <- tryCatch(
    .sm_smoothing_fit(
      fit_data = fit_data,
      method = method,
      model_formula = model_formula,
      ...
    ),
    error = function(error) {
      if (inherits(error, "sitemix_error")) {
        stop(error)
      }
      .sm_abort_smoothing_fit(
        "The GVF/log-variance model could not be fit.",
        actual = conditionMessage(error),
        model_formula = model_formula
      )
    }
  )
  fit_fact <- .sm_validate_smoothing_fit(
    fit,
    method = method,
    n_expected = nrow(fit_data),
    model_formula = model_formula
  )
  pred <- tryCatch(
    .sm_smoothing_predict(fit, fit_data = fit_data, method = method),
    error = function(error) {
      if (inherits(error, "sitemix_error")) {
        stop(error)
      }
      .sm_abort_argument(
        "The GVF/log-variance model could not produce predictions.",
        class = "sitemix_error_smoother_prediction",
        expected = paste0(nrow(fit_data), " finite numeric predictions"),
        actual = conditionMessage(error),
        fix = "Inspect the model specification and prediction method; no fallback is applied.",
        formula = paste(deparse(model_formula), collapse = " ")
      )
    }
  )
  pred_fact <- .sm_validate_smoothing_prediction(
    pred,
    n_expected = nrow(fit_data),
    model_formula = model_formula
  )
  correction <- if (isTRUE(bias_correct)) {
    .sm_smoothing_bias_correction(fit, method = method, model_formula = model_formula)
  } else {
    0
  }
  fitted_log_var <- pred + correction
  candidate <- sqrt(exp(fitted_log_var))
  .sm_validate_smoothed_values(
    candidate,
    n_expected = sum(eligible),
    model_formula = model_formula
  )
  smoothed <- selected
  smoothed[eligible] <- candidate

  target_column <- .sm_smoothing_target_column(scale)
  out[[target_column]] <- smoothed
  out$var_method_smoothed <- .sm_smoothing_var_method(
    out$var_method,
    smooth_rows = eligible,
    method = method
  )
  out <- .sm_apply_smoothing_overwrite(
    out = out,
    selected = selected,
    smoothed = smoothed,
    smooth_rows = eligible,
    scale = scale,
    overwrite = overwrite
  )
  if (isTRUE(return_diagnostics)) {
    residuals <- rep(NA_real_, nrow(out))
    residuals[eligible] <- stats::residuals(fit)
    out$residual_log_var <- residuals
    attr(out, "smoother_fit") <- fit
  }
  fit_summary <- .sm_smoother_summary(
    fit = fit,
    method = method,
    scale = scale,
    scope = scope,
    n_fit = sum(eligible),
    min_rows = min_rows,
    bias_correct = bias_correct,
    overwrite = overwrite,
    model_formula = model_formula,
    target_column = target_column,
    fit_fact = fit_fact,
    pred_fact = pred_fact,
    v_fact = v_fact
  )
  .sm_warn_unexpected_slope(fit_summary, out, eligible = eligible)
  attr(out, "smoother_fit_summary") <- fit_summary
  attr(out, "smoothing") <- .sm_smoothing_provenance(
    method = method,
    scale = scale,
    scope = scope,
    by = by,
    min_n = min_n,
    min_rows = min_rows,
    bias_correct = bias_correct,
    overwrite = overwrite,
    model_formula = model_formula,
    eligible = eligible,
    target_column = target_column,
    v_fact = v_fact,
    status = "fit"
  )

  validate.sitemix_estimates(out)
  out
}

.sm_validate_smooth_x <- function(x) {
  if (!inherits(x, "sitemix_estimates")) {
    .sm_abort_input(
      "`x` must be a `sitemix_estimates` object.",
      class = "sitemix_error_input_class",
      expected = "sitemix_estimates",
      actual = paste(class(x), collapse = "/"),
      fix = "Create estimates with `sm_estimate()` or `sm_estimate_from_aggregates()`."
    )
  }
  validate.sitemix_estimates(x)
  invisible(TRUE)
}

.sm_validate_smoothing_by <- function(by, x) {
  if (is.null(by)) {
    return(NULL)
  }
  if (!is.character(by) || length(by) == 0L || anyNA(by) || any(by == "") || anyDuplicated(by)) {
    .sm_abort_argument(
      "`by` must be NULL or distinct column names.",
      class = "sitemix_error_invalid_smoothing_by",
      expected = "NULL or distinct column names",
      actual = as.character(by),
      fix = "Use columns present in `x`."
    )
  }
  missing <- setdiff(by, names(x))
  if (length(missing) > 0L) {
    .sm_abort_argument(
      "`by` columns must exist in `x`.",
      class = "sitemix_error_invalid_smoothing_by",
      expected = by,
      actual = names(x),
      fix = paste0("Missing: ", .sm_cli_collapse(missing, quote = TRUE), ".")
    )
  }
  by
}

.sm_validate_smoothing_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .sm_abort_argument(
      paste0("`", arg, "` must be TRUE or FALSE."),
      class = "sitemix_error_invalid_smoothing_flag",
      expected = c("TRUE", "FALSE"),
      actual = paste(class(x), collapse = "/"),
      location = list(argument = arg),
      fix = "Pass a scalar logical value."
    )
  }
  x
}

.sm_validate_smoothing_min_rows <- function(min_rows) {
  if (!is.numeric(min_rows) || length(min_rows) != 1L || is.na(min_rows) || !is.finite(min_rows) || min_rows < 2) {
    .sm_abort_argument(
      "`min_rows` must be a finite number of at least 2.",
      class = "sitemix_error_invalid_smoothing_min_rows",
      expected = "finite min_rows >= 2",
      actual = as.character(min_rows),
      fix = "Use a positive fitting threshold such as 50."
    )
  }
  as.integer(min_rows)
}

.sm_validate_smoothing_min_n <- function(min_n) {
  if (is.null(min_n)) {
    return(NULL)
  }
  if (!is.numeric(min_n) || length(min_n) != 1L || is.na(min_n) || !is.finite(min_n) || min_n < 1) {
    .sm_abort_argument(
      "`min_n` must be NULL or a positive finite number.",
      class = "sitemix_error_invalid_min_n",
      expected = "NULL or min_n >= 1",
      actual = as.character(min_n),
      fix = "Use a positive denominator threshold."
    )
  }
  as.numeric(min_n)
}

.sm_validate_smoothing_formula <- function(formula) {
  if (!is.null(formula) && !inherits(formula, "formula")) {
    .sm_abort_argument(
      "`formula` must be NULL or a formula.",
      class = "sitemix_error_invalid_smoothing_formula",
      expected = "NULL or formula",
      actual = paste(class(formula), collapse = "/"),
      fix = "Pass a model formula such as `log_var ~ log_n`."
    )
  }
  invisible(TRUE)
}

.sm_validate_smoothing_model_formula <- function(formula, by, method) {
  if (length(formula) != 3L ||
      !is.symbol(formula[[2L]]) ||
      !identical(as.character(formula[[2L]]), "log_var")) {
    .sm_abort_argument(
      "The smoothing formula must have `log_var` as its exact left-hand side.",
      class = "sitemix_error_invalid_smoothing_formula",
      expected = "log_var ~ ...",
      actual = paste(deparse(formula), collapse = " "),
      fix = "Use `log_var` as the response; choose predictors on the right-hand side only."
    )
  }

  allowed_variables <- c(
    "log_var", "log_n", "n", "theta_raw", "p_offset",
    paste0("by_", seq_along(by))
  )
  variables <- all.vars(formula)
  unsupported_variables <- setdiff(variables, allowed_variables)
  if (length(unsupported_variables) > 0L) {
    .sm_abort_argument(
      "The smoothing formula refers to unsupported data variables.",
      class = "sitemix_error_invalid_smoothing_formula",
      expected = allowed_variables,
      actual = unsupported_variables,
      fix = "Use only documented helper variables and generated `by_*` factor terms."
    )
  }

  allowed_calls <- c("~", "+", "-", "*", "/", ":", "^", "(", "offset")
  if (identical(method, "gam")) {
    allowed_calls <- c(allowed_calls, "s", "te", "ti", "t2")
  }
  calls <- unique(.sm_smoothing_formula_calls(formula))
  unsupported_calls <- setdiff(calls, allowed_calls)
  if (length(unsupported_calls) > 0L) {
    .sm_abort_argument(
      "The smoothing formula contains unsupported functions or operators.",
      class = "sitemix_error_invalid_smoothing_formula",
      expected = allowed_calls,
      actual = unsupported_calls,
      fix = "Use ordinary model terms, `offset()`, and documented GAM smooth terms only."
    )
  }

  invisible(TRUE)
}

.sm_smoothing_formula_calls <- function(expression) {
  if (!is.call(expression)) {
    return(character())
  }
  head <- expression[[1L]]
  name <- if (is.symbol(head)) as.character(head) else "<computed-call>"
  children <- unlist(
    lapply(as.list(expression)[-1L], .sm_smoothing_formula_calls),
    use.names = FALSE
  )
  c(name, children)
}

.sm_smoothing_target_column <- function(scale) {
  if (identical(scale, "se")) "se_smoothed" else "se_raw_smoothed"
}

.sm_smoothing_expected_vcov_scale <- function(estimate_scale, scale) {
  if (identical(scale, "se_raw")) {
    return(rep("raw", length(estimate_scale)))
  }
  unname(c(
    none = "raw",
    arcsine = "arcsine_delta",
    arcsine_anscombe = "arcsine_delta",
    logit = "logit_delta"
  )[estimate_scale])
}

.sm_smoothing_v_fact <- function(x, scale, eligible) {
  expected <- .sm_smoothing_expected_vcov_scale(x$estimate_scale, scale)
  if (!"V" %in% names(x)) {
    return(list(
      present = FALSE,
      relation = "absent",
      expected_scales = unique(expected[eligible]),
      actual_scales = character(),
      matching_rows = integer(),
      incompatible_rows = integer(),
      matrix_effect = "absent"
    ))
  }

  actual <- vapply(x$V, function(value) value$vcov_scale, character(1))
  matching <- eligible & !is.na(expected) & actual == expected
  incompatible <- eligible & !matching
  relation <- if (any(matching) && any(incompatible)) {
    "mixed"
  } else if (any(matching)) {
    "matching"
  } else {
    "incompatible"
  }
  list(
    present = TRUE,
    relation = relation,
    expected_scales = unique(expected[eligible]),
    actual_scales = unique(actual[eligible]),
    matching_rows = which(matching),
    incompatible_rows = which(incompatible),
    matrix_effect = "unchanged"
  )
}

.sm_abort_matching_v_overwrite <- function(x, scale, overwrite, v_fact) {
  if (!isTRUE(overwrite) || !isTRUE(v_fact$present) || length(v_fact$matching_rows) == 0L) {
    return(invisible(FALSE))
  }
  first <- v_fact$matching_rows[[1L]]
  .sm_abort_argument(
    "Cannot overwrite scalar SEs while retaining a matching-scale `V` matrix.",
    class = "sitemix_error_smoothing_v_stale",
    expected = "append-only smoothing or an approved matrix rebuild policy",
    actual = paste0(
      "scale = \"", scale, "\"; ", length(v_fact$matching_rows),
      " eligible rows have matching V"
    ),
    row_identity = .sm_row_identity(x, first),
    fix = "Use `overwrite = FALSE`, or remove/rebuild `V` before requesting overwrite.",
    scale = scale,
    n_matching = length(v_fact$matching_rows),
    expected_scales = v_fact$expected_scales,
    vcov_scales = v_fact$actual_scales
  )
}

.sm_warn_unexpected_slope <- function(fit_summary, x, eligible, threshold = 0.15) {
  if (!is.data.frame(fit_summary) || nrow(fit_summary) != 1L) {
    return(invisible(FALSE))
  }
  if (!identical(fit_summary$method[[1]], "loglinear") ||
      !identical(fit_summary$scale[[1]], "se")) {
    return(invisible(FALSE))
  }
  eligible_scales <- unique(x$estimate_scale[eligible])
  if (length(eligible_scales) == 0L ||
      !all(eligible_scales %in% c("arcsine", "arcsine_anscombe"))) {
    return(invisible(FALSE))
  }
  slope_from_minus_one <- fit_summary$slope_from_minus_one[[1]]
  if (!is.finite(slope_from_minus_one) || abs(slope_from_minus_one) <= threshold) {
    return(invisible(FALSE))
  }
  slope_log_n <- fit_summary$slope_log_n[[1]]
  .sm_warn(
    "Loglinear arcsine variance smoothing found an unexpected denominator slope.",
    class = "sitemix_warning_unexpected_slope",
    expected = "loglinear arcsine slope near -1 (|slope_from_minus_one| <= 0.15)",
    actual = paste0(
      "slope_log_n = ", signif(slope_log_n, 6),
      "; slope_from_minus_one = ", signif(slope_from_minus_one, 6)
    ),
    fix = "Inspect `smoother_fit_summary`, use unsmoothed SEs, or change the smoothing specification.",
    slope_log_n = slope_log_n,
    slope_from_minus_one = slope_from_minus_one,
    threshold = threshold
  )
  invisible(TRUE)
}

.sm_smoothing_eligible_rows <- function(x, selected, scope, min_n) {
  eligible <- !x$flag_suppressed &
    is.finite(selected) &
    selected > 0 &
    is.finite(x$n) &
    x$n > 0
  if (identical(scope, "tier2")) {
    eligible <- eligible & x$n >= 11L & x$n <= 29L
  }
  if (!is.null(min_n)) {
    eligible <- eligible & x$n >= min_n
  }
  eligible
}

.sm_smoothing_offset_p <- function(theta_raw, n, flag_zero_cell, z = stats::qnorm(0.975)) {
  .sm_check_probability(theta_raw, allow_boundary = TRUE)
  .sm_check_positive_n(n)
  if (!is.logical(flag_zero_cell) || length(flag_zero_cell) != length(theta_raw) || anyNA(flag_zero_cell)) {
    .sm_abort_estimate(
      "`flag_zero_cell` must be a logical vector aligned with `theta_raw`.",
      class = "sitemix_error_estimate_var_method",
      expected = "logical vector without missing values",
      actual = paste(class(flag_zero_cell), collapse = "/"),
      fix = "Use validated `sitemix_estimates` output before smoothing."
    )
  }

  p <- theta_raw
  boundary <- flag_zero_cell & (p <= 0 | p >= 1)
  if (any(boundary)) {
    z2 <- z^2
    p[boundary] <- (p[boundary] + z2 / (2 * n[boundary])) / (1 + z2 / n[boundary])
  }
  pmin(1 - .Machine$double.eps, pmax(.Machine$double.eps, p))
}

.sm_smoothing_fit_data <- function(x, selected, eligible, by) {
  p_offset <- .sm_smoothing_offset_p(
    theta_raw = x$theta_raw[eligible],
    n = x$n[eligible],
    flag_zero_cell = x$flag_zero_cell[eligible]
  )
  fit_data <- data.frame(
    log_var = log(selected[eligible]^2),
    log_n = log(x$n[eligible]),
    n = as.numeric(x$n[eligible]),
    theta_raw = x$theta_raw[eligible],
    p_offset = log(p_offset * (1 - p_offset)),
    stringsAsFactors = FALSE
  )
  for (i in seq_along(by)) {
    fit_data[[paste0("by_", i)]] <- factor(x[[by[[i]]]][eligible])
  }
  fit_data
}

.sm_smoothing_default_formula <- function(method, scale, by) {
  by_terms <- if (length(by) > 0L) paste0("by_", seq_along(by)) else character()
  rhs <- if (identical(method, "gam")) "s(log_n)" else "log_n"
  if (identical(scale, "se_raw")) {
    rhs <- paste(rhs, "offset(p_offset)", sep = " + ")
  }
  if (length(by_terms) > 0L) {
    rhs <- paste(c(rhs, by_terms), collapse = " + ")
  }
  stats::as.formula(paste("log_var ~", rhs))
}

.sm_smoothing_fit <- function(fit_data, method, model_formula, ...) {
  if (identical(method, "gam")) {
    mgcv::gam(model_formula, data = fit_data, method = "REML", ...)
  } else {
    stats::lm(model_formula, data = fit_data, ...)
  }
}

.sm_smoothing_predict <- function(fit, fit_data, method) {
  pred <- stats::predict(fit, newdata = fit_data, type = "response")
  as.numeric(pred)
}

.sm_validate_smoothing_fit <- function(fit, method, n_expected, model_formula) {
  coefficients <- tryCatch(stats::coef(fit), error = function(error) numeric())
  model_rank <- fit$rank %||% NA_integer_
  residual_df <- tryCatch(stats::df.residual(fit), error = function(error) NA_real_)
  residuals <- tryCatch(stats::residuals(fit), error = function(error) numeric())
  n_observed <- tryCatch(stats::nobs(fit), error = function(error) NA_integer_)
  converged <- if (identical(method, "gam")) isTRUE(fit$converged) else inherits(fit, "lm")
  full_rank <- length(model_rank) == 1L &&
    is.finite(model_rank) &&
    identical(as.integer(model_rank), as.integer(length(coefficients))) &&
    length(coefficients) > 0L &&
    all(is.finite(coefficients))

  problems <- character()
  if (!identical(as.integer(n_observed), as.integer(n_expected))) {
    problems <- c(problems, "fitted-row count differs from eligible-row count")
  }
  if (!isTRUE(full_rank)) {
    problems <- c(problems, "model matrix is rank-deficient or coefficients are non-finite")
  }
  if (length(residual_df) != 1L || !is.finite(residual_df) || residual_df <= 0) {
    problems <- c(problems, "residual degrees of freedom are not positive")
  }
  if (!isTRUE(converged)) {
    problems <- c(problems, "fit did not converge")
  }
  if (length(residuals) != n_expected || any(!is.finite(residuals))) {
    problems <- c(problems, "fit residuals are missing or non-finite")
  }
  if (length(problems) > 0L) {
    .sm_abort_smoothing_fit(
      "The GVF/log-variance fit failed validity checks.",
      actual = unique(problems),
      model_formula = model_formula
    )
  }

  list(
    rank = as.integer(model_rank),
    n_coefficients = as.integer(length(coefficients)),
    residual_df = as.numeric(residual_df),
    converged = converged,
    full_rank = full_rank
  )
}

.sm_abort_smoothing_fit <- function(message, actual, model_formula) {
  .sm_abort_argument(
    message,
    class = "sitemix_error_smoother_fit",
    expected = "identifiable full-rank fit with positive residual df, convergence, and finite residuals",
    actual = actual,
    fix = "Simplify the formula, increase eligible rows, or use unsmoothed SEs.",
    formula = paste(deparse(model_formula), collapse = " ")
  )
}

.sm_validate_smoothing_prediction <- function(pred, n_expected, model_formula) {
  valid_type <- is.numeric(pred) && is.null(dim(pred))
  valid_length <- length(pred) == n_expected
  valid_finite <- valid_length && all(is.finite(pred))
  if (!valid_type || !valid_length || !valid_finite) {
    .sm_abort_argument(
      "The GVF/log-variance model returned invalid predictions.",
      class = "sitemix_error_smoother_prediction",
      expected = paste0(n_expected, " finite numeric predictions"),
      actual = paste0(
        "type = ", paste(class(pred), collapse = "/"),
        "; length = ", length(pred),
        "; all_finite = ", valid_finite
      ),
      fix = "Inspect the model specification and prediction method; no fallback is applied.",
      formula = paste(deparse(model_formula), collapse = " ")
    )
  }
  list(
    prediction_n = as.integer(length(pred)),
    prediction_finite = TRUE
  )
}

.sm_validate_smoothed_values <- function(values, n_expected, model_formula) {
  if (!is.numeric(values) ||
      length(values) != n_expected ||
      any(!is.finite(values)) ||
      any(values < 0)) {
    .sm_abort_argument(
      "Back-transformed GVF predictions are non-finite or negative.",
      class = "sitemix_error_smoother_prediction",
      expected = paste0(n_expected, " finite non-negative standard errors"),
      actual = paste0(
        "length = ", length(values),
        "; non_finite = ", sum(!is.finite(values)),
        "; negative = ", sum(values < 0, na.rm = TRUE)
      ),
      fix = "Revise the formula or disable smoothing; no fallback is applied.",
      formula = paste(deparse(model_formula), collapse = " ")
    )
  }
  invisible(TRUE)
}

.sm_smoothing_bias_correction <- function(fit, method, model_formula) {
  if (identical(method, "gam")) {
    sigma2 <- tryCatch(summary(fit)$scale, error = function(e) NA_real_)
  } else {
    sigma2 <- tryCatch(stats::sigma(fit)^2, error = function(e) NA_real_)
  }
  if (length(sigma2) != 1L || is.na(sigma2) || !is.finite(sigma2) || sigma2 < 0) {
    .sm_abort_smoothing_fit(
      "The GVF/log-variance fit has an invalid residual variance for bias correction.",
      actual = as.character(sigma2),
      model_formula = model_formula
    )
  }
  0.5 * sigma2
}

.sm_smoothing_var_method <- function(var_method, smooth_rows, method) {
  result <- var_method
  suffix <- paste0(" + gvf_smooth_", method)
  result[smooth_rows] <- paste0(.sm_strip_smoothing_var_method(result[smooth_rows]), suffix)
  result
}

.sm_apply_smoothing_overwrite <- function(out, selected, smoothed, smooth_rows, scale, overwrite) {
  if (!isTRUE(overwrite)) {
    return(out)
  }
  if (identical(scale, "se")) {
    out$se_pre_smoothing <- selected
    out$se[smooth_rows] <- smoothed[smooth_rows]
    out$var_method[smooth_rows] <- out$var_method_smoothed[smooth_rows]
  } else {
    out$se_raw_pre_smoothing <- selected
    out$se_raw[smooth_rows] <- smoothed[smooth_rows]
    raw_scale_rows <- smooth_rows & out$estimate_scale == "none"
    if (any(raw_scale_rows)) {
      out$se_pre_smoothing <- out$se
      out$se[raw_scale_rows] <- smoothed[raw_scale_rows]
      out$var_method[raw_scale_rows] <- out$var_method_smoothed[raw_scale_rows]
    }
  }
  out
}

.sm_smoother_summary <- function(
  fit,
  method,
  scale,
  scope,
  n_fit,
  min_rows,
  bias_correct,
  overwrite,
  model_formula,
  target_column,
  fit_fact,
  pred_fact,
  v_fact
) {
  slope <- NA_real_
  r_squared <- NA_real_
  deviance_explained <- NA_real_
  if (identical(method, "loglinear")) {
    coef <- stats::coef(fit)
    slope <- if ("log_n" %in% names(coef)) unname(coef[["log_n"]]) else NA_real_
    r_squared <- tryCatch(suppressWarnings(summary(fit)$r.squared), error = function(e) NA_real_)
  } else {
    deviance_explained <- tryCatch(suppressWarnings(summary(fit)$dev.expl), error = function(e) NA_real_)
  }
  data.frame(
    status = "fit",
    method = method,
    scale = scale,
    scope = scope,
    n_fit = as.integer(n_fit),
    min_rows = as.integer(min_rows),
    bias_correct = bias_correct,
    overwrite = overwrite,
    formula = paste(deparse(model_formula), collapse = " "),
    target_column = target_column,
    provenance_column = "var_method_smoothed",
    fit_rank = fit_fact$rank,
    n_coefficients = fit_fact$n_coefficients,
    residual_df = fit_fact$residual_df,
    full_rank = fit_fact$full_rank,
    converged = fit_fact$converged,
    prediction_n = pred_fact$prediction_n,
    prediction_finite = pred_fact$prediction_finite,
    v_present = v_fact$present,
    v_relation = v_fact$relation,
    v_matching_rows = as.integer(length(v_fact$matching_rows)),
    v_incompatible_rows = as.integer(length(v_fact$incompatible_rows)),
    slope_log_n = slope,
    slope_from_minus_one = if (is.finite(slope)) slope + 1 else NA_real_,
    r_squared = r_squared,
    deviance_explained = deviance_explained,
    stringsAsFactors = FALSE
  )
}

.sm_smoother_summary_skipped <- function(
  method,
  scale,
  scope,
  n_fit,
  min_rows,
  model_formula,
  target_column,
  v_fact
) {
  data.frame(
    status = "skipped",
    method = method,
    scale = scale,
    scope = scope,
    n_fit = as.integer(n_fit),
    min_rows = as.integer(min_rows),
    formula = paste(deparse(model_formula), collapse = " "),
    target_column = target_column,
    provenance_column = "var_method_smoothed",
    v_present = v_fact$present,
    v_relation = v_fact$relation,
    v_matching_rows = as.integer(length(v_fact$matching_rows)),
    v_incompatible_rows = as.integer(length(v_fact$incompatible_rows)),
    stringsAsFactors = FALSE
  )
}

.sm_smoothing_provenance <- function(
  method,
  scale,
  scope,
  by,
  min_n,
  min_rows,
  bias_correct,
  overwrite,
  model_formula,
  eligible,
  target_column,
  v_fact,
  status
) {
  list(
    status = status,
    model = "experimental_gvf_log_variance",
    method = method,
    scale = scale,
    scope = scope,
    by = by,
    formula = paste(deparse(model_formula), collapse = " "),
    min_n = min_n,
    min_rows = min_rows,
    bias_correct = bias_correct,
    overwrite = overwrite,
    target_column = target_column,
    provenance_column = "var_method_smoothed",
    eligible_rows = which(eligible),
    n_eligible = as.integer(sum(eligible)),
    v = v_fact
  )
}

.sm_warn_raw_scale_smoothing <- local({
  warned <- FALSE
  function() {
    if (isTRUE(warned)) {
      return(invisible(FALSE))
    }
    warned <<- TRUE
    .sm_warn(
      "Raw-scale variance smoothing is using a scale-aware default offset.",
      class = "sitemix_warning_raw_scale_smoothing",
      expected = "raw-scale model includes the rate-dependent offset `p_offset`",
      actual = "`scale = \"se_raw\"` requested",
      fix = "If you override `formula`, include `offset(p_offset)` or another rate-dependent term."
    )
    invisible(TRUE)
  }
})

.sm_warn_smoother_multi_year_default <- function(x, eligible, by) {
  if (!is.null(by)) {
    return(invisible(FALSE))
  }
  years <- sort(unique(x$year[eligible]))
  if (length(years) <= 1L) {
    return(invisible(FALSE))
  }
  .sm_warn(
    "Variance smoothing is pooling eligible rows across multiple years.",
    class = "sitemix_warning_smoother_multi_year_default",
    expected = "explicit year-aware smoothing when year effects matter",
    actual = paste0(length(years), " eligible years: ", .sm_cli_collapse(as.character(years), quote = FALSE)),
    fix = "Use `by = \"year\"` to add year fixed effects in one pooled smoothing model, or keep `by = NULL` only when a fully pooled trend is intended."
  )
  invisible(TRUE)
}
