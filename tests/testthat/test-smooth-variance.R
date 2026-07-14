smooth_counts <- function(n = 60L) {
  data.frame(
    site_id = sprintf("S%03d", seq_len(n)),
    year = rep(2025L, n),
    n_jt = as.integer(seq(12L, 12L + n - 1L)),
    c_jt_absent = as.integer(pmax(1L, pmin(seq(12L, 12L + n - 1L) - 1L, round(seq(12L, 12L + n - 1L) * 0.25)))),
    stringsAsFactors = FALSE
  )
}

smooth_raw_counts <- function(n = 60L) {
  n_jt <- as.integer(rep(seq(20L, 39L), length.out = n))
  p <- rep(c(0.1, 0.25, 0.5, 0.75), length.out = n)
  data.frame(
    site_id = sprintf("R%03d", seq_len(n)),
    year = rep(2025L, n),
    n_jt = n_jt,
    c_jt_absent = as.integer(pmax(1L, pmin(n_jt - 1L, round(n_jt * p)))),
    stringsAsFactors = FALSE
  )
}

smooth_raw_boundary_counts <- function(n = 60L) {
  n_jt <- as.integer(rep(seq(20L, 39L), length.out = n))
  p <- rep(c(0, 1, 0.05, 0.95, 0.5), length.out = n)
  C <- as.integer(round(n_jt * p))
  C[p <= 0] <- 0L
  C[p >= 1] <- n_jt[p >= 1]
  data.frame(
    site_id = sprintf("B%03d", seq_len(n)),
    year = rep(2025L, n),
    n_jt = n_jt,
    c_jt_absent = C,
    stringsAsFactors = FALSE
  )
}

smooth_multiyear_counts <- function(n = 60L) {
  out <- smooth_counts(n)
  out$year <- rep(c(2024L, 2025L), length.out = nrow(out))
  out
}

smooth_unexpected_slope_estimates <- function(n = 60L) {
  out <- sitemix::sm_estimate_from_counts(
    smooth_counts(n),
    family = "binomial",
    indicator = "absent",
    min_n = 1L
  )
  out$n_eff <- out$n^0.6
  out$se <- 1 / (2 * sqrt(out$n_eff))
  expect_true(validate.sitemix_estimates(out))
  out
}

smooth_d1_matrix_estimates <- function(n = 60L) {
  n_jt <- as.integer(rep(seq(40L, 40L + n - 1L), each = 2L))
  d1 <- data.frame(
    site_id = rep(sprintf("D%03d", seq_len(n)), each = 2L),
    year = rep(2025L, 2L * n),
    indicator = rep(c("snap", "frpm"), n),
    c_jt = as.integer(round(n_jt * rep(c(0.25, 0.55), n))),
    n_jt = n_jt,
    stringsAsFactors = FALSE
  )
  withCallingHandlers(
    sitemix::sm_estimate_from_aggregates(
      d1,
      family = "multivariate",
      aggregate_case = "D1",
      vjt = TRUE,
      min_n = 1L
    ),
    sitemix_warning_working_independence_default = function(w) invokeRestart("muffleWarning")
  )
}

test_that("sm_smooth_variance is exported with the locked helper signature", {
  expect_true("sm_smooth_variance" %in% getNamespaceExports("sitemix"))
  expect_equal(
    names(formals(sitemix::sm_smooth_variance)),
    c(
      "x", "method", "scale", "scope", "by", "formula", "bias_correct",
      "min_n", "min_rows", "overwrite", "return_diagnostics", "..."
    )
  )
})

test_that("sm_smooth_variance appends scale-specific SE and provenance by default", {
  out <- sitemix::sm_estimate_from_counts(
    smooth_counts(),
    family = "binomial",
    indicator = "absent",
    min_n = 1L
  )
  smoothed <- sitemix::sm_smooth_variance(out, min_rows = 10L)

  expect_s3_class(smoothed, "sitemix_estimates")
  expect_true("se_smoothed" %in% names(smoothed))
  expect_false("se_raw_smoothed" %in% names(smoothed))
  expect_true("var_method_smoothed" %in% names(smoothed))
  expect_equal(smoothed$se, out$se)
  expect_equal(smoothed$se_raw, out$se_raw)
  expect_equal(smoothed$se_smoothed, out$se, tolerance = 1e-10)
  expect_equal(smoothed$var_method, out$var_method)
  expect_equal(
    smoothed$var_method_smoothed,
    paste0(out$var_method, " + gvf_smooth_loglinear")
  )
  expect_true(validate.sitemix_estimates(smoothed))
  expect_equal(attr(smoothed, "smoother_fit_summary")$status, "fit")
  expect_equal(attr(smoothed, "smoothing")$model, "experimental_gvf_log_variance")
})

test_that("append smoothing preserves design-adjusted SEs and every FPC field", {
  counts <- smooth_counts()
  out <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    fpc = 2 * counts$n_jt,
    min_n = 1L
  )
  smoothed <- sitemix::sm_smooth_variance(out, min_rows = 10L)
  fpc_fields <- c(
    "population_size", "sampling_fraction",
    "fpc_variance_multiplier", "fpc_se_multiplier",
    "variance_multiplier_applied", "se_multiplier_applied",
    "sampling_design", "variance_rule"
  )

  expect_identical(smoothed$se, out$se)
  expect_identical(smoothed$se_raw, out$se_raw)
  for (field in fpc_fields) {
    expect_identical(smoothed[[field]], out[[field]], info = field)
  }
  expect_true(validate.sitemix_estimates(smoothed))
})

test_that("sm_smooth_variance warns on unexpected loglinear arcsine slope", {
  out <- smooth_unexpected_slope_estimates()
  warning <- NULL
  smoothed <- withCallingHandlers(
    sitemix::sm_smooth_variance(out, min_rows = 10L),
    sitemix_warning_unexpected_slope = function(w) {
      warning <<- w
      invokeRestart("muffleWarning")
    }
  )
  summary <- attr(smoothed, "smoother_fit_summary")

  expect_s3_class(warning, "sitemix_warning_unexpected_slope")
  expect_s3_class(warning, "sitemix_warning")
  expect_match(rlang::cnd_message(warning), "unexpected denominator slope", fixed = TRUE)
  expect_match(rlang::cnd_message(warning), "slope_from_minus_one", fixed = TRUE)
  expect_equal(warning$expected, "loglinear arcsine slope near -1 (|slope_from_minus_one| <= 0.15)")
  expect_match(warning$actual, "slope_log_n =", fixed = TRUE)
  expect_match(warning$actual, "slope_from_minus_one =", fixed = TRUE)
  expect_equal(warning$threshold, 0.15)
  expect_equal(warning$slope_log_n, summary$slope_log_n, tolerance = 1e-12)
  expect_equal(warning$slope_from_minus_one, summary$slope_from_minus_one, tolerance = 1e-12)
  expect_gt(abs(warning$slope_from_minus_one), 0.15)
  expect_true(validate.sitemix_estimates(smoothed))
})

test_that("sm_smooth_variance does not warn about slope outside loglinear arcsine smoothing", {
  out <- smooth_unexpected_slope_estimates()

  expect_no_warning(
    sitemix::sm_smooth_variance(
      out,
      method = "loglinear",
      formula = log_var ~ 1,
      min_rows = 10L
    ),
    class = "sitemix_warning_unexpected_slope"
  )
})

test_that("sm_smooth_variance tier2 scope changes only alternative provenance", {
  out <- sitemix::sm_estimate_from_counts(
    smooth_counts(35L),
    family = "binomial",
    indicator = "absent",
    min_n = 1L
  )
  smoothed <- sitemix::sm_smooth_variance(out, scope = "tier2", min_rows = 2L)
  tier2 <- out$n >= 11L & out$n <= 29L

  expect_equal(smoothed$var_method, out$var_method)
  expect_true(all(grepl("gvf_smooth_loglinear", smoothed$var_method_smoothed[tier2])))
  expect_equal(smoothed$var_method_smoothed[!tier2], out$var_method[!tier2])
  expect_equal(smoothed$se_smoothed[!tier2], out$se[!tier2])
  expect_true(validate.sitemix_estimates(smoothed))
})

test_that("sm_smooth_variance skips fitting below min_rows with a warning", {
  out <- sitemix::sm_estimate_from_counts(
    smooth_counts(5L),
    family = "binomial",
    indicator = "absent",
    min_n = 1L
  )
  expect_warning(
    skipped <- sitemix::sm_smooth_variance(out, min_rows = 50L),
    class = "sitemix_warning_smoother_skipped"
  )

  expect_equal(skipped$se_smoothed, out$se)
  expect_equal(skipped$var_method, out$var_method)
  expect_equal(skipped$var_method_smoothed, out$var_method)
  expect_equal(attr(skipped, "smoother_fit_summary")$status, "skipped")
  expect_true(validate.sitemix_estimates(skipped))
})

test_that("sm_smooth_variance overwrite preserves the pre-smoothing audit trail", {
  out <- sitemix::sm_estimate_from_counts(
    smooth_counts(),
    family = "binomial",
    indicator = "absent",
    min_n = 1L
  )
  smoothed <- sitemix::sm_smooth_variance(out, min_rows = 10L, overwrite = TRUE)

  expect_true("se_pre_smoothing" %in% names(smoothed))
  expect_equal(smoothed$se_pre_smoothing, out$se)
  expect_true(all(grepl("gvf_smooth_loglinear", smoothed$var_method)))
  expect_equal(smoothed$var_method, smoothed$var_method_smoothed)
  expect_true(validate.sitemix_estimates(smoothed))
})

test_that("sm_smooth_variance errors before matching-scale V can become stale", {
  out <- smooth_d1_matrix_estimates()
  error <- expect_error(
    sitemix::sm_smooth_variance(out, min_rows = 10L, overwrite = TRUE),
    class = "sitemix_error_smoothing_v_stale"
  )

  expect_s3_class(error, "sitemix_error_argument")
  expect_match(rlang::cnd_message(error), "matching-scale `V`", fixed = TRUE)
  expect_equal(error$scale, "se")
  expect_equal(error$n_matching, nrow(out))
  expect_equal(error$vcov_scales, "arcsine_delta")

  appended <- sitemix::sm_smooth_variance(out, min_rows = 10L, overwrite = FALSE)
  expect_equal(attr(appended, "smoothing")$v$relation, "matching")
  expect_equal(attr(appended, "smoothing")$v$matrix_effect, "unchanged")
  expect_true(all(vapply(seq_along(out$V), function(i) {
    sitemix:::.sm_vcov_value_equal(out$V[[i]], appended$V[[i]])
  }, logical(1))))
})

test_that("sm_smooth_variance raw-scale overwrite warns and keeps validation intact", {
  raw <- sitemix::sm_estimate_from_counts(
    smooth_raw_counts(),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    min_n = 1L
  )
  warning <- NULL
  smoothed <- withCallingHandlers(
    sitemix::sm_smooth_variance(
      raw,
      scale = "se_raw",
      min_rows = 10L,
      overwrite = TRUE
    ),
    sitemix_warning_raw_scale_smoothing = function(w) {
      warning <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_true(
    is.null(warning) || inherits(warning, "sitemix_warning_raw_scale_smoothing")
  )

  expect_true("se_raw_pre_smoothing" %in% names(smoothed))
  expect_true("se_pre_smoothing" %in% names(smoothed))
  expect_true("se_raw_smoothed" %in% names(smoothed))
  expect_false("se_smoothed" %in% names(smoothed))
  expect_equal(smoothed$se_raw_pre_smoothing, raw$se_raw)
  expect_equal(smoothed$se_pre_smoothing, raw$se)
  expect_equal(smoothed$se_raw, raw$se_raw, tolerance = 1e-10)
  expect_equal(smoothed$se, smoothed$se_raw)
  expect_true(validate.sitemix_estimates(smoothed))
})

test_that("sm_smooth_variance raw-scale default includes boundary-safe p_offset", {
  raw <- sitemix::sm_estimate_from_counts(
    smooth_raw_boundary_counts(),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    boundary_method = "wilson_floor",
    min_n = 1L
  )
  smoothed <- suppressWarnings(sitemix::sm_smooth_variance(
    raw,
    scale = "se_raw",
    min_rows = 10L,
    return_diagnostics = TRUE,
    bias_correct = FALSE
  ))
  explicit <- suppressWarnings(sitemix::sm_smooth_variance(
    raw,
    scale = "se_raw",
    formula = log_var ~ log_n + offset(p_offset),
    min_rows = 10L,
    bias_correct = FALSE
  ))
  fit <- attr(smoothed, "smoother_fit")
  terms <- stats::terms(fit)
  model_frame <- stats::model.frame(fit)
  offset <- stats::model.offset(model_frame)

  z <- stats::qnorm(0.975)
  z2 <- z^2
  expected_p <- raw$theta_raw
  boundary <- raw$flag_zero_cell & (expected_p <= 0 | expected_p >= 1)
  expected_p[boundary] <- (expected_p[boundary] + z2 / (2 * raw$n[boundary])) /
    (1 + z2 / raw$n[boundary])
  expected_offset <- log(expected_p * (1 - expected_p))

  expect_true(any(boundary))
  expect_true(all(raw$se_raw[boundary] > 0))
  expect_s3_class(fit, "lm")
  expect_match(paste(deparse(stats::formula(fit)), collapse = " "), "offset\\(p_offset\\)")
  expect_equal(length(attr(terms, "offset")), 1L)
  expect_equal(offset, expected_offset, tolerance = 1e-12)
  expect_equal(smoothed$se_raw_smoothed, explicit$se_raw_smoothed, tolerance = 1e-12)
  expect_gt(min(offset[boundary]), log(.Machine$double.eps) + 1)
  expect_true(validate.sitemix_estimates(smoothed))
})

test_that("sm_smooth_variance warns before pooling multiple years by default", {
  out <- sitemix::sm_estimate_from_counts(
    smooth_multiyear_counts(),
    family = "binomial",
    indicator = "absent",
    min_n = 1L
  )
  warning <- NULL
  pooled <- withCallingHandlers(
    sitemix::sm_smooth_variance(out, min_rows = 10L, return_diagnostics = TRUE),
    sitemix_warning_smoother_multi_year_default = function(w) {
      warning <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_no_warning(
    year_aware <- sitemix::sm_smooth_variance(out, min_rows = 10L, by = "year")
  )

  expect_s3_class(warning, "sitemix_warning_smoother_multi_year_default")
  expect_match(rlang::cnd_message(warning), 'by = "year"', fixed = TRUE)
  expect_equal(attr(stats::terms(attr(pooled, "smoother_fit")), "term.labels"), "log_n")
  expect_null(attr(pooled, "smoothing")$by)
  expect_equal(attr(year_aware, "smoothing")$by, "year")
  expect_true(validate.sitemix_estimates(pooled))
  expect_true(validate.sitemix_estimates(year_aware))
})

test_that("sm_smooth_variance supports diagnostics and validates arguments", {
  out <- sitemix::sm_estimate_from_counts(
    smooth_counts(),
    family = "binomial",
    indicator = "absent",
    min_n = 1L
  )
  smoothed <- sitemix::sm_smooth_variance(out, min_rows = 10L, return_diagnostics = TRUE)

  expect_true("residual_log_var" %in% names(smoothed))
  expect_s3_class(attr(smoothed, "smoother_fit"), "lm")
  expect_equal(length(smoothed$residual_log_var), nrow(out))
  expect_true(validate.sitemix_estimates(smoothed))

  expect_error(
    sitemix::sm_smooth_variance(out, method = "bogus"),
    class = "sitemix_error_invalid_smoothing_method"
  )
  expect_error(
    sitemix::sm_smooth_variance(out, scope = "bad"),
    class = "sitemix_error_invalid_smoothing_scope"
  )
})

test_that("scale by overwrite by V contract grid is explicit and atomic", {
  without_v <- sitemix::sm_estimate_from_counts(
    smooth_raw_counts(),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    vjt = FALSE,
    min_n = 1L
  )
  with_v <- sitemix::sm_estimate_from_counts(
    smooth_raw_counts(),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    vjt = TRUE,
    min_n = 1L
  )

  for (scale in c("se", "se_raw")) {
    for (overwrite in c(FALSE, TRUE)) {
      for (has_v in c(FALSE, TRUE)) {
        input <- if (has_v) with_v else without_v
        before_se <- input$se
        before_se_raw <- input$se_raw
        before_method <- input$var_method
        if (has_v && overwrite) {
          expect_error(
            suppressWarnings(sm_smooth_variance(
              input,
              scale = scale,
              overwrite = overwrite,
              min_rows = 10L,
              bias_correct = FALSE
            )),
            class = "sitemix_error_smoothing_v_stale"
          )
          expect_equal(input$se, before_se)
          expect_equal(input$se_raw, before_se_raw)
          expect_equal(input$var_method, before_method)
          next
        }

        result <- suppressWarnings(sm_smooth_variance(
          input,
          scale = scale,
          overwrite = overwrite,
          min_rows = 10L,
          bias_correct = FALSE
        ))
        target <- if (identical(scale, "se")) "se_smoothed" else "se_raw_smoothed"
        other <- if (identical(scale, "se")) "se_raw_smoothed" else "se_smoothed"
        expect_true(target %in% names(result))
        expect_false(other %in% names(result))
        expect_true("var_method_smoothed" %in% names(result))
        expect_equal(attr(result, "smoothing")$target_column, target)
        expect_equal(attr(result, "smoothing")$v$relation, if (has_v) "matching" else "absent")
        expect_equal(attr(result, "smoother_fit_summary")$prediction_n, 60L)
        expect_true(attr(result, "smoother_fit_summary")$full_rank)
        expect_true(attr(result, "smoother_fit_summary")$converged)

        if (!overwrite) {
          expect_equal(result$se, before_se)
          expect_equal(result$se_raw, before_se_raw)
          expect_equal(result$var_method, before_method)
        } else {
          expect_true(any(result[[scale]] != input[[scale]]))
          expect_true(all(grepl("gvf_smooth_loglinear", result$var_method)))
          expect_equal(result$var_method, result$var_method_smoothed)
        }
        if (has_v) {
          expect_true(all(vapply(seq_along(input$V), function(i) {
            sitemix:::.sm_vcov_value_equal(input$V[[i]], result$V[[i]])
          }, logical(1))))
        }
        expect_true(validate.sitemix_estimates(result))
      }
    }
  }
})

test_that("incompatible-scale V remains with an exact provenance fact", {
  out <- smooth_d1_matrix_estimates()
  before_v <- out$V
  before_se <- out$se
  before_method <- out$var_method

  result <- suppressWarnings(sm_smooth_variance(
    out,
    scale = "se_raw",
    overwrite = TRUE,
    min_rows = 10L,
    bias_correct = FALSE
  ))

  expect_equal(result$se, before_se)
  expect_equal(result$var_method, before_method)
  expect_true(any(result$se_raw != out$se_raw))
  expect_true("se_raw_pre_smoothing" %in% names(result))
  expect_false("se_pre_smoothing" %in% names(result))
  expect_equal(attr(result, "smoothing")$v$relation, "incompatible")
  expect_equal(attr(result, "smoothing")$v$expected_scales, "raw")
  expect_equal(attr(result, "smoothing")$v$actual_scales, "arcsine_delta")
  expect_true(all(vapply(seq_along(before_v), function(i) {
    sitemix:::.sm_vcov_value_equal(before_v[[i]], result$V[[i]])
  }, logical(1))))
  expect_true(validate.sitemix_estimates(result))
})

test_that("custom smoothing formula has a closed variable and term vocabulary", {
  out <- sitemix::sm_estimate_from_counts(
    smooth_raw_counts(),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    min_n = 1L
  )

  expect_error(
    sm_smooth_variance(out, formula = response ~ log_n, min_rows = 10L),
    class = "sitemix_error_invalid_smoothing_formula"
  )
  expect_error(
    sm_smooth_variance(out, formula = log_var ~ site_id, min_rows = 10L),
    class = "sitemix_error_invalid_smoothing_formula"
  )
  expect_error(
    sm_smooth_variance(out, formula = log_var ~ I(log_n^2), min_rows = 10L),
    class = "sitemix_error_invalid_smoothing_formula"
  )
  expect_no_error(sm_smooth_variance(
    out,
    formula = log_var ~ log_n + offset(p_offset),
    min_rows = 10L,
    bias_correct = FALSE
  ))
  expect_null(attr(out, "smoothing", exact = TRUE))
})

test_that("rank and residual-df failures abort before provenance exists", {
  out <- sitemix::sm_estimate_from_counts(
    smooth_raw_counts(),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    min_n = 1L
  )
  error <- expect_error(
    sm_smooth_variance(
      out,
      by = "site_id",
      formula = log_var ~ by_1,
      min_rows = 10L
    ),
    class = "sitemix_error_smoother_fit"
  )

  expect_match(rlang::cnd_message(error), "validity checks|could not be fit")
  expect_null(attr(out, "smoothing", exact = TRUE))
  expect_false("var_method_smoothed" %in% names(out))
})

test_that("non-finite and wrong-length predictions never silently fall back", {
  out <- sitemix::sm_estimate_from_counts(
    smooth_raw_counts(),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    min_n = 1L
  )

  testthat::local_mocked_bindings(
    .sm_smoothing_predict = function(fit, fit_data, method) {
      rep(Inf, nrow(fit_data))
    },
    .package = "sitemix"
  )
  expect_error(
    sm_smooth_variance(out, min_rows = 10L),
    class = "sitemix_error_smoother_prediction"
  )
  expect_null(attr(out, "smoothing", exact = TRUE))
  expect_false("se_smoothed" %in% names(out))
})

test_that("prediction length is validated exactly", {
  out <- sitemix::sm_estimate_from_counts(
    smooth_raw_counts(),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    min_n = 1L
  )

  testthat::local_mocked_bindings(
    .sm_smoothing_predict = function(fit, fit_data, method) numeric(),
    .package = "sitemix"
  )
  expect_error(
    sm_smooth_variance(out, min_rows = 10L),
    class = "sitemix_error_smoother_prediction"
  )
})

test_that("valid GAM fits satisfy rank, convergence, and prediction gates", {
  skip_if_not_installed("mgcv", minimum_version = "1.9.0")
  out <- sitemix::sm_estimate_from_counts(
    smooth_raw_counts(),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    min_n = 1L
  )
  result <- suppressWarnings(sm_smooth_variance(
    out,
    method = "gam",
    scale = "se_raw",
    formula = log_var ~ s(log_n, k = 5),
    min_rows = 10L,
    bias_correct = FALSE
  ))
  summary <- attr(result, "smoother_fit_summary")

  expect_equal(summary$fit_rank, summary$n_coefficients)
  expect_gt(summary$residual_df, 0)
  expect_true(summary$full_rank)
  expect_true(summary$converged)
  expect_equal(summary$prediction_n, nrow(out))
  expect_true(summary$prediction_finite)
  expect_true(validate.sitemix_estimates(result))
})

test_that("skipped overwrite never changes canonical SE, provenance, or V", {
  out <- smooth_d1_matrix_estimates(n = 4L)
  before_v <- out$V

  expect_warning(
    result <- sm_smooth_variance(out, min_rows = 50L, overwrite = TRUE),
    class = "sitemix_warning_smoother_skipped"
  )

  expect_equal(result$se, out$se)
  expect_equal(result$se_raw, out$se_raw)
  expect_equal(result$var_method, out$var_method)
  expect_false("se_pre_smoothing" %in% names(result))
  expect_equal(attr(result, "smoothing")$status, "skipped")
  expect_equal(attr(result, "smoothing")$v$relation, "matching")
  expect_true(all(vapply(seq_along(before_v), function(i) {
    sitemix:::.sm_vcov_value_equal(before_v[[i]], result$V[[i]])
  }, logical(1))))
  expect_true(validate.sitemix_estimates(result))
})

test_that("legacy fh smoothing provenance remains readable", {
  out <- sitemix::sm_estimate_from_counts(
    smooth_raw_counts(n = 4L),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    min_n = 1L
  )
  out$se_pre_smoothing <- out$se
  out$var_method <- paste0(out$var_method, " + fh_smooth_loglinear")

  expect_true(validate.sitemix_estimates(out))
  expect_equal(
    sitemix:::.sm_strip_smoothing_var_method(out$var_method),
    rep("binomial", nrow(out))
  )
})

test_that("fit validator rejects rank deficiency and non-convergence", {
  fit_data <- data.frame(
    log_var = seq(-4, -2, length.out = 12L),
    log_n = seq(2, 4, length.out = 12L)
  )
  fit <- stats::lm(log_var ~ log_n, data = fit_data)

  rank_deficient <- fit
  rank_deficient$rank <- fit$rank - 1L
  expect_error(
    sitemix:::.sm_validate_smoothing_fit(
      rank_deficient,
      method = "loglinear",
      n_expected = nrow(fit_data),
      model_formula = log_var ~ log_n
    ),
    class = "sitemix_error_smoother_fit"
  )

  not_converged <- fit
  not_converged$converged <- FALSE
  expect_error(
    sitemix:::.sm_validate_smoothing_fit(
      not_converged,
      method = "gam",
      n_expected = nrow(fit_data),
      model_formula = log_var ~ log_n
    ),
    class = "sitemix_error_smoother_fit"
  )
})

test_that("finite predictions that overflow on back-transform hard-error", {
  out <- sitemix::sm_estimate_from_counts(
    smooth_raw_counts(),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    min_n = 1L
  )
  testthat::local_mocked_bindings(
    .sm_smoothing_predict = function(fit, fit_data, method) {
      rep(log(.Machine$double.xmax) + 1, nrow(fit_data))
    },
    .package = "sitemix"
  )

  expect_error(
    sm_smooth_variance(out, min_rows = 10L, bias_correct = FALSE),
    class = "sitemix_error_smoother_prediction"
  )
  expect_null(attr(out, "smoothing", exact = TRUE))
})

test_that("public smoothing validation rejects malformed grouping and controls", {
  out <- sitemix::sm_estimate_from_counts(
    smooth_raw_counts(),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    min_n = 1L
  )
  cases <- list(
    invalid_by = list(
      call = function() sm_smooth_variance(out, by = 1, min_rows = 10L),
      class = "sitemix_error_invalid_smoothing_by",
      message = "`by` must be NULL or distinct column names."
    ),
    missing_by = list(
      call = function() sm_smooth_variance(out, by = "ghost", min_rows = 10L),
      class = "sitemix_error_invalid_smoothing_by",
      message = "`by` columns must exist in `x`."
    ),
    invalid_min_n = list(
      call = function() sm_smooth_variance(out, min_n = 0, min_rows = 10L),
      class = "sitemix_error_invalid_min_n",
      message = "`min_n` must be NULL or a positive finite number."
    ),
    invalid_formula_type = list(
      call = function() sm_smooth_variance(out, formula = 1, min_rows = 10L),
      class = "sitemix_error_invalid_smoothing_formula",
      message = "`formula` must be NULL or a formula."
    )
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    error <- expect_error(
      case$call(),
      class = case$class,
      info = case_name
    )
    expect_match(
      conditionMessage(error),
      case$message,
      fixed = TRUE,
      info = case_name
    )
  }

  filtered <- sm_smooth_variance(
    out,
    min_n = 30,
    min_rows = 10L,
    bias_correct = FALSE
  )
  provenance <- attr(filtered, "smoothing", exact = TRUE)
  expect_identical(provenance$min_n, 30)
  expect_true(all(out$n[provenance$eligible_rows] >= 30))
  expect_true(all(out$n[-provenance$eligible_rows] < 30))
  expect_true(validate.sitemix_estimates(filtered))
})

test_that("public smoothing fit failures are atomic and classed", {
  out <- sitemix::sm_estimate_from_counts(
    smooth_raw_counts(),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    min_n = 1L
  )

  formula_error <- expect_error(
    sm_smooth_variance(
      out,
      formula = log_var ~ offset(),
      min_rows = 10L
    ),
    class = "sitemix_error_smoother_fit"
  )
  expect_match(
    conditionMessage(formula_error),
    "could not be fit",
    fixed = TRUE
  )
  expect_match(
    formula_error$actual,
    "argument \"object\" is missing",
    fixed = TRUE
  )

  missing_group <- out
  missing_group$batch <- rep(c("a", "b"), length.out = nrow(missing_group))
  missing_group$batch[[1L]] <- NA_character_
  row_error <- expect_error(
    sm_smooth_variance(
      missing_group,
      by = "batch",
      min_rows = 10L
    ),
    class = "sitemix_error_smoother_fit"
  )
  expect_true(any(grepl(
    "fitted-row count differs from eligible-row count",
    row_error$actual,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "fit residuals are missing or non-finite",
    row_error$actual,
    fixed = TRUE
  )))

  expect_null(attr(out, "smoothing", exact = TRUE))
  expect_false("se_smoothed" %in% names(out))
})
