diagnostics_api_unsorted_long <- function(hidden = FALSE) {
  data.frame(
    site_id = c("O", "H", "I"),
    year = rep(2025L, 3L),
    indicator = rep("absent", 3L),
    c_jt = c(6L, NA_integer_, 4L),
    n_jt = c(40L, if (hidden) NA_integer_ else 8L, 20L),
    sup = c("Y", if (hidden) "Y" else "", ""),
    stringsAsFactors = FALSE
  )
}

diagnostics_api_multivariate_counts <- function(
  site_id = "R",
  indicators = c("a", "b")
) {
  out <- data.frame(
    site_id = site_id,
    year = 2025L,
    n_jt = 20L,
    first = 8L,
    second = 12L,
    joint = 5L,
    stringsAsFactors = FALSE
  )
  names(out)[4:6] <- c(
    paste0("c_jt_", indicators[[1L]]),
    paste0("c_jt_", indicators[[2L]]),
    paste0("c_jt_", indicators[[1L]], "_", indicators[[2L]])
  )
  out
}

diagnostics_api_multivariate_estimate <- function(
  site_id = "R",
  indicators = c("a", "b"),
  vst = "none"
) {
  sm_estimate_from_counts(
    diagnostics_api_multivariate_counts(site_id, indicators),
    family = "multivariate",
    indicators = indicators,
    vst = vst,
    vjt = TRUE,
    min_n = 2L
  )
}

diagnostics_api_stamp_smoothing <- function(
  x,
  overwrite = FALSE,
  status = "fit"
) {
  eligible <- rep(TRUE, nrow(x))
  x$se_smoothed <- x$se
  x$var_method_smoothed <- x$var_method
  if (isTRUE(overwrite) && identical(status, "fit")) {
    x$se_pre_smoothing <- x$se
  }
  v_fact <- sitemix:::.sm_smoothing_v_fact(x, "se", eligible)
  attr(x, "smoothing") <- sitemix:::.sm_smoothing_provenance(
    method = "loglinear",
    scale = "se",
    scope = "all",
    by = NULL,
    min_n = NULL,
    min_rows = 2L,
    bias_correct = TRUE,
    overwrite = overwrite,
    model_formula = log_var ~ log_n,
    eligible = eligible,
    target_column = "se_smoothed",
    v_fact = v_fact,
    status = status
  )
  x
}

test_that("unsorted long suppression flags stay aligned after normalization", {
  x <- diagnostics_api_unsorted_long()
  normalized <- sitemix:::.sm_prepare_aggregate_input(
    x,
    suppression_col = "sup",
    suppression_flag_value = "Y"
  )

  expect_equal(normalized$site_id, c("H", "I", "O"))
  expect_identical(normalized$suppression_flag, c(FALSE, FALSE, TRUE))
  expect_identical(normalized$flag_suppressed, c(TRUE, FALSE, TRUE))
  expect_identical(
    normalized$suppression_source,
    c("structural_na", "none", "publisher_flag")
  )

  report <- sm_suppression_report(
    x,
    by = "site_id",
    suppression_col = "sup",
    suppression_flag_value = "Y"
  )
  expect_equal(report$site_id, c("H", "I", "O"))
  expect_identical(report$n_tier1, c(1L, 0L, 1L))
  expect_identical(
    report$suppression_sources,
    c("structural_na", "", "publisher_flag")
  )

  estimated <- sm_estimate_from_aggregates(
    x,
    family = "binomial",
    indicator = "absent",
    suppression = "drop",
    suppression_col = "sup",
    suppression_flag_value = "Y",
    min_n = 2L
  )
  expect_equal(estimated$site_id, c("H", "I", "O"))
  expect_identical(estimated$flag_suppressed, c(TRUE, FALSE, TRUE))
  expect_identical(
    estimated$estimate_status,
    c("suppressed_missing", "identified", "suppressed_missing")
  )
  expect_true(is.finite(estimated$theta_hat[estimated$site_id == "I"]))
})

test_that("suppression report exposes canonical sensitivity fields and legacy aliases", {
  observed <- sm_suppression_report(
    diagnostics_api_unsorted_long(),
    by = NULL,
    suppression_col = "sup",
    suppression_flag_value = "Y"
  )
  hidden <- sm_suppression_report(
    diagnostics_api_unsorted_long(hidden = TRUE),
    by = NULL,
    suppression_col = "sup",
    suppression_flag_value = "Y"
  )
  none <- sm_suppression_report(
    diagnostics_api_unsorted_long()[3L, ],
    by = NULL,
    suppression_col = "sup",
    suppression_flag_value = "Y"
  )

  canonical <- c(
    "sensitivity_role",
    "sensitivity_numeric_variance_available",
    "sensitivity_requires_acknowledgement"
  )
  expect_true(all(canonical %in% names(observed)))

  expect_equal(observed$sensitivity_role, "nonidentified_variance_sensitivity")
  expect_true(observed$sensitivity_numeric_variance_available)
  expect_true(observed$sensitivity_requires_acknowledgement)
  expect_equal(observed$upper_bound_role, observed$sensitivity_role)
  expect_identical(
    observed$upper_bound_numeric_variance_available,
    observed$sensitivity_numeric_variance_available
  )
  expect_identical(
    observed$upper_bound_requires_acknowledgement,
    observed$sensitivity_requires_acknowledgement
  )

  expect_equal(hidden$sensitivity_role, "nonidentified_variance_sensitivity")
  expect_false(hidden$sensitivity_numeric_variance_available)
  expect_true(hidden$sensitivity_requires_acknowledgement)
  expect_equal(hidden$upper_bound_role, hidden$sensitivity_role)
  expect_identical(
    hidden$upper_bound_numeric_variance_available,
    hidden$sensitivity_numeric_variance_available
  )

  expect_equal(none$sensitivity_role, "none")
  expect_false(none$sensitivity_numeric_variance_available)
  expect_false(none$sensitivity_requires_acknowledgement)
  expect_equal(none$upper_bound_role, "not_applicable")
  expect_identical(
    none$upper_bound_numeric_variance_available,
    none$sensitivity_numeric_variance_available
  )
  expect_identical(
    none$upper_bound_requires_acknowledgement,
    none$sensitivity_requires_acknowledgement
  )
})

test_that("drop and sensitivity modes expose distinct diagnostic facts", {
  x <- diagnostics_api_unsorted_long()[2:3, ]
  drop <- sm_estimate_from_aggregates(
    x,
    family = "binomial",
    indicator = "absent",
    suppression = "drop",
    suppression_col = "sup",
    suppression_flag_value = "Y",
    min_n = 2L
  )
  drop_summary <- sm_diagnose(drop, verbose = FALSE)

  expect_identical(drop_summary$n_identified, 1L)
  expect_identical(drop_summary$n_suppressed_missing, 1L)
  expect_identical(drop_summary$n_suppression_sensitivity, 0L)
  expect_false(drop_summary$suppression_sensitivity_present)
  expect_equal(drop_summary$suppression_sensitivity_role, "none")
  expect_true(is.na(drop_summary$sensitivity_numeric_variance_available))
  expect_true(is.na(drop_summary$sensitivity_acknowledged))
  expect_equal(drop_summary$diag_severity, "warning")
  expect_match(drop_summary$diag_notes, "suppressed_missing", fixed = TRUE)

  upper <- sm_estimate_from_aggregates(
    x,
    family = "binomial",
    indicator = "absent",
    suppression = "upper_bound",
    suppression_col = "sup",
    suppression_flag_value = "Y",
    suppression_sensitivity_acknowledge = TRUE,
    min_n = 2L
  )
  upper_summary <- sm_diagnose(upper, verbose = FALSE)
  upper_rows <- sm_diagnose(upper, level = "row", verbose = FALSE)
  sensitivity <- upper_rows$estimate_status == "suppression_sensitivity"
  expect_identical(
    upper_rows$sensitivity_acknowledged,
    upper$sensitivity_acknowledged
  )

  expect_identical(upper_summary$n_identified, 1L)
  expect_identical(upper_summary$n_suppressed_missing, 0L)
  expect_identical(upper_summary$n_suppression_sensitivity, 1L)
  expect_true(upper_summary$suppression_sensitivity_present)
  expect_equal(
    upper_summary$suppression_sensitivity_role,
    "nonidentified_variance_sensitivity"
  )
  expect_true(upper_summary$sensitivity_numeric_variance_available)
  expect_true(upper_summary$sensitivity_acknowledged)
  expect_equal(upper_summary$diag_severity, "warning")
  expect_equal(
    upper_rows$suppression_sensitivity_role[sensitivity],
    "nonidentified_variance_sensitivity"
  )
  expect_true(upper_rows$sensitivity_numeric_variance_available[sensitivity])
  expect_true(upper_rows$sensitivity_acknowledged[sensitivity])

  hidden <- sm_estimate_from_aggregates(
    diagnostics_api_unsorted_long(hidden = TRUE)[2:3, ],
    family = "binomial",
    indicator = "absent",
    suppression = "upper_bound",
    suppression_col = "sup",
    suppression_flag_value = "Y",
    suppression_sensitivity_acknowledge = TRUE,
    suppressed_n_strategy = "worst_case_bound",
    suppressed_n_bound = 5L,
    min_n = 10L
  )
  hidden_summary <- sm_diagnose(hidden, verbose = FALSE)
  expect_false(hidden_summary$sensitivity_numeric_variance_available)
  expect_true(hidden_summary$sensitivity_acknowledged)
})

test_that("zero-uncertainty census is a note despite a nonpositive-SE fact", {
  census <- sm_estimate_from_counts(
    data.frame(
      site_id = "C",
      year = 2025L,
      n_jt = 10L,
      c_jt_absent = 5L
    ),
    family = "binomial",
    indicator = "absent",
    fpc = 10L,
    vjt = TRUE,
    min_n = 2L
  )
  expect_true(validate.sitemix_estimates(census))

  summary <- sm_diagnose(census, verbose = FALSE)
  rows <- sm_diagnose(census, level = "row", verbose = FALSE)
  matrices <- sm_diagnose(census, level = "vcov", verbose = FALSE)
  expect_true(summary$scalar_uncertainty_finite)
  expect_false(summary$scalar_se_positive)
  expect_false(summary$scalar_se_nonpositive_unexplained)
  expect_identical(summary$n_zero_uncertainty_census, 1L)
  expect_equal(summary$diag_severity, "note")
  expect_match(summary$diag_notes, "zero_uncertainty_census", fixed = TRUE)
  expect_true(rows$scalar_uncertainty_finite)
  expect_false(rows$scalar_se_positive)
  expect_false(rows$scalar_se_nonpositive_unexplained)
  expect_true(rows$zero_uncertainty_census)
  expect_equal(rows$diag_severity, "note")
  expect_equal(rows$diag_errors[[1L]], character())
  expect_true(matrices$zero_uncertainty_census)
  expect_equal(matrices$diag_severity, "note")
  expect_match(matrices$diag_notes, "zero_uncertainty_census", fixed = TRUE)
})

test_that("non-census zero SE remains an intrinsic diagnostic error", {
  boundary <- sm_estimate_from_counts(
    data.frame(
      site_id = "Z",
      year = 2025L,
      n_jt = 10L,
      c_jt_absent = 0L
    ),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    boundary_method = "none",
    min_n = 2L
  )
  expect_true(validate.sitemix_estimates(boundary))

  summary <- sm_diagnose(boundary, verbose = FALSE)
  rows <- sm_diagnose(boundary, level = "row", verbose = FALSE)
  expect_false(summary$scalar_se_positive)
  expect_true(summary$scalar_se_nonpositive_unexplained)
  expect_identical(summary$n_zero_uncertainty_census, 0L)
  expect_equal(summary$diag_severity, "error")
  expect_match(summary$diag_notes, "scalar_se_nonpositive", fixed = TRUE)
  expect_true(rows$scalar_se_nonpositive_unexplained)
  expect_false(rows$zero_uncertainty_census)
  expect_equal(rows$diag_severity, "error")
})

test_that("stale matching-scale V is an intrinsic diagnostic error", {
  stale <- diagnostics_api_stamp_smoothing(
    diagnostics_api_multivariate_estimate(vst = "none"),
    overwrite = TRUE,
    status = "fit"
  )
  expect_equal(attr(stale, "smoothing")$v$relation, "matching")

  # Ordinary validation runs first and still accepts this legacy/stale object;
  # diagnostics then classifies the intrinsic stale-V fact.
  expect_true(validate.sitemix_estimates(stale))
  summary <- sm_diagnose(stale, verbose = FALSE)
  rows <- sm_diagnose(stale, level = "row", verbose = FALSE)
  matrices <- sm_diagnose(stale, level = "vcov", verbose = FALSE)

  expect_true(summary$smoothing_present)
  expect_true(summary$smoothing_provenance_valid)
  expect_equal(summary$smoothing_v_relation, "matching")
  expect_true(summary$v_stale)
  expect_equal(summary$diag_severity, "error")
  expect_match(summary$diag_notes, "stale_matching_scale_vcov", fixed = TRUE)
  expect_true(all(rows$v_stale))
  expect_true(all(rows$diag_severity == "error"))
  expect_true(all(vapply(
    rows$diag_errors,
    function(error) "sitemix_error_smoothing_v_stale" %in% error,
    logical(1)
  )))
  expect_true(all(matrices$v_stale))
  expect_true(all(matrices$diag_severity == "error"))
})

test_that("skipped smoothing never creates a stale matching-scale V error", {
  expect_warning(
    skipped <- sm_smooth_variance(
      diagnostics_api_multivariate_estimate(vst = "none"),
      min_rows = 50L,
      overwrite = TRUE
    ),
    class = "sitemix_warning_smoother_skipped"
  )
  expect_true(validate.sitemix_estimates(skipped))

  summary <- sm_diagnose(skipped, verbose = FALSE)
  rows <- sm_diagnose(skipped, level = "row", verbose = FALSE)
  matrices <- sm_diagnose(skipped, level = "vcov", verbose = FALSE)
  expect_true(summary$smoothing_provenance_valid)
  expect_equal(summary$smoothing_v_relation, "matching")
  expect_false(summary$v_stale)
  expect_false(grepl("stale_matching_scale_vcov", summary$diag_notes, fixed = TRUE))
  expect_false(any(rows$v_stale))
  expect_false(any(vapply(
    rows$diag_errors,
    function(error) "sitemix_error_smoothing_v_stale" %in% error,
    logical(1)
  )))
  expect_false(any(matrices$v_stale))
  expect_false(any(matrices$diag_severity == "error"))
})

test_that("invalid smoothing provenance is a typed error at every level", {
  invalid <- diagnostics_api_stamp_smoothing(
    diagnostics_api_multivariate_estimate(vst = "none")
  )
  smoothing <- attr(invalid, "smoothing", exact = TRUE)
  smoothing$n_eligible <- 999L
  attr(invalid, "smoothing") <- smoothing
  expect_true(validate.sitemix_estimates(invalid))

  expect_no_error(summary <- sm_diagnose(invalid, verbose = FALSE))
  expect_no_error(rows <- sm_diagnose(invalid, level = "row", verbose = FALSE))
  expect_no_error(matrices <- sm_diagnose(
    invalid,
    level = "vcov",
    verbose = FALSE
  ))

  expect_false(summary$smoothing_provenance_valid)
  expect_equal(summary$smoothing_v_relation, "invalid")
  expect_true(is.na(summary$v_stale))
  expect_equal(summary$diag_severity, "error")
  expect_match(summary$diag_notes, "invalid_smoothing_provenance", fixed = TRUE)

  expect_true(all(!rows$smoothing_provenance_valid))
  expect_true(all(is.na(rows$v_stale)))
  expect_true(all(rows$diag_severity == "error"))
  expect_true(all(grepl(
    "invalid_smoothing_provenance",
    rows$diag_notes,
    fixed = TRUE
  )))

  expect_true(all(!matrices$smoothing_provenance_valid))
  expect_true(all(is.na(matrices$v_stale)))
  expect_true(all(matrices$diag_severity == "error"))
  expect_true(all(grepl(
    "invalid_smoothing_provenance",
    matrices$diag_notes,
    fixed = TRUE
  )))
})

test_that("mixed smoothing-to-V relations are warnings when row scales are coherent", {
  raw <- diagnostics_api_multivariate_estimate(
    site_id = "R",
    indicators = c("a", "b"),
    vst = "none"
  )
  arcsine <- diagnostics_api_multivariate_estimate(
    site_id = "A",
    indicators = c("c", "d"),
    vst = "arcsine"
  )
  mixed <- vctrs::vec_rbind(
    tibble::as_tibble(raw),
    tibble::as_tibble(arcsine)
  )
  attr(mixed, "family") <- "multivariate"
  attr(mixed, "sitemix_role") <- "summary_uncertainty"
  class(mixed) <- c(
    "sitemix_estimates",
    setdiff(class(mixed), "sitemix_estimates")
  )
  expect_true(validate.sitemix_estimates(mixed))

  mixed <- diagnostics_api_stamp_smoothing(mixed)
  expect_equal(attr(mixed, "smoothing")$v$relation, "mixed")

  summary <- sm_diagnose(mixed, verbose = FALSE)
  rows <- sm_diagnose(mixed, level = "row", verbose = FALSE)
  matrices <- sm_diagnose(mixed, level = "vcov", verbose = FALSE)

  expect_equal(summary$estimate_scale, "mixed")
  expect_true(summary$indicator_scale_consistent)
  expect_false(summary$estimate_vcov_scale_compatible)
  expect_true(summary$smoothing_provenance_valid)
  expect_equal(summary$smoothing_v_relation, "mixed")
  expect_false(summary$v_stale)
  expect_equal(summary$diag_severity, "warning")
  expect_match(
    summary$diag_notes,
    "mixed_smoothing_vcov_scale_relation",
    fixed = TRUE
  )
  expect_true(all(rows$diag_severity == "warning"))
  expect_true(all(vapply(
    rows$diag_warnings,
    function(warning) {
      "sitemix_warning_mixed_vcov_scale_relation" %in% warning
    },
    logical(1)
  )))
  expect_true(all(matrices$diag_severity == "warning"))
})

test_that("diagnostics CLI and print headers have stable normalized snapshots", {
  withr::local_options(list(
    cli.width = 200,
    cli.unicode = FALSE,
    cli.num_colors = 1
  ))
  stale <- diagnostics_api_stamp_smoothing(
    diagnostics_api_multivariate_estimate(vst = "none"),
    overwrite = TRUE,
    status = "fit"
  )

  messages <- utils::capture.output(
    invisible(sm_diagnose(stale, verbose = TRUE)),
    type = "message"
  )
  messages <- cli::ansi_strip(messages)
  expect_identical(
    messages,
    c(
      "",
      "-- sitemix_estimates diagnostics --",
      "",
      "i 2 cells | 1 groups | 2 indicators | family = multivariate",
      "i 0 small-n rows | 0 zero-cell rows | 2 below-accountability rows",
      "i Scalar uncertainty: finite=TRUE; SE positive=TRUE; indicator scales consistent=TRUE",
      "i V present: TRUE; valid=TRUE; estimate/V scales compatible=TRUE; PSD repairs: 0",
      "i Suppression: missing=0; sensitivity=0; role=none",
      "i Smoothing: present=TRUE; V relation=matching; stale=TRUE; diagnostic severity=error"
    )
  )

  row_print <- utils::capture.output(print(
    sm_diagnose(stale, level = "row", verbose = FALSE)
  ))
  expect_identical(
    row_print[[1L]],
    "sitemix_diagnostics_row: 2 rows | ok=0 note=0 warning=0 error=2"
  )
})

test_that("valid empty estimate objects return typed diagnostics without warnings", {
  empty <- diagnostics_api_multivariate_estimate(vst = "none")[0, ]
  expect_true(validate.sitemix_estimates(empty))

  expect_no_warning(summary <- sm_diagnose(empty, verbose = FALSE))
  expect_no_warning(rows <- sm_diagnose(empty, level = "row", verbose = FALSE))
  expect_no_warning(matrices <- sm_diagnose(
    empty,
    level = "vcov",
    verbose = FALSE
  ))

  expect_identical(summary$n_cells, 0L)
  expect_true(is.na(summary$min_n))
  expect_true(is.na(summary$median_n))
  expect_true(is.na(summary$max_n))
  expect_true(is.na(summary$estimate_scale))
  expect_true(is.na(summary$v_valid))
  expect_true(is.na(summary$estimate_vcov_scale_compatible))
  expect_equal(summary$diag_severity, "warning")
  expect_match(
    summary$diag_notes,
    "no_identified_scalar_uncertainty",
    fixed = TRUE
  )

  expect_s3_class(rows, "sitemix_diagnostics_row")
  expect_identical(nrow(rows), 0L)
  expect_s3_class(matrices, "sitemix_diagnostics_vcov")
  expect_identical(nrow(matrices), 0L)
  expect_identical(
    names(matrices),
    c(
      "site_id", "year", "family", "K", "indicator_order", "matrix_rank",
      "min_eigenvalue", "psd_tol", "psd_ok", "v_valid", "psd_repair",
      "vcov_method", "vcov_scale", "estimate_scale", "matrix_boundary_rule",
      "scalar_correction_rule", "positive_support", "n_jt", "n_eff",
      "simplex_residual", "row_sum_zero_ok", "repeated_v_equal",
      "zero_uncertainty_census", "estimate_vcov_scale_compatible",
      "smoothing_provenance_valid", "smoothing_v_relation", "v_stale",
      "diag_severity", "diag_notes"
    )
  )
})
