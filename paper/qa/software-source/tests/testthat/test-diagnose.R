diagnose_counts <- function() {
  data.frame(
    site_id = c("S1", "S2", "S3"),
    year = c(2024L, 2024L, 2024L),
    n_jt = c(5L, 40L, 40L),
    c_jt_absent = c(2L, 0L, 20L)
  )
}

diagnose_multivariate_counts <- function() {
  data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    n_jt = c(10L, 12L),
    c_jt_snap = c(4L, 6L),
    c_jt_frpm = c(7L, 8L),
    c_jt_snap_frpm = c(3L, 5L)
  )
}

test_that("sm_diagnose summary reports intrinsic uncertainty facts", {
  out <- sitemix::sm_estimate_from_counts(
    diagnose_counts(),
    family = "binomial",
    indicator = "absent",
    min_n = 10L,
    accountability_n = 30L
  )
  diag <- sitemix::sm_diagnose(out, verbose = FALSE)

  expect_s3_class(diag, "sitemix_diagnostics_summary")
  expect_s3_class(diag, "tbl_df")
  expect_equal(nrow(diag), 1L)
  expect_equal(diag$family, "binomial")
  expect_equal(diag$sitemix_role, "summary_uncertainty")
  expect_equal(diag$n_cells, 3L)
  expect_equal(diag$n_groups, 3L)
  expect_equal(diag$n_sites, 3L)
  expect_equal(diag$n_years, 1L)
  expect_equal(diag$n_indicators, 1L)
  expect_equal(diag$n_flag_small_n, 1L)
  expect_equal(diag$n_flag_zero_cell, 1L)
  expect_equal(diag$n_flag_both, 0L)
  expect_equal(diag$n_flag_below_accountability, 1L)
  expect_equal(diag$min_n, 5L)
  expect_equal(diag$median_n, 40)
  expect_equal(diag$max_n, 40L)
  expect_equal(diag$estimate_scale, "arcsine")
  expect_false(diag$v_present)
  expect_false(diag$k_present)
  expect_true(is.na(diag$n_psd_repair_fired))
  expect_true(diag$scalar_uncertainty_finite)
  expect_true(diag$scalar_se_positive)
  expect_true(diag$indicator_scale_consistent)
  expect_true(is.na(diag$v_valid))
  expect_true(is.na(diag$estimate_vcov_scale_compatible))
  expect_equal(diag$n_var_method_arcsine_vst, 3L)
})

test_that("sm_diagnose row level preserves rows and adds diagnostic columns", {
  out <- sitemix::sm_estimate_from_counts(
    diagnose_counts(),
    family = "binomial",
    indicator = "absent",
    min_n = 10L,
    accountability_n = 30L
  )
  diag <- sitemix::sm_diagnose(out, level = "row", verbose = FALSE)

  expect_s3_class(diag, "sitemix_diagnostics_row")
  expect_false(inherits(diag, "sitemix_estimates"))
  expect_equal(nrow(diag), nrow(out))
  expect_true(all(c("diag_severity", "diag_warnings", "diag_errors", "diag_notes") %in% names(diag)))
  expect_equal(diag$diag_severity, c("note", "note", "ok"))
  expect_match(diag$diag_notes[[1]], "below_accountability", fixed = TRUE)
  expect_match(diag$diag_notes[[1]], "small_n", fixed = TRUE)
  expect_match(diag$diag_notes[[2]], "zero_cell", fixed = TRUE)
  expect_equal(diag$diag_warnings[[3]], character())
  expect_equal(diag$diag_errors[[3]], character())
})

test_that("sm_diagnose reports V presence without consumer-specific warnings", {
  out <- sitemix::sm_estimate_from_counts(
    diagnose_multivariate_counts(),
    family = "multivariate",
    indicators = c("snap", "frpm"),
    min_n = 2L,
    vjt = TRUE
  )
  summary <- sitemix::sm_diagnose(out, verbose = FALSE)
  expect_true(summary$v_present)
  expect_equal(summary$n_psd_repair_fired, 0L)
  expect_true(summary$scalar_uncertainty_finite)
  expect_true(summary$scalar_se_positive)
  expect_true(summary$indicator_scale_consistent)
  expect_true(summary$v_valid)
  expect_false(summary$estimate_vcov_scale_compatible)

  rows <- sitemix::sm_diagnose(out, level = "row", verbose = FALSE)
  expect_true(all(vapply(
    rows$diag_warnings,
    function(x) "sitemix_warning_estimate_vcov_scale_mismatch" %in% x,
    logical(1)
  )))
  expect_true(all(rows$diag_severity == "warning"))
  expect_false(any(grepl("adapter", rows$diag_notes, fixed = TRUE)))
})

test_that("sm_diagnose treats suppressed_drop rows as warnings, not column errors", {
  out <- sitemix::sm_estimate_from_aggregates(
    data.frame(
      site_id = c("S1", "S2"),
      year = c(2025L, 2025L),
      indicator = c("absent", "absent"),
      c_jt = c(NA_integer_, 4L),
      n_jt = c(8L, 20L),
      stringsAsFactors = FALSE
    ),
    family = "binomial",
    indicator = "absent",
    suppression = "drop",
    min_n = 10L
  )

  summary <- sitemix::sm_diagnose(out, verbose = FALSE)
  expect_true(summary$scalar_uncertainty_finite)
  expect_true(summary$scalar_se_positive)
  expect_equal(summary$n_flag_suppressed, 1L)
  expect_equal(summary$n_var_method_suppressed_drop, 1L)

  rows <- sitemix::sm_diagnose(out, level = "row", verbose = FALSE)
  expect_equal(rows$diag_severity[[1]], "warning")
  expect_true("sitemix_warning_suppression_dropped" %in% rows$diag_warnings[[1]])
  expect_false("sitemix_error_estimate_var_method" %in% rows$diag_errors[[1]])
  expect_match(rows$diag_notes[[1]], "suppressed", fixed = TRUE)
})

test_that("sm_diagnose treats role as metadata and flags mixed transforms", {
  raw <- sitemix::sm_estimate_from_counts(
    diagnose_counts()[1, ],
    family = "binomial",
    indicator = "absent",
    vst = "none",
    min_n = 2L
  )
  arcsine <- sitemix::sm_estimate_from_counts(
    transform(diagnose_counts()[3, ], site_id = "S4"),
    family = "binomial",
    indicator = "absent",
    min_n = 2L
  )
  mixed <- vctrs::vec_rbind(raw, arcsine)
  attr(mixed, "family") <- "binomial"
  attr(mixed, "description") <- NULL
  attr(mixed, "sitemix_role") <- "descriptive"
  class(mixed) <- c("sitemix_estimates", setdiff(class(mixed), "sitemix_estimates"))

  summary <- sitemix::sm_diagnose(mixed, verbose = FALSE)
  expect_true(summary$scalar_uncertainty_finite)
  expect_true(summary$scalar_se_positive)
  expect_false(summary$indicator_scale_consistent)
  expect_equal(summary$estimate_scale, "mixed")

  rows <- sitemix::sm_diagnose(mixed, level = "row", verbose = FALSE)
  expect_true(all(rows$diag_severity == "error"))
  expect_true(all(vapply(rows$diag_errors, function(x) "sitemix_error_estimate_var_method" %in% x, logical(1))))
  expect_true(all(grepl("mixed_estimate_scale_within_indicator", rows$diag_notes, fixed = TRUE)))
})

test_that("diagnose separates V absence from scale compatibility", {
  no_v <- sitemix::sm_estimate_from_counts(
    diagnose_multivariate_counts(),
    family = "multivariate",
    indicators = c("snap", "frpm"),
    min_n = 2L,
    vjt = FALSE
  )
  no_v_diag <- sitemix::sm_diagnose(no_v, verbose = FALSE)
  expect_false(no_v_diag$v_present)
  expect_true(is.na(no_v_diag$v_valid))
  expect_true(is.na(no_v_diag$estimate_vcov_scale_compatible))

  incompatible <- sitemix::sm_estimate_from_counts(
    diagnose_multivariate_counts(),
    family = "multivariate",
    indicators = c("snap", "frpm"),
    min_n = 2L,
    vjt = TRUE
  )
  incompatible_diag <- sitemix::sm_diagnose(incompatible, verbose = FALSE)
  expect_true(incompatible_diag$v_present)
  expect_true(incompatible_diag$v_valid)
  expect_false(incompatible_diag$estimate_vcov_scale_compatible)

  compatible <- sitemix::sm_estimate_from_counts(
    diagnose_multivariate_counts(),
    family = "multivariate",
    indicators = c("snap", "frpm"),
    min_n = 2L,
    vjt = TRUE,
    vst = "none"
  )
  compatible_diag <- sitemix::sm_diagnose(compatible, verbose = FALSE)
  expect_true(compatible_diag$v_present)
  expect_true(compatible_diag$v_valid)
  expect_true(compatible_diag$estimate_vcov_scale_compatible)
})

test_that("row diagnostics do not overflag different scales across indicators", {
  raw <- sitemix::sm_estimate_from_counts(
    data.frame(site_id = "S1", year = 2024L, n_jt = 5L, c_jt_raw_absent = 2L),
    family = "binomial",
    indicator = "raw_absent",
    vst = "none",
    min_n = 2L
  )
  arcsine <- sitemix::sm_estimate_from_counts(
    data.frame(site_id = "S4", year = 2024L, n_jt = 40L, c_jt_arc_absent = 20L),
    family = "binomial",
    indicator = "arc_absent",
    min_n = 2L
  )
  mixed <- vctrs::vec_rbind(raw, arcsine)
  attr(mixed, "family") <- "binomial"
  attr(mixed, "description") <- NULL
  attr(mixed, "sitemix_role") <- "summary_uncertainty"
  class(mixed) <- c("sitemix_estimates", setdiff(class(mixed), "sitemix_estimates"))

  summary <- sitemix::sm_diagnose(mixed, verbose = FALSE)
  rows <- sitemix::sm_diagnose(mixed, level = "row", verbose = FALSE)

  expect_equal(summary$estimate_scale, "mixed")
  expect_true(summary$indicator_scale_consistent)
  expect_false(any(vapply(rows$diag_errors, function(x) "sitemix_error_estimate_var_method" %in% x, logical(1))))
})

test_that("sm_diagnose validates class, level, and verbose arguments", {
  out <- sitemix::sm_estimate_from_counts(
    diagnose_counts(),
    family = "binomial",
    indicator = "absent",
    min_n = 10L
  )

  expect_error(
    sitemix::sm_diagnose(tibble::as_tibble(out), verbose = FALSE),
    class = "sitemix_error_input_class"
  )
  expect_error(
    sitemix::sm_diagnose(out, level = "all", verbose = FALSE),
    class = "sitemix_error_invalid_diagnose_level"
  )
  expect_error(
    sitemix::sm_diagnose(out, verbose = c(TRUE, FALSE)),
    class = "sitemix_error_invalid_verbose"
  )
  expect_error(
    sitemix::sm_diagnose(out, level = "vcov", verbose = FALSE),
    class = "sitemix_error_diagnose_vcov_missing"
  )

  report_condition <- expect_error(
    sitemix::sm_suppression_report(
      diagnose_counts(),
      by = c("year", "year")
    ),
    class = "sitemix_error_invalid_id_cols"
  )
  expect_identical(report_condition$expected, "NULL or distinct column names")
})

test_that("sm_diagnose vcov level audits multivariate matrices by group", {
  out <- sitemix::sm_estimate_from_counts(
    diagnose_multivariate_counts(),
    family = "multivariate",
    indicators = c("snap", "frpm"),
    min_n = 2L,
    vjt = TRUE
  )
  diag <- sitemix::sm_diagnose(out, level = "vcov", verbose = FALSE)

  expect_s3_class(diag, "sitemix_diagnostics_vcov")
  expect_s3_class(diag, "tbl_df")
  expect_equal(nrow(diag), 2L)
  expect_true(all(c(
    "site_id", "year", "family", "K", "indicator_order", "matrix_rank",
    "min_eigenvalue", "psd_tol", "psd_ok", "psd_repair", "vcov_method",
    "vcov_scale", "estimate_scale", "matrix_boundary_rule", "positive_support",
    "n_jt", "n_eff", "simplex_residual", "row_sum_zero_ok", "repeated_v_equal"
  ) %in% names(diag)))
  expect_equal(diag$family, c("multivariate", "multivariate"))
  expect_equal(diag$K, c(2L, 2L))
  expect_equal(diag$indicator_order[[1]], c("snap", "frpm"))
  expect_equal(diag$vcov_method, c("sur", "sur"))
  expect_equal(diag$vcov_scale, c("raw", "raw"))
  expect_true(all(diag$psd_ok))
  expect_true(all(diag$repeated_v_equal))
  expect_true(all(is.na(diag$simplex_residual)))
  expect_true(all(is.na(diag$row_sum_zero_ok)))
})

test_that("sm_diagnose vcov level audits binomial and multinomial specifics", {
  binomial <- sitemix::sm_estimate_from_counts(
    diagnose_counts(),
    family = "binomial",
    indicator = "absent",
    min_n = 2L,
    vjt = TRUE
  )
  bin_diag <- sitemix::sm_diagnose(binomial, level = "vcov", verbose = FALSE)
  expect_equal(nrow(bin_diag), 3L)
  expect_equal(bin_diag$K, rep(1L, 3))
  expect_true(all(is.na(bin_diag$vcov_method)))
  expect_true(all(is.na(bin_diag$simplex_residual)))
  expect_true(all(is.na(bin_diag$row_sum_zero_ok)))

  multinomial <- sitemix::sm_estimate_from_counts(
    data.frame(
      site_id = "S1",
      year = 2024L,
      n_jt = 10L,
      c_jt_eng = 4L,
      c_jt_spa = 3L,
      c_jt_oth = 3L
    ),
    family = "multinomial",
    indicators = c("eng", "spa", "oth"),
    min_n = 2L,
    vjt = TRUE
  )
  multi_diag <- sitemix::sm_diagnose(multinomial, level = "vcov", verbose = FALSE)
  expect_equal(nrow(multi_diag), 1L)
  expect_equal(multi_diag$family, "multinomial")
  expect_equal(multi_diag$K, 3L)
  expect_lte(multi_diag$simplex_residual, 1e-10)
  expect_true(multi_diag$row_sum_zero_ok)
})

test_that("sm_diagnose vcov level keeps validation strict for corrupted repeated V", {
  out <- sitemix::sm_estimate_from_counts(
    diagnose_multivariate_counts()[1, ],
    family = "multivariate",
    indicators = c("snap", "frpm"),
    min_n = 2L,
    vjt = TRUE
  )
  out$V[[2]]$matrix[1, 1] <- out$V[[2]]$matrix[1, 1] + 0.001
  expect_error(
    sitemix::sm_diagnose(out, level = "vcov", verbose = FALSE),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})

test_that("diagnostic object print methods expose compact headers", {
  out <- sitemix::sm_estimate_from_counts(
    diagnose_multivariate_counts(),
    family = "multivariate",
    indicators = c("snap", "frpm"),
    min_n = 2L,
    vjt = TRUE
  )

  summary <- sitemix::sm_diagnose(out, verbose = FALSE)
  rows <- sitemix::sm_diagnose(out, level = "row", verbose = FALSE)
  matrices <- sitemix::sm_diagnose(out, level = "vcov", verbose = FALSE)

  expect_identical(
    format(summary),
    paste0("<sitemix_diagnostics_summary[1 x ", ncol(summary), "]>")
  )
  expect_identical(
    format(rows),
    paste0("<sitemix_diagnostics_row[", nrow(rows), " x ", ncol(rows), "]>")
  )
  expect_identical(
    format(matrices),
    paste0("<sitemix_diagnostics_vcov[", nrow(matrices), " x ", ncol(matrices), "]>")
  )

  summary_print <- utils::capture.output(print(summary))
  expect_match(summary_print[[1]], "sitemix_diagnostics_summary", fixed = TRUE)
  expect_match(summary_print[[1]], "scalar finite", fixed = TRUE)

  row_print <- utils::capture.output(print(rows))
  expect_match(row_print[[1]], "sitemix_diagnostics_row", fixed = TRUE)
  expect_match(row_print[[1]], "warning=", fixed = TRUE)

  vcov_print <- utils::capture.output(print(matrices))
  expect_match(vcov_print[[1]], "sitemix_diagnostics_vcov", fixed = TRUE)
  expect_match(vcov_print[[1]], "PSD", fixed = TRUE)
  expect_match(vcov_print[[1]], "scale", fixed = TRUE)
})

test_that("sm_diagnose verbose output is optional", {
  out <- sitemix::sm_estimate_from_counts(
    diagnose_counts(),
    family = "binomial",
    indicator = "absent",
    min_n = 10L
  )

  silent <- testthat::capture_output(
    diag <- sitemix::sm_diagnose(out, verbose = FALSE)
  )
  expect_equal(silent, "")
  expect_s3_class(diag, "sitemix_diagnostics_summary")

  noisy <- utils::capture.output(
    invisible(sitemix::sm_diagnose(out, verbose = TRUE)),
    type = "message"
  )
  noisy <- paste(noisy, collapse = "\n")
  expect_match(noisy, "sitemix_estimates diagnostics", fixed = TRUE)
  expect_match(noisy, "Scalar uncertainty", fixed = TRUE)
})
