public_binomial_data <- function() {
  data.frame(
    site_id = c("S1", "S1", "S2", "S2"),
    year = c(2024L, 2024L, 2024L, 2024L),
    absent = c(1L, 0L, 0L, 0L),
    stringsAsFactors = FALSE
  )
}

test_that("sm_estimate exports the locked v1.0 public signature", {
  exports <- getNamespaceExports("sitemix")
  expect_true("sm_estimate" %in% exports)
  expect_true("sm_estimate_from_counts" %in% exports)
  expect_true("sm_estimate_from_aggregates" %in% exports)

  v1_prefix <- c(
    "data", "family", "indicator", "indicators", "id_cols",
    "vst", "boundary_method", "bias_correction", "vjt", "min_n",
    "accountability_n", "fpc", "anscombe", "from_counts", "na_action",
    "description"
  )
  aggregate_suffix <- c(
    "from_aggregates", "aggregate_case", "framing", "sampling_relation", "suppression",
    "suppression_col", "suppression_flag_value", "suppression_when",
    "suppressed_theta_hat", "suppression_sensitivity_acknowledge",
    "suppressed_n_strategy", "suppressed_n_bound",
    "numerator_col", "denominator_col", "indicator_col", "subgroup_col"
  )
  expect_equal(names(formals(sitemix::sm_estimate)), c(v1_prefix, aggregate_suffix))
  expect_equal(names(formals(sitemix::sm_estimate))[seq_along(v1_prefix)], v1_prefix)
  expect_equal(eval(formals(sitemix::sm_estimate)$vst), c("arcsine", "logit", "none"))
  expect_equal(eval(formals(sitemix::sm_estimate)$boundary_method), c("wilson_floor", "agresti_coull", "none"))
  expect_equal(eval(formals(sitemix::sm_estimate)$na_action), c("drop_rows", "error"))
  expect_equal(eval(formals(sitemix::sm_estimate)$accountability_n), 30L)
  expect_false(eval(formals(sitemix::sm_estimate)$from_aggregates))
  expect_equal(eval(formals(sitemix::sm_estimate)$aggregate_case), c("auto", "D0", "D1"))
  expect_equal(
    eval(formals(sitemix::sm_estimate)$sampling_relation),
    c("unknown", "same_units", "different_units")
  )

  wrapper_expected <- c("data", "family", "indicator", "indicators", "id_cols",
                        "accountability_n", "...")
  expect_equal(names(formals(sitemix::sm_estimate_from_counts)), wrapper_expected)
  expect_null(eval(formals(sitemix::sm_estimate_from_counts)$indicator))
  expect_null(eval(formals(sitemix::sm_estimate_from_counts)$indicators))
  expect_equal(eval(formals(sitemix::sm_estimate_from_counts)$id_cols), c("site_id", "year"))

  aggregate_wrapper_expected <- c(
    "data", "family", "indicator", "indicators", "id_cols",
    "numerator_col", "denominator_col", "indicator_col", "subgroup_col",
    "aggregate_case", "framing", "sampling_relation", "accountability_n", "suppression",
    "suppression_col", "suppression_flag_value", "suppression_when",
    "suppressed_theta_hat", "suppression_sensitivity_acknowledge",
    "suppressed_n_strategy", "suppressed_n_bound", "..."
  )
  expect_equal(names(formals(sitemix::sm_estimate_from_aggregates)), aggregate_wrapper_expected)
  expect_null(eval(formals(sitemix::sm_estimate_from_aggregates)$indicator))
  expect_equal(eval(formals(sitemix::sm_estimate_from_aggregates)$aggregate_case), c("auto", "D0", "D1"))
  expect_equal(
    eval(formals(sitemix::sm_estimate_from_aggregates)$sampling_relation),
    c("unknown", "same_units", "different_units")
  )
})

test_that("sm_estimate dispatches binomial calls to the Scenario A engine", {
  out <- sitemix::sm_estimate(
    public_binomial_data(),
    family = "binomial",
    indicator = "absent",
    description = "public dispatch"
  )

  expect_s3_class(out, "sitemix_estimates")
  expect_equal(attr(out, "family"), "binomial")
  expect_equal(attr(out, "description"), "public dispatch")
  expect_equal(out$theta_raw, c(0.5, 0))
  expect_equal(out$input_mode, rep("student_level", 2))
  expect_false("V" %in% names(out))
})

test_that("sm_estimate_from_counts is equivalent to direct from_counts dispatch", {
  counts <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    n_jt = c(2L, 2L),
    c_jt_absent = c(1L, 0L)
  )

  direct <- sitemix::sm_estimate(
    counts,
    family = "binomial",
    indicator = "absent",
    from_counts = TRUE,
    vjt = TRUE
  )
  wrapper <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    vjt = TRUE
  )

  expect_equal(as.data.frame(direct), as.data.frame(wrapper), ignore_attr = TRUE)
  expect_equal(wrapper$input_mode, rep("counts_full_suff", 2))
  expect_true("V" %in% names(wrapper))
  expect_error(
    sitemix::sm_estimate_from_counts(
      counts,
      family = "binomial",
      indicator = "absent",
      from_counts = FALSE
    ),
    class = "sitemix_error_invalid_from_counts"
  )
  expect_error(
    sitemix::sm_estimate_from_counts(
      counts,
      family = "binomial",
      indicator = "absent",
      from_aggregates = TRUE
    ),
    class = "sitemix_error_input_path_conflict"
  )
})

test_that("sm_estimate_from_aggregates is equivalent to direct aggregate dispatch", {
  aggregate <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    indicator = c("absent", "absent"),
    c_jt = c(1L, 0L),
    n_jt = c(2L, 2L),
    stringsAsFactors = FALSE
  )

  direct <- sitemix::sm_estimate(
    aggregate,
    family = "binomial",
    indicator = "absent",
    from_aggregates = TRUE,
    vjt = TRUE
  )
  wrapper <- sitemix::sm_estimate_from_aggregates(
    aggregate,
    family = "binomial",
    indicator = "absent",
    vjt = TRUE
  )

  expect_equal(as.data.frame(direct), as.data.frame(wrapper), ignore_attr = TRUE)
  expect_equal(wrapper$input_mode, rep("aggregate", 2))
  expect_equal(attr(wrapper, "aggregate_case"), "D0")
  expect_true("V" %in% names(wrapper))
  expect_error(
    sitemix::sm_estimate_from_aggregates(
      aggregate,
      family = "binomial",
      indicator = "absent",
      from_aggregates = FALSE
    ),
    class = "sitemix_error_invalid_from_aggregates"
  )
  expect_error(
    sitemix::sm_estimate_from_aggregates(
      aggregate,
      family = "binomial",
      indicator = "absent",
      from_counts = TRUE
    ),
    class = "sitemix_error_input_path_conflict"
  )
  expect_error(
    sitemix::sm_estimate(
      aggregate,
      family = "binomial",
      indicator = "absent",
      from_counts = TRUE,
      from_aggregates = TRUE
    ),
    class = "sitemix_error_input_path_conflict"
  )
})

test_that("public dispatch preserves exact argument classes", {
  expect_error(
    sitemix::sm_estimate(public_binomial_data(), family = "D0", indicator = "absent"),
    class = "sitemix_error_invalid_family"
  )
  expect_error(
    sitemix::sm_estimate(public_binomial_data(), family = "binomial", indicator = "absent", vst = "identity"),
    class = "sitemix_error_invalid_vst"
  )
  expect_error(
    sitemix::sm_estimate(public_binomial_data(), family = "binomial", indicator = "absent", na_action = "omit"),
    class = "sitemix_error_invalid_na_action"
  )
  expect_error(
    sitemix::sm_estimate(public_binomial_data(), family = "binomial", indicator = "absent", vjt = c(TRUE, FALSE)),
    class = "sitemix_error_invalid_vjt"
  )
})
