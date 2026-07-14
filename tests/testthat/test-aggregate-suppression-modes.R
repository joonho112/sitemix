aggregate_suppression_rows <- function() {
  data.frame(
    site_id = c("S1", "S2", "S3"),
    year = c(2025L, 2025L, 2025L),
    indicator = c("absent", "absent", "absent"),
    c_jt = c(NA_integer_, 0L, 7L),
    n_jt = c(8L, 10L, 40L),
    stringsAsFactors = FALSE
  )
}

test_that("suppression drop retains descriptive suppressed rows", {
  out <- sitemix::sm_estimate_from_aggregates(
    aggregate_suppression_rows(),
    family = "binomial",
    indicator = "absent",
    suppression = "drop",
    min_n = 10L,
    accountability_n = 30L
  )

  expect_s3_class(out, "sitemix_estimates")
  expect_equal(
    names(out),
    c(
      sitemix:::.sm_sitemix_columns,
      "estimate_status", "sensitivity_probability", "sensitivity_var_raw",
      "sensitivity_var", "sensitivity_n", "sensitivity_method",
      "sensitivity_acknowledged"
    )
  )
  expect_true(validate.sitemix_estimates(out))

  suppressed <- out[out$site_id == "S1", ]
  expect_true(suppressed$flag_suppressed)
  expect_equal(suppressed$var_method, "suppressed_drop")
  expect_true(all(is.na(unlist(suppressed[c("theta_raw", "theta_hat", "se_raw", "se")]))))
  expect_true(is.na(suppressed$flag_zero_cell))
  expect_true(suppressed$flag_small_n)
  expect_true(suppressed$flag_below_accountability)

  boundary <- out[out$site_id == "S2", ]
  expect_false(boundary$flag_suppressed)
  expect_false(boundary$flag_small_n)
  expect_true(boundary$flag_zero_cell)
  expect_true(boundary$flag_below_accountability)
})

test_that("CA-style mixed Tier 1/2/3 subgroup aggregate mini-pipeline is stable", {
  ca_like <- data.frame(
    cds = rep("S1", 3L),
    reportingyear = rep(2025L, 3L),
    studentgroup = c("ALL", "FOS", "PI"),
    currnumer = c(342L, NA_integer_, 1L),
    currdenom = c(3388L, 8L, 23L),
    small_cell = c("", "Y", ""),
    stringsAsFactors = FALSE
  )
  pivoted <- sitemix::sm_pivot_subgroups_to_sites(
    ca_like,
    site_col = "cds",
    year_col = "reportingyear",
    subgroup_col = "studentgroup",
    numerator_col = "currnumer",
    denominator_col = "currdenom",
    indicator = "chronic_absence",
    suppression_col = "small_cell",
    suppression_flag_value = "Y"
  )
  report <- sitemix::sm_suppression_report(
    pivoted,
    by = NULL,
    suppression_flag_value = TRUE
  )
  out <- sitemix::sm_estimate_from_aggregates(
    pivoted,
    family = "binomial",
    indicator = "chronic_absence",
    aggregate_case = "D0",
    framing = "subgroup_as_site",
    suppression = "drop",
    min_n = 10L,
    accountability_n = 30L
  )

  expect_equal(attr(pivoted, "partition_target"), "none")
  expect_equal(report$n_tier1, 1L)
  expect_equal(report$n_tier2, 1L)
  expect_equal(report$n_tier3, 1L)
  expect_true(report$denominator_observed_on_suppressed)
  expect_equal(report$recommended_action, "drop_or_acknowledge_variance_sensitivity")
  expect_equal(out$site_id, c("S1_ALL", "S1_FOS", "S1_PI"))
  expect_equal(out$framing, rep("subgroup_as_site", 3L))
  expect_equal(out$input_mode, rep("aggregate", 3L))
  expect_equal(attr(out, "suppression")$n_suppressed, 1L)

  tier1 <- out[out$site_id == "S1_FOS", ]
  expect_true(tier1$flag_suppressed)
  expect_true(tier1$flag_small_n)
  expect_true(tier1$flag_below_accountability)
  expect_equal(tier1$var_method, "suppressed_drop")
  expect_true(all(is.na(unlist(tier1[c("theta_raw", "theta_hat", "se_raw", "se")]))))

  tier2 <- out[out$site_id == "S1_PI", ]
  expect_false(tier2$flag_suppressed)
  expect_false(tier2$flag_small_n)
  expect_true(tier2$flag_below_accountability)
  expect_equal(tier2$theta_raw, 1 / 23, tolerance = 1e-12)
  expect_true(is.finite(tier2$se))

  tier3 <- out[out$site_id == "S1_ALL", ]
  expect_false(tier3$flag_suppressed)
  expect_false(tier3$flag_small_n)
  expect_false(tier3$flag_below_accountability)
  expect_equal(tier3$theta_raw, 342 / 3388, tolerance = 1e-12)
  expect_true(validate.sitemix_estimates(out))
})

test_that("suppression upper_bound separates observed-denominator variance sensitivity", {
  out <- sitemix::sm_estimate_from_aggregates(
    aggregate_suppression_rows(),
    family = "binomial",
    indicator = "absent",
    suppression = "upper_bound",
    suppressed_theta_hat = 0.5,
    suppression_sensitivity_acknowledge = TRUE,
    min_n = 10L
  )

  suppressed <- out[out$site_id == "S1", ]
  expect_true(suppressed$flag_suppressed)
  expect_equal(suppressed$var_method, "suppression_sensitivity")
  expect_equal(suppressed$estimate_status, "suppression_sensitivity")
  expect_true(all(is.na(unlist(suppressed[c("theta_raw", "theta_hat", "se_raw", "se")]))))
  expect_equal(suppressed$sensitivity_probability, 0.5)
  expect_equal(suppressed$sensitivity_var_raw, 0.25 / 8)
  expect_equal(suppressed$sensitivity_var, 0.25 / 8)
  expect_true(is.na(suppressed$flag_zero_cell))
  expect_true(validate.sitemix_estimates(out))
})

test_that("hidden denominator upper_bound records no numeric variance claim", {
  hidden <- data.frame(
    site_id = "S1",
    year = 2025L,
    indicator = "absent",
    c_jt = NA_integer_,
    n_jt = NA_integer_,
    suppression_flag = TRUE,
    stringsAsFactors = FALSE
  )

  out <- sitemix::sm_estimate_from_aggregates(
    hidden,
    family = "binomial",
    indicator = "absent",
    suppression = "upper_bound",
    suppression_sensitivity_acknowledge = TRUE,
    suppressed_n_strategy = "worst_case_bound",
    suppressed_n_bound = 5L,
    min_n = 10L
  )

  expect_equal(out$n, 5L)
  expect_equal(out$n_eff, 5)
  expect_true(is.na(out$se))
  expect_true(is.na(out$sensitivity_n))
  expect_true(is.na(out$sensitivity_var_raw))
  expect_true(is.na(out$sensitivity_var))
  expect_equal(out$sensitivity_method, "unquantified_hidden_denominator")
  expect_equal(attr(out, "suppression")$has_hidden_denominator, TRUE)
  expect_equal(attr(out, "suppression")$suppressed_n_bound, 5L)

  expect_error(
    sitemix::sm_estimate_from_aggregates(
      hidden,
      family = "binomial",
      indicator = "absent",
      suppression = "upper_bound",
      suppression_sensitivity_acknowledge = TRUE,
      suppressed_n_strategy = "worst_case_bound",
      suppressed_n_bound = 11L,
      min_n = 10L
    ),
    class = "sitemix_error_invalid_suppressed_n"
  )
})

test_that("suppressed_theta_hat is validated before upper-bound estimation", {
  x <- aggregate_suppression_rows()
  for (bad in list(0, 1, NA_real_, "0.5")) {
    expect_error(
      sitemix::sm_estimate_from_aggregates(
        x,
        family = "binomial",
        indicator = "absent",
        suppression = "upper_bound",
        suppressed_theta_hat = bad
      ),
      class = "sitemix_error_invalid_suppressed_theta_hat"
    )
  }
})

test_that("suppression drop and upper_bound sensitivity both reject ordinary V", {
  x <- aggregate_suppression_rows()
  expect_error(
    sitemix::sm_estimate_from_aggregates(
      x,
      family = "binomial",
      indicator = "absent",
      suppression = "drop",
      vjt = TRUE
    ),
    class = "sitemix_error_estimate_vcov_invariant"
  )

  expect_error(
    sitemix::sm_estimate_from_aggregates(
      x,
      family = "binomial",
      indicator = "absent",
      suppression = "upper_bound",
      suppression_sensitivity_acknowledge = TRUE,
      vjt = TRUE
    ),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})

test_that("direct-column consumers explicitly filter suppressed-drop rows", {
  out <- sitemix::sm_estimate_from_aggregates(
    aggregate_suppression_rows(),
    family = "binomial",
    indicator = "absent",
    suppression = "drop"
  )

  expect_equal(nrow(out), 3L)
  usable <- out[!out$flag_suppressed, c("site_id", "year", "indicator", "theta_hat", "se", "n")]
  expect_equal(usable$site_id, c("S2", "S3"))
  expect_true(all(is.finite(usable$theta_hat)))
  expect_true(all(usable$se > 0))
})

test_that("upper-bound sensitivity rows remain explicit in direct columns", {
  out <- sitemix::sm_estimate_from_aggregates(
    aggregate_suppression_rows(),
    family = "binomial",
    indicator = "absent",
    suppression = "upper_bound",
    suppression_sensitivity_acknowledge = TRUE
  )

  expect_equal(nrow(out), 3L)
  expect_true(out$flag_suppressed[[1]])
  expect_equal(out$var_method[[1]], "suppression_sensitivity")
  expect_equal(out$estimate_status[[1]], "suppression_sensitivity")
  expect_true(is.na(out$theta_hat[[1]]))
  expect_true(is.na(out$se[[1]]))
  expect_equal(out$sensitivity_var[[1]], 0.25 / 8)
})

test_that("all suppressed-drop outputs report no scalar uncertainty", {
  all_suppressed <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2025L, 2025L),
    indicator = c("absent", "absent"),
    c_jt = c(NA_integer_, NA_integer_),
    n_jt = c(8L, 9L),
    stringsAsFactors = FALSE
  )
  out <- sitemix::sm_estimate_from_aggregates(
    all_suppressed,
    family = "binomial",
    indicator = "absent",
    suppression = "drop"
  )

  diag <- sitemix::sm_diagnose(out, verbose = FALSE)
  expect_false(diag$scalar_uncertainty_finite)
  expect_false(diag$scalar_se_positive)
  expect_false(diag$v_present)
})
