aggregate_d1_long <- function() {
  data.frame(
    site_id = c("S1", "S1", "S2", "S2"),
    year = c(2025L, 2025L, 2025L, 2025L),
    indicator = c("absent", "suspend", "absent", "suspend"),
    c_jt = c(10L, 2L, 15L, 0L),
    n_jt = c(100L, 100L, 80L, 80L),
    stringsAsFactors = FALSE
  )
}

quiet_working_independence <- function(expr) {
  withCallingHandlers(
    expr,
    sitemix_warning_working_independence_default = function(w) invokeRestart("muffleWarning")
  )
}

test_that("aggregate D1 long input emits multivariate marginal rows", {
  warning <- NULL
  out <- withCallingHandlers(
    sitemix::sm_estimate_from_aggregates(
      aggregate_d1_long(),
      family = "multivariate",
      min_n = 1L
    ),
    sitemix_warning_working_independence_default = function(w) {
      warning <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_true(
    is.null(warning) ||
      inherits(warning, "sitemix_warning_working_independence_default")
  )

  expect_s3_class(out, "sitemix_estimates")
  expect_equal(nrow(out), 4L)
  expect_equal(out$input_mode, rep("aggregate", 4L))
  expect_equal(attr(out, "family"), "multivariate")
  expect_equal(attr(out, "aggregate_case"), "D1")
  expect_equal(attr(out, "sampling_relation"), "unknown")
  expect_equal(attr(out, "denominator_pattern"), "common")
  expect_equal(attr(out, "d1_regime"), "unknown")
  expect_equal(attr(out, "d1_regime_by_group")$d1_regime, rep("unknown", 2L))
  expect_false("V" %in% names(out))
  expect_false(any(out$var_method == "working_independence"))
  expect_equal(out$theta_raw, c(0.10, 0.02, 0.1875, 0))
  expect_true(validate.sitemix_estimates(out))
})

test_that("aggregate D1 wide D1a emits repeated working-independence V", {
  wide <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2025L, 2025L),
    c_jt_snap = c(12L, 8L),
    c_jt_frpm = c(20L, 30L),
    n_jt = c(100L, 90L)
  )

  out <- quiet_working_independence(
    sitemix::sm_estimate_from_aggregates(
      wide,
      family = "multivariate",
      sampling_relation = "same_units",
      vjt = TRUE,
      min_n = 1L
    )
  )

  expect_equal(attr(out, "aggregate_case"), "D1")
  expect_equal(attr(out, "sampling_relation"), "same_units")
  expect_equal(attr(out, "denominator_pattern"), "common")
  expect_equal(attr(out, "d1_regime"), "D1a")
  expect_true("V" %in% names(out))
  expect_true("K" %in% names(out))
  expect_equal(out$K, rep(2L, 4L))

  group <- out[out$site_id == "S1", ]
  V <- group$V[[1]]
  mat <- as.matrix(V)
  expect_s3_class(V, "sm_vcov")
  expect_equal(V$vcov_method, "working_independence")
  expect_equal(V$family, "multivariate")
  expect_equal(V$vcov_scale, "arcsine_delta")
  expect_equal(V$n_jt, 100L)
  expect_equal(V$n_eff, 100)
  expect_equal(V$indicator_order, group$indicator)
  expect_equal(unname(diag(mat)), group$se^2, tolerance = 1e-12)
  expect_true(all(mat[row(mat) != col(mat)] == 0))
  expect_true(sitemix:::.sm_vcov_value_equal(group$V[[1]], group$V[[2]]))
  expect_true(validate.sitemix_estimates(out))
})

test_that("aggregate D1 wide D1a handles K >= 3 working-independence V", {
  wide <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2025L, 2025L),
    c_jt_a = c(10L, 4L),
    c_jt_b = c(20L, 5L),
    c_jt_c = c(30L, 6L),
    n_jt = c(100L, 50L)
  )

  out <- quiet_working_independence(
    sitemix::sm_estimate_from_aggregates(
      wide,
      family = "multivariate",
      sampling_relation = "same_units",
      vjt = TRUE,
      min_n = 1L
    )
  )

  expect_equal(nrow(out), 6L)
  expect_equal(out$K, rep(3L, 6L))
  expect_equal(attr(out, "d1_regime"), "D1a")
  group <- out[out$site_id == "S1", ]
  V <- group$V[[1]]
  mat <- as.matrix(V)
  expect_equal(dim(mat), c(3L, 3L))
  expect_equal(V$indicator_order, c("a", "b", "c"))
  expect_equal(V$matrix_rank, 3L)
  expect_equal(unname(diag(mat)), group$se^2, tolerance = 1e-12)
  expect_true(all(mat[row(mat) != col(mat)] == 0))
  expect_true(sitemix:::.sm_vcov_value_equal(group$V[[1]], group$V[[2]]))
  expect_true(sitemix:::.sm_vcov_value_equal(group$V[[2]], group$V[[3]]))
})

test_that("aggregate D1 wide D1b records heterogeneous-denominator metadata", {
  per_indicator <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2025L, 2025L),
    c_jt_AI = c(2L, 1L),
    c_jt_HI = c(15L, 18L),
    n_jt_AI = c(10L, 8L),
    n_jt_HI = c(60L, 70L)
  )

  out <- quiet_working_independence(
    sitemix::sm_estimate_from_aggregates(
      per_indicator,
      family = "multivariate",
      sampling_relation = "different_units",
      vjt = TRUE,
      min_n = 1L
    )
  )

  expect_equal(attr(out, "sampling_relation"), "different_units")
  expect_equal(attr(out, "denominator_pattern"), "varying")
  expect_equal(attr(out, "d1_regime"), "D1b")
  expect_equal(attr(out, "d1_regime_by_group")$denominator_pattern, rep("varying", 2L))
  expect_equal(out$V[[1]]$n_jt, NA_integer_)
  expect_equal(out$V[[1]]$n_eff, NA_real_)
  expect_equal(out$n[out$site_id == "S1"], c(10L, 60L))
  expect_equal(out$V[[1]]$indicator_order, out$indicator[out$site_id == "S1"])
})

test_that("aggregate D1 V follows the declared estimate scale", {
  wide <- data.frame(
    site_id = "S1",
    year = 2025L,
    c_jt_a = 2L,
    c_jt_b = 5L,
    n_jt = 10L
  )

  raw <- quiet_working_independence(
    sitemix::sm_estimate_from_aggregates(
      wide,
      family = "multivariate",
      vjt = TRUE,
      vst = "none",
      min_n = 1L
    )
  )

  expect_equal(raw$V[[1]]$vcov_scale, "raw")
  expect_equal(unname(diag(as.matrix(raw$V[[1]]))), raw$se_raw^2, tolerance = 1e-12)
  expect_equal(raw$se, raw$se_raw)
})

test_that("aggregate D1 direct dispatch and wrapper are equivalent", {
  x <- aggregate_d1_long()
  direct <- quiet_working_independence(
    sitemix::sm_estimate(
      x,
      family = "multivariate",
      from_aggregates = TRUE,
      sampling_relation = "same_units",
      vjt = TRUE,
      min_n = 1L
    )
  )
  wrapper <- quiet_working_independence(
    sitemix::sm_estimate_from_aggregates(
      x,
      family = "multivariate",
      sampling_relation = "same_units",
      vjt = TRUE,
      min_n = 1L
    )
  )

  expect_equal(as.data.frame(direct), as.data.frame(wrapper), ignore_attr = TRUE)
  expect_equal(attr(wrapper, "aggregate_case"), "D1")
  expect_equal(attr(wrapper, "family"), "multivariate")
  expect_equal(attr(direct, "sampling_relation"), "same_units")
  expect_equal(attr(wrapper, "sampling_relation"), "same_units")
  expect_equal(attr(direct, "d1_regime"), "D1a")
  expect_equal(attr(wrapper, "d1_regime"), "D1a")
})

test_that("aggregate D1 rejects D0/D1 dispatch mismatches", {
  multi <- data.frame(
    site_id = "S1",
    year = 2025L,
    c_jt_a = 1L,
    c_jt_b = 2L,
    n_jt = 10L
  )
  single <- data.frame(
    site_id = "S1",
    year = 2025L,
    indicator = "a",
    c_jt = 1L,
    n_jt = 10L,
    stringsAsFactors = FALSE
  )

  expect_error(
    sitemix::sm_estimate(multi, family = "binomial", from_aggregates = TRUE),
    class = "sitemix_error_ambiguous_dispatch"
  )
  expect_error(
    sitemix::sm_estimate_from_aggregates(single, family = "multivariate"),
    class = "sitemix_error_ambiguous_dispatch"
  )
  expect_error(
    sitemix::sm_estimate_from_aggregates(
      aggregate_d1_long(),
      family = "multivariate",
      indicator = "absent"
    ),
    class = "sitemix_error_invalid_indicator"
  )
})

test_that("aggregate D1 suppression modes mirror the D0 covariance contract", {
  suppressed <- data.frame(
    site_id = c("S1", "S1", "S2", "S2"),
    year = c(2025L, 2025L, 2025L, 2025L),
    indicator = c("absent", "suspend", "absent", "suspend"),
    c_jt = c(NA_integer_, 2L, 3L, 4L),
    n_jt = c(8L, 8L, 30L, 30L),
    stringsAsFactors = FALSE
  )

  drop <- quiet_working_independence(
    sitemix::sm_estimate_from_aggregates(
      suppressed,
      family = "multivariate",
      suppression = "drop",
      min_n = 10L
    )
  )
  expect_equal(drop$var_method[drop$flag_suppressed], "suppressed_drop")
  expect_true(all(is.na(drop$theta_hat[drop$flag_suppressed])))

  expect_error(
    quiet_working_independence(
      sitemix::sm_estimate_from_aggregates(
        suppressed,
        family = "multivariate",
        suppression = "drop",
        vjt = TRUE
      )
    ),
    class = "sitemix_error_estimate_vcov_invariant"
  )

  upper <- quiet_working_independence(
    sitemix::sm_estimate_from_aggregates(
      suppressed,
      family = "multivariate",
      suppression = "upper_bound",
      suppressed_theta_hat = 0.5,
      suppression_sensitivity_acknowledge = TRUE,
      min_n = 10L
    )
  )
  suppressed_row <- upper[upper$site_id == "S1" & upper$indicator == "absent", ]
  expect_equal(suppressed_row$var_method, "suppression_sensitivity")
  expect_true(is.na(suppressed_row$theta_raw))
  expect_equal(suppressed_row$sensitivity_probability, 0.5)
  expect_equal(suppressed_row$sensitivity_var, 0.25 / 8)
  expect_false("V" %in% names(upper))
  expect_error(
    quiet_working_independence(
      sitemix::sm_estimate_from_aggregates(
        suppressed,
        family = "multivariate",
        suppression = "upper_bound",
        suppression_sensitivity_acknowledge = TRUE,
        vjt = TRUE
      )
    ),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})

test_that("public D1 path rejects framing suppression FPC and scale conflicts", {
  wide <- data.frame(
    site_id = "S1",
    year = 2025L,
    c_jt_a = 2L,
    c_jt_b = 5L,
    n_jt = 10L,
    stringsAsFactors = FALSE
  )
  bad_framing <- rlang::catch_cnd(
    sm_estimate_from_aggregates(
      wide,
      family = "multivariate",
      framing = "subgroup_as_site",
      min_n = 1L
    )
  )
  expect_s3_class(bad_framing, "sitemix_error_invalid_framing")
  expect_match(conditionMessage(bad_framing), "does not accept", fixed = TRUE)
  expect_identical(bad_framing$actual, "subgroup_as_site")

  raw_subgroups <- data.frame(
    site_id = c("S1", "S1"),
    year = c(2025L, 2025L),
    indicator = c("a", "b"),
    subgroup = c("ALL", "ALL"),
    c_jt = c(2L, 5L),
    n_jt = c(10L, 10L),
    stringsAsFactors = FALSE
  )
  raw_subgroup <- rlang::catch_cnd(
    sm_estimate_from_aggregates(raw_subgroups, family = "multivariate", min_n = 1L)
  )
  expect_s3_class(raw_subgroup, "sitemix_error_invalid_framing")
  expect_match(conditionMessage(raw_subgroup), "does not pivot subgroup rows", fixed = TRUE)
  expect_identical(raw_subgroup$actual, "subgroup rows present")

  suppressed <- data.frame(
    site_id = c("S1", "S1"),
    year = c(2025L, 2025L),
    indicator = c("a", "b"),
    c_jt = c(NA_integer_, 5L),
    n_jt = c(8L, 10L),
    stringsAsFactors = FALSE
  )
  suppressed_fpc <- rlang::catch_cnd(
    sm_estimate_from_aggregates(
      suppressed,
      family = "multivariate",
      fpc = 100L,
      min_n = 1L
    )
  )
  expect_s3_class(suppressed_fpc, "sitemix_error_invalid_fpc")
  expect_match(conditionMessage(suppressed_fpc), "no Tier-1 suppression rows", fixed = TRUE)
  expect_identical(suppressed_fpc$actual, "1 suppressed row(s)")

  upper_bound_scale <- rlang::catch_cnd(
    sm_estimate_from_aggregates(
      suppressed,
      family = "multivariate",
      suppression = "upper_bound",
      suppression_sensitivity_acknowledge = TRUE,
      vst = "none",
      min_n = 1L
    )
  )
  expect_s3_class(upper_bound_scale, "sitemix_error_estimate_var_method")
  expect_match(conditionMessage(upper_bound_scale), "requires the default arcsine scale", fixed = TRUE)
  expect_identical(upper_bound_scale$expected, "vst = \"arcsine\" and anscombe = FALSE")
  expect_identical(upper_bound_scale$actual, "none")
})
