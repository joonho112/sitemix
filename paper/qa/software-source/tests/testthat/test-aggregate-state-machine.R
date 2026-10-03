state_quiet_d1 <- function(expr) {
  withCallingHandlers(
    expr,
    sitemix_warning_working_independence_default = function(w) invokeRestart("muffleWarning")
  )
}

state_d1_long <- function(denominators = c(100L, 100L, 80L, 80L)) {
  data.frame(
    site_id = c("S1", "S1", "S2", "S2"),
    year = rep(2025L, 4L),
    indicator = c("a", "b", "a", "b"),
    c_jt = c(10L, 20L, 8L, 16L),
    n_jt = denominators,
    stringsAsFactors = FALSE
  )
}

test_that("aggregate auto dispatch follows the D0/D1 shape truth table", {
  d0_long <- data.frame(
    site_id = c("S1", "S2"),
    year = rep(2025L, 2L),
    indicator = rep("a", 2L),
    c_jt = c(1L, 2L),
    n_jt = c(10L, 20L),
    stringsAsFactors = FALSE
  )
  d0_wide <- data.frame(
    site_id = c("S1", "S2"),
    year = rep(2025L, 2L),
    c_jt_a = c(1L, 2L),
    n_jt = c(10L, 20L)
  )
  d1_wide_common <- data.frame(
    site_id = c("S1", "S2"),
    year = rep(2025L, 2L),
    c_jt_a = c(1L, 2L),
    c_jt_b = c(3L, 4L),
    n_jt = c(10L, 20L)
  )
  d1_wide_varying <- data.frame(
    site_id = c("S1", "S2"),
    year = rep(2025L, 2L),
    c_jt_a = c(1L, 2L),
    c_jt_b = c(3L, 4L),
    n_jt_a = c(10L, 20L),
    n_jt_b = c(12L, 25L)
  )

  long_d0 <- sitemix::sm_estimate_from_aggregates(
    d0_long,
    family = "binomial",
    indicator = "a",
    min_n = 1L
  )
  wide_d0 <- sitemix::sm_estimate_from_aggregates(
    d0_wide,
    family = "binomial",
    indicator = "a",
    min_n = 1L
  )
  long_unknown <- state_quiet_d1(
    sitemix::sm_estimate_from_aggregates(
      state_d1_long(),
      family = "multivariate",
      min_n = 1L
    )
  )
  wide_same <- state_quiet_d1(
    sitemix::sm_estimate_from_aggregates(
      d1_wide_common,
      family = "multivariate",
      sampling_relation = "same_units",
      min_n = 1L
    )
  )
  wide_different <- state_quiet_d1(
    sitemix::sm_estimate_from_aggregates(
      d1_wide_varying,
      family = "multivariate",
      sampling_relation = "different_units",
      min_n = 1L
    )
  )

  expect_equal(attr(long_d0, "aggregate_case"), "D0")
  expect_equal(attr(wide_d0, "aggregate_case"), "D0")
  expect_equal(attr(long_unknown, "aggregate_case"), "D1")
  expect_equal(attr(long_unknown, "sampling_relation"), "unknown")
  expect_equal(attr(long_unknown, "denominator_pattern"), "common")
  expect_equal(attr(long_unknown, "d1_regime"), "unknown")
  expect_equal(attr(wide_same, "sampling_relation"), "same_units")
  expect_equal(attr(wide_same, "denominator_pattern"), "common")
  expect_equal(attr(wide_same, "d1_regime"), "D1a")
  expect_equal(attr(wide_different, "sampling_relation"), "different_units")
  expect_equal(attr(wide_different, "denominator_pattern"), "varying")
  expect_equal(attr(wide_different, "d1_regime"), "D1b")
})

test_that("sampling provenance and denominator pattern are independent facts", {
  varying_same <- state_quiet_d1(
    sitemix::sm_estimate_from_aggregates(
      state_d1_long(c(100L, 120L, 80L, 90L)),
      family = "multivariate",
      sampling_relation = "same_units",
      min_n = 1L
    )
  )
  common_different <- state_quiet_d1(
    sitemix::sm_estimate_from_aggregates(
      state_d1_long(),
      family = "multivariate",
      sampling_relation = "different_units",
      min_n = 1L
    )
  )
  mixed_unknown <- state_quiet_d1(
    sitemix::sm_estimate_from_aggregates(
      state_d1_long(c(100L, 100L, 80L, 90L)),
      family = "multivariate",
      min_n = 1L
    )
  )

  expect_equal(attr(varying_same, "d1_regime"), "D1a")
  expect_equal(attr(varying_same, "denominator_pattern"), "varying")
  expect_equal(attr(common_different, "d1_regime"), "D1b")
  expect_equal(attr(common_different, "denominator_pattern"), "common")
  expect_equal(attr(mixed_unknown, "d1_regime"), "unknown")
  expect_equal(attr(mixed_unknown, "denominator_pattern"), "mixed")

  by_group <- attr(mixed_unknown, "d1_regime_by_group")
  expect_named(
    by_group,
    c("site_id", "year", "K", "sampling_relation", "denominator_pattern", "d1_regime")
  )
  expect_equal(by_group$denominator_pattern, c("common", "varying"))
  expect_equal(by_group$sampling_relation, rep("unknown", 2L))
  expect_equal(by_group$d1_regime, rep("unknown", 2L))
})

test_that("D1 requires one complete ordered indicator set", {
  incomplete <- state_d1_long()
  incomplete <- incomplete[!(incomplete$site_id == "S2" & incomplete$indicator == "b"), ]

  err <- rlang::catch_cnd(
    state_quiet_d1(
      sitemix::sm_estimate_from_aggregates(
        incomplete,
        family = "multivariate",
        min_n = 1L
      )
    )
  )
  expect_s3_class(err, "sitemix_error_invalid_aggregate_schema")
  expect_equal(err$expected_indicators, c("a", "b"))
  expect_equal(err$actual_indicators, "a")
  expect_equal(err$missing_indicators, "b")
  expect_match(err$fix, "Complete every site-year", fixed = TRUE)

  heterogeneous <- data.frame(
    site_id = c("S1", "S1", "S2", "S2"),
    year = rep(2025L, 4L),
    indicator = c("a", "b", "a", "c"),
    c_jt = c(1L, 2L, 3L, 4L),
    n_jt = rep(10L, 4L),
    stringsAsFactors = FALSE
  )
  expect_error(
    state_quiet_d1(
      sitemix::sm_estimate_from_aggregates(
        heterogeneous,
        family = "multivariate",
        min_n = 1L
      )
    ),
    class = "sitemix_error_invalid_aggregate_schema"
  )
})

test_that("explicit order and subgroup-as-indicator framing are deterministic", {
  subgroup_rows <- data.frame(
    site_id = c("S1", "S1", "S2", "S2"),
    year = rep(2025L, 4L),
    indicator = c("ALL", "EL", "EL", "ALL"),
    c_jt = c(10L, 4L, 5L, 12L),
    n_jt = c(100L, 40L, 30L, 80L),
    stringsAsFactors = FALSE
  )
  out <- state_quiet_d1(
    sitemix::sm_estimate_from_aggregates(
      subgroup_rows,
      family = "multivariate",
      indicators = c("EL", "ALL"),
      framing = "subgroup_as_indicator",
      sampling_relation = "different_units",
      vjt = TRUE,
      min_n = 1L
    )
  )

  expect_equal(out$indicator[out$site_id == "S1"], c("EL", "ALL"))
  expect_equal(out$indicator[out$site_id == "S2"], c("EL", "ALL"))
  expect_equal(unique(out$framing), "subgroup_as_indicator")
  expect_equal(attr(out, "d1_regime"), "D1b")
  expect_equal(attr(out, "denominator_pattern"), "varying")
  expect_equal(out$V[[1L]]$vcov_method, "working_independence")
  expect_true(all(as.matrix(out$V[[1L]])[row(as.matrix(out$V[[1L]])) != col(as.matrix(out$V[[1L]]))] == 0))
})

test_that("sampling_relation rejects denominator-derived pseudo-provenance", {
  expect_error(
    state_quiet_d1(
      sitemix::sm_estimate_from_aggregates(
        state_d1_long(),
        family = "multivariate",
        sampling_relation = "equal_denominators",
        min_n = 1L
      )
    ),
    class = "sitemix_error_invalid_sampling_relation"
  )
})
