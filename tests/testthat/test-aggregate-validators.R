test_that("canonical long aggregate input normalizes with metadata", {
  x <- data.frame(
    site_id = c("S001", "S002"),
    year = c(2025, 2025),
    indicator = c("chronic_absent", "chronic_absent"),
    c_jt = c(34, NA),
    n_jt = c(312, 8),
    stringsAsFactors = FALSE
  )

  out <- sitemix:::.sm_prepare_aggregate_input(x)

  expect_s3_class(out, "tbl_df")
  expect_named(
    out,
    c(
      "site_id",
      "year",
      "indicator",
      "subgroup",
      "c_jt",
      "n_jt",
      "suppression_flag",
      "denominator_observed",
      "aggregate_form",
      "flag_suppressed",
      "suppression_source"
    )
  )
  expect_equal(attr(out, "input_mode"), "aggregate")
  expect_equal(attr(out, "aggregate_form"), "long")
  expect_equal(attr(out, "aggregate_case"), "D0")
  expect_equal(attr(out, "family"), "binomial")
  expect_equal(attr(out, "indicator_order"), "chronic_absent")
  expect_type(out$year, "integer")
  expect_type(out$c_jt, "integer")
  expect_type(out$n_jt, "integer")
})

test_that("mapped long aggregate columns preserve subgroup and suppression flags", {
  x <- data.frame(
    cds = c("06012340123456", "06012340123457"),
    reportingyear = c(2025, 2025),
    ind = c("chronic_absent", "chronic_absent"),
    studentgroup = c("ALL", "AI"),
    currnumer = c(58, NA),
    currdenom = c(402, 3),
    small_cell = c("", "Y"),
    stringsAsFactors = FALSE
  )

  out <- sitemix:::.sm_prepare_aggregate_input(
    x,
    id_cols = c("cds", "reportingyear"),
    numerator_col = "currnumer",
    denominator_col = "currdenom",
    indicator_col = "ind",
    subgroup_col = "studentgroup",
    suppression_col = "small_cell",
    suppression_flag_value = "Y"
  )

  expect_equal(out$site_id, x$cds)
  expect_equal(out$subgroup, c("ALL", "AI"))
  expect_equal(out$suppression_flag, c(FALSE, TRUE))
  expect_equal(out$flag_suppressed, c(FALSE, TRUE))
})

test_that("wide aggregate input normalizes common and per-indicator denominators", {
  common <- data.frame(
    site_id = c("A", "B"),
    year = c(2025, 2025),
    c_jt_snap = c(12, 7),
    c_jt_frpm = c(30, 20),
    n_jt = c(100, 90),
    stringsAsFactors = FALSE
  )
  common_out <- sitemix:::.sm_prepare_aggregate_input(common)

  expect_equal(attr(common_out, "aggregate_form"), "wide")
  expect_equal(attr(common_out, "aggregate_case"), "D1")
  expect_equal(attr(common_out, "family"), "multivariate")
  expect_equal(nrow(common_out), 4L)
  expect_equal(sort(unique(common_out$indicator)), c("frpm", "snap"))
  expect_equal(common_out$n_jt[common_out$indicator == "snap"], c(100L, 90L))

  per_indicator <- data.frame(
    site_id = c("A", "B"),
    year = c(2025, 2025),
    c_jt_AI = c(8, NA),
    c_jt_HI = c(94, 52),
    n_jt_AI = c(11, 6),
    n_jt_HI = c(142, 103),
    stringsAsFactors = FALSE
  )
  per_out <- sitemix:::.sm_prepare_aggregate_input(per_indicator)

  expect_equal(nrow(per_out), 4L)
  expect_equal(
    per_out$n_jt[per_out$site_id == "A" & per_out$indicator == "AI"],
    11L
  )
  expect_equal(
    per_out$n_jt[per_out$site_id == "A" & per_out$indicator == "HI"],
    142L
  )
})

test_that("wide suppression flags align after pivot sorting", {
  x <- data.frame(
    site_id = c("B", "A"),
    year = c(2025, 2025),
    c_jt_snap = c(NA, 12),
    c_jt_frpm = c(NA, 30),
    n_jt = c(8, 100),
    flag = c("S", ""),
    stringsAsFactors = FALSE
  )

  out <- sitemix:::.sm_prepare_aggregate_input(
    x,
    suppression_col = "flag",
    suppression_flag_value = "S"
  )

  expect_true(all(out$suppression_flag[out$site_id == "B"]))
  expect_false(any(out$suppression_flag[out$site_id == "A"]))
})

test_that("aggregate dispatch rejects ambiguous long and wide shapes", {
  both <- data.frame(
    site_id = "A",
    year = 2025,
    indicator = "snap",
    c_jt = 1,
    n_jt = 10,
    c_jt_snap = 1,
    stringsAsFactors = FALSE
  )
  expect_error(
    sitemix:::.sm_prepare_aggregate_input(both),
    class = "sitemix_error_ambiguous_dispatch"
  )

  neither <- data.frame(site_id = "A", year = 2025, value = 1)
  expect_error(
    sitemix:::.sm_prepare_aggregate_input(neither),
    class = "sitemix_error_ambiguous_dispatch"
  )
})

test_that("aggregate validators reject bad schema and duplicate keys", {
  expect_error(
    sitemix:::.sm_prepare_aggregate_input(
      data.frame(site_id = 1L, year = 2025, indicator = "snap", c_jt = 1, n_jt = 10)
    ),
    class = "sitemix_error_invalid_aggregate_schema"
  )

  expect_error(
    sitemix:::.sm_prepare_aggregate_input(
      data.frame(site_id = "A", year = 2025, indicator = "snap", c_jt = 1.5, n_jt = 10)
    ),
    class = "sitemix_error_invalid_aggregate_schema"
  )

  dup <- data.frame(
    site_id = c("A", "A"),
    year = c(2025, 2025),
    indicator = c("snap", "snap"),
    c_jt = c(1, 2),
    n_jt = c(10, 10),
    stringsAsFactors = FALSE
  )
  expect_error(
    sitemix:::.sm_prepare_aggregate_input(dup),
    class = "sitemix_error_invalid_aggregate_row"
  )

  mixed_n <- data.frame(
    site_id = "A",
    year = 2025,
    c_jt_snap = 1,
    c_jt_frpm = 2,
    n_jt = 10,
    n_jt_snap = 9,
    n_jt_frpm = 8
  )
  expect_error(
    sitemix:::.sm_prepare_aggregate_input(mixed_n),
    class = "sitemix_error_invalid_aggregate_schema"
  )
})

test_that("aggregate row invariants reject impossible count pairs", {
  bad_rows <- list(
    data.frame(site_id = "A", year = 2025, indicator = "snap", c_jt = 11, n_jt = 10),
    data.frame(site_id = "A", year = 2025, indicator = "snap", c_jt = 1, n_jt = 0),
    data.frame(site_id = "A", year = 2025, indicator = "snap", c_jt = -1, n_jt = 10),
    data.frame(site_id = "A", year = 2025, indicator = "snap", c_jt = 1L, n_jt = NA_integer_)
  )

  for (bad in bad_rows) {
    expect_error(
      sitemix:::.sm_prepare_aggregate_input(bad),
      class = "sitemix_error_invalid_aggregate_row"
    )
  }
})

test_that("hidden-denominator aggregate branch requires explicit flag and bound", {
  hidden <- data.frame(
    site_id = "A",
    year = 2025,
    indicator = "snap",
    c_jt = NA_integer_,
    n_jt = NA_integer_,
    suppression_flag = TRUE
  )

  expect_error(
    sitemix:::.sm_prepare_aggregate_input(hidden),
    class = "sitemix_error_invalid_aggregate_row"
  )

  expect_error(
    sitemix:::.sm_prepare_aggregate_input(
      hidden,
      suppression = "upper_bound",
      suppressed_n_strategy = "worst_case_bound"
    ),
    class = "sitemix_error_invalid_suppressed_n"
  )

  out <- sitemix:::.sm_prepare_aggregate_input(
    hidden,
    suppression = "upper_bound",
    suppressed_n_strategy = "worst_case_bound",
    suppressed_n_bound = 10
  )
  expect_true(is.na(out$c_jt[[1]]))
  expect_true(is.na(out$n_jt[[1]]))
  expect_true(out$suppression_flag[[1]])
  expect_true(out$flag_suppressed[[1]])
})

test_that("aggregate case resolution enforces D0 and D1 cardinality", {
  d0 <- data.frame(
    site_id = "A",
    year = 2025,
    indicator = "snap",
    c_jt = 1,
    n_jt = 10,
    stringsAsFactors = FALSE
  )
  expect_error(
    sitemix:::.sm_prepare_aggregate_input(d0, aggregate_case = "D1"),
    class = "sitemix_error_ambiguous_dispatch"
  )

  d1 <- data.frame(
    site_id = "A",
    year = 2025,
    c_jt_snap = 1,
    c_jt_frpm = 2,
    n_jt = 10,
    stringsAsFactors = FALSE
  )
  expect_error(
    sitemix:::.sm_prepare_aggregate_input(d1, aggregate_case = "D0"),
    class = "sitemix_error_ambiguous_dispatch"
  )
})

test_that("public aggregate path reports mapped and identity schema failures", {
  x <- data.frame(
    site_id = "A",
    year = 2025L,
    indicator = "snap",
    c_jt = 2L,
    n_jt = 10L,
    stringsAsFactors = FALSE
  )

  missing_mapped <- rlang::catch_cnd(
    sm_estimate_from_aggregates(
      x,
      family = "binomial",
      numerator_col = "published_numerator",
      min_n = 1L
    )
  )
  expect_s3_class(missing_mapped, "sitemix_error_invalid_aggregate_schema")
  expect_match(conditionMessage(missing_mapped), "missing mapped source columns", fixed = TRUE)
  expect_identical(missing_mapped$expected, "published_numerator")
  expect_identical(missing_mapped$actual, names(x))

  missing_identity <- rlang::catch_cnd(
    sm_estimate_from_aggregates(
      x,
      family = "binomial",
      id_cols = c("school", "year"),
      min_n = 1L
    )
  )
  expect_s3_class(missing_identity, "sitemix_error_invalid_aggregate_schema")
  expect_match(conditionMessage(missing_identity), "missing required identity columns", fixed = TRUE)
  expect_identical(missing_identity$expected, c("school", "year"))
  expect_identical(missing_identity$actual, names(x))
})

test_that("public aggregate path reports year indicator and required-column failures", {
  base <- data.frame(
    site_id = "A",
    year = 2025L,
    indicator = "snap",
    c_jt = 2L,
    n_jt = 10L,
    stringsAsFactors = FALSE
  )

  bad_year_data <- base
  bad_year_data$year <- 2025.5
  bad_year <- rlang::catch_cnd(
    sm_estimate_from_aggregates(bad_year_data, family = "binomial", min_n = 1L)
  )
  expect_s3_class(bad_year, "sitemix_error_invalid_aggregate_schema")
  expect_match(conditionMessage(bad_year), "integer-like column", fixed = TRUE)
  expect_identical(bad_year$expected, "integer-like year values")
  expect_identical(bad_year$actual, "numeric")

  bad_indicator_data <- base
  bad_indicator_data$indicator <- 1L
  bad_indicator <- rlang::catch_cnd(
    sm_estimate_from_aggregates(bad_indicator_data, family = "binomial", min_n = 1L)
  )
  expect_s3_class(bad_indicator, "sitemix_error_invalid_aggregate_schema")
  expect_match(conditionMessage(bad_indicator), "`indicator` must be", fixed = TRUE)
  expect_identical(bad_indicator$expected, "character/factor values")
  expect_identical(bad_indicator$actual, "integer")

  bad_override <- rlang::catch_cnd(
    sm_estimate_from_aggregates(
      base,
      family = "binomial",
      indicator = c("snap", "frpm"),
      min_n = 1L
    )
  )
  expect_s3_class(bad_override, "sitemix_error_invalid_indicator")
  expect_match(conditionMessage(bad_override), "one aggregate indicator label", fixed = TRUE)
  expect_identical(bad_override$expected, "NULL or one non-empty string")
  expect_identical(bad_override$actual, c("snap", "frpm"))

  missing_count_data <- base
  missing_count_data$c_jt <- NULL
  missing_count <- rlang::catch_cnd(
    sm_estimate_from_aggregates(missing_count_data, family = "binomial", min_n = 1L)
  )
  expect_s3_class(missing_count, "sitemix_error_invalid_aggregate_schema")
  expect_match(conditionMessage(missing_count), "missing required columns", fixed = TRUE)
  expect_true("c_jt" %in% missing_count$expected)
  expect_false("c_jt" %in% missing_count$actual)
})

test_that("public wide aggregate path validates indicator and denominator schema", {
  wide <- data.frame(
    site_id = "A",
    year = 2025L,
    c_jt_a = 2L,
    c_jt_b = 3L,
    n_jt = 10L,
    stringsAsFactors = FALSE
  )

  duplicate_indicators <- rlang::catch_cnd(
    sm_estimate_from_aggregates(
      wide,
      family = "binomial",
      indicators = c("a", "a"),
      min_n = 1L
    )
  )
  expect_s3_class(duplicate_indicators, "sitemix_error_invalid_indicators")
  expect_match(conditionMessage(duplicate_indicators), "distinct aggregate indicator labels", fixed = TRUE)
  expect_identical(duplicate_indicators$actual, c("a", "a"))

  missing_indicator <- rlang::catch_cnd(
    sm_estimate_from_aggregates(
      wide,
      family = "multivariate",
      indicators = c("a", "c"),
      min_n = 1L
    )
  )
  expect_s3_class(missing_indicator, "sitemix_error_invalid_aggregate_schema")
  expect_match(conditionMessage(missing_indicator), "missing required columns", fixed = TRUE)
  expect_identical(missing_indicator$expected, c("c_jt_a", "c_jt_c"))
  expect_false("c_jt_c" %in% missing_indicator$actual)

  pairwise_data <- wide
  pairwise_data$c_jt_a_b <- 1L
  pairwise <- rlang::catch_cnd(
    sm_estimate_from_aggregates(
      pairwise_data,
      family = "multivariate",
      indicators = c("a", "b"),
      min_n = 1L
    )
  )
  expect_s3_class(pairwise, "sitemix_error_invalid_aggregate_schema")
  expect_match(conditionMessage(pairwise), "does not accept pairwise", fixed = TRUE)
  expect_identical(pairwise$actual, "c_jt_a_b")

  no_denominator <- wide
  no_denominator$n_jt <- NULL
  missing_denominator <- rlang::catch_cnd(
    sm_estimate_from_aggregates(
      no_denominator,
      family = "multivariate",
      min_n = 1L
    )
  )
  expect_s3_class(missing_denominator, "sitemix_error_invalid_aggregate_schema")
  expect_match(conditionMessage(missing_denominator), "missing denominator columns", fixed = TRUE)
  expect_identical(missing_denominator$expected, c("n_jt", "n_jt_a"))

  ordered <- withCallingHandlers(
    sm_estimate_from_aggregates(
      wide,
      family = "multivariate",
      indicators = c("b", "a"),
      min_n = 1L
    ),
    sitemix_warning_working_independence_default = function(w) invokeRestart("muffleWarning")
  )
  expect_identical(ordered$indicator, c("b", "a"))
  expect_equal(ordered$theta_raw, c(0.3, 0.2))
  expect_true(validate.sitemix_estimates(ordered))
})
