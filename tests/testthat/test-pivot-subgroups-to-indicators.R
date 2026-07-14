pivot_indicator_data <- function() {
  data.frame(
    cds = c("S1", "S1", "S1", "S2", "S2", "S2"),
    year = rep(2025L, 6L),
    studentgroup = c("ALL", "EL", "FOS", "ALL", "EL", "FOS"),
    currnumer = c(10L, 4L, 2L, 12L, 5L, 1L),
    currdenom = c(100L, 40L, 20L, 80L, 30L, 10L),
    stringsAsFactors = FALSE
  )
}

quiet_d1_warning <- function(expr) {
  withCallingHandlers(
    expr,
    sitemix_warning_working_independence_default = function(w) invokeRestart("muffleWarning")
  )
}

test_that("subgroup-as-indicator pivot returns D1-ready long rows", {
  out <- sitemix::sm_pivot_subgroups_to_indicators(
    pivot_indicator_data(),
    site_col = "cds",
    year_col = "year",
    subgroup_col = "studentgroup",
    numerator_col = "currnumer",
    denominator_col = "currdenom"
  )

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("site_id", "year", "indicator", "c_jt", "n_jt", "suppression_flag", "framing") %in% names(out)))
  expect_false("subgroup" %in% names(out))
  expect_equal(unique(out$site_id), c("S1", "S2"))
  expect_equal(unique(out$framing), "subgroup_as_indicator")
  expect_equal(attr(out, "framing"), "subgroup_as_indicator")
  expect_equal(attr(out, "indicator_set"), c("ALL", "EL", "FOS"))
  expect_equal(attr(out, "na_action"), "drop_row")
})

test_that("subgroup-as-indicator pivot feeds D1 working-independence engine", {
  indicator_set <- c("FOS", "ALL", "EL")
  pivoted <- sitemix::sm_pivot_subgroups_to_indicators(
    pivot_indicator_data(),
    site_col = "cds",
    year_col = "year",
    subgroup_col = "studentgroup",
    numerator_col = "currnumer",
    denominator_col = "currdenom",
    indicator_set = indicator_set
  )

  out <- quiet_d1_warning(
    sitemix::sm_estimate_from_aggregates(
      pivoted,
      family = "multivariate",
      aggregate_case = "D1",
      framing = "subgroup_as_indicator",
      indicators = indicator_set,
      vjt = TRUE,
      min_n = 1L
    )
  )

  expect_s3_class(out, "sitemix_estimates")
  expect_equal(attr(out, "aggregate_case"), "D1")
  expect_equal(out$framing, rep("subgroup_as_indicator", nrow(out)))
  expect_equal(out$indicator[out$site_id == "S1"], indicator_set)
  expect_equal(out$V[[1]]$vcov_method, "working_independence")
  expect_equal(out$V[[1]]$indicator_order, indicator_set)
  expect_equal(unname(diag(as.matrix(out$V[[1]]))), out$se[out$site_id == "S1"]^2, tolerance = 1e-12)
  expect_true(all(as.matrix(out$V[[1]])[row(as.matrix(out$V[[1]])) != col(as.matrix(out$V[[1]]))] == 0))
})

test_that("subgroup-as-indicator drop_row drops incomplete site-years", {
  incomplete <- pivot_indicator_data()
  incomplete <- incomplete[!(incomplete$cds == "S2" & incomplete$studentgroup == "FOS"), ]

  out <- sitemix::sm_pivot_subgroups_to_indicators(
    incomplete,
    site_col = "cds",
    year_col = "year",
    subgroup_col = "studentgroup",
    numerator_col = "currnumer",
    denominator_col = "currdenom",
    indicator_set = c("ALL", "EL", "FOS"),
    na_action = "drop_row"
  )

  expect_equal(unique(out$site_id), "S1")
  expect_equal(out$indicator, c("ALL", "EL", "FOS"))
})

test_that("subgroup-as-indicator keep_na preserves suppressed rows for D1 suppression", {
  x <- pivot_indicator_data()
  x$small_cell <- ""
  x$currnumer[x$cds == "S2" & x$studentgroup == "FOS"] <- NA_integer_
  x$small_cell[x$cds == "S2" & x$studentgroup == "FOS"] <- "Y"

  pivoted <- sitemix::sm_pivot_subgroups_to_indicators(
    x,
    site_col = "cds",
    year_col = "year",
    subgroup_col = "studentgroup",
    numerator_col = "currnumer",
    denominator_col = "currdenom",
    indicator_set = c("ALL", "EL", "FOS"),
    na_action = "keep_na",
    suppression_col = "small_cell",
    suppression_flag_value = "Y"
  )

  expect_equal(unique(pivoted$site_id), c("S1", "S2"))
  expect_true(pivoted$suppression_flag[pivoted$site_id == "S2" & pivoted$indicator == "FOS"])

  out <- quiet_d1_warning(
    sitemix::sm_estimate_from_aggregates(
      pivoted,
      family = "multivariate",
      aggregate_case = "D1",
      framing = "subgroup_as_indicator",
      indicators = c("ALL", "EL", "FOS"),
      suppression = "drop",
      vjt = FALSE,
      min_n = 1L
    )
  )
  suppressed <- out[out$site_id == "S2" & out$indicator == "FOS", ]
  expect_true(suppressed$flag_suppressed)
  expect_equal(suppressed$var_method, "suppressed_drop")
  expect_true(is.na(suppressed$theta_hat))
})

test_that("subgroup-as-indicator pivot validates duplicates and retained K", {
  dup <- rbind(pivot_indicator_data(), pivot_indicator_data()[1, ])
  expect_error(
    sitemix::sm_pivot_subgroups_to_indicators(
      dup,
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom"
    ),
    class = "sitemix_error_invalid_aggregate_row"
  )

  mostly_dropped <- pivot_indicator_data()
  mostly_dropped$currnumer[mostly_dropped$studentgroup %in% c("EL", "FOS")] <- NA_integer_
  expect_error(
    sitemix::sm_pivot_subgroups_to_indicators(
      mostly_dropped,
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom",
      na_action = "drop_row"
    ),
    class = "sitemix_error_ambiguous_dispatch"
  )
})

test_that("subgroup-as-indicator pivot validates indicator_set and na_action", {
  expect_error(
    sitemix::sm_pivot_subgroups_to_indicators(
      pivot_indicator_data(),
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom",
      indicator_set = "ALL"
    ),
    class = "sitemix_error_invalid_indicators"
  )

  expect_error(
    sitemix::sm_pivot_subgroups_to_indicators(
      pivot_indicator_data(),
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom",
      indicator_set = c("ALL", "MISSING")
    ),
    class = "sitemix_error_invalid_indicators"
  )

  expect_error(
    sitemix::sm_pivot_subgroups_to_indicators(
      pivot_indicator_data(),
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom",
      na_action = "bad"
    ),
    class = "sitemix_error_invalid_na_action"
  )
})
