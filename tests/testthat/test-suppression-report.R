suppression_report_rows <- function() {
  data.frame(
    site_id = c("S1", "S2", "S3", "S4"),
    year = c(2025L, 2025L, 2025L, 2025L),
    subgroup = c("ALL", "ALL", "ALL", "ALL"),
    c_jt = c(NA_integer_, 4L, 20L, 8L),
    n_jt = c(8L, 20L, 40L, 25L),
    stringsAsFactors = FALSE
  )
}

test_that("sm_suppression_report is exported with the report signature", {
  expect_true("sm_suppression_report" %in% getNamespaceExports("sitemix"))
  expect_equal(
    names(formals(sitemix::sm_suppression_report)),
    c(
      "x", "by", "id_cols", "numerator_col", "denominator_col",
      "indicator_col", "subgroup_col", "suppression_col",
      "suppression_flag_value", "suppression_when", "min_n",
      "accountability_n"
    )
  )
})

test_that("sm_suppression_report counts the three denominator tiers", {
  report <- sitemix::sm_suppression_report(
    suppression_report_rows(),
    by = c("subgroup", "year"),
    indicator_col = NULL
  )

  expect_s3_class(report, "sitemix_suppression_report")
  expect_equal(nrow(report), 1L)
  expect_equal(report$n_rows, 4L)
  expect_equal(report$n_tier1, 1L)
  expect_equal(report$n_tier2, 2L)
  expect_equal(report$n_tier3, 1L)
  expect_equal(report$pct_suppressed, 0.25)
  expect_equal(report$pct_below_accountability, 0.75)
  expect_equal(report$median_n_suppressed, 8)
  expect_true(report$denominator_observed_on_suppressed)
  expect_equal(report$recommended_action, "drop_or_acknowledge_variance_sensitivity")
  expect_equal(attr(report, "suppression_detection_path"), "structural")
})

test_that("sm_suppression_report supports publisher flags and structural fallback", {
  x <- data.frame(
    site_id = c("S1", "S2", "S3"),
    year = c(2025L, 2025L, 2025L),
    indicator = c("absent", "absent", "absent"),
    c_jt = c(2L, NA_integer_, 12L),
    n_jt = c(50L, 8L, 80L),
    sup = c("Y", "", ""),
    stringsAsFactors = FALSE
  )

  report <- sitemix::sm_suppression_report(
    x,
    by = "indicator",
    suppression_col = "sup",
    suppression_flag_value = "Y"
  )

  expect_equal(report$n_tier1, 2L)
  expect_equal(report$n_tier3, 1L)
  expect_equal(report$suppression_sources, "publisher_flag,structural_na")
  expect_equal(attr(report, "suppression_detection_path"), "publisher_flag")
})

test_that("sm_suppression_report audits hidden denominators without estimation errors", {
  hidden <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2025L, 2025L),
    indicator = c("absent", "absent"),
    c_jt = c(NA_integer_, 12L),
    n_jt = c(NA_integer_, 80L),
    suppressed = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  report <- sitemix::sm_suppression_report(
    hidden,
    by = NULL,
    suppression_col = "suppressed"
  )

  expect_equal(report$n_tier1, 1L)
  expect_equal(report$n_suppressed_hidden_denominator, 1L)
  expect_false(report$denominator_observed_on_suppressed)
  expect_equal(report$recommended_action, "drop_or_acknowledge_unquantified_sensitivity")
})

test_that("sm_suppression_report works after subgroup-as-site pivoting", {
  x <- data.frame(
    cds = c("S1", "S1", "S2"),
    year = c(2025L, 2025L, 2025L),
    studentgroup = c("ALL", "FOS", "ALL"),
    currnumer = c(10L, NA_integer_, 12L),
    currdenom = c(100L, 8L, 80L),
    small_cell = c("", "Y", ""),
    stringsAsFactors = FALSE
  )
  pivoted <- sitemix::sm_pivot_subgroups_to_sites(
    x,
    site_col = "cds",
    year_col = "year",
    subgroup_col = "studentgroup",
    numerator_col = "currnumer",
    denominator_col = "currdenom",
    indicator = "chronic_absence",
    suppression_col = "small_cell",
    suppression_flag_value = "Y"
  )

  report <- sitemix::sm_suppression_report(
    pivoted,
    by = "source_subgroup",
    suppression_flag_value = TRUE
  )

  expect_equal(report$n_tier1[report$source_subgroup == "FOS"], 1L)
  expect_equal(report$n_tier3[report$source_subgroup == "ALL"], 2L)
  expect_true(report$denominator_observed_on_suppressed[report$source_subgroup == "FOS"])
})

test_that("sm_suppression_report distinguishes indicator pivot keep_na and drop_row", {
  x <- data.frame(
    cds = c("S1", "S1", "S1", "S2", "S2", "S2"),
    year = rep(2025L, 6L),
    studentgroup = c("ALL", "EL", "FOS", "ALL", "EL", "FOS"),
    currnumer = c(10L, 4L, 2L, 12L, 5L, NA_integer_),
    currdenom = c(100L, 40L, 20L, 80L, 30L, 8L),
    small_cell = c("", "", "", "", "", "Y"),
    stringsAsFactors = FALSE
  )
  kept <- sitemix::sm_pivot_subgroups_to_indicators(
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
  dropped <- sitemix::sm_pivot_subgroups_to_indicators(
    x,
    site_col = "cds",
    year_col = "year",
    subgroup_col = "studentgroup",
    numerator_col = "currnumer",
    denominator_col = "currdenom",
    indicator_set = c("ALL", "EL", "FOS"),
    na_action = "drop_row",
    suppression_col = "small_cell",
    suppression_flag_value = "Y"
  )

  kept_report <- sitemix::sm_suppression_report(kept, by = "indicator", suppression_flag_value = TRUE)
  dropped_report <- sitemix::sm_suppression_report(dropped, by = "indicator", suppression_flag_value = TRUE)

  expect_equal(kept_report$n_tier1[kept_report$indicator == "FOS"], 1L)
  expect_equal(sum(kept_report$n_rows), 6L)
  expect_equal(sum(dropped_report$n_rows), 3L)
  expect_equal(sum(dropped_report$n_tier1), 0L)
})
