pivot_subgroup_data <- function() {
  data.frame(
    cds = c("S1", "S1", "S2"),
    year = c(2025L, 2025L, 2025L),
    studentgroup = c("ALL", "LTEL", "ALL"),
    currnumer = c(10L, 2L, 12L),
    currdenom = c(100L, 20L, 80L),
    stringsAsFactors = FALSE
  )
}

race_partition_data <- function() {
  data.frame(
    cds = rep("S1", 9L),
    year = rep(2025L, 9L),
    studentgroup = c("ALL", "AA", "AI", "AS", "FI", "HI", "MR", "PI", "WH"),
    currnumer = c(20L, 4L, 1L, 2L, 1L, 8L, 2L, 1L, 1L),
    currdenom = c(100L, 20L, 5L, 10L, 5L, 40L, 10L, 5L, 5L),
    stringsAsFactors = FALSE
  )
}

test_that("subgroup-as-site pivot returns D0-ready aggregate rows", {
  out <- sitemix::sm_pivot_subgroups_to_sites(
    pivot_subgroup_data(),
    site_col = "cds",
    year_col = "year",
    subgroup_col = "studentgroup",
    numerator_col = "currnumer",
    denominator_col = "currdenom",
    indicator = "chronic_absence"
  )

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("site_id", "year", "indicator", "c_jt", "n_jt", "suppression_flag") %in% names(out)))
  expect_false("subgroup" %in% names(out))
  expect_equal(out$indicator, rep("chronic_absence", 3L))
  expect_equal(sort(out$site_id), c("S1_ALL", "S1_LTEL", "S2_ALL"))
  expect_equal(out$source_subgroup[out$site_id == "S1_LTEL"], "LTEL")
  expect_equal(attr(out, "framing"), "subgroup_as_site")
  expect_equal(attr(out, "partition_target"), "none")
})

test_that("subgroup-as-site pivot feeds D0 estimation with framing populated", {
  pivoted <- sitemix::sm_pivot_subgroups_to_sites(
    pivot_subgroup_data(),
    site_col = "cds",
    year_col = "year",
    subgroup_col = "studentgroup",
    numerator_col = "currnumer",
    denominator_col = "currdenom",
    indicator = "chronic_absence"
  )

  out <- sitemix::sm_estimate_from_aggregates(
    pivoted,
    family = "binomial",
    indicator = "chronic_absence",
    aggregate_case = "D0",
    framing = "subgroup_as_site",
    min_n = 1L
  )

  expect_s3_class(out, "sitemix_estimates")
  expect_equal(out$input_mode, rep("aggregate", 3L))
  expect_equal(out$framing, rep("subgroup_as_site", 3L))
  expect_false("V" %in% names(out))
  expect_false("K" %in% names(out))
  expect_true(validate.sitemix_estimates(out))
})

test_that("subgroup-as-site pivot preserves suppression evidence for D0", {
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

  expect_true(pivoted$suppression_flag[pivoted$source_subgroup == "FOS"])

  out <- sitemix::sm_estimate_from_aggregates(
    pivoted,
    family = "binomial",
    indicator = "chronic_absence",
    aggregate_case = "D0",
    framing = "subgroup_as_site",
    suppression = "drop"
  )
  suppressed <- out[out$site_id == "S1_FOS", ]
  expect_true(suppressed$flag_suppressed)
  expect_equal(suppressed$var_method, "suppressed_drop")
  expect_true(all(is.na(unlist(suppressed[c("theta_raw", "theta_hat", "se_raw", "se")]))))
})

test_that("subgroup-as-site pivot validates duplicate and colliding keys", {
  dup <- data.frame(
    cds = c("S1", "S1"),
    year = c(2025L, 2025L),
    studentgroup = c("ALL", "ALL"),
    currnumer = c(10L, 11L),
    currdenom = c(100L, 100L),
    stringsAsFactors = FALSE
  )
  expect_error(
    sitemix::sm_pivot_subgroups_to_sites(
      dup,
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom",
      indicator = "chronic_absence"
    ),
    class = "sitemix_error_invalid_aggregate_row"
  )

  collide <- data.frame(
    cds = c("A_B", "A"),
    year = c(2025L, 2025L),
    studentgroup = c("C", "B_C"),
    currnumer = c(1L, 2L),
    currdenom = c(10L, 10L),
    stringsAsFactors = FALSE
  )
  expect_error(
    sitemix::sm_pivot_subgroups_to_sites(
      collide,
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom",
      indicator = "chronic_absence"
    ),
    class = "sitemix_error_invalid_aggregate_row"
  )

  expect_error(
    sitemix::sm_pivot_subgroups_to_sites(
      pivot_subgroup_data(),
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom",
      indicator = ""
    ),
    class = "sitemix_error_invalid_indicator"
  )

  expect_error(
    sitemix::sm_pivot_subgroups_to_sites(
      pivot_subgroup_data(),
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom",
      indicator = "chronic_absence",
      separator = ""
    ),
    class = "sitemix_error_invalid_id_cols"
  )

  expect_error(
    sitemix::sm_pivot_subgroups_to_sites(
      race_partition_data(),
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom",
      partition_target = "denominator_composition",
      partition_tolerance = Inf
    ),
    class = "sitemix_error_invalid_partition_target"
  )
})

test_that("subgroup-as-site pivot does not auto-promote exact race partitions", {
  out <- sitemix::sm_pivot_subgroups_to_sites(
    race_partition_data(),
    site_col = "cds",
    year_col = "year",
    subgroup_col = "studentgroup",
    numerator_col = "currnumer",
    denominator_col = "currdenom",
    indicator = "chronic_absence"
  )

  expect_equal(attr(out, "partition_target"), "none")
  expect_equal(nrow(out), 9L)
  expect_true(all(out$framing == "subgroup_as_site"))
  expect_true(all(grepl("^S1_", out$site_id)))
  expect_false(any(grepl("^c_jt_", names(out))))
})

test_that("denominator composition emits Scenario C count input", {
  pivoted <- sitemix::sm_pivot_subgroups_to_sites(
    race_partition_data(),
    site_col = "cds",
    year_col = "year",
    subgroup_col = "studentgroup",
    numerator_col = "currnumer",
    denominator_col = "currdenom",
    partition_target = "denominator_composition"
  )
  categories <- attr(pivoted, "indicator_order")
  expected <- c(AA = 20L, AI = 5L, AS = 10L, FI = 5L, HI = 40L, MR = 10L, PI = 5L, WH = 5L)

  expect_equal(names(pivoted), c("site_id", "year", "n_jt", paste0("c_jt_", names(expected))))
  expect_equal(pivoted$n_jt, 100L)
  expect_false("c_jt_ALL" %in% names(pivoted))
  expect_equal(categories, names(expected))
  expect_equal(attr(pivoted, "partition_target"), "denominator_composition")
  expect_equal(attr(pivoted, "composition_count_source"), "denominator")
  expect_equal(attr(pivoted, "framing"), "subgroup_as_site")

  out <- sitemix::sm_estimate_from_counts(
    pivoted,
    family = "multinomial",
    indicators = categories,
    vjt = TRUE,
    min_n = 1L
  )
  expect_equal(out$indicator, names(expected))
  expect_equal(out$theta_raw, unname(expected / 100), tolerance = 1e-12)
  expect_equal(out$input_mode, rep("counts_full_suff", length(expected)))
  expect_equal(out$V[[1]]$indicator_order, names(expected))
})

test_that("case composition emits Scenario C count input", {
  pivoted <- sitemix::sm_pivot_subgroups_to_sites(
    race_partition_data(),
    site_col = "cds",
    year_col = "year",
    subgroup_col = "studentgroup",
    numerator_col = "currnumer",
    denominator_col = "currdenom",
    partition_target = "case_composition"
  )
  categories <- attr(pivoted, "indicator_order")
  expected <- c(AA = 4L, AI = 1L, AS = 2L, FI = 1L, HI = 8L, MR = 2L, PI = 1L, WH = 1L)

  expect_equal(pivoted$n_jt, 20L)
  expect_equal(attr(pivoted, "partition_target"), "case_composition")
  expect_equal(attr(pivoted, "composition_count_source"), "case")

  out <- sitemix::sm_estimate_from_counts(
    pivoted,
    family = "multinomial",
    indicators = categories,
    min_n = 1L
  )
  expect_equal(out$indicator, names(expected))
  expect_equal(out$theta_raw, unname(expected / 20), tolerance = 1e-12)
})

test_that("composition partition routing validates explicit opt-in data", {
  bad_denominator <- race_partition_data()
  bad_denominator$currdenom[bad_denominator$studentgroup == "WH"] <- 4L
  expect_error(
    sitemix::sm_pivot_subgroups_to_sites(
      bad_denominator,
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom",
      partition_target = "denominator_composition"
    ),
    class = "sitemix_error_invalid_partition_target"
  )

  no_all <- subset(race_partition_data(), studentgroup != "ALL")
  expect_error(
    sitemix::sm_pivot_subgroups_to_sites(
      no_all,
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom",
      partition_target = "case_composition"
    ),
    class = "sitemix_error_invalid_partition_target"
  )

  suppressed_case <- race_partition_data()
  suppressed_case$currnumer[suppressed_case$studentgroup == "AI"] <- NA_integer_
  suppressed_case$small_cell <- ifelse(suppressed_case$studentgroup == "AI", "Y", "")
  expect_error(
    sitemix::sm_pivot_subgroups_to_sites(
      suppressed_case,
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom",
      partition_target = "case_composition",
      suppression_col = "small_cell",
      suppression_flag_value = "Y"
    ),
    class = "sitemix_error_invalid_partition_target"
  )

  denominator_ok <- sitemix::sm_pivot_subgroups_to_sites(
    suppressed_case,
    site_col = "cds",
    year_col = "year",
    subgroup_col = "studentgroup",
    numerator_col = "currnumer",
    denominator_col = "currdenom",
    partition_target = "denominator_composition",
    suppression_col = "small_cell",
    suppression_flag_value = "Y"
  )
  expect_equal(denominator_ok$n_jt, 100L)

  missing_all <- race_partition_data()
  missing_all$currdenom[missing_all$studentgroup == "ALL"] <- NA_integer_
  missing_condition <- expect_error(
    sitemix::sm_pivot_subgroups_to_sites(
      missing_all,
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom",
      partition_target = "denominator_composition"
    ),
    "totals must be observed",
    class = "sitemix_error_invalid_partition_target"
  )
  expect_identical(
    missing_condition$row_identity,
    list(site_id = "S1", year = 2025L, indicator = "ALL")
  )

  nonpositive_all <- race_partition_data()
  nonpositive_all$currdenom[nonpositive_all$studentgroup == "ALL"] <- 0L
  nonpositive_condition <- expect_error(
    sitemix::sm_pivot_subgroups_to_sites(
      nonpositive_all,
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom",
      partition_target = "denominator_composition"
    ),
    "totals must be positive",
    class = "sitemix_error_invalid_partition_target"
  )
  expect_identical(
    nonpositive_condition$row_identity,
    list(site_id = "S1", year = 2025L, indicator = "ALL")
  )
})

test_that("composition count output is not multinomial aggregate input", {
  pivoted <- sitemix::sm_pivot_subgroups_to_sites(
    race_partition_data(),
    site_col = "cds",
    year_col = "year",
    subgroup_col = "studentgroup",
    numerator_col = "currnumer",
    denominator_col = "currdenom",
    partition_target = "denominator_composition"
  )
  expect_error(
    sitemix::sm_estimate_from_aggregates(
      pivoted,
      family = "multinomial",
      indicators = attr(pivoted, "indicator_order")
    ),
    class = "sitemix_error_ambiguous_dispatch"
  )
})

test_that("subgroup-as-site pivot still defers mixed-level mode", {
  expect_error(
    sitemix::sm_pivot_subgroups_to_sites(
      pivot_subgroup_data(),
      site_col = "cds",
      year_col = "year",
      subgroup_col = "studentgroup",
      numerator_col = "currnumer",
      denominator_col = "currdenom",
      level_override = list(school = "ALL")
    ),
    class = "sitemix_error_invalid_level_override"
  )
})
