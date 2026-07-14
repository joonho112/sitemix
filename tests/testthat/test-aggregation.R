aggregation_data <- function() {
  data.frame(
    site_id = c("S1", "S1", "S1", "S1", "S1", "S2", "S2"),
    year = c(2024L, 2024L, 2024L, 2025L, 2025L, 2024L, 2024L),
    absent = c(1L, 0L, 1L, 0L, 0L, 1L, 0L),
    snap = c(1L, 1L, 0L, 0L, 1L, 1L, 0L),
    frpm = c(1L, 0L, 1L, 1L, 1L, 1L, 0L),
    wic = c(0L, 1L, 1L, 0L, 1L, 1L, 0L),
    category = c("eng", "spa", "eng", "oth", "spa", "eng", "oth"),
    stringsAsFactors = FALSE
  )
}

test_that("binomial aggregation groups by site and year without pooling years", {
  counts <- sitemix:::.sm_prepare_counts(
    aggregation_data(),
    family = "binomial",
    indicator = "absent"
  )

  expect_s3_class(counts, "tbl_df")
  expect_equal(names(counts), c("site_id", "year", "n_jt", "c_jt_absent"))
  expect_equal(counts$site_id, c("S1", "S1", "S2"))
  expect_equal(counts$year, c(2024L, 2025L, 2024L))
  expect_equal(counts$n_jt, c(3L, 2L, 2L))
  expect_equal(counts$c_jt_absent, c(2L, 0L, 1L))
  expect_equal(attr(counts, "input_mode"), "student_level")
  expect_equal(attr(counts, "indicator_order"), "absent")
})

test_that("binomial student and counts_full_suff paths share one substrate", {
  student <- sitemix:::.sm_prepare_counts(
    aggregation_data(),
    family = "binomial",
    indicator = "absent"
  )
  supplied <- data.frame(
    site_id = c("S1", "S1", "S2"),
    year = c(2024L, 2025L, 2024L),
    n_jt = c(3L, 2L, 2L),
    c_jt_absent = c(2L, 0L, 1L)
  )
  counts <- sitemix:::.sm_prepare_counts(
    supplied,
    family = "binomial",
    indicator = "absent",
    from_counts = TRUE
  )

  expect_equal(as.data.frame(student), as.data.frame(counts), ignore_attr = TRUE)
  expect_equal(attr(counts, "input_mode"), "counts_full_suff")
})

test_that("multivariate aggregation preserves supplied indicator and pairwise order", {
  indicators <- c("wic", "snap", "frpm")
  counts <- sitemix:::.sm_prepare_counts(
    aggregation_data(),
    family = "multivariate",
    indicators = indicators
  )

  expected_pairs <- c("c_jt_wic_snap", "c_jt_wic_frpm", "c_jt_snap_frpm")
  expect_equal(attr(counts, "indicator_order"), indicators)
  expect_equal(attr(counts, "count_cols"), paste0("c_jt_", indicators))
  expect_equal(attr(counts, "pair_cols"), expected_pairs)
  expect_equal(
    names(counts),
    c("site_id", "year", "n_jt", "c_jt_wic", "c_jt_snap", "c_jt_frpm", expected_pairs)
  )

  s1_2024 <- counts[counts$site_id == "S1" & counts$year == 2024L, ]
  expect_equal(s1_2024$n_jt, 3L)
  expect_equal(s1_2024$c_jt_wic, 2L)
  expect_equal(s1_2024$c_jt_snap, 2L)
  expect_equal(s1_2024$c_jt_frpm, 2L)
  expect_equal(s1_2024$c_jt_wic_snap, 1L)
  expect_equal(s1_2024$c_jt_wic_frpm, 1L)
  expect_equal(s1_2024$c_jt_snap_frpm, 1L)
})

test_that("multivariate counts_full_suff is equivalent to student aggregation", {
  indicators <- c("wic", "snap", "frpm")
  student <- sitemix:::.sm_prepare_counts(
    aggregation_data(),
    family = "multivariate",
    indicators = indicators
  )
  supplied <- as.data.frame(student)
  counts <- sitemix:::.sm_prepare_counts(
    supplied,
    family = "multivariate",
    indicators = indicators,
    from_counts = TRUE
  )

  expect_equal(as.data.frame(student), as.data.frame(counts), ignore_attr = TRUE)
  expect_equal(attr(counts, "pair_cols"), c("c_jt_wic_snap", "c_jt_wic_frpm", "c_jt_snap_frpm"))
})

test_that("multinomial aggregation emits sorted observed category levels per site-year", {
  counts <- sitemix:::.sm_prepare_counts(
    aggregation_data(),
    family = "multinomial",
    indicator = "category"
  )

  expect_equal(attr(counts, "indicator_order"), c("eng", "oth", "spa"))
  expect_equal(names(counts), c("site_id", "year", "n_jt", "c_jt_eng", "c_jt_oth", "c_jt_spa"))

  s1_2024 <- counts[counts$site_id == "S1" & counts$year == 2024L, ]
  expect_equal(s1_2024$n_jt, 3L)
  expect_equal(s1_2024$c_jt_eng, 2L)
  expect_equal(s1_2024$c_jt_oth, 0L)
  expect_equal(s1_2024$c_jt_spa, 1L)
})

test_that("multinomial aggregation respects observed factor level order", {
  df <- aggregation_data()
  df$category <- factor(df$category, levels = c("spa", "eng", "oth", "unused"))

  counts <- sitemix:::.sm_prepare_counts(
    df,
    family = "multinomial",
    indicator = "category"
  )

  expect_equal(attr(counts, "indicator_order"), c("spa", "eng", "oth"))
  expect_equal(names(counts), c("site_id", "year", "n_jt", "c_jt_spa", "c_jt_eng", "c_jt_oth"))

  s1_2024 <- counts[counts$site_id == "S1" & counts$year == 2024L, ]
  expect_equal(s1_2024$c_jt_spa, 1L)
  expect_equal(s1_2024$c_jt_eng, 2L)
  expect_equal(s1_2024$c_jt_oth, 0L)
})

test_that("multinomial counts_full_suff preserves count-column category order including underscores", {
  supplied <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    n_jt = c(5L, 4L),
    c_jt_home_lang_eng = c(3L, 1L),
    c_jt_home_lang_spa = c(2L, 3L)
  )

  counts <- sitemix:::.sm_prepare_counts(
    supplied,
    family = "multinomial",
    from_counts = TRUE
  )

  expect_equal(names(counts), names(supplied))
  expect_equal(attr(counts, "indicator_order"), c("home_lang_eng", "home_lang_spa"))

  reordered <- sitemix:::.sm_prepare_counts(
    supplied,
    family = "multinomial",
    indicators = c("home_lang_spa", "home_lang_eng"),
    from_counts = TRUE
  )
  expect_equal(
    names(reordered),
    c("site_id", "year", "n_jt", "c_jt_home_lang_spa", "c_jt_home_lang_eng")
  )
  expect_equal(attr(reordered, "indicator_order"), c("home_lang_spa", "home_lang_eng"))

  extra_category <- data.frame(site_id = "S1", year = 2024L, n_jt = 10L, c_jt_A = 4L, c_jt_B = 5L, c_jt_C = 1L)
  expect_error(
    sitemix:::.sm_prepare_counts(extra_category, family = "multinomial", indicators = c("A", "B"), from_counts = TRUE),
    class = "sitemix_error_input_indicator_count"
  )
})

test_that("NA policy changes retained denominators and keeps exact missing class", {
  df <- aggregation_data()
  df$absent[[1]] <- NA_integer_

  expect_warning(
    counts <- sitemix:::.sm_prepare_counts(df, family = "binomial", indicator = "absent"),
    class = "sitemix_warning_dropped_rows"
  )
  s1_2024 <- counts[counts$site_id == "S1" & counts$year == 2024L, ]
  expect_equal(s1_2024$n_jt, 2L)
  expect_equal(s1_2024$c_jt_absent, 1L)

  expect_error(
    sitemix:::.sm_prepare_counts(df, family = "binomial", indicator = "absent", na_action = "error"),
    class = "sitemix_error_input_missing"
  )
})

test_that("multivariate NA dropping is listwise across indicators", {
  df <- aggregation_data()
  df$snap[[1]] <- NA_integer_
  df$frpm[[2]] <- NA_integer_

  expect_warning(
    counts <- sitemix:::.sm_prepare_counts(df, family = "multivariate", indicators = c("snap", "frpm", "wic")),
    class = "sitemix_warning_dropped_rows"
  )
  s1_2024 <- counts[counts$site_id == "S1" & counts$year == 2024L, ]
  expect_equal(s1_2024$n_jt, 1L)
  expect_equal(s1_2024$c_jt_snap, 0L)
  expect_equal(s1_2024$c_jt_frpm, 1L)
  expect_equal(s1_2024$c_jt_wic, 1L)
})

test_that("multinomial NA policy changes retained denominators", {
  df <- aggregation_data()
  df$category[[1]] <- NA_character_

  expect_warning(
    counts <- sitemix:::.sm_prepare_counts(df, family = "multinomial", indicator = "category"),
    class = "sitemix_warning_dropped_rows"
  )
  s1_2024 <- counts[counts$site_id == "S1" & counts$year == 2024L, ]
  expect_equal(s1_2024$n_jt, 2L)
  expect_equal(s1_2024$c_jt_eng, 1L)
  expect_equal(s1_2024$c_jt_spa, 1L)

  expect_error(
    sitemix:::.sm_prepare_counts(df, family = "multinomial", indicator = "category", na_action = "error"),
    class = "sitemix_error_input_missing"
  )
})

test_that("aggregation substrate raises exact classes on invalid inputs", {
  bad_binary <- aggregation_data()
  bad_binary$snap[[1]] <- 2L
  expect_error(
    sitemix:::.sm_prepare_counts(bad_binary, family = "multivariate", indicators = c("snap", "frpm")),
    class = "sitemix_error_input_type"
  )

  duplicate_counts <- data.frame(
    site_id = c("S1", "S1"),
    year = c(2024L, 2024L),
    n_jt = c(2L, 3L),
    c_jt_absent = c(1L, 2L)
  )
  expect_error(
    sitemix:::.sm_prepare_counts(duplicate_counts, family = "binomial", indicator = "absent", from_counts = TRUE),
    class = "sitemix_error_input_indicator_count"
  )

  bad_pair <- data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 10L,
    c_jt_snap = 6L,
    c_jt_frpm = 7L,
    c_jt_snap_frpm = 2L
  )
  expect_error(
    sitemix:::.sm_prepare_counts(bad_pair, family = "multivariate", indicators = c("snap", "frpm"), from_counts = TRUE),
    class = "sitemix_error_input_indicator_count"
  )

  bad_binomial_count <- data.frame(site_id = "S1", year = 2024L, n_jt = 2L, c_jt_absent = 3L)
  expect_error(
    sitemix:::.sm_prepare_counts(bad_binomial_count, family = "binomial", indicator = "absent", from_counts = TRUE),
    class = "sitemix_error_input_indicator_count"
  )

  bad_multinomial_sum <- data.frame(site_id = "S1", year = 2024L, n_jt = 5L, c_jt_A = 2L, c_jt_B = 2L)
  expect_error(
    sitemix:::.sm_prepare_counts(bad_multinomial_sum, family = "multinomial", from_counts = TRUE),
    class = "sitemix_error_input_indicator_count"
  )
})
