student_data <- function() {
  data.frame(
    site_id = c("S1", "S1", "S2", "S2"),
    year = c(2024L, 2024L, 2024L, 2024L),
    absent = c(0L, 1L, 1L, 0L),
    snap = c(TRUE, FALSE, TRUE, FALSE),
    frpm = c(1, 1, 0, 0),
    wic = c(0, 1, 1, 0),
    category = c("A", "B", "A", "B")
  )
}

test_that("argument validators accept valid Phase 1-4 defaults", {
  df <- student_data()
  expect_true(sitemix:::.sm_validate_arguments(df, family = "binomial", indicator = "absent"))
  expect_true(sitemix:::.sm_validate_arguments(df, family = "multivariate", indicators = c("snap", "frpm")))
  expect_true(sitemix:::.sm_validate_arguments(df, family = "multinomial", indicator = "category"))
})

test_that("argument validators raise exact leaf classes", {
  df <- student_data()

  expect_error(sitemix:::.sm_validate_family("D0"), class = "sitemix_error_invalid_family")
  expect_error(sitemix:::.sm_validate_indicator_args(df, "binomial", indicator = NULL), class = "sitemix_error_invalid_indicator")
  expect_error(sitemix:::.sm_validate_indicator_args(df, "multivariate", indicators = "snap"), class = "sitemix_error_invalid_indicators")
  expect_error(sitemix:::.sm_validate_indicator_args(df, "multivariate", indicator = "snap", indicators = c("snap", "frpm")), class = "sitemix_error_invalid_indicator")
  expect_error(sitemix:::.sm_validate_id_cols(df, "site_id"), class = "sitemix_error_invalid_id_cols")
  expect_error(sitemix:::.sm_validate_vst("identity"), class = "sitemix_error_invalid_vst")
  expect_error(sitemix:::.sm_validate_boundary_method("jeffreys"), class = "sitemix_error_invalid_boundary")
  expect_error(sitemix:::.sm_validate_bias_correction("plus_four"), class = "sitemix_error_invalid_bias")
  expect_error(sitemix:::.sm_validate_vjt(c(TRUE, FALSE)), class = "sitemix_error_invalid_vjt")
  expect_error(sitemix:::.sm_validate_min_n(0), class = "sitemix_error_invalid_min_n")
  expect_true(sitemix:::.sm_validate_fpc_arg(c(100, 200)))
  expect_error(sitemix:::.sm_validate_fpc_arg(c(100, 200.5)), class = "sitemix_error_invalid_fpc")
  expect_error(sitemix:::.sm_validate_anscombe_arg(TRUE, "logit"), class = "sitemix_error_anscombe_requires_arcsine")
  expect_error(sitemix:::.sm_validate_from_counts_arg(NA), class = "sitemix_error_invalid_from_counts")
  expect_error(sitemix:::.sm_validate_na_action("omit"), class = "sitemix_error_invalid_na_action")
  expect_error(sitemix:::.sm_validate_description(c("a", "b")), class = "sitemix_error_invalid_description")
})

test_that("id column validation does not require student_id", {
  df <- student_data()[c("site_id", "year", "absent")]
  expect_true(sitemix:::.sm_validate_id_cols(df, c("site_id", "year")))

  missing_site <- df
  missing_site$site_id <- NULL
  expect_error(sitemix:::.sm_validate_id_cols(missing_site, c("site_id", "year")), class = "sitemix_error_input_columns")

  bad_site <- df
  bad_site$site_id <- c(1L, 1L, 2L, 2L)
  expect_error(sitemix:::.sm_validate_id_cols(bad_site, c("site_id", "year")), class = "sitemix_error_input_type")

  numeric_year <- df
  numeric_year$year <- c(2024, 2024, 2024, 2024)
  expect_true(sitemix:::.sm_validate_id_cols(numeric_year, c("site_id", "year")))

  fractional_year <- df
  fractional_year$year <- c(2024.5, 2024, 2024, 2024)
  expect_error(sitemix:::.sm_validate_id_cols(fractional_year, c("site_id", "year")), class = "sitemix_error_input_type")
})

test_that("binary indicator validation accepts logical and numeric 0/1 only", {
  df <- student_data()
  expect_equal(sitemix:::.sm_validate_binary_column(df, "absent"), rep(TRUE, 4))
  expect_equal(sitemix:::.sm_validate_binary_column(df, "snap"), rep(TRUE, 4))

  bad <- df
  bad$absent[[1]] <- 2L
  expect_error(sitemix:::.sm_validate_binary_column(bad, "absent"), class = "sitemix_error_input_type")

  factor_bad <- df
  factor_bad$absent <- factor(factor_bad$absent)
  expect_error(sitemix:::.sm_validate_binary_column(factor_bad, "absent"), class = "sitemix_error_input_type")
})

test_that("NA policy errors or warns with classed conditions", {
  df <- student_data()
  df$absent[[1]] <- NA_integer_

  expect_error(
    sitemix:::.sm_validate_binary_column(df, "absent", na_action = "error"),
    class = "sitemix_error_input_missing"
  )
  expect_warning(
    mask <- sitemix:::.sm_validate_binary_column(df, "absent", na_action = "drop_rows"),
    class = "sitemix_warning_dropped_rows"
  )
  expect_equal(mask, c(FALSE, TRUE, TRUE, TRUE))
})

test_that("multivariate binary validator is listwise across indicators", {
  df <- student_data()
  df$snap[[1]] <- NA

  expect_warning(
    mask <- sitemix:::.sm_validate_binary_indicators(df, c("snap", "frpm"), na_action = "drop_rows"),
    class = "sitemix_warning_dropped_rows"
  )
  expect_equal(mask, c(FALSE, TRUE, TRUE, TRUE))

  bad <- student_data()
  bad$frpm[[2]] <- 2
  expect_error(
    sitemix:::.sm_validate_binary_indicators(bad, c("snap", "frpm"), na_action = "drop_rows"),
    class = "sitemix_error_input_type"
  )
})

test_that("multinomial validator accepts character/factor with K >= 2", {
  df <- student_data()
  expect_equal(sitemix:::.sm_validate_multinomial_column(df, "category"), rep(TRUE, 4))

  one_level <- df
  one_level$category <- "A"
  expect_error(
    sitemix:::.sm_validate_multinomial_column(one_level, "category"),
    class = "sitemix_error_input_indicator_count"
  )

  numeric_bad <- df
  numeric_bad$category <- c(1L, 2L, 1L, 2L)
  expect_error(
    sitemix:::.sm_validate_multinomial_column(numeric_bad, "category"),
    class = "sitemix_error_input_type"
  )
})

test_that("from_counts binomial validates count columns and invariants", {
  counts <- data.frame(site_id = "S1", year = 2024L, n_jt = 10L, c_jt_absent = 3L)
  schema <- sitemix:::.sm_validate_counts_input(counts, "binomial", indicator = "absent")
  expect_equal(schema$count_cols, "c_jt_absent")
  expect_equal(schema$pair_cols, character())

  missing <- counts
  missing$c_jt_absent <- NULL
  expect_error(
    sitemix:::.sm_validate_counts_input(missing, "binomial", indicator = "absent"),
    class = "sitemix_error_input_columns"
  )

  bad <- counts
  bad$c_jt_absent <- 11L
  expect_error(
    sitemix:::.sm_validate_counts_input(bad, "binomial", indicator = "absent"),
    class = "sitemix_error_input_indicator_count"
  )
})

test_that("from_counts multivariate requires supplied-order pairwise counts", {
  counts <- data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 10L,
    c_jt_snap = 6L,
    c_jt_frpm = 7L,
    c_jt_wic = 4L,
    c_jt_snap_frpm = 5L,
    c_jt_snap_wic = 3L,
    c_jt_frpm_wic = 2L
  )
  expected_pairs <- c("c_jt_snap_frpm", "c_jt_snap_wic", "c_jt_frpm_wic")
  expect_equal(sitemix:::.sm_pairwise_count_cols(c("snap", "frpm", "wic")), expected_pairs)
  schema <- sitemix:::.sm_validate_counts_input(counts, "multivariate", indicators = c("snap", "frpm", "wic"))
  expect_equal(schema$count_cols, c("c_jt_snap", "c_jt_frpm", "c_jt_wic"))
  expect_equal(schema$pair_cols, expected_pairs)

  alphabetical_only <- counts
  alphabetical_only$c_jt_snap_frpm <- NULL
  alphabetical_only$c_jt_frpm_snap <- 5L
  expect_error(
    sitemix:::.sm_validate_counts_input(alphabetical_only, "multivariate", indicators = c("snap", "frpm", "wic")),
    class = "sitemix_error_input_columns"
  )

  too_high <- counts
  too_high$c_jt_snap_wic <- 5L
  expect_error(
    sitemix:::.sm_validate_counts_input(too_high, "multivariate", indicators = c("snap", "frpm", "wic")),
    class = "sitemix_error_input_indicator_count"
  )

  too_low <- counts
  too_low$c_jt_snap_frpm <- 2L
  expect_error(
    sitemix:::.sm_validate_counts_input(too_low, "multivariate", indicators = c("snap", "frpm", "wic")),
    class = "sitemix_error_input_indicator_count"
  )
})

test_that("from_counts multinomial enforces category sum equals n_jt", {
  counts <- data.frame(site_id = "S1", year = 2024L, n_jt = 10L, c_jt_A = 4L, c_jt_B = 6L)
  schema <- sitemix:::.sm_validate_counts_input(counts, "multinomial")
  expect_equal(schema$count_cols, c("c_jt_A", "c_jt_B"))
  expect_equal(schema$pair_cols, character())

  bad <- counts
  bad$c_jt_B <- 5L
  expect_error(
    sitemix:::.sm_validate_counts_input(bad, "multinomial"),
    class = "sitemix_error_input_indicator_count"
  )

  one_col <- counts
  one_col$c_jt_B <- NULL
  expect_error(
    sitemix:::.sm_validate_counts_input(one_col, "multinomial"),
    class = "sitemix_error_input_columns"
  )
})
