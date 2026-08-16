test_that("structural aggregate suppression detection flags NA numerator rows", {
  x <- data.frame(
    site_id = c("A", "B", "C"),
    year = c(2025, 2025, 2025),
    indicator = c("snap", "snap", "snap"),
    c_jt = c(NA, 0, 4),
    n_jt = c(8, 8, 20),
    stringsAsFactors = FALSE
  )

  out <- sitemix:::.sm_prepare_aggregate_input(x)

  expect_equal(out$flag_suppressed, c(TRUE, FALSE, FALSE))
  expect_equal(out$suppression_source, c("structural_na", "none", "none"))
  expect_equal(attr(out, "suppression_detection_path"), "structural")
  expect_equal(attr(out, "n_suppressed"), 1L)
  expect_true(attr(out, "denominator_observed_on_suppressed"))
})

test_that("publisher flag marks observed rows and keeps structural NA fallback", {
  x <- data.frame(
    site_id = c("A", "B", "C"),
    year = c(2025, 2025, 2025),
    indicator = c("snap", "snap", "snap"),
    c_jt = c(2, NA, 4),
    n_jt = c(8, 8, 20),
    sup = c("Y", NA, ""),
    stringsAsFactors = FALSE
  )

  out <- sitemix:::.sm_prepare_aggregate_input(
    x,
    suppression_col = "sup",
    suppression_flag_value = "Y"
  )

  expect_equal(out$flag_suppressed, c(TRUE, TRUE, FALSE))
  expect_equal(out$suppression_source, c("publisher_flag", "structural_na", "none"))
  expect_equal(attr(out, "suppression_detection_path"), "publisher_flag")
})

test_that("suppression_when predicate has highest priority", {
  x <- data.frame(
    site_id = c("A", "B", "C"),
    year = c(2025, 2025, 2025),
    indicator = c("snap", "snap", "snap"),
    c_jt = c(2, NA, 4),
    n_jt = c(50, 8, 20),
    sup = c("", "Y", ""),
    quality = c("low", "ok", "ok"),
    stringsAsFactors = FALSE
  )

  out <- sitemix:::.sm_prepare_aggregate_input(
    x,
    suppression_col = "sup",
    suppression_flag_value = "Y",
    suppression_when = function(C, N, flag) C == 2L & N == 50L
  )

  expect_equal(out$flag_suppressed, c(TRUE, FALSE, FALSE))
  expect_equal(out$suppression_source, c("user_predicate", "none", "none"))
  expect_equal(attr(out, "suppression_detection_path"), "user_predicate")
})

test_that("suppression_when validates type, length, and missingness", {
  x <- data.frame(
    site_id = c("A", "B"),
    year = c(2025, 2025),
    indicator = c("snap", "snap"),
    c_jt = c(1, 2),
    n_jt = c(10, 10),
    stringsAsFactors = FALSE
  )

  expect_error(
    sitemix:::.sm_prepare_aggregate_input(x, suppression_when = "bad"),
    class = "sitemix_error_invalid_suppression_when"
  )
  expect_error(
    sitemix:::.sm_prepare_aggregate_input(x, suppression_when = function(C, N) c(TRUE, FALSE, TRUE)),
    class = "sitemix_error_invalid_suppression_when"
  )
  expect_error(
    sitemix:::.sm_prepare_aggregate_input(x, suppression_when = function(C, N) c(TRUE, NA)),
    class = "sitemix_error_invalid_suppression_when"
  )

  all_suppressed <- sitemix:::.sm_prepare_aggregate_input(
    x,
    suppression_when = function(C, N) TRUE
  )
  expect_equal(all_suppressed$flag_suppressed, c(TRUE, TRUE))
})

test_that("suppression_col validates flag types and values", {
  x <- data.frame(
    site_id = "A",
    year = 2025,
    indicator = "snap",
    c_jt = 1,
    n_jt = 10,
    sup = 1,
    stringsAsFactors = FALSE
  )

  expect_error(
    sitemix:::.sm_prepare_aggregate_input(x, suppression_col = "sup"),
    class = "sitemix_error_invalid_suppression_col"
  )

  x$sup <- "Y"
  expect_error(
    sitemix:::.sm_prepare_aggregate_input(
      x,
      suppression_col = "sup",
      suppression_flag_value = NULL
    ),
    class = "sitemix_error_invalid_suppression_col"
  )
})

test_that("hidden denominator requires final suppression flag and conservative bound", {
  hidden <- data.frame(
    site_id = "A",
    year = 2025,
    indicator = "snap",
    c_jt = NA_integer_,
    n_jt = NA_integer_,
    hidden = TRUE,
    stringsAsFactors = FALSE
  )

  expect_error(
    sitemix:::.sm_prepare_aggregate_input(hidden),
    class = "sitemix_error_invalid_aggregate_row"
  )
  expect_error(
    sitemix:::.sm_prepare_aggregate_input(
      hidden,
      suppression_when = function(C, N) TRUE,
      suppression = "upper_bound",
      suppressed_n_strategy = "observed_n"
    ),
    class = "sitemix_error_invalid_suppressed_n"
  )
  expect_error(
    sitemix:::.sm_prepare_aggregate_input(
      hidden,
      suppression_when = function(C, N) TRUE,
      suppression = "upper_bound",
      suppressed_n_strategy = "worst_case_bound",
      suppressed_n_bound = 11,
      min_n = 10
    ),
    class = "sitemix_error_invalid_suppressed_n"
  )

  out <- sitemix:::.sm_prepare_aggregate_input(
    hidden,
    suppression_when = function(C, N) TRUE,
    suppression = "upper_bound",
    suppressed_n_strategy = "worst_case_bound",
    suppressed_n_bound = 1,
    min_n = 10
  )
  expect_true(out$flag_suppressed[[1]])
  expect_false(out$denominator_observed[[1]])
  expect_false(attr(out, "denominator_observed_on_suppressed"))
  expect_true(attr(out, "has_hidden_denominator"))
})

test_that("wide structural suppression detection is per pivoted indicator", {
  x <- data.frame(
    site_id = "A",
    year = 2025,
    c_jt_a = NA_integer_,
    c_jt_b = 3L,
    n_jt = 8L,
    stringsAsFactors = FALSE
  )

  out <- sitemix:::.sm_prepare_aggregate_input(x)

  expect_equal(out$indicator, c("a", "b"))
  expect_equal(out$flag_suppressed, c(TRUE, FALSE))
  expect_equal(out$suppression_source, c("structural_na", "none"))
})
