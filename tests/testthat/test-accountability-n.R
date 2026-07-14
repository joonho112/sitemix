# Tests for the public `accountability_n` argument (Gate C Decision 2).
#
# Emphasis: fail-fast validation. Invalid values must abort at the validator
# layer with the `sitemix_error_invalid_accountability_n` leaf class BEFORE any
# engine work runs, and the error message must name the actual offending value.

test_that("accountability_n defaults to 30L on the public signature", {
  expect_true("accountability_n" %in% names(formals(sitemix::sm_estimate)))
  expect_equal(eval(formals(sitemix::sm_estimate)$accountability_n), 30L)

  expect_true("accountability_n" %in% names(formals(sitemix::sm_estimate_from_counts)))
  expect_equal(eval(formals(sitemix::sm_estimate_from_counts)$accountability_n), 30L)
})

test_that("accountability_n appears in .sm_validate_arguments signature with default 30L", {
  expect_true("accountability_n" %in% names(formals(sitemix:::.sm_validate_arguments)))
  expect_equal(eval(formals(sitemix:::.sm_validate_arguments)$accountability_n), 30L)
})

test_that("the new error class is registered as an argument-branch leaf", {
  expect_true(
    "sitemix_error_invalid_accountability_n" %in% sitemix:::.sm_argument_error_classes
  )
  expect_equal(
    sitemix:::.sm_error_branch("sitemix_error_invalid_accountability_n"),
    "sitemix_error_argument"
  )
  expect_setequal(
    sitemix:::.sm_error_classes("sitemix_error_invalid_accountability_n"),
    c(
      "sitemix_error_invalid_accountability_n",
      "sitemix_error_argument",
      "sitemix_error"
    )
  )
})

test_that(".sm_validate_accountability_n accepts positive integer scalars", {
  expect_true(sitemix:::.sm_validate_accountability_n(30L))
  expect_true(sitemix:::.sm_validate_accountability_n(1L))
  expect_true(sitemix:::.sm_validate_accountability_n(100L))
  # Whole-number numeric is allowed (mirrors `.sm_validate_min_n`).
  expect_true(sitemix:::.sm_validate_accountability_n(30))
  expect_true(sitemix:::.sm_validate_accountability_n(50))
})

test_that(".sm_validate_accountability_n rejects NA / negative / zero", {
  expect_error(sitemix:::.sm_validate_accountability_n(NA),
               class = "sitemix_error_invalid_accountability_n")
  expect_error(sitemix:::.sm_validate_accountability_n(NA_integer_),
               class = "sitemix_error_invalid_accountability_n")
  expect_error(sitemix:::.sm_validate_accountability_n(NA_real_),
               class = "sitemix_error_invalid_accountability_n")
  expect_error(sitemix:::.sm_validate_accountability_n(-1L),
               class = "sitemix_error_invalid_accountability_n")
  expect_error(sitemix:::.sm_validate_accountability_n(0L),
               class = "sitemix_error_invalid_accountability_n")
  expect_error(sitemix:::.sm_validate_accountability_n(-30),
               class = "sitemix_error_invalid_accountability_n")
})

test_that(".sm_validate_accountability_n rejects non-integer and non-numeric", {
  expect_error(sitemix:::.sm_validate_accountability_n(30.5),
               class = "sitemix_error_invalid_accountability_n")
  expect_error(sitemix:::.sm_validate_accountability_n(1.1),
               class = "sitemix_error_invalid_accountability_n")
  expect_error(sitemix:::.sm_validate_accountability_n("30"),
               class = "sitemix_error_invalid_accountability_n")
  expect_error(sitemix:::.sm_validate_accountability_n(TRUE),
               class = "sitemix_error_invalid_accountability_n")
  expect_error(sitemix:::.sm_validate_accountability_n(list(30L)),
               class = "sitemix_error_invalid_accountability_n")
})

test_that(".sm_validate_accountability_n rejects non-scalar vectors and non-finite", {
  expect_error(sitemix:::.sm_validate_accountability_n(c(30L, 40L)),
               class = "sitemix_error_invalid_accountability_n")
  expect_error(sitemix:::.sm_validate_accountability_n(integer(0)),
               class = "sitemix_error_invalid_accountability_n")
  expect_error(sitemix:::.sm_validate_accountability_n(Inf),
               class = "sitemix_error_invalid_accountability_n")
  expect_error(sitemix:::.sm_validate_accountability_n(-Inf),
               class = "sitemix_error_invalid_accountability_n")
  expect_error(sitemix:::.sm_validate_accountability_n(NaN),
               class = "sitemix_error_invalid_accountability_n")
})

test_that("the validator's error message names the actual offending value", {
  err <- tryCatch(
    sitemix:::.sm_validate_accountability_n(-7L),
    sitemix_error_invalid_accountability_n = function(e) e
  )
  expect_s3_class(err, "sitemix_error_invalid_accountability_n")
  expect_true(grepl("-7", err$actual, fixed = TRUE))
})

test_that("sm_estimate default accountability_n=30L flags small site-years (binomial)", {
  df <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    n_jt = c(10L, 50L),
    c_jt_absent = c(2L, 10L)
  )
  out <- sitemix::sm_estimate(
    df, family = "binomial", indicator = "absent", from_counts = TRUE
  )
  expect_s3_class(out, "sitemix_estimates")
  expect_true("flag_below_accountability" %in% names(out))
  expect_true(out$flag_below_accountability[out$site_id == "S1"])
  expect_false(out$flag_below_accountability[out$site_id == "S2"])
})

test_that("sm_estimate custom accountability_n flows through (binomial)", {
  df <- data.frame(
    site_id = c("S1", "S2", "S3"),
    year = c(2024L, 2024L, 2024L),
    n_jt = c(20L, 50L, 200L),
    c_jt_absent = c(2L, 10L, 50L)
  )
  out_low <- sitemix::sm_estimate(
    df, family = "binomial", indicator = "absent", from_counts = TRUE,
    accountability_n = 5L
  )
  expect_true(all(!out_low$flag_below_accountability))

  out_high <- sitemix::sm_estimate(
    df, family = "binomial", indicator = "absent", from_counts = TRUE,
    accountability_n = 100L
  )
  expect_true(out_high$flag_below_accountability[out_high$site_id == "S1"])
  expect_true(out_high$flag_below_accountability[out_high$site_id == "S2"])
  expect_false(out_high$flag_below_accountability[out_high$site_id == "S3"])
})

test_that("sm_estimate custom accountability_n flows through (multivariate)", {
  df <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    n_jt = c(15L, 40L),
    c_jt_snap = c(3L, 12L),
    c_jt_frpm = c(5L, 18L),
    c_jt_snap_frpm = c(2L, 8L)
  )
  out <- sitemix::sm_estimate(
    df, family = "multivariate", indicators = c("snap", "frpm"),
    from_counts = TRUE, accountability_n = 20L
  )
  expect_s3_class(out, "sitemix_estimates")
  expect_true(all(out$flag_below_accountability[out$site_id == "S1"]))
  expect_true(all(!out$flag_below_accountability[out$site_id == "S2"]))
})

test_that("sm_estimate custom accountability_n flows through (multinomial)", {
  df <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    n_jt = c(12L, 60L),
    c_jt_eng = c(6L, 30L),
    c_jt_spa = c(4L, 20L),
    c_jt_oth = c(2L, 10L)
  )
  out <- sitemix::sm_estimate(
    df, family = "multinomial", indicators = c("eng", "spa", "oth"),
    from_counts = TRUE, accountability_n = 25L
  )
  expect_s3_class(out, "sitemix_estimates")
  expect_true(all(out$flag_below_accountability[out$site_id == "S1"]))
  expect_true(all(!out$flag_below_accountability[out$site_id == "S2"]))
})

test_that("sm_estimate_from_counts wrapper accepts and forwards accountability_n", {
  df <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    n_jt = c(15L, 60L),
    c_jt_absent = c(3L, 12L)
  )
  direct <- sitemix::sm_estimate(
    df, family = "binomial", indicator = "absent",
    from_counts = TRUE, accountability_n = 25L
  )
  wrapped <- sitemix::sm_estimate_from_counts(
    df, family = "binomial", indicator = "absent", accountability_n = 25L
  )
  expect_equal(
    as.data.frame(direct), as.data.frame(wrapped), ignore_attr = TRUE
  )
  expect_true(wrapped$flag_below_accountability[wrapped$site_id == "S1"])
  expect_false(wrapped$flag_below_accountability[wrapped$site_id == "S2"])
})

test_that("sm_estimate aborts with the leaf class on invalid accountability_n", {
  df <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    n_jt = c(10L, 50L),
    c_jt_absent = c(2L, 10L)
  )
  expect_error(
    sitemix::sm_estimate(df, family = "binomial", indicator = "absent",
                         from_counts = TRUE, accountability_n = NA),
    class = "sitemix_error_invalid_accountability_n"
  )
  expect_error(
    sitemix::sm_estimate(df, family = "binomial", indicator = "absent",
                         from_counts = TRUE, accountability_n = -1L),
    class = "sitemix_error_invalid_accountability_n"
  )
  expect_error(
    sitemix::sm_estimate(df, family = "binomial", indicator = "absent",
                         from_counts = TRUE, accountability_n = 0L),
    class = "sitemix_error_invalid_accountability_n"
  )
  expect_error(
    sitemix::sm_estimate(df, family = "binomial", indicator = "absent",
                         from_counts = TRUE, accountability_n = 30.5),
    class = "sitemix_error_invalid_accountability_n"
  )
  expect_error(
    sitemix::sm_estimate(df, family = "binomial", indicator = "absent",
                         from_counts = TRUE, accountability_n = "30"),
    class = "sitemix_error_invalid_accountability_n"
  )
  expect_error(
    sitemix::sm_estimate(df, family = "binomial", indicator = "absent",
                         from_counts = TRUE, accountability_n = c(30L, 40L)),
    class = "sitemix_error_invalid_accountability_n"
  )
})

test_that("sm_estimate_from_counts aborts with the leaf class on invalid accountability_n", {
  df <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    n_jt = c(10L, 50L),
    c_jt_absent = c(2L, 10L)
  )
  expect_error(
    sitemix::sm_estimate_from_counts(df, family = "binomial",
                                     indicator = "absent",
                                     accountability_n = NA),
    class = "sitemix_error_invalid_accountability_n"
  )
  expect_error(
    sitemix::sm_estimate_from_counts(df, family = "binomial",
                                     indicator = "absent",
                                     accountability_n = -7L),
    class = "sitemix_error_invalid_accountability_n"
  )
  expect_error(
    sitemix::sm_estimate_from_counts(df, family = "binomial",
                                     indicator = "absent",
                                     accountability_n = "oops"),
    class = "sitemix_error_invalid_accountability_n"
  )
})

test_that("invalid accountability_n fails before engine work (fail-fast)", {
  good_df <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    n_jt = c(10L, 50L),
    c_jt_absent = c(2L, 10L)
  )
  err <- tryCatch(
    sitemix::sm_estimate(
      good_df, family = "binomial", indicator = "absent",
      from_counts = TRUE, accountability_n = -1L
    ),
    sitemix_error_invalid_accountability_n = function(e) e
  )
  expect_s3_class(err, "sitemix_error_invalid_accountability_n")
  expect_s3_class(err, "sitemix_error_argument")
  expect_s3_class(err, "sitemix_error")
  expect_true(grepl("-1", err$actual, fixed = TRUE))
})
