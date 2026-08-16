aggregate_d0_long <- function() {
  data.frame(
    site_id = c("S1", "S2", "S3"),
    year = c(2024L, 2024L, 2025L),
    indicator = c("absent", "absent", "absent"),
    c_jt = c(2L, 0L, 5L),
    n_jt = c(4L, 2L, 5L),
    stringsAsFactors = FALSE
  )
}

aggregate_d0_counts <- function() {
  data.frame(
    site_id = c("S1", "S2", "S3"),
    year = c(2024L, 2024L, 2025L),
    n_jt = c(4L, 2L, 5L),
    c_jt_absent = c(2L, 0L, 5L),
    stringsAsFactors = FALSE
  )
}

expect_d0_equivalent_to_counts <- function(aggregate, counts, ..., compare_v = FALSE) {
  d0 <- sitemix::sm_estimate_from_aggregates(
    aggregate,
    family = "binomial",
    indicator = "absent",
    ...
  )
  fc <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    ...
  )

  compare_cols <- setdiff(names(fc), "input_mode")
  expect_equal(
    as.data.frame(d0[compare_cols]),
    as.data.frame(fc[compare_cols]),
    ignore_attr = TRUE
  )
  expect_equal(d0$input_mode, rep("aggregate", nrow(d0)))
  expect_equal(fc$input_mode, rep("counts_full_suff", nrow(fc)))
  expect_equal(attr(d0, "aggregate_case"), "D0")
  expect_equal(attr(d0, "family"), "binomial")
  expect_true(validate.sitemix_estimates(d0))

  if (isTRUE(compare_v)) {
    expect_true("V" %in% names(d0))
    expect_equal(length(d0$V), nrow(d0))
    for (i in seq_len(nrow(d0))) {
      expect_equal(as.matrix(d0$V[[i]]), as.matrix(fc$V[[i]]))
      expect_equal(d0$V[[i]]$estimate_scale, fc$V[[i]]$estimate_scale)
      expect_equal(d0$V[[i]]$vcov_scale, fc$V[[i]]$vcov_scale)
    }
  }

  invisible(d0)
}

test_that("aggregate D0 equals Scenario A from-counts for complete rows", {
  expect_d0_equivalent_to_counts(
    aggregate_d0_long(),
    aggregate_d0_counts(),
    min_n = 3L
  )
})

test_that("aggregate D0 preserves scalar options from Scenario A", {
  aggregate <- aggregate_d0_long()
  counts <- aggregate_d0_counts()

  expect_d0_equivalent_to_counts(
    aggregate,
    counts,
    vst = "none",
    boundary_method = "agresti_coull",
    min_n = 1L
  )
  expect_d0_equivalent_to_counts(
    data.frame(
      site_id = c("S1", "S2"),
      year = c(2024L, 2024L),
      indicator = c("absent", "absent"),
      c_jt = c(1L, 2L),
      n_jt = c(4L, 5L),
      stringsAsFactors = FALSE
    ),
    data.frame(
      site_id = c("S1", "S2"),
      year = c(2024L, 2024L),
      n_jt = c(4L, 5L),
      c_jt_absent = c(1L, 2L)
    ),
    vst = "logit",
    min_n = 1L
  )
  expect_d0_equivalent_to_counts(
    aggregate,
    counts,
    anscombe = TRUE,
    min_n = 1L
  )
  expect_d0_equivalent_to_counts(
    aggregate,
    counts,
    vst = "none",
    fpc = 100,
    min_n = 1L
  )
  expect_d0_equivalent_to_counts(
    aggregate,
    counts,
    vjt = TRUE,
    min_n = 1L,
    compare_v = TRUE
  )
})

test_that("aggregate D0 Anscombe boundary rows match from-counts exactly", {
  aggregate <- data.frame(
    site_id = c("B0", "BI", "B1"),
    year = c(2025L, 2025L, 2025L),
    indicator = c("absent", "absent", "absent"),
    c_jt = c(0L, 5L, 10L),
    n_jt = c(10L, 10L, 10L),
    stringsAsFactors = FALSE
  )
  counts <- data.frame(
    site_id = c("B0", "BI", "B1"),
    year = c(2025L, 2025L, 2025L),
    n_jt = c(10L, 10L, 10L),
    c_jt_absent = c(0L, 5L, 10L)
  )

  for (boundary_method in c("wilson_floor", "none")) {
    out <- expect_d0_equivalent_to_counts(
      aggregate,
      counts,
      anscombe = TRUE,
      boundary_method = boundary_method,
      vjt = TRUE,
      min_n = 1L,
      compare_v = TRUE
    )

    row_index <- match(out$site_id, aggregate$site_id)
    c_jt <- aggregate$c_jt[row_index]
    n_jt <- aggregate$n_jt[row_index]
    expect_equal(out$theta_raw, c_jt / n_jt, tolerance = 1e-12)
    expect_equal(out$theta_hat, asin(sqrt((c_jt + 3 / 8) / (n_jt + 3 / 4))), tolerance = 1e-12)
    expect_equal(out$n_eff, n_jt + 0.5)
    expect_equal(out$se, rep(1 / (2 * sqrt(10.5)), 3), tolerance = 1e-12)
    expect_equal(out$var_method, rep("arcsine_anscombe", 3))
    expect_equal(out$flag_zero_cell, c_jt == 0L | c_jt == n_jt)
    expect_equal(
      vapply(out$V, function(V) as.matrix(V)[1, 1], numeric(1)),
      out$se^2,
      tolerance = 1e-12
    )
    expect_true(all(vapply(out$V, function(V) identical(V$estimate_scale, "arcsine_anscombe"), logical(1))))
  }
})

test_that("aggregate D0 accepts mapped publisher-style columns", {
  publisher <- data.frame(
    cds = c("S1", "S2"),
    reportingyear = c(2025, 2025),
    ind = c("absent", "absent"),
    currnumer = c(3L, 4L),
    currdenom = c(10L, 20L),
    stringsAsFactors = FALSE
  )
  counts <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2025L, 2025L),
    n_jt = c(10L, 20L),
    c_jt_absent = c(3L, 4L)
  )

  out <- sitemix::sm_estimate_from_aggregates(
    publisher,
    family = "binomial",
    indicator = "absent",
    id_cols = c("cds", "reportingyear"),
    numerator_col = "currnumer",
    denominator_col = "currdenom",
    indicator_col = "ind",
    min_n = 1L
  )
  fc <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    min_n = 1L
  )

  compare_cols <- setdiff(names(fc), "input_mode")
  expect_equal(as.data.frame(out[compare_cols]), as.data.frame(fc[compare_cols]), ignore_attr = TRUE)
  expect_equal(out$input_mode, c("aggregate", "aggregate"))
})

test_that("aggregate D0 creates a long-form key from a scalar indicator", {
  d0 <- data.frame(
    site_id = c("S1", "S2"),
    year = 2024L,
    cases = c(2L, 3L),
    total = c(10L, 12L)
  )

  out <- sitemix::sm_estimate_from_aggregates(
    d0,
    family = "binomial",
    indicator = "rate",
    numerator_col = "cases",
    denominator_col = "total",
    aggregate_case = "D0",
    min_n = 1L
  )

  expect_s3_class(out, "sitemix_estimates")
  expect_identical(out$indicator, c("rate", "rate"))
  expect_equal(out$theta_raw, c(0.2, 0.25))
  expect_identical(attr(out, "aggregate_case"), "D0")
})

test_that("aggregate D0 rejects D1 inputs", {
  suppressed <- data.frame(
    site_id = "S1",
    year = 2025L,
    indicator = "absent",
    c_jt = NA_integer_,
    n_jt = 8L,
    stringsAsFactors = FALSE
  )
  out <- sitemix::sm_estimate_from_aggregates(
    suppressed,
    family = "binomial",
    indicator = "absent"
  )
  expect_equal(out$var_method, "suppressed_drop")
  expect_true(out$flag_suppressed)

  d1 <- data.frame(
    site_id = "S1",
    year = 2025L,
    c_jt_absent = 1L,
    c_jt_present = 2L,
    n_jt = 10L
  )
  expect_error(
    sitemix::sm_estimate(
      d1,
      family = "binomial",
      from_aggregates = TRUE
    ),
    class = "sitemix_error_ambiguous_dispatch"
  )
})

test_that("aggregate D0 does not leak internal aggregate metadata columns", {
  out <- sitemix::sm_estimate_from_aggregates(
    aggregate_d0_long(),
    family = "binomial",
    indicator = "absent",
    min_n = 1L
  )

  expect_equal(names(out), sitemix:::.sm_sitemix_columns)
  expect_false(any(c("denominator_observed", "aggregate_form", "suppression_source") %in% names(out)))
  expect_s3_class(out, "sitemix_estimates")
  expect_true(validate.sitemix_estimates(out))
})
