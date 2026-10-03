test_that("multinomial covariance helper builds full-simplex raw covariance", {
  categories <- c("eng", "spa", "oth")
  cov <- sitemix:::.sm_multinomial_cov_from_counts(
    n = 10L,
    category_counts = c(6L, 3L, 1L),
    categories = categories,
    boundary_method = "none"
  )
  expected <- matrix(
    c(
      0.024, -0.018, -0.006,
      -0.018, 0.021, -0.003,
      -0.006, -0.003, 0.009
    ),
    3,
    3,
    byrow = TRUE,
    dimnames = list(categories, categories)
  )

  expect_equal(cov$theta_raw, c(eng = 0.6, spa = 0.3, oth = 0.1))
  expect_equal(cov$V_raw, expected, tolerance = 1e-12)
  expect_equal(as.vector(cov$V_raw %*% rep(1, 3)), rep(0, 3), tolerance = 1e-12)
  expect_true(all(cov$V_raw[row(cov$V_raw) != col(cov$V_raw)] <= 0))
  expect_equal(cov$matrix_rank, 2L)
  expect_equal(cov$positive_support, 3L)
  expect_equal(cov$matrix_boundary_rule, "none")
  expect_equal(unname(cov$scalar_correction_rule), rep("none", 3))
})

test_that("multinomial covariance helper preserves simplex matrix at zero categories", {
  categories <- c("eng", "spa", "oth")
  cov <- sitemix:::.sm_multinomial_cov_from_counts(
    n = 6L,
    category_counts = c(0L, 4L, 2L),
    categories = categories,
    boundary_method = "wilson_floor"
  )
  expected <- matrix(
    c(
      0, 0, 0,
      0, 1 / 27, -1 / 27,
      0, -1 / 27, 1 / 27
    ),
    3,
    3,
    byrow = TRUE,
    dimnames = list(categories, categories)
  )

  expect_equal(cov$V_raw, expected, tolerance = 1e-12)
  expect_equal(as.vector(cov$V_raw %*% rep(1, 3)), rep(0, 3), tolerance = 1e-12)
  expect_equal(cov$se_raw[[1]], sitemix:::.sm_wilson_se(0, 6), tolerance = 1e-12)
  expect_equal(cov$V_raw["eng", "eng"], 0)
  expect_equal(cov$matrix_boundary_rule, "simplex_preserve")
  expect_equal(
    unname(cov$scalar_correction_rule),
    c("wilson_boundary_surrogate", "none", "none")
  )
  expect_equal(cov$matrix_rank, 1L)
  expect_equal(cov$positive_support, 2L)
})

test_that("multinomial zero-category fixture keeps scalar Wilson out of V", {
  categories <- c("eng", "spa", "oth")
  cov <- sitemix:::.sm_multinomial_cov_from_counts(
    n = 5L,
    category_counts = c(0L, 2L, 3L),
    categories = categories,
    boundary_method = "wilson_floor"
  )
  expected <- matrix(
    c(
      0, 0, 0,
      0, 0.048, -0.048,
      0, -0.048, 0.048
    ),
    3,
    3,
    byrow = TRUE,
    dimnames = list(categories, categories)
  )

  expect_equal(cov$V_raw, expected, tolerance = 1e-12)
  expect_equal(unname(rowSums(cov$V_raw)), rep(0, 3), tolerance = 1e-12)
  expect_equal(cov$matrix_rank, 1L)
  expect_equal(cov$positive_support, 2L)
  expect_equal(cov$matrix_boundary_rule, "simplex_preserve")
  expect_equal(
    unname(cov$scalar_correction_rule),
    c("wilson_boundary_surrogate", "none", "none")
  )
  expect_gt(cov$se_raw[["eng"]], 0)
  expect_equal(cov$V_raw["eng", "eng"], 0)
})

test_that("multinomial covariance helper records degenerate full-category support", {
  categories <- c("eng", "spa", "oth")
  cov <- sitemix:::.sm_multinomial_cov_from_counts(
    n = 6L,
    category_counts = c(6L, 0L, 0L),
    categories = categories,
    boundary_method = "wilson_floor"
  )

  expect_equal(cov$V_raw, matrix(0, 3, 3, dimnames = list(categories, categories)))
  expect_equal(cov$matrix_rank, 0L)
  expect_equal(cov$positive_support, 1L)
  expect_equal(cov$matrix_boundary_rule, "simplex_preserve")
  expect_equal(
    unname(cov$scalar_correction_rule),
    rep("wilson_boundary_surrogate", 3)
  )
  expect_true(all(cov$se_raw > 0))
})

test_that("multinomial covariance helper applies FPC to the full simplex matrix", {
  categories <- c("eng", "spa", "oth")
  base <- sitemix:::.sm_multinomial_cov_from_counts(
    n = 10L,
    category_counts = c(6L, 3L, 1L),
    categories = categories,
    boundary_method = "none"
  )
  fpc <- sitemix:::.sm_multinomial_cov_from_counts(
    n = 10L,
    category_counts = c(6L, 3L, 1L),
    categories = categories,
    boundary_method = "none",
    fpc = 25
  )

  expect_equal(fpc$V_raw, base$V_raw * ((25 - 10) / (25 - 1)), tolerance = 1e-12)
  expect_equal(as.vector(fpc$V_raw %*% rep(1, 3)), rep(0, 3), tolerance = 1e-12)
  expect_equal(fpc$se_raw, base$se_raw * sqrt((25 - 10) / (25 - 1)), tolerance = 1e-12)
})

test_that("multinomial covariance helper constructs multinomial sm_vcov metadata", {
  categories <- c("eng", "spa", "oth")
  cov <- sitemix:::.sm_multinomial_cov_from_counts(
    n = 10L,
    category_counts = c(6L, 3L, 1L),
    categories = categories,
    boundary_method = "none"
  )
  v <- sitemix:::.sm_multinomial_vcov_from_cov(
    cov,
    site_id = "S1",
    year = 2024L,
    categories = categories,
    estimate_scale = "arcsine"
  )

  expect_s3_class(v, "sm_vcov")
  expect_equal(as.matrix(v), cov$V_raw)
  expect_equal(v$family, "multinomial")
  expect_equal(v$vcov_method, "multinomial")
  expect_equal(v$estimate_scale, "arcsine")
  expect_equal(v$vcov_scale, "raw")
  expect_equal(v$matrix_boundary_rule, "none")
  expect_equal(v$matrix_rank, 2L)
  expect_equal(v$positive_support, 3L)
  expect_equal(v$n_jt, 10L)
  expect_equal(v$n_eff, 10)
})

test_that("multinomial covariance helper rejects invalid count geometry", {
  expect_error(
    sitemix:::.sm_multinomial_cov_from_counts(
      n = 5L,
      category_counts = c(2L, 2L),
      categories = c("A", "B")
    ),
    class = "sitemix_error_input_indicator_count"
  )
  expect_error(
    sitemix:::.sm_multinomial_cov_from_counts(
      n = 5L,
      category_counts = c(2L, 2L, 1L),
      categories = c("A", "B")
    ),
    class = "sitemix_error_input_indicator_count"
  )
  expect_error(
    sitemix:::.sm_multinomial_cov_from_counts(
      n = 5L,
      category_counts = c(0L, 5L),
      categories = c("A", "B"),
      boundary_method = "agresti_coull"
    ),
    class = "sitemix_error_estimate_vcov_invariant"
  )
  expect_error(
    sitemix:::.sm_multinomial_cov_from_counts(
      n = 5L,
      category_counts = c(2L, 3L),
      categories = c("A", "A")
    ),
    class = "sitemix_error_invalid_indicators"
  )
  census <- sitemix:::.sm_multinomial_cov_from_counts(
    n = 5L,
    category_counts = c(2L, 3L),
    categories = c("A", "B"),
    fpc = 5
  )
  expect_equal(census$V_raw, matrix(0, 2, 2, dimnames = list(c("A", "B"), c("A", "B"))))
  expect_identical(census$sampling_design, "SRSWOR")
})

test_that("multinomial covariance matches hand-computed and formula-derived K=2 values", {
  n <- 8L
  category_counts <- c(3L, 5L)
  categories <- c("A", "B")
  p <- c(3 / 8, 5 / 8)

  # Formula-derived expected: V = (diag(p) - p p') / n.
  expected_V <- (diag(p) - tcrossprod(p)) / n
  dimnames(expected_V) <- list(categories, categories)

  # Sanity: hand-computed literals from blueprint Ch. 14 match the formula.
  expect_equal(
    as.numeric(expected_V),
    c(15 / 512, -15 / 512, -15 / 512, 15 / 512),
    tolerance = 1e-12
  )

  result <- sitemix:::.sm_multinomial_cov_from_counts(
    n = n,
    category_counts = category_counts,
    categories = categories,
    boundary_method = "wilson_floor",
    fpc = NULL
  )

  expect_equal(result$V_raw, expected_V, tolerance = 1e-12)
  expect_identical(dimnames(result$V_raw), list(categories, categories))
  expect_equal(rowSums(result$V_raw), c(A = 0, B = 0), tolerance = 1e-12)
  expect_gte(min(eigen(result$V_raw, only.values = TRUE)$values), -1e-10)
  expect_identical(result$matrix_rank, 1L)
  expect_identical(result$positive_support, 2L)
  expect_equal(result$theta_raw, c(A = 3 / 8, B = 5 / 8))
  expect_true(all(!result$boundary))
})
