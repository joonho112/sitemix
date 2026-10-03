multinomial_count_oracle <- function(counts, population_size = NULL, corrected = FALSE) {
  counts <- as.numeric(counts)
  n <- sum(counts)
  p <- counts / n
  kernel <- diag(p) - tcrossprod(p)

  if (isTRUE(corrected)) {
    if (is.null(population_size)) {
      return(kernel / (n - 1))
    }
    return((population_size - n) * kernel / (population_size * (n - 1)))
  }

  if (is.null(population_size)) {
    return(kernel / n)
  }
  q <- if (population_size == n) {
    0
  } else {
    (population_size - n) / (population_size - 1)
  }
  q * kernel / n
}

expect_multinomial_geometry <- function(result, counts, expected_matrix) {
  support <- sum(counts > 0)
  expect_equal(result$V_raw, expected_matrix, tolerance = 1e-14)
  expect_identical(result$positive_support, as.integer(support))
  expect_identical(result$matrix_rank, as.integer(max(0, support - 1)))
  expect_equal(
    as.vector(result$V_raw %*% rep(1, length(counts))),
    rep(0, length(counts)),
    tolerance = 1e-14
  )
  expect_equal(result$V_raw, t(result$V_raw), tolerance = 1e-14)
  expect_gte(
    min(eigen(result$V_raw, symmetric = TRUE, only.values = TRUE)$values),
    -1e-12
  )
}

test_that("independent count oracle fixes Scenario C whole-matrix formulas", {
  categories <- c("A", "B", "C")
  counts <- c(6L, 3L, 1L)

  cases <- list(
    list(N = NULL, corrected = FALSE),
    list(N = 25, corrected = FALSE),
    list(N = NULL, corrected = TRUE),
    list(N = 25, corrected = TRUE)
  )
  for (case in cases) {
    result <- sitemix:::.sm_multinomial_cov_from_counts(
      n = sum(counts),
      category_counts = counts,
      categories = categories,
      boundary_method = "none",
      bias_correction = if (case$corrected) "binomial_bc" else NULL,
      fpc = case$N
    )
    expected <- multinomial_count_oracle(
      counts,
      population_size = case$N,
      corrected = case$corrected
    )
    dimnames(expected) <- list(categories, categories)
    expect_multinomial_geometry(result, counts, expected)
  }
})

test_that("analytic rank is positive support minus one across support patterns", {
  categories <- c("A", "B", "C")
  fixtures <- list(
    full = c(3L, 2L, 1L),
    one_zero = c(0L, 4L, 2L),
    degenerate = c(6L, 0L, 0L)
  )

  for (counts in fixtures) {
    result <- sitemix:::.sm_multinomial_cov_from_counts(
      n = sum(counts),
      category_counts = counts,
      categories = categories,
      boundary_method = "none"
    )
    expected <- multinomial_count_oracle(counts)
    dimnames(expected) <- list(categories, categories)
    expect_multinomial_geometry(result, counts, expected)
  }
})

test_that("census keeps analytic support rank while sampling covariance is zero", {
  categories <- c("A", "B", "C")
  counts <- c(3L, 2L, 1L)
  result <- sitemix:::.sm_multinomial_cov_from_counts(
    n = sum(counts),
    category_counts = counts,
    categories = categories,
    boundary_method = "none",
    fpc = sum(counts)
  )
  expected <- matrix(0, 3, 3, dimnames = list(categories, categories))

  expect_multinomial_geometry(result, counts, expected)
  expect_identical(result$matrix_rank, 2L)
  expect_identical(sitemix:::.sm_matrix_rank(result$V_raw), 0L)

  V <- sitemix:::.sm_multinomial_vcov_from_cov(
    result,
    site_id = "S1",
    year = 2024L,
    categories = categories,
    estimate_scale = "none"
  )
  expect_true(validate.sm_vcov(V))

  wrong_rank <- V
  wrong_rank$matrix_rank <- 0L
  expect_error(
    validate.sm_vcov(wrong_rank),
    class = "sitemix_error_vcov_invariant"
  )

  wrong_support <- V
  wrong_support$positive_support <- 4L
  expect_error(
    validate.sm_vcov(wrong_support),
    class = "sitemix_error_vcov_invariant"
  )
})

test_that("zero-support boundary keeps scalar and global matrix provenance separate", {
  counts <- data.frame(
    site_id = "S1", year = 2024L, n_jt = 5L,
    c_jt_A = 0L, c_jt_B = 2L, c_jt_C = 3L
  )
  estimate <- function(population_size = NULL) {
    sm_estimate_from_counts(
      counts,
      family = "multinomial",
      indicators = c("A", "B", "C"),
      vst = "none",
      boundary_method = "none",
      bias_correction = "binomial_bc",
      vjt = TRUE,
      min_n = 1L,
      fpc = population_size
    )
  }

  no_fpc <- estimate()
  expect_true(validate.sitemix_estimates(no_fpc))
  expect_identical(
    no_fpc$V[[1L]]$variance_rule,
    rep("design_corrected", 3)
  )

  out <- estimate(10)
  V <- out$V[[1L]]
  q <- (10 - 5) / (10 - 1)
  design_multiplier <- (10 - 5) / 10

  expect_true(validate.sitemix_estimates(out))
  expect_identical(out$var_method, c("binomial", "binomial_bc", "binomial_bc"))
  expect_identical(V$scalar_correction_rule, c("none", "binomial_bc", "binomial_bc"))
  expect_identical(V$variance_rule, rep("design_corrected", 3))
  expect_equal(V$variance_multiplier_applied, rep(design_multiplier, 3))
  expect_equal(out$variance_multiplier_applied, c(q, design_multiplier, design_multiplier))
  expect_equal(unname(diag(as.matrix(V))), out$se_raw^2, tolerance = 1e-14)
  expect_equal(as.matrix(V)["A", ], c(A = 0, B = 0, C = 0))
})

test_that("Wilson scalar surrogate remains separate from simplex covariance", {
  counts <- data.frame(
    site_id = "S1", year = 2024L, n_jt = 5L,
    c_jt_A = 0L, c_jt_B = 2L, c_jt_C = 3L
  )
  out <- sm_estimate_from_counts(
    counts,
    family = "multinomial",
    indicators = c("A", "B", "C"),
    vst = "none",
    boundary_method = "wilson_floor",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L,
    fpc = 10
  )
  V <- out$V[[1L]]

  expect_true(validate.sitemix_estimates(out))
  expect_gt(out$se_raw[[1L]], 0)
  expect_equal(as.matrix(V)["A", "A"], 0)
  expect_identical(V$diag_contract, "row_se_raw_squared_except_boundary_surrogates")
  expect_identical(V$matrix_boundary_rule, "simplex_preserve")
  expect_identical(V$scalar_correction_rule[[1L]], "wilson_boundary_surrogate")
  expect_identical(V$variance_rule, rep("design_corrected", 3))
  expect_equal(as.vector(as.matrix(V) %*% rep(1, 3)), rep(0, 3), tolerance = 1e-14)
})

repeat_multinomial_v <- function(out, mutate) {
  V <- mutate(out$V[[1L]])
  out$V <- rep(list(V), nrow(out))
  out
}

expect_multinomial_tamper_error <- function(out, mutate) {
  expect_error(
    validate.sitemix_estimates(repeat_multinomial_v(out, mutate)),
    class = "sitemix_error_estimate_vcov_invariant"
  )
}

test_that("Scenario C package contract rejects repeated metadata tampering", {
  out <- sm_estimate_from_counts(
    data.frame(
      site_id = "S1", year = 2024L, n_jt = 5L,
      c_jt_A = 0L, c_jt_B = 2L, c_jt_C = 3L
    ),
    family = "multinomial",
    indicators = c("A", "B", "C"),
    vst = "none",
    boundary_method = "wilson_floor",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L,
    fpc = 10
  )

  expect_multinomial_tamper_error(out, function(V) {
    V$positive_support <- 1L
    V$matrix_rank <- 0L
    V
  })
  expect_multinomial_tamper_error(out, function(V) {
    V$vcov_scale <- "reference_raw"
    V
  })
  expect_multinomial_tamper_error(out, function(V) {
    V$matrix_boundary_rule <- "none"
    V
  })
  expect_multinomial_tamper_error(out, function(V) {
    V$scalar_correction_rule <- c("none", "binomial_bc", "binomial_bc")
    V
  })
  expect_multinomial_tamper_error(out, function(V) {
    V$n_jt <- 6L
    V
  })
  expect_multinomial_tamper_error(out, function(V) {
    V$n_eff <- 6
    V
  })
  expect_multinomial_tamper_error(out, function(V) {
    V$diag_contract <- "not_checked"
    V$matrix[, ] <- 0
    V
  })

  full_support <- sm_estimate_from_counts(
    data.frame(
      site_id = "S2", year = 2024L, n_jt = 6L,
      c_jt_A = 3L, c_jt_B = 2L, c_jt_C = 1L
    ),
    family = "multinomial",
    indicators = c("A", "B", "C"),
    vst = "none",
    vjt = TRUE,
    min_n = 1L
  )
  full_support$flag_zero_cell[[1L]] <- TRUE
  expect_error(
    validate.sitemix_estimates(full_support),
    class = "sitemix_error_estimate_vcov_invariant"
  )

  missing_zero_flag <- out
  missing_zero_flag$flag_zero_cell[[1L]] <- FALSE
  expect_error(
    validate.sitemix_estimates(missing_zero_flag),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})

test_that("Scenario C boundary exception requires an exact structural zero", {
  out <- sm_estimate_from_counts(
    data.frame(
      site_id = "S1", year = 2024L, n_jt = 5L,
      c_jt_A = 0L, c_jt_B = 2L, c_jt_C = 3L
    ),
    family = "multinomial",
    indicators = c("A", "B", "C"),
    vst = "none",
    boundary_method = "none",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L,
    fpc = 10
  )

  expect_multinomial_tamper_error(out, function(V) {
    epsilon <- 1e-11
    perturbation <- epsilon * matrix(
      c(
        1, -1, 0,
        -1, 1, 0,
        0, 0, 0
      ),
      3,
      3,
      byrow = TRUE,
      dimnames = dimnames(V$matrix)
    )
    V$matrix <- V$matrix + perturbation
    V
  })
})
