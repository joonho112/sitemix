sur_student_fixture <- function() {
  data.frame(
    site_id = rep("S1", 4),
    year = rep(2024L, 4),
    a = c(1L, 1L, 0L, 0L),
    b = c(1L, 1L, 0L, 0L),
    c = c(0L, 0L, 1L, 1L)
  )
}

sur_k2_oracle_matrix <- function() {
  cbind(
    a = c(1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L),
    b = c(1L, 1L, 1L, 0L, 0L, 1L, 0L, 0L)
  )
}

sur_k4_student_matrix <- function() {
  cbind(
    a = c(1L, 1L, 0L, 0L),
    b = c(1L, 0L, 1L, 0L),
    c = c(1L, 0L, 0L, 1L),
    d = c(0L, 1L, 1L, 0L)
  )
}

test_that("K=2 SUR count covariance matches a hand calculation", {
  indicators <- c("a", "b")
  out <- sitemix:::.sm_multivariate_sur_from_counts(
    n = 8L,
    marginal_counts = c(5L, 4L),
    pair_counts = 3L,
    indicators = indicators,
    boundary_method = "none"
  )
  expected <- matrix(
    c(15 / 512, 1 / 128, 1 / 128, 1 / 32),
    nrow = 2L,
    dimnames = list(indicators, indicators)
  )

  expect_equal(out$theta_raw, c(a = 5 / 8, b = 1 / 2))
  expect_equal(out$rho["a", "b"], 3 / 8)
  expect_equal(out$V_raw, expected, tolerance = 1e-15)
  expect_equal(out$se_raw^2, diag(expected), tolerance = 1e-15)
})

test_that("K=2 student rows and sufficient counts produce identical SUR covariance", {
  indicators <- c("a", "b")
  y <- sur_k2_oracle_matrix()
  from_student <- sitemix:::.sm_multivariate_sur_from_matrix(
    y,
    indicators = indicators,
    boundary_method = "none"
  )
  from_counts <- sitemix:::.sm_multivariate_sur_from_counts(
    n = nrow(y),
    marginal_counts = colSums(y),
    pair_counts = sum(y[, "a"] * y[, "b"]),
    indicators = indicators,
    boundary_method = "none"
  )

  expect_equal(from_student$theta_raw, from_counts$theta_raw)
  expect_equal(from_student$rho, from_counts$rho)
  expect_equal(from_student$V_raw, from_counts$V_raw, tolerance = 1e-15)
  expect_equal(from_student$se_raw, from_counts$se_raw, tolerance = 1e-15)
})

test_that("SUR covariance from counts matches residual cross-products", {
  indicators <- c("a", "b", "c")
  counts <- sitemix:::.sm_prepare_counts(
    sur_student_fixture(),
    family = "multivariate",
    indicators = indicators
  )

  sur <- sitemix:::.sm_multivariate_sur_from_count_row(
    counts[1, ],
    indicators = indicators,
    count_cols = attr(counts, "count_cols"),
    pair_cols = attr(counts, "pair_cols"),
    boundary_method = "none"
  )

  y <- as.matrix(sur_student_fixture()[indicators])
  theta <- colMeans(y)
  expected <- crossprod(sweep(y, 2, theta)) / nrow(y)^2

  expect_equal(sur$theta_raw, c(a = 0.5, b = 0.5, c = 0.5))
  expect_equal(sur$rho["a", "b"], 0.5)
  expect_equal(sur$rho["a", "c"], 0)
  expect_equal(sur$rho["b", "c"], 0)
  expect_equal(sur$V_raw, expected, tolerance = 1e-12)
  expect_equal(
    sur$V_raw,
    matrix(
      c(
        0.0625, 0.0625, -0.0625,
        0.0625, 0.0625, -0.0625,
        -0.0625, -0.0625, 0.0625
      ),
      3,
      3,
      byrow = TRUE,
      dimnames = list(indicators, indicators)
    )
  )
  expect_equal(sur$matrix_boundary_rule, "none")
  expect_equal(unname(sur$scalar_correction_rule), rep("none", 3))
  expect_equal(sur$psd_repair, "none")
  expect_equal(sur$matrix_rank, 1L)
  expect_true(is.na(sur$positive_support))
})

test_that("SUR matrix helper matches count helper and keeps supplied order", {
  indicators <- c("snap", "frpm")
  y <- rbind(
    c(1L, 1L),
    c(1L, 1L),
    c(0L, 1L),
    c(0L, 0L)
  )
  colnames(y) <- indicators

  from_matrix <- sitemix:::.sm_multivariate_sur_from_matrix(
    y,
    indicators = indicators,
    boundary_method = "none"
  )
  from_counts <- sitemix:::.sm_multivariate_sur_from_counts(
    n = 4L,
    marginal_counts = c(2L, 3L),
    pair_counts = 2L,
    indicators = indicators,
    boundary_method = "none"
  )
  expected <- matrix(
    c(1 / 16, 1 / 32, 1 / 32, 3 / 64),
    2,
    2,
    byrow = TRUE,
    dimnames = list(indicators, indicators)
  )

  expect_equal(from_matrix$V_raw, expected, tolerance = 1e-12)
  expect_equal(from_counts$V_raw, expected, tolerance = 1e-12)
  expect_equal(from_matrix$V_raw, from_counts$V_raw, tolerance = 1e-12)
  expect_equal(from_matrix$rho["snap", "frpm"], 0.5)
  expect_equal(sitemix:::.sm_pairwise_count_cols(indicators), "c_jt_snap_frpm")
  expect_equal(from_matrix$matrix_rank, 2L)
})

test_that("SUR helper constructs multivariate sm_vcov metadata", {
  indicators <- c("a", "b", "c")
  sur <- sitemix:::.sm_multivariate_sur_from_counts(
    n = 4L,
    marginal_counts = c(2L, 2L, 2L),
    pair_counts = c(2L, 0L, 0L),
    indicators = indicators,
    boundary_method = "none"
  )
  v <- sitemix:::.sm_multivariate_vcov_from_sur(
    sur,
    site_id = "S1",
    year = 2024L,
    indicators = indicators,
    estimate_scale = "arcsine"
  )

  expect_s3_class(v, "sm_vcov")
  expect_equal(as.matrix(v), sur$V_raw)
  expect_equal(v$family, "multivariate")
  expect_equal(v$vcov_method, "sur")
  expect_equal(v$estimate_scale, "arcsine")
  expect_equal(v$vcov_scale, "raw")
  expect_equal(v$matrix_boundary_rule, "none")
  expect_equal(v$psd_repair, "none")
  expect_equal(v$matrix_rank, 1L)
  expect_true(is.na(v$positive_support))
  expect_equal(v$n_jt, 4L)
  expect_equal(v$n_eff, 4)
})

test_that("Wilson boundary lift changes only boundary diagonals", {
  indicators <- c("a", "b")
  no_floor <- sitemix:::.sm_multivariate_sur_from_counts(
    n = 4L,
    marginal_counts = c(0L, 2L),
    pair_counts = 0L,
    indicators = indicators,
    boundary_method = "none"
  )
  floored <- sitemix:::.sm_multivariate_sur_from_counts(
    n = 4L,
    marginal_counts = c(0L, 2L),
    pair_counts = 0L,
    indicators = indicators,
    boundary_method = "wilson_floor"
  )

  expect_equal(no_floor$V_raw["a", "a"], 0)
  expect_equal(floored$V_raw["a", "a"], sitemix:::.sm_wilson_se(0, 4)^2, tolerance = 1e-12)
  expect_equal(floored$V_raw["a", "b"], 0)
  expect_equal(floored$V_raw["b", "b"], no_floor$V_raw["b", "b"])
  expect_equal(floored$matrix_boundary_rule, "diagonal_boundary_floor")
  expect_equal(
    unname(floored$scalar_correction_rule),
    c("wilson_boundary_surrogate", "none")
  )
  expect_equal(floored$se_raw, sqrt(diag(floored$V_raw)))
})

test_that("SUR matrix helper applies Wilson boundary lift and rejects Agresti-Coull boundary", {
  indicators <- c("a", "b")
  y <- cbind(
    a = c(0L, 0L, 0L, 0L),
    b = c(1L, 1L, 0L, 0L)
  )

  floored <- sitemix:::.sm_multivariate_sur_from_matrix(
    y,
    indicators = indicators,
    boundary_method = "wilson_floor"
  )

  expect_equal(floored$V_raw["a", "a"], sitemix:::.sm_wilson_se(0, 4)^2, tolerance = 1e-12)
  expect_equal(floored$V_raw["a", "b"], 0)
  expect_equal(floored$matrix_boundary_rule, "diagonal_boundary_floor")
  expect_equal(
    unname(floored$scalar_correction_rule),
    c("wilson_boundary_surrogate", "none")
  )
  expect_equal(floored$psd_repair, "none")

  expect_error(
    sitemix:::.sm_multivariate_sur_from_matrix(
      y,
      indicators = indicators,
      boundary_method = "agresti_coull"
    ),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})

test_that("Agresti-Coull boundary matrix output is rejected", {
  expect_error(
    sitemix:::.sm_multivariate_sur_from_counts(
      n = 4L,
      marginal_counts = c(0L, 2L),
      pair_counts = 0L,
      indicators = c("a", "b"),
      boundary_method = "agresti_coull"
    ),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})

test_that("SUR helper applies FPC to the full raw matrix", {
  base <- sitemix:::.sm_multivariate_sur_from_counts(
    n = 4L,
    marginal_counts = c(2L, 2L, 2L),
    pair_counts = c(2L, 0L, 0L),
    indicators = c("a", "b", "c"),
    boundary_method = "none"
  )
  fpc <- sitemix:::.sm_multivariate_sur_from_counts(
    n = 4L,
    marginal_counts = c(2L, 2L, 2L),
    pair_counts = c(2L, 0L, 0L),
    indicators = c("a", "b", "c"),
    boundary_method = "none",
    fpc = 10
  )

  expect_equal(fpc$V_raw, base$V_raw * ((10 - 4) / (10 - 1)), tolerance = 1e-12)
  expect_equal(fpc$se_raw, sqrt(diag(fpc$V_raw)), tolerance = 1e-12)
})

test_that("SUR helper rejects pairwise-infeasible counts", {
  expect_error(
    sitemix:::.sm_multivariate_sur_from_counts(
      n = 4L,
      marginal_counts = c(1L, 1L),
      pair_counts = 2L,
      indicators = c("a", "b")
    ),
    class = "sitemix_error_input_indicator_count"
  )

})

test_that("K=3 exact triple interval rejects pairwise-valid PSD counts with no joint table", {
  n <- 8L
  marginals <- c(4L, 4L, 4L)
  pairs <- c(1L, 1L, 1L)
  p <- marginals / n
  rho <- matrix(
    c(
      p[[1L]], pairs[[1L]] / n, pairs[[2L]] / n,
      pairs[[1L]] / n, p[[2L]], pairs[[3L]] / n,
      pairs[[2L]] / n, pairs[[3L]] / n, p[[3L]]
    ),
    nrow = 3L,
    byrow = TRUE
  )
  candidate <- (rho - tcrossprod(p)) / n

  expect_true(all(pairs >= pmax(0L, marginals[c(1L, 1L, 2L)] + marginals[c(2L, 3L, 3L)] - n)))
  expect_true(all(pairs <= pmin(marginals[c(1L, 1L, 2L)], marginals[c(2L, 3L, 3L)])))
  expect_gte(min(eigen(candidate, symmetric = TRUE, only.values = TRUE)$values), -1e-15)

  err <- tryCatch(
    sitemix:::.sm_multivariate_sur_from_counts(
      n = n,
      marginal_counts = marginals,
      pair_counts = pairs,
      indicators = c("a", "b", "c"),
      boundary_method = "none"
    ),
    error = identity
  )
  expect_s3_class(err, "sitemix_error_input_indicator_count")
  expect_s3_class(err, "sitemix_error_input")
  expect_equal(err$triple_count_lower, 0L)
  expect_equal(err$triple_count_upper, -1L)
  expect_equal(err$joint_feasibility, "infeasible")
  expect_equal(err$indicators, c("a", "b", "c"))
  expect_match(conditionMessage(err), "do not admit a joint Bernoulli table", fixed = TRUE)
  expect_match(err$fix, "common site-year sample", fixed = TRUE)
})

test_that("K>=4 sufficient pair counts fail closed while student rows remain supported", {
  indicators <- c("a", "b", "c", "d")
  y <- sur_k4_student_matrix()
  student_data <- data.frame(
    site_id = rep("S1", nrow(y)),
    year = rep(2024L, nrow(y)),
    y,
    check.names = FALSE
  )

  student <- sitemix::sm_estimate(
    student_data,
    family = "multivariate",
    indicators = indicators,
    boundary_method = "none",
    vjt = TRUE,
    min_n = 1L
  )
  direct <- sitemix:::.sm_multivariate_sur_from_matrix(
    y,
    indicators = indicators,
    boundary_method = "none"
  )
  expect_equal(unique(student$input_mode), "student_level")
  expect_equal(as.matrix(student$V[[1L]]), direct$V_raw, tolerance = 1e-15)

  counts <- sitemix:::.sm_prepare_counts(
    student_data,
    family = "multivariate",
    indicators = indicators
  )
  count_data <- as.data.frame(counts)
  err <- tryCatch(
    sitemix::sm_estimate(
      count_data,
      family = "multivariate",
      indicators = indicators,
      boundary_method = "none",
      from_counts = TRUE,
      vjt = TRUE,
      min_n = 1L
    ),
    error = identity
  )
  expect_s3_class(err, "sitemix_error_input_indicator_count")
  expect_equal(err$indicator_count, 4L)
  expect_equal(err$joint_feasibility, "unchecked")
  expect_equal(err$deferred_option, "joint_feasibility = \"unchecked\"")
  expect_match(conditionMessage(err), "not verified for sufficient pair-count inputs", fixed = TRUE)
  expect_match(err$fix, "student-level indicators", fixed = TRUE)
})

test_that("K=3 SUR accepts a singleton feasible triple-count interval", {
  out <- sitemix:::.sm_multivariate_sur_from_counts(
    n = 8L,
    marginal_counts = rep(4L, 3),
    pair_counts = c(2L, 2L, 0L),
    indicators = c("a", "b", "c"),
    boundary_method = "none"
  )

  values <- eigen(out$V_raw, symmetric = TRUE, only.values = TRUE)$values
  expect_gte(min(values), -sitemix:::.sm_psd_tolerance(out$V_raw))
  expect_equal(out$matrix_rank, 2L)
  expect_equal(out$psd_repair, "none")
  expect_true(is.na(out$positive_support))
})

test_that("public student-level SUR rejects one-unit noncensus correction", {
  data <- data.frame(
    site_id = "S1",
    year = 2025L,
    student_id = "u1",
    a = 1L,
    b = 0L,
    stringsAsFactors = FALSE
  )

  error <- expect_error(
    sitemix::sm_estimate(
      data,
      family = "multivariate",
      indicators = c("a", "b"),
      bias_correction = "binomial_bc",
      vjt = TRUE,
      min_n = 1L
    ),
    class = "sitemix_error_estimate_var_method"
  )

  expect_match(
    conditionMessage(error),
    "whole-matrix binomial correction requires n > 1",
    fixed = TRUE
  )
  expect_identical(error$expected, "n > 1")
  expect_identical(error$actual, 1L)
})

test_that("public sufficient-count SUR rejects infeasible Bernoulli moments", {
  data <- data.frame(
    site_id = "S1",
    year = 2025L,
    n_jt = 8L,
    c_jt_a = 4L,
    c_jt_b = 4L,
    c_jt_c = 4L,
    c_jt_a_b = 1L,
    c_jt_a_c = 1L,
    c_jt_b_c = 1L,
    stringsAsFactors = FALSE
  )

  error <- expect_error(
    sitemix::sm_estimate_from_counts(
      data,
      family = "multivariate",
      indicators = c("a", "b", "c"),
      vjt = TRUE,
      min_n = 1L
    ),
    class = "sitemix_error_input_indicator_count"
  )

  expect_match(
    conditionMessage(error),
    "do not admit a joint Bernoulli table",
    fixed = TRUE
  )
  expect_identical(error$triple_count_lower, 0L)
  expect_identical(error$triple_count_upper, -1L)
  expect_identical(error$joint_feasibility, "infeasible")
})
