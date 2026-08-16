fpc_vcov_quiet_d1 <- function(expr) {
  withCallingHandlers(
    expr,
    sitemix_warning_working_independence_default = function(w) {
      invokeRestart("muffleWarning")
    }
  )
}

test_that("independent B and C matrix oracles match both SRSWOR rules", {
  Y <- rbind(
    c(1, 1),
    c(1, 0),
    c(0, 1),
    c(0, 1),
    c(0, 0)
  )
  n <- nrow(Y)
  N <- 12
  centered <- sweep(Y, 2L, colMeans(Y))
  Q <- crossprod(centered)
  dimnames(Q) <- list(c("a", "b"), c("a", "b"))

  b_plugin <- sitemix:::.sm_multivariate_sur_from_counts(
    n = n,
    marginal_counts = colSums(Y),
    pair_counts = sum(Y[, 1] * Y[, 2]),
    indicators = c("a", "b"),
    boundary_method = "none",
    fpc = N
  )
  b_corrected <- sitemix:::.sm_multivariate_sur_from_counts(
    n = n,
    marginal_counts = colSums(Y),
    pair_counts = sum(Y[, 1] * Y[, 2]),
    indicators = c("a", "b"),
    boundary_method = "none",
    bias_correction = "binomial_bc",
    fpc = N
  )
  expect_equal(
    b_plugin$V_raw,
    ((N - n) / (N - 1)) * Q / n^2,
    tolerance = 1e-14
  )
  expect_equal(
    b_corrected$V_raw,
    (N - n) * Q / (N * n * (n - 1)),
    tolerance = 1e-14
  )

  counts <- c(3, 2, 1)
  n_c <- sum(counts)
  N_c <- 15
  p <- counts / n_c
  M <- diag(p) - tcrossprod(p)
  dimnames(M) <- list(c("x", "y", "z"), c("x", "y", "z"))
  c_plugin <- sitemix:::.sm_multinomial_cov_from_counts(
    n = n_c,
    category_counts = counts,
    categories = c("x", "y", "z"),
    boundary_method = "none",
    fpc = N_c
  )
  c_corrected <- sitemix:::.sm_multinomial_cov_from_counts(
    n = n_c,
    category_counts = counts,
    categories = c("x", "y", "z"),
    boundary_method = "none",
    bias_correction = "binomial_bc",
    fpc = N_c
  )
  expect_equal(
    c_plugin$V_raw,
    ((N_c - n_c) / (N_c - 1)) * M / n_c,
    tolerance = 1e-14
  )
  expect_equal(
    c_corrected$V_raw,
    (N_c - n_c) * M / (N_c * (n_c - 1)),
    tolerance = 1e-14
  )
})

test_that("Scenario B normalizes row-aligned populations and records matrix metadata", {
  counts <- data.frame(
    site_id = c("B", "A"),
    year = c(2024L, 2024L),
    n_jt = c(5L, 4L),
    c_jt_a = c(2L, 2L),
    c_jt_b = c(3L, 3L),
    c_jt_a_b = c(1L, 2L)
  )
  out <- sm_estimate_from_counts(
    counts,
    family = "multivariate",
    indicators = c("a", "b"),
    vst = "none",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L,
    fpc = c(12, 10)
  )

  expect_equal(out$population_size[out$site_id == "A"], c(10, 10))
  expect_equal(out$population_size[out$site_id == "B"], c(12, 12))
  for (site in c("A", "B")) {
    group <- out[out$site_id == site, ]
    V <- group$V[[1]]
    expect_identical(V$sampling_design, "SRSWOR")
    expect_identical(V$diag_contract, "row_se_raw_squared")
    expect_equal(unname(diag(as.matrix(V))), group$se_raw^2, tolerance = 1e-14)
    expect_equal(V$sampling_fraction, rep(group$n[[1]] / group$population_size[[1]], 2))
    expect_equal(V$fpc_variance_multiplier, rep(group$fpc_variance_multiplier[[1]], 2))
    expect_equal(V$variance_multiplier_applied, rep(group$variance_multiplier_applied[[1]], 2))
    expect_identical(V$variance_rule, rep("design_corrected", 2))
    expect_gte(min(eigen(as.matrix(V), symmetric = TRUE, only.values = TRUE)$values), -1e-12)
  }
})

test_that("Scenario C preserves simplex, PSD, and scalar companion metadata", {
  counts <- data.frame(
    site_id = c("B", "A"),
    year = c(2024L, 2024L),
    n_jt = c(6L, 5L),
    c_jt_x = c(3L, 2L),
    c_jt_y = c(2L, 2L),
    c_jt_z = c(1L, 1L)
  )
  out <- sm_estimate_from_counts(
    counts,
    family = "multinomial",
    indicators = c("x", "y", "z"),
    vst = "none",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L,
    fpc = c(14, 11)
  )

  expect_equal(out$population_size[out$site_id == "A"], rep(11, 3))
  expect_equal(out$population_size[out$site_id == "B"], rep(14, 3))
  for (site in c("A", "B")) {
    group <- out[out$site_id == site, ]
    V <- group$V[[1]]
    mat <- as.matrix(V)
    expect_equal(unname(diag(mat)), group$se_raw^2, tolerance = 1e-14)
    expect_equal(as.vector(mat %*% rep(1, 3)), rep(0, 3), tolerance = 1e-14)
    expect_gte(min(eigen(mat, symmetric = TRUE, only.values = TRUE)$values), -1e-12)
    expect_identical(V$variance_rule, rep("design_corrected", 3))
    expect_identical(V$diag_contract, "row_se_raw_squared")
  }
})

test_that("B and C censuses have zero whole-matrix uncertainty", {
  b <- sm_estimate_from_counts(
    data.frame(
      site_id = "A", year = 2024L, n_jt = 4L,
      c_jt_a = 2L, c_jt_b = 3L, c_jt_a_b = 2L
    ),
    family = "multivariate",
    indicators = c("a", "b"),
    vst = "none",
    vjt = TRUE,
    min_n = 1L,
    fpc = 4
  )
  c <- sm_estimate_from_counts(
    data.frame(
      site_id = "A", year = 2024L, n_jt = 5L,
      c_jt_x = 2L, c_jt_y = 2L, c_jt_z = 1L
    ),
    family = "multinomial",
    indicators = c("x", "y", "z"),
    vst = "none",
    vjt = TRUE,
    min_n = 1L,
    fpc = 5
  )

  expect_equal(as.matrix(b$V[[1]]), matrix(0, 2, 2, dimnames = list(c("a", "b"), c("a", "b"))))
  expect_equal(as.matrix(c$V[[1]]), matrix(0, 3, 3, dimnames = list(c("x", "y", "z"), c("x", "y", "z"))))
  expect_equal(b$se_raw, c(0, 0))
  expect_equal(c$se_raw, c(0, 0, 0))
  expect_equal(b$V[[1]]$matrix_rank, 0L)
  expect_equal(c$V[[1]]$matrix_rank, 2L)
  expect_equal(sitemix:::.sm_matrix_rank(as.matrix(c$V[[1]])), 0L)
  expect_equal(as.vector(c$V[[1]]$matrix %*% rep(1, 3)), rep(0, 3))
})

test_that("one-unit B and C censuses precede the n-minus-one matrix gate", {
  b <- sm_estimate_from_counts(
    data.frame(
      site_id = "A", year = 2024L, n_jt = 1L,
      c_jt_a = 1L, c_jt_b = 0L, c_jt_a_b = 0L
    ),
    family = "multivariate",
    indicators = c("a", "b"),
    vst = "none",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L,
    fpc = 1
  )
  c <- sm_estimate_from_counts(
    data.frame(
      site_id = "A", year = 2024L, n_jt = 1L,
      c_jt_x = 1L, c_jt_y = 0L
    ),
    family = "multinomial",
    indicators = c("x", "y"),
    vst = "none",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L,
    fpc = 1
  )

  for (out in list(b, c)) {
    expect_equal(as.matrix(out$V[[1]]),
                 matrix(0, 2, 2, dimnames = dimnames(as.matrix(out$V[[1]]))))
    expect_equal(out$se_raw, c(0, 0))
    expect_equal(out$se, c(0, 0))
    expect_equal(out$V[[1]]$population_size, 1)
    expect_equal(out$V[[1]]$sampling_fraction, c(1, 1))
    expect_equal(out$V[[1]]$fpc_variance_multiplier, c(0, 0))
    expect_equal(out$V[[1]]$variance_multiplier_applied, c(0, 0))
    expect_identical(out$V[[1]]$sampling_design, "SRSWOR")
  }

  expect_error(
    sitemix:::.sm_multivariate_sur_from_counts(
      n = 1L,
      marginal_counts = c(1L, 0L),
      pair_counts = 0L,
      indicators = c("a", "b"),
      bias_correction = "binomial_bc",
      fpc = 2
    ),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_multinomial_cov_from_counts(
      n = 1L,
      category_counts = c(1L, 0L),
      categories = c("x", "y"),
      bias_correction = "binomial_bc",
      fpc = 2
    ),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("D1 uses one keyed population with coordinate-specific denominators", {
  aggregate <- data.frame(
    site_id = c("B", "B", "A", "A"),
    year = rep(2024L, 4),
    indicator = c("a", "b", "a", "b"),
    c_jt = c(2L, 1L, 2L, 3L),
    n_jt = c(4L, 3L, 5L, 5L)
  )
  out <- fpc_vcov_quiet_d1(
    sm_estimate_from_aggregates(
      aggregate,
      family = "multivariate",
      sampling_relation = "different_units",
      vst = "none",
      bias_correction = "binomial_bc",
      vjt = TRUE,
      min_n = 1L,
      fpc = c(10, 10, 20, 20)
    )
  )

  group_b <- out[out$site_id == "B", ]
  V <- group_b$V[[1]]
  expect_equal(group_b$population_size, c(10, 10))
  expect_equal(V$population_size, 10)
  expect_equal(V$sampling_fraction, c(0.4, 0.3))
  expect_equal(V$fpc_variance_multiplier, c(6 / 9, 7 / 9), tolerance = 1e-14)
  expect_equal(V$variance_multiplier_applied, c(6 / 10, 7 / 10), tolerance = 1e-14)
  expect_equal(unname(diag(as.matrix(V))), group_b$se^2, tolerance = 1e-14)
  expect_true(is.na(V$n_jt))
  expect_true(is.na(V$n_eff))
  expect_identical(V$diag_contract, "row_se_squared")
})

test_that("D1 rejects implicit group-order and within-group population vectors", {
  aggregate <- data.frame(
    site_id = c("A", "A", "B", "B"),
    year = rep(2024L, 4),
    indicator = c("a", "b", "a", "b"),
    c_jt = c(2L, 3L, 2L, 1L),
    n_jt = c(5L, 5L, 4L, 3L)
  )
  expect_error(
    fpc_vcov_quiet_d1(
      sm_estimate_from_aggregates(
        aggregate,
        family = "multivariate",
        min_n = 1L,
        fpc = c(20, 10)
      )
    ),
    class = "sitemix_error_invalid_fpc"
  )
  expect_error(
    fpc_vcov_quiet_d1(
      sm_estimate_from_aggregates(
        aggregate,
        family = "multivariate",
        min_n = 1L,
        fpc = c(20, 21, 10, 10)
      )
    ),
    class = "sitemix_error_invalid_fpc"
  )
})

test_that("A and D0 1x1 matrices expose the same structured provenance", {
  a <- sm_estimate_from_counts(
    data.frame(site_id = "A", year = 2024L, n_jt = 8L, c_jt_x = 3L),
    family = "binomial",
    indicator = "x",
    vst = "logit",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L,
    fpc = 20
  )
  d0 <- sm_estimate_from_aggregates(
    data.frame(
      site_id = "A", year = 2024L, indicator = "x",
      c_jt = 3L, n_jt = 8L
    ),
    family = "binomial",
    indicator = "x",
    vst = "none",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L,
    fpc = 20
  )

  for (out in list(a, d0)) {
    V <- out$V[[1]]
    expect_identical(V$diag_contract, "row_se_squared")
    expect_identical(V$sampling_design, "SRSWOR")
    expect_equal(V$population_size, 20)
    expect_equal(V$sampling_fraction, 0.4)
    expect_equal(V$fpc_variance_multiplier, 12 / 19, tolerance = 1e-14)
    expect_equal(V$variance_multiplier_applied, 12 / 20, tolerance = 1e-14)
    expect_identical(V$variance_rule, "design_corrected")
    expect_equal(as.matrix(V)[1, 1], out$se^2, tolerance = 1e-14)
  }
})

test_that("sm_vcov rejects internally inconsistent SRSWOR metadata", {
  x <- sm_vcov(
    matrix = matrix(0.01, 1, 1, dimnames = list("x", "x")),
    indicator_order = "x",
    family = "binomial",
    estimate_scale = "none",
    vcov_scale = "raw",
    population_size = 20,
    sampling_fraction = 0.4,
    fpc_variance_multiplier = 12 / 19,
    fpc_se_multiplier = sqrt(12 / 19),
    variance_multiplier_applied = 12 / 19,
    se_multiplier_applied = sqrt(12 / 19),
    sampling_design = "SRSWOR",
    variance_rule = "plugin"
  )
  expect_true(validate.sm_vcov(x))
  x$variance_multiplier_applied <- 0.5
  expect_error(validate.sm_vcov(x), class = "sitemix_error_vcov_invariant")
})

test_that("sitemix_estimates rejects internally valid V metadata that disagrees with rows", {
  counts <- data.frame(
    site_id = "A", year = 2024L, n_jt = 4L,
    c_jt_a = 2L, c_jt_b = 3L, c_jt_a_b = 2L
  )
  out <- sm_estimate_from_counts(
    counts,
    family = "multivariate",
    indicators = c("a", "b"),
    vst = "none",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L,
    fpc = 10
  )
  forged <- out$V[[1]]
  forged_design <- sitemix:::.sm_vcov_fpc_metadata(
    n = 4,
    fpc = 20,
    variance_rule = "design_corrected",
    K = 2L
  )
  for (field in names(forged_design)) {
    forged[[field]] <- forged_design[[field]]
  }
  expect_true(validate.sm_vcov(forged))
  bad_population <- out
  bad_population$V <- list(forged, forged)
  expect_error(
    validate.sitemix_estimates(bad_population),
    class = "sitemix_error_estimate_vcov_invariant"
  )

  no_fpc <- sm_estimate_from_counts(
    counts,
    family = "multivariate",
    indicators = c("a", "b"),
    vst = "none",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L
  )
  wrong_rule <- no_fpc$V[[1]]
  wrong_rule$variance_rule <- rep("plugin", 2)
  expect_true(validate.sm_vcov(wrong_rule))
  bad_rule <- no_fpc
  bad_rule$V <- list(wrong_rule, wrong_rule)
  expect_error(
    validate.sitemix_estimates(bad_rule),
    class = "sitemix_error_estimate_vcov_invariant"
  )

  forged_no_row <- no_fpc$V[[1]]
  for (field in names(forged_design)) {
    forged_no_row[[field]] <- forged_design[[field]]
  }
  expect_true(validate.sm_vcov(forged_no_row))
  bad_no_row <- no_fpc
  bad_no_row$V <- list(forged_no_row, forged_no_row)
  expect_error(
    validate.sitemix_estimates(bad_no_row),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})

test_that("D1 mixed census coordinates remain singular with exact design metadata", {
  aggregate <- data.frame(
    site_id = c("S1", "S1"),
    year = c(2025L, 2025L),
    indicator = c("a", "b"),
    c_jt = c(2L, 1L),
    n_jt = c(4L, 3L),
    stringsAsFactors = FALSE
  )
  out <- fpc_vcov_quiet_d1(
    sm_estimate_from_aggregates(
      aggregate,
      family = "multivariate",
      aggregate_case = "D1",
      sampling_relation = "different_units",
      vst = "none",
      bias_correction = "binomial_bc",
      vjt = TRUE,
      min_n = 1L,
      fpc = c(4, 4)
    )
  )
  V <- out$V[[1L]]
  mat <- as.matrix(V)
  relative_tolerance <- 128 * .Machine$double.eps

  expect_equal(
    unname(diag(mat)),
    out$se^2,
    tolerance = relative_tolerance
  )
  expect_identical(mat["a", "a"], 0)
  expect_gt(mat["b", "b"], 0)
  expect_equal(mat["b", "b"], 1 / 36, tolerance = relative_tolerance)
  expect_identical(V$matrix_rank, 1L)
  expect_equal(V$sampling_fraction, c(1, 0.75), tolerance = relative_tolerance)
  expect_equal(V$fpc_variance_multiplier, c(0, 1 / 3), tolerance = relative_tolerance)
  expect_equal(V$variance_multiplier_applied, c(0, 0.25), tolerance = relative_tolerance)
  expect_identical(V$variance_rule, rep("design_corrected", 2L))
  expect_true(validate.sitemix_estimates(out))
})
