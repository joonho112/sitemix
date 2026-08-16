step38_k2_student_matrix <- function() {
  rbind(
    c(1L, 1L),
    c(1L, 1L),
    c(1L, 1L),
    c(1L, 0L),
    c(1L, 0L),
    c(0L, 1L),
    c(0L, 0L),
    c(0L, 0L)
  ) |>
    `colnames<-`(c("a", "b"))
}

step38_k3_student_matrix <- function() {
  rbind(
    c(1L, 1L, 1L),
    c(1L, 1L, 0L),
    c(1L, 0L, 1L),
    c(1L, 0L, 0L),
    c(0L, 1L, 1L),
    c(0L, 1L, 0L),
    c(0L, 0L, 0L),
    c(0L, 0L, 0L)
  ) |>
    `colnames<-`(c("a", "b", "c"))
}

step38_pair_counts <- function(Y) {
  pairs <- utils::combn(seq_len(ncol(Y)), 2L, simplify = FALSE)
  vapply(pairs, function(pair) sum(Y[, pair[[1L]]] * Y[, pair[[2L]]]), integer(1))
}

step38_q_oracle <- function(Y) {
  centered <- sweep(Y, 2L, colMeans(Y), FUN = "-")
  Q <- crossprod(centered)
  dimnames(Q) <- list(colnames(Y), colnames(Y))
  Q
}

step38_expected_v <- function(Q, n, population_size = NULL, corrected = FALSE) {
  if (isTRUE(corrected)) {
    if (is.null(population_size)) {
      return(Q / (n * (n - 1)))
    }
    return((population_size - n) * Q / (population_size * n * (n - 1)))
  }

  q <- if (is.null(population_size)) 1 else (population_size - n) / (population_size - 1)
  q * Q / n^2
}

step38_repeat_tampered_v <- function(out, mutate) {
  v <- mutate(out$V[[1L]])
  out$V <- rep(list(v), nrow(out))
  list(output = out, covariance = v)
}

test_that("Step 3.8 K=2 hand oracle locks plug-in and corrected whole matrices", {
  indicators <- c("a", "b")
  Y <- step38_k2_student_matrix()
  plugin <- sitemix:::.sm_multivariate_sur_from_counts(
    n = 8L,
    marginal_counts = c(5L, 4L),
    pair_counts = 3L,
    indicators = indicators,
    boundary_method = "none"
  )
  corrected <- sitemix:::.sm_multivariate_sur_from_counts(
    n = 8L,
    marginal_counts = c(5L, 4L),
    pair_counts = 3L,
    indicators = indicators,
    boundary_method = "none",
    bias_correction = "binomial_bc"
  )
  plugin_from_students <- sitemix:::.sm_multivariate_sur_from_matrix(
    Y,
    indicators = indicators,
    boundary_method = "none"
  )
  corrected_from_students <- sitemix:::.sm_multivariate_sur_from_matrix(
    Y,
    indicators = indicators,
    boundary_method = "none",
    bias_correction = "binomial_bc"
  )

  expected_plugin <- matrix(
    c(15 / 512, 1 / 128, 1 / 128, 1 / 32),
    nrow = 2L,
    dimnames = list(indicators, indicators)
  )
  expected_corrected <- matrix(
    c(15 / 448, 1 / 112, 1 / 112, 1 / 28),
    nrow = 2L,
    dimnames = list(indicators, indicators)
  )

  expect_equal(plugin$V_raw, expected_plugin, tolerance = 1e-15)
  expect_equal(corrected$V_raw, expected_corrected, tolerance = 1e-15)
  expect_equal(plugin_from_students$V_raw, expected_plugin, tolerance = 1e-15)
  expect_equal(corrected_from_students$V_raw, expected_corrected, tolerance = 1e-15)
  expect_equal(plugin$V_raw, step38_q_oracle(Y) / nrow(Y)^2, tolerance = 1e-15)
  expect_equal(
    corrected$V_raw,
    step38_q_oracle(Y) / (nrow(Y) * (nrow(Y) - 1)),
    tolerance = 1e-15
  )
  expect_equal(corrected$V_raw[1, 2], plugin$V_raw[1, 2] * 8 / 7, tolerance = 1e-15)
})

test_that("Step 3.8 legal K=3 student and count paths match four matrix rules", {
  Y <- step38_k3_student_matrix()
  indicators <- colnames(Y)
  n <- nrow(Y)
  N <- 20
  Q <- step38_q_oracle(Y)
  cases <- list(
    plugin = list(fpc = NULL, bias = NULL, corrected = FALSE),
    corrected = list(fpc = NULL, bias = "binomial_bc", corrected = TRUE),
    plugin_fpc = list(fpc = N, bias = NULL, corrected = FALSE),
    corrected_fpc = list(fpc = N, bias = "binomial_bc", corrected = TRUE)
  )

  for (case in cases) {
    from_students <- sitemix:::.sm_multivariate_sur_from_matrix(
      Y,
      indicators = indicators,
      boundary_method = "none",
      bias_correction = case$bias,
      fpc = case$fpc
    )
    from_counts <- sitemix:::.sm_multivariate_sur_from_counts(
      n = n,
      marginal_counts = colSums(Y),
      pair_counts = step38_pair_counts(Y),
      indicators = indicators,
      boundary_method = "none",
      bias_correction = case$bias,
      fpc = case$fpc
    )
    expected <- step38_expected_v(
      Q,
      n = n,
      population_size = case$fpc,
      corrected = case$corrected
    )

    expect_equal(from_students$V_raw, expected, tolerance = 1e-14)
    expect_equal(from_counts$V_raw, expected, tolerance = 1e-14)
    expect_equal(from_students$V_raw, from_counts$V_raw, tolerance = 1e-15)
    expect_equal(from_students$se_raw^2, diag(expected), tolerance = 1e-14)
    expect_equal(from_counts$se_raw^2, diag(expected), tolerance = 1e-14)
    expect_equal(from_students$V_raw, t(from_students$V_raw), tolerance = 1e-15)
    expect_gte(
      min(eigen(from_students$V_raw, symmetric = TRUE, only.values = TRUE)$values),
      -sitemix:::.sm_psd_tolerance(from_students$V_raw)
    )
    expect_identical(from_students$diag_contract, "row_se_raw_squared")
  }
})

test_that("Step 3.8 correction scales off-diagonals and preserves correlations", {
  Y <- step38_k3_student_matrix()
  n <- nrow(Y)
  N <- 20
  plugin <- sitemix:::.sm_multivariate_sur_from_matrix(
    Y,
    boundary_method = "none",
    fpc = N
  )
  corrected <- sitemix:::.sm_multivariate_sur_from_matrix(
    Y,
    boundary_method = "none",
    bias_correction = "binomial_bc",
    fpc = N
  )
  expected_ratio <- ((N - n) / N) * n / ((n - 1) * ((N - n) / (N - 1)))
  nonzero <- abs(plugin$V_raw) > 0

  expect_true(abs(plugin$V_raw[1, 3]) > 0)
  expect_equal(
    corrected$V_raw[nonzero] / plugin$V_raw[nonzero],
    rep(expected_ratio, sum(nonzero)),
    tolerance = 1e-14
  )
  expect_false(isTRUE(all.equal(corrected$V_raw[1, 3], plugin$V_raw[1, 3])))
  expect_equal(
    stats::cov2cor(corrected$V_raw),
    stats::cov2cor(plugin$V_raw),
    tolerance = 1e-14
  )
})

test_that("Step 3.8 FPC variants and census obey explicit matrix provenance", {
  Y <- step38_k2_student_matrix()
  n <- nrow(Y)
  N <- 20
  Q <- step38_q_oracle(Y)
  plugin <- sitemix:::.sm_multivariate_sur_from_matrix(
    Y,
    boundary_method = "none",
    fpc = N
  )
  corrected <- sitemix:::.sm_multivariate_sur_from_matrix(
    Y,
    boundary_method = "none",
    bias_correction = "binomial_bc",
    fpc = N
  )
  census_plugin <- sitemix:::.sm_multivariate_sur_from_matrix(
    Y,
    boundary_method = "none",
    fpc = n
  )
  census_corrected <- sitemix:::.sm_multivariate_sur_from_matrix(
    Y,
    boundary_method = "none",
    bias_correction = "binomial_bc",
    fpc = n
  )

  expect_equal(plugin$V_raw, (12 / 19) * Q / n^2, tolerance = 1e-14)
  expect_equal(corrected$V_raw, 12 * Q / (20 * n * (n - 1)), tolerance = 1e-14)
  expect_equal(plugin$fpc_variance_multiplier, rep(12 / 19, 2), tolerance = 1e-14)
  expect_equal(plugin$variance_multiplier_applied, rep(12 / 19, 2), tolerance = 1e-14)
  expect_identical(plugin$variance_rule, rep("plugin", 2))
  expect_equal(corrected$fpc_variance_multiplier, rep(12 / 19, 2), tolerance = 1e-14)
  expect_equal(corrected$variance_multiplier_applied, rep(12 / 20, 2), tolerance = 1e-14)
  expect_identical(corrected$variance_rule, rep("design_corrected", 2))
  expect_identical(corrected$sampling_design, "SRSWOR")
  expect_equal(census_plugin$V_raw, matrix(0, 2, 2, dimnames = dimnames(Q)))
  expect_equal(census_corrected$V_raw, matrix(0, 2, 2, dimnames = dimnames(Q)))
  expect_equal(census_plugin$se_raw, c(a = 0, b = 0))
  expect_equal(census_corrected$se_raw, c(a = 0, b = 0))
  expect_identical(census_plugin$matrix_rank, 0L)
  expect_identical(census_corrected$matrix_rank, 0L)
})

test_that("Step 3.8 constant indicators have zero cross-covariance and named Wilson provenance", {
  indicators <- c("constant", "varying")
  Y <- cbind(
    constant = c(0L, 0L, 0L, 0L),
    varying = c(1L, 1L, 0L, 0L)
  )
  N <- 10
  q <- (N - nrow(Y)) / (N - 1)
  plugin <- sitemix:::.sm_multivariate_sur_from_matrix(
    Y,
    indicators = indicators,
    boundary_method = "wilson_floor",
    fpc = N
  )
  corrected <- sitemix:::.sm_multivariate_sur_from_matrix(
    Y,
    indicators = indicators,
    boundary_method = "wilson_floor",
    bias_correction = "binomial_bc",
    fpc = N
  )

  for (out in list(plugin, corrected)) {
    expect_equal(out$V_raw["constant", "varying"], 0)
    expect_equal(out$V_raw["varying", "constant"], 0)
    expect_equal(
      out$V_raw["constant", "constant"],
      sitemix:::.sm_wilson_se(0, nrow(Y))^2 * q,
      tolerance = 1e-14
    )
    expect_gt(out$V_raw["constant", "constant"], 0)
    expect_identical(out$matrix_boundary_rule, "diagonal_boundary_floor")
    expect_identical(
      unname(out$scalar_correction_rule[["constant"]]),
      "wilson_boundary_surrogate"
    )
    expect_identical(unname(out$variance_rule[[1L]]), "plugin")
    expect_equal(out$se_raw^2, diag(out$V_raw), tolerance = 1e-14)
  }
  expect_identical(unname(corrected$variance_rule[[2L]]), "design_corrected")
  expect_identical(
    unname(corrected$scalar_correction_rule[["varying"]]),
    "binomial_bc"
  )

  none_corrected <- sitemix:::.sm_multivariate_sur_from_matrix(
    Y,
    indicators = indicators,
    boundary_method = "none",
    bias_correction = "binomial_bc",
    fpc = N
  )
  expect_equal(none_corrected$V_raw["constant", ], c(constant = 0, varying = 0))
  expect_identical(
    unname(none_corrected$scalar_correction_rule),
    c("none", "binomial_bc")
  )
  expect_identical(
    unname(none_corrected$variance_rule),
    c("plugin", "design_corrected")
  )

  public_none <- sm_estimate_from_counts(
    data.frame(
      site_id = "S1",
      year = 2024L,
      n_jt = 4L,
      c_jt_constant = 0L,
      c_jt_varying = 2L,
      c_jt_constant_varying = 0L
    ),
    family = "multivariate",
    indicators = indicators,
    vst = "none",
    boundary_method = "none",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L,
    fpc = N
  )
  expect_identical(public_none$var_method, c("binomial", "binomial_bc"))
  expect_identical(
    unname(public_none$V[[1L]]$variance_rule),
    c("plugin", "design_corrected")
  )
  expect_true(validate.sitemix_estimates(public_none))

  public_wilson <- sm_estimate_from_counts(
    data.frame(
      site_id = "S1",
      year = 2024L,
      n_jt = 4L,
      c_jt_constant = 0L,
      c_jt_varying = 2L,
      c_jt_constant_varying = 0L
    ),
    family = "multivariate",
    indicators = indicators,
    vst = "none",
    boundary_method = "wilson_floor",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L,
    fpc = N
  )
  expect_identical(
    public_wilson$var_method,
    c("wilson_boundary_surrogate", "binomial_bc")
  )
  expect_identical(
    unname(public_wilson$V[[1L]]$variance_rule),
    c("plugin", "design_corrected")
  )
  expect_equal(
    unname(diag(as.matrix(public_wilson$V[[1L]]))),
    public_wilson$se_raw^2,
    tolerance = 1e-14
  )
  expect_true(validate.sitemix_estimates(public_wilson))

  expect_error(
    sitemix:::.sm_multivariate_sur_from_matrix(
      Y,
      indicators = indicators,
      boundary_method = "agresti_coull"
    ),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})

test_that("Step 3.8 transformed rows retain an intentional raw-matrix diagonal contract", {
  counts <- data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 8L,
    c_jt_a = 5L,
    c_jt_b = 4L,
    c_jt_a_b = 3L
  )
  out <- sm_estimate_from_counts(
    counts,
    family = "multivariate",
    indicators = c("a", "b"),
    vst = "arcsine",
    boundary_method = "none",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L,
    fpc = 20
  )
  V <- out$V[[1L]]

  expect_identical(unique(out$estimate_scale), "arcsine")
  expect_identical(V$estimate_scale, "arcsine")
  expect_identical(V$vcov_scale, "raw")
  expect_identical(V$diag_contract, "row_se_raw_squared")
  expect_equal(unname(diag(as.matrix(V))), out$se_raw^2, tolerance = 1e-14)
  expect_false(isTRUE(all.equal(unname(diag(as.matrix(V))), out$se^2)))
  expect_identical(out$var_method, rep("arcsine_delta_binomial_bc", 2))
  expect_true(validate.sitemix_estimates(out))
})

test_that("Step 3.8 row validation rejects SUR provenance and scale-contract tampering", {
  counts <- data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 8L,
    c_jt_a = 5L,
    c_jt_b = 4L,
    c_jt_a_b = 3L
  )
  out <- sm_estimate_from_counts(
    counts,
    family = "multivariate",
    indicators = c("a", "b"),
    vst = "arcsine",
    boundary_method = "none",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L
  )

  wrong_rule_v <- out$V[[1L]]
  wrong_rule_v$variance_rule <- rep("plugin", 2)
  expect_true(validate.sm_vcov(wrong_rule_v))
  wrong_rule <- out
  wrong_rule$V <- list(wrong_rule_v, wrong_rule_v)
  expect_error(
    validate.sitemix_estimates(wrong_rule),
    class = "sitemix_error_estimate_vcov_invariant"
  )

  wrong_scale_v <- out$V[[1L]]
  wrong_scale_v$diag_contract <- "row_se_squared"
  expect_true(validate.sm_vcov(wrong_scale_v))
  wrong_scale <- out
  wrong_scale$V <- list(wrong_scale_v, wrong_scale_v)
  expect_error(
    validate.sitemix_estimates(wrong_scale),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})

test_that("Step 3.8 post-construction validation rejects all-row SUR metadata tampering", {
  counts <- data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 8L,
    c_jt_a = 5L,
    c_jt_b = 4L,
    c_jt_a_b = 3L
  )
  out <- sm_estimate_from_counts(
    counts,
    family = "multivariate",
    indicators = c("a", "b"),
    vst = "arcsine",
    boundary_method = "none",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L
  )
  mutations <- list(
    vcov_method = function(v) {
      v$vcov_method <- "working_independence"
      v
    },
    vcov_scale = function(v) {
      v$vcov_scale <- "reference_raw"
      v
    },
    estimate_scale = function(v) {
      v$estimate_scale <- "logit"
      v
    },
    matrix_boundary_rule = function(v) {
      v$matrix_boundary_rule <- "diagonal_boundary_floor"
      v
    },
    scalar_correction_rule = function(v) {
      v$scalar_correction_rule <- rep("none", 2)
      v
    },
    n_jt = function(v) {
      v$n_jt <- 9L
      v
    },
    n_eff = function(v) {
      v$n_eff <- 9
      v
    },
    site_id = function(v) {
      v$site_id <- "forged"
      v
    },
    year = function(v) {
      v$year <- 2025L
      v
    },
    family = function(v) {
      v$family <- "binomial"
      v$vcov_method <- NA_character_
      v
    },
    diagonal_bypass = function(v) {
      v$matrix <- v$matrix * 2
      v$diag_contract <- "not_checked"
      v
    }
  )

  for (mutation in mutations) {
    tampered <- step38_repeat_tampered_v(out, mutation)
    expect_true(validate.sm_vcov(tampered$covariance))
    expect_error(
      validate.sitemix_estimates(tampered$output),
      class = "sitemix_error_estimate_vcov_invariant"
    )
  }
})

test_that("Step 3.8 Wilson scalar and matrix boundary provenance cannot drift together", {
  counts <- data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 4L,
    c_jt_a = 0L,
    c_jt_b = 2L,
    c_jt_a_b = 0L
  )
  out <- sm_estimate_from_counts(
    counts,
    family = "multivariate",
    indicators = c("a", "b"),
    vst = "arcsine",
    boundary_method = "wilson_floor",
    vjt = TRUE,
    min_n = 1L
  )
  mutations <- list(
    scalar_only = function(v) {
      v$scalar_correction_rule[[1L]] <- "none"
      v
    },
    boundary_only = function(v) {
      v$matrix_boundary_rule <- "none"
      v
    },
    both = function(v) {
      v$scalar_correction_rule[[1L]] <- "none"
      v$matrix_boundary_rule <- "none"
      v
    }
  )

  for (mutation in mutations) {
    tampered <- step38_repeat_tampered_v(out, mutation)
    expect_true(validate.sm_vcov(tampered$covariance))
    expect_error(
      validate.sitemix_estimates(tampered$output),
      class = "sitemix_error_estimate_vcov_invariant"
    )
  }
})
