multivariate_engine_data <- function() {
  data.frame(
    site_id = rep("S1", 4),
    year = rep(2024L, 4),
    snap = c(1L, 1L, 0L, 0L),
    frpm = c(1L, 1L, 1L, 0L),
    stringsAsFactors = FALSE
  )
}

multivariate_count_data <- function() {
  data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 4L,
    c_jt_snap = 2L,
    c_jt_frpm = 3L,
    c_jt_snap_frpm = 2L
  )
}

multivariate_k3_student_data <- function() {
  data.frame(
    site_id = rep(c("S1", "S2"), each = 4),
    year = rep(2024L, 8),
    snap = c(1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L),
    frpm = c(1L, 1L, 1L, 0L, 1L, 1L, 0L, 0L),
    wic = c(0L, 1L, 1L, 0L, 1L, 1L, 1L, 1L),
    stringsAsFactors = FALSE
  )
}

multivariate_k3_count_data <- function() {
  data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    n_jt = c(4L, 4L),
    c_jt_snap = c(2L, 0L),
    c_jt_frpm = c(3L, 2L),
    c_jt_wic = c(2L, 4L),
    c_jt_snap_frpm = c(2L, 0L),
    c_jt_snap_wic = c(1L, 0L),
    c_jt_frpm_wic = c(2L, 2L)
  )
}

test_that("multivariate engine emits K rows per site-year on the default arcsine scale", {
  out <- sitemix::sm_estimate(
    multivariate_engine_data(),
    family = "multivariate",
    indicators = c("snap", "frpm"),
    min_n = 2L,
    description = "scenario b"
  )

  expect_s3_class(out, "sitemix_estimates")
  expect_equal(attr(out, "family"), "multivariate")
  expect_equal(attr(out, "description"), "scenario b")
  expect_equal(out$indicator, c("snap", "frpm"))
  expect_equal(out$theta_raw, c(0.5, 0.75))
  expect_equal(out$theta_hat, asin(sqrt(out$theta_raw)), tolerance = 1e-12)
  expect_equal(out$se_raw, c(sqrt(0.5 * 0.5 / 4), sqrt(0.75 * 0.25 / 4)), tolerance = 1e-12)
  expect_equal(out$se, c(0.25, 0.25), tolerance = 1e-12)
  expect_equal(out$estimate_scale, c("arcsine", "arcsine"))
  expect_equal(out$var_method, c("arcsine_vst", "arcsine_vst"))
  expect_equal(out$input_mode, c("student_level", "student_level"))
  expect_false("V" %in% names(out))
  expect_false("K" %in% names(out))
  expect_true(validate.sitemix_estimates(out))
})

test_that("multivariate engine preserves K=3 supplied order and per-cell SUR matrices", {
  indicators <- c("snap", "frpm", "wic")
  student <- sitemix::sm_estimate(
    multivariate_k3_student_data(),
    family = "multivariate",
    indicators = indicators,
    vjt = TRUE,
    min_n = 2L
  )
  counts <- sitemix::sm_estimate_from_counts(
    multivariate_k3_count_data(),
    family = "multivariate",
    indicators = indicators,
    vjt = TRUE,
    min_n = 2L
  )

  expect_equal(student$indicator, rep(indicators, 2))
  expect_equal(counts$indicator, rep(indicators, 2))
  expect_equal(counts$theta_raw, c(0.5, 0.75, 0.5, 0, 0.5, 1))
  expect_equal(counts$theta_hat, c(pi / 4, pi / 3, pi / 4, 0, pi / 4, pi / 2), tolerance = 1e-12)
  expect_equal(counts$se, rep(0.25, 6), tolerance = 1e-12)
  expect_equal(counts$flag_zero_cell, c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE))
  expect_equal(counts$K, rep(3L, 6))

  compare_cols <- setdiff(names(student), c("input_mode", "V"))
  expect_equal(as.data.frame(student[compare_cols]), as.data.frame(counts[compare_cols]), ignore_attr = TRUE)

  expected_s1 <- matrix(
    c(
      1 / 16, 1 / 32, 0,
      1 / 32, 3 / 64, 1 / 32,
      0, 1 / 32, 1 / 16
    ),
    3,
    3,
    byrow = TRUE,
    dimnames = list(indicators, indicators)
  )
  wilson_var <- sitemix:::.sm_wilson_se(0, 4)^2
  expected_s2 <- matrix(
    c(
      wilson_var, 0, 0,
      0, 1 / 16, 0,
      0, 0, wilson_var
    ),
    3,
    3,
    byrow = TRUE,
    dimnames = list(indicators, indicators)
  )

  expect_equal(as.matrix(counts$V[[1]]), expected_s1, tolerance = 1e-12)
  expect_true(sitemix:::.sm_vcov_value_equal(counts$V[[1]], counts$V[[2]]))
  expect_true(sitemix:::.sm_vcov_value_equal(counts$V[[2]], counts$V[[3]]))
  expect_equal(as.matrix(counts$V[[4]]), expected_s2, tolerance = 1e-12)
  expect_true(sitemix:::.sm_vcov_value_equal(counts$V[[4]], counts$V[[5]]))
  expect_true(sitemix:::.sm_vcov_value_equal(counts$V[[5]], counts$V[[6]]))
  expect_equal(counts$V[[4]]$matrix_boundary_rule, "diagonal_boundary_floor")
  expect_equal(
    counts$V[[4]]$scalar_correction_rule,
    c("wilson_boundary_surrogate", "none", "wilson_boundary_surrogate")
  )
})

test_that("multivariate count input is equivalent to student input", {
  student <- sitemix::sm_estimate(
    multivariate_engine_data(),
    family = "multivariate",
    indicators = c("snap", "frpm"),
    min_n = 2L
  )
  counts <- sitemix::sm_estimate_from_counts(
    multivariate_count_data(),
    family = "multivariate",
    indicators = c("snap", "frpm"),
    min_n = 2L
  )

  compare_cols <- setdiff(names(student), "input_mode")
  expect_equal(as.data.frame(student[compare_cols]), as.data.frame(counts[compare_cols]), ignore_attr = TRUE)
  expect_equal(counts$input_mode, c("counts_full_suff", "counts_full_suff"))
})

test_that("multivariate vjt output repeats one SUR sm_vcov and K", {
  out <- sitemix::sm_estimate_from_counts(
    multivariate_count_data(),
    family = "multivariate",
    indicators = c("snap", "frpm"),
    vjt = TRUE,
    min_n = 2L
  )
  expected_v <- matrix(
    c(1 / 16, 1 / 32, 1 / 32, 3 / 64),
    2,
    2,
    byrow = TRUE,
    dimnames = list(c("snap", "frpm"), c("snap", "frpm"))
  )

  expect_equal(names(out)[(ncol(out) - 1):ncol(out)], c("V", "K"))
  expect_equal(out$K, c(2L, 2L))
  expect_s3_class(out$V[[1]], "sm_vcov")
  expect_true(sitemix:::.sm_vcov_value_equal(out$V[[1]], out$V[[2]]))
  expect_equal(as.matrix(out$V[[1]]), expected_v, tolerance = 1e-12)
  expect_equal(out$V[[1]]$indicator_order, c("snap", "frpm"))
  expect_equal(out$V[[1]]$vcov_method, "sur")
  expect_equal(out$V[[1]]$vcov_scale, "raw")
  expect_equal(out$V[[1]]$matrix_rank, 2L)
  expect_equal(out$var_method, c("arcsine_vst", "arcsine_vst"))
  expect_equal(out$se, c(0.25, 0.25), tolerance = 1e-12)
  expect_equal(out$se_raw, unname(sqrt(diag(expected_v))), tolerance = 1e-12)
  expect_false(isTRUE(all.equal(out$se, out$se_raw, tolerance = 1e-12)))
  expect_false("vcov_method" %in% names(out))
  expect_true(validate.sitemix_estimates(out))
})

test_that("multivariate binomial correction applies to the whole raw matrix", {
  out <- sitemix::sm_estimate_from_counts(
    multivariate_count_data(),
    family = "multivariate",
    indicators = c("snap", "frpm"),
    vst = "none",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 2L
  )

  expect_equal(out$theta_hat, out$theta_raw)
  expect_equal(out$se, sqrt(c(0.5 * 0.5 / 3, 0.75 * 0.25 / 3)), tolerance = 1e-12)
  expect_equal(out$var_method, c("binomial_bc", "binomial_bc"))
  expect_equal(out$V[[1]]$scalar_correction_rule, c("binomial_bc", "binomial_bc"))
  expect_equal(unname(diag(as.matrix(out$V[[1]]))), c(1 / 12, 1 / 16), tolerance = 1e-12)
  expect_equal(out$se^2, unname(diag(as.matrix(out$V[[1]]))), tolerance = 1e-12)
  expect_identical(out$V[[1]]$variance_rule, rep("design_corrected", 2))
  expect_identical(out$V[[1]]$diag_contract, "row_se_raw_squared")
})

test_that("multivariate boundary behavior is explicit for matrix output", {
  counts <- data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 4L,
    c_jt_snap = 0L,
    c_jt_frpm = 2L,
    c_jt_snap_frpm = 0L
  )
  raw <- sitemix::sm_estimate_from_counts(
    counts,
    family = "multivariate",
    indicators = c("snap", "frpm"),
    vst = "none",
    vjt = TRUE,
    min_n = 2L
  )

  expect_equal(raw$var_method, c("wilson_boundary_surrogate", "binomial"))
  expect_equal(raw$V[[1]]$matrix_boundary_rule, "diagonal_boundary_floor")
  expect_equal(raw$V[[1]]$scalar_correction_rule, c("wilson_boundary_surrogate", "none"))
  expect_equal(as.matrix(raw$V[[1]])["snap", "frpm"], 0)
  expect_equal(as.matrix(raw$V[[1]])["snap", "snap"], sitemix:::.sm_wilson_se(0, 4)^2, tolerance = 1e-12)

  scalar_only <- sitemix::sm_estimate_from_counts(
    counts,
    family = "multivariate",
    indicators = c("snap", "frpm"),
    vst = "none",
    boundary_method = "agresti_coull",
    vjt = FALSE,
    min_n = 2L
  )
  expect_equal(scalar_only$theta_raw, c(0, 0.5))
  expect_equal(
    scalar_only$var_method,
    c("agresti_coull_boundary_surrogate", "binomial")
  )
  expect_false("V" %in% names(scalar_only))

  expect_error(
    sitemix::sm_estimate_from_counts(
      counts,
      family = "multivariate",
      indicators = c("snap", "frpm"),
      boundary_method = "agresti_coull",
      vjt = TRUE
    ),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})

test_that("multivariate public dispatch preserves exact classes", {
  expect_error(
    sitemix::sm_estimate(multivariate_engine_data(), family = "multivariate", indicator = "snap"),
    class = "sitemix_error_invalid_indicator"
  )
  expect_error(
    sitemix::sm_estimate(multivariate_engine_data(), family = "multivariate", indicators = "snap"),
    class = "sitemix_error_invalid_indicators"
  )
  expect_error(
    sitemix::sm_estimate_from_counts(
      data.frame(site_id = "S1", year = 2024L, n_jt = 4L, c_jt_snap = 2L, c_jt_frpm = 2L, c_jt_frpm_snap = 1L),
      family = "multivariate",
      indicators = c("snap", "frpm")
    ),
    class = "sitemix_error_input_columns"
  )
  expect_error(
    sitemix::sm_estimate_from_counts(
      data.frame(site_id = "S1", year = 2024L, n_jt = 4L, c_jt_snap = 0L, c_jt_frpm = 2L, c_jt_snap_frpm = 0L),
      family = "multivariate",
      indicators = c("snap", "frpm"),
      vst = "logit"
    ),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("multivariate engine propagates FPC end-to-end through public API", {
  n_val <- 10L
  N_val <- 100
  fpc_scalar <- sqrt((N_val - n_val) / (N_val - 1))
  fpc_variance <- (N_val - n_val) / (N_val - 1)

  fixture <- data.frame(
    site_id = "site_A",
    year = 2024L,
    n_jt = n_val,
    c_jt_a = 4L,
    c_jt_b = 6L,
    c_jt_a_b = 3L,
    stringsAsFactors = FALSE
  )

  no_fpc <- sm_estimate(
    fixture, family = "multivariate", indicators = c("a", "b"),
    from_counts = TRUE, vjt = TRUE, fpc = NULL
  )
  fpc <- sm_estimate(
    fixture, family = "multivariate", indicators = c("a", "b"),
    from_counts = TRUE, vjt = TRUE, fpc = N_val
  )

  # se_raw scales by sqrt((N - n) / (N - 1))
  expect_equal(fpc$se_raw, no_fpc$se_raw * fpc_scalar, tolerance = 1e-12)

  # V matrix entries scale by (N - n) / (N - 1)
  expect_equal(
    as.matrix(fpc$V[[1]]),
    as.matrix(no_fpc$V[[1]]) * fpc_variance,
    tolerance = 1e-12
  )

  # Transformed scalar uncertainty carries the same approved SRSWOR FPC.
  expect_equal(fpc$se, no_fpc$se * fpc_scalar, tolerance = 1e-12)

  # Accountability flag is unchanged by FPC
  expect_equal(
    fpc$flag_below_accountability,
    no_fpc$flag_below_accountability
  )
})
