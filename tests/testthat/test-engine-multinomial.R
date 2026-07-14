multinomial_engine_data <- function() {
  data.frame(
    site_id = rep("S1", 10),
    year = rep(2024L, 10),
    language = c(rep("eng", 6), rep("spa", 3), "oth"),
    stringsAsFactors = FALSE
  )
}

multinomial_count_data <- function() {
  data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 10L,
    c_jt_eng = 6L,
    c_jt_spa = 3L,
    c_jt_oth = 1L
  )
}

test_that("multinomial engine emits all full-simplex rows on default arcsine scale", {
  out <- sitemix::sm_estimate(
    multinomial_engine_data(),
    family = "multinomial",
    indicator = "language",
    min_n = 2L,
    description = "scenario c"
  )

  expect_s3_class(out, "sitemix_estimates")
  expect_equal(attr(out, "family"), "multinomial")
  expect_equal(attr(out, "description"), "scenario c")
  expect_equal(out$indicator, c("eng", "oth", "spa"))
  expect_equal(out$theta_raw, c(0.6, 0.1, 0.3))
  expect_equal(out$theta_hat, asin(sqrt(out$theta_raw)), tolerance = 1e-12)
  expect_equal(out$se, rep(1 / (2 * sqrt(10)), 3), tolerance = 1e-12)
  expect_equal(out$estimate_scale, rep("arcsine", 3))
  expect_equal(out$var_method, rep("arcsine_vst", 3))
  expect_equal(out$input_mode, rep("student_level", 3))
  expect_false("V" %in% names(out))
  expect_false("K" %in% names(out))
  expect_true(validate.sitemix_estimates(out))
})

test_that("multinomial count input can reorder the full category set", {
  out <- sitemix::sm_estimate_from_counts(
    multinomial_count_data(),
    family = "multinomial",
    indicators = c("spa", "eng", "oth"),
    min_n = 2L
  )

  expect_equal(out$indicator, c("spa", "eng", "oth"))
  expect_equal(out$theta_raw, c(0.3, 0.6, 0.1))
  expect_equal(out$input_mode, rep("counts_full_suff", 3))
})

test_that("multinomial student and count inputs are equivalent after matching order", {
  student <- sitemix::sm_estimate(
    multinomial_engine_data(),
    family = "multinomial",
    indicator = "language",
    min_n = 2L
  )
  counts <- sitemix::sm_estimate_from_counts(
    multinomial_count_data(),
    family = "multinomial",
    indicators = c("eng", "oth", "spa"),
    min_n = 2L
  )

  compare_cols <- setdiff(names(student), "input_mode")
  expect_equal(as.data.frame(student[compare_cols]), as.data.frame(counts[compare_cols]), ignore_attr = TRUE)
  expect_equal(counts$input_mode, rep("counts_full_suff", 3))
})

test_that("multinomial factor levels control student output order", {
  df <- multinomial_engine_data()
  df$language <- factor(df$language, levels = c("spa", "eng", "oth"))
  out <- sitemix::sm_estimate(
    df,
    family = "multinomial",
    indicator = "language",
    min_n = 2L
  )

  expect_equal(out$indicator, c("spa", "eng", "oth"))
  expect_equal(out$theta_raw, c(0.3, 0.6, 0.1))
})

test_that("multinomial count reorder controls V dimnames and row order", {
  cats <- c("spa", "eng", "oth")
  counts <- data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 10L,
    c_jt_eng = 6L,
    c_jt_oth = 1L,
    c_jt_spa = 3L
  )
  out <- sitemix::sm_estimate_from_counts(
    counts,
    family = "multinomial",
    indicators = cats,
    vjt = TRUE,
    min_n = 2L
  )
  expected <- matrix(
    c(
      0.021, -0.018, -0.003,
      -0.018, 0.024, -0.006,
      -0.003, -0.006, 0.009
    ),
    3,
    3,
    byrow = TRUE,
    dimnames = list(cats, cats)
  )

  expect_equal(out$indicator, cats)
  expect_equal(out$theta_raw, c(0.3, 0.6, 0.1))
  expect_equal(out$se_raw, sqrt(c(0.3 * 0.7 / 10, 0.6 * 0.4 / 10, 0.1 * 0.9 / 10)), tolerance = 1e-12)
  expect_equal(names(out), c(sitemix:::.sm_sitemix_columns, "V", "K"))
  expect_equal(as.matrix(out$V[[1]]), expected, tolerance = 1e-12)
  expect_equal(out$V[[1]]$indicator_order, cats)
})

test_that("multinomial vjt output repeats full-simplex sm_vcov and K", {
  out <- sitemix::sm_estimate_from_counts(
    multinomial_count_data(),
    family = "multinomial",
    indicators = c("eng", "spa", "oth"),
    vjt = TRUE,
    min_n = 2L
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
    dimnames = list(c("eng", "spa", "oth"), c("eng", "spa", "oth"))
  )

  expect_equal(names(out)[(ncol(out) - 1):ncol(out)], c("V", "K"))
  expect_equal(out$K, rep(3L, 3))
  expect_s3_class(out$V[[1]], "sm_vcov")
  expect_true(sitemix:::.sm_vcov_value_equal(out$V[[1]], out$V[[2]]))
  expect_true(sitemix:::.sm_vcov_value_equal(out$V[[2]], out$V[[3]]))
  expect_equal(as.matrix(out$V[[1]]), expected, tolerance = 1e-12)
  expect_equal(as.vector(as.matrix(out$V[[1]]) %*% rep(1, 3)), rep(0, 3), tolerance = 1e-12)
  expect_equal(out$V[[1]]$vcov_method, "multinomial")
  expect_equal(out$V[[1]]$vcov_scale, "raw")
  expect_equal(out$V[[1]]$matrix_rank, 2L)
  expect_equal(out$V[[1]]$positive_support, 3L)
  expect_false("vcov_method" %in% names(out))
  expect_true(validate.sitemix_estimates(out))
})

test_that("multinomial boundary preserves simplex V and scalar-only Wilson floors", {
  counts <- data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 5L,
    c_jt_eng = 0L,
    c_jt_spa = 2L,
    c_jt_oth = 3L
  )
  raw <- sitemix::sm_estimate_from_counts(
    counts,
    family = "multinomial",
    indicators = c("eng", "spa", "oth"),
    vst = "none",
    vjt = TRUE,
    min_n = 2L
  )

  expect_equal(raw$theta_raw, c(0, 0.4, 0.6))
  expect_equal(
    raw$var_method,
    c("wilson_boundary_surrogate", "binomial", "binomial")
  )
  expect_gt(raw$se_raw[[1]], 0)
  expect_equal(as.matrix(raw$V[[1]])["eng", "eng"], 0)
  expect_equal(raw$V[[1]]$matrix_boundary_rule, "simplex_preserve")
  expect_equal(
    raw$V[[1]]$scalar_correction_rule,
    c("wilson_boundary_surrogate", "none", "none")
  )
  expect_equal(as.vector(as.matrix(raw$V[[1]]) %*% rep(1, 3)), rep(0, 3), tolerance = 1e-12)

  scalar_only <- sitemix::sm_estimate_from_counts(
    counts,
    family = "multinomial",
    indicators = c("eng", "spa", "oth"),
    vst = "none",
    boundary_method = "agresti_coull",
    vjt = FALSE,
    min_n = 2L
  )
  expect_equal(scalar_only$theta_raw, c(0, 0.4, 0.6))
  expect_equal(
    scalar_only$var_method,
    c("agresti_coull_boundary_surrogate", "binomial", "binomial")
  )
  expect_false("V" %in% names(scalar_only))

  expect_error(
    sitemix::sm_estimate_from_counts(
      counts,
      family = "multinomial",
      indicators = c("eng", "spa", "oth"),
      boundary_method = "agresti_coull",
      vjt = TRUE
    ),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})

test_that("multinomial binomial correction applies to the whole simplex matrix", {
  out <- sitemix::sm_estimate_from_counts(
    multinomial_count_data(),
    family = "multinomial",
    indicators = c("eng", "spa", "oth"),
    vst = "none",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 2L
  )

  expect_equal(out$theta_hat, out$theta_raw)
  expect_equal(out$var_method, rep("binomial_bc", 3))
  expect_equal(out$V[[1]]$scalar_correction_rule, rep("binomial_bc", 3))
  expect_equal(unname(diag(as.matrix(out$V[[1]]))), c(0.024, 0.021, 0.009) * 10 / 9, tolerance = 1e-12)
  expect_equal(out$se^2, unname(diag(as.matrix(out$V[[1]]))), tolerance = 1e-12)
  expect_identical(out$V[[1]]$variance_rule, rep("design_corrected", 3))
  expect_identical(out$V[[1]]$diag_contract, "row_se_raw_squared")
})

test_that("multinomial public dispatch preserves exact classes", {
  expect_error(
    sitemix::sm_estimate(multinomial_engine_data(), family = "multinomial", indicators = c("eng", "spa")),
    class = "sitemix_error_invalid_indicator"
  )
  expect_error(
    sitemix::sm_estimate_from_counts(
      data.frame(site_id = "S1", year = 2024L, n_jt = 5L, c_jt_eng = 0L, c_jt_spa = 2L, c_jt_oth = 3L),
      family = "multinomial",
      indicators = c("eng", "spa", "oth"),
      vst = "logit"
    ),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix::sm_estimate_from_counts(
      data.frame(site_id = "S1", year = 2024L, n_jt = 5L, c_jt_eng = 3L, c_jt_spa = 1L),
      family = "multinomial",
      indicators = c("eng", "spa")
    ),
    class = "sitemix_error_input_indicator_count"
  )
  expect_error(
    sitemix::sm_estimate_from_counts(
      multinomial_count_data(),
      family = "multinomial",
      indicators = c("eng", "spa")
    ),
    class = "sitemix_error_input_indicator_count"
  )
})

test_that("multinomial engine propagates FPC end-to-end through public API", {
  n_val <- 10L
  N_val <- 100
  fpc_scalar <- sqrt((N_val - n_val) / (N_val - 1))
  fpc_variance <- (N_val - n_val) / (N_val - 1)

  fixture <- data.frame(
    site_id = "site_A",
    year = 2024L,
    n_jt = n_val,
    c_jt_eng = 6L,
    c_jt_spa = 3L,
    c_jt_oth = 1L,
    stringsAsFactors = FALSE
  )

  no_fpc <- sm_estimate(
    fixture, family = "multinomial", indicators = c("eng", "spa", "oth"),
    from_counts = TRUE, vjt = TRUE, fpc = NULL
  )
  fpc <- sm_estimate(
    fixture, family = "multinomial", indicators = c("eng", "spa", "oth"),
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
