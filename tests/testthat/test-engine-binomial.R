binomial_engine_data <- function() {
  data.frame(
    site_id = c("S1", "S1", "S1", "S1", "S2", "S2", "S2", "S2"),
    year = c(2024L, 2024L, 2024L, 2024L, 2024L, 2024L, 2025L, 2025L),
    absent = c(1L, 0L, 1L, 0L, 0L, 0L, 1L, 1L),
    stringsAsFactors = FALSE
  )
}

test_that("binomial engine default arcsine output is schema-valid", {
  out <- sitemix:::.sm_engine_binomial(
    binomial_engine_data(),
    indicator = "absent",
    min_n = 3L,
    description = "binomial default"
  )

  expect_s3_class(out, "sitemix_estimates")
  expect_equal(attr(out, "family"), "binomial")
  expect_equal(attr(out, "description"), "binomial default")
  expect_equal(out$site_id, c("S1", "S2", "S2"))
  expect_equal(out$year, c(2024L, 2024L, 2025L))
  expect_equal(out$theta_raw, c(0.5, 0, 1))
  expect_equal(out$theta_hat, asin(sqrt(out$theta_raw)))
  expect_equal(out$se, 1 / (2 * sqrt(out$n)))
  expect_equal(out$var_method, rep("arcsine_vst", 3))
  expect_equal(out$input_mode, rep("student_level", 3))
  expect_equal(out$flag_small_n, c(FALSE, TRUE, TRUE))
  expect_equal(out$flag_zero_cell, c(FALSE, TRUE, TRUE))
  expect_true(validate.sitemix_estimates(out))
})

test_that("binomial engine count input is equivalent to student input", {
  student <- sitemix:::.sm_engine_binomial(
    binomial_engine_data(),
    indicator = "absent",
    min_n = 3L
  )
  counts <- data.frame(
    site_id = c("S1", "S2", "S2"),
    year = c(2024L, 2024L, 2025L),
    n_jt = c(4L, 2L, 2L),
    c_jt_absent = c(2L, 0L, 2L)
  )
  supplied <- sitemix:::.sm_engine_binomial(
    counts,
    indicator = "absent",
    from_counts = TRUE,
    min_n = 3L
  )

  compare_cols <- setdiff(names(student), "input_mode")
  expect_equal(
    as.data.frame(student[compare_cols]),
    as.data.frame(supplied[compare_cols]),
    ignore_attr = TRUE
  )
  expect_equal(student$input_mode, rep("student_level", 3))
  expect_equal(supplied$input_mode, rep("counts_full_suff", 3))
})

test_that("binomial engine raw scale preserves boundary and bias provenance", {
  raw <- sitemix:::.sm_engine_binomial(
    binomial_engine_data(),
    indicator = "absent",
    vst = "none",
    min_n = 1L
  )

  z <- stats::qnorm(0.975)
  expect_equal(raw$theta_hat, raw$theta_raw)
  expect_equal(raw$se, raw$se_raw)
  expect_equal(
    raw$var_method,
    c("binomial", "wilson_boundary_surrogate", "wilson_boundary_surrogate")
  )
  expect_equal(raw$se_raw[[2]], sitemix:::.sm_wilson_se(0, 2, z = z), tolerance = 1e-12)

  ac <- sitemix:::.sm_engine_binomial(
    data.frame(site_id = "S1", year = 2024L, absent = c(0L, 0L)),
    indicator = "absent",
    vst = "none",
    boundary_method = "agresti_coull",
    min_n = 1L
  )
  expect_equal(ac$theta_raw, 0)
  expect_equal(ac$theta_hat, 0)
  expect_equal(ac$var_method, "agresti_coull_boundary_surrogate")
  expect_gt(ac$se, 0)

  interior <- data.frame(site_id = "S1", year = 2024L, absent = c(1L, 1L, 0L, 0L))
  bc <- sitemix:::.sm_engine_binomial(
    interior,
    indicator = "absent",
    vst = "none",
    bias_correction = "binomial_bc",
    min_n = 1L
  )
  expect_equal(bc$var_method, "binomial_bc")
  expect_equal(bc$se, sqrt(0.25 / 3), tolerance = 1e-12)

  bc_arcsine <- sitemix:::.sm_engine_binomial(
    interior,
    indicator = "absent",
    vst = "arcsine",
    bias_correction = "binomial_bc",
    min_n = 1L
  )
  expect_equal(bc_arcsine$se, 1 / (2 * sqrt(3)), tolerance = 1e-12)
  expect_equal(bc_arcsine$var_method, "arcsine_delta_binomial_bc")

  bc_logit <- sitemix:::.sm_engine_binomial(
    interior,
    indicator = "absent",
    vst = "logit",
    bias_correction = "binomial_bc",
    min_n = 1L
  )
  expect_equal(bc_logit$se, 1 / sqrt(3 * 0.5 * 0.5), tolerance = 1e-12)
  expect_equal(bc_logit$var_method, "logit_delta_binomial_bc")

  bc_logit_v <- sitemix:::.sm_engine_binomial(
    interior,
    indicator = "absent",
    vst = "logit",
    bias_correction = "binomial_bc",
    vjt = TRUE,
    min_n = 1L
  )
  expect_equal(
    as.matrix(bc_logit_v$V[[1]]),
    matrix(
      bc_logit_v$se[[1]]^2,
      1,
      1,
      dimnames = list("absent", "absent")
    ),
    tolerance = 1e-12
  )
  expect_equal(bc_logit_v$V[[1]]$scalar_correction_rule, "binomial_bc")
  expect_equal(bc_logit_v$V[[1]]$vcov_scale, "logit_delta")
})

test_that("binomial engine supports logit interior and Anscombe arcsine rows", {
  interior <- data.frame(site_id = "S1", year = 2024L, absent = c(1L, 0L, 0L, 0L))
  logit <- sitemix:::.sm_engine_binomial(
    interior,
    indicator = "absent",
    vst = "logit",
    min_n = 1L
  )
  expect_equal(logit$theta_hat, log(0.25 / 0.75), tolerance = 1e-12)
  expect_equal(logit$se, 1 / sqrt(4 * 0.25 * 0.75), tolerance = 1e-12)
  expect_equal(logit$var_method, "logit_delta")

  expect_error(
    sitemix:::.sm_engine_binomial(
      data.frame(site_id = "S1", year = 2024L, absent = c(0L, 0L)),
      indicator = "absent",
      vst = "logit",
      min_n = 1L
    ),
    class = "sitemix_error_estimate_var_method"
  )

  anscombe <- sitemix:::.sm_engine_binomial(
    binomial_engine_data(),
    indicator = "absent",
    anscombe = TRUE,
    min_n = 1L
  )
  expect_equal(anscombe$estimate_scale, rep("arcsine_anscombe", 3))
  expect_equal(anscombe$n_eff, anscombe$n + 0.5)
  expect_equal(anscombe$var_method, rep("arcsine_anscombe", 3))
})

test_that("binomial engine applies FPC to raw companion standard errors", {
  raw <- sitemix:::.sm_engine_binomial(
    data.frame(site_id = "S1", year = 2024L, absent = c(1L, 1L, 0L, 0L)),
    indicator = "absent",
    vst = "none",
    fpc = 10,
    min_n = 1L
  )

  expect_equal(
    raw$se_raw,
    sqrt(0.25 / 4) * sqrt((10 - 4) / (10 - 1)),
    tolerance = 1e-12
  )
})

test_that("binomial engine emits 1x1 sm_vcov when vjt is requested", {
  out <- sitemix:::.sm_engine_binomial(
    binomial_engine_data(),
    indicator = "absent",
    vjt = TRUE,
    min_n = 3L
  )

  expect_true("V" %in% names(out))
  expect_false("K" %in% names(out))
  expect_s3_class(out$V[[1]], "sm_vcov")
  expect_equal(as.matrix(out$V[[1]]), matrix(out$se[[1]]^2, 1, 1, dimnames = list("absent", "absent")))
  expect_equal(out$V[[1]]$family, "binomial")
  expect_true(is.na(out$V[[1]]$vcov_method))
  expect_equal(out$V[[1]]$vcov_scale, "arcsine_delta")
  expect_equal(out$V[[1]]$n_jt, out$n[[1]])
  expect_equal(out$V[[1]]$n_eff, out$n_eff[[1]])
  expect_true(validate.sitemix_estimates(out))
})

test_that("binomial engine preserves n=1 raw boundary behavior from counts", {
  counts <- data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    n_jt = c(1L, 1L),
    c_jt_absent = c(0L, 1L)
  )
  out <- sitemix:::.sm_engine_binomial(
    counts,
    indicator = "absent",
    from_counts = TRUE,
    vst = "none",
    boundary_method = "none",
    min_n = 2L
  )

  expect_equal(out$theta_raw, c(0, 1))
  expect_equal(out$se_raw, c(0, 0))
  expect_equal(out$se, c(0, 0))
  expect_equal(out$var_method, c("binomial", "binomial"))
  expect_equal(out$flag_zero_cell, c(TRUE, TRUE))
  expect_equal(out$flag_small_n, c(TRUE, TRUE))
  expect_equal(out$input_mode, c("counts_full_suff", "counts_full_suff"))
  expect_true(validate.sitemix_estimates(out))
})

test_that("binomial engine propagates exact argument and input classes", {
  expect_error(
    sitemix:::.sm_engine_binomial(binomial_engine_data(), indicator = "absent", vst = "identity"),
    class = "sitemix_error_invalid_vst"
  )
  expect_error(
    sitemix:::.sm_engine_binomial(binomial_engine_data(), indicator = "missing"),
    class = "sitemix_error_input_columns"
  )
  expect_error(
    sitemix:::.sm_engine_binomial(binomial_engine_data(), indicator = "absent", anscombe = TRUE, vst = "logit"),
    class = "sitemix_error_anscombe_requires_arcsine"
  )
})

test_that("AC boundary row keeps the observed point and uses adjusted-Wald SE", {
  counts <- data.frame(
    site_id = c("boundary", "interior"),
    year = c(2024L, 2024L),
    n_jt = c(2L, 10L),
    c_jt_absent = c(0L, 2L)
  )

  out <- sm_estimate(
    counts, family = "binomial", indicator = "absent",
    from_counts = TRUE,
    vst = "none",
    boundary_method = "agresti_coull",
    min_n = 1L
  )

  ac_row <- out[out$site_id == "boundary", ]
  int_row <- out[out$site_id == "interior", ]

  expect_equal(ac_row$var_method, "agresti_coull_boundary_surrogate")
  expect_equal(ac_row$theta_raw, 0, tolerance = 1e-12)
  z <- stats::qnorm(0.975)
  n_tilde <- ac_row$n + z^2
  p_tilde <- (z^2 / 2) / n_tilde
  expected_se_ac <- sqrt(p_tilde * (1 - p_tilde) / n_tilde)
  expect_equal(ac_row$se, expected_se_ac, tolerance = 1e-12)
  expect_equal(ac_row$n, 2L)

  # Interior row: plain binomial SE under vst="none".
  expect_equal(int_row$var_method, "binomial")
  expect_equal(int_row$theta_raw, 2 / 10, tolerance = 1e-12)
  expected_se_int <- sqrt(int_row$theta_raw * (1 - int_row$theta_raw) / int_row$n)
  expect_equal(int_row$se, expected_se_int, tolerance = 1e-12)
})

test_that("AC boundary matrix output remains illegal for Scenario A", {
  counts <- data.frame(
    site_id = "boundary",
    year = 2024L,
    n_jt = 10L,
    c_jt_absent = 0L
  )

  expect_error(
    sm_estimate(
      counts,
      family = "binomial",
      indicator = "absent",
      from_counts = TRUE,
      vst = "none",
      boundary_method = "agresti_coull",
      vjt = TRUE,
      min_n = 1L
    ),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})
