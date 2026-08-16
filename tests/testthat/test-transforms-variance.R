test_that("arcsine transform and back-transform are stable at boundaries", {
  p <- c(0, 0.25, 1)
  transformed <- sitemix:::.sm_transform_arcsine(p)

  expect_equal(transformed, c(0, pi / 6, pi / 2), tolerance = 1e-12)
  expect_equal(sitemix:::.sm_backtransform_arcsine(transformed), p, tolerance = 1e-12)
  expect_equal(sitemix:::.sm_transform_none(p), p)
  expect_equal(sitemix:::.sm_backtransform_logit(c(-Inf, 0, Inf)), c(0, 0.5, 1))

  expect_error(
    sitemix:::.sm_transform_arcsine(-0.01),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_transform_arcsine(1.01),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_transform_none(-0.01),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("probability, n, and count guards reject invalid domains", {
  expect_true(sitemix:::.sm_check_probability(c(0, 0.5, 1)))
  expect_true(sitemix:::.sm_check_probability(0.5, allow_boundary = FALSE))

  expect_error(
    sitemix:::.sm_check_probability(c(0, NA_real_)),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_check_probability("0.5"),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_check_probability(c(-0.1, 0.5)),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_check_probability(c(0, 0.5, 1), allow_boundary = FALSE),
    class = "sitemix_error_estimate_var_method"
  )

  expect_true(sitemix:::.sm_check_positive_n(c(1, 10)))
  expect_error(
    sitemix:::.sm_check_positive_n(c(1, 0)),
    class = "sitemix_error_estimate_zero_n"
  )
  expect_error(
    sitemix:::.sm_check_positive_n(c(1, Inf)),
    class = "sitemix_error_estimate_zero_n"
  )

  recycled <- sitemix:::.sm_check_counts(C = 1, n = c(5, 10))
  expect_equal(recycled$C, c(1, 1))
  expect_equal(recycled$n, c(5, 10))

  expect_error(
    sitemix:::.sm_check_counts(C = c(1, 2), n = c(5, 10, 15)),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_check_counts(C = c(1, NA_real_), n = c(5, 10)),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_check_counts(C = c(-1, 11), n = c(5, 10)),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("Anscombe transform uses locked count and n_eff formulas", {
  p_anscombe <- sitemix:::.sm_anscombe_p(C = 0, n = 10)
  expect_equal(p_anscombe, (0 + 3 / 8) / (10 + 3 / 4), tolerance = 1e-12)
  expect_equal(sitemix:::.sm_anscombe_n_eff(10), 10.5)
  expect_equal(
    sitemix:::.sm_transform_arcsine_anscombe(C = 0, n = 10),
    asin(sqrt((0 + 3 / 8) / (10 + 3 / 4))),
    tolerance = 1e-12
  )
  expect_equal(
    sitemix:::.sm_anscombe_p(C = c(0, 5, 10), n = 10),
    (c(0, 5, 10) + 3 / 8) / (10 + 3 / 4),
    tolerance = 1e-12
  )
  expect_error(
    sitemix:::.sm_anscombe_n_eff(0),
    class = "sitemix_error_estimate_zero_n"
  )
  expect_error(
    sitemix:::.sm_anscombe_n_eff(-1),
    class = "sitemix_error_estimate_zero_n"
  )
})

test_that("logit transform requires interior probabilities", {
  expect_equal(sitemix:::.sm_transform_logit(0.2), log(0.2 / 0.8), tolerance = 1e-12)
  expect_equal(sitemix:::.sm_backtransform_logit(log(0.2 / 0.8)), 0.2, tolerance = 1e-12)

  expect_error(
    sitemix:::.sm_transform_logit(0),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_transform_logit(1),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_transform_logit(-0.1),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_transform_logit(1.1),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("transform dispatcher returns scale metadata and n_eff", {
  arcsine <- sitemix:::.sm_transform_probability(theta_raw = 0.25, n = 100, vst = "arcsine")
  expect_equal(arcsine$theta_hat, pi / 6, tolerance = 1e-12)
  expect_equal(arcsine$n_eff, 100)
  expect_equal(arcsine$estimate_scale, "arcsine")
  expect_equal(arcsine$transform, "arcsine")

  anscombe <- sitemix:::.sm_transform_probability(
    theta_raw = 0,
    C = 0,
    n = 10,
    vst = "arcsine",
    anscombe = TRUE
  )
  expect_equal(anscombe$n_eff, 10.5)
  expect_equal(anscombe$estimate_scale, "arcsine_anscombe")

  raw <- sitemix:::.sm_transform_probability(theta_raw = 0.25, n = 100, vst = "none")
  expect_equal(raw$theta_hat, 0.25)
  expect_equal(raw$estimate_scale, "none")

  logit <- sitemix:::.sm_transform_probability(theta_raw = 0.25, n = 100, vst = "logit")
  expect_equal(logit$theta_hat, log(0.25 / 0.75), tolerance = 1e-12)
  expect_equal(logit$n_eff, 100)
  expect_equal(logit$estimate_scale, "logit")
  expect_equal(logit$transform, "logit")

  anscombe_ignores_theta_raw <- sitemix:::.sm_transform_probability(
    theta_raw = 0.99,
    C = 0,
    n = 10,
    vst = "arcsine",
    anscombe = TRUE
  )
  expect_equal(
    anscombe_ignores_theta_raw$theta_hat,
    asin(sqrt((0 + 3 / 8) / (10 + 3 / 4))),
    tolerance = 1e-12
  )

  anscombe_full <- sitemix:::.sm_transform_probability(
    theta_raw = 1,
    C = 10,
    n = 10,
    vst = "arcsine",
    anscombe = TRUE
  )
  expect_lt(anscombe_full$theta_hat, pi / 2)

  expect_error(
    sitemix:::.sm_transform_probability(theta_raw = 0.25, n = 100, vst = "logit", anscombe = TRUE),
    class = "sitemix_error_anscombe_requires_arcsine"
  )
  expect_error(
    sitemix:::.sm_transform_probability(theta_raw = 0.25, n = 100, vst = c("arcsine", "none")),
    class = "sitemix_error_invalid_vst"
  )
  expect_error(
    sitemix:::.sm_transform_probability(theta_raw = 0.25, n = 100, vst = "sqrt"),
    class = "sitemix_error_invalid_vst"
  )
  expect_error(
    sitemix:::.sm_transform_probability(theta_raw = 0.25, n = 100, vst = "arcsine", anscombe = NA),
    class = "sitemix_error_invalid_anscombe"
  )
  expect_error(
    sitemix:::.sm_transform_probability(theta_raw = 0.25, n = 100, vst = "arcsine", anscombe = "TRUE"),
    class = "sitemix_error_invalid_anscombe"
  )
  expect_error(
    sitemix:::.sm_transform_probability(theta_raw = 0.25, n = Inf, vst = "arcsine"),
    class = "sitemix_error_estimate_zero_n"
  )
  expect_error(
    sitemix:::.sm_transform_probability(theta_raw = 0, n = 100, vst = "logit"),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_transform_probability(theta_raw = 0, n = 10, vst = "arcsine", anscombe = TRUE),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("binomial scalar SE helpers match locked formulas", {
  expect_equal(sitemix:::.sm_arcsine_se(100), 0.05, tolerance = 1e-12)
  expect_equal(sitemix:::.sm_arcsine_se(10), 1 / (2 * sqrt(10)), tolerance = 1e-12)
  expect_equal(sitemix:::.sm_arcsine_se(10.5), 1 / (2 * sqrt(10.5)), tolerance = 1e-12)
  expect_equal(sitemix:::.sm_binomial_se(0.5, 10), sqrt(0.25 / 10), tolerance = 1e-12)
  expect_equal(sitemix:::.sm_binomial_bc_se(0.5, 10), sqrt(0.25 / 9), tolerance = 1e-12)
  expect_equal(sitemix:::.sm_logit_delta_se(0.2, 50), 1 / sqrt(50 * 0.2 * 0.8), tolerance = 1e-12)

  z <- stats::qnorm(0.975)
  expected_wilson <- sqrt(0 + z^2 / (4 * 10^2)) / (1 + z^2 / 10)
  expect_equal(sitemix:::.sm_wilson_se(0, 10, z = z), expected_wilson, tolerance = 1e-12)

  adjusted <- sitemix:::.sm_agresti_coull_adjust(C = 0, n = 10)
  expect_equal(adjusted$p, (z^2 / 2) / (10 + z^2), tolerance = 1e-12)
  expect_equal(adjusted$n, 10 + z^2, tolerance = 1e-12)
  expect_equal(
    sitemix:::.sm_agresti_coull_se(C = 0, n = 10, z = z),
    sqrt(adjusted$p * (1 - adjusted$p) / adjusted$n),
    tolerance = 1e-12
  )
  expect_equal(sitemix:::.sm_agresti_coull_se(C = 0, n = 10, z = z), 0.0929205369362129, tolerance = 1e-12)
  expect_equal(sitemix:::.sm_arcsine_bc_delta_se(0.2, 10), 1 / (2 * sqrt(9)), tolerance = 1e-12)
  expect_equal(sitemix:::.sm_logit_bc_delta_se(0.2, 10), 1 / sqrt(9 * 0.2 * 0.8), tolerance = 1e-12)
})

test_that("scalar SE helpers reject unsupported variance domains", {
  expect_error(
    sitemix:::.sm_arcsine_se(0),
    class = "sitemix_error_estimate_zero_n"
  )
  expect_error(
    sitemix:::.sm_binomial_se(1.2, 10),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_binomial_bc_se(0.5, 1),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_wilson_se(0.5, 10, z = 0),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_wilson_se(0.5, 10, z = NA_real_),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_wilson_se(0.5, 10, z = Inf),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_wilson_se(0.5, 10, z = c(1.96, 2)),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_agresti_coull_se(0, 10, z = 0),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_logit_delta_se(0, 10),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("transformed SE dispatcher returns row var_method labels", {
  arcsine <- sitemix:::.sm_transformed_se(
    theta_raw = 0,
    n = 10,
    n_eff = 10,
    estimate_scale = "arcsine",
    se_raw = 0.07,
    var_method_raw = "wilson_boundary_surrogate"
  )
  expect_equal(arcsine$se, 1 / (2 * sqrt(10)), tolerance = 1e-12)
  expect_equal(arcsine$var_method, "arcsine_vst")

  anscombe <- sitemix:::.sm_transformed_se(
    theta_raw = 0,
    n = 10,
    n_eff = 10.5,
    estimate_scale = "arcsine_anscombe"
  )
  expect_equal(anscombe$se, 1 / (2 * sqrt(10.5)), tolerance = 1e-12)
  expect_equal(anscombe$var_method, "arcsine_anscombe")

  logit <- sitemix:::.sm_transformed_se(
    theta_raw = 2 / 14,
    n = 10,
    estimate_scale = "logit"
  )
  expect_equal(logit$se, 1 / sqrt(10 * (2 / 14) * (12 / 14)), tolerance = 1e-12)
  expect_equal(logit$var_method, "logit_delta")

  arcsine_bc <- sitemix:::.sm_transformed_se(
    theta_raw = 0.2,
    n = 10,
    estimate_scale = "arcsine",
    se_raw = sqrt(0.2 * 0.8 / 9),
    var_method_raw = "binomial_bc"
  )
  expect_equal(arcsine_bc$se, 1 / (2 * sqrt(9)), tolerance = 1e-12)
  expect_equal(arcsine_bc$var_method, "arcsine_delta_binomial_bc")

  logit_bc <- sitemix:::.sm_transformed_se(
    theta_raw = 0.2,
    n = 10,
    estimate_scale = "logit",
    se_raw = sqrt(0.2 * 0.8 / 9),
    var_method_raw = "binomial_bc"
  )
  expect_equal(logit_bc$se, 1 / sqrt(9 * 0.2 * 0.8), tolerance = 1e-12)
  expect_equal(logit_bc$var_method, "logit_delta_binomial_bc")

  raw <- sitemix:::.sm_transformed_se(
    theta_raw = 0,
    n = 10,
    estimate_scale = "none",
    se_raw = 0.07,
    var_method_raw = "wilson_boundary_surrogate"
  )
  expect_equal(raw$se, 0.07)
  expect_equal(raw$var_method, "wilson_boundary_surrogate")
})

test_that("transformed SE dispatcher rejects invalid scale requests", {
  expect_error(
    sitemix:::.sm_transformed_se(theta_raw = 0.2, n = 10, estimate_scale = c("arcsine", "none")),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_transformed_se(theta_raw = 0.2, n = 10, estimate_scale = "sqrt"),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_transformed_se(theta_raw = 0.2, n = 10, estimate_scale = "none", se_raw = 0.1),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_transformed_se(theta_raw = 0, n = 10, estimate_scale = "logit"),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("row var_method validator rejects matrix metadata labels", {
  expect_true(sitemix:::.sm_validate_var_method(c(
    "arcsine_vst",
    "arcsine_delta_binomial_bc",
    "logit_delta_binomial_bc",
    "binomial_bc",
    "arcsine_vst + fh_smooth_loglinear",
    "wilson_boundary_surrogate + fh_smooth_gam"
  )))
  expect_error(
    sitemix:::.sm_validate_var_method("sur"),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_validate_var_method("multinomial"),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_validate_var_method("working_independence"),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_validate_var_method("se_smoothed"),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_validate_var_method("arcsine_vst + smoothed"),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_validate_var_method("arcsine_vst + fh_smooth_loess"),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_validate_var_method(c("arcsine_vst", NA_character_)),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_validate_var_method(TRUE),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("FPC supports scalar or aligned populations and valid censuses", {
  expect_equal(sitemix:::.sm_fpc_multiplier(n = c(50, 80), fpc = NULL), c(1, 1))
  expect_equal(
    sitemix:::.sm_fpc_multiplier(n = 50, fpc = 100),
    sqrt((100 - 50) / (100 - 1)),
    tolerance = 1e-12
  )
  expect_equal(
    sitemix:::.sm_fpc_multiplier(n = c(50, 80), fpc = 100),
    sqrt((100 - c(50, 80)) / (100 - 1)),
    tolerance = 1e-12
  )
  expect_equal(
    sitemix:::.sm_apply_fpc(se = 0.2, n = 50, fpc = 100),
    0.2 * sqrt(50 / 99),
    tolerance = 1e-12
  )
  expect_equal(
    sitemix:::.sm_fpc_variance_multiplier(n = 50, fpc = 100),
    50 / 99,
    tolerance = 1e-12
  )
  expect_equal(
    sitemix:::.sm_apply_fpc_variance(v = matrix(1, 2, 2), n = 50, fpc = 100),
    matrix(50 / 99, 2, 2),
    tolerance = 1e-12
  )

  expect_equal(
    sitemix:::.sm_fpc_multiplier(n = c(50, 80), fpc = c(100, 120)),
    sqrt(c((100 - 50) / (100 - 1), (120 - 80) / (120 - 1))),
    tolerance = 1e-12
  )
  expect_error(
    sitemix:::.sm_fpc_multiplier(n = 50, fpc = NA_real_),
    class = "sitemix_error_invalid_fpc"
  )
  expect_error(
    sitemix:::.sm_fpc_multiplier(n = 50, fpc = "100"),
    class = "sitemix_error_invalid_fpc"
  )
  expect_equal(sitemix:::.sm_fpc_multiplier(n = 50, fpc = 50), 0)
  expect_equal(sitemix:::.sm_fpc_multiplier(n = 1, fpc = 1), 0)
  expect_error(
    sitemix:::.sm_fpc_multiplier(n = 0.25, fpc = 0.5),
    class = "sitemix_error_invalid_fpc"
  )
  expect_error(
    sitemix:::.sm_apply_fpc(se = c(0.2, -0.1), n = c(50, 60), fpc = 100),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_apply_fpc(se = c(0.2, NA_real_), n = c(50, 60), fpc = 100),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_apply_fpc_variance(v = c(0.1, NA_real_), n = 50, fpc = 100),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_apply_fpc_variance(v = matrix(c(1, NA_real_, 0, 1), 2), n = 50, fpc = 100),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("raw binomial scalar helper applies boundary and bias rules", {
  z <- stats::qnorm(0.975)

  wilson <- sitemix:::.sm_binomial_scalar_raw(C = 0, n = 10, boundary_method = "wilson_floor", z = z)
  expect_equal(wilson$theta_raw, 0)
  expect_equal(wilson$se_raw, sitemix:::.sm_wilson_se(0, 10, z = z), tolerance = 1e-12)
  expect_equal(wilson$var_method_raw, "wilson_boundary_surrogate")
  expect_true(wilson$flag_zero_cell)

  agresti <- sitemix:::.sm_binomial_scalar_raw(C = 0, n = 10, boundary_method = "agresti_coull", z = z)
  expect_equal(agresti$theta_raw, 0, tolerance = 1e-12)
  expect_equal(agresti$se_raw, 0.0929205369362129, tolerance = 1e-12)
  expect_equal(agresti$var_method_raw, "agresti_coull_boundary_surrogate")

  interior_bc <- sitemix:::.sm_binomial_scalar_raw(
    C = 5,
    n = 10,
    boundary_method = "wilson_floor",
    bias_correction = "binomial_bc"
  )
  expect_equal(interior_bc$se_raw, sqrt(0.25 / 9), tolerance = 1e-12)
  expect_equal(interior_bc$var_method_raw, "binomial_bc")

  no_boundary <- sitemix:::.sm_binomial_scalar_raw(C = 0, n = 10, boundary_method = "none")
  expect_equal(no_boundary$se_raw, 0)
  expect_equal(no_boundary$var_method_raw, "binomial")
})

test_that("raw binomial scalar helper handles mixed boundary and interior rows", {
  z <- stats::qnorm(0.975)
  raw <- sitemix:::.sm_binomial_scalar_raw(
    C = c(0, 5, 10),
    n = c(10, 10, 10),
    boundary_method = "wilson_floor",
    bias_correction = "binomial_bc",
    z = z
  )

  expect_equal(raw$theta_raw, c(0, 0.5, 1))
  expect_equal(raw$var_method_raw, c("wilson_boundary_surrogate", "binomial_bc", "wilson_boundary_surrogate"))
  expect_equal(raw$scalar_correction_rule, c("wilson_boundary_surrogate", "binomial_bc", "wilson_boundary_surrogate"))
  expect_equal(raw$flag_zero_cell, c(TRUE, FALSE, TRUE))
  expect_equal(raw$se_raw[1], sitemix:::.sm_wilson_se(0, 10, z = z), tolerance = 1e-12)
  expect_equal(raw$se_raw[2], sitemix:::.sm_binomial_bc_se(0.5, 10), tolerance = 1e-12)
  expect_equal(raw$se_raw[3], sitemix:::.sm_wilson_se(1, 10, z = z), tolerance = 1e-12)

  agresti <- sitemix:::.sm_binomial_scalar_raw(
    C = c(0, 5, 10),
    n = c(10, 10, 10),
    boundary_method = "agresti_coull",
    bias_correction = "binomial_bc",
    z = z
  )
  expect_equal(agresti$theta_raw, c(0, 0.5, 1), tolerance = 1e-12)
  expect_equal(
    agresti$var_method_raw,
    c("agresti_coull_boundary_surrogate", "binomial_bc", "agresti_coull_boundary_surrogate")
  )
  expect_equal(
    agresti$scalar_correction_rule,
    c("agresti_coull_boundary_surrogate", "binomial_bc", "agresti_coull_boundary_surrogate")
  )

  none <- sitemix:::.sm_binomial_scalar_raw(
    C = c(0, 5, 10),
    n = c(10, 10, 10),
    boundary_method = "none",
    bias_correction = "binomial_bc",
    z = z
  )
  expect_equal(none$theta_raw, c(0, 0.5, 1))
  expect_equal(none$se_raw[c(1, 3)], c(0, 0))
  expect_equal(none$var_method_raw, c("binomial", "binomial_bc", "binomial"))
  expect_equal(none$scalar_correction_rule, c("none", "binomial_bc", "none"))

  with_fpc <- sitemix:::.sm_binomial_scalar_raw(
    C = c(0, 5, 10),
    n = c(10, 10, 10),
    boundary_method = "wilson_floor",
    bias_correction = "binomial_bc",
    fpc = 100,
    z = z
  )
  expect_equal(
    with_fpc$se_raw,
    raw$se_raw * c(
      sqrt((100 - 10) / (100 - 1)),
      sqrt((100 - 10) / 100),
      sqrt((100 - 10) / (100 - 1))
    ),
    tolerance = 1e-12
  )
  expect_equal(with_fpc$theta_raw, raw$theta_raw)
  expect_equal(with_fpc$var_method_raw, raw$var_method_raw)
  expect_equal(with_fpc$scalar_correction_rule, raw$scalar_correction_rule)
  expect_equal(with_fpc$flag_zero_cell, raw$flag_zero_cell)
})

test_that("boundary and bias validators reject malformed scalar options", {
  expect_true(sitemix:::.sm_validate_boundary_method("wilson_floor"))
  expect_true(sitemix:::.sm_validate_boundary_method("agresti_coull"))
  expect_true(sitemix:::.sm_validate_boundary_method("none"))
  expect_true(sitemix:::.sm_validate_bias_correction(NULL))
  expect_true(sitemix:::.sm_validate_bias_correction("binomial_bc"))

  expect_error(
    sitemix:::.sm_validate_boundary_method(c("wilson_floor", "none")),
    class = "sitemix_error_invalid_boundary"
  )
  expect_error(
    sitemix:::.sm_validate_boundary_method(NA_character_),
    class = "sitemix_error_invalid_boundary"
  )
  expect_error(
    sitemix:::.sm_validate_boundary_method("wald"),
    class = "sitemix_error_invalid_boundary"
  )
  expect_error(
    sitemix:::.sm_validate_boundary_method(TRUE),
    class = "sitemix_error_invalid_boundary"
  )
  expect_error(
    sitemix:::.sm_validate_bias_correction(c("binomial_bc", "binomial_bc")),
    class = "sitemix_error_invalid_bias"
  )
  expect_error(
    sitemix:::.sm_validate_bias_correction(NA_character_),
    class = "sitemix_error_invalid_bias"
  )
  expect_error(
    sitemix:::.sm_validate_bias_correction("jeffreys"),
    class = "sitemix_error_invalid_bias"
  )
  expect_error(
    sitemix:::.sm_validate_bias_correction(TRUE),
    class = "sitemix_error_invalid_bias"
  )
})
