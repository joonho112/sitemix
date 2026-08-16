fpc_scalar_counts <- function(C = 3L, n = 8L) {
  data.frame(
    site_id = "A",
    year = 2024L,
    n_jt = as.integer(n),
    c_jt_x = as.integer(C)
  )
}

test_that("FPC propagates through raw, arcsine, logit, and Anscombe scales", {
  data <- fpc_scalar_counts()
  raw <- sm_estimate_from_counts(
    data, family = "binomial", indicator = "x", vst = "none", fpc = 20
  )
  arcsine <- sm_estimate_from_counts(
    data, family = "binomial", indicator = "x", vst = "arcsine", fpc = 20
  )
  logit <- sm_estimate_from_counts(
    data, family = "binomial", indicator = "x", vst = "logit", fpc = 20
  )
  anscombe <- sm_estimate_from_counts(
    data,
    family = "binomial",
    indicator = "x",
    vst = "arcsine",
    boundary_method = "none",
    anscombe = TRUE,
    fpc = 20
  )

  expect_equal(raw$theta_raw, 0.375, tolerance = 1e-15)
  expect_equal(raw$theta_hat, 0.375, tolerance = 1e-15)
  expect_equal(raw$se^2, 0.0185032894736842, tolerance = 1e-14)
  expect_equal(arcsine$theta_hat, 0.6590580358264089, tolerance = 1e-15)
  expect_equal(arcsine$se^2, 0.0197368421052632, tolerance = 1e-14)
  expect_equal(logit$theta_hat, -0.5108256237659907, tolerance = 1e-15)
  expect_equal(logit$se^2, 0.3368421052631579, tolerance = 1e-14)
  expect_equal(anscombe$theta_hat, 0.6700931577543916, tolerance = 1e-15)
  expect_equal(anscombe$se^2, 0.0185758513931889, tolerance = 1e-14)
  expect_equal(raw$n_eff, 8)
  expect_equal(arcsine$n_eff, 8)
  expect_equal(logit$n_eff, 8)
  expect_equal(anscombe$n_eff, 8.5)

  for (result in list(raw, arcsine, logit, anscombe)) {
    expect_equal(result$population_size, 20)
    expect_equal(result$sampling_fraction, 0.4)
    expect_equal(result$fpc_variance_multiplier, 12 / 19, tolerance = 1e-15)
    expect_equal(result$fpc_se_multiplier, sqrt(12 / 19), tolerance = 1e-15)
    expect_equal(result$variance_multiplier_applied, 12 / 19, tolerance = 1e-15)
    expect_equal(result$se_multiplier_applied, sqrt(12 / 19), tolerance = 1e-15)
    expect_identical(result$sampling_design, "SRSWOR")
    expect_identical(result$variance_rule, "plugin")
  }
})

test_that("binomial_bc uses the design-corrected FPC formula on every delta scale", {
  data <- fpc_scalar_counts()
  raw <- sm_estimate_from_counts(
    data,
    family = "binomial",
    indicator = "x",
    vst = "none",
    bias_correction = "binomial_bc",
    fpc = 20
  )
  arcsine <- sm_estimate_from_counts(
    data,
    family = "binomial",
    indicator = "x",
    vst = "arcsine",
    bias_correction = "binomial_bc",
    fpc = 20
  )
  logit <- sm_estimate_from_counts(
    data,
    family = "binomial",
    indicator = "x",
    vst = "logit",
    bias_correction = "binomial_bc",
    fpc = 20
  )

  expect_equal(raw$se^2, 0.0200892857142857, tolerance = 1e-14)
  expect_equal(arcsine$se^2, 0.0214285714285714, tolerance = 1e-14)
  expect_equal(logit$se^2, 0.3657142857142857, tolerance = 1e-14)
  expect_identical(raw$var_method, "binomial_bc")
  expect_identical(arcsine$var_method, "arcsine_delta_binomial_bc")
  expect_identical(logit$var_method, "logit_delta_binomial_bc")
  expect_identical(raw$variance_rule, "design_corrected")
  expect_identical(arcsine$variance_rule, "design_corrected")
  expect_identical(logit$variance_rule, "design_corrected")
  expect_equal(raw$fpc_variance_multiplier, 12 / 19, tolerance = 1e-15)
  expect_equal(raw$variance_multiplier_applied, 12 / 20, tolerance = 1e-15)
  expect_equal(raw$se_multiplier_applied, sqrt(12 / 20), tolerance = 1e-15)
})

test_that("a census is valid and has zero uncertainty without changing points or n_eff", {
  interior <- fpc_scalar_counts(C = 3L, n = 8L)
  for (vst in c("none", "arcsine", "logit")) {
    result <- sm_estimate_from_counts(
      interior, family = "binomial", indicator = "x", vst = vst, fpc = 8
    )
    expect_equal(result$se_raw, 0)
    expect_equal(result$se, 0)
    expect_equal(result$fpc_variance_multiplier, 0)
    expect_equal(result$sampling_fraction, 1)
    expect_equal(result$n_eff, 8)
  }

  anscombe <- sm_estimate_from_counts(
    interior,
    family = "binomial",
    indicator = "x",
    vst = "arcsine",
    boundary_method = "none",
    anscombe = TRUE,
    fpc = 8
  )
  expect_equal(anscombe$se, 0)
  expect_equal(anscombe$n_eff, 8.5)

  one_unit <- sm_estimate_from_counts(
    fpc_scalar_counts(C = 1L, n = 1L),
    family = "binomial",
    indicator = "x",
    vst = "none",
    fpc = 1
  )
  expect_equal(one_unit$theta_raw, 1)
  expect_equal(one_unit$se_raw, 0)
  expect_equal(one_unit$se, 0)
})

test_that("input-row-aligned population sizes are keyed and group-constant", {
  data <- data.frame(
    school = c(rep("A", 4L), rep("B", 5L)),
    school_year = 2024L,
    x = c(1L, 1L, 0L, 0L, 1L, 1L, 1L, 0L, 0L),
    population_size = c(rep(4L, 4L), rep(10L, 5L))
  )
  result <- sm_estimate(
    data,
    family = "binomial",
    indicator = "x",
    id_cols = c("school", "school_year"),
    vst = "none",
    fpc = data$population_size
  )

  expect_identical(result$site_id, c("A", "B"))
  expect_equal(result$population_size, c(4, 10))
  expect_equal(result$sampling_fraction, c(1, 0.5))
  expect_equal(result$fpc_variance_multiplier, c(0, 5 / 9), tolerance = 1e-15)
  expect_equal(result$se[[1L]], 0)

  inconsistent <- data$population_size
  inconsistent[[4L]] <- 5L
  expect_error(
    sm_estimate(
      data,
      family = "binomial",
      indicator = "x",
      id_cols = c("school", "school_year"),
      vst = "none",
      fpc = inconsistent
    ),
    class = "sitemix_error_invalid_fpc"
  )
  expect_error(
    sm_estimate(
      data,
      family = "binomial",
      indicator = "x",
      id_cols = c("school", "school_year"),
      vst = "none",
      fpc = c(4, 10)
    ),
    class = "sitemix_error_invalid_fpc"
  )
})

test_that("D0 uses the same FPC contract and rejects synthetic suppression rows", {
  aggregate <- data.frame(
    site_id = c("A", "B"),
    year = 2024L,
    indicator = "x",
    c_jt = c(3L, 5L),
    n_jt = c(8L, 10L),
    population_size = c(20L, 10L)
  )
  result <- sm_estimate_from_aggregates(
    aggregate,
    family = "binomial",
    indicator = "x",
    vst = "arcsine",
    fpc = aggregate$population_size
  )
  expect_equal(result$population_size, c(20, 10))
  expect_equal(result$se^2, c(12 / 19 / 32, 0), tolerance = 1e-14)
  expect_identical(attr(result, "aggregate_case", exact = TRUE), "D0")

  suppressed <- aggregate[1L, ]
  suppressed$c_jt <- NA_integer_
  suppressed$suppression_flag <- "S"
  expect_error(
    sm_estimate_from_aggregates(
      suppressed,
      family = "binomial",
      indicator = "x",
      fpc = 20,
      suppression_col = "suppression_flag",
      suppression_flag_value = "S"
    ),
    class = "sitemix_error_invalid_fpc"
  )

  boundary <- aggregate[1L, ]
  boundary$c_jt <- 0L
  expect_error(
    sm_estimate_from_aggregates(
      boundary,
      family = "binomial",
      indicator = "x",
      anscombe = TRUE,
      boundary_method = "agresti_coull",
      fpc = 20
    ),
    class = "sitemix_error_anscombe_incompatible_correction"
  )
  expect_error(
    sm_estimate_from_aggregates(
      boundary,
      family = "binomial",
      indicator = "x",
      boundary_method = "agresti_coull",
      vjt = TRUE,
      fpc = 20
    ),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})

test_that("omitting fpc preserves the infinite-population scalar contract", {
  data <- fpc_scalar_counts()
  result <- sm_estimate_from_counts(
    data, family = "binomial", indicator = "x", vst = "arcsine"
  )
  expect_equal(result$se_raw^2, 0.375 * 0.625 / 8, tolerance = 1e-15)
  expect_equal(result$se^2, 1 / 32, tolerance = 1e-15)
  expect_false(any(c(
    "population_size",
    "sampling_fraction",
    "fpc_variance_multiplier",
    "fpc_se_multiplier",
    "variance_multiplier_applied",
    "se_multiplier_applied",
    "sampling_design",
    "variance_rule"
  ) %in% names(result)))
})
