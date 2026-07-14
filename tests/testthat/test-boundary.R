boundary_count_data <- function() {
  data.frame(
    site_id = c("A_zero", "B_full", "C_one_zero", "D_one_full", "E_interior"),
    year = rep(2024L, 5),
    n_jt = c(4L, 4L, 1L, 1L, 4L),
    c_jt_absent = c(0L, 4L, 0L, 1L, 2L)
  )
}

test_that("public binomial arcsine output handles zero, full, and n=1 cells", {
  out <- sitemix::sm_estimate_from_counts(
    boundary_count_data(),
    family = "binomial",
    indicator = "absent",
    min_n = 2L
  )

  expect_equal(out$theta_raw, c(0, 1, 0, 1, 0.5))
  expect_equal(out$theta_hat, asin(sqrt(out$theta_raw)), tolerance = 1e-12)
  expect_equal(out$se, 1 / (2 * sqrt(out$n)), tolerance = 1e-12)
  expect_equal(out$estimate_scale, rep("arcsine", 5))
  expect_equal(out$var_method, rep("arcsine_vst", 5))
  expect_equal(out$flag_zero_cell, c(TRUE, TRUE, TRUE, TRUE, FALSE))
  expect_equal(out$flag_small_n, c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(out$flag_below_accountability, rep(TRUE, 5))
  expect_equal(out$input_mode, rep("counts_full_suff", 5))
  expect_true(all(out$se_raw[out$flag_zero_cell] > 0))
  expect_true(validate.sitemix_estimates(out))
})

test_that("boundary, small-n, and accountability flags are independent", {
  counts <- data.frame(
    site_id = c("F1", "I5", "Z1", "Z50"),
    year = rep(2024L, 4),
    n_jt = c(1L, 5L, 1L, 50L),
    c_jt_absent = c(1L, 3L, 0L, 0L)
  )
  out <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    vst = "none",
    min_n = 10L
  )

  z <- stats::qnorm(0.975)
  expect_equal(out$theta_raw, c(1, 0.6, 0, 0))
  expect_equal(
    out$se_raw,
    c(
      sitemix:::.sm_wilson_se(1, 1, z = z),
      sqrt(0.6 * 0.4 / 5),
      sitemix:::.sm_wilson_se(0, 1, z = z),
      sitemix:::.sm_wilson_se(0, 50, z = z)
    ),
    tolerance = 1e-12
  )
  expect_equal(
    out$var_method,
    c(
      "wilson_boundary_surrogate",
      "binomial",
      "wilson_boundary_surrogate",
      "wilson_boundary_surrogate"
    )
  )
  expect_equal(out$flag_zero_cell, c(TRUE, FALSE, TRUE, TRUE))
  expect_equal(out$flag_small_n, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(out$flag_below_accountability, c(TRUE, TRUE, TRUE, FALSE))
  expect_true(validate.sitemix_estimates(out))
})

test_that("raw-scale boundary methods preserve observed theta_raw provenance", {
  counts <- boundary_count_data()

  wilson <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    vst = "none",
    boundary_method = "wilson_floor",
    min_n = 1L
  )
  expect_equal(wilson$theta_raw, c(0, 1, 0, 1, 0.5))
  expect_equal(wilson$theta_hat, wilson$theta_raw)
  expect_equal(wilson$se, wilson$se_raw)
  expect_equal(
    wilson$var_method,
    c(
      rep("wilson_boundary_surrogate", 4),
      "binomial"
    )
  )
  expect_true(all(wilson$se[wilson$flag_zero_cell] > 0))

  agresti <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    vst = "none",
    boundary_method = "agresti_coull",
    min_n = 1L
  )
  expect_equal(agresti$theta_raw, c(0, 1, 0, 1, 0.5))
  expect_equal(agresti$theta_hat, agresti$theta_raw)
  expect_equal(
    agresti$var_method,
    c(rep("agresti_coull_boundary_surrogate", 4), "binomial")
  )
  expect_equal(
    agresti$se[seq_len(4)],
    sitemix:::.sm_agresti_coull_se(counts$c_jt_absent[seq_len(4)], counts$n_jt[seq_len(4)]),
    tolerance = 1e-12
  )

  none <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    vst = "none",
    boundary_method = "none",
    min_n = 1L
  )
  expect_equal(none$var_method, rep("binomial", 5))
  expect_equal(none$se[none$flag_zero_cell], rep(0, 4))
  expect_equal(none$se[[5]], sqrt(0.5 * 0.5 / 4), tolerance = 1e-12)
  expect_true(validate.sitemix_estimates(none))
})

test_that("VST and boundary cross-products are explicit for Scenario A", {
  counts <- boundary_count_data()

  for (method in c("wilson_floor", "agresti_coull", "none")) {
    arcsine <- sitemix::sm_estimate_from_counts(
      counts,
      family = "binomial",
      indicator = "absent",
      vst = "arcsine",
      boundary_method = method,
      min_n = 1L
    )
    expected_theta_raw <- c(0, 1, 0, 1, 0.5)
    expect_equal(arcsine$theta_raw, expected_theta_raw, info = method)
    expect_equal(arcsine$var_method, rep("arcsine_vst", 5), info = method)
    expect_equal(arcsine$se, 1 / (2 * sqrt(arcsine$n)), tolerance = 1e-12, info = method)

    expect_error(
      sitemix::sm_estimate_from_counts(
        counts,
        family = "binomial",
        indicator = "absent",
        vst = "logit",
        boundary_method = method,
        min_n = 1L
      ),
      class = "sitemix_error_estimate_var_method",
      info = method
    )
  }

  interior <- subset(counts, site_id == "E_interior")
  logit <- sitemix::sm_estimate_from_counts(
    interior,
    family = "binomial",
    indicator = "absent",
    vst = "logit",
    boundary_method = "none",
    min_n = 1L
  )
  expect_equal(logit$theta_hat, 0, tolerance = 1e-12)
  expect_equal(logit$se, 1, tolerance = 1e-12)
  expect_equal(logit$var_method, "logit_delta")
})

test_that("FPC adjusts the raw companion SE and 1x1 raw-scale V", {
  counts <- subset(boundary_count_data(), site_id == "E_interior")
  no_fpc <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    vst = "none",
    vjt = TRUE,
    min_n = 1L
  )
  with_fpc <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    vst = "none",
    fpc = 10,
    vjt = TRUE,
    min_n = 1L
  )

  multiplier <- sqrt((10 - with_fpc$n) / (10 - 1))
  expect_equal(with_fpc$se_raw, no_fpc$se_raw * multiplier, tolerance = 1e-12)
  expect_equal(with_fpc$se, with_fpc$se_raw)
  expect_equal(as.matrix(with_fpc$V[[1]]), matrix(with_fpc$se[[1]]^2, 1, 1, dimnames = list("absent", "absent")))
  expect_equal(with_fpc$V[[1]]$vcov_scale, "raw")
})

test_that("public boundary arguments keep exact condition classes", {
  expect_error(
    sitemix::sm_estimate_from_counts(boundary_count_data(), family = "binomial", indicator = "absent", boundary_method = "wald"),
    class = "sitemix_error_invalid_boundary"
  )
  expect_error(
    sitemix::sm_estimate_from_counts(boundary_count_data(), family = "binomial", indicator = "absent", fpc = 3),
    class = "sitemix_error_invalid_fpc"
  )
  expect_error(
    sitemix::sm_estimate_from_counts(boundary_count_data(), family = "binomial", indicator = "absent", fpc = c(10, 20)),
    class = "sitemix_error_invalid_fpc"
  )
  expect_error(
    sitemix::sm_estimate_from_counts(boundary_count_data(), family = "binomial", indicator = "absent", fpc = Inf),
    class = "sitemix_error_invalid_fpc"
  )
  expect_error(
    sitemix::sm_estimate_from_counts(boundary_count_data(), family = "binomial", indicator = "absent", min_n = 0L),
    class = "sitemix_error_invalid_min_n"
  )
  expect_error(
    sitemix::sm_estimate_from_counts(boundary_count_data(), family = "binomial", indicator = "absent", anscombe = TRUE, vst = "logit"),
    class = "sitemix_error_anscombe_requires_arcsine"
  )
})
