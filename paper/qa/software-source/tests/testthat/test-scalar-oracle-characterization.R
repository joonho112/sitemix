# Independent scalar formula oracles for the Phase 3 characterization gate.
#
# These helpers deliberately use only base/statistical primitives. They do not
# call sitemix functions, including unexported helpers, so that they can detect
# an implementation that merely reproduces its own formula mistake.

.oracle_wilson_score_se <- function(C, n, z = stats::qnorm(0.975)) {
  p <- C / n
  sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
}

.oracle_agresti_coull <- function(C, n, z = stats::qnorm(0.975)) {
  n_tilde <- n + z^2
  p_tilde <- (C + z^2 / 2) / n_tilde
  list(
    p_tilde = p_tilde,
    n_tilde = n_tilde,
    se = sqrt(p_tilde * (1 - p_tilde) / n_tilde)
  )
}

.oracle_anscombe <- function(C, n) {
  p_adjusted <- (C + 3 / 8) / (n + 3 / 4)
  list(
    p_adjusted = p_adjusted,
    theta = asin(sqrt(p_adjusted)),
    se = 1 / (2 * sqrt(n + 1 / 2))
  )
}

.oracle_binomial_se <- function(C, n) {
  p <- C / n
  sqrt(p * (1 - p) / n)
}

.oracle_binomial_bc_se <- function(C, n) {
  p <- C / n
  sqrt(p * (1 - p) / (n - 1))
}

.oracle_logit_delta <- function(C, n, se_raw = .oracle_binomial_se(C, n)) {
  p <- C / n
  list(
    theta = log(p / (1 - p)),
    se = se_raw / (p * (1 - p))
  )
}

.scalar_oracle_counts <- function(C, n) {
  data.frame(
    site_id = sprintf("S%02d_C%d_n%d", seq_along(C), C, n),
    year = rep(2024L, length(C)),
    n_jt = as.integer(n),
    c_jt_absent = as.integer(C)
  )
}

test_that("scalar oracle helpers are independent of package internals", {
  helpers <- list(
    .oracle_wilson_score_se,
    .oracle_agresti_coull,
    .oracle_anscombe,
    .oracle_binomial_se,
    .oracle_binomial_bc_se,
    .oracle_logit_delta
  )
  helper_source <- vapply(
    helpers,
    function(helper) paste(deparse(body(helper)), collapse = "\n"),
    character(1)
  )

  expect_false(any(grepl("sitemix:::|[.]sm_", helper_source)))
})

test_that("hand constants pin independent scalar formulas", {
  # C = 0, n = 10: Wilson score-radius/z boundary surrogate.
  expect_equal(
    .oracle_wilson_score_se(0, 10),
    0.0708004846140114,
    tolerance = 1e-14
  )

  # C = 0, n = 10: z-general adjusted-Wald Agresti-Coull construction.
  ac <- .oracle_agresti_coull(0, 10)
  expect_equal(ac$p_tilde, 0.1387663999314445, tolerance = 1e-14)
  expect_equal(ac$se, 0.0929205369362129, tolerance = 1e-14)

  # C = 2, n = 10: ordinary, n-1 corrected, and logit delta values.
  expect_equal(.oracle_binomial_se(2, 10), 0.1264911064067352, tolerance = 1e-14)
  expect_equal(.oracle_binomial_bc_se(2, 10), 0.1333333333333334, tolerance = 1e-14)
  logit <- .oracle_logit_delta(2, 10)
  expect_equal(logit$theta, -1.3862943611198906, tolerance = 1e-14)
  expect_equal(logit$se, 0.7905694150420948, tolerance = 1e-14)

  # C = 0, n = 1: small-n Anscombe boundary point and approximation.
  anscombe <- .oracle_anscombe(0, 1)
  expect_equal(anscombe$theta, 0.4812753739423435, tolerance = 1e-14)
  expect_equal(anscombe$se, 0.4082482904638631, tolerance = 1e-14)
})

test_that("oracles cover zero, full, interior, and small-n cells", {
  C <- c(0, 10, 2, 0, 1)
  n <- c(10, 10, 10, 1, 1)
  wilson <- .oracle_wilson_score_se(C, n)
  anscombe <- .oracle_anscombe(C, n)

  expect_equal(wilson[[1]], wilson[[2]], tolerance = 1e-14)
  expect_equal(wilson[[4]], wilson[[5]], tolerance = 1e-14)
  expect_true(all(is.finite(wilson) & wilson > 0))
  expect_true(all(is.finite(anscombe$theta)))
  expect_true(all(is.finite(anscombe$se) & anscombe$se > 0))
  expect_equal(anscombe$theta[[1]] + anscombe$theta[[2]], pi / 2, tolerance = 1e-14)
  expect_equal(anscombe$theta[[4]] + anscombe$theta[[5]], pi / 2, tolerance = 1e-14)
  expect_equal(.oracle_binomial_se(c(0, 10), 10), c(0, 0))
})

test_that("current Wilson boundary output agrees with the independent surrogate", {
  C <- c(0, 10, 0, 1)
  n <- c(10, 10, 1, 1)
  out <- sitemix::sm_estimate_from_counts(
    .scalar_oracle_counts(C, n),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    boundary_method = "wilson_floor",
    min_n = 1L
  )

  expect_equal(out$theta_raw, C / n, tolerance = 1e-14)
  expect_equal(out$se_raw, .oracle_wilson_score_se(C, n), tolerance = 1e-12)
  expect_equal(out$var_method, rep("wilson_boundary_surrogate", length(C)))
})

test_that("Agresti-Coull boundary output matches the approved oracle", {
  C <- c(0, 10, 0, 1)
  n <- c(10, 10, 1, 1)
  out <- sitemix::sm_estimate_from_counts(
    .scalar_oracle_counts(C, n),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    boundary_method = "agresti_coull",
    min_n = 1L
  )
  standard <- .oracle_agresti_coull(C, n)

  expect_equal(out$theta_raw, C / n, tolerance = 1e-14)
  expect_equal(out$theta_hat, C / n, tolerance = 1e-14)
  expect_equal(out$se_raw, standard$se, tolerance = 1e-12)
  expect_equal(out$se, standard$se, tolerance = 1e-12)
  expect_equal(
    out$var_method,
    rep("agresti_coull_boundary_surrogate", length(C))
  )
  expect_equal(out$se_raw[[1]], 0.0929205369362129, tolerance = 1e-14)
})

test_that("approved boundary surrogate takes precedence over n-1 correction", {
  C <- c(0, 10, 2)
  n <- c(10, 10, 10)
  out <- sitemix::sm_estimate_from_counts(
    .scalar_oracle_counts(C, n),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    boundary_method = "agresti_coull",
    bias_correction = "binomial_bc",
    min_n = 1L
  )
  standard_boundary <- .oracle_agresti_coull(C[1:2], n[1:2])

  expect_equal(
    out$var_method,
    c(
      "agresti_coull_boundary_surrogate",
      "agresti_coull_boundary_surrogate",
      "binomial_bc"
    )
  )
  expect_equal(out$theta_raw, C / n, tolerance = 1e-14)
  expect_equal(out$se_raw[1:2], standard_boundary$se, tolerance = 1e-12)
  expect_equal(out$se_raw[[3]], .oracle_binomial_bc_se(2, 10), tolerance = 1e-12)
})

test_that("interior plug-in, n-1 correction, and logit match independent oracles", {
  C <- c(2, 7)
  n <- c(10, 10)
  counts <- .scalar_oracle_counts(C, n)

  raw <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    vst = "none",
    boundary_method = "none",
    min_n = 1L
  )
  corrected <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    vst = "none",
    boundary_method = "none",
    bias_correction = "binomial_bc",
    min_n = 1L
  )
  logit <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    vst = "logit",
    boundary_method = "none",
    min_n = 1L
  )
  corrected_arcsine <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    vst = "arcsine",
    boundary_method = "none",
    bias_correction = "binomial_bc",
    min_n = 1L
  )
  corrected_logit <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    vst = "logit",
    boundary_method = "none",
    bias_correction = "binomial_bc",
    min_n = 1L
  )
  logit_oracle <- .oracle_logit_delta(C, n)
  corrected_logit_oracle <- .oracle_logit_delta(
    C,
    n,
    se_raw = .oracle_binomial_bc_se(C, n)
  )

  expect_equal(raw$theta_raw, C / n, tolerance = 1e-14)
  expect_equal(raw$se_raw, .oracle_binomial_se(C, n), tolerance = 1e-12)
  expect_equal(corrected$se_raw, .oracle_binomial_bc_se(C, n), tolerance = 1e-12)
  expect_equal(logit$theta_hat, logit_oracle$theta, tolerance = 1e-12)
  expect_equal(logit$se, logit_oracle$se, tolerance = 1e-12)
  expect_equal(corrected_arcsine$se, 1 / (2 * sqrt(n - 1)), tolerance = 1e-12)
  expect_equal(
    corrected_arcsine$var_method,
    rep("arcsine_delta_binomial_bc", length(C))
  )
  expect_equal(corrected_logit$se, corrected_logit_oracle$se, tolerance = 1e-12)
  expect_equal(
    corrected_logit$var_method,
    rep("logit_delta_binomial_bc", length(C))
  )
})

test_that("current logit boundary cells fail for every boundary method", {
  counts <- .scalar_oracle_counts(c(0, 10), c(10, 10))

  for (method in c("wilson_floor", "agresti_coull", "none")) {
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
})

test_that("Anscombe oracle matches legal cells and rejects illegal combinations", {
  C <- c(0, 10, 2, 0, 1)
  n <- c(10, 10, 10, 1, 1)
  counts <- .scalar_oracle_counts(C, n)
  oracle <- .oracle_anscombe(C, n)
  legal <- sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "absent",
    vst = "arcsine",
    anscombe = TRUE,
    boundary_method = "none",
    min_n = 1L
  )

  expect_equal(legal$theta_raw, C / n, tolerance = 1e-14)
  expect_equal(legal$theta_hat, oracle$theta, tolerance = 1e-12)
  expect_equal(legal$se, oracle$se, tolerance = 1e-12)

  expect_error(
    sitemix::sm_estimate_from_counts(
      counts[3, ],
      family = "binomial",
      indicator = "absent",
      vst = "logit",
      anscombe = TRUE,
      min_n = 1L
    ),
    class = "sitemix_error_anscombe_requires_arcsine"
  )

  expect_error(
    sitemix::sm_estimate_from_counts(
      counts[1, ],
      family = "binomial",
      indicator = "absent",
      vst = "arcsine",
      anscombe = TRUE,
      boundary_method = "agresti_coull",
      min_n = 1L
    ),
    regexp = "incompatible with Agresti-Coull",
    class = "sitemix_error_anscombe_incompatible_correction"
  )

  expect_error(
    sitemix::sm_estimate_from_counts(
      counts[3, ],
      family = "binomial",
      indicator = "absent",
      vst = "arcsine",
      anscombe = TRUE,
      bias_correction = "binomial_bc",
      boundary_method = "none",
      min_n = 1L
    ),
    regexp = "incompatible with `binomial_bc`",
    class = "sitemix_error_anscombe_incompatible_correction"
  )
})
