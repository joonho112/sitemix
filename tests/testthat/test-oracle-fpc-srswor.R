# Independent finite-population oracle. These helpers intentionally use only
# base R arithmetic and do not call sitemix production helpers.

.oracle_fpc_q <- function(n, population_size) {
  if (!is.numeric(n) || !is.numeric(population_size)) {
    stop("`n` and `population_size` must be numeric.", call. = FALSE)
  }
  size <- max(length(n), length(population_size))
  if (!length(n) %in% c(1L, size) ||
      !length(population_size) %in% c(1L, size)) {
    stop("Inputs must be scalar or have a common length.", call. = FALSE)
  }
  n <- rep_len(n, size)
  population_size <- rep_len(population_size, size)
  whole <- function(x) is.finite(x) & x == floor(x)
  if (any(!whole(n)) || any(n < 1) ||
      any(!whole(population_size)) || any(population_size < 1)) {
    stop("Sample and population sizes must be positive whole numbers.", call. = FALSE)
  }
  if (any(population_size < n)) {
    stop("SRSWOR requires `population_size >= n`.", call. = FALSE)
  }

  # The algebraic ratio is 0/0 for the one-unit census, but its design
  # variance is exactly zero. Handle every census before evaluating it.
  census <- population_size == n
  q <- numeric(size)
  q[!census] <- (population_size[!census] - n[!census]) /
    (population_size[!census] - 1)
  q
}

.oracle_fpc_scalar <- function(
  C,
  n,
  population_size,
  scale = c("raw", "arcsine", "logit", "anscombe"),
  correction = c("plugin", "design_corrected")
) {
  scale <- match.arg(scale)
  correction <- match.arg(correction)
  size <- max(length(C), length(n), length(population_size))
  if (any(!c(length(C), length(n), length(population_size)) %in% c(1L, size))) {
    stop("Inputs must be scalar or have a common length.", call. = FALSE)
  }
  C <- rep_len(C, size)
  n <- rep_len(n, size)
  population_size <- rep_len(population_size, size)
  q <- .oracle_fpc_q(n, population_size)

  if (!is.numeric(C) || any(!is.finite(C)) || any(C != floor(C)) ||
      any(C < 0) || any(C > n)) {
    stop("`C` must be a whole-number count in [0, n].", call. = FALSE)
  }
  census <- population_size == n
  p <- C / n
  interior <- p > 0 & p < 1

  if (identical(correction, "design_corrected") && any(!census & n <= 1)) {
    stop("The design-corrected scalar oracle requires n > 1 outside a census.", call. = FALSE)
  }
  if (identical(correction, "design_corrected") &&
      identical(scale, "arcsine") && any(!census & !interior)) {
    stop("The design-corrected arcsine delta oracle requires interior p outside a census.", call. = FALSE)
  }
  if (identical(scale, "logit") && any(!interior)) {
    stop("The logit oracle requires interior proportions.", call. = FALSE)
  }
  if (identical(scale, "anscombe") &&
      identical(correction, "design_corrected")) {
    stop("Anscombe plus design correction is outside the approved contract.", call. = FALSE)
  }

  raw_variance <- if (identical(correction, "plugin")) {
    q * p * (1 - p) / n
  } else {
    value <- numeric(size)
    value[!census] <- (population_size[!census] - n[!census]) *
      p[!census] * (1 - p[!census]) /
      (population_size[!census] * (n[!census] - 1))
    value
  }

  if (identical(scale, "raw")) {
    estimate <- p
    variance <- raw_variance
  } else if (identical(scale, "arcsine")) {
    estimate <- asin(sqrt(p))
    variance <- if (identical(correction, "plugin")) {
      q / (4 * n)
    } else {
      value <- numeric(size)
      value[!census] <- (population_size[!census] - n[!census]) /
        (4 * population_size[!census] * (n[!census] - 1))
      value
    }
  } else if (identical(scale, "logit")) {
    estimate <- log(p / (1 - p))
    variance <- raw_variance / (p^2 * (1 - p)^2)
  } else {
    estimate <- asin(sqrt((C + 3 / 8) / (n + 3 / 4)))
    variance <- q / (4 * (n + 1 / 2))
  }

  data.frame(
    C = C,
    n = n,
    population_size = population_size,
    p = p,
    fpc_variance_multiplier = q,
    estimate = estimate,
    variance = variance,
    se = sqrt(variance)
  )
}

test_that("SRSWOR enumeration identifies the fixed-population estimand", {
  population <- c(1, 1, 1, 0, 0, 0, 0, 0)
  sample_indices <- combn(length(population), 4L)
  sample_means <- apply(sample_indices, 2L, function(index) {
    mean(population[index])
  })
  population_proportion <- mean(population)
  exact_design_variance <- mean((sample_means - population_proportion)^2)
  formula_variance <- (8 - 4) / (8 - 1) *
    population_proportion * (1 - population_proportion) / 4

  expect_equal(mean(sample_means), population_proportion, tolerance = 1e-15)
  expect_equal(exact_design_variance, 0.0334821428571429, tolerance = 1e-14)
  expect_equal(exact_design_variance, formula_variance, tolerance = 1e-15)

  observed_counts <- colSums(matrix(population[sample_indices], nrow = 4L))
  design_variance_estimates <- .oracle_fpc_scalar(
    C = observed_counts,
    n = 4,
    population_size = 8,
    scale = "raw",
    correction = "design_corrected"
  )$variance
  expect_equal(mean(design_variance_estimates), exact_design_variance, tolerance = 1e-15)
})

test_that("fixed numeric fixture distinguishes plug-in and design correction", {
  raw_plugin <- .oracle_fpc_scalar(3, 8, 20, "raw", "plugin")
  raw_corrected <- .oracle_fpc_scalar(3, 8, 20, "raw", "design_corrected")
  arcsine_plugin <- .oracle_fpc_scalar(3, 8, 20, "arcsine", "plugin")
  arcsine_corrected <- .oracle_fpc_scalar(3, 8, 20, "arcsine", "design_corrected")
  logit_plugin <- .oracle_fpc_scalar(3, 8, 20, "logit", "plugin")
  logit_corrected <- .oracle_fpc_scalar(3, 8, 20, "logit", "design_corrected")
  anscombe <- .oracle_fpc_scalar(3, 8, 20, "anscombe", "plugin")

  expect_equal(raw_plugin$fpc_variance_multiplier, 0.6315789473684210, tolerance = 1e-15)
  expect_equal(raw_plugin$estimate, 0.375, tolerance = 1e-15)
  expect_equal(raw_plugin$variance, 0.0185032894736842, tolerance = 1e-15)
  expect_equal(raw_plugin$se, 0.1360267968956272, tolerance = 1e-15)
  expect_equal(raw_corrected$variance, 0.0200892857142857, tolerance = 1e-15)
  expect_equal(raw_corrected$se, 0.1417366773784602, tolerance = 1e-15)

  expect_equal(arcsine_plugin$estimate, 0.6590580358264089, tolerance = 1e-15)
  expect_equal(arcsine_plugin$variance, 0.0197368421052632, tolerance = 1e-14)
  expect_equal(arcsine_corrected$variance, 0.0214285714285714, tolerance = 1e-14)
  expect_equal(logit_plugin$estimate, -0.5108256237659907, tolerance = 1e-15)
  expect_equal(logit_plugin$variance, 0.3368421052631579, tolerance = 1e-15)
  expect_equal(logit_corrected$variance, 0.3657142857142857, tolerance = 1e-15)
  expect_equal(anscombe$estimate, 0.6700931577543916, tolerance = 1e-15)
  expect_equal(anscombe$variance, 0.0185758513931889, tolerance = 1e-14)
})

test_that("FPC approaches one and supports scalar or aligned population sizes", {
  population_sizes <- c(1e3, 1e6, 1e12)
  q <- .oracle_fpc_q(n = 8, population_size = population_sizes)
  expect_true(all(diff(q) > 0))
  expect_equal(q[[3]], 1, tolerance = 1e-10)

  plugin <- .oracle_fpc_scalar(3, 8, population_sizes, "raw", "plugin")
  corrected <- .oracle_fpc_scalar(3, 8, population_sizes, "raw", "design_corrected")
  expect_equal(plugin$variance[[3]], 0.375 * 0.625 / 8, tolerance = 1e-10)
  expect_equal(corrected$variance[[3]], 0.375 * 0.625 / 7, tolerance = 1e-10)

  recycled <- .oracle_fpc_scalar(C = c(2, 3), n = c(5, 8), population_size = 20)
  aligned <- .oracle_fpc_scalar(C = c(2, 3), n = c(5, 8), population_size = c(20, 40))
  expect_equal(recycled$population_size, c(20, 20))
  expect_equal(aligned$population_size, c(20, 40))
  expect_false(identical(recycled$variance, aligned$variance))
})

test_that("census is valid and has zero sampling uncertainty", {
  raw <- .oracle_fpc_scalar(C = c(0, 1, 3), n = c(1, 1, 8), population_size = c(1, 1, 8))
  arcsine <- .oracle_fpc_scalar(C = c(0, 1, 3), n = c(1, 1, 8), population_size = c(1, 1, 8), scale = "arcsine")
  anscombe <- .oracle_fpc_scalar(C = c(0, 1, 3), n = c(1, 1, 8), population_size = c(1, 1, 8), scale = "anscombe")
  corrected <- .oracle_fpc_scalar(3, 8, 8, correction = "design_corrected")

  expect_equal(raw$fpc_variance_multiplier, c(0, 0, 0))
  expect_equal(raw$variance, c(0, 0, 0))
  expect_equal(arcsine$variance, c(0, 0, 0))
  expect_equal(anscombe$variance, c(0, 0, 0))
  expect_equal(corrected$variance, 0)
})

test_that("transform propagation follows delta formulas where approved", {
  for (correction in c("plugin", "design_corrected")) {
    raw <- .oracle_fpc_scalar(3, 8, 20, "raw", correction)
    arcsine <- .oracle_fpc_scalar(3, 8, 20, "arcsine", correction)
    logit <- .oracle_fpc_scalar(3, 8, 20, "logit", correction)
    p <- raw$p
    arcsine_derivative <- 1 / (2 * sqrt(p * (1 - p)))
    logit_derivative <- 1 / (p * (1 - p))

    expect_equal(arcsine$variance, raw$variance * arcsine_derivative^2, tolerance = 1e-15)
    expect_equal(logit$variance, raw$variance * logit_derivative^2, tolerance = 1e-15)
  }
})

test_that("invalid finite-population cells fail before arithmetic", {
  expect_error(
    .oracle_fpc_scalar(3, 8, 7),
    "population_size >= n",
    fixed = TRUE
  )
  expect_error(
    .oracle_fpc_scalar(3, 8, 20.5),
    "positive whole numbers",
    fixed = TRUE
  )
  expect_error(
    .oracle_fpc_scalar(0, 8, 20, scale = "logit"),
    "interior proportions",
    fixed = TRUE
  )
  expect_error(
    .oracle_fpc_scalar(3, 8, 20, scale = "anscombe", correction = "design_corrected"),
    "outside the approved contract",
    fixed = TRUE
  )
})
