vcov_test_error <- function(expr) {
  tryCatch(force(expr), error = identity)
}

vcov_test_matrix <- function(labels = c("x", "y"), diag_values = seq_along(labels) / 10) {
  mat <- diag(diag_values, length(labels))
  dimnames(mat) <- list(labels, labels)
  mat
}

vcov_test_sur <- function(...) {
  labels <- c("x", "y")
  args <- list(
    matrix = vcov_test_matrix(labels),
    site_id = "S000",
    year = 2024L,
    indicator_order = labels,
    family = "multivariate",
    vcov_method = "sur",
    estimate_scale = "none",
    vcov_scale = "raw",
    scalar_correction_rule = rep("none", length(labels)),
    n_jt = 20L,
    n_eff = 20
  )
  do.call(sm_vcov, utils::modifyList(args, list(...)))
}

test_that("sm_vcov constructs and formats a valid 1x1 binomial covariance", {
  V <- sm_vcov(
    matrix = matrix(0.0025, 1, 1, dimnames = list("chronic_absent", "chronic_absent")),
    site_id = "S001",
    year = 2024L,
    indicator_order = "chronic_absent",
    family = "binomial",
    vcov_method = NA_character_,
    estimate_scale = "arcsine",
    vcov_scale = "arcsine_delta",
    scalar_correction_rule = "none",
    n_jt = 100L,
    n_eff = 100
  )

  expect_s3_class(V, "sm_vcov")
  expect_true(validate.sm_vcov(V))
  expect_equal(as.matrix(V), matrix(0.0025, 1, 1, dimnames = list("chronic_absent", "chronic_absent")))
  expect_match(format(V), "<sm_vcov[1x1] binomial/NA arcsine_delta rank=1>", fixed = TRUE)
  expect_equal(V$site_id, "S001")
  expect_equal(V$year, 2024L)
  expect_true(validate.sm_vcov(sitemix:::.sm_new_sm_vcov(
    matrix = matrix(0.0025, 1, 1, dimnames = list("chronic_absent", "chronic_absent")),
    site_id = "S001",
    year = 2024L,
    indicator_order = "chronic_absent",
    family = "binomial",
    vcov_method = NA_character_,
    estimate_scale = "arcsine",
    vcov_scale = "arcsine_delta",
    scalar_correction_rule = "none",
    n_jt = 100L,
    n_eff = 100
  )))
})

test_that("sm_vcov constructs valid SUR covariance with metadata", {
  mat <- matrix(
    c(
      0.024, 0.006, 0.002,
      0.006, 0.024, 0.008,
      0.002, 0.008, 0.016
    ),
    3,
    3,
    byrow = TRUE,
    dimnames = list(c("frpm", "snap", "wic"), c("frpm", "snap", "wic"))
  )

  V <- sm_vcov(
    matrix = mat,
    site_id = "S002",
    year = 2025L,
    indicator_order = c("frpm", "snap", "wic"),
    family = "multivariate",
    vcov_method = "sur",
    estimate_scale = "arcsine",
    vcov_scale = "raw",
    matrix_boundary_rule = "none",
    scalar_correction_rule = c("none", "binomial_bc", "wilson_boundary_surrogate"),
    psd_repair = "none",
    positive_support = NA_integer_,
    n_jt = 10L,
    n_eff = 10
  )

  expect_true(validate.sm_vcov(V))
  expect_equal(V$matrix_rank, 3L)
  expect_equal(V$vcov_method, "sur")
  expect_equal(V$indicator_order, c("frpm", "snap", "wic"))
  expect_equal(
    V$scalar_correction_rule,
    c("none", "binomial_bc", "wilson_boundary_surrogate")
  )
})

test_that("sm_vcov accepts locked non-emitted metadata lexicon values", {
  V <- sm_vcov(
    matrix = vcov_test_matrix(),
    site_id = "S002b",
    year = 2025L,
    indicator_order = c("x", "y"),
    family = "multivariate",
    vcov_method = "working_independence",
    estimate_scale = "none",
    vcov_scale = "reference_raw",
    matrix_boundary_rule = "diagonal_boundary_floor",
    scalar_correction_rule = c("agresti_coull_boundary_surrogate", "none"),
    psd_repair = "eigen_clip_tol",
    positive_support = 2L,
    n_jt = 10L,
    n_eff = 10
  )

  expect_true(validate.sm_vcov(V))
  expect_equal(V$vcov_scale, "reference_raw")
  expect_equal(V$psd_repair, "eigen_clip_tol")
  expect_equal(
    V$scalar_correction_rule,
    c("agresti_coull_boundary_surrogate", "none")
  )
})

test_that("sm_vcov constructs valid full-simplex multinomial covariance", {
  p <- c(A = 0.6, B = 0.35, C = 0.05)
  mat <- (diag(p) - tcrossprod(p)) / 100
  dimnames(mat) <- list(names(p), names(p))

  V <- sm_vcov(
    matrix = mat,
    site_id = "S003",
    year = 2026L,
    indicator_order = names(p),
    family = "multinomial",
    vcov_method = "multinomial",
    estimate_scale = "arcsine",
    vcov_scale = "raw",
    matrix_boundary_rule = "simplex_preserve",
    scalar_correction_rule = rep("none", 3),
    positive_support = 3L,
    n_jt = 100L,
    n_eff = 100
  )

  expect_true(validate.sm_vcov(V))
  expect_equal(V$matrix_rank, 2L)
  expect_equal(as.vector(as.matrix(V) %*% rep(1, 3)), rep(0, 3), tolerance = 1e-12)
  expect_equal(V$positive_support, 3L)
})

test_that("sm_vcov fills missing matrix dimnames from indicator_order", {
  V <- sm_vcov(
    matrix = diag(c(0.1, 0.2)),
    site_id = "S004",
    year = 2024L,
    indicator_order = c("x", "y"),
    family = "multivariate",
    vcov_method = "sur",
    estimate_scale = "none",
    vcov_scale = "raw",
    scalar_correction_rule = c("none", "none"),
    n_jt = 20L,
    n_eff = 20
  )

  expect_equal(rownames(as.matrix(V)), c("x", "y"))
  expect_equal(colnames(as.matrix(V)), c("x", "y"))
})

test_that("sm_vcov rejects dimname mismatches", {
  mat <- diag(2)
  dimnames(mat) <- list(c("x", "y"), c("x", "z"))

  expect_error(
    sm_vcov(
      matrix = mat,
      site_id = "S005",
      year = 2024L,
      indicator_order = c("x", "y"),
      family = "multivariate",
      vcov_method = "sur",
      estimate_scale = "none",
      vcov_scale = "raw",
      scalar_correction_rule = c("none", "none"),
      n_jt = 20L,
      n_eff = 20
    ),
    class = "sitemix_error_vcov_dimnames"
  )

  err <- vcov_test_error(sm_vcov(
    matrix = mat,
    site_id = "S005",
    year = 2024L,
    indicator_order = c("x", "y"),
    family = "multivariate",
    vcov_method = "sur",
    estimate_scale = "none",
    vcov_scale = "raw",
    scalar_correction_rule = c("none", "none"),
    n_jt = 20L,
    n_eff = 20
  ))
  expect_s3_class(err, "sitemix_error_vcov_dimnames")
  expect_s3_class(err, "sitemix_error_vcov")
  expect_equal(err$expected, c("x", "y"))
  expect_equal(err$fix, "Use the same indicator order in row metadata and covariance matrices.")
})

test_that("sm_vcov rejects malformed matrix inputs with stable messages", {
  err <- vcov_test_error(sm_vcov(
    matrix = data.frame(x = 1),
    indicator_order = "x",
    family = "binomial",
    estimate_scale = "none",
    vcov_scale = "raw"
  ))
  expect_s3_class(err, "sitemix_error_vcov_invariant")
  expect_match(conditionMessage(err), "`matrix` must be a numeric matrix.", fixed = TRUE)

  mat <- matrix(letters[1:4], 2, 2, dimnames = list(c("x", "y"), c("x", "y")))
  err <- vcov_test_error(sm_vcov(
    matrix = mat,
    indicator_order = c("x", "y"),
    family = "multivariate",
    vcov_method = "sur",
    estimate_scale = "none",
    vcov_scale = "raw",
    scalar_correction_rule = c("none", "none")
  ))
  expect_s3_class(err, "sitemix_error_vcov_invariant")
  expect_match(conditionMessage(err), "`matrix` must be a numeric matrix.", fixed = TRUE)

  mat <- matrix(1:6, 2, 3)
  err <- vcov_test_error(sm_vcov(
    matrix = mat,
    indicator_order = c("x", "y"),
    family = "multivariate",
    vcov_method = "sur",
    estimate_scale = "none",
    vcov_scale = "raw",
    scalar_correction_rule = c("none", "none")
  ))
  expect_s3_class(err, "sitemix_error_vcov_invariant")
  expect_s3_class(err, "sitemix_error_vcov")
  expect_match(conditionMessage(err), "`matrix` must be square.", fixed = TRUE)
  expect_equal(err$expected, "K x K matrix")
  expect_equal(err$actual, "2 x 3")
  expect_equal(err$fix, "Check covariance helper output before constructing `sm_vcov`.")

  mat <- matrix(c(1, Inf, Inf, 1), 2, 2, dimnames = list(c("x", "y"), c("x", "y")))
  err <- vcov_test_error(sm_vcov(
    matrix = mat,
    indicator_order = c("x", "y"),
    family = "multivariate",
    vcov_method = "sur",
    estimate_scale = "none",
    vcov_scale = "raw",
    scalar_correction_rule = c("none", "none")
  ))
  expect_s3_class(err, "sitemix_error_vcov_invariant")
  expect_match(conditionMessage(err), "`matrix` must contain finite values.", fixed = TRUE)
})

test_that("sm_vcov rejects malformed indicator order and partial dimnames", {
  mat <- vcov_test_matrix()
  expect_error(
    sm_vcov(
      matrix = mat,
      indicator_order = c("x", ""),
      family = "multivariate",
      vcov_method = "sur",
      estimate_scale = "none",
      vcov_scale = "raw",
      scalar_correction_rule = c("none", "none")
    ),
    class = "sitemix_error_vcov_dimnames"
  )
  expect_error(
    sm_vcov(
      matrix = mat,
      indicator_order = c("x", "x"),
      family = "multivariate",
      vcov_method = "sur",
      estimate_scale = "none",
      vcov_scale = "raw",
      scalar_correction_rule = c("none", "none")
    ),
    class = "sitemix_error_vcov_dimnames"
  )
  expect_error(
    sm_vcov(
      matrix = mat,
      indicator_order = c("x", "y", "z"),
      family = "multivariate",
      vcov_method = "sur",
      estimate_scale = "none",
      vcov_scale = "raw",
      scalar_correction_rule = c("none", "none", "none")
    ),
    class = "sitemix_error_vcov_dimnames"
  )

  partial <- diag(2)
  dimnames(partial) <- list(c("x", "y"), NULL)
  expect_error(
    sm_vcov(
      matrix = partial,
      indicator_order = c("x", "y"),
      family = "multivariate",
      vcov_method = "sur",
      estimate_scale = "none",
      vcov_scale = "raw",
      scalar_correction_rule = c("none", "none")
    ),
    class = "sitemix_error_vcov_dimnames"
  )
})

test_that("sm_vcov rejects nonsymmetric matrices", {
  mat <- matrix(c(1, 0.4, 0.2, 1), 2, 2, dimnames = list(c("x", "y"), c("x", "y")))

  expect_error(
    sm_vcov(
      matrix = mat,
      site_id = "S006",
      year = 2024L,
      indicator_order = c("x", "y"),
      family = "multivariate",
      vcov_method = "sur",
      estimate_scale = "none",
      vcov_scale = "raw",
      scalar_correction_rule = c("none", "none"),
      n_jt = 20L,
      n_eff = 20
    ),
    class = "sitemix_error_vcov_invariant"
  )

  err <- vcov_test_error(sm_vcov(
    matrix = mat,
    site_id = "S006",
    year = 2024L,
    indicator_order = c("x", "y"),
    family = "multivariate",
    vcov_method = "sur",
    estimate_scale = "none",
    vcov_scale = "raw",
    scalar_correction_rule = c("none", "none"),
    matrix_rank = 2L,
    n_jt = 20L,
    n_eff = 20
  ))
  expect_s3_class(err, "sitemix_error_vcov_invariant")
  expect_match(conditionMessage(err), "`matrix` must be symmetric.", fixed = TRUE)
})

test_that("sm_vcov rejects materially non-PSD matrices", {
  mat <- matrix(c(1, 2, 2, 1), 2, 2, dimnames = list(c("x", "y"), c("x", "y")))

  expect_error(
    sm_vcov(
      matrix = mat,
      site_id = "S007",
      year = 2024L,
      indicator_order = c("x", "y"),
      family = "multivariate",
      vcov_method = "sur",
      estimate_scale = "none",
      vcov_scale = "raw",
      scalar_correction_rule = c("none", "none"),
      n_jt = 20L,
      n_eff = 20
    ),
    class = "sitemix_error_vcov_invariant"
  )

  err <- vcov_test_error(vcov_test_sur(vcov_method = "sandwich"))
  expect_s3_class(err, "sitemix_error_vcov_invariant")
  expect_s3_class(err, "sitemix_error_vcov")
  expect_match(conditionMessage(err), "`vcov_method` has an invalid value.", fixed = TRUE)
  expect_equal(err$expected, c("sur", "multinomial", "working_independence"))
  expect_equal(err$actual, "sandwich")
})

test_that("sm_vcov preserves the PSD tolerance boundary", {
  reference <- diag(c(1, 0), 2)
  tol <- sitemix:::.sm_psd_tolerance(reference)
  mat <- diag(c(1, -tol / 2), 2)
  dimnames(mat) <- list(c("x", "y"), c("x", "y"))

  V <- sm_vcov(
    matrix = mat,
    site_id = "S007b",
    year = 2024L,
    indicator_order = c("x", "y"),
    family = "multivariate",
    vcov_method = "sur",
    estimate_scale = "none",
    vcov_scale = "raw",
    scalar_correction_rule = c("none", "none"),
    n_jt = 20L,
    n_eff = 20
  )

  expect_true(validate.sm_vcov(V))
  expect_equal(V$matrix_rank, 1L)
})

test_that("validate.sm_vcov rejects rank mismatches and simplex violations", {
  expect_error(
    sm_vcov(
      matrix = vcov_test_matrix(),
      site_id = "S010",
      year = 2024L,
      indicator_order = c("x", "y"),
      family = "multivariate",
      vcov_method = "sur",
      estimate_scale = "none",
      vcov_scale = "raw",
      scalar_correction_rule = c("none", "none"),
      matrix_rank = 1L,
      n_jt = 20L,
      n_eff = 20
    ),
    class = "sitemix_error_vcov_invariant"
  )

  mat <- vcov_test_matrix(labels = c("A", "B", "C"), diag_values = c(0.1, 0.2, 0.3))
  err <- vcov_test_error(sm_vcov(
    matrix = mat,
    site_id = "S011",
    year = 2024L,
    indicator_order = c("A", "B", "C"),
    family = "multinomial",
    vcov_method = "multinomial",
    estimate_scale = "arcsine",
    vcov_scale = "raw",
    matrix_boundary_rule = "simplex_preserve",
    scalar_correction_rule = rep("none", 3),
    positive_support = 3L,
    n_jt = 100L,
    n_eff = 100
  ))
  expect_s3_class(err, "sitemix_error_vcov_invariant")
  expect_match(
    conditionMessage(err),
    "Multinomial covariance matrices must satisfy the simplex row-sum-zero identity.",
    fixed = TRUE
  )
})

test_that("sm_vcov rejects invalid metadata lexicons", {
  mat <- diag(2)
  dimnames(mat) <- list(c("x", "y"), c("x", "y"))

  expect_error(
    sm_vcov(
      matrix = mat,
      site_id = "S008",
      year = 2024L,
      indicator_order = c("x", "y"),
      family = "multivariate",
      vcov_method = "multinomial",
      estimate_scale = "none",
      vcov_scale = "raw",
      scalar_correction_rule = c("none", "none"),
      n_jt = 20L,
      n_eff = 20
    ),
    class = "sitemix_error_vcov_invariant"
  )

  expect_error(
    sm_vcov(
      matrix = mat,
      site_id = "S008",
      year = 2024L,
      indicator_order = c("x", "y"),
      family = "mvbernoulli",
      vcov_method = "sur",
      estimate_scale = "none",
      vcov_scale = "raw",
      scalar_correction_rule = c("none", "none"),
      n_jt = 20L,
      n_eff = 20
    ),
    class = "sitemix_error_vcov_invariant"
  )

  expect_error(
    sm_vcov(
      matrix = matrix(0.01, 1, 1, dimnames = list("x", "x")),
      indicator_order = "x",
      family = "binomial",
      vcov_method = "sur",
      estimate_scale = "none",
      vcov_scale = "raw",
      scalar_correction_rule = "none"
    ),
    class = "sitemix_error_vcov_invariant"
  )

  expect_error(
    sm_vcov(
      matrix = mat,
      site_id = "S008",
      year = 2024L,
      indicator_order = c("x", "y"),
      family = "multinomial",
      vcov_method = NA_character_,
      estimate_scale = "none",
      vcov_scale = "raw",
      scalar_correction_rule = c("none", "none"),
      n_jt = 20L,
      n_eff = 20
    ),
    class = "sitemix_error_vcov_invariant"
  )
})

test_that("sm_vcov rejects invalid metadata field values", {
  expect_error(vcov_test_sur(estimate_scale = "sqrt"), class = "sitemix_error_vcov_invariant")
  expect_error(vcov_test_sur(vcov_scale = "arcsine"), class = "sitemix_error_vcov_invariant")
  expect_error(vcov_test_sur(matrix_boundary_rule = "floor"), class = "sitemix_error_vcov_invariant")
  expect_error(vcov_test_sur(psd_repair = "higham"), class = "sitemix_error_vcov_invariant")
  expect_error(vcov_test_sur(scalar_correction_rule = c("none")), class = "sitemix_error_vcov_invariant")
  expect_error(vcov_test_sur(scalar_correction_rule = c("none", NA_character_)), class = "sitemix_error_vcov_invariant")
  expect_error(vcov_test_sur(scalar_correction_rule = c("none", "jeffreys")), class = "sitemix_error_vcov_invariant")
  expect_error(vcov_test_sur(n_jt = 0L), class = "sitemix_error_vcov_invariant")
  expect_error(vcov_test_sur(n_jt = c(20L, 21L)), class = "sitemix_error_vcov_invariant")
  expect_error(vcov_test_sur(n_eff = 0), class = "sitemix_error_vcov_invariant")
  expect_error(vcov_test_sur(n_eff = c(20, 21)), class = "sitemix_error_vcov_invariant")

  V <- vcov_test_sur()
  V$site_id <- c("S000", "S001")
  expect_error(validate.sm_vcov(V), class = "sitemix_error_vcov_invariant")

  V <- vcov_test_sur()
  V$year <- "2024"
  expect_error(validate.sm_vcov(V), class = "sitemix_error_vcov_invariant")

  V <- vcov_test_sur()
  V$matrix_rank <- c(2L, 2L)
  expect_error(validate.sm_vcov(V), class = "sitemix_error_vcov_invariant")

  V <- vcov_test_sur()
  V$positive_support <- c(1L, 2L)
  expect_error(validate.sm_vcov(V), class = "sitemix_error_vcov_invariant")
})

test_that("validate.sm_vcov rejects non-sm_vcov objects", {
  err <- vcov_test_error(validate.sm_vcov(list(matrix = diag(2))))
  expect_s3_class(err, "sitemix_error_vcov_invariant")
  expect_match(conditionMessage(err), "`x` must be an `sm_vcov` object.", fixed = TRUE)
})

test_that("sm_matrix_rank returns NA for malformed matrix inputs", {
  expect_identical(sitemix:::.sm_matrix_rank(data.frame(x = 1)), NA_integer_)
  expect_identical(sitemix:::.sm_matrix_rank(matrix(letters[1:4], 2, 2)), NA_integer_)
  expect_identical(sitemix:::.sm_matrix_rank(matrix(1:6, 2, 3)), NA_integer_)
  expect_identical(sitemix:::.sm_matrix_rank(matrix(c(1, Inf, Inf, 1), 2, 2)), NA_integer_)
  expect_identical(sitemix:::.sm_matrix_rank(matrix(c(1, 0.4, 0.2, 1), 2, 2)), NA_integer_)
})

test_that("print.sm_vcov includes high-level metadata", {
  V <- sm_vcov(
    matrix = matrix(0.01, 1, 1, dimnames = list("x", "x")),
    site_id = "S009",
    year = 2024L,
    indicator_order = "x",
    family = "binomial",
    vcov_method = NA_character_,
    estimate_scale = "none",
    vcov_scale = "raw",
    scalar_correction_rule = "none",
    n_jt = 10L,
    n_eff = 10
  )

  printed <- utils::capture.output(print(V))
  expect_match(printed[[1]], "sm_vcov[1x1] site_id=S009 year=2024 family=binomial", fixed = TRUE)
  expect_true(any(grepl("matrix_rank=1 psd_repair=none", printed, fixed = TRUE)))
  expect_true(any(grepl("estimate_scale=none matrix_boundary_rule=none indicators=x", printed, fixed = TRUE)))
})

test_that("public sm_vcov methods reject stale matrix and design metadata", {
  base <- vcov_test_sur()
  cases <- list(
    matrix_type = list(
      message = "`matrix` must be a numeric matrix.",
      mutate = function(x) {
        x$matrix <- matrix(
          letters[seq_along(x$matrix)],
          nrow = nrow(x$matrix),
          dimnames = dimnames(x$matrix)
        )
        x
      }
    ),
    nonneutral_unspecified = list(
      message = "Unspecified sampling designs must use neutral finite-population metadata.",
      mutate = function(x) {
        x$sampling_fraction <- 0.5
        x
      }
    ),
    invalid_srswor_fraction = list(
      message = "SRSWOR covariance metadata requires a whole population size and valid sampling fractions.",
      mutate = function(x) {
        x$sampling_design <- "SRSWOR"
        x$population_size <- 10
        x$sampling_fraction <- NA_real_
        x
      }
    ),
    fractional_sample_size = list(
      message = "SRSWOR sampling fractions must imply whole-number sample sizes.",
      mutate = function(x) {
        x$sampling_design <- "SRSWOR"
        x$population_size <- 10
        x$sampling_fraction <- 0.33
        x
      }
    ),
    numeric_coordinate_type = list(
      message = "sampling_fraction has invalid coordinate metadata.",
      mutate = function(x) {
        x$sampling_fraction <- "bad"
        x
      }
    ),
    character_coordinate_value = list(
      message = "variance_rule has invalid coordinate metadata.",
      mutate = function(x) {
        x$variance_rule <- "bad"
        x
      }
    )
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    error <- expect_error(
      format(case$mutate(base)),
      class = "sitemix_error_vcov_invariant",
      info = case_name
    )
    expect_true(inherits(error, "sitemix_error_vcov"), info = case_name)
    expect_match(
      conditionMessage(error),
      case$message,
      fixed = TRUE,
      info = case_name
    )
  }
})
