sm_vcov_api_args <- function() {
  labels <- c("x", "y")
  list(
    matrix = diag(c(x = 0.1, y = 0.2)),
    site_id = "S-API",
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
}

sm_vcov_api_error <- function(args) {
  tryCatch(do.call(sm_vcov, args), error = identity)
}

test_that("sm_vcov has one public constructor and three registered methods", {
  exports <- getNamespaceExports("sitemix")
  expect_true("sm_vcov" %in% exports)
  expect_false("new_sm_vcov" %in% exports)
  expect_false("validate.sm_vcov" %in% exports)

  ns <- asNamespace("sitemix")
  expect_true(exists(".sm_new_sm_vcov", envir = ns, inherits = FALSE))
  expect_true(exists("new_sm_vcov", envir = ns, inherits = FALSE))
  expect_true(exists(".sm_validate_sm_vcov", envir = ns, inherits = FALSE))
  expect_true(exists("validate.sm_vcov", envir = ns, inherits = FALSE))
  expect_identical(
    get("new_sm_vcov", envir = ns),
    get(".sm_new_sm_vcov", envir = ns)
  )
  expect_identical(
    get("validate.sm_vcov", envir = ns),
    get(".sm_validate_sm_vcov", envir = ns)
  )

  expect_true(is.function(getS3method("format", "sm_vcov", optional = TRUE)))
  expect_true(is.function(getS3method("print", "sm_vcov", optional = TRUE)))
  expect_true(is.function(getS3method("as.matrix", "sm_vcov", optional = TRUE)))

  registered <- getNamespaceInfo(ns, "S3methods")
  registered_vcov <- registered[registered[, 2L] == "sm_vcov", 1:2, drop = FALSE]
  expect_identical(
    unname(registered_vcov),
    cbind(
      c("as.matrix", "format", "print"),
      rep("sm_vcov", 3L)
    )
  )
  expect_false(any(registered[, 1L] == "validate" & registered[, 2L] == "sm_vcov"))
})

test_that("sm_vcov public formals remain locked", {
  expect_identical(
    names(formals(sitemix::sm_vcov)),
    c(
      "matrix", "site_id", "year", "indicator_order", "family",
      "vcov_method", "estimate_scale", "vcov_scale",
      "matrix_boundary_rule", "scalar_correction_rule", "psd_repair",
      "matrix_rank", "positive_support", "n_jt", "n_eff",
      "population_size", "sampling_fraction", "fpc_variance_multiplier",
      "fpc_se_multiplier", "variance_multiplier_applied",
      "se_multiplier_applied", "sampling_design", "variance_rule",
      "diag_contract"
    )
  )
  expect_identical(eval(formals(sitemix::sm_vcov)$year), NA_integer_)
  expect_null(eval(formals(sitemix::sm_vcov)$matrix_rank))
  expect_identical(eval(formals(sitemix::sm_vcov)$positive_support), NA_integer_)
  expect_identical(eval(formals(sitemix::sm_vcov)$n_jt), NA_integer_)
})

test_that("sm_vcov rejects fractional integer metadata before coercion", {
  base <- sm_vcov_api_args()
  cases <- list(
    year = 2024.5,
    n_jt = 20.5,
    matrix_rank = 2.5,
    positive_support = 1.5
  )

  for (field in names(cases)) {
    args <- base
    args[[field]] <- cases[[field]]
    err <- sm_vcov_api_error(args)
    expect_s3_class(err, "sitemix_error_vcov_invariant")
    expect_s3_class(err, "sitemix_error_vcov")
    expect_match(conditionMessage(err), field, fixed = TRUE, info = field)
    expect_match(conditionMessage(err), "whole-number", fixed = TRUE, info = field)
    expect_match(err$expected, "whole-number", fixed = TRUE, info = field)
    expect_match(err$fix, field, fixed = TRUE, info = field)
  }
})

test_that("sm_vcov accepts whole-number numeric metadata without truncation", {
  V <- do.call(sm_vcov, utils::modifyList(
    sm_vcov_api_args(),
    list(year = 2025, n_jt = 21, matrix_rank = 2, positive_support = 2)
  ))

  expect_identical(V$year, 2025L)
  expect_identical(V$n_jt, 21L)
  expect_identical(V$matrix_rank, 2L)
  expect_identical(V$positive_support, 2L)

  missing_values <- do.call(sm_vcov, utils::modifyList(
    sm_vcov_api_args(),
    list(year = NA, n_jt = NA_real_, positive_support = NA)
  ))
  expect_identical(missing_values$year, NA_integer_)
  expect_identical(missing_values$n_jt, NA_integer_)
  expect_identical(missing_values$positive_support, NA_integer_)
})

test_that("sm_vcov integer metadata guards reject non-finite and overflow values", {
  cases <- list(
    year = NaN,
    n_jt = Inf,
    matrix_rank = .Machine$integer.max + 1,
    positive_support = -Inf
  )

  for (field in names(cases)) {
    args <- sm_vcov_api_args()
    args[[field]] <- cases[[field]]
    expect_error(
      do.call(sm_vcov, args),
      class = "sitemix_error_vcov_invariant"
    )
  }
})

test_that("sm_vcov rejects non-character coordinates and empty matrices", {
  for (bad_order in list(
    c(1, 2),
    factor(c("x", "y")),
    matrix(c("x", "y"), ncol = 1L)
  )) {
    err <- sm_vcov_api_error(utils::modifyList(
      sm_vcov_api_args(),
      list(indicator_order = bad_order)
    ))
    expect_s3_class(err, "sitemix_error_vcov_dimnames")
    expect_s3_class(err, "sitemix_error_vcov")
    expect_match(conditionMessage(err), "must be a character vector", fixed = TRUE)
  }

  args <- sm_vcov_api_args()
  args$matrix <- matrix(numeric(), 0L, 0L)
  args$indicator_order <- character()
  args$scalar_correction_rule <- character()
  err <- sm_vcov_api_error(args)
  expect_s3_class(err, "sitemix_error_vcov_invariant")
  expect_s3_class(err, "sitemix_error_vcov")
  expect_match(conditionMessage(err), "at least one covariance coordinate", fixed = TRUE)
  expect_identical(err$actual, "0 x 0")
})

test_that("sm_vcov reports missing required scale metadata as classed errors", {
  required <- list(
    family = c("binomial", "multivariate", "multinomial"),
    estimate_scale = c("none", "arcsine", "arcsine_anscombe", "logit"),
    vcov_scale = c("raw", "arcsine_delta", "logit_delta", "reference_raw")
  )

  for (field in names(required)) {
    args <- sm_vcov_api_args()
    args[[field]] <- NULL
    err <- sm_vcov_api_error(args)
    expect_s3_class(err, "sitemix_error_vcov_invariant")
    expect_s3_class(err, "sitemix_error_vcov")
    expect_match(conditionMessage(err), paste0("`", field, "` is required."), fixed = TRUE)
    expect_identical(err$expected, required[[field]], info = field)
    expect_identical(err$actual, "missing", info = field)
    expect_match(err$fix, field, fixed = TRUE, info = field)
  }
})

test_that("public construction performs one canonical object validation", {
  validation_calls <- 0L
  validator <- sitemix:::.sm_validate_sm_vcov
  testthat::local_mocked_bindings(
    .sm_validate_sm_vcov = function(x) {
      validation_calls <<- validation_calls + 1L
      validator(x)
    },
    .package = "sitemix"
  )

  V <- do.call(sm_vcov, sm_vcov_api_args())
  expect_s3_class(V, "sm_vcov")
  expect_identical(validation_calls, 1L)
})

test_that("sm_vcov format print and matrix methods retain their snapshots", {
  V <- sm_vcov(
    matrix = matrix(0.01, 1L, 1L, dimnames = list("x", "x")),
    site_id = "S009",
    year = 2024L,
    indicator_order = "x",
    family = "binomial",
    estimate_scale = "none",
    vcov_scale = "raw",
    scalar_correction_rule = "none",
    n_jt = 10L,
    n_eff = 10
  )

  expect_identical(format(V), "<sm_vcov[1x1] binomial/NA raw rank=1>")
  expect_identical(
    utils::capture.output(print(V)),
    c(
      "sm_vcov[1x1] site_id=S009 year=2024 family=binomial vcov_method=NA vcov_scale=raw",
      "     x",
      "x 0.01",
      "matrix_rank=1 psd_repair=none estimate_scale=none matrix_boundary_rule=none indicators=x"
    )
  )
  expect_identical(
    as.matrix(V),
    matrix(0.01, 1L, 1L, dimnames = list("x", "x"))
  )

  tampered <- V
  tampered$year <- 2024.5
  expect_error(format(tampered), class = "sitemix_error_vcov_invariant")
  expect_error(print(tampered), class = "sitemix_error_vcov_invariant")
  expect_error(as.matrix(tampered), class = "sitemix_error_vcov_invariant")
})
