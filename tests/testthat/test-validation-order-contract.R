validation_contract_table <- function() {
  utils::read.csv(
    testthat::test_path(
      "_data", "conditions", "public-validation-contract.csv"
    ),
    stringsAsFactors = FALSE
  )
}

validation_contract_quiet <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(cnd) invokeRestart("muffleWarning")
  )
}

validation_contract_context <- function() {
  student <- data.frame(
    site_id = rep("S1", 4L),
    year = rep(2024L, 4L),
    y = c(1L, 0L, 1L, 0L),
    a = c(1L, 1L, 0L, 0L),
    b = c(1L, 0L, 1L, 0L)
  )
  scalar <- sitemix::sm_estimate_from_counts(
    data.frame(
      site_id = "S1", year = 2024L, n_jt = 8L, c_jt_y = 3L
    ),
    family = "binomial",
    indicator = "y",
    vjt = TRUE,
    min_n = 1L
  )
  corrupt_v <- scalar
  corrupt_v$V[[1L]]$matrix[1L, 1L] <- -1

  d1_data <- data.frame(
    site_id = c("S1", "S1"),
    year = c(2024L, 2024L),
    indicator = c("a", "b"),
    c_jt = c(3L, 5L),
    n_jt = c(8L, 8L),
    stringsAsFactors = FALSE
  )
  d1 <- validation_contract_quiet(
    sitemix::sm_estimate_from_aggregates(
      d1_data,
      family = "multivariate",
      sampling_relation = "same_units",
      vjt = TRUE,
      min_n = 1L
    )
  )
  aggregate_d0 <- data.frame(
    site_id = "S1",
    year = 2024L,
    indicator = "y",
    c_jt = 3L,
    n_jt = 8L,
    stringsAsFactors = FALSE
  )
  aggregate_suppressed <- aggregate_d0
  aggregate_suppressed$c_jt <- NA_integer_

  list(
    student = student,
    scalar = scalar,
    corrupt_v = corrupt_v,
    d1_data = d1_data,
    d1 = d1,
    aggregate_d0 = aggregate_d0,
    aggregate_suppressed = aggregate_suppressed
  )
}

validation_contract_capture <- function(id, ctx) {
  tryCatch(
    validation_contract_quiet(switch(
      id,
    estimate_family_before_scale = sitemix::sm_estimate(
      ctx$student,
      family = "D0",
      indicator = "y",
      vst = "bogus"
    ),
    estimate_path_before_scale = sitemix::sm_estimate(
      ctx$student,
      family = "binomial",
      indicator = "y",
      from_counts = TRUE,
      from_aggregates = TRUE,
      vst = "bogus"
    ),
    estimate_scale_before_indicator = sitemix::sm_estimate(
      ctx$student,
      family = "binomial",
      indicator = "y",
      indicators = c("a", "b"),
      vst = "bogus"
    ),
    estimate_indicator_before_anscombe = sitemix::sm_estimate(
      ctx$student,
      family = "multivariate",
      indicator = "y",
      indicators = c("a", "b"),
      vst = "logit",
      anscombe = TRUE
    ),
    estimate_sampling_relation_leaf = sitemix::sm_estimate(
      ctx$student,
      family = "binomial",
      indicator = "y",
      sampling_relation = "equal_denominators"
    ),
    counts_missing_family_before_lock = sitemix::sm_estimate_from_counts(
      ctx$student,
      indicator = "y",
      from_counts = FALSE
    ),
    aggregates_missing_family_before_lock = sitemix::sm_estimate_from_aggregates(
      ctx$aggregate_d0,
      indicator = "y",
      from_aggregates = FALSE
    ),
    diagnose_object_before_level = sitemix::sm_diagnose(
      data.frame(),
      level = "bogus"
    ),
    diagnose_vcov_before_level = sitemix::sm_diagnose(
      ctx$corrupt_v,
      level = "bogus"
    ),
    diagnose_level_before_verbose = sitemix::sm_diagnose(
      ctx$scalar,
      level = "bogus",
      verbose = "yes"
    ),
    frechet_object_before_flag = sitemix::sm_frechet_envelope(
      data.frame(),
      return_correlations = "yes"
    ),
    frechet_regime_before_flag = sitemix::sm_frechet_envelope(
      ctx$d1,
      return_correlations = "yes"
    ),
    frechet_flag_before_psd = sitemix::sm_frechet_envelope(
      ctx$d1,
      population_regime = "d1a",
      return_correlations = "yes",
      psd_method = "bogus"
    ),
    smooth_object_before_method = sitemix::sm_smooth_variance(
      data.frame(),
      method = "bogus"
    ),
    smooth_vcov_before_method = sitemix::sm_smooth_variance(
      ctx$corrupt_v,
      method = "bogus"
    ),
    smooth_method_before_flag = sitemix::sm_smooth_variance(
      ctx$scalar,
      method = "bogus",
      bias_correct = "yes"
    ),
    smooth_bias_flag_first = sitemix::sm_smooth_variance(
      ctx$scalar,
      bias_correct = "yes",
      overwrite = "yes"
    ),
    smooth_overwrite_flag_second = sitemix::sm_smooth_variance(
      ctx$scalar,
      bias_correct = TRUE,
      overwrite = "yes",
      return_diagnostics = "yes"
    ),
    smooth_return_flag_third = sitemix::sm_smooth_variance(
      ctx$scalar,
      bias_correct = TRUE,
      overwrite = FALSE,
      return_diagnostics = "yes"
    ),
    suppression_id_before_predicate = sitemix::sm_suppression_report(
      ctx$aggregate_d0,
      id_cols = "site_id",
      suppression_when = "bad"
    ),
    suppression_schema_before_predicate = sitemix::sm_suppression_report(
      data.frame(site_id = "S1", year = 2024L),
      suppression_when = "bad"
    ),
    suppression_ack_before_vcov = sitemix::sm_estimate_from_aggregates(
      ctx$aggregate_suppressed,
      family = "binomial",
      indicator = "y",
      suppression = "upper_bound",
      suppression_sensitivity_acknowledge = FALSE,
      vjt = TRUE
    ),
    vcov_matrix_before_metadata = sitemix::sm_vcov(
      matrix = matrix("x", 1L, 1L),
      indicator_order = "a",
      family = "bogus",
      estimate_scale = "bogus",
      vcov_scale = "bogus"
    ),
    vcov_dimnames_before_metadata = sitemix::sm_vcov(
      matrix = matrix(
        c(1, 0, 0, 1),
        2L,
        dimnames = list(c("a", "b"), c("a", "b"))
      ),
      indicator_order = c("x", "y"),
      family = "bogus",
      estimate_scale = "bogus",
      vcov_scale = "bogus"
    ),
    pivot_sites_column_before_policy = sitemix::sm_pivot_subgroups_to_sites(
      data.frame(),
      subgroup_col = 1,
      numerator_col = "c_jt",
      denominator_col = "n_jt",
      level_override = "unsupported"
    ),
    pivot_indicators_column_before_choice = sitemix::sm_pivot_subgroups_to_indicators(
      data.frame(),
      subgroup_col = 1,
      numerator_col = "c_jt",
      denominator_col = "n_jt",
      na_action = "bogus"
    ),
      stop("Unknown validation contract id: ", id, call. = FALSE)
    )),
    error = identity
  )
}

test_that("public validation matrix covers every exported entry point", {
  contract <- validation_contract_table()
  expected_entrypoints <- c(
    "sm_estimate",
    "sm_estimate_from_counts",
    "sm_estimate_from_aggregates",
    "sm_diagnose",
    "sm_frechet_envelope",
    "sm_smooth_variance",
    "sm_suppression_report",
    "sm_pivot_subgroups_to_sites",
    "sm_pivot_subgroups_to_indicators",
    "sm_vcov"
  )

  expect_named(
    contract,
    c(
      "id", "entrypoint", "stage_rank", "first_invalid",
      "second_invalid", "expected_leaf", "expected_branch",
      "policy_status"
    )
  )
  expect_equal(nrow(contract), 26L)
  expect_equal(anyDuplicated(contract$id), 0L)
  expect_setequal(contract$entrypoint, expected_entrypoints)
  expect_true(all(contract$stage_rank > 0L))
  expect_true(all(contract$policy_status == "locked"))
})

test_that("multi-invalid public inputs stop at the locked first condition", {
  contract <- validation_contract_table()
  ctx <- validation_contract_context()

  for (i in seq_len(nrow(contract))) {
    row <- contract[i, ]
    err <- validation_contract_capture(row$id, ctx)

    expect_s3_class(err, row$expected_leaf)
    expect_s3_class(err, row$expected_branch)
    expect_s3_class(err, "sitemix_error")
    expect_false(inherits(err, "simpleError"))
    expect_false(is.null(err$expected))
    expect_false(is.null(err$actual))
    expect_true(is.character(err$fix) && length(err$fix) == 1L && nzchar(err$fix))
  }
})

test_that("new argument-specific leaves carry argument locations", {
  ctx <- validation_contract_context()
  cases <- c(
    estimate_sampling_relation_leaf = "sampling_relation",
    counts_missing_family_before_lock = "family",
    aggregates_missing_family_before_lock = "family",
    frechet_flag_before_psd = "return_correlations",
    smooth_bias_flag_first = "bias_correct",
    smooth_overwrite_flag_second = "overwrite",
    smooth_return_flag_third = "return_diagnostics"
  )

  for (id in names(cases)) {
    err <- validation_contract_capture(id, ctx)
    expect_equal(err$location, list(argument = unname(cases[[id]])))
  }
})

test_that("dedicated public leaves preserve exact expected actual and fix metadata", {
  ctx <- validation_contract_context()
  cases <- list(
    estimate_sampling_relation_leaf = list(
      expected = c("unknown", "same_units", "different_units"),
      actual = "equal_denominators",
      fix = "Use `unknown`, `same_units`, or `different_units`; denominator equality is not sampling-unit provenance."
    ),
    counts_missing_family_before_lock = list(
      expected = c("binomial", "multivariate", "multinomial"),
      actual = "missing",
      fix = "Pass `family = \"binomial\"`, `\"multivariate\"`, or `\"multinomial\"`."
    ),
    aggregates_missing_family_before_lock = list(
      expected = c("binomial", "multivariate", "multinomial"),
      actual = "missing",
      fix = "Pass `family = \"binomial\"`, `\"multivariate\"`, or `\"multinomial\"`."
    ),
    frechet_flag_before_psd = list(
      expected = c("TRUE", "FALSE"),
      actual = "character",
      fix = "Pass a scalar logical value."
    ),
    smooth_bias_flag_first = list(
      expected = c("TRUE", "FALSE"),
      actual = "character",
      fix = "Pass a scalar logical value."
    ),
    suppression_ack_before_vcov = list(
      expected = "suppression_sensitivity_acknowledge = TRUE",
      actual = "FALSE",
      fix = "Set the acknowledgement only if separated sensitivity fields, rather than canonical estimates or covariance, are appropriate."
    ),
    vcov_matrix_before_metadata = list(
      expected = "numeric matrix",
      actual = "matrix/array",
      fix = "Pass a square numeric covariance matrix."
    )
  )

  for (id in names(cases)) {
    err <- validation_contract_capture(id, ctx)
    expect_equal(err$expected, cases[[id]]$expected)
    expect_equal(err$actual, cases[[id]]$actual)
    expect_equal(err$fix, cases[[id]]$fix)
  }
})

test_that("inactive argument enforcement is locked by Step 4.3", {
  ctx <- validation_contract_context()
  err <- validation_contract_capture("estimate_sampling_relation_leaf", ctx)

  expect_s3_class(err, "sitemix_error_invalid_sampling_relation")
  expect_match(
    err$fix,
    "denominator equality is not sampling-unit provenance",
    fixed = TRUE
  )
  expect_equal(
    validation_contract_table()$policy_status[
      validation_contract_table()$id == "estimate_sampling_relation_leaf"
    ],
    "locked"
  )
})
