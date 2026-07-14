test_that("error class expansion includes leaf, branch, and root", {
  input_classes <- sitemix:::.sm_error_classes("sitemix_error_input_columns")
  expect_equal(
    input_classes,
    c(
      "sitemix_error_input_columns",
      "sitemix_error_input",
      "sitemix_error"
    )
  )

  argument_classes <- sitemix:::.sm_error_classes("sitemix_error_invalid_family")
  expect_equal(
    argument_classes,
    c(
      "sitemix_error_invalid_family",
      "sitemix_error_argument",
      "sitemix_error"
    )
  )

  expect_equal(
    sitemix:::.sm_condition_classes("sitemix_warning_dropped_rows"),
    c("sitemix_warning_dropped_rows", "sitemix_warning")
  )
  expect_equal(
    sitemix:::.sm_condition_classes("sitemix_warning_working_independence_default"),
    c("sitemix_warning_working_independence_default", "sitemix_warning")
  )
  expect_equal(
    sitemix:::.sm_condition_classes("sitemix_warning_frechet_d1b_heuristic"),
    c("sitemix_warning_frechet_d1b_heuristic", "sitemix_warning")
  )
  expect_equal(
    sitemix:::.sm_condition_classes("sitemix_warning_smoother_skipped"),
    c("sitemix_warning_smoother_skipped", "sitemix_warning")
  )
  expect_equal(
    sitemix:::.sm_condition_classes("sitemix_warning_raw_scale_smoothing"),
    c("sitemix_warning_raw_scale_smoothing", "sitemix_warning")
  )
  expect_equal(
    sitemix:::.sm_condition_classes("sitemix_warning_smoother_multi_year_default"),
    c("sitemix_warning_smoother_multi_year_default", "sitemix_warning")
  )
  expect_equal(
    sitemix:::.sm_condition_classes("sitemix_warning_unexpected_slope"),
    c("sitemix_warning_unexpected_slope", "sitemix_warning")
  )
  expect_equal(
    sitemix:::.sm_condition_classes("sitemix_warning_frechet_non_diagonal_v"),
    c("sitemix_warning_frechet_non_diagonal_v", "sitemix_warning")
  )
  expect_equal(
    sitemix:::.sm_condition_classes("sitemix_warning"),
    "sitemix_warning"
  )
})

test_that("registered leaves map to expected branches", {
  cases <- data.frame(
    leaf = c(
      "sitemix_error_input_class",
      "sitemix_error_input_columns",
      "sitemix_error_input_type",
      "sitemix_error_input_missing",
      "sitemix_error_input_indicator_count",
      "sitemix_error_frechet_envelope_missing",
      "sitemix_error_invalid_family",
      "sitemix_error_invalid_indicator",
      "sitemix_error_invalid_indicators",
      "sitemix_error_invalid_id_cols",
      "sitemix_error_invalid_vst",
      "sitemix_error_invalid_boundary",
      "sitemix_error_invalid_bias",
      "sitemix_error_invalid_vjt",
      "sitemix_error_invalid_min_n",
      "sitemix_error_invalid_accountability_n",
      "sitemix_error_invalid_fpc",
      "sitemix_error_invalid_anscombe",
      "sitemix_error_invalid_from_counts",
      "sitemix_error_invalid_na_action",
      "sitemix_error_invalid_description",
      "sitemix_error_invalid_diagnose_level",
      "sitemix_error_invalid_verbose",
      "sitemix_error_diagnose_vcov_missing",
      "sitemix_error_invalid_from_aggregates",
      "sitemix_error_invalid_aggregate_case",
      "sitemix_error_invalid_sampling_relation",
      "sitemix_error_invalid_suppression_col",
      "sitemix_error_invalid_suppression_when",
      "sitemix_error_invalid_suppressed_theta_hat",
      "sitemix_error_suppression_sensitivity_acknowledgement",
      "sitemix_error_suppression_sensitivity_excluded",
      "sitemix_error_invalid_subgroup_col",
      "sitemix_error_invalid_partition_target",
      "sitemix_error_invalid_level_override",
      "sitemix_error_population_regime_required",
      "sitemix_error_invalid_population_regime",
      "sitemix_error_frechet_d1b_disallowed",
      "sitemix_error_invalid_return_correlations",
      "sitemix_error_invalid_psd_method",
      "sitemix_error_invalid_psd_tol",
      "sitemix_error_invalid_psd_max_iter",
      "sitemix_error_invalid_shrink_alpha",
      "sitemix_error_invalid_smoothing_method",
      "sitemix_error_invalid_smoothing_scale",
      "sitemix_error_invalid_smoothing_scope",
      "sitemix_error_invalid_smoothing_by",
      "sitemix_error_invalid_smoothing_flag",
      "sitemix_error_invalid_smoothing_min_rows",
      "sitemix_error_invalid_smoothing_formula",
      "sitemix_error_smoother_fit",
      "sitemix_error_smoother_prediction",
      "sitemix_error_smoothing_v_stale",
      "sitemix_error_smoother_gam_unavailable",
      "sitemix_error_anscombe_requires_arcsine",
      "sitemix_error_anscombe_incompatible_correction",
      "sitemix_error_estimate_zero_n",
      "sitemix_error_estimate_var_method",
      "sitemix_error_estimate_vcov_invariant",
      "sitemix_error_vcov_invariant",
      "sitemix_error_vcov_projection_nonconvergence",
      "sitemix_error_vcov_dimnames",
      "sitemix_error_invalid_aggregate_schema",
      "sitemix_error_invalid_aggregate_row",
      "sitemix_error_ambiguous_dispatch",
      "sitemix_error_invalid_framing",
      "sitemix_error_invalid_suppression_mode",
      "sitemix_error_invalid_suppressed_n",
      "sitemix_error_input_path_conflict"
    ),
    branch = c(
      rep("sitemix_error_input", 6),
      rep("sitemix_error_argument", 50),
      rep("sitemix_error_estimate", 3),
      rep("sitemix_error_vcov", 3),
      rep("sitemix_error_aggregate", 7)
    )
  )

  for (i in seq_len(nrow(cases))) {
    classes <- sitemix:::.sm_condition_classes(cases$leaf[[i]])
    expect_true(cases$leaf[[i]] %in% classes)
    expect_true(cases$branch[[i]] %in% classes)
    expect_true("sitemix_error" %in% classes)
  }
})

test_that("registered warning classes are locked", {
  expect_equal(
    sitemix:::.sm_warning_classes_known,
    c(
      "sitemix_warning",
      "sitemix_warning_dropped_rows",
      "sitemix_warning_working_independence_default",
      "sitemix_warning_frechet_d1b_heuristic",
      "sitemix_warning_frechet_non_diagonal_v",
      "sitemix_warning_smoother_skipped",
      "sitemix_warning_smoother_multi_year_default",
      "sitemix_warning_unexpected_slope",
      "sitemix_warning_raw_scale_smoothing"
    )
  )
})

test_that("diagnostic warning codes are separate from emitted conditions", {
  expect_equal(
    sitemix:::.sm_diagnostic_warning_codes_known,
    c(
      suppression_sensitivity = "sitemix_warning_suppression_sensitivity",
      suppression_dropped = "sitemix_warning_suppression_dropped",
      estimate_vcov_scale = "sitemix_warning_estimate_vcov_scale_mismatch",
      mixed_vcov_scale = "sitemix_warning_mixed_vcov_scale_relation"
    )
  )
  expect_false(any(
    unname(sitemix:::.sm_diagnostic_warning_codes_known) %in%
      sitemix:::.sm_warning_classes_known
  ))
})

test_that("retired and orphan condition leaves are absent from active registries", {
  active <- unique(c(
    sitemix:::.sm_input_error_classes,
    sitemix:::.sm_argument_error_classes,
    sitemix:::.sm_estimate_error_classes,
    sitemix:::.sm_vcov_error_classes,
    sitemix:::.sm_aggregate_error_classes,
    sitemix:::.sm_warning_classes_known
  ))
  retired <- c(
    "sitemix_error_diagnose_vcov_unavailable",
    "sitemix_error_invalid_emit_suppression_report"
  )

  expect_false(any(retired %in% active))
  expect_false(any(grepl("adapter|ebrecipe|eb_handoff", active)))
})

test_that("source and generated docs name only registered condition tokens", {
  package_root <- normalizePath(
    testthat::test_path("..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
  files <- c(
    list.files(file.path(package_root, "R"), pattern = "\\.R$", full.names = TRUE),
    list.files(file.path(package_root, "man"), pattern = "\\.Rd$", full.names = TRUE)
  )
  text <- paste(
    unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE),
    collapse = "\n"
  )
  matches <- gregexpr(
    "sitemix_(?:error|warning)(?:_[A-Za-z0-9]+)*",
    text,
    perl = TRUE
  )
  documented <- unique(regmatches(text, matches)[[1L]])
  registered <- unique(c(
    "sitemix_error",
    sitemix:::.sm_input_error_classes,
    sitemix:::.sm_argument_error_classes,
    sitemix:::.sm_estimate_error_classes,
    sitemix:::.sm_vcov_error_classes,
    sitemix:::.sm_aggregate_error_classes,
    sitemix:::.sm_warning_classes_known,
    unname(sitemix:::.sm_diagnostic_warning_codes_known)
  ))

  expect_length(setdiff(documented, registered), 0L)
})

test_that("all registered error classes preserve inheritance, message body, and metadata", {
  registry <- list(
    sitemix_error_input = sitemix:::.sm_input_error_classes,
    sitemix_error_argument = sitemix:::.sm_argument_error_classes,
    sitemix_error_estimate = sitemix:::.sm_estimate_error_classes,
    sitemix_error_vcov = sitemix:::.sm_vcov_error_classes,
    sitemix_error_aggregate = sitemix:::.sm_aggregate_error_classes
  )

  aborters <- list(
    sitemix_error_input = sitemix:::.sm_abort_input,
    sitemix_error_argument = sitemix:::.sm_abort_argument,
    sitemix_error_estimate = sitemix:::.sm_abort_estimate,
    sitemix_error_vcov = sitemix:::.sm_abort_vcov,
    sitemix_error_aggregate = sitemix:::.sm_abort_aggregate
  )

  for (branch in names(registry)) {
    for (class in registry[[branch]]) {
      err <- rlang::catch_cnd(
        aborters[[branch]](
          sprintf("Synthetic condition for %s.", class),
          class = class,
          expected = "expected value",
          actual = "actual value",
          row_identity = list(site_id = "S1", year = 2026L),
          location = list(argument = "arg"),
          fix = "Use a valid input."
        )
      )

      expect_s3_class(err, class)
      expect_s3_class(err, branch)
      expect_s3_class(err, "sitemix_error")
      expect_equal(err$expected, "expected value")
      expect_equal(err$actual, "actual value")
      expect_equal(err$row_identity, list(site_id = "S1", year = 2026L))
      expect_equal(err$location, list(argument = "arg"))
      expect_equal(err$fix, "Use a valid input.")
      expect_match(rlang::cnd_message(err), "Expected", fixed = TRUE)
      expect_match(rlang::cnd_message(err), "Actual", fixed = TRUE)
      expect_match(rlang::cnd_message(err), "Location", fixed = TRUE)
      expect_match(rlang::cnd_message(err), "Fix", fixed = TRUE)
    }
  }
})

test_that("all registered warning classes preserve inheritance, message body, and metadata", {
  for (class in sitemix:::.sm_warning_classes_known) {
    warning <- rlang::catch_cnd(
      sitemix:::.sm_warn(
        sprintf("Synthetic warning for %s.", class),
        class = class,
        expected = "expected value",
        actual = "actual value",
        row_identity = list(row_index = 1L),
        location = list(site_id = "S1", year = 2026L),
        fix = "Inspect the diagnostic output."
      )
    )

    expect_s3_class(warning, class)
    expect_s3_class(warning, "sitemix_warning")
    expect_equal(warning$expected, "expected value")
    expect_equal(warning$actual, "actual value")
    expect_equal(warning$row_identity, list(row_index = 1L))
    expect_equal(warning$location, list(site_id = "S1", year = 2026L))
    expect_equal(warning$fix, "Inspect the diagnostic output.")
    expect_match(rlang::cnd_message(warning), "Expected", fixed = TRUE)
    expect_match(rlang::cnd_message(warning), "Actual", fixed = TRUE)
    expect_match(rlang::cnd_message(warning), "Row identity", fixed = TRUE)
    expect_match(rlang::cnd_message(warning), "Location", fixed = TRUE)
    expect_match(rlang::cnd_message(warning), "Fix", fixed = TRUE)
  }
})

test_that("abort helpers preserve class inheritance and metadata", {
  err <- rlang::catch_cnd(
    sitemix:::.sm_abort_argument(
      "Invalid `family`.",
      class = "sitemix_error_invalid_family",
      expected = c("binomial", "multivariate", "multinomial"),
      actual = "D0",
      row_identity = list(site_id = "A", year = 2026L),
      fix = "Use `aggregate_case` for D0/D1 aggregate inputs."
    )
  )

  expect_s3_class(err, "sitemix_error_invalid_family")
  expect_s3_class(err, "sitemix_error_argument")
  expect_s3_class(err, "sitemix_error")
  expect_equal(err$expected, c("binomial", "multivariate", "multinomial"))
  expect_equal(err$actual, "D0")
  expect_equal(err$row_identity, list(site_id = "A", year = 2026L))
  expect_match(err$fix, "aggregate_case", fixed = TRUE)
  expect_match(rlang::cnd_message(err), "Expected")
  expect_match(rlang::cnd_message(err), "D0", fixed = TRUE)
  expect_match(rlang::cnd_message(err), "Row identity")
})

test_that("package root and branch handlers catch sitemix errors", {
  root <- tryCatch(
    sitemix:::.sm_abort_input(
      "Missing required columns.",
      class = "sitemix_error_input_columns"
    ),
    sitemix_error = function(err) err
  )
  expect_s3_class(root, "sitemix_error")

  branch <- tryCatch(
    sitemix:::.sm_abort_argument(
      "Invalid `vst`.",
      class = "sitemix_error_invalid_vst"
    ),
    sitemix_error_argument = function(err) err
  )
  expect_s3_class(branch, "sitemix_error_argument")
})

test_that("every error branch can be caught by its branch handler", {
  cases <- list(
    input = list(
      abort = sitemix:::.sm_abort_input,
      leaf = "sitemix_error_input_columns",
      branch = "sitemix_error_input",
      handler = function(expr) tryCatch(expr, sitemix_error_input = function(err) err)
    ),
    argument = list(
      abort = sitemix:::.sm_abort_argument,
      leaf = "sitemix_error_invalid_family",
      branch = "sitemix_error_argument",
      handler = function(expr) tryCatch(expr, sitemix_error_argument = function(err) err)
    ),
    estimate = list(
      abort = sitemix:::.sm_abort_estimate,
      leaf = "sitemix_error_estimate_var_method",
      branch = "sitemix_error_estimate",
      handler = function(expr) tryCatch(expr, sitemix_error_estimate = function(err) err)
    ),
    vcov = list(
      abort = sitemix:::.sm_abort_vcov,
      leaf = "sitemix_error_vcov_invariant",
      branch = "sitemix_error_vcov",
      handler = function(expr) tryCatch(expr, sitemix_error_vcov = function(err) err)
    ),
    aggregate = list(
      abort = sitemix:::.sm_abort_aggregate,
      leaf = "sitemix_error_invalid_aggregate_schema",
      branch = "sitemix_error_aggregate",
      handler = function(expr) tryCatch(expr, sitemix_error_aggregate = function(err) err)
    )
  )

  for (case in cases) {
    err <- case$handler(
      case$abort("Synthetic branch error.", class = case$leaf)
    )
    expect_s3_class(err, case$leaf)
    expect_s3_class(err, case$branch)
    expect_s3_class(err, "sitemix_error")
  }
})

test_that("warning helpers preserve class inheritance and metadata", {
  warning <- rlang::catch_cnd(
    sitemix:::.sm_warn(
      "Dropped rows encountered.",
      class = "sitemix_warning_dropped_rows",
      location = list(site_id = "A", year = 2026L, indicator = "x"),
      expected = "positive numerator",
      actual = "zero numerator",
      fix = "Inspect `flag_zero_cell`."
    )
  )

  expect_s3_class(warning, "sitemix_warning_dropped_rows")
  expect_s3_class(warning, "sitemix_warning")
  expect_equal(
    warning$location,
    list(site_id = "A", year = 2026L, indicator = "x")
  )
  expect_equal(warning$expected, "positive numerator")
  expect_match(rlang::cnd_message(warning), "Fix")
})

test_that("condition data omits null fields", {
  fields <- sitemix:::.sm_condition_data(
    expected = "binary",
    actual = "factor",
    row_identity = list(row_index = 3L),
    location = NULL,
    fix = "Recode explicitly."
  )

  expect_named(fields, c("expected", "actual", "row_identity", "fix"))
})

test_that("CLI helpers format stable bullets", {
  expect_equal(
    sitemix:::.sm_cli_missing_columns(c("site_id", "year")),
    "Missing required columns: `site_id` and `year`."
  )
  expect_equal(
    sitemix:::.sm_cli_expected_actual("binomial", "D0"),
    c(x = "Expected `binomial`.", i = "Actual: `D0`.")
  )
  expect_equal(
    sitemix:::.sm_cli_fix("Use `family = \"binomial\"`."),
    c(i = "Fix: Use `family = \"binomial\"`.")
  )
  expect_equal(
    sitemix:::.sm_cli_row_identity(
      list(site_id = "S034", year = 2024L, indicator = "snap", row_index = 17L)
    ),
    'site_id = "S034", year = 2024, indicator = "snap", row_index = 17'
  )
  expect_match(
    paste(
      sitemix:::.sm_cli_condition_body(
        expected = c("binomial", "multivariate"),
        actual = "D0",
        fix = "Use `aggregate_case = \"D0\"`."
      ),
      collapse = "\n"
    ),
    "aggregate_case",
    fixed = TRUE
  )
})
