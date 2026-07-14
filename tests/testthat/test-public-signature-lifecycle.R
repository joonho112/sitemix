public_lifecycle_table <- function() {
  utils::read.csv(
    testthat::test_path(
      "_data", "api", "public-signature-lifecycle.csv"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

public_default_fingerprint <- function(value, formal_name) {
  if (identical(formal_name, "...")) {
    return("<ellipsis>")
  }
  if (identical(value, quote(expr = ))) {
    return("<required>")
  }

  out <- paste(deparse(value, width.cutoff = 500L), collapse = "")
  if (identical(out, "\"\"")) {
    return("<empty_string>")
  }
  out <- gsub("\"", "'", out, fixed = TRUE)
  out <- gsub("[[:space:]]", "", out)
  gsub(",", ";", out, fixed = TRUE)
}

public_capture_warnings <- function(expr) {
  warnings <- list()
  value <- withCallingHandlers(
    expr,
    warning = function(cnd) {
      warnings[[length(warnings) + 1L]] <<- cnd
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}

public_student_fixture <- function() {
  data.frame(
    site_id = rep("S1", 4L),
    year = rep(2024L, 4L),
    y = c(1L, 0L, 1L, 0L),
    stringsAsFactors = FALSE
  )
}

test_that("the v0.2 public signature and defaults match the lifecycle matrix", {
  lifecycle <- public_lifecycle_table()
  active <- lifecycle[lifecycle$surface_status == "stable_v0.2", , drop = FALSE]
  exports <- sort(getNamespaceExports("sitemix"))

  expect_setequal(active$entrypoint, exports)
  expect_equal(nrow(active), 10L)
  expect_equal(anyDuplicated(lifecycle$entrypoint), 0L)

  for (i in seq_len(nrow(active))) {
    entrypoint <- active$entrypoint[[i]]
    fmls <- formals(getExportedValue("sitemix", entrypoint))
    actual_names <- paste(names(fmls), collapse = "|")
    actual_defaults <- paste(
      vapply(
        seq_along(fmls),
        function(j) public_default_fingerprint(fmls[[j]], names(fmls)[[j]]),
        character(1)
      ),
      collapse = "|"
    )

    expect_identical(actual_names, active$formal_names[[i]], info = entrypoint)
    expect_identical(actual_defaults, active$formal_defaults[[i]], info = entrypoint)
  }
})

test_that("the lifecycle matrix has no current deprecation or warning schedule", {
  lifecycle <- public_lifecycle_table()
  active <- lifecycle[lifecycle$surface_status == "stable_v0.2", , drop = FALSE]
  retired <- lifecycle[lifecycle$surface_status == "retired_v0.2", , drop = FALSE]

  expect_true(all(active$deprecated_formals == "none"))
  expect_true(all(active$lifecycle_warning == "none"))
  expect_true(all(active$removal_target == "none"))
  expect_identical(retired$entrypoint, "as_eb_input")
  expect_false("as_eb_input" %in% getNamespaceExports("sitemix"))
  expect_identical(retired$removal_target, "completed")
})

test_that("valid inactive aggregate controls are silent and have no effect", {
  data <- public_student_fixture()
  baseline <- sitemix::sm_estimate(
    data,
    family = "binomial",
    indicator = "y",
    min_n = 1L
  )

  inactive <- public_capture_warnings(sitemix::sm_estimate(
    data,
    family = "binomial",
    indicator = "y",
    min_n = 1L,
    aggregate_case = "D1",
    framing = "subgroup_as_indicator",
    sampling_relation = "different_units",
    suppression = "upper_bound",
    suppression_col = "unused_suppression_flag",
    suppression_flag_value = "S",
    suppression_when = function(x) rep(TRUE, nrow(x)),
    suppressed_theta_hat = 0.4,
    suppression_sensitivity_acknowledge = TRUE,
    suppressed_n_strategy = "worst_case_bound",
    suppressed_n_bound = 5L,
    numerator_col = "unused_numerator",
    denominator_col = "unused_denominator",
    indicator_col = "unused_indicator",
    subgroup_col = "unused_subgroup"
  ))

  expect_length(inactive$warnings, 0L)
  expect_identical(inactive$value, baseline)

  counts <- data.frame(
    site_id = "S1", year = 2024L, n_jt = 4L, c_jt_y = 2L
  )
  count_baseline <- sitemix::sm_estimate_from_counts(
    counts, family = "binomial", indicator = "y", min_n = 1L
  )
  count_inactive <- public_capture_warnings(sitemix::sm_estimate_from_counts(
    counts,
    family = "binomial",
    indicator = "y",
    min_n = 1L,
    aggregate_case = "D1",
    framing = "subgroup_as_indicator",
    sampling_relation = "same_units",
    suppression = "upper_bound",
    suppression_col = "unused_suppression_flag",
    suppression_when = function(x) rep(TRUE, nrow(x)),
    numerator_col = "unused_numerator"
  ))

  expect_length(count_inactive$warnings, 0L)
  expect_identical(count_inactive$value, count_baseline)
})

test_that("all invalid path controls are classed and silent across every dispatch", {
  counts <- data.frame(
    site_id = "S1", year = 2024L, n_jt = 4L, c_jt_y = 2L
  )
  aggregate <- data.frame(
    site_id = "S1", year = 2024L, indicator = "y", c_jt = 2L, n_jt = 4L
  )
  paths <- list(
    student = list(
      fn = sitemix::sm_estimate,
      args = list(data = public_student_fixture(), family = "binomial", indicator = "y")
    ),
    direct_counts = list(
      fn = sitemix::sm_estimate,
      args = list(data = counts, family = "binomial", indicator = "y", from_counts = TRUE)
    ),
    counts_wrapper = list(
      fn = sitemix::sm_estimate_from_counts,
      args = list(data = counts, family = "binomial", indicator = "y")
    ),
    direct_aggregate = list(
      fn = sitemix::sm_estimate,
      args = list(data = aggregate, family = "binomial", indicator = "y", from_aggregates = TRUE)
    ),
    aggregate_wrapper = list(
      fn = sitemix::sm_estimate_from_aggregates,
      args = list(data = aggregate, family = "binomial", indicator = "y")
    )
  )
  invalid <- list(
    aggregate_case = list(
      value = list(aggregate_case = "D2"),
      class = "sitemix_error_invalid_aggregate_case",
      location = NULL
    ),
    framing = list(
      value = list(framing = "unsupported"),
      class = "sitemix_error_invalid_framing",
      location = NULL
    ),
    sampling_relation = list(
      value = list(sampling_relation = "equal_denominators"),
      class = "sitemix_error_invalid_sampling_relation",
      location = "sampling_relation"
    ),
    suppression = list(
      value = list(suppression = "impute"),
      class = "sitemix_error_invalid_suppression_mode",
      location = NULL
    ),
    suppression_col = list(
      value = list(suppression_col = 1),
      class = "sitemix_error_invalid_suppression_col",
      location = "suppression_col"
    ),
    suppression_flag_value = list(
      value = list(suppression_flag_value = list("S")),
      class = "sitemix_error_invalid_suppression_col",
      location = NULL
    ),
    suppression_when = list(
      value = list(suppression_when = "bad"),
      class = "sitemix_error_invalid_suppression_when",
      location = NULL
    ),
    suppressed_theta_hat = list(
      value = list(suppressed_theta_hat = Inf),
      class = "sitemix_error_invalid_suppressed_theta_hat",
      location = NULL
    ),
    suppression_sensitivity_acknowledge = list(
      value = list(suppression_sensitivity_acknowledge = "yes"),
      class = "sitemix_error_suppression_sensitivity_acknowledgement",
      location = NULL
    ),
    suppressed_n_strategy = list(
      value = list(suppressed_n_strategy = "guess"),
      class = "sitemix_error_invalid_suppressed_n",
      location = NULL
    ),
    suppressed_n_bound = list(
      value = list(suppressed_n_bound = 1e20),
      class = "sitemix_error_invalid_suppressed_n",
      location = "suppressed_n_bound"
    ),
    numerator_col = list(
      value = list(numerator_col = 1),
      class = "sitemix_error_invalid_aggregate_schema",
      location = "numerator_col"
    ),
    denominator_col = list(
      value = list(denominator_col = 1),
      class = "sitemix_error_invalid_aggregate_schema",
      location = "denominator_col"
    ),
    indicator_col = list(
      value = list(indicator_col = 1),
      class = "sitemix_error_invalid_aggregate_schema",
      location = "indicator_col"
    ),
    subgroup_col = list(
      value = list(subgroup_col = 1),
      class = "sitemix_error_invalid_aggregate_schema",
      location = "subgroup_col"
    )
  )

  for (path_name in names(paths)) {
    path <- paths[[path_name]]
    for (case_name in names(invalid)) {
      case <- invalid[[case_name]]
      info <- paste(path_name, case_name, sep = " / ")
      captured <- public_capture_warnings(tryCatch(
        do.call(path$fn, c(path$args, case$value)),
        error = identity
      ))
      err <- captured$value

      expect_length(captured$warnings, 0L)
      expect_s3_class(err, case$class)
      expect_s3_class(err, "sitemix_error")
      expect_false(inherits(err, "simpleError"), info = info)
      expect_false(is.null(err$expected), info = info)
      expect_false(is.null(err$actual), info = info)
      expect_true(
        is.character(err$fix) && length(err$fix) == 1L && nzchar(err$fix),
        info = info
      )
      expected_location <- if (is.null(case$location)) {
        NULL
      } else {
        list(argument = case$location)
      }
      expect_identical(err$location, expected_location, info = info)
    }
  }
})

test_that("valid legacy direct and wrapper call shapes remain identical", {
  counts <- data.frame(
    site_id = "S1", year = 2024L, n_jt = 4L, c_jt_y = 2L
  )
  aggregate <- data.frame(
    site_id = "S1", year = 2024L, indicator = "y", c_jt = 2L, n_jt = 4L
  )

  direct_counts <- sitemix::sm_estimate(
    counts, family = "binomial", indicator = "y", from_counts = TRUE, vjt = TRUE
  )
  wrapper_counts <- sitemix::sm_estimate_from_counts(
    counts, family = "binomial", indicator = "y", vjt = TRUE
  )
  direct_aggregate <- sitemix::sm_estimate(
    aggregate, family = "binomial", indicator = "y", from_aggregates = TRUE, vjt = TRUE
  )
  wrapper_aggregate <- sitemix::sm_estimate_from_aggregates(
    aggregate, family = "binomial", indicator = "y", vjt = TRUE
  )

  expect_identical(direct_counts, wrapper_counts)
  expect_identical(direct_aggregate, wrapper_aggregate)
})
