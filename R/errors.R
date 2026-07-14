# Condition helpers ---------------------------------------------------------

.sm_input_error_classes <- c(
  "sitemix_error_input",
  "sitemix_error_input_class",
  "sitemix_error_input_columns",
  "sitemix_error_input_type",
  "sitemix_error_input_missing",
  "sitemix_error_input_indicator_count",
  "sitemix_error_frechet_envelope_missing"
)

.sm_argument_error_classes <- c(
  "sitemix_error_argument",
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
  "sitemix_error_anscombe_incompatible_correction"
)

.sm_estimate_error_classes <- c(
  "sitemix_error_estimate",
  "sitemix_error_estimate_zero_n",
  "sitemix_error_estimate_var_method",
  "sitemix_error_estimate_vcov_invariant"
)

.sm_vcov_error_classes <- c(
  "sitemix_error_vcov",
  "sitemix_error_vcov_invariant",
  "sitemix_error_vcov_projection_nonconvergence",
  "sitemix_error_vcov_dimnames"
)

.sm_aggregate_error_classes <- c(
  "sitemix_error_aggregate",
  "sitemix_error_invalid_aggregate_schema",
  "sitemix_error_invalid_aggregate_row",
  "sitemix_error_ambiguous_dispatch",
  "sitemix_error_invalid_framing",
  "sitemix_error_invalid_suppression_mode",
  "sitemix_error_invalid_suppressed_n",
  "sitemix_error_input_path_conflict"
)

.sm_warning_classes_known <- c(
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

.sm_error_branch <- function(class) {
  if (class %in% .sm_input_error_classes) {
    "sitemix_error_input"
  } else if (class %in% .sm_argument_error_classes) {
    "sitemix_error_argument"
  } else if (class %in% .sm_estimate_error_classes) {
    "sitemix_error_estimate"
  } else if (class %in% .sm_vcov_error_classes) {
    "sitemix_error_vcov"
  } else if (class %in% .sm_aggregate_error_classes) {
    "sitemix_error_aggregate"
  } else if (identical(class, "sitemix_error")) {
    "sitemix_error"
  } else {
    stop("Unknown sitemix error class: ", class, call. = FALSE)
  }
}

.sm_error_classes <- function(class) {
  if (!is.character(class) || length(class) != 1L || is.na(class)) {
    stop("`class` must be a single non-missing character string.", call. = FALSE)
  }

  branch <- .sm_error_branch(class)
  unique(c(class, branch, "sitemix_error"))
}

.sm_warning_classes <- function(class) {
  if (!is.character(class) || length(class) != 1L || is.na(class)) {
    stop("`class` must be a single non-missing character string.", call. = FALSE)
  }
  if (!class %in% .sm_warning_classes_known) {
    stop("Unknown sitemix warning class: ", class, call. = FALSE)
  }

  unique(c(class, "sitemix_warning"))
}

.sm_condition_classes <- function(class) {
  if (grepl("^sitemix_warning", class)) {
    .sm_warning_classes(class)
  } else {
    .sm_error_classes(class)
  }
}

.sm_condition_data <- function(
  expected = NULL,
  actual = NULL,
  row_identity = NULL,
  location = NULL,
  fix = NULL
) {
  fields <- list(
    expected = expected,
    actual = actual,
    row_identity = row_identity,
    location = location,
    fix = fix
  )
  fields[!vapply(fields, is.null, logical(1))]
}

.sm_abort <- function(
  message,
  class,
  ...,
  expected = NULL,
  actual = NULL,
  row_identity = NULL,
  location = NULL,
  fix = NULL,
  body = NULL,
  footer = NULL,
  parent = NULL,
  call = rlang::caller_env()
) {
  if (is.null(body)) {
    body <- .sm_cli_condition_body(
      expected = expected,
      actual = actual,
      row_identity = row_identity,
      location = location,
      fix = fix
    )
  }

  args <- c(
    list(
      message = message,
      class = .sm_error_classes(class),
      body = body,
      footer = footer,
      parent = parent,
      call = call
    ),
    list(...),
    .sm_condition_data(
      expected = expected,
      actual = actual,
      row_identity = row_identity,
      location = location,
      fix = fix
    )
  )
  do.call(rlang::abort, args)
}

.sm_abort_input <- function(
  message,
  class = "sitemix_error_input",
  ...,
  call = rlang::caller_env()
) {
  .sm_abort(message, class = class, ..., call = call)
}

.sm_abort_argument <- function(
  message,
  class = "sitemix_error_argument",
  ...,
  call = rlang::caller_env()
) {
  .sm_abort(message, class = class, ..., call = call)
}

.sm_abort_estimate <- function(
  message,
  class = "sitemix_error_estimate",
  ...,
  call = rlang::caller_env()
) {
  .sm_abort(message, class = class, ..., call = call)
}

.sm_abort_vcov <- function(
  message,
  class = "sitemix_error_vcov",
  ...,
  call = rlang::caller_env()
) {
  .sm_abort(message, class = class, ..., call = call)
}

.sm_abort_aggregate <- function(
  message,
  class = "sitemix_error_aggregate",
  ...,
  call = rlang::caller_env()
) {
  .sm_abort(message, class = class, ..., call = call)
}

.sm_warn <- function(
  message,
  class,
  ...,
  expected = NULL,
  actual = NULL,
  row_identity = NULL,
  location = NULL,
  fix = NULL,
  body = NULL,
  footer = NULL,
  call = rlang::caller_env()
) {
  if (is.null(body)) {
    body <- .sm_cli_condition_body(
      expected = expected,
      actual = actual,
      row_identity = row_identity,
      location = location,
      fix = fix
    )
  }

  args <- c(
    list(
      message = message,
      class = .sm_warning_classes(class),
      body = body,
      footer = footer,
      call = call
    ),
    list(...),
    .sm_condition_data(
      expected = expected,
      actual = actual,
      row_identity = row_identity,
      location = location,
      fix = fix
    )
  )
  do.call(rlang::warn, args)
}
