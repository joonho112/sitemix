# sitemix_estimates class ---------------------------------------------------

.sm_sitemix_columns <- c(
  "site_id",
  "year",
  "indicator",
  "theta_raw",
  "theta_hat",
  "se_raw",
  "se",
  "n",
  "n_eff",
  "estimate_scale",
  "transform",
  "var_method",
  "flag_small_n",
  "flag_zero_cell",
  "input_mode",
  "flag_suppressed",
  "framing",
  "flag_below_accountability"
)

.sm_sitemix_optional_columns <- c(
  "V",
  "K",
  "se_smoothed",
  "se_raw_smoothed",
  "var_method_smoothed",
  "se_pre_smoothing",
  "se_raw_pre_smoothing",
  "residual_log_var",
  "population_size",
  "sampling_fraction",
  "fpc_variance_multiplier",
  "fpc_se_multiplier",
  "variance_multiplier_applied",
  "se_multiplier_applied",
  "sampling_design",
  "variance_rule",
  "estimate_status",
  "sensitivity_probability",
  "sensitivity_var_raw",
  "sensitivity_var",
  "sensitivity_n",
  "sensitivity_method",
  "sensitivity_acknowledged"
)

.sm_sitemix_reserved_extra_names <- c(
  "matrix",
  "indicator_order",
  "family",
  "vcov_method",
  "vcov_scale",
  "matrix_boundary_rule",
  "scalar_correction_rule",
  "psd_repair",
  "matrix_rank",
  "positive_support",
  "n_jt",
  "diag_contract",
  "description",
  "sitemix_role",
  "aggregate_case",
  "sampling_relation",
  "denominator_pattern",
  "d1_regime",
  "d1_regime_by_group",
  "suppression",
  "smoothing"
)

.sm_sitemix_estimates <- function(
  rows,
  description = NULL,
  family = NULL,
  sitemix_role = "summary_uncertainty"
) {
  rows <- tibble::as_tibble(rows)
  attr(rows, "description") <- description
  attr(rows, "family") <- family
  attr(rows, "sitemix_role") <- sitemix_role
  class(rows) <- c("sitemix_estimates", setdiff(class(rows), "sitemix_estimates"))
  validate.sitemix_estimates(rows)
  rows
}

validate.sitemix_estimates <- function(x) {
  if (!inherits(x, "sitemix_estimates")) {
    .sm_abort_estimate(
      "`x` must be a `sitemix_estimates` object.",
      class = "sitemix_error_estimate_var_method",
      expected = "sitemix_estimates",
      actual = paste(class(x), collapse = "/"),
      fix = "Construct outputs with `.sm_sitemix_estimates()`."
    )
  }

  .sm_validate_sitemix_columns(x)
  .sm_validate_sitemix_types(x)
  .sm_validate_sitemix_lexicons(x)
  .sm_validate_sitemix_identity(x)
  .sm_validate_sitemix_reproducibility(x)
  .sm_validate_sitemix_vcov(x)
  .sm_validate_sitemix_attributes(x)

  invisible(TRUE)
}

#' @noRd
#' @export
format.sitemix_estimates <- function(x, ...) {
  paste0("<sitemix_estimates[", nrow(x), " x ", ncol(x), "]>")
}

#' @noRd
#' @export
print.sitemix_estimates <- function(x, ...) {
  description <- attr(x, "description", exact = TRUE)
  family <- attr(x, "family", exact = TRUE)
  role <- attr(x, "sitemix_role", exact = TRUE)
  header <- paste0("sitemix_estimates: ", nrow(x), " rows x ", ncol(x), " columns")
  if (!is.null(family)) {
    header <- paste0(header, " | family=", family)
  }
  if (!is.null(role)) {
    header <- paste0(header, " | role=", role)
  }
  if (!is.null(description)) {
    header <- paste0(header, " | ", description)
  }
  cat(header, "\n", sep = "")
  cat(
    "groups=",
    length(unique(paste(x$site_id, x$year, sep = "\r"))),
    " sites=",
    length(unique(x$site_id)),
    " years=",
    length(unique(x$year)),
    " indicators=",
    length(unique(x$indicator)),
    " V=",
    "V" %in% names(x),
    " K=",
    "K" %in% names(x),
    "\n",
    sep = ""
  )
  print(tibble::as_tibble(x), ...)
  invisible(x)
}

#' @noRd
#' @export
`[.sitemix_estimates` <- function(x, i, j, ..., drop = FALSE) {
  out <- NextMethod("[")
  .sm_restore_sitemix_subset(out, template = x)
}

#' Restore the output contract after optional dplyr verbs
#'
#' This method intentionally has no runtime dependency on dplyr. Registering
#' the S3 method lets dplyr restore a valid `sitemix_estimates` subclass when
#' dplyr is installed, while the implementation remains ordinary tibble code.
#'
#' @param data Reconstructed data returned by a dplyr verb.
#' @param template Original `sitemix_estimates` object.
#' @keywords internal
#' @noRd
#' @exportS3Method dplyr::dplyr_reconstruct
dplyr_reconstruct.sitemix_estimates <- function(data, template) {
  .sm_restore_sitemix_subset(tibble::as_tibble(data), template = template)
}

.sm_restore_sitemix_subset <- function(data, template) {
  if (!.sm_has_locked_sitemix_schema(data)) {
    return(.sm_drop_sitemix_contract(data, template = template))
  }

  data <- .sm_copy_sitemix_contract_attributes(data, template = template)
  class(data) <- c(
    "sitemix_estimates",
    setdiff(class(data), "sitemix_estimates")
  )
  data <- .sm_realign_subset_vcov(data)
  data <- .sm_refresh_subset_provenance(data, template = template)
  validate.sitemix_estimates(data)
  data
}

.sm_has_locked_sitemix_schema <- function(x) {
  names_x <- names(x)
  length(names_x) >= length(.sm_sitemix_columns) &&
    identical(
      names_x[seq_along(.sm_sitemix_columns)],
      .sm_sitemix_columns
    )
}

.sm_drop_sitemix_contract <- function(x, template) {
  out <- tibble::as_tibble(x)
  contract_attributes <- setdiff(
    names(attributes(template)),
    c("names", "row.names", "class", "groups")
  )
  for (attribute in contract_attributes) {
    attr(out, attribute) <- NULL
  }
  class(out) <- c("tbl_df", "tbl", "data.frame")
  out
}

.sm_copy_sitemix_contract_attributes <- function(x, template) {
  contract_attributes <- setdiff(
    names(attributes(template)),
    c("names", "row.names", "class", "groups")
  )
  for (attribute in contract_attributes) {
    attr(x, attribute) <- attr(template, attribute, exact = TRUE)
  }
  x
}

.sm_realign_subset_vcov <- function(x) {
  if (!"V" %in% names(x) || nrow(x) == 0L) {
    return(x)
  }

  groups <- split(
    seq_len(nrow(x)),
    factor(
      paste(x$site_id, x$year, sep = "\r"),
      levels = unique(paste(x$site_id, x$year, sep = "\r"))
    )
  )
  for (idx in groups) {
    first <- x$V[[idx[[1L]]]]
    indicators <- x$indicator[idx]
    if (length(indicators) != length(first$indicator_order) ||
        !setequal(indicators, first$indicator_order)) {
      .sm_v_schema_error(
        "Cannot retain `sitemix_estimates` after selecting a partial covariance group.",
        expected = first$indicator_order,
        actual = indicators,
        row_identity = .sm_row_identity(x, idx[[1L]])
      )
    }
    reordered <- .sm_reorder_vcov_coordinates(first, indicators)
    x$V[idx] <- rep(list(reordered), length(idx))
    if ("K" %in% names(x)) {
      x$K[idx] <- rep(as.integer(length(indicators)), length(idx))
    }
  }
  x
}

.sm_reorder_vcov_coordinates <- function(x, indicators) {
  permutation <- match(indicators, x$indicator_order)
  if (anyNA(permutation) || length(permutation) != length(x$indicator_order)) {
    .sm_v_schema_error(
      "Covariance coordinates cannot be aligned to the selected row order.",
      expected = x$indicator_order,
      actual = indicators
    )
  }
  if (identical(permutation, seq_along(permutation))) {
    return(x)
  }

  out <- x
  out$matrix <- x$matrix[permutation, permutation, drop = FALSE]
  out$indicator_order <- indicators
  coordinate_fields <- c(
    "scalar_correction_rule",
    "sampling_fraction",
    "fpc_variance_multiplier",
    "fpc_se_multiplier",
    "variance_multiplier_applied",
    "se_multiplier_applied",
    "variance_rule"
  )
  for (field in coordinate_fields) {
    if (length(out[[field]]) == length(permutation)) {
      out[[field]] <- out[[field]][permutation]
    }
  }
  validate.sm_vcov(out)
  out
}

.sm_refresh_subset_provenance <- function(x, template) {
  x <- .sm_refresh_d1_subset_provenance(x)
  x <- .sm_refresh_suppression_subset_provenance(x)
  x <- .sm_refresh_smoothing_subset_provenance(x, template = template)
  x
}

.sm_refresh_d1_subset_provenance <- function(x) {
  by_group <- attr(x, "d1_regime_by_group", exact = TRUE)
  if (is.null(by_group) || !is.data.frame(by_group)) {
    return(x)
  }

  if (nrow(x) == 0L) {
    attr(x, "d1_regime_by_group") <- tibble::as_tibble(by_group[0, , drop = FALSE])
    return(x)
  }
  data_keys <- unique(paste(x$site_id, x$year, sep = "\r"))
  metadata_keys <- paste(by_group$site_id, by_group$year, sep = "\r")
  positions <- match(data_keys, metadata_keys)
  if (anyNA(positions)) {
    .sm_schema_error(
      "D1 group provenance cannot be aligned after row selection.",
      expected = metadata_keys,
      actual = data_keys
    )
  }
  by_group <- tibble::as_tibble(by_group[positions, , drop = FALSE])
  attr(x, "d1_regime_by_group") <- by_group
  attr(x, "denominator_pattern") <- .sm_subset_metadata_summary(
    by_group$denominator_pattern
  )
  attr(x, "d1_regime") <- .sm_subset_metadata_summary(by_group$d1_regime)
  x
}

.sm_subset_metadata_summary <- function(x) {
  values <- unique(as.character(x))
  if (length(values) == 1L) values[[1L]] else "mixed"
}

.sm_refresh_suppression_subset_provenance <- function(x) {
  suppression <- attr(x, "suppression", exact = TRUE)
  if (is.null(suppression) || !is.list(suppression)) {
    return(x)
  }

  n_suppressed <- as.integer(sum(x$flag_suppressed))
  has_sensitivity <- any(.sm_is_suppression_sensitivity_row(x))
  suppression$n_suppressed <- n_suppressed
  if (n_suppressed == 0L) {
    suppression$denominator_observed_on_suppressed <- TRUE
    suppression$has_hidden_denominator <- FALSE
  } else {
    drop <- .sm_is_suppressed_drop_row(x)
    sensitivity <- .sm_is_suppression_sensitivity_row(x)
    hidden_sensitivity <- sensitivity &
      x$sensitivity_method == "unquantified_hidden_denominator"
    if (!any(drop)) {
      # Sensitivity rows retain an explicit method, so observed versus hidden
      # denominator status is exactly recoverable after row selection.
      suppression$has_hidden_denominator <- any(hidden_sensitivity)
      suppression$denominator_observed_on_suppressed <- !any(hidden_sensitivity)
    } else {
      # A canonical suppressed-missing row does not retain row-level
      # denominator observability. Preserve the original flag conservatively,
      # while still incorporating any retained explicit hidden sensitivity.
      suppression$has_hidden_denominator <-
        any(hidden_sensitivity) || isTRUE(suppression$has_hidden_denominator)
      suppression$denominator_observed_on_suppressed <-
        !any(hidden_sensitivity) &&
        isTRUE(suppression$denominator_observed_on_suppressed)
    }
  }
  suppression$sensitivity_acknowledged <- has_sensitivity &&
    isTRUE(suppression$sensitivity_acknowledgement_requested)
  suppression$sensitivity_role <- if (has_sensitivity) {
    "nonidentified_variance_sensitivity"
  } else {
    "none"
  }
  attr(x, "suppression") <- suppression
  x
}

.sm_refresh_smoothing_subset_provenance <- function(x, template) {
  smoothing <- attr(x, "smoothing", exact = TRUE)
  if (is.null(smoothing) || !is.list(smoothing)) {
    return(x)
  }
  target_column <- smoothing$target_column
  if (!is.character(target_column) || length(target_column) != 1L ||
      !target_column %in% names(x)) {
    attr(x, "smoothing") <- NULL
    attr(x, "smoother_fit") <- NULL
    attr(x, "smoother_fit_summary") <- NULL
    return(x)
  }

  template_keys <- .sm_sitemix_row_keys(template)
  data_keys <- .sm_sitemix_row_keys(x)
  eligible_keys <- template_keys[smoothing$eligible_rows]
  eligible <- data_keys %in% eligible_keys
  smoothing$eligible_rows <- which(eligible)
  smoothing$n_eligible <- as.integer(sum(eligible))
  smoothing$v <- .sm_smoothing_v_fact(x, scale = smoothing$scale, eligible = eligible)
  attr(x, "smoothing") <- smoothing

  summary <- attr(x, "smoother_fit_summary", exact = TRUE)
  if (is.data.frame(summary) && nrow(summary) == 1L) {
    summary$v_present <- smoothing$v$present
    summary$v_relation <- smoothing$v$relation
    summary$v_matching_rows <- as.integer(length(smoothing$v$matching_rows))
    summary$v_incompatible_rows <- as.integer(length(smoothing$v$incompatible_rows))
    attr(x, "smoother_fit_summary") <- summary
  }
  x
}

.sm_sitemix_row_keys <- function(x) {
  paste(x$site_id, x$year, x$indicator, sep = "\r")
}

.sm_validate_sitemix_columns <- function(x) {
  names_x <- names(x)
  if (anyDuplicated(names_x)) {
    .sm_abort_estimate(
      "The `sitemix_estimates` object must use unique column names.",
      class = "sitemix_error_estimate_var_method",
      expected = "unique column names",
      actual = names_x[duplicated(names_x)],
      fix = "Rename duplicate audit or schema columns before reconstruction."
    )
  }
  missing <- setdiff(.sm_sitemix_columns, names_x)
  if (length(missing) > 0L) {
    .sm_abort_estimate(
      "The `sitemix_estimates` object is missing required columns.",
      class = "sitemix_error_estimate_var_method",
      expected = .sm_sitemix_columns,
      actual = names_x,
      fix = paste0("Add missing columns: ", .sm_cli_collapse(missing, quote = TRUE), ".")
    )
  }

  first_columns <- names_x[seq_along(.sm_sitemix_columns)]
  if (!identical(first_columns, .sm_sitemix_columns)) {
    .sm_abort_estimate(
      "The default `sitemix_estimates` columns are out of order.",
      class = "sitemix_error_estimate_var_method",
      expected = .sm_sitemix_columns,
      actual = first_columns,
      fix = "Emit the 18 locked default columns before optional `V` and `K`."
    )
  }

  # Unknown columns after the locked schema are user/audit payload and do not
  # participate in the statistical contract. Reserved optional names are still
  # fully validated by their dedicated validators below.
  extras <- setdiff(names_x, c(.sm_sitemix_columns, .sm_sitemix_optional_columns))
  reserved_conflicts <- intersect(extras, .sm_sitemix_reserved_extra_names)
  if (length(reserved_conflicts) > 0L) {
    .sm_abort_estimate(
      "The `sitemix_estimates` object uses reserved names as audit columns.",
      class = "sitemix_error_estimate_var_method",
      expected = "non-reserved audit column names",
      actual = reserved_conflicts,
      fix = "Rename audit columns so they do not shadow covariance or provenance fields."
    )
  }
  if ("K" %in% names_x && !"V" %in% names_x) {
    .sm_abort_estimate(
      "`K` may only be emitted alongside the `V` list-column.",
      class = "sitemix_error_estimate_vcov_invariant",
      expected = "both `V` and `K`",
      actual = "`K` without `V`",
      fix = "Emit `K` only for matrix-bearing output."
    )
  }

  invisible(TRUE)
}

.sm_validate_sitemix_types <- function(x) {
  .sm_col_type(x$site_id, "site_id", "character")
  .sm_col_type(x$year, "year", "integer")
  .sm_col_type(x$indicator, "indicator", "character")
  .sm_col_type(x$theta_raw, "theta_raw", "numeric", allow_na = TRUE)
  .sm_col_type(x$theta_hat, "theta_hat", "numeric", allow_na = TRUE)
  .sm_col_type(x$se_raw, "se_raw", "numeric", allow_na = TRUE)
  .sm_col_type(x$se, "se", "numeric", allow_na = TRUE)
  .sm_col_type(x$n, "n", "integer")
  .sm_col_type(x$n_eff, "n_eff", "numeric")
  .sm_col_type(x$estimate_scale, "estimate_scale", "character")
  .sm_col_type(x$transform, "transform", "character")
  .sm_col_type(x$var_method, "var_method", "character")
  .sm_col_type(x$flag_small_n, "flag_small_n", "logical")
  .sm_col_type(x$flag_zero_cell, "flag_zero_cell", "logical", allow_na = TRUE)
  .sm_col_type(x$input_mode, "input_mode", "character")
  .sm_col_type(x$flag_suppressed, "flag_suppressed", "logical")
  .sm_col_type(x$framing, "framing", "character", allow_na = TRUE)
  .sm_col_type(x$flag_below_accountability, "flag_below_accountability", "logical")

  if (anyNA(x$site_id) || any(x$site_id == "")) {
    .sm_schema_error("`site_id` must contain non-missing site identifiers.")
  }
  if (anyNA(x$year)) {
    .sm_schema_error("`year` must not contain missing values.")
  }
  if (anyNA(x$indicator) || any(x$indicator == "")) {
    .sm_schema_error("`indicator` must contain non-missing labels.")
  }
  if (anyNA(x$n) || any(x$n <= 0L)) {
    .sm_schema_error("`n` must contain positive integers.")
  }
  if (anyNA(x$n_eff) || any(!is.finite(x$n_eff)) || any(x$n_eff <= 0)) {
    .sm_schema_error("`n_eff` must contain positive finite values.")
  }
  suppressed_unavailable <- .sm_is_suppressed_unavailable_row(x)
  if (anyNA(x$flag_small_n) ||
      anyNA(x$flag_suppressed) ||
      anyNA(x$flag_below_accountability) ||
      anyNA(x$flag_zero_cell[!suppressed_unavailable])) {
    .sm_schema_error("Flag columns must not contain missing values.")
  }
  if (any(suppressed_unavailable & !is.na(x$flag_zero_cell))) {
    .sm_schema_error(
      "`flag_zero_cell` must be NA on unavailable suppression rows.",
      expected = "NA on suppressed-missing and suppression-sensitivity rows",
      actual = unique(as.character(x$flag_zero_cell[suppressed_unavailable]))
    )
  }

  .sm_validate_estimate_numeric_columns(x)
  .sm_validate_suppression_sensitivity_columns(x)
  .sm_validate_smoothing_columns(x)
  .sm_validate_fpc_columns(x)
  invisible(TRUE)
}

.sm_validate_fpc_columns <- function(x, tol = 1e-10) {
  columns <- c(
    "population_size",
    "sampling_fraction",
    "fpc_variance_multiplier",
    "fpc_se_multiplier",
    "variance_multiplier_applied",
    "se_multiplier_applied",
    "sampling_design",
    "variance_rule"
  )
  present <- columns %in% names(x)
  if (!any(present)) {
    return(invisible(TRUE))
  }
  if (!all(present)) {
    .sm_schema_error(
      "Finite-population provenance columns must be emitted as one complete set.",
      expected = columns,
      actual = columns[present]
    )
  }

  for (column in columns[seq_len(6L)]) {
    .sm_col_type(x[[column]], column, "numeric")
  }
  .sm_col_type(x$sampling_design, "sampling_design", "character")
  .sm_col_type(x$variance_rule, "variance_rule", "character")
  if (any(!is.finite(x$population_size)) ||
      any(x$population_size < 1) ||
      any(x$population_size != floor(x$population_size)) ||
      any(x$population_size < x$n)) {
    .sm_schema_error(
      "`population_size` must contain whole finite values at least as large as `n`.",
      expected = "whole population_size >= n",
      actual = x$population_size
    )
  }
  if (any(x$flag_suppressed)) {
    .sm_schema_error(
      "Finite-population SRSWOR provenance currently requires rows with no suppression audit state.",
      expected = "fully observed rows",
      actual = "suppressed rows present"
    )
  }
  expected_q <- .sm_fpc_variance_multiplier(x$n, x$population_size)
  expected_fraction <- x$n / x$population_size
  if (!isTRUE(all.equal(x$sampling_fraction, expected_fraction, tolerance = tol, check.attributes = FALSE)) ||
      !isTRUE(all.equal(x$fpc_variance_multiplier, expected_q, tolerance = tol, check.attributes = FALSE)) ||
      !isTRUE(all.equal(x$fpc_se_multiplier, sqrt(expected_q), tolerance = tol, check.attributes = FALSE))) {
    .sm_schema_error(
      "Finite-population multiplier provenance is not reproducible from `n` and `population_size`.",
      expected = "sampling_fraction = n/N and q = (N-n)/(N-1), with census q = 0",
      actual = "inconsistent finite-population provenance"
    )
  }
  .sm_validate_lexicon(x$sampling_design, "sampling_design", "SRSWOR")
  .sm_validate_lexicon(x$variance_rule, "variance_rule", c("plugin", "design_corrected"))
  base_method <- .sm_strip_smoothing_var_method(x$var_method)
  corrected <- grepl("binomial_bc$", base_method)
  expected_rule <- rep("plugin", length(corrected))
  expected_rule[corrected] <- "design_corrected"
  if (!identical(x$variance_rule, expected_rule)) {
    .sm_schema_error(
      "`variance_rule` is inconsistent with scalar variance provenance.",
      expected = expected_rule,
      actual = x$variance_rule
    )
  }
  expected_applied <- expected_q
  if (any(corrected)) {
    expected_applied[corrected] <- .sm_fpc_design_variance_multiplier(
      x$n[corrected],
      x$population_size[corrected]
    )
  }
  if (!isTRUE(all.equal(x$variance_multiplier_applied, expected_applied, tolerance = tol, check.attributes = FALSE)) ||
      !isTRUE(all.equal(x$se_multiplier_applied, sqrt(expected_applied), tolerance = tol, check.attributes = FALSE))) {
    .sm_schema_error(
      "Applied finite-population multipliers are inconsistent with `variance_rule`.",
      expected = "q for plug-in rows and (N-n)/N for design-corrected rows",
      actual = "inconsistent applied multiplier provenance"
    )
  }
  invisible(TRUE)
}

.sm_validate_smoothing_columns <- function(x) {
  suppressed_drop <- .sm_is_suppressed_drop_row(x)
  positive_optional <- c(
    "se_smoothed",
    "se_raw_smoothed",
    "se_pre_smoothing",
    "se_raw_pre_smoothing"
  )
  for (column in intersect(positive_optional, names(x))) {
    .sm_col_type(x[[column]], column, "numeric", allow_na = TRUE)
    retained <- !suppressed_drop
    if (any(retained) && (anyNA(x[[column]][retained]) || any(!is.finite(x[[column]][retained])) || any(x[[column]][retained] < 0))) {
      .sm_schema_error(
        paste0("`", column, "` must contain finite non-negative values on retained rows."),
        expected = paste0(column, " >= 0 on retained rows"),
        actual = "missing, non-finite, or negative values"
      )
    }
    if (any(suppressed_drop) && any(!is.na(x[[column]][suppressed_drop]))) {
      .sm_schema_error(
        paste0("`", column, "` must be NA on suppressed-drop rows."),
        expected = "NA on suppressed-drop rows",
        actual = "finite values"
      )
    }
  }
  if ("var_method_smoothed" %in% names(x)) {
    .sm_col_type(x$var_method_smoothed, "var_method_smoothed", "character")
    .sm_validate_var_method(x$var_method_smoothed)
  }
  if ("residual_log_var" %in% names(x)) {
    .sm_col_type(x$residual_log_var, "residual_log_var", "numeric", allow_na = TRUE)
    if (any(is.infinite(x$residual_log_var))) {
      .sm_schema_error(
        "`residual_log_var` must not contain infinite values.",
        expected = "finite values or NA",
        actual = "infinite values"
      )
    }
  }
  invisible(TRUE)
}

.sm_col_type <- function(x, column, type, allow_na = FALSE) {
  ok <- switch(
    type,
    character = is.character(x),
    integer = is.integer(x),
    numeric = is.numeric(x),
    logical = is.logical(x),
    FALSE
  )
  if (!ok || (!allow_na && anyNA(x))) {
    .sm_schema_error(
      paste0("`", column, "` has an invalid type or missing value."),
      expected = type,
      actual = paste(class(x), collapse = "/")
    )
  }
  invisible(TRUE)
}

.sm_validate_estimate_numeric_columns <- function(x) {
  numeric_cols <- c("theta_raw", "theta_hat", "se_raw", "se")
  suppressed_drop <- .sm_is_suppressed_drop_row(x)
  suppressed_sensitivity <- .sm_is_suppression_sensitivity_row(x)
  suppressed_unavailable <- suppressed_drop | suppressed_sensitivity
  suppressed_method <- x$var_method %in% c("suppressed_drop", "suppression_sensitivity")

  if (any(x$flag_suppressed & !suppressed_method)) {
    .sm_schema_error(
      "Suppressed output rows must use a suppression-specific `var_method`.",
      expected = c("suppressed_drop", "suppression_sensitivity"),
      actual = unique(x$var_method[x$flag_suppressed])
    )
  }

  if (any(suppressed_drop & (!x$flag_suppressed | !identical(x$input_mode[suppressed_drop], rep("aggregate", sum(suppressed_drop)))))) {
    .sm_schema_error(
      "`suppressed_drop` rows must be aggregate rows with `flag_suppressed = TRUE`.",
      expected = "aggregate suppressed rows",
      actual = "non-aggregate or unsuppressed suppressed_drop row"
    )
  }
  if (any(suppressed_sensitivity & (!x$flag_suppressed | !identical(x$input_mode[suppressed_sensitivity], rep("aggregate", sum(suppressed_sensitivity)))))) {
    .sm_schema_error(
      "`suppression_sensitivity` rows must be aggregate rows with `flag_suppressed = TRUE`.",
      expected = "aggregate suppressed rows",
      actual = "non-aggregate or unsuppressed suppression_sensitivity row"
    )
  }
  if (any(suppressed_sensitivity & !identical(x$estimate_scale[suppressed_sensitivity], rep("arcsine", sum(suppressed_sensitivity))))) {
    .sm_schema_error(
      "`suppression_sensitivity` rows currently require `estimate_scale = \"arcsine\"`.",
      expected = "arcsine",
      actual = unique(x$estimate_scale[suppressed_sensitivity])
    )
  }

  for (column in numeric_cols) {
    value <- x[[column]]
    if (any(suppressed_unavailable)) {
      if (any(!is.na(value[suppressed_unavailable]))) {
        .sm_schema_error(
          "Suppressed-missing and suppression-sensitivity rows must carry NA canonical estimate and SE columns.",
          expected = "NA",
          actual = paste0("finite values in `", column, "`")
        )
      }
    }
    retained <- !suppressed_unavailable
    if (any(retained) && (anyNA(value[retained]) || any(!is.finite(value[retained])))) {
      .sm_schema_error(
        paste0("`", column, "` must contain finite values."),
        expected = "finite numeric values",
        actual = "NA, NaN, or Inf present"
      )
    }
  }
  retained <- !suppressed_unavailable
  if (any(retained) && any(x$theta_raw[retained] < 0 | x$theta_raw[retained] > 1)) {
    .sm_schema_error("`theta_raw` must be in [0, 1].", expected = "[0, 1]", actual = paste(range(x$theta_raw[retained]), collapse = " to "))
  }
  if (any(retained) && any(x$se_raw[retained] < 0 | x$se[retained] < 0)) {
    .sm_schema_error("Standard errors must be non-negative.", expected = "se_raw >= 0 and se >= 0")
  }
  invisible(TRUE)
}

.sm_is_suppressed_drop_row <- function(x) {
  x$flag_suppressed & .sm_value_in(x$var_method, "suppressed_drop")
}

.sm_is_suppressed_upper_bound_row <- function(x) {
  .sm_is_suppression_sensitivity_row(x)
}

.sm_is_suppression_sensitivity_row <- function(x) {
  x$flag_suppressed & .sm_value_in(x$var_method, "suppression_sensitivity")
}

.sm_is_suppressed_unavailable_row <- function(x) {
  .sm_is_suppressed_drop_row(x) | .sm_is_suppression_sensitivity_row(x)
}

.sm_validate_suppression_sensitivity_columns <- function(x, tol = 1e-12) {
  columns <- c(
    "estimate_status",
    "sensitivity_probability",
    "sensitivity_var_raw",
    "sensitivity_var",
    "sensitivity_n",
    "sensitivity_method",
    "sensitivity_acknowledged"
  )
  present <- columns %in% names(x)
  if (!any(present)) {
    if (any(x$flag_suppressed)) {
      .sm_schema_error(
        "Aggregate suppression rows require explicit estimate-status and sensitivity provenance.",
        expected = columns,
        actual = "suppression provenance columns absent"
      )
    }
    return(invisible(TRUE))
  }
  if (!all(present)) {
    .sm_schema_error(
      "Suppression provenance columns must be emitted as one complete set.",
      expected = columns,
      actual = columns[present]
    )
  }
  .sm_col_type(x$estimate_status, "estimate_status", "character")
  .sm_col_type(x$sensitivity_probability, "sensitivity_probability", "numeric", allow_na = TRUE)
  .sm_col_type(x$sensitivity_var_raw, "sensitivity_var_raw", "numeric", allow_na = TRUE)
  .sm_col_type(x$sensitivity_var, "sensitivity_var", "numeric", allow_na = TRUE)
  .sm_col_type(x$sensitivity_n, "sensitivity_n", "integer", allow_na = TRUE)
  .sm_col_type(x$sensitivity_method, "sensitivity_method", "character", allow_na = TRUE)
  .sm_col_type(x$sensitivity_acknowledged, "sensitivity_acknowledged", "logical")
  .sm_validate_lexicon(
    x$estimate_status,
    "estimate_status",
    c("identified", "suppressed_missing", "suppression_sensitivity")
  )
  .sm_validate_lexicon(
    x$sensitivity_method,
    "sensitivity_method",
    c("worst_case_variance_observed_n", "unquantified_hidden_denominator"),
    allow_na = TRUE
  )

  drop <- .sm_is_suppressed_drop_row(x)
  sensitivity <- .sm_is_suppression_sensitivity_row(x)
  identified <- !drop & !sensitivity
  expected_status <- rep("identified", length(drop))
  expected_status[drop] <- "suppressed_missing"
  expected_status[sensitivity] <- "suppression_sensitivity"
  if (!identical(x$estimate_status, expected_status)) {
    .sm_schema_error(
      "`estimate_status` is inconsistent with suppression provenance.",
      expected = expected_status,
      actual = x$estimate_status
    )
  }
  if (any(x$sensitivity_acknowledged != sensitivity)) {
    .sm_schema_error(
      "Only suppression-sensitivity rows may carry sensitivity acknowledgement.",
      expected = sensitivity,
      actual = x$sensitivity_acknowledged
    )
  }
  non_sensitivity <- identified | drop
  sensitivity_fields <- c("sensitivity_probability", "sensitivity_var_raw", "sensitivity_var", "sensitivity_n", "sensitivity_method")
  for (column in sensitivity_fields) {
    if (any(!is.na(x[[column]][non_sensitivity]))) {
      .sm_schema_error(
        "Sensitivity fields must be missing outside suppression-sensitivity rows.",
        expected = paste0(column, " = NA outside sensitivity rows"),
        actual = column
      )
    }
  }
  if (any(sensitivity)) {
    if (!identical(x$sensitivity_probability[sensitivity], rep(0.5, sum(sensitivity)))) {
      .sm_schema_error(
        "Suppression variance sensitivity must use the Bernoulli worst-case probability 0.5.",
        expected = "sensitivity_probability = 0.5",
        actual = x$sensitivity_probability[sensitivity]
      )
    }
    if (anyNA(x$sensitivity_method[sensitivity])) {
      .sm_schema_error(
        "Every suppression-sensitivity row must declare one complete sensitivity method.",
        expected = c("worst_case_variance_observed_n", "unquantified_hidden_denominator"),
        actual = x$sensitivity_method[sensitivity]
      )
    }
    observed <- sensitivity & x$sensitivity_method == "worst_case_variance_observed_n"
    hidden <- sensitivity & x$sensitivity_method == "unquantified_hidden_denominator"
    if (any(observed)) {
      expected_var <- 0.25 / x$sensitivity_n[observed]
      if (anyNA(x$sensitivity_n[observed]) || any(x$sensitivity_n[observed] <= 0L) ||
          !identical(x$sensitivity_n[observed], x$n[observed]) ||
          !isTRUE(all.equal(x$sensitivity_var_raw[observed], expected_var, tolerance = tol, check.attributes = FALSE)) ||
          !isTRUE(all.equal(x$sensitivity_var[observed], expected_var, tolerance = tol, check.attributes = FALSE))) {
        .sm_schema_error(
          "Observed-denominator sensitivity variance is not reproducible.",
          expected = "sensitivity_n = n and sensitivity_var_raw = sensitivity_var = 0.25 / sensitivity_n",
          actual = "inconsistent observed-denominator sensitivity fields"
        )
      }
    }
    if (any(hidden) &&
        (any(!is.na(x$sensitivity_n[hidden])) ||
         any(!is.na(x$sensitivity_var_raw[hidden])) ||
         any(!is.na(x$sensitivity_var[hidden])))) {
      .sm_schema_error(
        "Hidden-denominator sensitivity rows must not make a numeric variance claim.",
        expected = "missing sensitivity_n and sensitivity variances",
        actual = "numeric hidden-denominator sensitivity fields"
      )
    }
  }
  invisible(TRUE)
}

.sm_value_in <- function(x, value) {
  !is.na(x) & x %in% value
}

.sm_validate_sitemix_lexicons <- function(x) {
  valid_scale <- c("none", "arcsine", "arcsine_anscombe", "logit")
  .sm_validate_lexicon(x$estimate_scale, "estimate_scale", valid_scale)
  .sm_validate_lexicon(x$transform, "transform", valid_scale)
  .sm_validate_var_method(x$var_method)
  .sm_validate_lexicon(x$input_mode, "input_mode", c("student_level", "counts_full_suff", "aggregate"))
  .sm_validate_lexicon(x$framing, "framing", c("subgroup_as_site", "subgroup_as_indicator"), allow_na = TRUE)

  if (!identical(x$transform, x$estimate_scale)) {
    .sm_schema_error(
      "`transform` must exactly match `estimate_scale`.",
      expected = x$estimate_scale,
      actual = x$transform
    )
  }

  invisible(TRUE)
}

.sm_validate_lexicon <- function(x, column, valid, allow_na = FALSE) {
  ok <- x %in% valid
  if (allow_na) {
    ok <- ok | is.na(x)
  }
  if (anyNA(ok) || any(!ok)) {
    .sm_schema_error(
      paste0("`", column, "` contains unsupported values."),
      expected = valid,
      actual = unique(as.character(x[!ok | is.na(ok)]))
    )
  }
  invisible(TRUE)
}

.sm_validate_sitemix_identity <- function(x) {
  key <- paste(x$site_id, x$year, x$indicator, sep = "\r")
  if (anyDuplicated(key)) {
    dup <- which(duplicated(key))[1]
    .sm_schema_error(
      "`sitemix_estimates` rows must be unique by `(site_id, year, indicator)`.",
      expected = "unique row identity",
      actual = paste(x$site_id[[dup]], x$year[[dup]], x$indicator[[dup]], sep = " / ")
    )
  }
  invisible(TRUE)
}

.sm_validate_sitemix_reproducibility <- function(x, tol = 1e-8) {
  for (i in seq_len(nrow(x))) {
    if (.sm_is_suppressed_unavailable_row(x)[[i]]) {
      next
    }
    scale <- x$estimate_scale[[i]]
    expected <- .sm_expected_theta_se(x, i)
    if (!isTRUE(all.equal(x$theta_hat[[i]], expected$theta_hat, tolerance = tol, check.attributes = FALSE))) {
      .sm_schema_error(
        "`theta_hat` is not reproducible from the declared scale.",
        expected = expected$theta_hat,
        actual = x$theta_hat[[i]],
        row_identity = .sm_row_identity(x, i)
      )
    }
    observed_se <- x$se[[i]]
    if (.sm_var_method_has_smoothing_suffix(x$var_method[[i]]) &&
        "se_pre_smoothing" %in% names(x) &&
        !is.na(x$se_pre_smoothing[[i]])) {
      observed_se <- x$se_pre_smoothing[[i]]
    }
    if (!isTRUE(all.equal(observed_se, expected$se, tolerance = tol, check.attributes = FALSE))) {
      .sm_schema_error(
        "`se` is not reproducible from the declared scale and method.",
        expected = expected$se,
        actual = observed_se,
        row_identity = .sm_row_identity(x, i)
      )
    }

    expected_method <- .sm_expected_var_method(x, i)
    base_method <- .sm_strip_smoothing_var_method(x$var_method[[i]])
    if (!base_method %in% expected_method) {
      .sm_schema_error(
        "`var_method` is not compatible with `estimate_scale`.",
        expected = expected_method,
        actual = x$var_method[[i]],
        row_identity = .sm_row_identity(x, i)
      )
    }

    if (identical(scale, "logit") && (x$theta_raw[[i]] <= 0 || x$theta_raw[[i]] >= 1)) {
      .sm_schema_error(
        "Logit-scale rows require interior `theta_raw`.",
        expected = "0 < theta_raw < 1",
        actual = x$theta_raw[[i]],
        row_identity = .sm_row_identity(x, i)
      )
    }
  }
  invisible(TRUE)
}

.sm_strip_smoothing_var_method <- function(var_method) {
  sub(" \\+ (fh|gvf)_smooth_(gam|loglinear)$", "", var_method)
}

.sm_var_method_has_smoothing_suffix <- function(var_method) {
  grepl(" \\+ (fh|gvf)_smooth_(gam|loglinear)$", var_method)
}

.sm_expected_theta_se <- function(x, i) {
  scale <- x$estimate_scale[[i]]
  theta_raw <- x$theta_raw[[i]]
  n <- x$n[[i]]
  n_eff <- x$n_eff[[i]]
  method <- .sm_strip_smoothing_var_method(x$var_method[[i]])
  fpc <- if ("population_size" %in% names(x)) x$population_size[[i]] else NULL
  fpc_se <- .sm_fpc_multiplier(n, fpc = fpc)

  if (identical(scale, "arcsine")) {
    se <- if (identical(method, "arcsine_delta_binomial_bc")) {
      .sm_arcsine_bc_delta_se(theta_raw, n) *
        sqrt(.sm_fpc_design_variance_multiplier(n, fpc = fpc))
    } else {
      .sm_arcsine_se(n_eff) * fpc_se
    }
    list(theta_hat = asin(sqrt(theta_raw)), se = se)
  } else if (identical(scale, "arcsine_anscombe")) {
    n_raw <- n_eff - 0.5
    p_anscombe <- (theta_raw * n_raw + 3 / 8) / (n_raw + 3 / 4)
    list(theta_hat = asin(sqrt(p_anscombe)), se = .sm_arcsine_se(n_eff) * fpc_se)
  } else if (identical(scale, "logit")) {
    se <- if (identical(method, "logit_delta_binomial_bc")) {
      .sm_logit_bc_delta_se(theta_raw, n) *
        sqrt(.sm_fpc_design_variance_multiplier(n, fpc = fpc))
    } else {
      .sm_logit_delta_se(theta_raw, n) * fpc_se
    }
    list(theta_hat = log(theta_raw / (1 - theta_raw)), se = se)
  } else {
    se_raw <- x$se_raw[[i]]
    if (.sm_var_method_has_smoothing_suffix(x$var_method[[i]]) &&
        "se_raw_pre_smoothing" %in% names(x) &&
        !is.na(x$se_raw_pre_smoothing[[i]])) {
      se_raw <- x$se_raw_pre_smoothing[[i]]
    }
    list(theta_hat = theta_raw, se = se_raw)
  }
}

.sm_expected_var_method <- function(x, i) {
  if (isTRUE(x$flag_suppressed[[i]]) && identical(x$var_method[[i]], "suppression_sensitivity")) {
    return("suppression_sensitivity")
  }
  if (isTRUE(x$flag_suppressed[[i]]) && identical(x$var_method[[i]], "suppressed_drop")) {
    return("suppressed_drop")
  }
  switch(
    x$estimate_scale[[i]],
    arcsine = c("arcsine_vst", "arcsine_delta_binomial_bc"),
    arcsine_anscombe = "arcsine_anscombe",
    logit = c("logit_delta", "logit_delta_binomial_bc"),
    none = c(
      "binomial",
      "binomial_bc",
      "wilson_boundary_surrogate",
      "agresti_coull_boundary_surrogate"
    )
  )
}

.sm_validate_sitemix_vcov <- function(x) {
  has_v <- "V" %in% names(x)
  has_k <- "K" %in% names(x)

  if (!has_v) {
    return(invisible(TRUE))
  }
  if (any(.sm_is_suppression_sensitivity_row(x))) {
    .sm_v_schema_error(
      "Suppression-sensitivity rows are excluded from ordinary covariance construction.",
      expected = "no suppression_sensitivity row when V is present",
      actual = "suppression sensitivity with V"
    )
  }
  if (!is.list(x$V) || length(x$V) != nrow(x)) {
    .sm_v_schema_error("`V` must be a list-column with one entry per row.")
  }
  for (i in seq_along(x$V)) {
    if (!inherits(x$V[[i]], "sm_vcov")) {
      .sm_v_schema_error(
        "`V` entries must be `sm_vcov` objects.",
        actual = paste(class(x$V[[i]]), collapse = "/"),
        row_identity = .sm_row_identity(x, i)
      )
    }
  }

  family <- attr(x, "family", exact = TRUE)
  if (!is.character(family) || length(family) != 1L || is.na(family)) {
    .sm_v_schema_error(
      "Matrix-bearing `sitemix_estimates` require one family attribute.",
      expected = c("binomial", "multivariate", "multinomial"),
      actual = as.character(family)
    )
  }
  .sm_validate_output_vcov_alignment(x, family = family)

  if (has_k) {
    .sm_col_type(x$K, "K", "integer")
    if (anyNA(x$K) || any(x$K <= 0L)) {
      .sm_v_schema_error("`K` must contain positive integers.")
    }
    v_dims <- vapply(x$V, function(v) nrow(v$matrix), integer(1))
    if (!identical(x$K, v_dims)) {
      .sm_v_schema_error(
        "`K` must match the dimension of each `sm_vcov` object.",
        expected = v_dims,
        actual = x$K
      )
    }
  }

  .sm_validate_repeated_v(x, has_k = has_k)
  .sm_validate_sitemix_sur_contract(x, family = family)
  .sm_validate_sitemix_multinomial_contract(x, family = family, has_k = has_k)
  invisible(TRUE)
}

.sm_validate_sitemix_sur_contract <- function(x, family) {
  if (!identical(family, "multivariate")) {
    return(invisible(TRUE))
  }

  groups <- split(seq_len(nrow(x)), paste(x$site_id, x$year, sep = "\r"))
  for (idx in groups) {
    # Scenario D1 also has family="multivariate", but its identified input
    # provenance is aggregate and its matrix is working independence. Scenario
    # B is the non-aggregate multivariate path and must retain the SUR contract
    # even when every repeated V object is tampered in the same way.
    if (all(x$input_mode[idx] == "aggregate")) {
      next
    }
    if (any(x$input_mode[idx] == "aggregate")) {
      .sm_v_schema_error(
        "A multivariate covariance group cannot mix aggregate and jointly observed input modes.",
        expected = "one input mode per site-year group",
        actual = unique(x$input_mode[idx]),
        row_identity = .sm_row_identity(x, idx[[1L]])
      )
    }

    v <- x$V[[idx[[1L]]]]
    fixed_contract <- c(
      vcov_method = "sur",
      vcov_scale = "raw",
      diag_contract = "row_se_raw_squared"
    )
    actual_contract <- c(
      vcov_method = v$vcov_method,
      vcov_scale = v$vcov_scale,
      diag_contract = v$diag_contract
    )
    if (!identical(actual_contract, fixed_contract)) {
      .sm_v_schema_error(
        "Scenario B package output must retain its SUR/raw/row-se-raw covariance contract.",
        expected = fixed_contract,
        actual = actual_contract,
        row_identity = .sm_row_identity(x, idx[[1L]])
      )
    }

    group_n <- unique(x$n[idx])
    group_n_eff <- unique(x$n_eff[idx])
    if (length(group_n) != 1L || !identical(v$n_jt, as.integer(group_n[[1L]]))) {
      .sm_v_schema_error(
        "Scenario B covariance `n_jt` must match the common row denominator.",
        expected = if (length(group_n) == 1L) as.integer(group_n[[1L]]) else group_n,
        actual = v$n_jt,
        row_identity = .sm_row_identity(x, idx[[1L]])
      )
    }
    if (length(group_n_eff) != 1L ||
        !isTRUE(all.equal(v$n_eff, as.numeric(group_n_eff[[1L]]), tolerance = 1e-12, check.attributes = FALSE))) {
      .sm_v_schema_error(
        "Scenario B covariance `n_eff` must match the common row transform denominator.",
        expected = group_n_eff,
        actual = v$n_eff,
        row_identity = .sm_row_identity(x, idx[[1L]])
      )
    }

    actual_scalar <- unname(v$scalar_correction_rule)
    base_method <- .sm_strip_smoothing_var_method(x$var_method[idx])
    expected_scalar <- rep("none", length(idx))
    corrected <- base_method %in% c(
      "binomial_bc",
      "arcsine_delta_binomial_bc",
      "logit_delta_binomial_bc"
    )
    expected_scalar[corrected] <- "binomial_bc"
    explicit_wilson <- base_method == "wilson_boundary_surrogate"
    expected_scalar[explicit_wilson] <- "wilson_boundary_surrogate"
    explicit_ac <- base_method == "agresti_coull_boundary_surrogate"
    if (any(explicit_ac)) {
      .sm_v_schema_error(
        "Scenario B matrix output cannot carry Agresti-Coull boundary provenance.",
        expected = "Wilson, none, or an interior binomial correction",
        actual = base_method[explicit_ac],
        row_identity = .sm_row_identity(x, idx[which(explicit_ac)[[1L]]])
      )
    }

    transformed_boundary <- x$estimate_scale[idx] != "none" &
      x$flag_zero_cell[idx] & !corrected
    noncensus <- rep(TRUE, length(idx))
    if ("population_size" %in% names(x)) {
      noncensus <- x$population_size[idx] != x$n[idx]
    }
    positive_boundary <- transformed_boundary & x$se_raw[idx] > 0
    zero_noncensus_boundary <- transformed_boundary & x$se_raw[idx] == 0 & noncensus
    expected_scalar[positive_boundary] <- "wilson_boundary_surrogate"
    expected_scalar[zero_noncensus_boundary] <- "none"

    # A transformed boundary in a zero-uncertainty census cannot distinguish
    # Wilson from no regularization using row numerics alone. Both remain legal,
    # but the scalar and matrix boundary labels must still agree below.
    ambiguous_census <- transformed_boundary & x$se_raw[idx] == 0 & !noncensus
    comparable <- !ambiguous_census
    if (any(actual_scalar[comparable] != expected_scalar[comparable])) {
      first <- which(comparable & actual_scalar != expected_scalar)[[1L]]
      .sm_v_schema_error(
        "Scenario B scalar correction provenance is inconsistent with its row companion.",
        expected = expected_scalar[[first]],
        actual = actual_scalar[[first]],
        row_identity = .sm_row_identity(x, idx[[first]])
      )
    }
    if (any(ambiguous_census & !actual_scalar %in% c("none", "wilson_boundary_surrogate"))) {
      first <- which(ambiguous_census & !actual_scalar %in% c("none", "wilson_boundary_surrogate"))[[1L]]
      .sm_v_schema_error(
        "Scenario B census-boundary provenance must remain Wilson or none.",
        expected = c("none", "wilson_boundary_surrogate"),
        actual = actual_scalar[[first]],
        row_identity = .sm_row_identity(x, idx[[first]])
      )
    }

    expected_boundary_rule <- if (any(actual_scalar == "wilson_boundary_surrogate")) {
      "diagonal_boundary_floor"
    } else {
      "none"
    }
    if (!identical(v$matrix_boundary_rule, expected_boundary_rule)) {
      .sm_v_schema_error(
        "Scenario B matrix boundary provenance must agree with its scalar Wilson rules.",
        expected = expected_boundary_rule,
        actual = v$matrix_boundary_rule,
        row_identity = .sm_row_identity(x, idx[[1L]])
      )
    }
  }

  invisible(TRUE)
}

.sm_validate_sitemix_multinomial_contract <- function(x, family, has_k) {
  if (!identical(family, "multinomial")) {
    return(invisible(TRUE))
  }
  if (!isTRUE(has_k)) {
    .sm_v_schema_error(
      "Scenario C package output requires the full-simplex `K` column.",
      expected = "K present",
      actual = "K absent"
    )
  }

  groups <- split(seq_len(nrow(x)), paste(x$site_id, x$year, sep = "\r"))
  for (idx in groups) {
    v <- x$V[[idx[[1L]]]]
    identity <- .sm_row_identity(x, idx[[1L]])

    fixed_contract <- c(
      vcov_method = "multinomial",
      vcov_scale = "raw",
      psd_repair = "none"
    )
    actual_contract <- c(
      vcov_method = v$vcov_method,
      vcov_scale = v$vcov_scale,
      psd_repair = v$psd_repair
    )
    if (!identical(actual_contract, fixed_contract)) {
      .sm_v_schema_error(
        "Scenario C package output must retain its multinomial/raw/no-repair covariance contract.",
        expected = fixed_contract,
        actual = actual_contract,
        row_identity = identity
      )
    }

    group_n <- unique(x$n[idx])
    group_n_eff <- unique(x$n_eff[idx])
    if (length(group_n) != 1L ||
        !identical(v$n_jt, as.integer(group_n[[1L]]))) {
      .sm_v_schema_error(
        "Scenario C covariance `n_jt` must match the common row denominator.",
        expected = if (length(group_n) == 1L) as.integer(group_n[[1L]]) else group_n,
        actual = v$n_jt,
        row_identity = identity
      )
    }
    if (length(group_n_eff) != 1L ||
        !isTRUE(all.equal(
          v$n_eff,
          as.numeric(group_n_eff[[1L]]),
          tolerance = 1e-12,
          check.attributes = FALSE
        ))) {
      .sm_v_schema_error(
        "Scenario C covariance `n_eff` must match the common row transform denominator.",
        expected = group_n_eff,
        actual = v$n_eff,
        row_identity = identity
      )
    }

    theta <- x$theta_raw[idx]
    if (!isTRUE(all.equal(sum(theta), 1, tolerance = 1e-12))) {
      .sm_v_schema_error(
        "Scenario C row points must sum to one within each site-year.",
        expected = 1,
        actual = sum(theta),
        row_identity = identity
      )
    }
    expected_zero_cell <- theta == 0 | theta == 1
    if (!identical(x$flag_zero_cell[idx], expected_zero_cell)) {
      .sm_v_schema_error(
        "Scenario C zero-cell flags must match the observed simplex points.",
        expected = expected_zero_cell,
        actual = x$flag_zero_cell[idx],
        row_identity = identity
      )
    }
    positive_support <- as.integer(sum(theta > 0))
    analytic_rank <- as.integer(positive_support - 1L)
    if (!identical(v$positive_support, positive_support) ||
        !identical(v$matrix_rank, analytic_rank)) {
      .sm_v_schema_error(
        "Scenario C support and analytic rank metadata must match the row points.",
        expected = c(
          positive_support = positive_support,
          matrix_rank = analytic_rank
        ),
        actual = c(
          positive_support = v$positive_support,
          matrix_rank = v$matrix_rank
        ),
        row_identity = identity
      )
    }

    base_method <- .sm_strip_smoothing_var_method(x$var_method[idx])
    expected_scalar <- rep("none", length(idx))
    corrected <- base_method %in% c(
      "binomial_bc",
      "arcsine_delta_binomial_bc",
      "logit_delta_binomial_bc"
    )
    expected_scalar[corrected] <- "binomial_bc"
    explicit_wilson <- base_method == "wilson_boundary_surrogate"
    expected_scalar[explicit_wilson] <- "wilson_boundary_surrogate"
    explicit_ac <- base_method == "agresti_coull_boundary_surrogate"
    if (any(explicit_ac)) {
      first <- which(explicit_ac)[[1L]]
      .sm_v_schema_error(
        "Scenario C matrix output cannot carry Agresti-Coull boundary provenance.",
        expected = "Wilson, none, or an interior binomial correction",
        actual = base_method[[first]],
        row_identity = .sm_row_identity(x, idx[[first]])
      )
    }

    transformed_boundary <- x$estimate_scale[idx] != "none" &
      x$flag_zero_cell[idx] & !corrected
    noncensus <- rep(TRUE, length(idx))
    if ("population_size" %in% names(x)) {
      noncensus <- x$population_size[idx] != x$n[idx]
    }
    positive_boundary <- transformed_boundary & x$se_raw[idx] > 0
    zero_noncensus_boundary <- transformed_boundary &
      x$se_raw[idx] == 0 & noncensus
    expected_scalar[positive_boundary] <- "wilson_boundary_surrogate"
    expected_scalar[zero_noncensus_boundary] <- "none"

    actual_scalar <- unname(v$scalar_correction_rule)
    ambiguous_census <- transformed_boundary &
      x$se_raw[idx] == 0 & !noncensus
    comparable <- !ambiguous_census
    if (any(actual_scalar[comparable] != expected_scalar[comparable])) {
      first <- which(comparable & actual_scalar != expected_scalar)[[1L]]
      .sm_v_schema_error(
        "Scenario C scalar correction provenance is inconsistent with its row companion.",
        expected = expected_scalar[[first]],
        actual = actual_scalar[[first]],
        row_identity = .sm_row_identity(x, idx[[first]])
      )
    }
    if (any(ambiguous_census &
        !actual_scalar %in% c("none", "wilson_boundary_surrogate"))) {
      first <- which(
        ambiguous_census &
          !actual_scalar %in% c("none", "wilson_boundary_surrogate")
      )[[1L]]
      .sm_v_schema_error(
        "Scenario C census-boundary provenance must remain Wilson or none.",
        expected = c("none", "wilson_boundary_surrogate"),
        actual = actual_scalar[[first]],
        row_identity = .sm_row_identity(x, idx[[first]])
      )
    }

    has_wilson <- any(actual_scalar == "wilson_boundary_surrogate")
    expected_boundary_rule <- if (has_wilson) "simplex_preserve" else "none"
    expected_diag_contract <- if (has_wilson) {
      "row_se_raw_squared_except_boundary_surrogates"
    } else {
      "row_se_raw_squared"
    }
    if (!identical(v$matrix_boundary_rule, expected_boundary_rule) ||
        !identical(v$diag_contract, expected_diag_contract)) {
      .sm_v_schema_error(
        "Scenario C boundary and diagonal contracts must agree with scalar provenance.",
        expected = c(
          matrix_boundary_rule = expected_boundary_rule,
          diag_contract = expected_diag_contract
        ),
        actual = c(
          matrix_boundary_rule = v$matrix_boundary_rule,
          diag_contract = v$diag_contract
        ),
        row_identity = identity
      )
    }

    matrix_rule <- unique(v$variance_rule)
    if (length(matrix_rule) != 1L) {
      .sm_v_schema_error(
        "Scenario C uses one global whole-matrix variance rule.",
        expected = "uniform plugin or design_corrected",
        actual = v$variance_rule,
        row_identity = identity
      )
    }
    # With at least one interior coordinate, row provenance identifies the
    # global matrix rule. In the all-boundary support-one case, a zero matrix
    # cannot distinguish plugin from design-corrected construction.
    has_interior <- any(!x$flag_zero_cell[idx])
    if (has_interior) {
      expected_matrix_rule <- if (any(corrected)) {
        "design_corrected"
      } else {
        "plugin"
      }
      if (!identical(matrix_rule, expected_matrix_rule)) {
        .sm_v_schema_error(
          "Scenario C whole-matrix variance provenance is inconsistent with interior rows.",
          expected = expected_matrix_rule,
          actual = matrix_rule,
          row_identity = identity
        )
      }
    }

    applied <- v$variance_multiplier_applied
    if (length(unique(applied)) != 1L) {
      .sm_v_schema_error(
        "Scenario C applies one multiplier to the full simplex matrix.",
        expected = "uniform variance_multiplier_applied",
        actual = applied,
        row_identity = identity
      )
    }
    kernel <- diag(theta, nrow = length(theta)) - tcrossprod(theta)
    dimnames(kernel) <- list(x$indicator[idx], x$indicator[idx])
    n <- group_n[[1L]]
    expected_matrix <- if (identical(matrix_rule, "design_corrected")) {
      if (n == 1 && isTRUE(all.equal(applied[[1L]], 0))) {
        kernel * 0
      } else {
        kernel / (n - 1) * applied[[1L]]
      }
    } else {
      kernel / n * applied[[1L]]
    }
    if (!isTRUE(all.equal(
      v$matrix,
      expected_matrix,
      tolerance = 1e-10,
      check.attributes = TRUE
    ))) {
      .sm_v_schema_error(
        "Scenario C covariance matrix is inconsistent with its rows and global rule.",
        expected = "full-simplex plug-in or design-corrected matrix",
        actual = "matrix values differ",
        row_identity = identity
      )
    }
  }

  invisible(TRUE)
}

.sm_validate_repeated_v <- function(x, has_k) {
  groups <- split(seq_len(nrow(x)), paste(x$site_id, x$year, sep = "\r"))
  for (idx in groups) {
    first <- x$V[[idx[[1]]]]
    if (!identical(first$indicator_order, x$indicator[idx])) {
      .sm_v_schema_error(
        "`V` indicator order must match row order within each `(site_id, year)` group.",
        expected = first$indicator_order,
        actual = x$indicator[idx],
        row_identity = .sm_row_identity(x, idx[[1]])
      )
    }
    for (row in idx[-1]) {
      if (!.sm_vcov_value_equal(first, x$V[[row]])) {
        .sm_v_schema_error(
          "`V` must be value-equal within each `(site_id, year)` group.",
          row_identity = .sm_row_identity(x, row)
        )
      }
    }
    if (has_k && length(unique(x$K[idx])) != 1L) {
      .sm_v_schema_error(
        "`K` must be constant within each `(site_id, year)` group.",
        row_identity = .sm_row_identity(x, idx[[1]])
      )
    }
    if (has_k && !identical(x$K[[idx[[1]]]], length(idx))) {
      .sm_v_schema_error(
        "`K` must equal the number of rows in each `(site_id, year)` group.",
        expected = length(idx),
        actual = x$K[[idx[[1]]]],
        row_identity = .sm_row_identity(x, idx[[1]])
      )
    }
  }
  invisible(TRUE)
}

.sm_vcov_value_equal <- function(x, y) {
  if (!inherits(x, "sm_vcov") || !inherits(y, "sm_vcov")) {
    return(FALSE)
  }
  if (identical(x, y)) {
    return(TRUE)
  }

  fields <- c(
    "site_id",
    "year",
    "indicator_order",
    "family",
    "vcov_method",
    "estimate_scale",
    "vcov_scale",
    "matrix_boundary_rule",
    "scalar_correction_rule",
    "psd_repair",
    "matrix_rank",
    "positive_support",
    "n_jt",
    "n_eff",
    "population_size",
    "sampling_fraction",
    "fpc_variance_multiplier",
    "fpc_se_multiplier",
    "variance_multiplier_applied",
    "se_multiplier_applied",
    "sampling_design",
    "variance_rule",
    "diag_contract"
  )

  isTRUE(all.equal(x$matrix, y$matrix, tolerance = 1e-12, check.attributes = TRUE)) &&
    all(vapply(fields, function(field) identical(x[[field]], y[[field]]), logical(1)))
}

.sm_validate_sitemix_attributes <- function(x) {
  family <- attr(x, "family", exact = TRUE)
  if (!is.null(family) && (!is.character(family) || length(family) != 1L || is.na(family) || !family %in% c("binomial", "multivariate", "multinomial"))) {
    .sm_schema_error(
      "`family` attribute must be NULL or a valid family label.",
      expected = c("binomial", "multivariate", "multinomial"),
      actual = as.character(family)
    )
  }

  description <- attr(x, "description", exact = TRUE)
  if (!is.null(description) && (!is.character(description) || length(description) != 1L || is.na(description))) {
    .sm_schema_error(
      "`description` attribute must be NULL or a single string.",
      expected = "NULL or one character string",
      actual = paste(class(description), collapse = "/")
    )
  }

  role <- attr(x, "sitemix_role", exact = TRUE)
  if (!is.null(role) && (!is.character(role) || length(role) != 1L || is.na(role) || !role %in% c("summary_uncertainty", "descriptive"))) {
    .sm_schema_error(
      "`sitemix_role` attribute must be NULL or a valid role label.",
      expected = c("summary_uncertainty", "descriptive"),
      actual = as.character(role)
    )
  }

  suppression <- attr(x, "suppression", exact = TRUE)
  if (!is.null(suppression)) {
    if (!is.list(suppression) ||
        !is.logical(suppression$sensitivity_acknowledgement_requested) ||
        length(suppression$sensitivity_acknowledgement_requested) != 1L ||
        is.na(suppression$sensitivity_acknowledgement_requested) ||
        !is.logical(suppression$sensitivity_acknowledged) ||
        length(suppression$sensitivity_acknowledged) != 1L ||
        is.na(suppression$sensitivity_acknowledged) ||
        !is.character(suppression$sensitivity_role) ||
        length(suppression$sensitivity_role) != 1L ||
        is.na(suppression$sensitivity_role)) {
      .sm_schema_error(
        "The `suppression` attribute must carry complete sensitivity acknowledgement and role provenance.",
        expected = c("sensitivity_acknowledgement_requested", "sensitivity_acknowledged", "sensitivity_role"),
        actual = suppression
      )
    }
    has_sensitivity <- any(.sm_is_suppression_sensitivity_row(x))
    expected_acknowledged <- has_sensitivity && isTRUE(suppression$sensitivity_acknowledgement_requested)
    expected_role <- if (has_sensitivity) "nonidentified_variance_sensitivity" else "none"
    if (!identical(suppression$sensitivity_acknowledged, expected_acknowledged) ||
        !identical(suppression$sensitivity_role, expected_role)) {
      .sm_schema_error(
        "The `suppression` attribute is inconsistent with row-level sensitivity provenance.",
        expected = list(sensitivity_acknowledged = expected_acknowledged, sensitivity_role = expected_role),
        actual = list(
          sensitivity_acknowledged = suppression$sensitivity_acknowledged,
          sensitivity_role = suppression$sensitivity_role
        )
      )
    }
  }

  invisible(TRUE)
}

.sm_schema_error <- function(message, expected = NULL, actual = NULL, row_identity = NULL) {
  .sm_abort_estimate(
    message,
    class = "sitemix_error_estimate_var_method",
    expected = expected,
    actual = actual,
    row_identity = row_identity
  )
}

.sm_v_schema_error <- function(message, expected = NULL, actual = NULL, row_identity = NULL) {
  .sm_abort_estimate(
    message,
    class = "sitemix_error_estimate_vcov_invariant",
    expected = expected,
    actual = actual,
    row_identity = row_identity
  )
}

.sm_row_identity <- function(x, i) {
  list(
    site_id = x$site_id[[i]],
    year = x$year[[i]],
    indicator = x$indicator[[i]]
  )
}
