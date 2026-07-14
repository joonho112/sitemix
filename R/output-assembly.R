# Output assembly helpers ---------------------------------------------------

.sm_one_row <- function(
  site_id,
  year,
  indicator,
  theta_raw,
  se_raw,
  n,
  C = NULL,
  estimate_scale = "arcsine",
  var_method_raw = NULL,
  n_eff = NULL,
  min_n = 10L,
  accountability_n = 30L,
  flag_small_n = NULL,
  flag_zero_cell = NULL,
  input_mode = "student_level",
  flag_suppressed = FALSE,
  framing = NA_character_,
  flag_below_accountability = NULL,
  V = NULL,
  K = NULL,
  fpc = NULL
) {
  site_id <- .sm_output_chr_scalar(site_id, "site_id")
  indicator <- .sm_output_chr_scalar(indicator, "indicator")
  year <- .sm_output_int_scalar(year, "year")
  n <- .sm_output_int_scalar(n, "n", positive = TRUE)
  theta_raw <- .sm_output_num_scalar(theta_raw, "theta_raw")
  se_raw <- .sm_output_num_scalar(se_raw, "se_raw")
  if (se_raw < 0) {
    .sm_output_error("`se_raw` must be non-negative.", expected = "se_raw >= 0", actual = se_raw)
  }

  estimate_scale <- .sm_output_chr_scalar(estimate_scale, "estimate_scale")
  input_mode <- .sm_output_chr_scalar(input_mode, "input_mode")
  framing <- .sm_output_chr_scalar(framing, "framing", allow_na = TRUE)
  flag_suppressed <- .sm_output_lgl_scalar(flag_suppressed, "flag_suppressed")

  if (is.null(n_eff)) {
    n_eff <- if (identical(estimate_scale, "arcsine_anscombe")) .sm_anscombe_n_eff(n) else as.numeric(n)
  } else {
    n_eff <- .sm_output_num_scalar(n_eff, "n_eff", positive = TRUE)
  }

  transform <- .sm_compute_transform(
    theta_raw = theta_raw,
    n = n,
    C = C,
    estimate_scale = estimate_scale
  )
  se_info <- .sm_transformed_se(
    theta_raw = theta_raw,
    n = n,
    n_eff = n_eff,
    estimate_scale = estimate_scale,
    se_raw = se_raw,
    var_method_raw = var_method_raw,
    fpc = fpc
  )

  if (is.null(flag_small_n)) {
    min_n <- .sm_output_int_scalar(min_n, "min_n", positive = TRUE)
    flag_small_n <- n < min_n
  } else {
    flag_small_n <- .sm_output_lgl_scalar(flag_small_n, "flag_small_n")
  }

  if (is.null(flag_zero_cell)) {
    flag_zero_cell <- .sm_default_zero_cell(theta_raw = theta_raw, C = C, n = n)
  } else {
    flag_zero_cell <- .sm_output_lgl_scalar(flag_zero_cell, "flag_zero_cell")
  }

  if (is.null(flag_below_accountability)) {
    flag_below_accountability <- if (is.null(accountability_n)) {
      FALSE
    } else {
      accountability_n <- .sm_output_int_scalar(accountability_n, "accountability_n", positive = TRUE)
      n < accountability_n
    }
  } else {
    flag_below_accountability <- .sm_output_lgl_scalar(flag_below_accountability, "flag_below_accountability")
  }

  row <- tibble::tibble(
    site_id = site_id,
    year = year,
    indicator = indicator,
    theta_raw = theta_raw,
    theta_hat = transform$theta_hat,
    se_raw = se_raw,
    se = se_info$se,
    n = n,
    n_eff = as.numeric(n_eff),
    estimate_scale = estimate_scale,
    transform = transform$transform,
    var_method = se_info$var_method,
    flag_small_n = flag_small_n,
    flag_zero_cell = flag_zero_cell,
    input_mode = input_mode,
    flag_suppressed = flag_suppressed,
    framing = framing,
    flag_below_accountability = flag_below_accountability
  )

  if (!is.null(V)) {
    row$V <- list(V)
  }
  if (!is.null(K)) {
    row$K <- .sm_output_int_scalar(K, "K", positive = TRUE)
  }
  fpc_columns <- .sm_fpc_row_columns(
    n = n,
    fpc = fpc,
    var_method_raw = var_method_raw
  )
  if (!is.null(fpc_columns)) {
    for (column in names(fpc_columns)) {
      row[[column]] <- fpc_columns[[column]]
    }
  }

  row
}

.sm_bc_category_rows <- function(
  site_id,
  year,
  indicator_order,
  scalar,
  n,
  n_eff,
  estimate_scale,
  min_n,
  accountability_n,
  input_mode,
  V,
  K,
  fpc
) {
  rows <- vector("list", length(indicator_order))
  for (k in seq_along(indicator_order)) {
    rows[[k]] <- .sm_one_row(
      site_id = site_id,
      year = year,
      indicator = indicator_order[[k]],
      theta_raw = scalar$theta_raw[[k]],
      se_raw = scalar$se_raw[[k]],
      n = n,
      C = scalar$C[[k]],
      estimate_scale = estimate_scale,
      var_method_raw = scalar$var_method_raw[[k]],
      n_eff = n_eff,
      min_n = min_n,
      accountability_n = accountability_n,
      flag_small_n = n < min_n,
      flag_zero_cell = scalar$flag_zero_cell[[k]],
      input_mode = input_mode,
      flag_suppressed = FALSE,
      framing = NA_character_,
      V = V,
      K = K,
      fpc = fpc
    )
  }
  rows
}

.sm_bind_sitemix_rows <- function(
  rows,
  description = NULL,
  family = NULL,
  sitemix_role = "summary_uncertainty"
) {
  rows <- .sm_normalize_row_list(rows)
  has_v <- vapply(rows, function(row) "V" %in% names(row), logical(1))
  has_k <- vapply(rows, function(row) "K" %in% names(row), logical(1))

  if (any(has_v) && !all(has_v)) {
    .sm_output_v_error(
      "Output rows must either all carry `V` or none carry `V`.",
      expected = "uniform optional `V` list-column",
      actual = has_v
    )
  }
  if (any(has_k) && !all(has_k)) {
    .sm_output_v_error(
      "Output rows must either all carry `K` or none carry `K`.",
      expected = "uniform optional `K` column",
      actual = has_k
    )
  }
  if (any(has_k) && !any(has_v)) {
    .sm_output_v_error(
      "`K` may only be emitted alongside `V`.",
      expected = "both `V` and `K`",
      actual = "`K` without `V`"
    )
  }

  bound <- do.call(vctrs::vec_rbind, rows)
  bound <- tibble::as_tibble(bound)
  .sm_validate_output_vcov_alignment(bound, family = family)

  .sm_sitemix_estimates(
    bound,
    description = description,
    family = family,
    sitemix_role = sitemix_role
  )
}

.sm_suppressed_drop_row <- function(
  site_id,
  year,
  indicator,
  n,
  n_eff = NULL,
  estimate_scale = "arcsine",
  min_n = 10L,
  accountability_n = 30L,
  framing = NA_character_
) {
  site_id <- .sm_output_chr_scalar(site_id, "site_id")
  indicator <- .sm_output_chr_scalar(indicator, "indicator")
  year <- .sm_output_int_scalar(year, "year")
  n <- .sm_output_int_scalar(n, "n", positive = TRUE)
  estimate_scale <- .sm_output_chr_scalar(estimate_scale, "estimate_scale")
  framing <- .sm_output_chr_scalar(framing, "framing", allow_na = TRUE)
  min_n <- .sm_output_int_scalar(min_n, "min_n", positive = TRUE)
  accountability_n <- .sm_output_int_scalar(accountability_n, "accountability_n", positive = TRUE)
  if (is.null(n_eff)) {
    n_eff <- as.numeric(n)
  } else {
    n_eff <- .sm_output_num_scalar(n_eff, "n_eff", positive = TRUE)
  }

  tibble::tibble(
    site_id = site_id,
    year = year,
    indicator = indicator,
    theta_raw = NA_real_,
    theta_hat = NA_real_,
    se_raw = NA_real_,
    se = NA_real_,
    n = n,
    n_eff = as.numeric(n_eff),
    estimate_scale = estimate_scale,
    transform = estimate_scale,
    var_method = "suppressed_drop",
    flag_small_n = n < min_n,
    flag_zero_cell = NA,
    input_mode = "aggregate",
    flag_suppressed = TRUE,
    framing = framing,
    flag_below_accountability = n < accountability_n
  )
}

.sm_suppressed_upper_bound_row <- function(
  site_id,
  year,
  indicator,
  n,
  suppressed_theta_hat,
  denominator_observed = TRUE,
  min_n = 10L,
  accountability_n = 30L,
  framing = NA_character_
) {
  site_id <- .sm_output_chr_scalar(site_id, "site_id")
  indicator <- .sm_output_chr_scalar(indicator, "indicator")
  year <- .sm_output_int_scalar(year, "year")
  n <- .sm_output_int_scalar(n, "n", positive = TRUE)
  framing <- .sm_output_chr_scalar(framing, "framing", allow_na = TRUE)
  .sm_validate_suppressed_theta_hat(suppressed_theta_hat)
  denominator_observed <- .sm_output_lgl_scalar(denominator_observed, "denominator_observed")
  min_n <- .sm_output_int_scalar(min_n, "min_n", positive = TRUE)
  accountability_n <- .sm_output_int_scalar(accountability_n, "accountability_n", positive = TRUE)

  sensitivity_probability <- as.numeric(suppressed_theta_hat)
  sensitivity_var_raw <- if (isTRUE(denominator_observed)) {
    sensitivity_probability * (1 - sensitivity_probability) / n
  } else {
    NA_real_
  }
  tibble::tibble(
    site_id = site_id,
    year = year,
    indicator = indicator,
    theta_raw = NA_real_,
    theta_hat = NA_real_,
    se_raw = NA_real_,
    se = NA_real_,
    n = n,
    n_eff = as.numeric(n),
    estimate_scale = "arcsine",
    transform = "arcsine",
    var_method = "suppression_sensitivity",
    flag_small_n = n < min_n,
    flag_zero_cell = NA,
    input_mode = "aggregate",
    flag_suppressed = TRUE,
    framing = framing,
    flag_below_accountability = n < accountability_n,
    estimate_status = "suppression_sensitivity",
    sensitivity_probability = sensitivity_probability,
    sensitivity_var_raw = sensitivity_var_raw,
    sensitivity_var = sensitivity_var_raw,
    sensitivity_n = if (isTRUE(denominator_observed)) as.integer(n) else NA_integer_,
    sensitivity_method = if (isTRUE(denominator_observed)) {
      "worst_case_variance_observed_n"
    } else {
      "unquantified_hidden_denominator"
    },
    sensitivity_acknowledged = TRUE
  )
}

.sm_add_aggregate_suppression_provenance <- function(x) {
  n_rows <- nrow(x)
  if (!"estimate_status" %in% names(x)) {
    x$estimate_status <- rep("identified", n_rows)
  }
  drop <- .sm_is_suppressed_drop_row(x)
  sensitivity <- .sm_is_suppression_sensitivity_row(x)
  x$estimate_status[!drop & !sensitivity] <- "identified"
  x$estimate_status[drop] <- "suppressed_missing"
  x$estimate_status[sensitivity] <- "suppression_sensitivity"

  numeric_fields <- c("sensitivity_probability", "sensitivity_var_raw", "sensitivity_var")
  for (column in numeric_fields) {
    if (!column %in% names(x)) {
      x[[column]] <- rep(NA_real_, n_rows)
    }
  }
  if (!"sensitivity_n" %in% names(x)) {
    x$sensitivity_n <- rep(NA_integer_, n_rows)
  }
  if (!"sensitivity_method" %in% names(x)) {
    x$sensitivity_method <- rep(NA_character_, n_rows)
  }
  if (!"sensitivity_acknowledged" %in% names(x)) {
    x$sensitivity_acknowledged <- rep(FALSE, n_rows)
  }
  x$sensitivity_acknowledged[!sensitivity] <- FALSE
  x
}

.sm_compute_transform <- function(theta_raw, n, C, estimate_scale) {
  if (identical(estimate_scale, "arcsine_anscombe")) {
    .sm_transform_probability(theta_raw, n, C = C, vst = "arcsine", anscombe = TRUE)
  } else if (identical(estimate_scale, "arcsine")) {
    .sm_transform_probability(theta_raw, n, C = C, vst = "arcsine", anscombe = FALSE)
  } else if (identical(estimate_scale, "logit")) {
    .sm_transform_probability(theta_raw, n, C = C, vst = "logit", anscombe = FALSE)
  } else if (identical(estimate_scale, "none")) {
    .sm_transform_probability(theta_raw, n, C = C, vst = "none", anscombe = FALSE)
  } else {
    .sm_output_error(
      "`estimate_scale` is not supported.",
      expected = c("none", "arcsine", "arcsine_anscombe", "logit"),
      actual = estimate_scale
    )
  }
}

.sm_default_zero_cell <- function(theta_raw, C, n) {
  if (!is.null(C)) {
    counts <- .sm_check_counts(C, n)
    return(counts$C == 0 | counts$C == counts$n)
  }
  theta_raw == 0 || theta_raw == 1
}

.sm_normalize_row_list <- function(rows) {
  if (inherits(rows, "data.frame")) {
    rows <- list(rows)
  }
  if (!is.list(rows) || length(rows) == 0L) {
    .sm_output_error(
      "`rows` must be a non-empty list of output rows.",
      expected = "non-empty list of data frames",
      actual = paste(class(rows), collapse = "/")
    )
  }
  for (i in seq_along(rows)) {
    if (!inherits(rows[[i]], "data.frame")) {
      .sm_output_error(
        "`rows` must contain only data frames or tibbles.",
        expected = "data.frame",
        actual = paste(class(rows[[i]]), collapse = "/")
      )
    }
    rows[[i]] <- tibble::as_tibble(rows[[i]])
  }
  rows
}

.sm_validate_output_vcov_alignment <- function(rows, family = NULL) {
  if (!"V" %in% names(rows)) {
    return(invisible(TRUE))
  }

  if (!is.list(rows$V)) {
    .sm_output_v_error("`V` must be a list-column.", expected = "list", actual = paste(class(rows$V), collapse = "/"))
  }

  group_key <- paste(rows$site_id, rows$year, sep = "\r")
  first_index <- match(group_key, group_key)
  for (i in seq_len(nrow(rows))) {
    v <- rows$V[[i]]
    if (!inherits(v, "sm_vcov")) {
      .sm_output_v_error(
        "`V` entries must be `sm_vcov` objects.",
        expected = "sm_vcov",
        actual = paste(class(v), collapse = "/"),
        row_identity = .sm_row_identity(rows, i)
      )
    }
    first <- rows$V[[first_index[[i]]]]
    reusable <- !identical(first_index[[i]], i) &&
      identical(first, v) &&
      tryCatch(
        .sm_vcov_value_equal(first, v),
        error = function(...) FALSE
      )
    if (!reusable) {
      validate.sm_vcov(v)
    }
    if (!identical(v$site_id, rows$site_id[[i]])) {
      .sm_output_v_error("`V$site_id` must match the output row.", expected = rows$site_id[[i]], actual = v$site_id, row_identity = .sm_row_identity(rows, i))
    }
    if (!identical(v$year, rows$year[[i]])) {
      .sm_output_v_error("`V$year` must match the output row.", expected = rows$year[[i]], actual = v$year, row_identity = .sm_row_identity(rows, i))
    }
    if (!is.null(family) && !identical(v$family, family)) {
      .sm_output_v_error("`V$family` must match the output family.", expected = family, actual = v$family, row_identity = .sm_row_identity(rows, i))
    }
    if (!identical(v$estimate_scale, rows$estimate_scale[[i]])) {
      .sm_output_v_error("`V$estimate_scale` must match the output row.", expected = rows$estimate_scale[[i]], actual = v$estimate_scale, row_identity = .sm_row_identity(rows, i))
    }
    .sm_validate_output_vcov_diagonal(rows, i = i, v = v)
    .sm_validate_output_vcov_design(rows, i = i, v = v)
  }

  invisible(TRUE)
}

.sm_validate_output_vcov_diagonal <- function(rows, i, v, tol = 1e-10) {
  contract <- v$diag_contract
  if (identical(contract, "not_checked")) {
    return(invisible(TRUE))
  }
  coordinate <- match(rows$indicator[[i]], v$indicator_order)
  if (is.na(coordinate)) {
    .sm_output_v_error(
      "The output indicator is absent from V indicator_order.",
      expected = v$indicator_order,
      actual = rows$indicator[[i]],
      row_identity = .sm_row_identity(rows, i)
    )
  }
  if (identical(contract, "row_se_raw_squared_except_boundary_surrogates") &&
      v$scalar_correction_rule[[coordinate]] %in%
        c("wilson_boundary_surrogate", "agresti_coull_boundary_surrogate")) {
    return(invisible(TRUE))
  }
  expected <- if (identical(contract, "row_se_squared")) {
    rows$se[[i]]^2
  } else {
    rows$se_raw[[i]]^2
  }
  actual <- v$matrix[coordinate, coordinate]
  scale <- max(1, abs(expected), abs(actual))
  if (!isTRUE(all.equal(actual, expected, tolerance = tol * scale, check.attributes = FALSE))) {
    .sm_output_v_error(
      "The covariance diagonal violates its declared row-SE contract.",
      expected = paste0(contract, ": ", signif(expected, 8)),
      actual = signif(actual, 8),
      row_identity = .sm_row_identity(rows, i)
    )
  }
  invisible(TRUE)
}

.sm_validate_output_vcov_design <- function(rows, i, v, tol = 1e-10) {
  coordinate <- match(rows$indicator[[i]], v$indicator_order)
  if (is.na(coordinate)) {
    return(invisible(TRUE))
  }
  has_row_fpc <- "population_size" %in% names(rows)
  expected_rule <- if (has_row_fpc) {
    rows$variance_rule[[i]]
  } else {
    method <- .sm_strip_smoothing_var_method(rows$var_method[[i]])
    if (grepl("binomial_bc$", method)) "design_corrected" else "plugin"
  }
  actual_rule <- rep_len(v$variance_rule, length(v$indicator_order))[[coordinate]]
  multinomial_boundary_exception <-
    .sm_is_multinomial_boundary_design_exception(
      rows,
      i = i,
      v = v,
      coordinate = coordinate,
      expected_rule = expected_rule,
      actual_rule = actual_rule,
      tol = tol
    )

  if (!has_row_fpc) {
    if (!identical(v$sampling_design, "not_specified") ||
        !is.na(v$population_size) ||
        (!identical(actual_rule, expected_rule) &&
          !multinomial_boundary_exception)) {
      .sm_output_v_error(
        "Covariance design metadata is inconsistent with an output row that has no FPC provenance.",
        expected = paste0("not_specified / ", expected_rule),
        actual = paste0(v$sampling_design, " / ", actual_rule),
        row_identity = .sm_row_identity(rows, i)
      )
    }
    return(invisible(TRUE))
  }

  if (!identical(v$sampling_design, rows$sampling_design[[i]]) ||
      !isTRUE(all.equal(v$population_size, rows$population_size[[i]], tolerance = tol, check.attributes = FALSE)) ||
      (!identical(actual_rule, expected_rule) &&
        !multinomial_boundary_exception)) {
    .sm_output_v_error(
      "Covariance design identity is inconsistent with row FPC provenance.",
      expected = paste0(rows$sampling_design[[i]], " / N=", rows$population_size[[i]], " / ", expected_rule),
      actual = paste0(v$sampling_design, " / N=", v$population_size, " / ", actual_rule),
      row_identity = .sm_row_identity(rows, i)
    )
  }

  fields <- c(
    "sampling_fraction",
    "fpc_variance_multiplier",
    "fpc_se_multiplier",
    "variance_multiplier_applied",
    "se_multiplier_applied"
  )
  for (field in fields) {
    actual <- rep_len(v[[field]], length(v$indicator_order))[[coordinate]]
    expected <- rows[[field]][[i]]
    applied_exception <- multinomial_boundary_exception &&
      field %in% c("variance_multiplier_applied", "se_multiplier_applied")
    if (!isTRUE(all.equal(actual, expected, tolerance = tol, check.attributes = FALSE)) &&
        !applied_exception) {
      .sm_output_v_error(
        paste0("Covariance ", field, " is inconsistent with row FPC provenance."),
        expected = expected,
        actual = actual,
        row_identity = .sm_row_identity(rows, i)
      )
    }
  }
  invisible(TRUE)
}

.sm_is_multinomial_boundary_design_exception <- function(
  rows,
  i,
  v,
  coordinate,
  expected_rule,
  actual_rule,
  tol = 1e-10
) {
  if (!identical(v$family, "multinomial") ||
      !isTRUE(rows$flag_zero_cell[[i]]) ||
      !identical(expected_rule, "plugin") ||
      !identical(actual_rule, "design_corrected")) {
    return(FALSE)
  }

  all(v$matrix[coordinate, ] == 0) &&
    all(v$matrix[, coordinate] == 0)
}

.sm_output_chr_scalar <- function(x, field, allow_na = FALSE) {
  ok <- (is.character(x) || is.factor(x)) && length(x) == 1L
  if (!ok || (!allow_na && is.na(x)) || (!allow_na && identical(as.character(x), ""))) {
    .sm_output_error(
      paste0("`", field, "` must be a single character value."),
      expected = "character scalar",
      actual = paste(class(x), collapse = "/")
    )
  }
  as.character(x)
}

.sm_output_int_scalar <- function(x, field, positive = FALSE) {
  ok <- (is.integer(x) || is.numeric(x)) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x) &&
    x == floor(x)
  if (!ok || (positive && x <= 0)) {
    .sm_output_error(
      paste0("`", field, "` must be an integer scalar."),
      expected = if (positive) "positive integer scalar" else "integer scalar",
      actual = paste(class(x), collapse = "/")
    )
  }
  as.integer(x)
}

.sm_output_num_scalar <- function(x, field, positive = FALSE) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
  if (!ok || (positive && x <= 0)) {
    .sm_output_error(
      paste0("`", field, "` must be a finite numeric scalar."),
      expected = if (positive) "positive numeric scalar" else "finite numeric scalar",
      actual = paste(class(x), collapse = "/")
    )
  }
  as.numeric(x)
}

.sm_output_lgl_scalar <- function(x, field) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .sm_output_error(
      paste0("`", field, "` must be TRUE or FALSE."),
      expected = "logical scalar",
      actual = paste(class(x), collapse = "/")
    )
  }
  x
}

.sm_output_error <- function(message, expected = NULL, actual = NULL) {
  .sm_abort_estimate(
    message,
    class = "sitemix_error_estimate_var_method",
    expected = expected,
    actual = actual
  )
}

.sm_output_v_error <- function(message, expected = NULL, actual = NULL, row_identity = NULL) {
  .sm_abort_estimate(
    message,
    class = "sitemix_error_estimate_vcov_invariant",
    expected = expected,
    actual = actual,
    row_identity = row_identity
  )
}
