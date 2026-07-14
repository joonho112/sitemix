# Intrinsic uncertainty-contract helpers -----------------------------------

.sm_has_v <- function(x) {
  "V" %in% names(x)
}

.sm_estimate_vcov_scale_compatible <- function(x) {
  if (!.sm_has_v(x) || nrow(x) == 0L) {
    return(NA)
  }

  all(.sm_estimate_vcov_scale_compatible_rows(x))
}

.sm_estimate_vcov_scale_compatible_rows <- function(x) {
  if (!.sm_has_v(x)) {
    return(rep(NA, nrow(x)))
  }

  vcov_scale <- vapply(x$V, function(value) value$vcov_scale, character(1))
  pair <- paste(x$estimate_scale, vcov_scale, sep = "\r")
  compatible <- paste(
    c("none", "arcsine", "arcsine_anscombe", "logit"),
    c("raw", "arcsine_delta", "arcsine_delta", "logit_delta"),
    sep = "\r"
  )
  pair %in% compatible
}

.sm_suppression_uncertainty_facts <- function(x) {
  suppressed_missing <- .sm_is_suppressed_drop_row(x)
  sensitivity <- .sm_is_suppression_sensitivity_row(x)
  identified <- !(suppressed_missing | sensitivity)
  has_sensitivity <- any(sensitivity)

  numeric_sensitivity <- if (has_sensitivity) {
    all(
      is.finite(x$sensitivity_n[sensitivity]) &
        is.finite(x$sensitivity_var_raw[sensitivity]) &
        is.finite(x$sensitivity_var[sensitivity]) &
        x$sensitivity_n[sensitivity] > 0 &
        x$sensitivity_var_raw[sensitivity] >= 0 &
        x$sensitivity_var[sensitivity] >= 0
    )
  } else {
    NA
  }

  list(
    n_identified = as.integer(sum(identified)),
    n_suppressed_missing = as.integer(sum(suppressed_missing)),
    n_suppression_sensitivity = as.integer(sum(sensitivity)),
    suppression_sensitivity_present = has_sensitivity,
    suppression_sensitivity_role = if (has_sensitivity) {
      "nonidentified_variance_sensitivity"
    } else {
      "none"
    },
    sensitivity_numeric_variance_available = numeric_sensitivity,
    sensitivity_acknowledged = if (has_sensitivity) {
      all(x$sensitivity_acknowledged[sensitivity])
    } else {
      NA
    }
  )
}

.sm_smoothing_uncertainty_facts <- function(x) {
  n <- nrow(x)
  smoothing <- attr(x, "smoothing", exact = TRUE)
  absent <- list(
    smoothing_present = FALSE,
    smoothing_provenance_valid = NA,
    smoothing_v_relation = NA_character_,
    v_stale = FALSE,
    v_stale_rows = rep(FALSE, n)
  )
  if (is.null(smoothing)) {
    return(absent)
  }

  basic_valid <- is.list(smoothing) &&
    is.character(smoothing$status) && length(smoothing$status) == 1L &&
    !is.na(smoothing$status) && smoothing$status %in% c("fit", "skipped") &&
    is.character(smoothing$scale) && length(smoothing$scale) == 1L &&
    !is.na(smoothing$scale) && smoothing$scale %in% c("se", "se_raw") &&
    is.logical(smoothing$overwrite) && length(smoothing$overwrite) == 1L &&
    !is.na(smoothing$overwrite) &&
    is.character(smoothing$target_column) &&
    length(smoothing$target_column) == 1L &&
    !is.na(smoothing$target_column) &&
    smoothing$target_column %in% names(x) &&
    identical(smoothing$provenance_column, "var_method_smoothed") &&
    "var_method_smoothed" %in% names(x) &&
    is.numeric(smoothing$eligible_rows) &&
    all(is.finite(smoothing$eligible_rows)) &&
    all(smoothing$eligible_rows == floor(smoothing$eligible_rows)) &&
    all(smoothing$eligible_rows >= 1L & smoothing$eligible_rows <= n) &&
    !anyDuplicated(smoothing$eligible_rows) &&
    is.numeric(smoothing$n_eligible) && length(smoothing$n_eligible) == 1L &&
    is.finite(smoothing$n_eligible) &&
    smoothing$n_eligible == length(smoothing$eligible_rows)
  if (!basic_valid) {
    return(list(
      smoothing_present = TRUE,
      smoothing_provenance_valid = FALSE,
      smoothing_v_relation = "invalid",
      v_stale = NA,
      v_stale_rows = rep(NA, n)
    ))
  }

  eligible <- seq_len(n) %in% as.integer(smoothing$eligible_rows)
  v_fact <- .sm_smoothing_v_fact(x, scale = smoothing$scale, eligible = eligible)
  recorded_v <- smoothing$v
  recorded_valid <- is.list(recorded_v) &&
    is.logical(recorded_v$present) && length(recorded_v$present) == 1L &&
    !is.na(recorded_v$present) &&
    identical(recorded_v$present, v_fact$present) &&
    is.character(recorded_v$relation) && length(recorded_v$relation) == 1L &&
    !is.na(recorded_v$relation) &&
    identical(recorded_v$relation, v_fact$relation) &&
    identical(recorded_v$expected_scales, v_fact$expected_scales) &&
    identical(recorded_v$actual_scales, v_fact$actual_scales) &&
    identical(recorded_v$matrix_effect, v_fact$matrix_effect) &&
    identical(as.integer(recorded_v$matching_rows), as.integer(v_fact$matching_rows)) &&
    identical(as.integer(recorded_v$incompatible_rows), as.integer(v_fact$incompatible_rows))

  stale_rows <- rep(FALSE, n)
  if (identical(smoothing$status, "fit") &&
      isTRUE(smoothing$overwrite) && isTRUE(v_fact$present) &&
      length(v_fact$matching_rows) > 0L) {
    stale_rows[v_fact$matching_rows] <- TRUE
  }

  list(
    smoothing_present = TRUE,
    smoothing_provenance_valid = recorded_valid,
    smoothing_v_relation = v_fact$relation,
    v_stale = any(stale_rows),
    v_stale_rows = stale_rows
  )
}

.sm_zero_uncertainty_census_rows <- function(x) {
  n <- nrow(x)
  required <- c("sampling_design", "population_size", "n", "se_raw", "se")
  if (!all(required %in% names(x))) {
    return(rep(FALSE, n))
  }

  identified <- !.sm_is_suppressed_unavailable_row(x)
  identified &
    !is.na(x$sampling_design) &
    x$sampling_design == "SRSWOR" &
    is.finite(x$population_size) &
    x$population_size == as.numeric(x$n) &
    is.finite(x$se_raw) &
    is.finite(x$se) &
    x$se_raw == 0 &
    x$se == 0
}

.sm_uncertainty_facts <- function(x) {
  retained <- !.sm_is_suppressed_unavailable_row(x)
  has_retained <- any(retained)
  scale_by_indicator <- split(x$estimate_scale, x$indicator)
  census <- .sm_zero_uncertainty_census_rows(x)
  nonpositive_unexplained <- retained & is.finite(x$se) & x$se <= 0 & !census

  scalar_and_v <- list(
    scalar_uncertainty_finite = has_retained &&
      all(is.finite(x$theta_hat[retained])) &&
      all(is.finite(x$se[retained])),
    scalar_se_positive = has_retained && all(x$se[retained] > 0),
    scalar_se_nonpositive_unexplained = any(nonpositive_unexplained),
    n_zero_uncertainty_census = as.integer(sum(census)),
    zero_uncertainty_census_rows = census,
    indicator_scale_consistent = all(vapply(
      scale_by_indicator,
      function(value) length(unique(value)) == 1L,
      logical(1)
    )),
    v_valid = if (.sm_has_v(x) && nrow(x) > 0L) TRUE else NA,
    estimate_vcov_scale_compatible = .sm_estimate_vcov_scale_compatible(x),
    estimate_vcov_scale_compatible_rows = .sm_estimate_vcov_scale_compatible_rows(x)
  )

  c(
    scalar_and_v,
    .sm_suppression_uncertainty_facts(x),
    .sm_smoothing_uncertainty_facts(x)
  )
}
