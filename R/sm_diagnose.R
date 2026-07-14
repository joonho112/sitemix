# Public diagnostics --------------------------------------------------------

# These strings annotate row diagnostics. They are not emitted R condition
# classes and therefore do not belong to `.sm_warning_classes_known`.
.sm_diagnostic_warning_codes_known <- c(
  suppression_sensitivity = "sitemix_warning_suppression_sensitivity",
  suppression_dropped = "sitemix_warning_suppression_dropped",
  estimate_vcov_scale = "sitemix_warning_estimate_vcov_scale_mismatch",
  mixed_vcov_scale = "sitemix_warning_mixed_vcov_scale_relation"
)

#' Diagnose uncertainty in a sitemix_estimates tibble
#'
#' @encoding UTF-8
#'
#' @description
#' `sm_diagnose()` is the canonical uncertainty audit for a
#' \code{sitemix_estimates} tibble. It returns one of three S3-classed
#' tibbles depending on \code{level}: an object-level summary
#' (\code{"summary"}), a row-level audit with all canonical flags
#' (\code{"row"}), or a covariance-level audit reporting PSD and
#' covariance-contract facts (\code{"vcov"}).
#'
#' @details
#' The three diagnostic levels return distinct S3 classes:
#'
#' \describe{
#'   \item{\code{level = "summary"}}{Returns a
#'     \code{sitemix_diagnostics_summary} tibble: one row
#'     summarizing the object, including denominator percentiles,
#'     tier counts, and intrinsic scalar/covariance validity facts. The
#'     cheapest level; suitable for an interactive sanity check.}
#'   \item{\code{level = "row"}}{Returns a
#'     \code{sitemix_diagnostics_row} tibble: one row per
#'     site-year-indicator with every canonical flag described below and the
#'     row-level severity tier. Use before any audit that needs
#'     per-row provenance.}
#'   \item{\code{level = "vcov"}}{Returns a
#'     \code{sitemix_diagnostics_vcov} tibble: one row per
#'     site-year-indicator block of the \code{V} list-column with
#'     PSD margin (smallest eigenvalue), scale compatibility, smoothing
#'     relation, and stale-matrix facts. Only meaningful when \code{x}
#'     was produced with \code{vjt = TRUE}.}
#' }
#'
#' Row diagnostics include these canonical flags:
#' \itemize{
#'   \item \code{flag_small_n} and \code{flag_below_accountability}.
#'   \item \code{flag_zero_cell} and \code{flag_suppressed}.
#' }
#'
#' Summary diagnostics describe properties of the estimate object itself:
#' finite scalar uncertainty, positive standard errors, indicator-level
#' scale consistency, covariance validity, and estimate/covariance scale
#' compatibility. They do not inspect or assume a downstream consumer.
#'
#' Diagnostic severity follows an intrinsic four-level matrix. \code{"error"}
#' records invalid scalar uncertainty, mixed scales within an indicator,
#' invalid smoothing provenance, or a stale matching-scale covariance.
#' \code{"warning"} records unavailable suppression rows, non-identified
#' variance sensitivity, or an explicit estimate/covariance scale mismatch.
#' \code{"note"} records descriptive facts such as small denominators,
#' boundary cells, and accountability thresholds. \code{"ok"} means none of
#' those facts applies. Severity priority is error, warning, note, then ok.
#' A diagnostic reports these facts after ordinary object validation; it does
#' not replace \code{validate.sitemix_estimates()}.
#'
#' The summary field matrix is grouped as follows:
#' \describe{
#'   \item{Scalar facts}{\code{scalar_uncertainty_finite},
#'     \code{scalar_se_positive}, and
#'     \code{indicator_scale_consistent}. An exact zero-uncertainty SRSWOR
#'     census is reported separately and is a note, not an error.}
#'   \item{Covariance facts}{\code{v_present}, \code{v_valid}, and
#'     \code{estimate_vcov_scale_compatible}.}
#'   \item{Suppression facts}{Counts for identified, suppressed-missing, and
#'     sensitivity rows, plus role provenance, numeric-variance availability,
#'     and acknowledgement.}
#'   \item{Smoothing facts}{\code{smoothing_present}, provenance validity,
#'     \code{smoothing_v_relation}, and \code{v_stale}.}
#'   \item{Classification}{\code{diag_severity} and semicolon-delimited
#'     \code{diag_notes}.}
#' }
#' The suppression role-provenance field is
#' \code{suppression_sensitivity_role}.
#'
#' @param x A \code{sitemix_estimates} tibble produced by
#'   [sm_estimate()] or one of its wrappers.
#' @param level Character scalar. Output granularity. One of
#'   \code{"summary"} (default), \code{"row"}, or \code{"vcov"}. See
#'   \emph{Details} for the per-level return class.
#' @param verbose Logical scalar. If \code{TRUE} (default), print a
#'   compact CLI summary alongside the returned tibble.
#'
#' @return One of three S3-classed tibbles depending on \code{level};
#'   see \emph{Details}. The summary variant has one object-level
#'   row and adds one integer \code{n_var_method_<label>} column for each
#'   observed \code{var_method}; this dynamic column set therefore depends
#'   on the diagnosed object. The row and vcov variants have row-per-unit
#'   structures suitable for filtering / dplyr operations.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=sm_estimate]{sm_estimate()}} for the upstream producer
#'     and canonical column glossary.
#'   \item \code{\link[=sm_suppression_report]{sm_suppression_report()}} for
#'     publisher-side suppression auditing alongside \code{sm_diagnose()}.
#'   \item \code{vignette("a6-diagnostics-and-suppression")} for the applied
#'     walkthrough of all three levels.
#' }
#'
#' @examples
#' \dontshow{set.seed(1L)}
#' data(alprek_subset, package = "sitemix")
#' est <- sm_estimate(
#'   subset(alprek_subset, year == 2024),
#'   family    = "binomial",
#'   indicator = "frpm"
#' )
#'
#' # Summary diagnostics (the default; cheapest):
#' diag_s <- sm_diagnose(est, verbose = FALSE)
#' class(diag_s)
#'
#' # Row-level diagnostics for a full audit:
#' diag_r <- sm_diagnose(est, level = "row", verbose = FALSE)
#' head(diag_r, 5)
#'
#' @family audit
#' @export
sm_diagnose <- function(
  x,
  level = c("summary", "row", "vcov"),
  verbose = TRUE
) {
  .sm_diagnose_validate_x(x)
  level <- .sm_diagnose_level(level)
  .sm_diagnose_validate_verbose(verbose)

  out <- switch(
    level,
    summary = .sm_diagnose_summary(x),
    row = .sm_diagnose_row(x),
    vcov = .sm_diagnose_vcov(x)
  )

  if (isTRUE(verbose)) {
    .sm_diagnose_cli_summary(x, summary = .sm_diagnose_summary(x))
  }
  out
}

.sm_diagnose_validate_x <- function(x) {
  if (!inherits(x, "sitemix_estimates")) {
    .sm_abort_input(
      "`x` must be a `sitemix_estimates` object.",
      class = "sitemix_error_input_class",
      expected = "sitemix_estimates",
      actual = paste(class(x), collapse = "/"),
      fix = "Diagnose an object returned by `sm_estimate()` or `sm_estimate_from_counts()`."
    )
  }
  validate.sitemix_estimates(x)
  invisible(TRUE)
}

.sm_diagnose_level <- function(level) {
  choices <- c("summary", "row", "vcov")
  if (is.character(level) && identical(level, choices)) {
    return(choices[[1]])
  }
  if (!is.character(level) || length(level) != 1L || is.na(level) || !level %in% choices) {
    .sm_abort_argument(
      "`level` must be one supported diagnostic level.",
      class = "sitemix_error_invalid_diagnose_level",
      expected = choices,
      actual = as.character(level),
      fix = "Use `level = \"summary\"`, `\"row\"`, or `\"vcov\"`."
    )
  }
  level
}

.sm_diagnose_validate_verbose <- function(verbose) {
  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    .sm_abort_argument(
      "`verbose` must be TRUE or FALSE.",
      class = "sitemix_error_invalid_verbose",
      expected = c("TRUE", "FALSE"),
      actual = paste(class(verbose), collapse = "/"),
      fix = "Pass a scalar logical value."
    )
  }
  invisible(TRUE)
}

.sm_diagnose_vcov_missing <- function() {
  .sm_abort_argument(
    "`sm_diagnose(level = \"vcov\")` requires a `V` list-column.",
    class = "sitemix_error_diagnose_vcov_missing",
    expected = "`sitemix_estimates` with `V`",
    actual = "no `V` column",
    fix = "Re-run `sm_estimate(..., vjt = TRUE)` or use `level = \"summary\"`."
  )
}

.sm_diagnose_summary <- function(x) {
  facts <- .sm_uncertainty_facts(x)
  severity <- .sm_diagnose_summary_severity(x, facts)
  var_method_counts <- table(x$var_method, useNA = "no")
  denominators <- if (nrow(x) == 0L) {
    list(min = NA_integer_, median = NA_real_, max = NA_integer_)
  } else {
    list(
      min = as.integer(min(x$n)),
      median = as.numeric(stats::median(x$n)),
      max = as.integer(max(x$n))
    )
  }
  out <- tibble::tibble(
    family = .sm_diagnose_attr_chr(x, "family"),
    sitemix_role = .sm_diagnose_attr_chr(x, "sitemix_role"),
    n_cells = as.integer(nrow(x)),
    n_groups = as.integer(length(unique(paste(x$site_id, x$year, sep = "\r")))),
    n_sites = as.integer(length(unique(x$site_id))),
    n_years = as.integer(length(unique(x$year))),
    n_indicators = as.integer(length(unique(x$indicator))),
    n_flag_small_n = as.integer(sum(x$flag_small_n)),
    n_flag_zero_cell = as.integer(sum(x$flag_zero_cell, na.rm = TRUE)),
    n_flag_both = as.integer(sum(x$flag_small_n & x$flag_zero_cell, na.rm = TRUE)),
    n_flag_suppressed = as.integer(sum(x$flag_suppressed)),
    n_flag_below_accountability = as.integer(sum(x$flag_below_accountability)),
    n_identified = facts$n_identified,
    n_suppressed_missing = facts$n_suppressed_missing,
    n_suppression_sensitivity = facts$n_suppression_sensitivity,
    n_zero_uncertainty_census = facts$n_zero_uncertainty_census,
    min_n = denominators$min,
    median_n = denominators$median,
    max_n = denominators$max,
    estimate_scale = .sm_diagnose_estimate_scale(x),
    v_present = .sm_has_v(x),
    k_present = "K" %in% names(x),
    n_psd_repair_fired = .sm_diagnose_psd_repair_count(x),
    scalar_uncertainty_finite = facts$scalar_uncertainty_finite,
    scalar_se_positive = facts$scalar_se_positive,
    scalar_se_nonpositive_unexplained = facts$scalar_se_nonpositive_unexplained,
    indicator_scale_consistent = facts$indicator_scale_consistent,
    v_valid = facts$v_valid,
    estimate_vcov_scale_compatible = facts$estimate_vcov_scale_compatible,
    suppression_sensitivity_present = facts$suppression_sensitivity_present,
    suppression_sensitivity_role = facts$suppression_sensitivity_role,
    sensitivity_numeric_variance_available = facts$sensitivity_numeric_variance_available,
    sensitivity_acknowledged = facts$sensitivity_acknowledged,
    smoothing_present = facts$smoothing_present,
    smoothing_provenance_valid = facts$smoothing_provenance_valid,
    smoothing_v_relation = facts$smoothing_v_relation,
    v_stale = facts$v_stale,
    diag_severity = severity$severity,
    diag_notes = severity$notes
  )

  for (method in names(var_method_counts)) {
    out[[paste0("n_var_method_", method)]] <- as.integer(var_method_counts[[method]])
  }

  class(out) <- c("sitemix_diagnostics_summary", class(out))
  out
}

.sm_diagnose_summary_severity <- function(x, facts) {
  errors <- character()
  warnings <- character()
  notes <- character()

  if (facts$n_identified > 0L && !isTRUE(facts$scalar_uncertainty_finite)) {
    errors <- c(errors, "scalar_uncertainty_nonfinite")
  }
  if (isTRUE(facts$scalar_se_nonpositive_unexplained)) {
    errors <- c(errors, "scalar_se_nonpositive")
  }
  if (!isTRUE(facts$indicator_scale_consistent)) {
    errors <- c(errors, "mixed_estimate_scale_within_indicator")
  }
  if (identical(facts$v_valid, FALSE)) {
    errors <- c(errors, "invalid_vcov")
  }
  if (identical(facts$smoothing_provenance_valid, FALSE)) {
    errors <- c(errors, "invalid_smoothing_provenance")
  }
  if (isTRUE(facts$v_stale)) {
    errors <- c(errors, "stale_matching_scale_vcov")
  }

  if (facts$n_identified == 0L) {
    warnings <- c(warnings, "no_identified_scalar_uncertainty")
  }
  if (facts$n_suppressed_missing > 0L) {
    warnings <- c(warnings, "suppressed_missing")
  }
  if (isTRUE(facts$suppression_sensitivity_present)) {
    warnings <- c(warnings, "nonidentified_variance_sensitivity")
  }
  if (identical(facts$estimate_vcov_scale_compatible, FALSE)) {
    warnings <- c(warnings, "estimate_vcov_scale_incompatible")
  }
  if (identical(facts$smoothing_v_relation, "mixed")) {
    warnings <- c(warnings, "mixed_smoothing_vcov_scale_relation")
  }

  if (sum(x$flag_small_n) > 0L) notes <- c(notes, "small_n")
  if (sum(x$flag_zero_cell, na.rm = TRUE) > 0L) notes <- c(notes, "zero_cell")
  if (sum(x$flag_below_accountability) > 0L) {
    notes <- c(notes, "below_accountability")
  }
  if (facts$n_zero_uncertainty_census > 0L) {
    notes <- c(notes, "zero_uncertainty_census")
  }

  severity <- if (length(errors)) {
    "error"
  } else if (length(warnings)) {
    "warning"
  } else if (length(notes)) {
    "note"
  } else {
    "ok"
  }
  list(
    severity = severity,
    notes = paste(unique(c(errors, warnings, notes)), collapse = "; ")
  )
}

.sm_diagnose_estimate_scale <- function(x) {
  scales <- unique(x$estimate_scale)
  if (length(scales) == 0L) {
    NA_character_
  } else if (length(scales) == 1L) {
    scales[[1L]]
  } else {
    "mixed"
  }
}

.sm_diagnose_attr_chr <- function(x, name) {
  value <- attr(x, name, exact = TRUE)
  if (is.null(value)) {
    return(NA_character_)
  }
  as.character(value)
}

.sm_diagnose_psd_repair_count <- function(x) {
  if (!.sm_has_v(x)) {
    return(NA_integer_)
  }
  groups <- split(seq_len(nrow(x)), paste(x$site_id, x$year, sep = "\r"))
  as.integer(sum(vapply(groups, function(idx) {
    identical(x$V[[idx[[1]]]]$psd_repair, "eigen_clip_tol")
  }, logical(1))))
}

.sm_diagnose_row <- function(x) {
  facts <- .sm_uncertainty_facts(x)
  warnings <- vector("list", nrow(x))
  errors <- vector("list", nrow(x))
  notes <- character(nrow(x))
  scale_by_indicator <- split(x$estimate_scale, x$indicator)
  mixed_indicators <- names(scale_by_indicator)[
    vapply(scale_by_indicator, function(value) length(unique(value)) > 1L, logical(1))
  ]

  for (i in seq_len(nrow(x))) {
    row_warnings <- character()
    row_errors <- character()
    row_notes <- character()

    if (!.sm_is_suppressed_unavailable_row(x)[[i]] &&
        (!is.finite(x$theta_hat[[i]]) ||
          !is.finite(x$se[[i]]) ||
          (x$se[[i]] <= 0 && !facts$zero_uncertainty_census_rows[[i]]))) {
      row_errors <- c(row_errors, "sitemix_error_estimate_var_method")
    }
    if (x$indicator[[i]] %in% mixed_indicators) {
      row_errors <- c(row_errors, "sitemix_error_estimate_var_method")
      row_notes <- c(row_notes, "mixed_estimate_scale_within_indicator")
    }
    if (identical(facts$smoothing_provenance_valid, FALSE)) {
      row_errors <- c(row_errors, "sitemix_error_estimate_var_method")
      row_notes <- c(row_notes, "invalid_smoothing_provenance")
    }
    if (isTRUE(facts$v_stale_rows[[i]])) {
      row_errors <- c(row_errors, "sitemix_error_smoothing_v_stale")
      row_notes <- c(row_notes, "stale_matching_scale_vcov")
    }
    if (identical(facts$estimate_vcov_scale_compatible_rows[[i]], FALSE)) {
      row_warnings <- c(
        row_warnings,
        unname(.sm_diagnostic_warning_codes_known[["estimate_vcov_scale"]])
      )
      row_notes <- c(row_notes, "estimate_vcov_scale_incompatible")
    }
    if (identical(facts$smoothing_v_relation, "mixed")) {
      row_warnings <- c(
        row_warnings,
        unname(.sm_diagnostic_warning_codes_known[["mixed_vcov_scale"]])
      )
      row_notes <- c(row_notes, "mixed_smoothing_vcov_scale_relation")
    }
    if (isTRUE(x$flag_small_n[[i]])) {
      row_notes <- c(row_notes, "small_n")
    }
    if (isTRUE(x$flag_zero_cell[[i]])) {
      row_notes <- c(row_notes, "zero_cell")
    }
    if (isTRUE(x$flag_below_accountability[[i]])) {
      row_notes <- c(row_notes, "below_accountability")
    }
    if (isTRUE(facts$zero_uncertainty_census_rows[[i]])) {
      row_notes <- c(row_notes, "zero_uncertainty_census")
    }
    if (isTRUE(x$flag_suppressed[[i]])) {
      if (.sm_is_suppression_sensitivity_row(x)[[i]]) {
        row_warnings <- c(
          row_warnings,
          unname(.sm_diagnostic_warning_codes_known[["suppression_sensitivity"]])
        )
        row_notes <- c(row_notes, "nonidentified_variance_sensitivity")
      } else {
        row_warnings <- c(
          row_warnings,
          unname(.sm_diagnostic_warning_codes_known[["suppression_dropped"]])
        )
        row_notes <- c(row_notes, "suppressed_missing")
      }
    }

    warnings[[i]] <- unique(row_warnings)
    errors[[i]] <- unique(row_errors)
    notes[[i]] <- paste(unique(row_notes), collapse = "; ")
  }

  severity <- vapply(seq_len(nrow(x)), function(i) {
    if (length(errors[[i]]) > 0L) {
      "error"
    } else if (length(warnings[[i]]) > 0L) {
      "warning"
    } else if (nzchar(notes[[i]])) {
      "note"
    } else {
      "ok"
    }
  }, character(1))

  out <- tibble::as_tibble(x)
  unavailable <- .sm_is_suppressed_unavailable_row(x)
  row_scalar_finite <- is.finite(x$theta_hat) & is.finite(x$se)
  row_scalar_positive <- row_scalar_finite & x$se > 0
  row_scalar_finite[unavailable] <- NA
  row_scalar_positive[unavailable] <- NA
  sensitivity <- .sm_is_suppression_sensitivity_row(x)
  numeric_sensitivity <- rep(NA, nrow(x))
  if (any(sensitivity)) {
    numeric_sensitivity[sensitivity] <-
      is.finite(x$sensitivity_n[sensitivity]) &
      is.finite(x$sensitivity_var_raw[sensitivity]) &
      is.finite(x$sensitivity_var[sensitivity])
  }
  out$scalar_uncertainty_finite <- row_scalar_finite
  out$scalar_se_positive <- row_scalar_positive
  out$scalar_se_nonpositive_unexplained <-
    !unavailable & row_scalar_finite & x$se <= 0 &
    !facts$zero_uncertainty_census_rows
  out$zero_uncertainty_census <- facts$zero_uncertainty_census_rows
  out$v_present <- rep(.sm_has_v(x), nrow(x))
  out$v_valid <- rep(facts$v_valid, nrow(x))
  out$estimate_vcov_scale_compatible <- facts$estimate_vcov_scale_compatible_rows
  out$suppression_sensitivity_role <- ifelse(
    sensitivity,
    "nonidentified_variance_sensitivity",
    "none"
  )
  out$sensitivity_numeric_variance_available <- numeric_sensitivity
  out$smoothing_provenance_valid <- rep(
    facts$smoothing_provenance_valid,
    nrow(x)
  )
  out$smoothing_v_relation <- rep(facts$smoothing_v_relation, nrow(x))
  out$v_stale <- facts$v_stale_rows
  out$diag_severity <- severity
  out$diag_warnings <- warnings
  out$diag_errors <- errors
  out$diag_notes <- notes
  class(out) <- c("sitemix_diagnostics_row", class(out))
  out
}

.sm_diagnose_vcov <- function(x) {
  if (!.sm_has_v(x)) {
    .sm_diagnose_vcov_missing()
  }
  if (nrow(x) == 0L) {
    return(.sm_diagnose_vcov_empty())
  }

  facts <- .sm_uncertainty_facts(x)
  groups <- split(seq_len(nrow(x)), paste(x$site_id, x$year, sep = "\r"))
  rows <- lapply(groups, function(idx) {
    V <- x$V[[idx[[1]]]]
    mat <- (as.matrix(V) + t(as.matrix(V))) / 2
    values <- eigen(mat, symmetric = TRUE, only.values = TRUE)$values
    min_eig <- min(values)
    psd_tol <- .sm_psd_tolerance(mat)
    simplex <- .sm_diagnose_simplex_residual(V, mat)
    repeated <- .sm_diagnose_repeated_v_equal(x, idx)
    scale_compatible <- all(facts$estimate_vcov_scale_compatible_rows[idx])
    stale <- if (anyNA(facts$v_stale_rows[idx])) {
      NA
    } else {
      any(facts$v_stale_rows[idx])
    }
    census <- any(facts$zero_uncertainty_census_rows[idx]) &&
      all(facts$zero_uncertainty_census_rows[idx])
    v_valid <- min_eig >= -psd_tol && repeated &&
      !identical(simplex$ok, FALSE)
    notes <- character()
    if (!scale_compatible) notes <- c(notes, "estimate_vcov_scale_incompatible")
    if (identical(facts$smoothing_v_relation, "mixed")) {
      notes <- c(notes, "mixed_smoothing_vcov_scale_relation")
    }
    if (isTRUE(stale)) notes <- c(notes, "stale_matching_scale_vcov")
    if (identical(facts$smoothing_provenance_valid, FALSE)) {
      notes <- c(notes, "invalid_smoothing_provenance")
    }
    if (census) notes <- c(notes, "zero_uncertainty_census")
    severity <- if (!v_valid || isTRUE(stale) ||
        identical(facts$smoothing_provenance_valid, FALSE)) {
      "error"
    } else if (!scale_compatible ||
        identical(facts$smoothing_v_relation, "mixed")) {
      "warning"
    } else if (census) {
      "note"
    } else {
      "ok"
    }
    tibble::tibble(
      site_id = x$site_id[[idx[[1]]]],
      year = x$year[[idx[[1]]]],
      family = V$family,
      K = as.integer(nrow(mat)),
      indicator_order = list(V$indicator_order),
      matrix_rank = V$matrix_rank,
      min_eigenvalue = min_eig,
      psd_tol = psd_tol,
      psd_ok = min_eig >= -psd_tol,
      v_valid = v_valid,
      psd_repair = V$psd_repair,
      vcov_method = if (is.na(V$vcov_method)) NA_character_ else V$vcov_method,
      vcov_scale = V$vcov_scale,
      estimate_scale = V$estimate_scale,
      matrix_boundary_rule = V$matrix_boundary_rule,
      scalar_correction_rule = list(V$scalar_correction_rule),
      positive_support = V$positive_support,
      n_jt = V$n_jt,
      n_eff = V$n_eff,
      simplex_residual = simplex$residual,
      row_sum_zero_ok = simplex$ok,
      repeated_v_equal = repeated,
      zero_uncertainty_census = census,
      estimate_vcov_scale_compatible = scale_compatible,
      smoothing_provenance_valid = facts$smoothing_provenance_valid,
      smoothing_v_relation = facts$smoothing_v_relation,
      v_stale = stale,
      diag_severity = severity,
      diag_notes = paste(unique(notes), collapse = "; ")
    )
  })

  out <- vctrs::vec_rbind(!!!rows)
  class(out) <- c("sitemix_diagnostics_vcov", class(out))
  out
}

.sm_diagnose_vcov_empty <- function() {
  out <- tibble::tibble(
    site_id = character(),
    year = integer(),
    family = character(),
    K = integer(),
    indicator_order = list(),
    matrix_rank = integer(),
    min_eigenvalue = numeric(),
    psd_tol = numeric(),
    psd_ok = logical(),
    v_valid = logical(),
    psd_repair = character(),
    vcov_method = character(),
    vcov_scale = character(),
    estimate_scale = character(),
    matrix_boundary_rule = character(),
    scalar_correction_rule = list(),
    positive_support = integer(),
    n_jt = integer(),
    n_eff = numeric(),
    simplex_residual = numeric(),
    row_sum_zero_ok = logical(),
    repeated_v_equal = logical(),
    zero_uncertainty_census = logical(),
    estimate_vcov_scale_compatible = logical(),
    smoothing_provenance_valid = logical(),
    smoothing_v_relation = character(),
    v_stale = logical(),
    diag_severity = character(),
    diag_notes = character()
  )
  class(out) <- c("sitemix_diagnostics_vcov", class(out))
  out
}

.sm_diagnose_repeated_v_equal <- function(x, idx) {
  first <- x$V[[idx[[1]]]]
  all(vapply(idx, function(row) .sm_vcov_value_equal(first, x$V[[row]]), logical(1)))
}

.sm_diagnose_simplex_residual <- function(V, mat) {
  if (!identical(V$family, "multinomial")) {
    return(list(residual = NA_real_, ok = NA))
  }
  tol <- .sm_simplex_tolerance(mat)
  residual <- max(abs(as.vector(mat %*% rep(1, ncol(mat)))))
  list(residual = residual, ok = residual <= tol)
}

.sm_diagnose_cli_summary <- function(x, summary) {
  cli::cli_h2("sitemix_estimates diagnostics")
  # Repeated `i` names are cli information-bullet markers, not call arguments.
  # nolint start: duplicate_argument_linter.
  cli::cli_inform(c(
    "i" = "{summary$n_cells} cells | {summary$n_groups} groups | {summary$n_indicators} indicators | family = {attr(x, 'family', exact = TRUE)}",
    "i" = "{summary$n_flag_small_n} small-n rows | {summary$n_flag_zero_cell} zero-cell rows | {summary$n_flag_below_accountability} below-accountability rows",
    "i" = paste0(
      "Scalar uncertainty: finite={summary$scalar_uncertainty_finite}; ",
      "SE positive={summary$scalar_se_positive}; ",
      "indicator scales consistent={summary$indicator_scale_consistent}"
    ),
    "i" = paste0(
      "V present: {summary$v_present}; valid={summary$v_valid}; ",
      "estimate/V scales compatible=",
      "{summary$estimate_vcov_scale_compatible}; ",
      "PSD repairs: {summary$n_psd_repair_fired}"
    ),
    "i" = paste0(
      "Suppression: missing={summary$n_suppressed_missing}; ",
      "sensitivity={summary$n_suppression_sensitivity}; ",
      "role={summary$suppression_sensitivity_role}"
    ),
    "i" = paste0(
      "Smoothing: present={summary$smoothing_present}; ",
      "V relation={summary$smoothing_v_relation}; stale={summary$v_stale}; ",
      "diagnostic severity={summary$diag_severity}"
    )
  ))
  # nolint end
  invisible(TRUE)
}

#' @noRd
#' @export
format.sitemix_diagnostics_summary <- function(x, ...) {
  paste0("<sitemix_diagnostics_summary[", nrow(x), " x ", ncol(x), "]>")
}

#' @noRd
#' @export
print.sitemix_diagnostics_summary <- function(x, ...) {
  cat(
    "sitemix_diagnostics_summary: ",
    x$n_cells[[1]],
    " cells | ",
    x$n_groups[[1]],
    " groups | ",
    x$n_indicators[[1]],
    " indicators | scalar finite=",
    x$scalar_uncertainty_finite[[1]],
    " SE positive=",
    x$scalar_se_positive[[1]],
    " scales consistent=",
    x$indicator_scale_consistent[[1]],
    " V valid=",
    x$v_valid[[1]],
    " sensitivity role=",
    x$suppression_sensitivity_role[[1]],
    " V stale=",
    x$v_stale[[1]],
    " severity=",
    x$diag_severity[[1]],
    "\n",
    sep = ""
  )
  print(tibble::as_tibble(x), ...)
  invisible(x)
}

#' @noRd
#' @export
format.sitemix_diagnostics_row <- function(x, ...) {
  paste0("<sitemix_diagnostics_row[", nrow(x), " x ", ncol(x), "]>")
}

#' @noRd
#' @export
print.sitemix_diagnostics_row <- function(x, ...) {
  counts <- table(factor(x$diag_severity, levels = c("ok", "note", "warning", "error")))
  cat(
    "sitemix_diagnostics_row: ",
    nrow(x),
    " rows | ok=",
    unname(counts[["ok"]]),
    " note=",
    unname(counts[["note"]]),
    " warning=",
    unname(counts[["warning"]]),
    " error=",
    unname(counts[["error"]]),
    "\n",
    sep = ""
  )
  print(tibble::as_tibble(x), ...)
  invisible(x)
}

#' @noRd
#' @export
format.sitemix_diagnostics_vcov <- function(x, ...) {
  paste0("<sitemix_diagnostics_vcov[", nrow(x), " x ", ncol(x), "]>")
}

#' @noRd
#' @export
print.sitemix_diagnostics_vcov <- function(x, ...) {
  counts <- table(factor(
    x$diag_severity,
    levels = c("ok", "note", "warning", "error")
  ))
  cat(
    "sitemix_diagnostics_vcov: ",
    nrow(x),
    " matrices | PSD ok=",
    sum(x$psd_ok),
    "/",
    nrow(x),
    " | PSD repairs=",
    sum(x$psd_repair == "eigen_clip_tol"),
    " | scale=",
    paste(unique(x$vcov_scale), collapse = ","),
    " | note=",
    unname(counts[["note"]]),
    " | warning=",
    unname(counts[["warning"]]),
    " error=",
    unname(counts[["error"]]),
    "\n",
    sep = ""
  )
  print(tibble::as_tibble(x), ...)
  invisible(x)
}
