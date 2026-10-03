# Aggregate suppression detection ------------------------------------------

#' Summarize suppression and reporting thresholds in aggregate data
#'
#' @encoding UTF-8
#'
#' @description
#' `sm_suppression_report()` counts publisher-suppressed rows and rows
#' below a chosen denominator threshold. It also reports whether
#' suppressed denominators are observed. Use it to examine published
#' input data before calling [sm_estimate_from_aggregates()]. It does
#' not fill in hidden values.
#'
#' @details
#' The report groups rows into three tiers:
#' \describe{
#'   \item{Tier 1 -- suppressed}{Rows identified by a publisher flag or
#'     a missing numerator with an observed positive denominator. A
#'     \code{suppression_when} predicate overrides both rules. Missing
#'     denominators alone do not identify suppression.}
#'   \item{Tier 2 -- below threshold}{Rows that are not suppressed and have
#'     an observed denominator smaller than \code{accountability_n}.}
#'   \item{Tier 3 -- meets threshold}{Rows that are not suppressed and have
#'     an observed denominator at least as large as \code{accountability_n}.}
#' }
#' These tiers describe suppression and the chosen size threshold.
#' The report retains missing-denominator counts separately. Its
#' \code{accountability_n} threshold can be changed; it is distinct
#' from the fixed 11--29 denominator range used by
#' \code{sm_smooth_variance(scope = "tier2")}.
#'
#' Each group defined by \code{by} (for example,
#' \code{c("subgroup", "year")}) receives counts and proportions.
#' For suppressed rows, the report distinguishes retaining missing
#' estimates from an explicitly acknowledged variance-sensitivity
#' analysis. Use [sm_diagnose()] to examine the resulting estimates.
#'
#' @param x A data frame or tibble of aggregate input data. Required
#'   columns depend on the input format; see
#'   [sm_estimate_from_aggregates()].
#' @param by A character vector, or \code{NULL} for one overall row.
#'   Columns to group the report by after
#'   aggregate normalization. Defaults to
#'   \code{c("subgroup", "year")}. Each group gets its own row in
#'   the returned tibble.
#' @param id_cols Character vector of length two. Site and year
#'   column names. Defaults to \code{c("site_id", "year")}.
#' @param numerator_col Character scalar or \code{NULL} (default
#'   \code{NULL}). Source numerator column name. Required when the
#'   publisher uses other column names.
#' @param denominator_col Character scalar or \code{NULL} (default
#'   \code{NULL}). Source denominator column name. Required when the
#'   publisher uses other column names.
#' @param indicator_col Character scalar or \code{NULL} (default
#'   \code{NULL}). Source indicator column for long-form input (one
#'   row per site-year-indicator).
#' @param subgroup_col Character scalar or \code{NULL} (default
#'   \code{NULL}). Source subgroup column for publisher files with
#'   subgroup decomposition.
#' @param suppression_col Character scalar or \code{NULL} (default
#'   \code{NULL}). Source publisher suppression flag column. With
#'   \code{NULL}, an existing \code{suppression_flag} column is used.
#' @param suppression_flag_value Value or vector of values marking
#'   publisher suppression in \code{suppression_col}. Defaults to
#'   \code{""} (the empty string).
#' @param suppression_when Function or \code{NULL} (default
#'   \code{NULL}). Optional predicate returning one logical value per
#'   normalized row; overrides flag and missing-numerator detection.
#' @param min_n A positive whole number stored in the report's
#'   \code{min_n} attribute. Defaults to \code{10L}. The tier counts
#'   use suppression status and \code{accountability_n}; changing
#'   \code{min_n} alone does not change those counts.
#' @param accountability_n Positive integer scalar. Tier-2 /
#'   Tier-3 boundary. Among rows that are not suppressed, those with
#'   \eqn{n_{jt} <}{n_jt <} \code{accountability_n} are Tier 2;
#'   those meeting the threshold are Tier 3.
#'   Defaults to \code{30L}.
#'
#' @return A \code{sitemix_suppression_report} tibble with one row
#'   per group defined by \code{by} and the following columns:
#'   \describe{
#'     \item{group columns}{The grouping columns from \code{by}
#'       (e.g., \code{subgroup}, \code{year}).}
#'     \item{\code{n_rows}}{Integer total row count in the group.}
#'     \item{\code{n_tier1}}{Integer count of Tier 1 (publisher-
#'       suppressed) rows in the group.}
#'     \item{\code{n_tier2}}{Integer count of Tier 2 (observed
#'       below \code{accountability_n}) rows.}
#'     \item{\code{n_tier3}}{Integer count of Tier 3 (observed and
#'       meets threshold) rows.}
#'     \item{\code{n_suppressed_hidden_denominator}}{Integer count
#'       of suppressed rows whose denominator is also hidden.}
#'     \item{\code{n_denominator_missing}}{Integer count of rows
#'       with a missing denominator value.}
#'     \item{\code{pct_suppressed}}{Numeric share of suppressed
#'       rows in the group (Tier 1).}
#'     \item{\code{pct_below_accountability}}{Numeric share of
#'       suppressed rows plus rows that are not suppressed and fall below
#'       the threshold:
#'       Tier 1 rows plus Tier 2 rows
#'       below \code{accountability_n}.}
#'     \item{\code{median_n_suppressed}}{Numeric; median denominator
#'       of suppressed rows when observable, else \code{NA}.}
#'     \item{\code{denominator_observed_on_suppressed}}{Logical;
#'       \code{TRUE} when every Tier 1 row in the group carries an
#'       observable denominator.}
#'     \item{\code{suppression_sources}}{Character; a compact
#'       list of the detection rules used (publisher flag,
#'       missing numerator, or user predicate).}
#'     \item{\code{recommended_action}}{Character; a one-line
#'       recommendation distinguishing retained missing estimates from
#'       an acknowledged variance sensitivity.}
#'     \item{\code{sensitivity_role}}{Character with two values:
#'       \itemize{
#'         \item \code{"none"} when Tier 1 is absent.
#'         \item \code{"nonidentified_variance_sensitivity"} otherwise.
#'       }
#'     }
#'     \item{\code{sensitivity_numeric_variance_available}}{Logical;
#'       \code{TRUE} only when Tier-1 denominators are all observed, so a
#'       separated worst-case variance can be quantified.}
#'     \item{\code{sensitivity_requires_acknowledgement}}{Logical;
#'       \code{TRUE} whenever a Tier-1 row is present.}
#'     \item{\code{upper_bound_role}}{Character; identifies the
#'       legacy \code{"upper_bound"} option as a non-identified
#'       variance-sensitivity scenario rather than an estimate. This older
#'       counterpart of \code{sensitivity_role} uses \code{"not_applicable"}
#'       when Tier 1 is absent, whereas \code{sensitivity_role} uses
#'       \code{"none"}.}
#'   }
#'
#' Two older column names are also retained:
#' \code{upper_bound_numeric_variance_available} mirrors
#' \code{sensitivity_numeric_variance_available};
#' \code{upper_bound_requires_acknowledgement} mirrors
#' \code{sensitivity_requires_acknowledgement}.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}}
#'     for estimating proportions and handling suppressed input rows.
#'   \item \code{\link[=sm_diagnose]{sm_diagnose()}} for the post-estimation
#'     diagnostics.
#'   \item \code{\link[=sm_pivot_subgroups_to_sites]{sm_pivot_subgroups_to_sites()}}
#'     and \code{\link[=sm_pivot_subgroups_to_indicators]{sm_pivot_subgroups_to_indicators()}}
#'     for reshaping subgroup data before making the report.
#'   \item \code{vignette("a5-published-aggregates")} and
#'     \code{vignette("a6-diagnostics-and-suppression")} for the applied
#'     walkthroughs.
#' }
#'
#' @examples
#' \dontshow{set.seed(1L)}
#' # Prepare aggregate counts from the simulated data:
#' counts_path <- system.file(
#'   "extdata", "prek_sim_counts.rds",
#'   package = "sitemix", mustWork = TRUE
#' )
#' counts <- readRDS(counts_path)
#'
#' d0 <- counts[, c("site_id", "year", "n_jt", "c_jt_frpm")]
#' d0$indicator <- "frpm"
#' d0$c_jt <- d0$c_jt_frpm
#' d0$subgroup <- "all"
#' d0 <- d0[c("site_id", "year", "subgroup", "indicator", "c_jt", "n_jt")]
#'
#' report <- sm_suppression_report(
#'   d0,
#'   by              = c("subgroup", "year"),
#'   numerator_col   = "c_jt",
#'   denominator_col = "n_jt",
#'   indicator_col   = "indicator",
#'   subgroup_col    = "subgroup"
#' )
#' head(report)
#'
#' @family audit
#' @export
sm_suppression_report <- function(
  x,
  by = c("subgroup", "year"),
  id_cols = c("site_id", "year"),
  numerator_col = NULL,
  denominator_col = NULL,
  indicator_col = NULL,
  subgroup_col = NULL,
  suppression_col = NULL,
  suppression_flag_value = "",
  suppression_when = NULL,
  min_n = 10L,
  accountability_n = 30L
) {
  .sm_validate_data_frame(x)
  .sm_validate_aggregate_id_cols(id_cols)
  .sm_validate_min_n(min_n)
  .sm_validate_accountability_n(accountability_n)
  by <- .sm_validate_suppression_report_by(by)

  normalized <- .sm_suppression_report_normalize(
    data = x,
    id_cols = id_cols,
    numerator_col = numerator_col,
    denominator_col = denominator_col,
    indicator_col = indicator_col,
    subgroup_col = subgroup_col,
    suppression_col = suppression_col,
    suppression_flag_value = suppression_flag_value
  )
  .sm_validate_suppression_report_by_columns(by, normalized)

  detection <- .sm_detect_aggregate_suppression(
    normalized,
    suppression_when = suppression_when,
    has_publisher_flag = isTRUE(attr(normalized, "has_publisher_flag", exact = TRUE))
  )
  normalized$flag_suppressed <- detection$flag_suppressed
  normalized$suppression_source <- detection$suppression_source

  report <- .sm_build_suppression_report(
    normalized,
    by = by,
    min_n = as.integer(min_n),
    accountability_n = as.integer(accountability_n)
  )
  attr(report, "suppression_detection_path") <- detection$suppression_detection_path
  attr(report, "min_n") <- as.integer(min_n)
  attr(report, "accountability_n") <- as.integer(accountability_n)
  class(report) <- c("sitemix_suppression_report", class(report))
  report
}

.sm_suppression_report_normalize <- function(
  data,
  id_cols,
  numerator_col,
  denominator_col,
  indicator_col,
  subgroup_col,
  suppression_col,
  suppression_flag_value
) {
  mapped <- .sm_aggregate_apply_column_mapping(
    data = data,
    id_cols = id_cols,
    numerator_col = numerator_col,
    denominator_col = denominator_col,
    indicator_col = indicator_col,
    subgroup_col = subgroup_col,
    suppression_col = suppression_col
  )
  if (!"indicator" %in% names(mapped) &&
      all(c("c_jt", "n_jt") %in% names(mapped)) &&
      !any(grepl("^c_jt_", names(mapped)))) {
    mapped$indicator <- "aggregate"
  }
  form <- .sm_detect_aggregate_form(mapped)
  out <- if (identical(form, "long")) {
    .sm_aggregate_normalize_long(mapped, indicator = NULL)
  } else {
    .sm_aggregate_normalize_wide(mapped, indicators = NULL)
  }
  out <- .sm_suppression_report_preserve_columns(mapped, out)
  has_publisher_flag <- "suppression_flag" %in% names(mapped)
  out$suppression_flag <- .sm_aggregate_suppression_flag(
    mapped = mapped,
    form = form,
    out = out,
    suppression_flag_value = suppression_flag_value
  )
  out$denominator_observed <- !is.na(out$n_jt)
  attr(out, "has_publisher_flag") <- has_publisher_flag
  out
}

.sm_suppression_report_preserve_columns <- function(mapped, out) {
  keep <- setdiff(
    names(mapped),
    c("site_id", "year", "indicator", "subgroup", "c_jt", "n_jt", "suppression_flag")
  )
  keep <- keep[!grepl("^c_jt_", keep) & !grepl("^n_jt_", keep)]
  if (length(keep) == 0L || !".aggregate_input_row" %in% names(out)) {
    return(out)
  }
  for (column in keep) {
    if (!column %in% names(out) && length(mapped[[column]]) == nrow(mapped)) {
      out[[column]] <- mapped[[column]][out$.aggregate_input_row]
    }
  }
  out
}

.sm_validate_suppression_report_by <- function(by) {
  if (is.null(by)) {
    return(character())
  }
  if (!is.character(by) || anyNA(by) || any(by == "") || anyDuplicated(by)) {
    .sm_abort_argument(
      "`by` must be NULL or distinct report column names.",
      class = "sitemix_error_invalid_id_cols",
      expected = "NULL or distinct column names",
      actual = as.character(by),
      fix = "Use columns in the normalized aggregate input, such as `subgroup`, `indicator`, or `year`."
    )
  }
  by
}

.sm_validate_suppression_report_by_columns <- function(by, x) {
  missing <- setdiff(by, names(x))
  if (length(missing) > 0L) {
    .sm_abort_argument(
      "`by` columns must exist after aggregate normalization.",
      class = "sitemix_error_invalid_id_cols",
      expected = names(x),
      actual = by,
      fix = paste0("Missing: ", .sm_cli_collapse(missing, quote = TRUE), ".")
    )
  }
  invisible(TRUE)
}

.sm_build_suppression_report <- function(x, by, min_n, accountability_n) {
  groups <- .sm_report_groups(x, by)
  rows <- vector("list", length(groups))
  for (i in seq_along(groups)) {
    idx <- groups[[i]]
    group <- x[idx, , drop = FALSE]
    tier1 <- group$flag_suppressed
    tier2 <- !tier1 & !is.na(group$n_jt) & group$n_jt < accountability_n
    tier3 <- !tier1 & !is.na(group$n_jt) & group$n_jt >= accountability_n
    denom_suppressed <- group$n_jt[tier1]
    observed_suppressed <- !is.na(denom_suppressed)
    n_tier1 <- sum(tier1)
    n_tier2 <- sum(tier2)
    n_tier3 <- sum(tier3)
    n_rows <- nrow(group)
    sensitivity_role <- if (n_tier1 == 0L) {
      "none"
    } else {
      "nonidentified_variance_sensitivity"
    }
    sensitivity_numeric_variance_available <-
      n_tier1 > 0L && all(observed_suppressed)
    sensitivity_requires_acknowledgement <- n_tier1 > 0L
    stats <- data.frame(
        n_rows = as.integer(n_rows),
        n_tier1 = as.integer(n_tier1),
        n_tier2 = as.integer(n_tier2),
        n_tier3 = as.integer(n_tier3),
        n_suppressed_hidden_denominator = as.integer(sum(tier1 & is.na(group$n_jt))),
        n_denominator_missing = as.integer(sum(is.na(group$n_jt))),
        pct_suppressed = if (n_rows > 0L) n_tier1 / n_rows else NA_real_,
        pct_below_accountability = if (n_rows > 0L) (n_tier1 + n_tier2) / n_rows else NA_real_,
        median_n_suppressed = if (any(tier1 & !is.na(group$n_jt))) stats::median(group$n_jt[tier1 & !is.na(group$n_jt)]) else NA_real_,
        denominator_observed_on_suppressed = if (n_tier1 == 0L) TRUE else all(observed_suppressed),
        suppression_sources = paste(sort(unique(group$suppression_source[tier1])), collapse = ","),
        recommended_action = .sm_suppression_recommended_action(
          n_tier1 = n_tier1,
          denominator_observed_on_suppressed = if (n_tier1 == 0L) TRUE else all(observed_suppressed)
        ),
        sensitivity_role = sensitivity_role,
        sensitivity_numeric_variance_available = sensitivity_numeric_variance_available,
        sensitivity_requires_acknowledgement = sensitivity_requires_acknowledgement,
        upper_bound_role = if (n_tier1 == 0L) "not_applicable" else sensitivity_role,
        upper_bound_numeric_variance_available = sensitivity_numeric_variance_available,
        upper_bound_requires_acknowledgement = sensitivity_requires_acknowledgement,
        stringsAsFactors = FALSE
    )
    rows[[i]] <- if (length(by) > 0L) {
      cbind(group[1, by, drop = FALSE], stats)
    } else {
      stats
    }
  }
  tibble::as_tibble(do.call(rbind, rows))
}

.sm_report_groups <- function(x, by) {
  if (length(by) == 0L) {
    return(list(seq_len(nrow(x))))
  }
  key_data <- x[by]
  key <- do.call(
    paste,
    c(lapply(key_data, function(col) ifelse(is.na(col), "<NA>", as.character(col))), sep = "\r")
  )
  split(seq_len(nrow(x)), key)
}

.sm_suppression_recommended_action <- function(n_tier1, denominator_observed_on_suppressed) {
  if (n_tier1 == 0L) {
    return("no_suppression_detected")
  }
  if (isTRUE(denominator_observed_on_suppressed)) {
    "drop_or_acknowledge_variance_sensitivity"
  } else {
    "drop_or_acknowledge_unquantified_sensitivity"
  }
}

.sm_detect_aggregate_suppression <- function(
  x,
  suppression_when = NULL,
  has_publisher_flag = "suppression_flag" %in% names(x)
) {
  .sm_validate_suppression_when_arg(suppression_when)
  structural_flag <- is.na(x$c_jt) & !is.na(x$n_jt) & x$n_jt > 0L

  if (!is.null(suppression_when)) {
    flag <- .sm_call_suppression_when(suppression_when, x)
    path <- "user_predicate"
    flag <- .sm_validate_suppression_result(flag, nrow(x), path)
    source <- ifelse(flag, "user_predicate", "none")
  } else if (isTRUE(has_publisher_flag)) {
    publisher_flag <- x$suppression_flag
    flag <- publisher_flag | structural_flag
    path <- "publisher_flag"
    source <- ifelse(
      publisher_flag,
      "publisher_flag",
      ifelse(structural_flag, "structural_na", "none")
    )
  } else {
    flag <- structural_flag
    path <- "structural"
    source <- ifelse(flag, "structural_na", "none")
  }

  flag <- .sm_validate_suppression_result(flag, nrow(x), path)
  flag <- as.logical(flag)
  denominator_observed <- !is.na(x$n_jt)
  suppressed_n <- denominator_observed[flag]

  list(
    flag_suppressed = flag,
    suppression_source = as.character(source),
    suppression_detection_path = path,
    n_suppressed = as.integer(sum(flag)),
    denominator_observed_on_suppressed = if (length(suppressed_n) == 0L) TRUE else all(suppressed_n)
  )
}

.sm_validate_suppression_when_arg <- function(suppression_when) {
  if (is.null(suppression_when)) {
    return(invisible(TRUE))
  }
  if (!is.function(suppression_when)) {
    .sm_abort_argument(
      "`suppression_when` must be NULL or a predicate function.",
      class = "sitemix_error_invalid_suppression_when",
      expected = "NULL or function",
      actual = paste(class(suppression_when), collapse = "/"),
      fix = "Pass a function returning one logical value per aggregate row."
    )
  }
  invisible(TRUE)
}

.sm_call_suppression_when <- function(suppression_when, x) {
  args <- list(
    C = x$c_jt,
    N = x$n_jt,
    flag = x$suppression_flag,
    c_jt = x$c_jt,
    n_jt = x$n_jt,
    suppression_flag = x$suppression_flag
  )
  formals_names <- names(formals(suppression_when))

  result <- tryCatch(
    {
      if ("..." %in% formals_names) {
        do.call(suppression_when, args)
      } else {
        matched <- intersect(formals_names, names(args))
        if (length(matched) > 0L) {
          do.call(suppression_when, args[matched])
        } else {
          suppression_when(x$c_jt, x$n_jt)
        }
      }
    },
    error = function(err) {
      .sm_abort_argument(
        "`suppression_when` failed while evaluating aggregate rows.",
        class = "sitemix_error_invalid_suppression_when",
        expected = "a predicate returning logical values",
        actual = conditionMessage(err),
        fix = "Use a vectorized predicate over `C`, `N`, and optionally `flag`."
      )
    }
  )
  result
}

.sm_validate_suppression_result <- function(flag, n, path) {
  if (!is.logical(flag) || !(length(flag) %in% c(1L, n)) || anyNA(flag)) {
    .sm_abort_argument(
      "`suppression_when` must return non-missing logical values.",
      class = "sitemix_error_invalid_suppression_when",
      expected = paste0("logical scalar or logical vector of length ", n),
      actual = paste0("type ", paste(class(flag), collapse = "/"), ", length ", length(flag)),
      fix = "Return TRUE for Tier-1 suppressed rows and FALSE otherwise."
    )
  }
  if (length(flag) == 1L && n != 1L) {
    flag <- rep(flag, n)
  }
  if (!identical(path, "user_predicate") && length(flag) != n) {
    .sm_abort_argument(
      "Suppression detector returned an invalid length.",
      class = "sitemix_error_invalid_suppression_when",
      expected = paste0("length ", n),
      actual = paste0("length ", length(flag)),
      fix = "Report this as an internal aggregate suppression bug."
    )
  }
  as.logical(flag)
}

.sm_validate_suppression_flag_value <- function(suppression_flag_value) {
  if (is.null(suppression_flag_value) ||
      !is.atomic(suppression_flag_value) ||
      length(suppression_flag_value) == 0L ||
      anyNA(suppression_flag_value)) {
    .sm_abort_argument(
      "`suppression_flag_value` must contain one or more non-missing values.",
      class = "sitemix_error_invalid_suppression_col",
      expected = "non-missing scalar or vector",
      actual = paste(class(suppression_flag_value), collapse = "/"),
      fix = "Use the publisher value or values that mark Tier-1 suppression."
    )
  }
  invisible(TRUE)
}

.sm_validate_suppression_flag_col <- function(flag) {
  if (!(is.logical(flag) || is.character(flag) || is.factor(flag))) {
    .sm_abort_argument(
      "`suppression_col` must be logical, character, or factor.",
      class = "sitemix_error_invalid_suppression_col",
      expected = "logical/character/factor publisher flag column",
      actual = paste(class(flag), collapse = "/"),
      fix = "Convert the publisher suppression flag to logical or character labels."
    )
  }
  invisible(TRUE)
}
