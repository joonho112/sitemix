# Subgroup aggregate pivot helpers -----------------------------------------

#' Prepare published counts for subgroup proportions or compositions
#'
#' @encoding UTF-8
#'
#' @description
#' `sm_pivot_subgroups_to_sites()` reshapes published subgroup counts
#' for estimation. By default, each site-subgroup pair receives a
#' combined \code{site_id}, keeping its own numerator and denominator.
#' Pass this table to [sm_estimate_from_aggregates()] to estimate a rate
#' within each subgroup. The documentation calls this \strong{Framing X}.
#'
#' A composition \code{partition_target} instead prepares category counts
#' with a common denominator for [sm_estimate_from_counts()] with
#' \code{family = "multinomial"} (Scenario C). These counts describe
#' each subgroup's share of a site total, rather than its within-subgroup
#' rate. The function reshapes counts; it does not estimate proportions or
#' standard errors.
#'
#' @details
#' \strong{Proportions and compositions.} Choose \code{partition_target}
#' according to the quantity you want to estimate:
#' \describe{
#'   \item{\code{"none"} (default)}{Retains each subgroup's numerator
#'     and denominator for a conditional rate: cases within that subgroup
#'     divided by its own denominator. The combined \code{site_id}
#'     joins the source site and subgroup label with \code{separator}.
#'     This is binomial aggregate input (D0).}
#'   \item{\code{"denominator_composition"}}{Uses subgroup denominator
#'     counts as category counts. The common denominator is their sum,
#'     checked against the \code{ALL} denominator. The resulting
#'     proportions describe each subgroup's share of the site's
#'     denominator total.}
#'   \item{\code{"case_composition"}}{Uses subgroup numerator counts
#'     as category counts. The common denominator is their sum, checked
#'     against the \code{ALL} numerator. The resulting proportions
#'     describe how the site's cases are distributed across subgroups.}
#' }
#' For a composition, the subgroups must represent mutually exclusive
#' categories covering the population or cases being counted. Both targets
#' require at least two categories, the same complete category set in every
#' site-year, and exactly one total row. A missing category row is an error;
#' supply an explicit zero for a category with no observations. Category
#' counts must sum to the relevant total within \code{partition_tolerance},
#' and that total must be positive. These count checks do not establish
#' whether the source categories overlap.
#'
#' Denominator composition requires observed denominators for every category
#' and the total, but permits suppressed numerators. For case composition,
#' every category and the total must have an observed numerator that is not
#' suppressed.
#'
#' To keep subgroups as marginal indicators of the original site instead
#' of assigning combined site identifiers, use
#' [sm_pivot_subgroups_to_indicators()] (Framing Y). That alternative
#' retains each subgroup's own numerator and denominator.
#'
#' \strong{Total labels.} The function recognizes the publisher labels
#' \code{"ALL"}, \code{"ALL STUDENT"}, \code{"ALL STUDENTS"},
#' \code{"TOTAL"}, and \code{"OVERALL"} as \code{"ALL"}.
#' Matching ignores case, surrounding whitespace, and punctuation between
#' words. Two recognized total labels in the same site-year become
#' duplicate \code{"ALL"} rows and raise an error. With the default
#' target, the combined \code{site_id} uses \code{"ALL"}, while
#' \code{source_subgroup} preserves the publisher's spelling.
#'
#' \strong{Reporting levels.} Split files that mix school, district, or
#' state rows into separate tables for each reporting level before calling
#' either subgroup helper. The functions do not separate these levels.
#' Supplying \code{level_override} or \code{rtype_col} raises
#' \code{sitemix_error_invalid_level_override}; both must remain \code{NULL}.
#'
#' @param data A data frame or tibble containing subgroup aggregate
#'   rows, one row per \code{(site, year, subgroup)} triple.
#' @param site_col A single column name containing source
#'   site identifiers. Defaults to \code{"site_id"}.
#' @param year_col A single column name containing
#'   integer-like years. Defaults to \code{"year"}.
#' @param subgroup_col A single column name containing
#'   subgroup labels. Required.
#' @param numerator_col A single column name containing
#'   aggregate numerator counts. Required.
#' @param denominator_col A single column name containing
#'   aggregate denominator counts. Required.
#' @param indicator A single string giving the indicator label to
#'   place in the output \code{indicator} column. Defaults to
#'   \code{"subgroup_rate"}.
#' @param separator A single non-empty string used to join the source
#'   site and subgroup labels. Defaults to \code{"_"}.
#' @param level_override Must be \code{NULL} (default). Other values raise
#'   \code{sitemix_error_invalid_level_override}.
#' @param rtype_col Must be \code{NULL} (default). Other values raise
#'   \code{sitemix_error_invalid_level_override}; split the source by
#'   reporting level before using this function.
#' @param partition_target A single string selecting the counts to
#'   prepare: \code{"none"} (default; D0 conditional-rate
#'   rows), \code{"denominator_composition"}, or
#'   \code{"case_composition"} (both return Scenario C count input).
#' @param partition_tolerance A single finite, non-negative number. Absolute
#'   tolerance for composition partition checks against the
#'   required \code{ALL} row. Defaults to \code{0.5}.
#' @param suppression_col A single column name, or \code{NULL}
#'   (default). Names the publisher suppression flag column. With
#'   \code{NULL}, an existing \code{suppression_flag} column is used
#'   if present. If neither source is available, source rows receive
#'   \code{suppression_flag = FALSE}.
#' @param suppression_flag_value Value or vector of values marking
#'   publisher suppression in the flag column. Defaults to
#'   \code{""}.
#'
#' @return With \code{partition_target = "none"}, a tibble with one
#'   row per site-year-subgroup for [sm_estimate_from_aggregates()].
#'   It contains:
#'   \describe{
#'     \item{\code{site_id}}{Composite identifier constructed by
#'       joining the source site and subgroup labels with
#'       \code{separator}.}
#'     \item{\code{year}}{The source year, stored as an integer.}
#'     \item{\code{indicator}}{Character scalar (the \code{indicator}
#'       argument).}
#'     \item{\code{c_jt}, \code{n_jt}}{Numerator and denominator
#'       copied from \code{numerator_col} and \code{denominator_col}.}
#'     \item{\code{suppression_flag}}{Always-present logical. It is
#'       \code{TRUE} for rows flagged by the publisher and otherwise
#'       \code{FALSE}. Flags come from \code{suppression_col}, or from an
#'       existing \code{suppression_flag} column when that argument is
#'       \code{NULL}. If neither source flag column is available, all rows
#'       receive \code{FALSE}.}
#'     \item{\code{framing}}{Character scalar; the framing label
#'       (\code{"subgroup_as_site"}).}
#'     \item{\code{source_site_id}, \code{source_subgroup}}{The
#'       original site and publisher subgroup labels, preserved for
#'       reference. A recognized total label becomes \code{"ALL"} only in
#'       the combined \code{site_id}; \code{source_subgroup} keeps its source
#'       spelling.}
#'   }
#'
#'   With a composition target, a tibble with one row per original
#'   site-year, containing \code{site_id}, \code{year}, a common
#'   \code{n_jt}, and one \verb{c_jt_<category>} column per non-total
#'   subgroup. The common \code{n_jt} is the sum of these category counts;
#'   the \code{ALL} row supplies the comparison total and is not a category.
#'   The \code{partition_categories} and \code{indicator_order}
#'   attributes record category order. Use this order as \code{indicators}
#'   in [sm_estimate_from_counts()] with \code{family = "multinomial"}.
#'
#' @seealso
#' \itemize{
#'   \item \link[=sm_pivot_subgroups_to_indicators]{Framing Y helper}.
#'   \item \link[=sm_estimate_from_aggregates]{Aggregate estimation} for the
#'     default output.
#'   \item \link[=sm_estimate_from_counts]{Estimation from counts} for composition
#'     targets.
#'   \item \link[=sm_suppression_report]{Suppression audit} before pivoting.
#'   \item \code{vignette("a5-published-aggregates")} for the walkthrough.
#' }
#'
#' @examples
#' \dontshow{set.seed(1L)}
#' # Synthetic publisher file with school-by-subgroup rows
#' subgroups <- expand.grid(
#'   site_id  = paste0("S", sprintf("%03d", 1:5)),
#'   year     = 2024L,
#'   subgroup = c("frpm_yes", "frpm_no"),
#'   stringsAsFactors = FALSE
#' )
#' subgroups$c_jt <- c(8, 4, 7, 5, 9, 3, 10, 6, 5, 8)
#' subgroups$n_jt <- c(12, 6, 11, 9, 13, 7, 15, 10, 8, 12)
#'
#' pivoted <- sm_pivot_subgroups_to_sites(
#'   subgroups,
#'   subgroup_col    = "subgroup",
#'   numerator_col   = "c_jt",
#'   denominator_col = "n_jt",
#'   indicator       = "frpm_take_up"
#' )
#' head(pivoted)
#'
#' @family reshape
#' @export
sm_pivot_subgroups_to_sites <- function(
  data,
  site_col = "site_id",
  year_col = "year",
  subgroup_col,
  numerator_col,
  denominator_col,
  indicator = "subgroup_rate",
  separator = "_",
  level_override = NULL,
  rtype_col = NULL,
  partition_target = c("none", "denominator_composition", "case_composition"),
  partition_tolerance = 0.5,
  suppression_col = NULL,
  suppression_flag_value = ""
) {
  .sm_validate_data_frame(data)
  .sm_validate_pivot_column_arg(site_col, "site_col")
  .sm_validate_pivot_column_arg(year_col, "year_col")
  .sm_validate_pivot_column_arg(subgroup_col, "subgroup_col")
  .sm_validate_pivot_column_arg(numerator_col, "numerator_col")
  .sm_validate_pivot_column_arg(denominator_col, "denominator_col")
  .sm_validate_pivot_indicator_label(indicator)
  .sm_validate_pivot_separator(separator)
  .sm_validate_partition_tolerance(partition_tolerance)
  partition_target <- .sm_public_choice(
    partition_target,
    c("none", "denominator_composition", "case_composition"),
    "partition_target",
    "sitemix_error_invalid_partition_target"
  )
  if (!is.null(level_override) || !is.null(rtype_col)) {
    supplied <- c(
      if (!is.null(level_override)) "level_override" else character(),
      if (!is.null(rtype_col)) "rtype_col" else character()
    )
    .sm_abort_argument(
      "Mixed-level pivot routing is not supported.",
      class = "sitemix_error_invalid_level_override",
      expected = "`level_override = NULL` and `rtype_col = NULL`",
      actual = paste0("supplied: ", paste(supplied, collapse = ", ")),
      fix = "Split publisher rows into one homogeneous reporting level, then call the pivot helper separately."
    )
  }
  if (!is.null(suppression_col)) {
    .sm_validate_pivot_column_arg(suppression_col, "suppression_col")
  }

  required <- c(site_col, year_col, subgroup_col, numerator_col, denominator_col)
  if (!is.null(suppression_col)) {
    required <- c(required, suppression_col)
  }
  .sm_require_columns(data, required)

  source_site_id <- .sm_aggregate_site_id(data[[site_col]])
  year <- .sm_aggregate_year(data[[year_col]])
  source_subgroup <- .sm_aggregate_chr(data[[subgroup_col]], subgroup_col)
  canonical_subgroup <- .sm_normalize_pivot_subgroup_aliases(source_subgroup)
  c_jt <- .sm_aggregate_integerish(data[[numerator_col]], numerator_col, allow_na = TRUE)
  n_jt <- .sm_aggregate_integerish(data[[denominator_col]], denominator_col, allow_na = TRUE)
  suppression_flag <- .sm_pivot_suppression_flag(
    data = data,
    suppression_col = suppression_col,
    suppression_flag_value = suppression_flag_value
  )

  source <- tibble::tibble(
    site_id = source_site_id,
    year = year,
    subgroup = canonical_subgroup,
    source_subgroup = source_subgroup,
    c_jt = c_jt,
    n_jt = n_jt,
    suppression_flag = suppression_flag
  )

  if (!identical(partition_target, "none")) {
    return(.sm_pivot_subgroups_to_composition_counts(
      source = source,
      partition_target = partition_target,
      partition_tolerance = partition_tolerance
    ))
  }

  out <- tibble::tibble(
    site_id = paste(source_site_id, canonical_subgroup, sep = separator),
    year = year,
    indicator = indicator,
    c_jt = c_jt,
    n_jt = n_jt,
    suppression_flag = suppression_flag,
    framing = "subgroup_as_site",
    source_site_id = source_site_id,
    source_subgroup = source_subgroup
  )
  .sm_validate_pivot_site_duplicates(out)
  out <- out[order(out$site_id, out$year, out$indicator), , drop = FALSE]
  attr(out, "framing") <- "subgroup_as_site"
  attr(out, "partition_target") <- partition_target
  attr(out, "site_id_separator") <- separator
  attr(out, "indicator") <- indicator
  out
}

.sm_pivot_all_alias_keys <- c(
  "ALL",
  "ALL_STUDENT",
  "ALL_STUDENTS",
  "ALLSTUDENT",
  "ALLSTUDENTS",
  "TOTAL",
  "OVER_ALL",
  "OVERALL"
)

.sm_pivot_subgroup_alias_key <- function(x) {
  key <- toupper(trimws(x))
  key <- gsub("[^[:alnum:]]+", "_", key)
  gsub("^_+|_+$", "", key)
}

.sm_normalize_pivot_subgroup_aliases <- function(x) {
  key <- .sm_pivot_subgroup_alias_key(x)
  x[key %in% .sm_pivot_all_alias_keys] <- "ALL"
  x
}

.sm_validate_pivot_column_arg <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || x == "") {
    .sm_abort_argument(
      paste0("`", arg, "` must be one non-empty column name."),
      class = "sitemix_error_invalid_subgroup_col",
      expected = "one column name",
      actual = as.character(x),
      fix = "Pass the publisher column name as a string."
    )
  }
  invisible(TRUE)
}

.sm_validate_pivot_indicator_label <- function(indicator) {
  if (!is.character(indicator) || length(indicator) != 1L || is.na(indicator) || indicator == "") {
    .sm_abort_argument(
      "`indicator` must be one non-empty output indicator label.",
      class = "sitemix_error_invalid_indicator",
      expected = "one label",
      actual = as.character(indicator),
      fix = "Use a label such as `\"chronic_absence\"`."
    )
  }
  invisible(TRUE)
}

.sm_validate_pivot_separator <- function(separator) {
  if (!is.character(separator) || length(separator) != 1L || is.na(separator) || separator == "") {
    .sm_abort_argument(
      "`separator` must be one non-empty string.",
      class = "sitemix_error_invalid_id_cols",
      expected = "one separator string",
      actual = as.character(separator),
      fix = "Use a stable separator for composite subgroup-as-site IDs."
    )
  }
  invisible(TRUE)
}

.sm_validate_partition_tolerance <- function(partition_tolerance) {
  if (!is.numeric(partition_tolerance) ||
      length(partition_tolerance) != 1L ||
      is.na(partition_tolerance) ||
      !is.finite(partition_tolerance) ||
      partition_tolerance < 0) {
    .sm_abort_argument(
      "`partition_tolerance` must be a non-negative finite scalar.",
      class = "sitemix_error_invalid_partition_target",
      expected = "non-negative finite scalar",
      actual = as.character(partition_tolerance),
      fix = "Use a non-negative tolerance for explicit composition checks."
    )
  }
  invisible(TRUE)
}

.sm_pivot_suppression_flag <- function(data, suppression_col, suppression_flag_value) {
  if (is.null(suppression_col)) {
    if ("suppression_flag" %in% names(data)) {
      flag <- data$suppression_flag
    } else {
      return(rep(FALSE, nrow(data)))
    }
  } else {
    flag <- data[[suppression_col]]
  }
  .sm_validate_suppression_flag_col(flag)
  if (is.logical(flag)) {
    return(!is.na(flag) & flag)
  }
  .sm_validate_suppression_flag_value(suppression_flag_value)
  !is.na(flag) & as.character(flag) %in% as.character(suppression_flag_value)
}

.sm_validate_pivot_site_duplicates <- function(x) {
  key <- paste(x$site_id, x$year, x$indicator, sep = "\r")
  if (anyDuplicated(key)) {
    i <- which(duplicated(key))[[1]]
    .sm_abort_aggregate(
      "Subgroup-as-site pivot output must be unique by `(site_id, year, indicator)`.",
      class = "sitemix_error_invalid_aggregate_row",
      expected = "unique composite site-year rows",
      actual = paste(x$site_id[[i]], x$year[[i]], x$indicator[[i]], sep = " / "),
      row_identity = list(
        site_id = x$source_site_id[[i]],
        year = x$year[[i]],
        indicator = x$source_subgroup[[i]],
        row_index = i
      ),
      fix = "Remove duplicate source `(site, year, subgroup)` rows before pivoting."
    )
  }
  invisible(TRUE)
}

.sm_pivot_subgroups_to_composition_counts <- function(
  source,
  partition_target,
  partition_tolerance
) {
  .sm_validate_pivot_source_duplicates(source)
  value_col <- if (identical(partition_target, "denominator_composition")) "n_jt" else "c_jt"
  total_kind <- if (identical(partition_target, "denominator_composition")) "denominator" else "case"

  category_source <- source[toupper(source$subgroup) != "ALL", , drop = FALSE]
  categories <- unique(category_source$subgroup)
  if (length(categories) < 2L) {
    .sm_abort_argument(
      "Composition partition targets require at least two non-ALL categories.",
      class = "sitemix_error_invalid_partition_target",
      expected = "two or more partition categories",
      actual = categories,
      fix = "Filter to the partition categories before requesting a composition target."
    )
  }

  groups <- unique(source[c("site_id", "year")])
  rows <- vector("list", nrow(groups))
  for (i in seq_len(nrow(groups))) {
    group <- groups[i, , drop = FALSE]
    current <- source[source$site_id == group$site_id[[1]] & source$year == group$year[[1]], , drop = FALSE]
    current_category <- current[toupper(current$subgroup) != "ALL", , drop = FALSE]
    all_row <- current[toupper(current$subgroup) == "ALL", , drop = FALSE]
    .sm_validate_partition_all_row(
      all_row = all_row,
      group = group,
      total_kind = total_kind
    )
    .sm_validate_partition_category_grid(
      current_category = current_category,
      categories = categories,
      group = group
    )

    counts <- rep.int(0L, length(categories))
    names(counts) <- categories
    matched <- match(current_category$subgroup, categories)
    counts[matched] <- current_category[[value_col]]
    .sm_validate_partition_count_vector(
      counts,
      source = current_category,
      value_col = value_col,
      total_kind = total_kind
    )

    reference_total <- all_row[[value_col]][[1]]
    .sm_validate_partition_reference_total(
      reference_total,
      source = all_row,
      value_col = value_col,
      total_kind = total_kind
    )
    residual <- abs(sum(counts) - reference_total)
    if (residual > partition_tolerance) {
      .sm_abort_argument(
        "Composition counts do not match the `ALL` total within `partition_tolerance`.",
        class = "sitemix_error_invalid_partition_target",
        expected = paste0("absolute residual <= ", partition_tolerance),
        actual = paste0(
          "site_id = ", group$site_id[[1]],
          ", year = ", group$year[[1]],
          ", sum(categories) = ", sum(counts),
          ", ALL ", total_kind, " = ", reference_total,
          ", residual = ", residual
        ),
        row_identity = list(site_id = group$site_id[[1]], year = group$year[[1]]),
        fix = "Use `partition_target = \"none\"` for conditional rates, or filter to a complete partition."
      )
    }

    n_total <- as.integer(sum(counts))
    if (n_total <= 0L) {
      .sm_abort_argument(
        "Composition count totals must be positive.",
        class = "sitemix_error_invalid_partition_target",
        expected = "positive composition total",
        actual = paste0("site_id = ", group$site_id[[1]], ", year = ", group$year[[1]], ", total = ", n_total),
        row_identity = list(site_id = group$site_id[[1]], year = group$year[[1]]),
        fix = "Composition targets are undefined for empty denominator/case partitions."
      )
    }

    row <- data.frame(
      site_id = group$site_id[[1]],
      year = group$year[[1]],
      n_jt = n_total,
      stringsAsFactors = FALSE
    )
    for (category in categories) {
      row[[paste0("c_jt_", category)]] <- as.integer(counts[[category]])
    }
    rows[[i]] <- row
  }

  out <- tibble::as_tibble(do.call(rbind, rows))
  out <- out[order(out$site_id, out$year), , drop = FALSE]
  attr(out, "partition_target") <- partition_target
  attr(out, "partition_tolerance") <- partition_tolerance
  attr(out, "partition_categories") <- categories
  attr(out, "composition_count_source") <- total_kind
  attr(out, "composition_count_column") <- value_col
  attr(out, "indicator_order") <- categories
  attr(out, "partition_reference") <- "ALL"
  attr(out, "framing") <- "subgroup_as_site"
  out
}

.sm_validate_partition_category_grid <- function(
  current_category,
  categories,
  group
) {
  observed <- unique(current_category$subgroup)
  missing <- setdiff(categories, observed)
  extra <- setdiff(observed, categories)
  if (length(missing) > 0L || length(extra) > 0L) {
    actual_parts <- c(
      if (length(missing) > 0L) {
        paste0("missing = ", paste(missing, collapse = ", "))
      } else {
        character()
      },
      if (length(extra) > 0L) {
        paste0("extra = ", paste(extra, collapse = ", "))
      } else {
        character()
      }
    )
    .sm_abort_argument(
      "Composition targets require the same complete category grid in every site-year.",
      class = "sitemix_error_invalid_partition_target",
      expected = paste0("categories = ", paste(categories, collapse = ", ")),
      actual = paste0(
        "site_id = ", group$site_id[[1]],
        ", year = ", group$year[[1]],
        ", ", paste(actual_parts, collapse = "; ")
      ),
      row_identity = list(site_id = group$site_id[[1]], year = group$year[[1]]),
      fix = "Add the missing category rows explicitly; do not encode an absent row as a zero count."
    )
  }
  invisible(TRUE)
}

.sm_validate_pivot_source_duplicates <- function(source) {
  key <- paste(source$site_id, source$year, source$subgroup, sep = "\r")
  if (anyDuplicated(key)) {
    i <- which(duplicated(key))[[1]]
    .sm_abort_aggregate(
      "Subgroup pivot input must be unique by `(site_id, year, subgroup)`.",
      class = "sitemix_error_invalid_aggregate_row",
      expected = "unique source subgroup rows",
      actual = paste(source$site_id[[i]], source$year[[i]], source$subgroup[[i]], sep = " / "),
      row_identity = list(
        site_id = source$site_id[[i]],
        year = source$year[[i]],
        indicator = source$subgroup[[i]],
        row_index = i
      ),
      fix = "Remove duplicate source subgroup rows before pivoting."
    )
  }
  invisible(TRUE)
}

.sm_validate_partition_all_row <- function(
  all_row,
  group,
  total_kind
) {
  if (nrow(all_row) != 1L) {
    .sm_abort_argument(
      "Composition partition targets require exactly one `ALL` row per site-year.",
      class = "sitemix_error_invalid_partition_target",
      expected = "one `ALL` row",
      actual = paste0(
        "site_id = ", group$site_id[[1]],
        ", year = ", group$year[[1]],
        ", ALL rows = ", nrow(all_row)
      ),
      row_identity = list(site_id = group$site_id[[1]], year = group$year[[1]]),
      fix = paste0("Provide the `ALL` ", total_kind, " total before requesting a composition target.")
    )
  }
  invisible(TRUE)
}

.sm_validate_partition_count_vector <- function(
  counts,
  source,
  value_col,
  total_kind
) {
  if (identical(value_col, "c_jt") && any(source$suppression_flag)) {
    first <- which(source$suppression_flag)[[1]]
    .sm_abort_argument(
      "Case-composition targets cannot use publisher-suppressed numerators.",
      class = "sitemix_error_invalid_partition_target",
      expected = "unsuppressed category numerators",
      actual = paste(source$site_id[[first]], source$year[[first]], source$subgroup[[first]], sep = " / "),
      row_identity = list(
        site_id = source$site_id[[first]],
        year = source$year[[first]],
        indicator = source$subgroup[[first]]
      ),
      fix = "Use `partition_target = \"none\"`, denominator composition, or a source with unsuppressed case counts."
    )
  }
  bad <- is.na(counts)
  if (any(bad)) {
    first_category <- names(counts)[which(bad)[[1]]]
    .sm_abort_argument(
      "Composition category counts must be observed.",
      class = "sitemix_error_invalid_partition_target",
      expected = "non-missing category counts",
      actual = paste0("missing ", total_kind, " count for `", first_category, "`"),
      fix = paste0(
        "Use `partition_target = \"none\"` or provide observed ",
        total_kind,
        " counts for every category."
      )
    )
  }
  invisible(TRUE)
}

.sm_validate_partition_reference_total <- function(
  reference_total,
  source,
  value_col,
  total_kind
) {
  if (identical(value_col, "c_jt") && isTRUE(source$suppression_flag[[1]])) {
    .sm_abort_argument(
      "Case-composition targets require an unsuppressed `ALL` numerator.",
      class = "sitemix_error_invalid_partition_target",
      expected = "unsuppressed `ALL` case total",
      actual = paste(source$site_id[[1]], source$year[[1]], source$subgroup[[1]], sep = " / "),
      row_identity = list(
        site_id = source$site_id[[1]],
        year = source$year[[1]],
        indicator = source$subgroup[[1]]
      ),
      fix = "Use denominator composition or a source with an observed case total."
    )
  }
  if (is.na(reference_total)) {
    .sm_abort_argument(
      "Composition `ALL` totals must be observed.",
      class = "sitemix_error_invalid_partition_target",
      expected = "non-missing `ALL` total",
      actual = paste0("missing `ALL` ", total_kind, " count"),
      row_identity = list(
        site_id = source$site_id[[1]],
        year = source$year[[1]],
        indicator = source$subgroup[[1]]
      ),
      fix = paste0(
        "Use `partition_target = \"none\"` or provide the `ALL` ",
        total_kind,
        " count."
      )
    )
  }
  if (reference_total <= 0L) {
    .sm_abort_argument(
      "Composition `ALL` totals must be positive.",
      class = "sitemix_error_invalid_partition_target",
      expected = "positive `ALL` total",
      actual = paste0("ALL ", total_kind, " count = ", reference_total),
      row_identity = list(
        site_id = source$site_id[[1]],
        year = source$year[[1]],
        indicator = source$subgroup[[1]]
      ),
      fix = "Composition targets are undefined for empty denominator/case partitions."
    )
  }
  invisible(TRUE)
}

#' Prepare subgroup proportions as indicators within each site
#'
#' @encoding UTF-8
#'
#' @description
#' `sm_pivot_subgroups_to_indicators()` prepares published subgroup
#' counts for comparing subgroup proportions within each site. It keeps the
#' original \code{site_id} and uses each subgroup label as an
#' \code{indicator}, preserving that subgroup's numerator and denominator.
#' The documentation calls this \strong{Framing Y}.
#'
#' Pass the result to [sm_estimate_from_aggregates()] with
#' \code{family = "multivariate"} for marginal aggregate estimates
#' (D1). This helper reshapes the counts; it does not estimate proportions,
#' standard errors, or dependence between subgroups.
#'
#' @details
#' \strong{Analysis unit and denominator.} Each returned site-year has
#' several subgroup indicators. Their proportions are conditional on membership
#' in each subgroup, using that subgroup's own denominator. Equal counts or
#' denominators do not show that the same observational units contributed to
#' different indicators. For example, two disjoint subgroups can have the
#' same number of students. Subgroups can also overlap; labels and marginal
#' counts alone do not identify the joint counts.
#'
#' When estimating D1 proportions, set \code{sampling_relation} in
#' [sm_estimate_from_aggregates()] from the source information:
#' \code{"same_units"} only when the marginals describe the same
#' observed units, \code{"different_units"} when they differ, or
#' \code{"unknown"} when this is not known. The pivot does not determine
#' this relationship. If covariance is requested, D1 uses working
#' independence; its off-diagonal zeros are an assumption. See
#' [sm_frechet_envelope()] for the conditions on raw pairwise intervals
#' and projected stress scenarios.
#'
#' Use [sm_pivot_subgroups_to_sites()] with its default
#' \code{partition_target = "none"} when each site-subgroup pair should
#' instead have its own site identifier (Framing X). Both defaults preserve
#' subgroup-specific numerators and denominators. If the question concerns
#' subgroup shares of a common site total, use that function's explicit
#' composition targets; merely representing subgroups as indicators does
#' not create multinomial composition counts.
#'
#' \strong{Subgroup selection and missing rows.} \code{indicator_set}
#' selects and orders subgroup labels. Each retained site-year must have
#' at least two indicators and the same selected indicator set. With
#' \code{na_action = "drop_row"}, any missing count or publisher-suppressed
#' indicator removes the entire site-year group. With \code{"keep_na"},
#' the function retains those groups and inserts missing subgroup rows with
#' \code{NA} counts and \code{suppression_flag = TRUE}. These are missing
#' values, not zero counts. Review the suppression and hidden-denominator
#' settings in [sm_estimate_from_aggregates()] before estimation.
#'
#' Recognized total labels, listed in [sm_pivot_subgroups_to_sites()],
#' become \code{"ALL"} before duplicate and indicator-set checks.
#' \code{source_subgroup} retains the publisher's spelling for observed
#' rows. Split files containing school, district, or state rows by reporting
#' level before using either helper; these functions do not separate levels.
#'
#' @inheritParams sm_pivot_subgroups_to_sites
#' @param indicator_set A character vector of at least two distinct
#'   subgroup labels to retain in the specified order, or \code{NULL}
#'   (default). With \code{NULL}, labels follow their first appearance in
#'   \code{subgroup_col}. Supplied labels must occur in the data.
#'   Recognized total labels become \code{"ALL"}; labels that then
#'   coincide are rejected as duplicate indicators.
#' @param na_action A single string: \code{"drop_row"} (default) or
#'   \code{"keep_na"}. The default removes the entire site-year group
#'   if any selected subgroup has a missing numerator, missing denominator,
#'   or publisher suppression flag. \code{"keep_na"} retains these
#'   groups and inserts \code{NA} rows for absent subgroup indicators.
#'
#' @return A tibble with one row per retained site-year-subgroup,
#'   ordered by site, year, and \code{indicator_set}. It is structured
#'   for [sm_estimate_from_aggregates()] with
#'   \code{family = "multivariate"} and \code{aggregate_case = "D1"}.
#'   It contains:
#'   \describe{
#'     \item{\code{site_id}}{Original source site identifier.}
#'     \item{\code{year}}{Integer year.}
#'     \item{\code{indicator}}{Character scalar; the subgroup label
#'       (each subgroup becomes a marginal indicator).}
#'     \item{\code{c_jt}, \code{n_jt}}{Numerator and denominator
#'       from \code{numerator_col} and \code{denominator_col}.}
#'     \item{\code{suppression_flag}}{Always-present logical. It is
#'       \code{TRUE} for publisher-flagged rows and otherwise \code{FALSE}.
#'       Flags come from \code{suppression_col}, or from an existing
#'       \code{suppression_flag} column when that argument is \code{NULL}.
#'       Without either source flag column, observed rows are \code{FALSE};
#'       missing subgroup rows inserted by \code{na_action = "keep_na"} are
#'       \code{TRUE}.}
#'     \item{\code{framing}}{Character scalar; the framing label
#'       (\code{"subgroup_as_indicator"}).}
#'     \item{\code{source_subgroup}}{Original publisher subgroup label for
#'       observed rows; inserted missing-subgroup rows carry
#'       \code{NA_character_}.}
#'   }
#'
#'   The \code{indicator_set} attribute records the selected subgroup
#'   order. The \code{framing} and \code{na_action} attributes record
#'   how the table was prepared. An error is raised if no site-year groups
#'   remain or fewer than two indicators are retained per group.
#'
#' @seealso
#' \itemize{
#'   \item \link[=sm_pivot_subgroups_to_sites]{Subgroup-as-site and composition counts}
#'     for the alternative analysis units and denominators.
#'   \item \link[=sm_estimate_from_aggregates]{Aggregate estimation} for the D1
#'     estimator.
#'   \item \link[=sm_frechet_envelope]{Fréchet diagnostic} for pairwise
#'     intervals and projected stress.
#'   \item \link[=sm_suppression_report]{Suppression audit} before pivoting.
#'   \item \code{vignette("a5-published-aggregates")} for the walkthrough.
#' }
#'
#' @examples
#' \dontshow{set.seed(1L)}
#' # Synthetic publisher file (same as Framing X example)
#' subgroups <- expand.grid(
#'   site_id  = paste0("S", sprintf("%03d", 1:5)),
#'   year     = 2024L,
#'   subgroup = c("frpm_yes", "frpm_no"),
#'   stringsAsFactors = FALSE
#' )
#' subgroups$c_jt <- c(8, 4, 7, 5, 9, 3, 10, 6, 5, 8)
#' subgroups$n_jt <- c(12, 6, 11, 9, 13, 7, 15, 10, 8, 12)
#'
#' pivoted_y <- sm_pivot_subgroups_to_indicators(
#'   subgroups,
#'   subgroup_col    = "subgroup",
#'   numerator_col   = "c_jt",
#'   denominator_col = "n_jt"
#' )
#' head(pivoted_y)
#'
#' @family reshape
#' @export
sm_pivot_subgroups_to_indicators <- function(
  data,
  site_col = "site_id",
  year_col = "year",
  subgroup_col,
  numerator_col,
  denominator_col,
  indicator_set = NULL,
  na_action = c("drop_row", "keep_na"),
  suppression_col = NULL,
  suppression_flag_value = ""
) {
  .sm_validate_data_frame(data)
  .sm_validate_pivot_column_arg(site_col, "site_col")
  .sm_validate_pivot_column_arg(year_col, "year_col")
  .sm_validate_pivot_column_arg(subgroup_col, "subgroup_col")
  .sm_validate_pivot_column_arg(numerator_col, "numerator_col")
  .sm_validate_pivot_column_arg(denominator_col, "denominator_col")
  if (!is.null(suppression_col)) {
    .sm_validate_pivot_column_arg(suppression_col, "suppression_col")
  }
  na_action <- .sm_public_choice(
    na_action,
    c("drop_row", "keep_na"),
    "na_action",
    "sitemix_error_invalid_na_action"
  )

  required <- c(site_col, year_col, subgroup_col, numerator_col, denominator_col)
  if (!is.null(suppression_col)) {
    required <- c(required, suppression_col)
  }
  .sm_require_columns(data, required)

  source_subgroup <- .sm_aggregate_chr(data[[subgroup_col]], subgroup_col)
  source <- tibble::tibble(
    site_id = .sm_aggregate_site_id(data[[site_col]]),
    year = .sm_aggregate_year(data[[year_col]]),
    indicator = .sm_normalize_pivot_subgroup_aliases(source_subgroup),
    source_subgroup = source_subgroup,
    c_jt = .sm_aggregate_integerish(data[[numerator_col]], numerator_col, allow_na = TRUE),
    n_jt = .sm_aggregate_integerish(data[[denominator_col]], denominator_col, allow_na = TRUE),
    suppression_flag = .sm_pivot_suppression_flag(
      data = data,
      suppression_col = suppression_col,
      suppression_flag_value = suppression_flag_value
    )
  )
  .sm_validate_pivot_indicator_duplicates(source)
  indicator_set <- .sm_resolve_pivot_indicator_set(source$indicator, indicator_set)

  source <- source[source$indicator %in% indicator_set, , drop = FALSE]
  source <- .sm_pivot_complete_indicator_grid(source, indicator_set = indicator_set)
  if (identical(na_action, "drop_row")) {
    source <- .sm_pivot_drop_incomplete_d1_groups(source)
  }
  .sm_validate_pivot_d1_groups(source)

  source$indicator <- factor(source$indicator, levels = indicator_set)
  source <- source[order(source$site_id, source$year, source$indicator), , drop = FALSE]
  source$indicator <- as.character(source$indicator)

  out <- tibble::as_tibble(source)
  out$framing <- "subgroup_as_indicator"
  attr(out, "framing") <- "subgroup_as_indicator"
  attr(out, "indicator_set") <- indicator_set
  attr(out, "na_action") <- na_action
  out
}

.sm_validate_pivot_indicator_duplicates <- function(x) {
  key <- paste(x$site_id, x$year, x$indicator, sep = "\r")
  if (anyDuplicated(key)) {
    i <- which(duplicated(key))[[1]]
    .sm_abort_aggregate(
      "Subgroup-as-indicator pivot input must be unique by `(site_id, year, subgroup)`.",
      class = "sitemix_error_invalid_aggregate_row",
      expected = "unique source subgroup rows",
      actual = paste(x$site_id[[i]], x$year[[i]], x$indicator[[i]], sep = " / "),
      row_identity = list(
        site_id = x$site_id[[i]],
        year = x$year[[i]],
        indicator = x$indicator[[i]],
        row_index = i
      ),
      fix = "Remove duplicate source subgroup rows before pivoting."
    )
  }
  invisible(TRUE)
}

.sm_resolve_pivot_indicator_set <- function(indicator, indicator_set) {
  observed <- unique(indicator)
  if (is.null(indicator_set)) {
    return(observed)
  }
  if (!is.character(indicator_set) ||
      length(indicator_set) < 2L ||
      anyNA(indicator_set) ||
      any(indicator_set == "")) {
    .sm_abort_argument(
      "`indicator_set` must be NULL or at least two distinct subgroup labels.",
      class = "sitemix_error_invalid_indicators",
      expected = "NULL or two or more distinct labels",
      actual = as.character(indicator_set),
      fix = "Pass subgroup labels in the intended D1 coordinate order."
    )
  }
  publisher_indicator_set <- indicator_set
  indicator_set <- .sm_normalize_pivot_subgroup_aliases(indicator_set)
  if (anyDuplicated(indicator_set)) {
    .sm_abort_argument(
      "`indicator_set` contains labels that collapse to one canonical subgroup.",
      class = "sitemix_error_invalid_indicators",
      expected = "distinct labels after total-alias normalization",
      actual = publisher_indicator_set,
      fix = "Keep one total label; recognized publisher total aliases all normalize to `ALL`."
    )
  }
  missing <- setdiff(indicator_set, observed)
  if (length(missing) > 0L) {
    .sm_abort_argument(
      "`indicator_set` contains subgroup labels not present in `data`.",
      class = "sitemix_error_invalid_indicators",
      expected = observed,
      actual = indicator_set,
      fix = paste0("Missing: ", .sm_cli_collapse(missing, quote = TRUE), ".")
    )
  }
  indicator_set
}

.sm_pivot_complete_indicator_grid <- function(source, indicator_set) {
  groups <- unique(source[c("site_id", "year")])
  rows <- vector("list", nrow(groups))
  for (i in seq_len(nrow(groups))) {
    group <- groups[i, , drop = FALSE]
    current <- source[source$site_id == group$site_id[[1]] & source$year == group$year[[1]], , drop = FALSE]
    current$indicator <- factor(current$indicator, levels = indicator_set)
    current <- current[order(current$indicator), , drop = FALSE]
    missing <- setdiff(indicator_set, as.character(current$indicator))
    if (length(missing) > 0L) {
      fill <- tibble::tibble(
        site_id = rep(group$site_id[[1]], length(missing)),
        year = rep(group$year[[1]], length(missing)),
        indicator = missing,
        source_subgroup = rep(NA_character_, length(missing)),
        c_jt = rep(NA_integer_, length(missing)),
        n_jt = rep(NA_integer_, length(missing)),
        suppression_flag = rep(TRUE, length(missing))
      )
      current$indicator <- as.character(current$indicator)
      current <- vctrs::vec_rbind(current, fill)
    } else {
      current$indicator <- as.character(current$indicator)
    }
    rows[[i]] <- current
  }
  vctrs::vec_rbind(!!!rows)
}

.sm_pivot_drop_incomplete_d1_groups <- function(source) {
  key <- paste(source$site_id, source$year, sep = "\r")
  incomplete <- source$suppression_flag | is.na(source$c_jt) | is.na(source$n_jt)
  drop_keys <- unique(key[incomplete])
  if (length(drop_keys) == 0L) {
    return(source)
  }
  source[!key %in% drop_keys, , drop = FALSE]
}

.sm_validate_pivot_d1_groups <- function(x) {
  if (nrow(x) == 0L) {
    .sm_abort_aggregate(
      "Subgroup-as-indicator pivot produced no retained rows.",
      class = "sitemix_error_ambiguous_dispatch",
      expected = "two or more subgroup indicators per site-year",
      actual = "zero rows",
      fix = "Use `na_action = \"keep_na\"`, expand `indicator_set`, or inspect suppression/missingness."
    )
  }
  groups <- split(seq_len(nrow(x)), paste(x$site_id, x$year, sep = "\r"))
  group_k <- vapply(groups, function(idx) length(unique(x$indicator[idx])), integer(1))
  if (any(group_k < 2L)) {
    first_group <- which(group_k < 2L)[[1]]
    first_row <- groups[[first_group]][[1]]
    .sm_abort_aggregate(
      "Subgroup-as-indicator pivot requires at least two retained indicators in every site-year group.",
      class = "sitemix_error_ambiguous_dispatch",
      expected = "K >= 2 after pivot filtering",
      actual = paste0("K = ", group_k[[first_group]]),
      row_identity = list(
        site_id = x$site_id[[first_row]],
        year = x$year[[first_row]],
        indicator = x$indicator[[first_row]],
        row_index = first_row
      ),
      fix = "Use Framing X for single retained subgroup groups, or keep additional subgroups."
    )
  }
  invisible(TRUE)
}
