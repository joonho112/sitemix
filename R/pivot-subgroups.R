# Subgroup aggregate pivot helpers -----------------------------------------

#' Pivot subgroup aggregate rows into subgroup-as-site input (Framing X)
#'
#' @encoding UTF-8
#'
#' @description
#' `sm_pivot_subgroups_to_sites()` implements \strong{Framing X} for
#' school-by-subgroup aggregate rows: each \code{(site, subgroup)}
#' pair becomes its own aggregate site with a composite
#' \code{site_id}. The output is consumable by
#' [sm_estimate_from_aggregates()] (default) or, with an explicit
#' composition \code{partition_target}, by [sm_estimate_from_counts()]
#' as Scenario C multinomial count input. Use this when subgroup × site
#' is the unit of analysis; for the alternative framing where
#' subgroups become indicators of the original site, see
#' [sm_pivot_subgroups_to_indicators()] (Framing Y).
#'
#' @details
#' \strong{Framing X vs Framing Y.} The two pivot helpers solve the
#' same input problem (publisher files where each row is a
#' school-by-subgroup observation) but produce different schemas:
#'
#' \describe{
#'   \item{\strong{Framing X (this function)}}{Each subgroup becomes
#'     its own site. The composite \code{site_id} is constructed by
#'     concatenating the source site identifier and the subgroup
#'     label with \code{separator}. Downstream estimation treats
#'     subgroup × site as the keyed pair. Use when the analyst's
#'     question is "what is each subgroup's rate within each site?"}
#'   \item{\strong{Framing Y (sister function)}}{Each subgroup
#'     becomes a marginal indicator of the original site. The
#'     site_id is preserved; the indicator column carries the
#'     subgroup label. Use when the analyst's question is "what does each
#'     site's subgroup profile look like?" See the
#'     \link[=sm_pivot_subgroups_to_indicators]{Framing Y helper}.}
#' }
#'
#' \strong{Partition targets.} Pass \code{partition_target = "none"}
#' (default) for D0-ready conditional-rate output. Pass
#' \code{"denominator_composition"} or \code{"case_composition"} to
#' emit Scenario C multinomial count input; both partition targets
#' require one complete, identical category grid per site-year and one
#' explicit total row in \code{subgroup_col}. Total labels are normalized to
#' canonical \code{"ALL"} from the fixed publisher vocabulary
#' \code{"ALL"}, \code{"ALL STUDENT"}, \code{"ALL STUDENTS"},
#' \code{"TOTAL"}, and \code{"OVERALL"}; matching ignores case, surrounding
#' whitespace, and punctuation between words. Two labels that collapse to
#' \code{"ALL"} in the same site-year are duplicates and fail closed.
#' Composition sums are checked against the canonical total within
#' \code{partition_tolerance}; a missing category row is never inferred to be
#' a structural zero.
#'
#' \strong{Mixed-level scope.} Mixed school/district/state routing is not
#' identified by the current public arguments. Any non-\code{NULL}
#' \code{level_override} or \code{rtype_col} therefore fails closed with the
#' stable invalid-level condition documented for \code{level_override}. Split
#' mixed-level publisher files into homogeneous tables before calling either
#' pivot helper.
#'
#' @param data A data frame or tibble containing subgroup aggregate
#'   rows, one row per \code{(site, year, subgroup)} triple.
#' @param site_col Character scalar. Column name containing source
#'   site identifiers. Defaults to \code{"site_id"}.
#' @param year_col Character scalar. Column name containing
#'   integer-like years. Defaults to \code{"year"}.
#' @param subgroup_col Character scalar. Column name containing
#'   subgroup labels. Required.
#' @param numerator_col Character scalar. Column name containing
#'   aggregate numerators. Required.
#' @param denominator_col Character scalar. Column name containing
#'   aggregate denominators. Required.
#' @param indicator Character scalar. Single indicator label to
#'   place in the output \code{indicator} column. Defaults to
#'   \code{"subgroup_rate"}.
#' @param separator Character scalar. Separator used to construct
#'   composite subgroup-as-site IDs. Defaults to \code{"_"}.
#' @param level_override Must be \code{NULL} (default). Mixed-level override
#'   semantics are intentionally unsupported and any other value raises
#'   \code{sitemix_error_invalid_level_override}.
#' @param rtype_col Must be \code{NULL} (default). Declaring a publisher row-
#'   type column raises the same stable invalid-level condition as
#'   \code{level_override}; split the source into one homogeneous reporting
#'   level first.
#' @param partition_target Character scalar. Explicit partition
#'   estimand. One of \code{"none"} (default; D0 conditional-rate
#'   rows), \code{"denominator_composition"}, or
#'   \code{"case_composition"} (both return Scenario C count input).
#' @param partition_tolerance Non-negative numeric scalar. Absolute
#'   tolerance for composition partition checks against the
#'   required \code{ALL} row. Defaults to \code{0.5}.
#' @param suppression_col Character scalar or \code{NULL} (default
#'   \code{NULL}). Optional publisher suppression flag column.
#' @param suppression_flag_value Value or vector of values marking
#'   publisher suppression in \code{suppression_col}. Defaults to
#'   \code{""}.
#'
#' @return A tibble consumable by [sm_estimate_from_aggregates()]
#'   when \code{partition_target = "none"}, or by
#'   [sm_estimate_from_counts()] with
#'   \code{family = "multinomial"} when a composition target is
#'   requested. Schema for the default case:
#'   \describe{
#'     \item{\code{site_id}}{Composite identifier constructed by
#'       joining the source site and subgroup labels with
#'       \code{separator}.}
#'     \item{\code{year}}{Integer year (copied verbatim).}
#'     \item{\code{indicator}}{Character scalar (the \code{indicator}
#'       argument).}
#'     \item{\code{c_jt}, \code{n_jt}}{Numerator and denominator
#'       copied from \code{numerator_col} and \code{denominator_col}.}
#'     \item{\code{suppression_flag}}{Always-present logical. It is
#'       \code{TRUE} for rows flagged by the publisher and otherwise
#'       \code{FALSE}; when no \code{suppression_col} is supplied, all rows
#'       are \code{FALSE}.}
#'     \item{\code{framing}}{Character scalar; the framing label
#'       (\code{"subgroup_as_site"}).}
#'     \item{\code{source_site_id}, \code{source_subgroup}}{The
#'       original site and publisher subgroup labels, preserved for
#'       traceback. A recognized total alias is canonicalized only in the
#'       composite \code{site_id}; \code{source_subgroup} keeps its source
#'       spelling.}
#'   }
#'
#' @seealso
#' \itemize{
#'   \item \link[=sm_pivot_subgroups_to_indicators]{Framing Y helper}.
#'   \item \link[=sm_estimate_from_aggregates]{Aggregate wrapper} for the
#'     default output.
#'   \item \link[=sm_estimate_from_counts]{Counts wrapper} for composition
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

#' Pivot subgroup aggregate rows into subgroup-as-indicator input (Framing Y)
#'
#' @encoding UTF-8
#'
#' @description
#' `sm_pivot_subgroups_to_indicators()` implements \strong{Framing Y}
#' for school-by-subgroup aggregate rows: each subgroup becomes a D1
#' marginal indicator while the original site remains the
#' \code{site_id}. The \link[=sm_estimate_from_aggregates]{aggregate wrapper}
#' consumes this D1 output. For the alternative framing
#' where subgroups become composite sites, see the
#' \link[=sm_pivot_subgroups_to_sites]{Framing X helper}.
#'
#' @details
#' \strong{Framing X vs Framing Y.} The two pivot helpers solve the
#' same input problem but produce different schemas (see the
#' \link[=sm_pivot_subgroups_to_sites]{Framing X details} for the side-by-side
#' comparison). Pick Framing Y when the analyst's question is "what
#' does each site's subgroup profile look like?" — each subgroup
#' becomes a column-like marginal indicator and the downstream D1
#' estimator computes per-marginal SEs with working-independence
#' cross-marginal covariance. The fixed total-alias vocabulary documented for
#' [sm_pivot_subgroups_to_sites()] is normalized to canonical \code{"ALL"}
#' before duplicate and grid checks. Mixed-level routing is likewise
#' unsupported and must be split upstream.
#'
#' @inheritParams sm_pivot_subgroups_to_sites
#' @param indicator_set Character vector or \code{NULL} (default
#'   \code{NULL}). Optional subgroup labels to retain and order in
#'   the output. When \code{NULL}, labels are taken from first
#'   appearance in \code{subgroup_col}. Recognized total aliases in this
#'   vector are normalized to canonical \code{"ALL"}; alias collisions are
#'   rejected as duplicate indicators.
#' @param na_action Character scalar. Missing/suppressed row
#'   handling. One of \code{"drop_row"} (default; removes rows
#'   whose numerator or denominator is missing or whose suppression
#'   flag is present) or \code{"keep_na"} (keeps/inserts NA rows
#'   for downstream suppression handling).
#'
#' @return A tibble consumable by the
#'   \link[=sm_estimate_from_aggregates]{aggregate wrapper} with
#'   \code{family = "multivariate"} and
#'   \code{aggregate_case = "D1"}. Schema:
#'   \describe{
#'     \item{\code{site_id}}{Original source site identifier
#'       (preserved, not composite).}
#'     \item{\code{year}}{Integer year.}
#'     \item{\code{indicator}}{Character scalar; the subgroup label
#'       (each subgroup becomes a marginal indicator).}
#'     \item{\code{c_jt}, \code{n_jt}}{Numerator and denominator
#'       from \code{numerator_col} and \code{denominator_col}.}
#'     \item{\code{suppression_flag}}{Always-present logical. It is
#'       \code{TRUE} for publisher-flagged rows and otherwise \code{FALSE}.
#'       Without a source flag, observed rows are \code{FALSE}; synthesized
#'       incomplete-grid rows created by \code{na_action = "keep_na"} are
#'       \code{TRUE}.}
#'     \item{\code{framing}}{Character scalar; the framing label
#'       (\code{"subgroup_as_indicator"}).}
#'     \item{\code{source_subgroup}}{Original publisher subgroup label for
#'       observed rows; synthesized incomplete-grid rows carry
#'       \code{NA_character_}.}
#'   }
#'
#' @seealso
#' \itemize{
#'   \item \link[=sm_pivot_subgroups_to_sites]{Framing X helper}.
#'   \item \link[=sm_estimate_from_aggregates]{Aggregate wrapper} for the D1
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
