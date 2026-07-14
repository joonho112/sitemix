# Aggregate-input normalization --------------------------------------------

.sm_prepare_aggregate_input <- function(
  data,
  id_cols = c("site_id", "year"),
  indicator = NULL,
  indicators = NULL,
  numerator_col = NULL,
  denominator_col = NULL,
  indicator_col = NULL,
  subgroup_col = NULL,
  suppression_col = NULL,
  suppression_flag_value = "",
  suppression_when = NULL,
  aggregate_case = "auto",
  suppression = "drop",
  suppressed_n_strategy = "observed_n",
  suppressed_n_bound = NULL,
  min_n = 10L
) {
  .sm_validate_data_frame(data)
  .sm_validate_aggregate_id_cols(id_cols)
  .sm_validate_min_n(min_n)
  min_n <- as.integer(min_n)
  aggregate_case <- .sm_validate_aggregate_case_arg(aggregate_case)
  suppression <- .sm_validate_aggregate_suppression_arg(suppression)
  suppressed_n_strategy <- .sm_validate_suppressed_n_strategy(suppressed_n_strategy)

  mapped <- .sm_aggregate_apply_column_mapping(
    data = data,
    id_cols = id_cols,
    numerator_col = numerator_col,
    denominator_col = denominator_col,
    indicator_col = indicator_col,
    subgroup_col = subgroup_col,
    suppression_col = suppression_col
  )
  has_indicator_key <- "indicator" %in% names(mapped)
  has_wide_counts <- any(grepl("^c_jt_", names(mapped)))
  has_valid_scalar_indicator <- !is.null(indicator) &&
    is.character(indicator) &&
    length(indicator) == 1L &&
    !is.na(indicator) &&
    nzchar(indicator)
  if (!has_indicator_key && !has_wide_counts && has_valid_scalar_indicator) {
    mapped$indicator <- rep(indicator, nrow(mapped))
  }
  form <- .sm_detect_aggregate_form(mapped)
  out <- if (identical(form, "long")) {
    .sm_aggregate_normalize_long(mapped, indicator = indicator)
  } else {
    .sm_aggregate_normalize_wide(mapped, indicators = indicators)
  }

  out$suppression_flag <- .sm_aggregate_suppression_flag(
    mapped = mapped,
    form = form,
    out = out,
    suppression_flag_value = suppression_flag_value
  )
  out$denominator_observed <- !is.na(out$n_jt)
  out$aggregate_form <- form
  detection <- .sm_detect_aggregate_suppression(
    out,
    suppression_when = suppression_when,
    has_publisher_flag = "suppression_flag" %in% names(mapped)
  )
  out$flag_suppressed <- detection$flag_suppressed
  out$suppression_source <- detection$suppression_source

  .sm_validate_aggregate_rows(
    out,
    suppression = suppression,
    suppressed_n_strategy = suppressed_n_strategy,
    suppressed_n_bound = suppressed_n_bound,
    min_n = min_n
  )
  resolved <- .sm_resolve_aggregate_case(out$indicator, aggregate_case = aggregate_case)

  out$.aggregate_input_row <- NULL
  out <- tibble::as_tibble(out)
  attr(out, "aggregate_form") <- form
  attr(out, "aggregate_case") <- resolved$aggregate_case
  attr(out, "family") <- resolved$family
  attr(out, "input_mode") <- "aggregate"
  attr(out, "indicator_order") <- unique(out$indicator)
  attr(out, "suppression_detection_path") <- detection$suppression_detection_path
  attr(out, "n_suppressed") <- detection$n_suppressed
  attr(out, "denominator_observed_on_suppressed") <- detection$denominator_observed_on_suppressed
  attr(out, "has_hidden_denominator") <- any(is.na(out$c_jt) & is.na(out$n_jt))
  out
}

.sm_validate_aggregate_id_cols <- function(id_cols) {
  if (!is.character(id_cols) || length(id_cols) != 2L || anyNA(id_cols) || any(id_cols == "") || anyDuplicated(id_cols)) {
    .sm_abort_aggregate(
      "`id_cols` must contain exactly two distinct column names.",
      class = "sitemix_error_invalid_aggregate_schema",
      expected = "site and year column names",
      actual = as.character(id_cols),
      fix = "Use the aggregate site and year columns, usually `c(\"site_id\", \"year\")`."
    )
  }
  invisible(TRUE)
}

.sm_validate_aggregate_case_arg <- function(aggregate_case) {
  valid <- c("auto", "D0", "D1")
  if (!is.character(aggregate_case) || length(aggregate_case) != 1L || is.na(aggregate_case) || !aggregate_case %in% valid) {
    .sm_abort_argument(
      "`aggregate_case` must be one supported aggregate case.",
      class = "sitemix_error_invalid_aggregate_case",
      expected = valid,
      actual = as.character(aggregate_case),
      fix = "Use `aggregate_case = \"auto\"`, `\"D0\"`, or `\"D1\"`."
    )
  }
  aggregate_case
}

.sm_validate_sampling_relation_arg <- function(sampling_relation) {
  valid <- c("unknown", "same_units", "different_units")
  if (is.character(sampling_relation) &&
      length(sampling_relation) > 1L &&
      identical(sampling_relation, valid)) {
    sampling_relation <- valid[[1L]]
  }
  if (!is.character(sampling_relation) ||
      length(sampling_relation) != 1L ||
      is.na(sampling_relation) ||
      !sampling_relation %in% valid) {
    .sm_abort_argument(
      "`sampling_relation` must describe whether D1 marginals use the same observational units.",
      class = "sitemix_error_invalid_sampling_relation",
      expected = valid,
      actual = as.character(sampling_relation),
      location = list(argument = "sampling_relation"),
      fix = "Use `unknown`, `same_units`, or `different_units`; denominator equality is not sampling-unit provenance."
    )
  }
  sampling_relation
}

.sm_validate_aggregate_suppression_arg <- function(suppression) {
  valid <- c("drop", "upper_bound")
  if (!is.character(suppression) || length(suppression) != 1L || is.na(suppression) || !suppression %in% valid) {
    .sm_abort_aggregate(
      "`suppression` must be one supported aggregate suppression mode.",
      class = "sitemix_error_invalid_suppression_mode",
      expected = valid,
      actual = as.character(suppression),
      fix = "Use `suppression = \"drop\"` or `\"upper_bound\"`."
    )
  }
  suppression
}

.sm_validate_aggregate_framing_arg <- function(framing) {
  valid <- c("subgroup_as_site", "subgroup_as_indicator")
  if (is.null(framing) || (length(framing) == 1L && is.na(framing))) {
    return(NA_character_)
  }
  if (!is.character(framing) || length(framing) != 1L || !framing %in% valid) {
    .sm_abort_aggregate(
      "`framing` must be NA or one supported aggregate framing.",
      class = "sitemix_error_invalid_framing",
      expected = c(NA_character_, valid),
      actual = as.character(framing),
      fix = "Use `framing = NA` for direct D0 rows; subgroup pivots are implemented by dedicated helpers."
    )
  }
  framing
}

.sm_validate_suppressed_n_strategy <- function(suppressed_n_strategy) {
  valid <- c("observed_n", "worst_case_bound")
  if (!is.character(suppressed_n_strategy) || length(suppressed_n_strategy) != 1L || is.na(suppressed_n_strategy) || !suppressed_n_strategy %in% valid) {
    .sm_abort_aggregate(
      "`suppressed_n_strategy` must be one supported value.",
      class = "sitemix_error_invalid_suppressed_n",
      expected = valid,
      actual = as.character(suppressed_n_strategy),
      fix = "Use `observed_n` when denominators are published or `worst_case_bound` with a valid `suppressed_n_bound`."
    )
  }
  suppressed_n_strategy
}

.sm_aggregate_apply_column_mapping <- function(
  data,
  id_cols,
  numerator_col,
  denominator_col,
  indicator_col,
  subgroup_col,
  suppression_col
) {
  column_args <- c(
    numerator_col,
    denominator_col,
    indicator_col,
    subgroup_col,
    suppression_col
  )
  if (length(column_args) > 0L) {
    valid_args <- !is.na(column_args) & nzchar(column_args)
    if (!all(valid_args)) {
      .sm_abort_aggregate(
        "Aggregate column-mapping arguments must be non-empty column names.",
        class = "sitemix_error_invalid_aggregate_schema",
        expected = "non-empty source column names",
        actual = as.character(column_args),
        fix = "Remove empty mapping arguments or point them to columns in `data`."
      )
    }
    missing_mapped <- setdiff(column_args, names(data))
    if (length(missing_mapped) > 0L) {
      .sm_abort_aggregate(
        "Aggregate input is missing mapped source columns.",
        class = "sitemix_error_invalid_aggregate_schema",
        expected = column_args,
        actual = names(data),
        fix = paste0("Missing: ", .sm_cli_collapse(missing_mapped, quote = TRUE), ".")
      )
    }
  }

  mapping <- c(
    site_id = id_cols[[1]],
    year = id_cols[[2]],
    c_jt = numerator_col %||% "c_jt",
    n_jt = denominator_col %||% "n_jt",
    indicator = indicator_col %||% "indicator",
    subgroup = subgroup_col %||% "subgroup",
    suppression_flag = suppression_col %||% "suppression_flag"
  )
  required <- c(mapping[["site_id"]], mapping[["year"]])
  optional_sources <- unname(mapping[c("c_jt", "n_jt", "indicator", "subgroup", "suppression_flag")])
  required <- unique(c(required, optional_sources[optional_sources %in% names(data)]))
  missing_ids <- setdiff(c(mapping[["site_id"]], mapping[["year"]]), names(data))
  if (length(missing_ids) > 0L) {
    .sm_abort_aggregate(
      "Aggregate input is missing required identity columns.",
      class = "sitemix_error_invalid_aggregate_schema",
      expected = c(mapping[["site_id"]], mapping[["year"]]),
      actual = names(data),
      fix = paste0("Missing: ", .sm_cli_collapse(missing_ids, quote = TRUE), ".")
    )
  }

  out <- as.data.frame(data, stringsAsFactors = FALSE)
  for (target in names(mapping)) {
    source <- mapping[[target]]
    if (!is.null(source) && source %in% names(out) && !identical(source, target)) {
      out[[target]] <- out[[source]]
    }
  }
  out
}

.sm_detect_aggregate_form <- function(data) {
  has_long <- "indicator" %in% names(data)
  has_wide <- any(grepl("^c_jt_", names(data)))
  if (has_long && has_wide) {
    .sm_abort_aggregate(
      "Aggregate input contains both long-form and wide-form column patterns.",
      class = "sitemix_error_ambiguous_dispatch",
      expected = "either `indicator` + `c_jt`/`n_jt` or wide `c_jt_<indicator>` columns",
      actual = names(data),
      fix = "Drop the redundant form columns before aggregate normalization."
    )
  }
  if (!has_long && !has_wide) {
    .sm_abort_aggregate(
      "Aggregate input form cannot be detected.",
      class = "sitemix_error_ambiguous_dispatch",
      expected = "long `indicator` column or wide `c_jt_<indicator>` columns",
      actual = names(data),
      fix = "Supply canonical aggregate columns or column-mapping arguments."
    )
  }
  if (has_long) "long" else "wide"
}

.sm_aggregate_normalize_long <- function(data, indicator = NULL) {
  required <- c("site_id", "year", "indicator", "c_jt", "n_jt")
  .sm_aggregate_require_columns(data, required)
  out <- data.frame(
    site_id = .sm_aggregate_site_id(data$site_id),
    year = .sm_aggregate_year(data$year),
    indicator = .sm_aggregate_chr(data$indicator, "indicator"),
    subgroup = if ("subgroup" %in% names(data)) .sm_aggregate_chr(data$subgroup, "subgroup") else NA_character_,
    c_jt = .sm_aggregate_integerish(data$c_jt, "c_jt", allow_na = TRUE),
    n_jt = .sm_aggregate_integerish(data$n_jt, "n_jt", allow_na = TRUE),
    .aggregate_input_row = seq_len(nrow(data)),
    stringsAsFactors = FALSE
  )
  if (!is.null(indicator)) {
    if (!is.character(indicator) || length(indicator) != 1L || is.na(indicator) || indicator == "") {
      .sm_abort_argument(
        "`indicator` must be NULL or one aggregate indicator label.",
        class = "sitemix_error_invalid_indicator",
        expected = "NULL or one non-empty string",
        actual = as.character(indicator),
        fix = "Use an indicator value present in the aggregate input."
      )
    }
    out$indicator <- indicator
  }
  .sm_aggregate_sort(out)
}

.sm_aggregate_normalize_wide <- function(data, indicators = NULL) {
  .sm_aggregate_require_columns(data, c("site_id", "year"))
  count_cols <- grep("^c_jt_", names(data), value = TRUE)
  per_indicator_n_cols <- grep("^n_jt_", names(data), value = TRUE)
  if ("n_jt" %in% names(data) && length(per_indicator_n_cols) > 0L) {
    .sm_abort_aggregate(
      "Wide aggregate input contains both common and per-indicator denominator columns.",
      class = "sitemix_error_invalid_aggregate_schema",
      expected = "either common `n_jt` or per-indicator `n_jt_<indicator>` columns",
      actual = c("n_jt", per_indicator_n_cols),
      fix = "Drop one denominator representation before aggregate normalization."
    )
  }
  if (length(count_cols) == 0L) {
    .sm_abort_aggregate(
      "Wide aggregate input requires one or more `c_jt_<indicator>` columns.",
      class = "sitemix_error_invalid_aggregate_schema",
      expected = "`c_jt_<indicator>` columns",
      actual = names(data),
      fix = "Use long form or add wide aggregate numerator columns."
    )
  }
  indicator_order <- sub("^c_jt_", "", count_cols)
  if (!is.null(indicators)) {
    if (!is.character(indicators) || length(indicators) == 0L || anyNA(indicators) || any(indicators == "") || anyDuplicated(indicators)) {
      .sm_abort_argument(
        "`indicators` must be a non-empty character vector of distinct aggregate indicator labels.",
        class = "sitemix_error_invalid_indicators",
        expected = "distinct indicator labels",
        actual = as.character(indicators),
        fix = "Use the suffixes of the wide `c_jt_<indicator>` columns."
      )
    }
    expected <- paste0("c_jt_", indicators)
    .sm_aggregate_require_columns(data, expected)
    pair_cols <- intersect(.sm_pairwise_count_cols(indicators), names(data))
    if (length(pair_cols) > 0L) {
      .sm_abort_aggregate(
        "Aggregate wide input does not accept pairwise co-occurrence columns.",
        class = "sitemix_error_invalid_aggregate_schema",
        expected = "marginal `c_jt_<indicator>` columns only",
        actual = pair_cols,
        fix = "Use `from_counts = TRUE` for sufficient-count Scenario B inputs with pairwise co-occurrences."
      )
    }
    count_cols <- expected
    indicator_order <- indicators
  }

  site_id <- .sm_aggregate_site_id(data$site_id)
  year <- .sm_aggregate_year(data$year)
  rows <- vector("list", length(count_cols))
  for (i in seq_along(count_cols)) {
    indicator_i <- indicator_order[[i]]
    n_col <- paste0("n_jt_", indicator_i)
    n_value <- if ("n_jt" %in% names(data)) {
      data$n_jt
    } else if (n_col %in% names(data)) {
      data[[n_col]]
    } else {
      .sm_abort_aggregate(
        "Wide aggregate input is missing denominator columns.",
        class = "sitemix_error_invalid_aggregate_schema",
        expected = c("n_jt", n_col),
        actual = names(data),
        fix = "Provide common `n_jt` or one `n_jt_<indicator>` per numerator column."
      )
    }
    rows[[i]] <- data.frame(
      site_id = site_id,
      year = year,
      indicator = indicator_i,
      subgroup = NA_character_,
      c_jt = .sm_aggregate_integerish(data[[count_cols[[i]]]], count_cols[[i]], allow_na = TRUE),
      n_jt = .sm_aggregate_integerish(n_value, n_col, allow_na = TRUE),
      .aggregate_input_row = seq_along(site_id),
      stringsAsFactors = FALSE
    )
  }
  .sm_aggregate_sort(do.call(rbind, rows))
}

.sm_aggregate_suppression_flag <- function(mapped, form, out, suppression_flag_value) {
  if (!"suppression_flag" %in% names(mapped)) {
    return(rep(FALSE, nrow(out)))
  }
  flag <- mapped$suppression_flag
  .sm_validate_suppression_flag_col(flag)
  if (is.logical(flag)) {
    base <- !is.na(flag) & flag
  } else {
    .sm_validate_suppression_flag_value(suppression_flag_value)
    base <- !is.na(flag) & as.character(flag) %in% as.character(suppression_flag_value)
  }
  if (".aggregate_input_row" %in% names(out)) {
    return(as.logical(base[out$.aggregate_input_row]))
  }
  rep(as.logical(base), length(unique(out$indicator)))
}

.sm_validate_aggregate_rows <- function(
  x,
  suppression,
  suppressed_n_strategy,
  suppressed_n_bound,
  min_n
) {
  .sm_validate_aggregate_duplicate_keys(x)
  has_hidden_denominator <- any(is.na(x$c_jt) & is.na(x$n_jt))
  hidden_ok <- FALSE
  if (has_hidden_denominator) {
    hidden_ok <- .sm_validate_hidden_suppressed_n(
      suppression = suppression,
      suppressed_n_strategy = suppressed_n_strategy,
      suppressed_n_bound = suppressed_n_bound,
      min_n = min_n
    )
  }
  for (i in seq_len(nrow(x))) {
    c_i <- x$c_jt[[i]]
    n_i <- x$n_jt[[i]]
    if (is.na(c_i) && is.na(n_i)) {
      if (isTRUE(x$flag_suppressed[[i]]) && hidden_ok) {
        next
      }
      .sm_invalid_aggregate_row(
        x,
        i,
        "Suppressed rows with hidden denominators require an explicit Tier-1 flag and valid bound.",
        expected = "explicit suppression flag plus hidden-N upper-bound settings",
        actual = "(c_jt = NA, n_jt = NA)"
      )
    }
    if (!is.na(c_i) && is.na(n_i)) {
      .sm_invalid_aggregate_row(x, i, "`n_jt` cannot be missing when `c_jt` is observed.")
    }
    if (!is.na(n_i) && n_i <= 0L) {
      .sm_invalid_aggregate_row(x, i, "`n_jt` must be positive when observed.", expected = "n_jt > 0", actual = n_i)
    }
    if (!is.na(c_i) && c_i < 0L) {
      .sm_invalid_aggregate_row(x, i, "`c_jt` must be non-negative.", expected = "c_jt >= 0", actual = c_i)
    }
    if (!is.na(c_i) && !is.na(n_i) && c_i > n_i) {
      .sm_invalid_aggregate_row(x, i, "`c_jt` cannot exceed `n_jt`.", expected = "c_jt <= n_jt", actual = paste0(c_i, " > ", n_i))
    }
  }
  invisible(TRUE)
}

.sm_validate_aggregate_duplicate_keys <- function(x) {
  subgroup <- ifelse(is.na(x$subgroup), "<NA>", x$subgroup)
  key <- paste(x$site_id, x$year, x$indicator, subgroup, sep = "\r")
  if (anyDuplicated(key)) {
    i <- which(duplicated(key))[[1]]
    .sm_invalid_aggregate_row(
      x,
      i,
      "Aggregate rows must be unique by `(site_id, year, indicator, subgroup)`.",
      expected = "unique aggregate row identity",
      actual = paste(x$site_id[[i]], x$year[[i]], x$indicator[[i]], x$subgroup[[i]], sep = " / ")
    )
  }
  invisible(TRUE)
}

.sm_validate_hidden_suppressed_n <- function(
  suppression,
  suppressed_n_strategy,
  suppressed_n_bound,
  min_n
) {
  if (!identical(suppression, "upper_bound")) {
    return(FALSE)
  }
  if (!identical(suppressed_n_strategy, "worst_case_bound")) {
    .sm_abort_aggregate(
      "Hidden-denominator upper-bound rows require `suppressed_n_strategy = \"worst_case_bound\"`.",
      class = "sitemix_error_invalid_suppressed_n",
      expected = "`worst_case_bound`",
      actual = suppressed_n_strategy,
      fix = "Use a conservative `suppressed_n_bound` for rows with hidden denominators."
    )
  }
  .sm_validate_min_n(min_n)
  min_n <- as.integer(min_n)
  if (is.null(suppressed_n_bound) ||
      !is.numeric(suppressed_n_bound) ||
      length(suppressed_n_bound) != 1L ||
      is.na(suppressed_n_bound) ||
      !is.finite(suppressed_n_bound) ||
      suppressed_n_bound <= 0 ||
      suppressed_n_bound != as.integer(suppressed_n_bound) ||
      suppressed_n_bound > min_n) {
    .sm_abort_aggregate(
      "`suppressed_n_bound` must be a conservative positive integer for hidden-denominator upper bounds.",
      class = "sitemix_error_invalid_suppressed_n",
      expected = paste0("positive integer scalar <= `min_n` (", min_n, ")"),
      actual = as.character(suppressed_n_bound),
      fix = "Pass an explicit conservative hidden-denominator bound."
    )
  }
  TRUE
}

.sm_resolve_aggregate_case <- function(indicator, aggregate_case = "auto") {
  k <- length(unique(indicator))
  resolved <- if (identical(aggregate_case, "auto")) {
    if (k == 1L) "D0" else "D1"
  } else {
    aggregate_case
  }
  if (identical(resolved, "D0") && k != 1L) {
    .sm_abort_aggregate(
      "`aggregate_case = \"D0\"` requires exactly one indicator.",
      class = "sitemix_error_ambiguous_dispatch",
      expected = "one indicator",
      actual = paste0(k, " indicators"),
      fix = "Use `aggregate_case = \"D1\"` for multiple marginal indicators."
    )
  }
  if (identical(resolved, "D1") && k < 2L) {
    .sm_abort_aggregate(
      "`aggregate_case = \"D1\"` requires at least two indicators.",
      class = "sitemix_error_ambiguous_dispatch",
      expected = "two or more indicators",
      actual = paste0(k, " indicator"),
      fix = "Use `aggregate_case = \"D0\"` for single-indicator aggregate input."
    )
  }
  list(
    aggregate_case = resolved,
    family = if (identical(resolved, "D0")) "binomial" else "multivariate"
  )
}

.sm_aggregate_require_columns <- function(data, columns) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    .sm_abort_aggregate(
      "Aggregate input is missing required columns.",
      class = "sitemix_error_invalid_aggregate_schema",
      expected = columns,
      actual = names(data),
      fix = paste0("Missing: ", .sm_cli_collapse(missing, quote = TRUE), ".")
    )
  }
  invisible(TRUE)
}

.sm_aggregate_site_id <- function(x) {
  if (!(is.character(x) || is.factor(x)) || anyNA(x) || any(as.character(x) == "")) {
    .sm_abort_aggregate(
      "`site_id` must be a non-missing character or factor column.",
      class = "sitemix_error_invalid_aggregate_schema",
      expected = "character/factor site identifiers",
      actual = paste(class(x), collapse = "/"),
      fix = "Read publisher site IDs as strings to preserve leading zeros."
    )
  }
  as.character(x)
}

.sm_aggregate_year <- function(x) {
  if (!(is.integer(x) || is.numeric(x)) || anyNA(x) || any(!is.finite(x)) || any(x != as.integer(x))) {
    .sm_abort_aggregate(
      "`year` must be an integer-like column with no missing values.",
      class = "sitemix_error_invalid_aggregate_schema",
      expected = "integer-like year values",
      actual = paste(class(x), collapse = "/"),
      fix = "Use one explicit year value per aggregate row."
    )
  }
  as.integer(x)
}

.sm_aggregate_chr <- function(x, column) {
  if (!(is.character(x) || is.factor(x)) || anyNA(x) || any(as.character(x) == "")) {
    .sm_abort_aggregate(
      paste0("`", column, "` must be a non-missing character or factor column."),
      class = "sitemix_error_invalid_aggregate_schema",
      expected = "character/factor values",
      actual = paste(class(x), collapse = "/"),
      fix = "Preserve publisher labels as non-empty strings."
    )
  }
  as.character(x)
}

.sm_aggregate_integerish <- function(x, column, allow_na = FALSE) {
  if (!(is.integer(x) || is.numeric(x)) || any(!is.na(x) & !is.finite(x)) || any(!is.na(x) & x != as.integer(x)) || (!allow_na && anyNA(x))) {
    .sm_abort_aggregate(
      paste0("`", column, "` must be integer-like", if (allow_na) " or NA." else "."),
      class = "sitemix_error_invalid_aggregate_schema",
      expected = "integer-like values",
      actual = paste(class(x), collapse = "/"),
      fix = "Use whole-number aggregate counts and denominators."
    )
  }
  as.integer(x)
}

.sm_invalid_aggregate_row <- function(x, i, message, expected = NULL, actual = NULL) {
  .sm_abort_aggregate(
    message,
    class = "sitemix_error_invalid_aggregate_row",
    expected = expected,
    actual = actual,
    row_identity = list(
      site_id = x$site_id[[i]],
      year = x$year[[i]],
      indicator = x$indicator[[i]],
      row_index = i
    ),
    fix = "Inspect the aggregate numerator/denominator pair before estimation."
  )
}

.sm_aggregate_sort <- function(out) {
  out[order(out$site_id, out$year, out$indicator, out$subgroup, na.last = TRUE), , drop = FALSE]
}
