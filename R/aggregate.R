# Aggregation substrate -----------------------------------------------------

.sm_prepare_counts <- function(
  data,
  family,
  indicator = NULL,
  indicators = NULL,
  id_cols = c("site_id", "year"),
  from_counts = FALSE,
  na_action = "drop_rows"
) {
  .sm_validate_data_frame(data)
  family <- .sm_validate_family(family)
  .sm_validate_from_counts_arg(from_counts)

  if (isTRUE(from_counts)) {
    .sm_extract_counts_input(
      data = data,
      family = family,
      indicator = indicator,
      indicators = indicators,
      id_cols = id_cols
    )
  } else {
    .sm_aggregate_student_counts(
      data = data,
      family = family,
      indicator = indicator,
      indicators = indicators,
      id_cols = id_cols,
      na_action = na_action
    )
  }
}

.sm_aggregate_student_counts <- function(
  data,
  family,
  indicator = NULL,
  indicators = NULL,
  id_cols = c("site_id", "year"),
  na_action = "drop_rows"
) {
  family <- .sm_validate_family(family)
  .sm_validate_id_cols(data, id_cols)
  .sm_validate_na_action(na_action)

  if (identical(family, "binomial")) {
    .sm_aggregate_binomial_student(data, indicator, id_cols, na_action)
  } else if (identical(family, "multivariate")) {
    .sm_aggregate_multivariate_student(data, indicators, id_cols, na_action)
  } else {
    .sm_aggregate_multinomial_student(data, indicator, id_cols, na_action)
  }
}

.sm_aggregate_binomial_student <- function(data, indicator, id_cols, na_action) {
  .sm_validate_indicator_args(data, "binomial", indicator = indicator)
  complete <- .sm_validate_binary_column(data, indicator, na_action = na_action)
  work <- .sm_standardize_student_frame(data, id_cols, complete)
  .sm_require_retained_rows(work)

  value <- as.integer(data[[indicator]][complete])
  groups <- .sm_group_slices(work$site_id, work$year)
  count_col <- paste0("c_jt_", indicator)

  rows <- lapply(groups, function(idx) {
    data.frame(
      site_id = work$site_id[[idx[[1]]]],
      year = work$year[[idx[[1]]]],
      n_jt = as.integer(length(idx)),
      count = as.integer(sum(value[idx])),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  names(out)[names(out) == "count"] <- count_col
  .sm_finalize_count_table(
    out,
    family = "binomial",
    input_mode = "student_level",
    indicator_order = indicator,
    count_cols = count_col,
    pair_cols = character()
  )
}

.sm_aggregate_multivariate_student <- function(data, indicators, id_cols, na_action) {
  .sm_validate_indicator_args(data, "multivariate", indicators = indicators)
  complete <- .sm_validate_binary_indicators(data, indicators, na_action = na_action)
  work <- .sm_standardize_student_frame(data, id_cols, complete)
  .sm_require_retained_rows(work)

  y <- as.data.frame(data[complete, indicators, drop = FALSE])
  y[] <- lapply(y, as.integer)

  count_cols <- paste0("c_jt_", indicators)
  pair_cols <- .sm_pairwise_count_cols(indicators)
  pair_index <- utils::combn(seq_along(indicators), 2, simplify = FALSE)
  groups <- .sm_group_slices(work$site_id, work$year)

  rows <- lapply(groups, function(idx) {
    row <- data.frame(
      site_id = work$site_id[[idx[[1]]]],
      year = work$year[[idx[[1]]]],
      n_jt = as.integer(length(idx)),
      stringsAsFactors = FALSE
    )
    for (i in seq_along(indicators)) {
      row[[count_cols[[i]]]] <- as.integer(sum(y[[i]][idx]))
    }
    for (i in seq_along(pair_index)) {
      pair <- pair_index[[i]]
      row[[pair_cols[[i]]]] <- as.integer(sum(y[[pair[[1]]]][idx] * y[[pair[[2]]]][idx]))
    }
    row
  })

  .sm_finalize_count_table(
    do.call(rbind, rows),
    family = "multivariate",
    input_mode = "student_level",
    indicator_order = indicators,
    count_cols = count_cols,
    pair_cols = pair_cols
  )
}

.sm_aggregate_multinomial_student <- function(data, indicator, id_cols, na_action) {
  .sm_validate_indicator_args(data, "multinomial", indicator = indicator)
  complete <- .sm_validate_multinomial_column(data, indicator, na_action = na_action)
  work <- .sm_standardize_student_frame(data, id_cols, complete)
  .sm_require_retained_rows(work)

  categories <- .sm_multinomial_levels(data[[indicator]][complete])
  count_cols <- paste0("c_jt_", categories)
  value <- as.character(data[[indicator]][complete])
  groups <- .sm_group_slices(work$site_id, work$year)

  rows <- lapply(groups, function(idx) {
    row <- data.frame(
      site_id = work$site_id[[idx[[1]]]],
      year = work$year[[idx[[1]]]],
      n_jt = as.integer(length(idx)),
      stringsAsFactors = FALSE
    )
    for (i in seq_along(categories)) {
      row[[count_cols[[i]]]] <- as.integer(sum(value[idx] == categories[[i]]))
    }
    row
  })

  .sm_finalize_count_table(
    do.call(rbind, rows),
    family = "multinomial",
    input_mode = "student_level",
    indicator_order = categories,
    count_cols = count_cols,
    pair_cols = character()
  )
}

.sm_extract_counts_input <- function(
  data,
  family,
  indicator = NULL,
  indicators = NULL,
  id_cols = c("site_id", "year")
) {
  family <- .sm_validate_family(family)
  .sm_validate_id_cols(data, id_cols)
  schema <- .sm_validate_counts_input(
    data,
    family = family,
    indicator = indicator,
    indicators = indicators
  )

  if (identical(family, "multinomial") && !is.null(indicators)) {
    expected <- paste0("c_jt_", indicators)
    .sm_require_columns(data, expected)
    if (!setequal(expected, schema$count_cols)) {
      .sm_abort_input(
        "Explicit multinomial count levels must match the full category count set.",
        class = "sitemix_error_input_indicator_count",
        expected = schema$count_cols,
        actual = expected,
        fix = "Use `levels` only to reorder all emitted categories, not to drop categories."
      )
    }
    schema$count_cols <- expected
  }

  ids <- .sm_standardize_ids(data, id_cols)
  .sm_validate_unique_count_keys(ids$site_id, ids$year)

  needed <- c(schema$n_col, schema$count_cols, schema$pair_cols)
  out <- data.frame(
    site_id = ids$site_id,
    year = ids$year,
    stringsAsFactors = FALSE
  )
  for (col in needed) {
    out[[col]] <- as.integer(data[[col]])
  }
  out <- .sm_sort_count_rows(out)

  .sm_finalize_count_table(
    out,
    family = family,
    input_mode = "counts_full_suff",
    indicator_order = .sm_indicator_order_from_counts(family, indicator, indicators, schema$count_cols),
    count_cols = schema$count_cols,
    pair_cols = schema$pair_cols
  )
}

.sm_indicator_order_from_counts <- function(family, indicator, indicators, count_cols) {
  if (identical(family, "binomial")) {
    indicator
  } else if (identical(family, "multivariate")) {
    indicators
  } else {
    sub("^c_jt_", "", count_cols)
  }
}

.sm_finalize_count_table <- function(
  out,
  family,
  input_mode,
  indicator_order,
  count_cols,
  pair_cols
) {
  out <- .sm_sort_count_rows(out)
  out <- tibble::as_tibble(out)
  attr(out, "family") <- family
  attr(out, "input_mode") <- input_mode
  attr(out, "indicator_order") <- indicator_order
  attr(out, "count_cols") <- count_cols
  attr(out, "pair_cols") <- pair_cols
  out
}

.sm_standardize_student_frame <- function(data, id_cols, complete) {
  ids <- .sm_standardize_ids(data, id_cols)
  data.frame(
    site_id = ids$site_id[complete],
    year = ids$year[complete],
    stringsAsFactors = FALSE
  )
}

.sm_standardize_ids <- function(data, id_cols) {
  data.frame(
    site_id = as.character(data[[id_cols[[1]]]]),
    year = as.integer(data[[id_cols[[2]]]]),
    stringsAsFactors = FALSE
  )
}

.sm_group_slices <- function(site_id, year) {
  keys <- data.frame(site_id = site_id, year = year, stringsAsFactors = FALSE)
  keys <- keys[order(keys$site_id, keys$year), , drop = FALSE]
  key_values <- unique(keys)
  lapply(seq_len(nrow(key_values)), function(i) {
    which(site_id == key_values$site_id[[i]] & year == key_values$year[[i]])
  })
}

.sm_sort_count_rows <- function(out) {
  out[order(out$site_id, out$year), , drop = FALSE]
}

.sm_multinomial_levels <- function(x) {
  observed <- as.character(x)
  if (is.factor(x)) {
    out <- levels(x)
    out <- out[out %in% observed]
  } else {
    out <- sort(unique(observed))
  }
  if (length(out) < 2L) {
    .sm_abort_input(
      "Multinomial aggregation requires at least two retained categories.",
      class = "sitemix_error_input_indicator_count",
      expected = "two or more categories",
      actual = out,
      fix = "Check filtering and missingness before aggregation."
    )
  }
  out
}

.sm_require_retained_rows <- function(work) {
  if (nrow(work) == 0L) {
    .sm_abort_input(
      "No complete input rows remain after applying `na_action`.",
      class = "sitemix_error_input_missing",
      expected = "at least one complete row",
      actual = "zero complete rows",
      fix = "Use `na_action = \"error\"` to locate missing values or provide complete indicators."
    )
  }
  invisible(TRUE)
}

.sm_validate_unique_count_keys <- function(site_id, year) {
  key <- paste(site_id, year, sep = "\r")
  duplicated_key <- duplicated(key)
  if (any(duplicated_key)) {
    first <- which(duplicated_key)[[1]]
    .sm_abort_input(
      "Count input must contain one row per `(site_id, year)` cell.",
      class = "sitemix_error_input_indicator_count",
      expected = "unique site-year rows",
      actual = paste0("duplicate key at row ", first),
      row_identity = list(site_id = site_id[[first]], year = year[[first]]),
      fix = "Pre-aggregate duplicate count rows before calling `sm_estimate()`."
    )
  }
  invisible(TRUE)
}
