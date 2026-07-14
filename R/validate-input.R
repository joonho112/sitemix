# Input and argument validation helpers -------------------------------------

.sm_validate_arguments <- function(
  data,
  family,
  indicator = NULL,
  indicators = NULL,
  id_cols = c("site_id", "year"),
  vst = "arcsine",
  boundary_method = "wilson_floor",
  bias_correction = NULL,
  vjt = FALSE,
  min_n = 10L,
  accountability_n = 30L,
  fpc = NULL,
  anscombe = FALSE,
  from_counts = FALSE,
  na_action = "drop_rows",
  description = NULL
) {
  .sm_validate_data_frame(data)
  family <- .sm_validate_family(family)
  .sm_validate_from_counts_arg(from_counts)
  if (isTRUE(from_counts)) {
    .sm_validate_counts_input(data, family, indicator = indicator, indicators = indicators)
  } else {
    .sm_validate_indicator_args(data, family, indicator, indicators)
  }
  .sm_validate_id_cols(data, id_cols)
  .sm_validate_vst(vst)
  .sm_validate_boundary_method(boundary_method)
  .sm_validate_bias_correction(bias_correction)
  .sm_validate_vjt(vjt)
  .sm_validate_min_n(min_n)
  .sm_validate_accountability_n(accountability_n)
  .sm_validate_fpc_arg(fpc)
  .sm_validate_anscombe_arg(anscombe, vst)
  .sm_validate_anscombe_correction_args(
    anscombe = anscombe,
    boundary_method = boundary_method,
    bias_correction = bias_correction
  )
  .sm_validate_na_action(na_action)
  .sm_validate_description(description)

  invisible(TRUE)
}

.sm_validate_data_frame <- function(data) {
  if (!inherits(data, "data.frame")) {
    .sm_abort_input(
      "`data` must be a data frame or tibble.",
      class = "sitemix_error_input_class",
      expected = "data.frame",
      actual = paste(class(data), collapse = "/"),
      fix = "Pass a data frame-like object to `sm_estimate()`."
    )
  }
  invisible(TRUE)
}

.sm_validate_family <- function(family) {
  valid <- c("binomial", "multivariate", "multinomial")
  if (!is.character(family) || length(family) != 1L || is.na(family) || !family %in% valid) {
    .sm_abort_argument(
      "`family` must name one supported scenario.",
      class = "sitemix_error_invalid_family",
      expected = valid,
      actual = as.character(family),
      fix = "Use `binomial`, `multivariate`, or `multinomial`; D0/D1 belong to `aggregate_case` later."
    )
  }
  family
}

.sm_validate_vst <- function(vst) {
  valid <- c("arcsine", "logit", "none")
  if (!is.character(vst) || length(vst) != 1L || is.na(vst) || !vst %in% valid) {
    .sm_abort_argument(
      "`vst` must name one supported transform.",
      class = "sitemix_error_invalid_vst",
      expected = valid,
      actual = as.character(vst),
      fix = "Use `arcsine`, `logit`, or `none`."
    )
  }
  invisible(TRUE)
}

.sm_validate_vjt <- function(vjt) {
  if (!is.logical(vjt) || length(vjt) != 1L || is.na(vjt)) {
    .sm_abort_argument(
      "`vjt` must be TRUE or FALSE.",
      class = "sitemix_error_invalid_vjt",
      expected = c("TRUE", "FALSE"),
      actual = paste(class(vjt), collapse = "/"),
      fix = "Pass a scalar logical value."
    )
  }
  invisible(TRUE)
}

.sm_validate_min_n <- function(min_n) {
  if (!is.numeric(min_n) || length(min_n) != 1L || is.na(min_n) || !is.finite(min_n) || min_n <= 0 || min_n != as.integer(min_n)) {
    .sm_abort_argument(
      "`min_n` must be a positive integer scalar.",
      class = "sitemix_error_invalid_min_n",
      expected = "positive integer scalar",
      actual = as.character(min_n),
      fix = "Use a positive whole-number threshold."
    )
  }
  invisible(TRUE)
}

.sm_validate_accountability_n <- function(accountability_n) {
  if (!is.numeric(accountability_n) ||
      length(accountability_n) != 1L ||
      is.na(accountability_n) ||
      !is.finite(accountability_n) ||
      accountability_n <= 0 ||
      accountability_n != as.integer(accountability_n)) {
    .sm_abort_argument(
      "`accountability_n` must be a positive integer scalar.",
      class = "sitemix_error_invalid_accountability_n",
      expected = "positive integer scalar",
      actual = paste0(
        "value = ",
        if (length(accountability_n) == 0L) "<empty>" else paste(as.character(accountability_n), collapse = ", "),
        " (class: ", paste(class(accountability_n), collapse = "/"), ")"
      ),
      fix = "Pass a positive whole-number threshold (default `accountability_n = 30L`) that controls the `flag_below_accountability` column."
    )
  }
  invisible(TRUE)
}

.sm_validate_fpc_arg <- function(fpc, n = NULL) {
  if (is.null(fpc)) {
    return(invisible(TRUE))
  }
  if (!is.numeric(fpc) ||
      length(fpc) == 0L ||
      anyNA(fpc) ||
      any(!is.finite(fpc)) ||
      any(fpc < 1) ||
      any(fpc != floor(fpc))) {
    .sm_abort_argument(
      "`fpc` must be NULL or positive whole-number finite-population sizes.",
      class = "sitemix_error_invalid_fpc",
      expected = "NULL, one population size, or an input-row-aligned vector",
      actual = paste0("class = ", paste(class(fpc), collapse = "/"), "; length = ", length(fpc)),
      fix = "Use a scalar whole population size or repeat a group-constant size on every input row."
    )
  }
  if (!is.null(n)) {
    if (!length(fpc) %in% c(1L, length(n))) {
      .sm_abort_argument(
        "`fpc` must be scalar or aligned with `n`.",
        class = "sitemix_error_invalid_fpc",
        expected = paste0("length 1 or ", length(n)),
        actual = paste0("length ", length(fpc)),
        fix = "Use a scalar population size or one population size per sample-size cell."
      )
    }
    fpc_n <- rep_len(as.numeric(fpc), length(n))
    invalid <- fpc_n < n
  } else {
    invalid <- logical()
  }
  if (any(invalid)) {
    first <- which(invalid)[[1L]]
    .sm_abort_argument(
      "Finite-population SRSWOR requires `fpc >= n`.",
      class = "sitemix_error_invalid_fpc",
      expected = "population_size >= sample size",
      actual = paste0("fpc = ", fpc_n[[first]], ", n = ", n[[first]]),
      fix = "Use the fixed population size; equality is a valid census."
    )
  }
  invisible(TRUE)
}

.sm_validate_anscombe_arg <- function(anscombe, vst) {
  if (!is.logical(anscombe) || length(anscombe) != 1L || is.na(anscombe)) {
    .sm_abort_argument(
      "`anscombe` must be TRUE or FALSE.",
      class = "sitemix_error_invalid_anscombe",
      expected = c("TRUE", "FALSE"),
      actual = paste(class(anscombe), collapse = "/"),
      fix = "Pass a scalar logical value."
    )
  }
  if (isTRUE(anscombe) && !identical(vst, "arcsine")) {
    .sm_abort_argument(
      "`anscombe = TRUE` requires `vst = \"arcsine\"`.",
      class = "sitemix_error_anscombe_requires_arcsine",
      expected = "vst = \"arcsine\"",
      actual = paste0("vst = \"", vst, "\""),
      fix = "Set `anscombe = FALSE` or use the arcsine transform."
    )
  }
  invisible(TRUE)
}

.sm_validate_anscombe_correction_args <- function(
  anscombe,
  boundary_method,
  bias_correction
) {
  if (!isTRUE(anscombe)) {
    return(invisible(TRUE))
  }

  if (identical(boundary_method, "agresti_coull")) {
    .sm_abort_argument(
      "`anscombe = TRUE` is incompatible with Agresti-Coull boundary regularization.",
      class = "sitemix_error_anscombe_incompatible_correction",
      expected = "boundary_method = \"wilson_floor\" or \"none\"",
      actual = "boundary_method = \"agresti_coull\"",
      fix = "Use the Anscombe transform without Agresti-Coull regularization."
    )
  }

  if (identical(bias_correction, "binomial_bc")) {
    .sm_abort_argument(
      "`anscombe = TRUE` is incompatible with `binomial_bc`.",
      class = "sitemix_error_anscombe_incompatible_correction",
      expected = "bias_correction = NULL",
      actual = "bias_correction = \"binomial_bc\"",
      fix = "Use the Anscombe approximation without an additional n-1 correction."
    )
  }

  invisible(TRUE)
}

.sm_validate_from_counts_arg <- function(from_counts) {
  if (!is.logical(from_counts) || length(from_counts) != 1L || is.na(from_counts)) {
    .sm_abort_argument(
      "`from_counts` must be TRUE or FALSE.",
      class = "sitemix_error_invalid_from_counts",
      expected = c("TRUE", "FALSE"),
      actual = paste(class(from_counts), collapse = "/"),
      fix = "Pass a scalar logical value."
    )
  }
  invisible(TRUE)
}

.sm_validate_from_aggregates_arg <- function(from_aggregates) {
  if (!is.logical(from_aggregates) || length(from_aggregates) != 1L || is.na(from_aggregates)) {
    .sm_abort_argument(
      "`from_aggregates` must be TRUE or FALSE.",
      class = "sitemix_error_invalid_from_aggregates",
      expected = c("TRUE", "FALSE"),
      actual = paste(class(from_aggregates), collapse = "/"),
      fix = "Pass a scalar logical value."
    )
  }
  invisible(TRUE)
}

.sm_validate_suppressed_theta_hat <- function(suppressed_theta_hat) {
  if (!is.numeric(suppressed_theta_hat) ||
      length(suppressed_theta_hat) != 1L ||
      is.na(suppressed_theta_hat) ||
      !is.finite(suppressed_theta_hat) ||
      suppressed_theta_hat <= 0 ||
      suppressed_theta_hat >= 1) {
    .sm_abort_argument(
      "`suppressed_theta_hat` must be a finite raw-scale probability in (0, 1).",
      class = "sitemix_error_invalid_suppressed_theta_hat",
      expected = "one finite value with 0 < suppressed_theta_hat < 1",
      actual = as.character(suppressed_theta_hat),
      fix = "Use a conservative interior midpoint such as `0.5`."
    )
  }
  invisible(TRUE)
}

.sm_validate_suppression_sensitivity_acknowledge <- function(x) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .sm_abort_argument(
      "`suppression_sensitivity_acknowledge` must be TRUE or FALSE.",
      class = "sitemix_error_suppression_sensitivity_acknowledgement",
      expected = c("TRUE", "FALSE"),
      actual = paste(class(x), collapse = "/"),
      fix = "Pass TRUE only after accepting that `upper_bound` returns a non-identified variance-sensitivity scenario, not an estimate."
    )
  }
  invisible(TRUE)
}

.sm_require_suppression_sensitivity_acknowledgement <- function(
  suppression,
  has_suppressed,
  acknowledged,
  suppressed_theta_hat
) {
  .sm_validate_suppression_sensitivity_acknowledge(acknowledged)
  if (!identical(suppression, "upper_bound") || !isTRUE(has_suppressed)) {
    return(invisible(TRUE))
  }
  if (!isTRUE(acknowledged)) {
    .sm_abort_argument(
      "`suppression = \"upper_bound\"` is a non-identified variance-sensitivity scenario and requires explicit acknowledgement.",
      class = "sitemix_error_suppression_sensitivity_acknowledgement",
      expected = "suppression_sensitivity_acknowledge = TRUE",
      actual = "FALSE",
      fix = "Set the acknowledgement only if separated sensitivity fields, rather than canonical estimates or covariance, are appropriate."
    )
  }
  if (!isTRUE(all.equal(as.numeric(suppressed_theta_hat), 0.5, tolerance = 0))) {
    .sm_abort_argument(
      "`suppressed_theta_hat` is retained only as the worst-case Bernoulli variance probability and must equal 0.5.",
      class = "sitemix_error_invalid_suppressed_theta_hat",
      expected = "0.5",
      actual = as.character(suppressed_theta_hat),
      fix = "Use the default 0.5; it is recorded in `sensitivity_probability`, never in canonical estimate columns."
    )
  }
  invisible(TRUE)
}

.sm_validate_na_action <- function(na_action) {
  valid <- c("drop_rows", "error")
  if (!is.character(na_action) || length(na_action) != 1L || is.na(na_action) || !na_action %in% valid) {
    .sm_abort_argument(
      "`na_action` must be one supported NA policy.",
      class = "sitemix_error_invalid_na_action",
      expected = valid,
      actual = as.character(na_action),
      fix = "Use `drop_rows` or `error`."
    )
  }
  invisible(TRUE)
}

.sm_validate_description <- function(description) {
  if (!is.null(description) && (!is.character(description) || length(description) != 1L || is.na(description))) {
    .sm_abort_argument(
      "`description` must be NULL or a single string.",
      class = "sitemix_error_invalid_description",
      expected = "NULL or one character string",
      actual = paste(class(description), collapse = "/"),
      fix = "Use a single metadata label or leave `description = NULL`."
    )
  }
  invisible(TRUE)
}

.sm_validate_id_cols <- function(data, id_cols = c("site_id", "year")) {
  if (!is.character(id_cols) || length(id_cols) != 2L || anyNA(id_cols) || any(id_cols == "") || anyDuplicated(id_cols)) {
    .sm_abort_argument(
      "`id_cols` must be a two-element character vector.",
      class = "sitemix_error_invalid_id_cols",
      expected = "two distinct column names",
      actual = as.character(id_cols),
      fix = "Use `id_cols = c(\"site_id\", \"year\")` or an equivalent pair."
    )
  }
  .sm_require_columns(data, id_cols)

  site <- data[[id_cols[[1]]]]
  year <- data[[id_cols[[2]]]]
  if (!(is.character(site) || is.factor(site)) || anyNA(site)) {
    .sm_abort_input(
      "`site_id` column must be character or factor and non-missing.",
      class = "sitemix_error_input_type",
      expected = "non-missing character or factor",
      actual = paste(class(site), collapse = "/"),
      fix = "Convert site identifiers to character or factor before validation."
    )
  }
  if (!.sm_is_integerish(year) || anyNA(year)) {
    .sm_abort_input(
      "`year` column must contain integer years.",
      class = "sitemix_error_input_type",
      expected = "integer or whole-number numeric year",
      actual = paste(class(year), collapse = "/"),
      fix = "Convert year values to integer years before validation."
    )
  }

  invisible(TRUE)
}

.sm_validate_indicator_args <- function(data, family, indicator = NULL, indicators = NULL) {
  if (identical(family, "binomial") || identical(family, "multinomial")) {
    if (!is.character(indicator) || length(indicator) != 1L || is.na(indicator) || indicator == "") {
      .sm_abort_argument(
        "`indicator` must be a single column name for binomial and multinomial families.",
        class = "sitemix_error_invalid_indicator",
        expected = "one indicator column name",
        actual = as.character(indicator),
        fix = "Pass `indicator = \"...\"` and leave `indicators = NULL`."
      )
    }
    if (!is.null(indicators)) {
      .sm_abort_argument(
        "`indicators` must be NULL for binomial and multinomial families.",
        class = "sitemix_error_invalid_indicators",
        expected = "NULL",
        actual = as.character(indicators),
        fix = "Use `indicator` for single-column families."
      )
    }
    .sm_require_columns(data, indicator)
  } else {
    if (!is.null(indicator)) {
      .sm_abort_argument(
        "`indicator` must be NULL for the multivariate family.",
        class = "sitemix_error_invalid_indicator",
        expected = "NULL",
        actual = as.character(indicator),
        fix = "Use `indicators` for multivariate binary indicators."
      )
    }
    if (!is.character(indicators) || length(indicators) < 2L || anyNA(indicators) || any(indicators == "") || anyDuplicated(indicators)) {
      .sm_abort_argument(
        "`indicators` must contain at least two distinct column names.",
        class = "sitemix_error_invalid_indicators",
        expected = "two or more distinct indicator column names",
        actual = as.character(indicators),
        fix = "Pass indicators in the intended covariance order."
      )
    }
    .sm_require_columns(data, indicators)
  }

  invisible(TRUE)
}

.sm_validate_binary_column <- function(data, column, na_action = "drop_rows") {
  .sm_require_columns(data, column)
  .sm_validate_na_action(na_action)
  x <- data[[column]]
  if (!(is.logical(x) || is.numeric(x) || is.integer(x))) {
    .sm_abort_input(
      "Binary indicators must be logical or numeric 0/1.",
      class = "sitemix_error_input_type",
      expected = "logical or numeric/integer 0/1",
      actual = paste(class(x), collapse = "/"),
      fix = "Recode binary indicators explicitly; factors are not silently recoded."
    )
  }

  missing <- is.na(x)
  complete <- !missing
  values <- x[complete]
  if (length(values) > 0L && any(!values %in% c(0, 1, FALSE, TRUE))) {
    .sm_abort_input(
      "Binary indicators must contain only 0/1 or TRUE/FALSE values.",
      class = "sitemix_error_input_type",
      expected = "0/1 or TRUE/FALSE",
      actual = unique(as.character(values[!values %in% c(0, 1, FALSE, TRUE)])),
      fix = "Recode the indicator before calling `sm_estimate()`."
    )
  }
  .sm_handle_missing(missing, column = column, na_action = na_action)
  complete
}

.sm_validate_binary_indicators <- function(data, indicators, na_action = "drop_rows") {
  if (!is.character(indicators) || length(indicators) < 2L || anyNA(indicators) || anyDuplicated(indicators)) {
    .sm_abort_argument(
      "`indicators` must contain at least two distinct binary columns.",
      class = "sitemix_error_invalid_indicators",
      expected = "two or more distinct indicator column names",
      actual = as.character(indicators),
      fix = "Pass indicators in the intended covariance order."
    )
  }
  .sm_validate_na_action(na_action)

  masks <- lapply(indicators, function(col) {
    if (identical(na_action, "error")) {
      .sm_validate_binary_column(data, col, na_action = na_action)
    } else {
      withCallingHandlers(
        .sm_validate_binary_column(data, col, na_action = na_action),
        sitemix_warning_dropped_rows = function(w) {
          invokeRestart("muffleWarning")
        }
      )
    }
  })
  complete <- Reduce(`&`, masks)
  .sm_handle_missing(!complete, column = paste(indicators, collapse = ", "), na_action = na_action)
  complete
}

.sm_validate_multinomial_column <- function(data, column, na_action = "drop_rows") {
  .sm_require_columns(data, column)
  .sm_validate_na_action(na_action)
  x <- data[[column]]
  if (!(is.character(x) || is.factor(x))) {
    .sm_abort_input(
      "Multinomial indicators must be character or factor columns.",
      class = "sitemix_error_input_type",
      expected = "character or factor",
      actual = paste(class(x), collapse = "/"),
      fix = "Convert categories to character or factor before validation."
    )
  }
  missing <- is.na(x)
  .sm_handle_missing(missing, column = column, na_action = na_action)
  levels <- unique(as.character(x[!missing]))
  if (length(levels) < 2L) {
    .sm_abort_input(
      "Multinomial indicators require at least two observed categories.",
      class = "sitemix_error_input_indicator_count",
      expected = "two or more categories",
      actual = levels,
      fix = "Use `family = \"binomial\"` for a single binary category or check filtering."
    )
  }
  !missing
}

.sm_validate_counts_input <- function(
  data,
  family,
  indicator = NULL,
  indicators = NULL
) {
  .sm_validate_data_frame(data)
  family <- .sm_validate_family(family)
  .sm_require_columns(data, "n_jt")
  n <- .sm_validate_count_vector(data$n_jt, column = "n_jt", positive = TRUE)

  if (identical(family, "binomial")) {
    if (!is.character(indicator) || length(indicator) != 1L || is.na(indicator)) {
      .sm_abort_argument(
        "`indicator` is required for binomial count inputs.",
        class = "sitemix_error_invalid_indicator",
        expected = "one indicator name",
        actual = as.character(indicator),
        fix = "Pass the indicator name used in `c_jt_<indicator>`."
      )
    }
    cols <- paste0("c_jt_", indicator)
    .sm_require_columns(data, cols)
    .sm_validate_count_bounds(data[[cols]], n, cols)
    return(invisible(list(n_col = "n_jt", count_cols = cols, pair_cols = character())))
  }

  if (identical(family, "multivariate")) {
    if (!is.character(indicators) || length(indicators) < 2L || anyNA(indicators) || anyDuplicated(indicators)) {
      .sm_abort_argument(
        "`indicators` is required for multivariate count inputs.",
        class = "sitemix_error_invalid_indicators",
        expected = "two or more distinct indicators",
        actual = as.character(indicators),
        fix = "Pass indicators in the intended covariance order."
      )
    }
    count_cols <- paste0("c_jt_", indicators)
    pair_cols <- .sm_pairwise_count_cols(indicators)
    pair_index <- utils::combn(seq_along(indicators), 2, simplify = FALSE)
    .sm_require_columns(data, c(count_cols, pair_cols))
    for (col in count_cols) {
      .sm_validate_count_bounds(data[[col]], n, col)
    }
    for (i in seq_along(pair_cols)) {
      col <- pair_cols[[i]]
      .sm_validate_count_bounds(data[[col]], n, col)
      pair <- pair_index[[i]]
      first <- data[[count_cols[[pair[[1]]]]]]
      second <- data[[count_cols[[pair[[2]]]]]]
      if (any(data[[col]] > pmin(first, second))) {
        .sm_abort_input(
          "Pairwise co-occurrence counts cannot exceed either marginal count.",
          class = "sitemix_error_input_indicator_count",
          expected = paste0(col, " <= both marginals"),
          actual = col,
          fix = "Check pairwise count construction and indicator ordering."
        )
      }
      lower <- pmax(0, first + second - n)
      if (any(data[[col]] < lower)) {
        .sm_abort_input(
          "Pairwise co-occurrence counts are below the feasible lower bound.",
          class = "sitemix_error_input_indicator_count",
          expected = paste0(col, " >= max(0, C_k + C_l - n)"),
          actual = col,
          fix = "Check pairwise count construction and indicator ordering."
        )
      }
    }
    return(invisible(list(n_col = "n_jt", count_cols = count_cols, pair_cols = pair_cols)))
  }

  count_cols <- grep("^c_jt_", names(data), value = TRUE)
  if (length(count_cols) < 2L) {
    .sm_abort_input(
      "Multinomial count input requires at least two category count columns.",
      class = "sitemix_error_input_columns",
      expected = "two or more `c_jt_<category>` columns",
      actual = names(data),
      fix = "Provide one count column per emitted category."
    )
  }
  for (col in count_cols) {
    .sm_validate_count_bounds(data[[col]], n, col)
  }
  total <- Reduce(`+`, lapply(count_cols, function(col) data[[col]]))
  if (any(total != n)) {
    .sm_abort_input(
      "Multinomial category counts must sum to `n_jt` on every row.",
      class = "sitemix_error_input_indicator_count",
      expected = "sum category counts == n_jt",
      actual = "at least one row differs",
      fix = "Check category count columns and row filters before estimation."
    )
  }

  invisible(list(n_col = "n_jt", count_cols = count_cols, pair_cols = character()))
}

.sm_pairwise_count_cols <- function(indicators) {
  pairs <- .sm_multivariate_pair_index(indicators)
  vapply(pairs, function(pair) paste0("c_jt_", pair[[1]], "_", pair[[2]]), character(1))
}

.sm_multivariate_pair_index <- function(indicators) {
  if (length(indicators) < 2L) {
    return(list())
  }
  utils::combn(indicators, 2L, simplify = FALSE)
}

.sm_require_columns <- function(data, columns) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    .sm_abort_input(
      "Input data are missing required columns.",
      class = "sitemix_error_input_columns",
      expected = columns,
      actual = names(data),
      fix = paste0("Add missing columns: ", .sm_cli_collapse(missing, quote = TRUE), ".")
    )
  }
  invisible(TRUE)
}

.sm_handle_missing <- function(missing, column, na_action) {
  if (!any(missing)) {
    return(invisible(TRUE))
  }
  if (identical(na_action, "error")) {
    .sm_abort_input(
      "Input indicator columns contain missing values.",
      class = "sitemix_error_input_missing",
      expected = "no missing indicator values",
      actual = paste0(sum(missing), " missing in `", column, "`"),
      fix = "Drop or impute rows explicitly, or set `na_action = \"drop_rows\"`."
    )
  }
  .sm_warn(
    "Rows with missing indicator values will be dropped.",
    class = "sitemix_warning_dropped_rows",
    actual = paste0(sum(missing), " missing in `", column, "`"),
    fix = "Inspect the returned estimates and dropped-row diagnostics."
  )
  invisible(TRUE)
}

.sm_validate_count_vector <- function(x, column, positive = FALSE) {
  if (!.sm_is_integerish(x) || anyNA(x)) {
    .sm_abort_input(
      paste0("`", column, "` must contain integer-like counts."),
      class = "sitemix_error_input_type",
      expected = "integer-like numeric values",
      actual = paste(class(x), collapse = "/"),
      fix = "Use whole-number count columns."
    )
  }
  x <- as.numeric(x)
  if (any(x < 0) || (positive && any(x <= 0))) {
    .sm_abort_input(
      paste0("`", column, "` contains invalid count values."),
      class = "sitemix_error_input_indicator_count",
      expected = if (positive) "positive counts" else "non-negative counts",
      actual = paste(range(x), collapse = " to "),
      fix = "Check count aggregation before estimation."
    )
  }
  x
}

.sm_validate_count_bounds <- function(counts, n, column) {
  counts <- .sm_validate_count_vector(counts, column = column, positive = FALSE)
  if (length(counts) != length(n)) {
    .sm_abort_input(
      paste0("`", column, "` must have the same length as `n_jt`."),
      class = "sitemix_error_input_indicator_count",
      expected = paste0("length ", length(n)),
      actual = paste0("length ", length(counts)),
      fix = "Check count-column construction."
    )
  }
  if (any(counts > n)) {
    .sm_abort_input(
      paste0("`", column, "` cannot exceed `n_jt`."),
      class = "sitemix_error_input_indicator_count",
      expected = "count <= n_jt",
      actual = paste0("max(count - n_jt) = ", max(counts - n)),
      fix = "Check count aggregation before estimation."
    )
  }
  invisible(TRUE)
}

.sm_is_integerish <- function(x) {
  (is.integer(x) || is.numeric(x)) &&
    all(is.finite(x[!is.na(x)])) &&
    all(x[!is.na(x)] == floor(x[!is.na(x)]))
}
