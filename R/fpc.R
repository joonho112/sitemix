# Finite-population normalization and provenance ---------------------------

.sm_fpc_group_key <- function(site_id, year) {
  site_id <- as.character(site_id)
  year <- as.integer(year)
  paste0(nchar(site_id), ":", site_id, "\r", year)
}

.sm_normalize_fpc_by_group <- function(data, fpc, id_cols, groups, n) {
  if (is.null(fpc)) {
    return(NULL)
  }
  .sm_validate_fpc_arg(fpc)
  if (!inherits(data, "data.frame") || !inherits(groups, "data.frame")) {
    .sm_abort_argument(
      "Finite-population normalization requires data-frame inputs.",
      class = "sitemix_error_invalid_fpc",
      expected = "input data and normalized group table",
      actual = c(class(data), class(groups)),
      fix = "Pass `fpc` through a public sitemix estimation function."
    )
  }
  if (length(n) != nrow(groups)) {
    .sm_abort_argument(
      "Finite-population sample sizes must align with normalized groups.",
      class = "sitemix_error_invalid_fpc",
      expected = paste0(nrow(groups), " sample sizes"),
      actual = paste0(length(n), " sample sizes"),
      fix = "Check group normalization before applying finite-population correction."
    )
  }

  input_ids <- .sm_standardize_ids(data, id_cols)
  input_key <- .sm_fpc_group_key(input_ids$site_id, input_ids$year)
  group_key <- .sm_fpc_group_key(groups$site_id, groups$year)

  if (length(fpc) == 1L) {
    population_size <- rep(as.numeric(fpc), nrow(groups))
  } else {
    if (length(fpc) != nrow(data)) {
      .sm_abort_argument(
        "A non-scalar `fpc` must be aligned with the input rows.",
        class = "sitemix_error_invalid_fpc",
        expected = paste0("length 1 or nrow(data) = ", nrow(data)),
        actual = paste0("length ", length(fpc)),
        fix = "Use a scalar population size or `fpc = data$population_size`; do not rely on implicit group order."
      )
    }
    input_groups <- split(seq_along(input_key), input_key)
    group_population <- vapply(input_groups, function(index) {
      values <- unique(as.numeric(fpc[index]))
      if (length(values) != 1L) {
        first <- index[[1L]]
        .sm_abort_argument(
          "Input-row-aligned `fpc` must be constant within each site-year group.",
          class = "sitemix_error_invalid_fpc",
          expected = "one population size per site-year group",
          actual = values,
          fix = "Repeat the same finite-population size on every input row in a group.",
          site_id = input_ids$site_id[[first]],
          year = input_ids$year[[first]]
        )
      }
      values[[1L]]
    }, numeric(1))
    matched <- match(group_key, names(group_population))
    if (anyNA(matched)) {
      .sm_abort_argument(
        "A normalized group could not be matched to input-row-aligned `fpc`.",
        class = "sitemix_error_invalid_fpc",
        expected = "one keyed population size for every retained group",
        actual = unique(group_key[is.na(matched)]),
        fix = "Check `id_cols` and the population-size vector alignment."
      )
    }
    population_size <- unname(group_population[matched])
  }

  n <- as.numeric(n)
  if (any(!is.finite(n)) || any(n < 1) || any(n != floor(n))) {
    .sm_abort_argument(
      "Retained sample sizes must be positive whole numbers before applying FPC.",
      class = "sitemix_error_invalid_fpc",
      expected = "positive whole-number n",
      actual = n,
      fix = "Resolve missing or synthetic denominators before requesting SRSWOR correction."
    )
  }
  invalid <- population_size < n
  if (any(invalid)) {
    first <- which(invalid)[[1L]]
    .sm_abort_argument(
      "Finite-population SRSWOR requires `fpc >= n` in every retained group.",
      class = "sitemix_error_invalid_fpc",
      expected = "population_size >= sample size",
      actual = paste0("fpc = ", population_size[[first]], ", n = ", n[[first]]),
      fix = "Use the fixed population size for each site-year; `fpc = n` is a valid census.",
      site_id = as.character(groups$site_id[[first]]),
      year = as.integer(groups$year[[first]])
    )
  }

  data.frame(
    site_id = as.character(groups$site_id),
    year = as.integer(groups$year),
    population_size = as.numeric(population_size),
    sampling_fraction = n / population_size,
    fpc_variance_multiplier = .sm_fpc_variance_multiplier(n, population_size),
    fpc_se_multiplier = .sm_fpc_multiplier(n, population_size),
    sampling_design = rep("SRSWOR", length(n)),
    stringsAsFactors = FALSE
  )
}

.sm_fpc_row_columns <- function(n, fpc, var_method_raw) {
  if (is.null(fpc)) {
    return(NULL)
  }
  q <- .sm_fpc_variance_multiplier(n, fpc)
  design_corrected <- identical(var_method_raw, "binomial_bc")
  applied <- if (design_corrected) {
    .sm_fpc_design_variance_multiplier(n, fpc)
  } else {
    q
  }
  list(
    population_size = as.numeric(fpc),
    sampling_fraction = as.numeric(n / fpc),
    fpc_variance_multiplier = as.numeric(q),
    fpc_se_multiplier = as.numeric(sqrt(q)),
    variance_multiplier_applied = as.numeric(applied),
    se_multiplier_applied = as.numeric(sqrt(applied)),
    sampling_design = "SRSWOR",
    variance_rule = if (design_corrected) "design_corrected" else "plugin"
  )
}

.sm_vcov_fpc_metadata <- function(
  n,
  fpc = NULL,
  variance_rule = "plugin",
  K = length(n)
) {
  if (!is.numeric(n) || length(n) == 0L || anyNA(n) ||
      any(!is.finite(n)) || any(n < 1)) {
    .sm_abort_vcov(
      "Covariance FPC metadata requires positive sample sizes.",
      class = "sitemix_error_vcov_invariant",
      expected = "positive finite n aligned to covariance coordinates",
      actual = n,
      fix = "Pass the site-year sample size for each covariance coordinate."
    )
  }
  if (!is.numeric(K) || length(K) != 1L || is.na(K) ||
      !is.finite(K) || K < 1 || K != floor(K)) {
    .sm_abort_vcov(
      "Covariance FPC metadata requires a positive matrix dimension.",
      class = "sitemix_error_vcov_invariant",
      expected = "positive whole-number K",
      actual = K,
      fix = "Use the covariance matrix dimension for `K`."
    )
  }
  K <- as.integer(K)
  if (!length(n) %in% c(1L, K)) {
    .sm_abort_vcov(
      "Covariance sample sizes must be scalar or coordinate-aligned.",
      class = "sitemix_error_vcov_invariant",
      expected = paste0("length 1 or K = ", K),
      actual = paste0("length ", length(n)),
      fix = "Use one common sample size or one size per covariance coordinate."
    )
  }
  n <- rep_len(as.numeric(n), K)

  if (!is.character(variance_rule) || length(variance_rule) == 0L ||
      anyNA(variance_rule) ||
      any(!variance_rule %in% c("plugin", "design_corrected")) ||
      !length(variance_rule) %in% c(1L, K)) {
    .sm_abort_vcov(
      "Covariance variance rules must be scalar or coordinate-aligned.",
      class = "sitemix_error_vcov_invariant",
      expected = c("plugin", "design_corrected"),
      actual = as.character(variance_rule),
      fix = "Record the matrix rule or one rule per working-independence diagonal."
    )
  }
  variance_rule <- rep_len(variance_rule, K)

  if (is.null(fpc)) {
    return(list(
      population_size = NA_real_,
      sampling_fraction = rep(NA_real_, K),
      fpc_variance_multiplier = rep(1, K),
      fpc_se_multiplier = rep(1, K),
      variance_multiplier_applied = rep(1, K),
      se_multiplier_applied = rep(1, K),
      sampling_design = "not_specified",
      variance_rule = variance_rule
    ))
  }

  .sm_validate_fpc_arg(fpc, n = n)
  if (!length(fpc) %in% c(1L, K)) {
    .sm_abort_vcov(
      "Covariance population sizes must be scalar or coordinate-aligned.",
      class = "sitemix_error_vcov_invariant",
      expected = paste0("length 1 or K = ", K),
      actual = paste0("length ", length(fpc)),
      fix = "Use the keyed site-year population size for all covariance coordinates."
    )
  }
  population <- rep_len(as.numeric(fpc), K)
  if (length(unique(population)) != 1L) {
    .sm_abort_vcov(
      "A site-year covariance object must have one fixed population size.",
      class = "sitemix_error_vcov_invariant",
      expected = "one population_size per site-year",
      actual = population,
      fix = "Normalize input-row population sizes by site-year before covariance construction."
    )
  }
  q <- .sm_fpc_variance_multiplier(n, population)
  applied <- q
  corrected <- variance_rule == "design_corrected"
  if (any(corrected)) {
    applied[corrected] <- .sm_fpc_design_variance_multiplier(
      n[corrected],
      population[corrected]
    )
  }

  list(
    population_size = population[[1L]],
    sampling_fraction = n / population,
    fpc_variance_multiplier = q,
    fpc_se_multiplier = sqrt(q),
    variance_multiplier_applied = applied,
    se_multiplier_applied = sqrt(applied),
    sampling_design = "SRSWOR",
    variance_rule = variance_rule
  )
}
