# Scenario B SUR covariance helpers ----------------------------------------

.sm_multivariate_sur_from_count_row <- function(
  row,
  indicators,
  count_cols = paste0("c_jt_", indicators),
  pair_cols = .sm_pairwise_count_cols(indicators),
  boundary_method = "wilson_floor",
  bias_correction = NULL,
  fpc = NULL,
  z = stats::qnorm(0.975)
) {
  if (!inherits(row, "data.frame") || nrow(row) != 1L) {
    .sm_abort_input(
      "`row` must be a single count-table row.",
      class = "sitemix_error_input_indicator_count",
      expected = "one-row data frame",
      actual = paste(class(row), collapse = "/"),
      fix = "Call the SUR helper one `(site_id, year)` count row at a time."
    )
  }
  .sm_require_columns(row, c("n_jt", count_cols, pair_cols))

  input_mode <- attr(row, "input_mode", exact = TRUE)

  .sm_multivariate_sur_from_counts(
    n = row$n_jt[[1]],
    marginal_counts = unname(unlist(row[count_cols], use.names = FALSE)),
    pair_counts = unname(unlist(row[pair_cols], use.names = FALSE)),
    indicators = indicators,
    boundary_method = boundary_method,
    bias_correction = bias_correction,
    fpc = fpc,
    z = z,
    jointly_observed = identical(input_mode, "student_level")
  )
}

.sm_multivariate_sur_from_counts <- function(
  n,
  marginal_counts,
  pair_counts,
  indicators,
  boundary_method = "wilson_floor",
  bias_correction = NULL,
  fpc = NULL,
  z = stats::qnorm(0.975),
  jointly_observed = FALSE
) {
  indicators <- .sm_validate_sur_indicators(indicators)
  .sm_validate_boundary_method(boundary_method)
  .sm_validate_bias_correction(bias_correction)
  n <- .sm_validate_sur_n(n)
  marginal_counts <- .sm_validate_sur_count_vector(marginal_counts, n = n, expected_length = length(indicators), column = "marginal_counts")
  pair_counts <- .sm_validate_sur_count_vector(
    pair_counts,
    n = n,
    expected_length = choose(length(indicators), 2L),
    column = "pair_counts"
  )
  .sm_validate_sur_pair_bounds(pair_counts, marginal_counts = marginal_counts, n = n, indicators = indicators)
  .sm_validate_sur_joint_feasibility(
    pair_counts,
    marginal_counts = marginal_counts,
    n = n,
    indicators = indicators,
    jointly_observed = jointly_observed
  )

  theta_raw <- marginal_counts / n
  rho <- .sm_pairwise_rate_matrix(pair_counts, theta_raw = theta_raw, n = n, indicators = indicators)
  mat <- matrix(0, length(indicators), length(indicators), dimnames = list(indicators, indicators))
  diag(mat) <- theta_raw * (1 - theta_raw) / n

  pairs <- utils::combn(seq_along(indicators), 2L, simplify = FALSE)
  for (i in seq_along(pairs)) {
    pair <- pairs[[i]]
    covariance <- (rho[pair[[1]], pair[[2]]] - theta_raw[[pair[[1]]]] * theta_raw[[pair[[2]]]]) / n
    mat[pair[[1]], pair[[2]]] <- covariance
    mat[pair[[2]], pair[[1]]] <- covariance
  }

  boundary <- marginal_counts == 0L | marginal_counts == n
  scalar_correction_rule <- rep("none", length(indicators))
  matrix_boundary_rule <- "none"
  variance_rule <- if (identical(bias_correction, "binomial_bc")) {
    "design_corrected"
  } else {
    "plugin"
  }

  if (identical(variance_rule, "design_corrected")) {
    one_unit_census <- n == 1L && !is.null(fpc) &&
      length(fpc) == 1L && isTRUE(as.numeric(fpc) == 1)
    if (n <= 1L && !one_unit_census) {
      .sm_abort_estimate(
        "Scenario B whole-matrix binomial correction requires n > 1.",
        class = "sitemix_error_estimate_var_method",
        expected = "n > 1",
        actual = n,
        fix = "Use the plug-in matrix for a one-unit group."
      )
    }
    if (!one_unit_census) {
      mat <- mat * n / (n - 1)
    }
    scalar_correction_rule[!boundary] <- "binomial_bc"
  }

  matrix_multiplier <- if (identical(variance_rule, "design_corrected")) {
    .sm_fpc_design_variance_multiplier(n, fpc = fpc)
  } else {
    .sm_fpc_variance_multiplier(n, fpc = fpc)
  }
  mat <- mat * matrix_multiplier

  if (any(boundary) && identical(boundary_method, "wilson_floor")) {
    boundary_q <- .sm_fpc_variance_multiplier(n, fpc = fpc)
    diag(mat)[boundary] <-
      .sm_wilson_se(theta_raw[boundary], n, z = z)^2 * boundary_q
    scalar_correction_rule[boundary] <- "wilson_boundary_surrogate"
    matrix_boundary_rule <- "diagonal_boundary_floor"
  } else if (any(boundary) && identical(boundary_method, "agresti_coull")) {
    .sm_abort_estimate(
      "Agresti-Coull boundary adjustment is not defined for Scenario B matrix output.",
      class = "sitemix_error_estimate_vcov_invariant",
      expected = "no boundary cells, or boundary_method = \"wilson_floor\" / \"none\"",
      actual = paste(indicators[boundary], collapse = ", "),
      fix = "Use `boundary_method = \"wilson_floor\"` for `vjt = TRUE`, or request scalar-only output."
    )
  }

  mat <- (mat + t(mat)) / 2
  .sm_validate_vcov_psd(mat)
  metadata_rule <- rep(variance_rule, length(indicators))
  if (any(boundary)) {
    metadata_rule[boundary] <- "plugin"
  }
  design <- .sm_vcov_fpc_metadata(
    n = n,
    fpc = fpc,
    variance_rule = metadata_rule,
    K = length(indicators)
  )

  list(
    theta_raw = stats::setNames(as.numeric(theta_raw), indicators),
    rho = rho,
    V_raw = mat,
    se_raw = stats::setNames(sqrt(pmax(diag(mat), 0)), indicators),
    boundary = stats::setNames(boundary, indicators),
    matrix_boundary_rule = matrix_boundary_rule,
    scalar_correction_rule = stats::setNames(scalar_correction_rule, indicators),
    psd_repair = "none",
    matrix_rank = .sm_matrix_rank(mat),
    positive_support = NA_integer_,
    n_jt = as.integer(n),
    n_eff = as.numeric(n),
    population_size = design$population_size,
    sampling_fraction = design$sampling_fraction,
    fpc_variance_multiplier = design$fpc_variance_multiplier,
    fpc_se_multiplier = design$fpc_se_multiplier,
    variance_multiplier_applied = design$variance_multiplier_applied,
    se_multiplier_applied = design$se_multiplier_applied,
    sampling_design = design$sampling_design,
    variance_rule = design$variance_rule,
    diag_contract = "row_se_raw_squared"
  )
}

.sm_multivariate_sur_from_matrix <- function(
  Y,
  indicators = colnames(Y),
  boundary_method = "wilson_floor",
  bias_correction = NULL,
  fpc = NULL,
  z = stats::qnorm(0.975)
) {
  indicators <- .sm_validate_sur_indicators(indicators)
  .sm_validate_boundary_method(boundary_method)
  .sm_validate_bias_correction(bias_correction)
  if (!(is.matrix(Y) || inherits(Y, "data.frame"))) {
    .sm_abort_input(
      "`Y` must be a matrix or data frame of binary indicators.",
      class = "sitemix_error_input_class",
      expected = "matrix or data.frame",
      actual = paste(class(Y), collapse = "/"),
      fix = "Pass one site-year cell with one column per indicator."
    )
  }
  if (ncol(Y) != length(indicators)) {
    .sm_abort_argument(
      "`indicators` must match the columns of `Y`.",
      class = "sitemix_error_invalid_indicators",
      expected = paste0(ncol(Y), " indicator names"),
      actual = paste0(length(indicators), " indicator names"),
      fix = "Pass indicator names in the same order as the columns of `Y`."
    )
  }
  Y <- as.matrix(Y)
  if (!is.numeric(Y) && !is.logical(Y) && !is.integer(Y)) {
    .sm_abort_input(
      "`Y` must contain binary 0/1 values.",
      class = "sitemix_error_input_type",
      expected = "numeric, integer, or logical matrix",
      actual = paste(class(Y), collapse = "/"),
      fix = "Recode binary indicators before covariance construction."
    )
  }
  if (nrow(Y) == 0L || anyNA(Y) || any(!is.finite(Y)) || any(!(Y %in% c(0, 1, FALSE, TRUE)))) {
    .sm_abort_input(
      "`Y` must contain complete binary rows.",
      class = "sitemix_error_input_indicator_count",
      expected = "one or more complete 0/1 rows",
      actual = "empty, missing, non-finite, or non-binary values",
      fix = "Apply missingness handling before covariance construction."
    )
  }

  Y <- matrix(as.integer(Y), ncol = length(indicators), dimnames = list(NULL, indicators))
  n <- nrow(Y)
  theta_raw <- colMeans(Y)
  residual <- sweep(Y, 2L, theta_raw)
  mat <- crossprod(residual) / n^2
  dimnames(mat) <- list(indicators, indicators)
  rho <- crossprod(Y) / n
  dimnames(rho) <- list(indicators, indicators)

  marginal_counts <- colSums(Y)
  boundary <- marginal_counts == 0L | marginal_counts == n
  scalar_correction_rule <- rep("none", length(indicators))
  matrix_boundary_rule <- "none"
  variance_rule <- if (identical(bias_correction, "binomial_bc")) {
    "design_corrected"
  } else {
    "plugin"
  }

  if (identical(variance_rule, "design_corrected")) {
    one_unit_census <- n == 1L && !is.null(fpc) &&
      length(fpc) == 1L && isTRUE(as.numeric(fpc) == 1)
    if (n <= 1L && !one_unit_census) {
      .sm_abort_estimate(
        "Scenario B whole-matrix binomial correction requires n > 1.",
        class = "sitemix_error_estimate_var_method",
        expected = "n > 1",
        actual = n,
        fix = "Use the plug-in matrix for a one-unit group."
      )
    }
    if (!one_unit_census) {
      mat <- mat * n / (n - 1)
    }
    scalar_correction_rule[!boundary] <- "binomial_bc"
  }

  matrix_multiplier <- if (identical(variance_rule, "design_corrected")) {
    .sm_fpc_design_variance_multiplier(n, fpc = fpc)
  } else {
    .sm_fpc_variance_multiplier(n, fpc = fpc)
  }
  mat <- mat * matrix_multiplier

  if (any(boundary) && identical(boundary_method, "wilson_floor")) {
    boundary_q <- .sm_fpc_variance_multiplier(n, fpc = fpc)
    diag(mat)[boundary] <-
      .sm_wilson_se(theta_raw[boundary], n, z = z)^2 * boundary_q
    scalar_correction_rule[boundary] <- "wilson_boundary_surrogate"
    matrix_boundary_rule <- "diagonal_boundary_floor"
  } else if (any(boundary) && identical(boundary_method, "agresti_coull")) {
    .sm_abort_estimate(
      "Agresti-Coull boundary adjustment is not defined for Scenario B matrix output.",
      class = "sitemix_error_estimate_vcov_invariant",
      expected = "no boundary cells, or boundary_method = \"wilson_floor\" / \"none\"",
      actual = paste(indicators[boundary], collapse = ", "),
      fix = "Use `boundary_method = \"wilson_floor\"` for `vjt = TRUE`, or request scalar-only output."
    )
  }

  mat <- (mat + t(mat)) / 2
  .sm_validate_vcov_psd(mat)
  metadata_rule <- rep(variance_rule, length(indicators))
  if (any(boundary)) {
    metadata_rule[boundary] <- "plugin"
  }
  design <- .sm_vcov_fpc_metadata(
    n = n,
    fpc = fpc,
    variance_rule = metadata_rule,
    K = length(indicators)
  )

  list(
    theta_raw = stats::setNames(as.numeric(theta_raw), indicators),
    rho = rho,
    V_raw = mat,
    se_raw = stats::setNames(sqrt(pmax(diag(mat), 0)), indicators),
    boundary = stats::setNames(boundary, indicators),
    matrix_boundary_rule = matrix_boundary_rule,
    scalar_correction_rule = stats::setNames(scalar_correction_rule, indicators),
    psd_repair = "none",
    matrix_rank = .sm_matrix_rank(mat),
    positive_support = NA_integer_,
    n_jt = as.integer(n),
    n_eff = as.numeric(n),
    population_size = design$population_size,
    sampling_fraction = design$sampling_fraction,
    fpc_variance_multiplier = design$fpc_variance_multiplier,
    fpc_se_multiplier = design$fpc_se_multiplier,
    variance_multiplier_applied = design$variance_multiplier_applied,
    se_multiplier_applied = design$se_multiplier_applied,
    sampling_design = design$sampling_design,
    variance_rule = design$variance_rule,
    diag_contract = "row_se_raw_squared"
  )
}

.sm_multivariate_vcov_from_sur <- function(
  sur,
  site_id,
  year,
  indicators,
  estimate_scale,
  vcov_scale = "raw"
) {
  sm_vcov(
    matrix = sur$V_raw,
    site_id = site_id,
    year = year,
    indicator_order = indicators,
    family = "multivariate",
    vcov_method = "sur",
    estimate_scale = estimate_scale,
    vcov_scale = vcov_scale,
    matrix_boundary_rule = sur$matrix_boundary_rule,
    scalar_correction_rule = unname(sur$scalar_correction_rule),
    psd_repair = sur$psd_repair,
    matrix_rank = sur$matrix_rank,
    positive_support = sur$positive_support,
    n_jt = sur$n_jt,
    n_eff = sur$n_eff,
    population_size = sur$population_size,
    sampling_fraction = sur$sampling_fraction,
    fpc_variance_multiplier = sur$fpc_variance_multiplier,
    fpc_se_multiplier = sur$fpc_se_multiplier,
    variance_multiplier_applied = sur$variance_multiplier_applied,
    se_multiplier_applied = sur$se_multiplier_applied,
    sampling_design = sur$sampling_design,
    variance_rule = sur$variance_rule,
    diag_contract = sur$diag_contract
  )
}

.sm_pairwise_rate_matrix <- function(pair_counts, theta_raw, n, indicators) {
  rho <- matrix(NA_real_, length(indicators), length(indicators), dimnames = list(indicators, indicators))
  diag(rho) <- theta_raw
  pairs <- utils::combn(seq_along(indicators), 2L, simplify = FALSE)
  for (i in seq_along(pairs)) {
    pair <- pairs[[i]]
    value <- pair_counts[[i]] / n
    rho[pair[[1]], pair[[2]]] <- value
    rho[pair[[2]], pair[[1]]] <- value
  }
  rho
}

.sm_validate_sur_indicators <- function(indicators) {
  if (!is.character(indicators) || length(indicators) < 2L || anyNA(indicators) || any(indicators == "") || anyDuplicated(indicators)) {
    .sm_abort_argument(
      "`indicators` must contain at least two distinct indicator names.",
      class = "sitemix_error_invalid_indicators",
      expected = "two or more distinct non-missing names",
      actual = as.character(indicators),
      fix = "Pass indicators in the intended covariance order."
    )
  }
  indicators
}

.sm_validate_sur_n <- function(n) {
  if (!.sm_is_integerish(n) || length(n) != 1L || is.na(n) || n <= 0) {
    .sm_abort_input(
      "`n` must be a positive integer count.",
      class = "sitemix_error_input_indicator_count",
      expected = "positive integer scalar",
      actual = as.character(n),
      fix = "Use the retained `n_jt` for one site-year cell."
    )
  }
  as.integer(n)
}

.sm_validate_sur_count_vector <- function(counts, n, expected_length, column) {
  if (!.sm_is_integerish(counts) || length(counts) != expected_length || anyNA(counts)) {
    .sm_abort_input(
      paste0("`", column, "` must contain integer-like counts with the expected length."),
      class = "sitemix_error_input_indicator_count",
      expected = paste0("length ", expected_length),
      actual = paste0("length ", length(counts)),
      fix = "Check count extraction and indicator ordering."
    )
  }
  counts <- as.integer(counts)
  if (any(counts < 0L) || any(counts > n)) {
    .sm_abort_input(
      paste0("`", column, "` counts must be between 0 and `n`."),
      class = "sitemix_error_input_indicator_count",
      expected = "0 <= count <= n",
      actual = paste(range(counts), collapse = " to "),
      fix = "Check count aggregation before covariance construction."
    )
  }
  counts
}

.sm_validate_sur_pair_bounds <- function(pair_counts, marginal_counts, n, indicators) {
  pairs <- utils::combn(seq_along(indicators), 2L, simplify = FALSE)
  for (i in seq_along(pairs)) {
    pair <- pairs[[i]]
    lower <- max(0L, marginal_counts[[pair[[1]]]] + marginal_counts[[pair[[2]]]] - n)
    upper <- min(marginal_counts[[pair[[1]]]], marginal_counts[[pair[[2]]]])
    if (pair_counts[[i]] < lower || pair_counts[[i]] > upper) {
      .sm_abort_input(
        "Pairwise co-occurrence counts are outside feasible marginal bounds.",
        class = "sitemix_error_input_indicator_count",
        expected = paste0(lower, " <= C_kl <= ", upper),
        actual = pair_counts[[i]],
        fix = "Check pairwise count construction and indicator ordering."
      )
    }
  }
  invisible(TRUE)
}

.sm_validate_sur_joint_feasibility <- function(
  pair_counts,
  marginal_counts,
  n,
  indicators,
  jointly_observed = FALSE
) {
  if (!is.logical(jointly_observed) || length(jointly_observed) != 1L || is.na(jointly_observed)) {
    stop("`jointly_observed` must be TRUE or FALSE.", call. = FALSE)
  }

  K <- length(indicators)
  if (K <= 2L) {
    return(invisible(TRUE))
  }

  if (K == 3L) {
    c1 <- marginal_counts[[1L]]
    c2 <- marginal_counts[[2L]]
    c3 <- marginal_counts[[3L]]
    c12 <- pair_counts[[1L]]
    c13 <- pair_counts[[2L]]
    c23 <- pair_counts[[3L]]

    lower <- max(
      0L,
      c12 + c13 - c1,
      c12 + c23 - c2,
      c13 + c23 - c3
    )
    upper <- min(
      c12,
      c13,
      c23,
      n - c1 - c2 - c3 + c12 + c13 + c23
    )

    if (lower > upper) {
      .sm_abort_input(
        "Three-indicator pair counts do not admit a joint Bernoulli table.",
        class = "sitemix_error_input_indicator_count",
        expected = paste0("an integer triple count with ", lower, " <= C_123 <= ", upper),
        actual = paste0("empty triple-count interval [", lower, ", ", upper, "]"),
        fix = "Check the three marginal and pairwise counts for a common site-year sample.",
        indicators = indicators,
        marginal_counts = marginal_counts,
        pair_counts = pair_counts,
        triple_count_lower = as.integer(lower),
        triple_count_upper = as.integer(upper),
        joint_feasibility = "infeasible"
      )
    }
    return(invisible(TRUE))
  }

  if (!isTRUE(jointly_observed)) {
    .sm_abort_input(
      "Joint feasibility is not verified for sufficient pair-count inputs with four or more indicators.",
      class = "sitemix_error_input_indicator_count",
      expected = "jointly observed student rows, or sufficient counts with K <= 3",
      actual = paste0("K = ", K, " sufficient pair-count input"),
      fix = "Use jointly observed student-level indicators or restrict this covariance request to at most three indicators.",
      indicators = indicators,
      indicator_count = as.integer(K),
      joint_feasibility = "unchecked",
      deferred_option = "joint_feasibility = \"unchecked\""
    )
  }

  invisible(TRUE)
}
