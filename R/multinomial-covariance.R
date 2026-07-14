# Scenario C multinomial covariance helpers --------------------------------

.sm_multinomial_cov_from_count_row <- function(
  row,
  categories,
  count_cols = paste0("c_jt_", categories),
  boundary_method = "wilson_floor",
  bias_correction = NULL,
  fpc = NULL
) {
  if (!inherits(row, "data.frame") || nrow(row) != 1L) {
    .sm_abort_input(
      "`row` must be a single count-table row.",
      class = "sitemix_error_input_indicator_count",
      expected = "one-row data frame",
      actual = paste(class(row), collapse = "/"),
      fix = "Call the multinomial covariance helper one `(site_id, year)` count row at a time."
    )
  }
  .sm_require_columns(row, c("n_jt", count_cols))

  .sm_multinomial_cov_from_counts(
    n = row$n_jt[[1]],
    category_counts = unname(unlist(row[count_cols], use.names = FALSE)),
    categories = categories,
    boundary_method = boundary_method,
    bias_correction = bias_correction,
    fpc = fpc
  )
}

.sm_multinomial_cov_from_counts <- function(
  n,
  category_counts,
  categories,
  boundary_method = "wilson_floor",
  bias_correction = NULL,
  fpc = NULL
) {
  categories <- .sm_validate_multinomial_categories(categories)
  .sm_validate_boundary_method(boundary_method)
  .sm_validate_bias_correction(bias_correction)
  n <- .sm_validate_sur_n(n)
  category_counts <- .sm_validate_sur_count_vector(
    category_counts,
    n = n,
    expected_length = length(categories),
    column = "category_counts"
  )
  if (sum(category_counts) != n) {
    .sm_abort_input(
      "Multinomial category counts must sum to `n`.",
      class = "sitemix_error_input_indicator_count",
      expected = "sum(category_counts) == n",
      actual = paste0("sum = ", sum(category_counts), ", n = ", n),
      fix = "Check category count construction before covariance assembly."
    )
  }

  pi_raw <- category_counts / n
  mat <- (diag(pi_raw, nrow = length(categories)) - tcrossprod(pi_raw)) / n
  dimnames(mat) <- list(categories, categories)

  boundary <- category_counts == 0L | category_counts == n
  positive_support <- as.integer(sum(category_counts > 0L))
  matrix_boundary_rule <- "none"
  scalar_correction_rule <- rep("none", length(categories))
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
        "Scenario C whole-matrix binomial correction requires n > 1.",
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

  if (any(boundary) && identical(boundary_method, "wilson_floor")) {
    scalar_correction_rule[boundary] <- "wilson_boundary_surrogate"
    matrix_boundary_rule <- "simplex_preserve"
  } else if (any(boundary) && identical(boundary_method, "agresti_coull")) {
    .sm_abort_estimate(
      "Agresti-Coull boundary adjustment is not defined for Scenario C matrix output.",
      class = "sitemix_error_estimate_vcov_invariant",
      expected = "no boundary cells, or boundary_method = \"wilson_floor\" / \"none\"",
      actual = paste(categories[boundary], collapse = ", "),
      fix = "Use `boundary_method = \"wilson_floor\"` for `vjt = TRUE`, or request scalar-only output."
    )
  }

  matrix_multiplier <- if (identical(variance_rule, "design_corrected")) {
    .sm_fpc_design_variance_multiplier(n, fpc = fpc)
  } else {
    .sm_fpc_variance_multiplier(n, fpc = fpc)
  }
  mat <- mat * matrix_multiplier
  mat <- (mat + t(mat)) / 2
  .sm_validate_vcov_psd(mat)
  .sm_validate_vcov_simplex(mat)
  # Scenario C applies one global rule to the entire simplex matrix. At a
  # boundary coordinate its zero row/column makes plugin versus corrected
  # scaling numerically indistinguishable; the matrix metadata nevertheless
  # retains the globally requested rule while scalar provenance stays separate.
  metadata_rule <- rep(variance_rule, length(categories))
  design <- .sm_vcov_fpc_metadata(
    n = n,
    fpc = fpc,
    variance_rule = metadata_rule,
    K = length(categories)
  )

  # Scalar Wilson SE treats each category as a 2-outcome binomial
  # (category-k vs. not-k), per Ch. 13 sec-ch13-edge. This preserves a
  # non-zero scalar se_raw at C_k=0 while the multinomial diagonal stays at
  # 0 to keep the simplex row-sum-zero invariant on V.
  se_raw <- vapply(seq_along(category_counts), function(i) {
    .sm_binomial_scalar_raw(
      C = category_counts[[i]],
      n = n,
      boundary_method = boundary_method,
      bias_correction = bias_correction,
      fpc = fpc
    )$se_raw[[1]]
  }, numeric(1))

  list(
    theta_raw = stats::setNames(as.numeric(pi_raw), categories),
    V_raw = mat,
    se_raw = stats::setNames(se_raw, categories),
    boundary = stats::setNames(boundary, categories),
    matrix_boundary_rule = matrix_boundary_rule,
    scalar_correction_rule = stats::setNames(scalar_correction_rule, categories),
    psd_repair = "none",
    matrix_rank = as.integer(max(0L, positive_support - 1L)),
    positive_support = positive_support,
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
    diag_contract = if (any(boundary) && identical(boundary_method, "wilson_floor")) {
      "row_se_raw_squared_except_boundary_surrogates"
    } else {
      "row_se_raw_squared"
    }
  )
}

.sm_multinomial_vcov_from_cov <- function(
  cov,
  site_id,
  year,
  categories,
  estimate_scale,
  vcov_scale = "raw"
) {
  sm_vcov(
    matrix = cov$V_raw,
    site_id = site_id,
    year = year,
    indicator_order = categories,
    family = "multinomial",
    vcov_method = "multinomial",
    estimate_scale = estimate_scale,
    vcov_scale = vcov_scale,
    matrix_boundary_rule = cov$matrix_boundary_rule,
    scalar_correction_rule = unname(cov$scalar_correction_rule),
    psd_repair = cov$psd_repair,
    matrix_rank = cov$matrix_rank,
    positive_support = cov$positive_support,
    n_jt = cov$n_jt,
    n_eff = cov$n_eff,
    population_size = cov$population_size,
    sampling_fraction = cov$sampling_fraction,
    fpc_variance_multiplier = cov$fpc_variance_multiplier,
    fpc_se_multiplier = cov$fpc_se_multiplier,
    variance_multiplier_applied = cov$variance_multiplier_applied,
    se_multiplier_applied = cov$se_multiplier_applied,
    sampling_design = cov$sampling_design,
    variance_rule = cov$variance_rule,
    diag_contract = cov$diag_contract
  )
}

.sm_validate_multinomial_categories <- function(categories) {
  if (!is.character(categories) || length(categories) < 2L || anyNA(categories) || any(categories == "") || anyDuplicated(categories)) {
    .sm_abort_argument(
      "`categories` must contain at least two distinct category labels.",
      class = "sitemix_error_invalid_indicators",
      expected = "two or more distinct non-missing category labels",
      actual = as.character(categories),
      fix = "Pass categories in the intended full-simplex order."
    )
  }
  categories
}
