# Scalar variance and standard error helpers --------------------------------

.sm_arcsine_se <- function(n_eff) {
  .sm_check_positive_n(n_eff, arg = "n_eff")
  1 / (2 * sqrt(n_eff))
}

.sm_binomial_se <- function(p, n) {
  .sm_check_probability(p, allow_boundary = TRUE)
  .sm_check_positive_n(n)
  sqrt(p * (1 - p) / n)
}

.sm_binomial_bc_se <- function(p, n) {
  .sm_check_probability(p, allow_boundary = TRUE)
  .sm_check_positive_n(n)
  if (any(n <= 1)) {
    .sm_abort_estimate(
      "`binomial_bc` requires n > 1.",
      class = "sitemix_error_estimate_var_method",
      expected = "n > 1",
      actual = paste(range(n), collapse = " to "),
      fix = "Use the Wilson floor for one-student boundary cells."
    )
  }

  sqrt(p * (1 - p) / (n - 1))
}

.sm_wilson_se <- function(p, n, z = stats::qnorm(0.975)) {
  .sm_check_probability(p, allow_boundary = TRUE)
  .sm_check_positive_n(n)
  .sm_validate_positive_z(z)

  sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
}

.sm_agresti_coull_adjust <- function(C, n, z = stats::qnorm(0.975)) {
  counts <- .sm_check_counts(C, n)
  .sm_validate_positive_z(z)
  z2 <- z^2
  list(
    C = counts$C + z2 / 2,
    n = counts$n + z2,
    p = (counts$C + z2 / 2) / (counts$n + z2)
  )
}

.sm_agresti_coull_se <- function(C, n, z = stats::qnorm(0.975)) {
  adjusted <- .sm_agresti_coull_adjust(C, n, z = z)
  sqrt(adjusted$p * (1 - adjusted$p) / adjusted$n)
}

.sm_validate_positive_z <- function(z) {
  if (!is.numeric(z) || length(z) != 1L || is.na(z) || !is.finite(z) || z <= 0) {
    .sm_abort_estimate(
      "`z` must be a positive finite scalar.",
      class = "sitemix_error_estimate_var_method",
      expected = "positive finite scalar",
      actual = paste(class(z), collapse = "/"),
      fix = "Use a positive Normal quantile."
    )
  }
  invisible(TRUE)
}

.sm_logit_delta_se <- function(p, n) {
  .sm_check_probability(p, allow_boundary = FALSE)
  .sm_check_positive_n(n)
  1 / sqrt(n * p * (1 - p))
}

.sm_arcsine_bc_delta_se <- function(p, n) {
  .sm_binomial_bc_se(p, n) / (2 * sqrt(p * (1 - p)))
}

.sm_logit_bc_delta_se <- function(p, n) {
  .sm_binomial_bc_se(p, n) / (p * (1 - p))
}

.sm_transformed_se <- function(
  theta_raw,
  n,
  n_eff = n,
  estimate_scale,
  se_raw = NULL,
  var_method_raw = NULL,
  fpc = NULL
) {
  if (!is.character(estimate_scale) || length(estimate_scale) != 1L || is.na(estimate_scale)) {
    .sm_abort_estimate(
      "`estimate_scale` must be a single scale label.",
      class = "sitemix_error_estimate_var_method",
      expected = c("none", "arcsine", "arcsine_anscombe", "logit"),
      actual = paste(class(estimate_scale), collapse = "/"),
      fix = "Use a locked estimate scale."
    )
  }
  if (!estimate_scale %in% c("none", "arcsine", "arcsine_anscombe", "logit")) {
    .sm_abort_estimate(
      "`estimate_scale` is not supported.",
      class = "sitemix_error_estimate_var_method",
      expected = c("none", "arcsine", "arcsine_anscombe", "logit"),
      actual = estimate_scale,
      fix = "Use a locked estimate scale."
    )
  }

  raw_methods <- if (is.null(var_method_raw)) {
    rep("binomial", length(theta_raw))
  } else if (length(var_method_raw) == 1L) {
    rep(var_method_raw, length(theta_raw))
  } else {
    var_method_raw
  }
  if (length(raw_methods) != length(theta_raw)) {
    .sm_abort_estimate(
      "`var_method_raw` must align with `theta_raw`.",
      class = "sitemix_error_estimate_var_method",
      expected = paste0("length 1 or ", length(theta_raw)),
      actual = paste0("length ", length(raw_methods)),
      fix = "Pass one raw-scale provenance label per estimate."
    )
  }
  bias_corrected <- raw_methods == "binomial_bc"
  fpc_se <- .sm_fpc_multiplier(n, fpc = fpc)

  if (identical(estimate_scale, "arcsine")) {
    se <- .sm_arcsine_se(n_eff) * fpc_se
    var_method <- rep("arcsine_vst", length(se))
    if (any(bias_corrected)) {
      se[bias_corrected] <- se_raw[bias_corrected] /
        (2 * sqrt(theta_raw[bias_corrected] * (1 - theta_raw[bias_corrected])))
      var_method[bias_corrected] <- "arcsine_delta_binomial_bc"
    }
  } else if (identical(estimate_scale, "arcsine_anscombe")) {
    if (any(bias_corrected)) {
      .sm_abort_estimate(
        "Anscombe output does not support `binomial_bc`.",
        class = "sitemix_error_estimate_var_method",
        expected = "unadjusted raw variance",
        actual = "binomial_bc",
        fix = "Set `bias_correction = NULL` when `anscombe = TRUE`."
      )
    }
    se <- .sm_arcsine_se(n_eff) * fpc_se
    var_method <- rep("arcsine_anscombe", length(se))
  } else if (identical(estimate_scale, "logit")) {
    se <- .sm_logit_delta_se(theta_raw, n) * fpc_se
    var_method <- rep("logit_delta", length(se))
    if (any(bias_corrected)) {
      se[bias_corrected] <- se_raw[bias_corrected] /
        (theta_raw[bias_corrected] * (1 - theta_raw[bias_corrected]))
      var_method[bias_corrected] <- "logit_delta_binomial_bc"
    }
  } else {
    if (is.null(se_raw) || is.null(var_method_raw)) {
      .sm_abort_estimate(
        "Raw-scale output requires `se_raw` and `var_method_raw`.",
        class = "sitemix_error_estimate_var_method",
        expected = c("se_raw", "var_method_raw"),
        actual = "NULL",
        fix = "Compute raw scalar standard errors before `vst = \"none\"` output."
      )
    }
    se <- se_raw
    var_method <- var_method_raw
  }

  list(se = se, var_method = var_method)
}

.sm_validate_var_method <- function(var_method) {
  valid <- c(
    "arcsine_vst",
    "arcsine_delta_binomial_bc",
    "arcsine_anscombe",
    "logit_delta",
    "logit_delta_binomial_bc",
    "binomial",
    "binomial_bc",
    "wilson_boundary_surrogate",
    "agresti_coull_boundary_surrogate",
    "suppressed_drop",
    "suppression_sensitivity"
  )
  suffix <- " \\+ (fh|gvf)_smooth_(gam|loglinear)$"
  base <- if (is.character(var_method)) sub(suffix, "", var_method) else var_method
  smooth_suffix <- if (is.character(var_method)) {
    grepl(" \\+ (fh|gvf)_smooth_", var_method)
  } else {
    FALSE
  }
  valid_smooth_suffix <- if (is.character(var_method)) {
    grepl(suffix, var_method) | !smooth_suffix
  } else {
    FALSE
  }
  if (!is.character(var_method) ||
      anyNA(var_method) ||
      any(!base %in% valid) ||
      any(!valid_smooth_suffix)) {
    .sm_abort_estimate(
      "`var_method` contains unsupported scalar SE provenance labels.",
      class = "sitemix_error_estimate_var_method",
      expected = c(
        valid,
        paste0(valid, " + fh_smooth_loglinear"),
        paste0(valid, " + fh_smooth_gam"),
        paste0(valid, " + gvf_smooth_loglinear"),
        paste0(valid, " + gvf_smooth_gam")
      ),
      actual = unique(as.character(var_method)),
      fix = "Keep matrix provenance in `sm_vcov$vcov_method`, not row `var_method`."
    )
  }

  invisible(TRUE)
}

.sm_fpc_multiplier <- function(n, fpc = NULL) {
  .sm_check_positive_n(n)
  if (is.null(fpc)) {
    return(rep(1, length(n)))
  }
  if (!is.numeric(fpc) ||
      !length(fpc) %in% c(1L, length(n)) ||
      anyNA(fpc) ||
      any(!is.finite(fpc)) ||
      any(fpc < 1) ||
      any(fpc != floor(fpc))) {
    .sm_abort_argument(
      "`fpc` must contain positive whole-number population sizes aligned with `n`.",
      class = "sitemix_error_invalid_fpc",
      expected = paste0("one value or ", length(n), " aligned values"),
      actual = paste0("class = ", paste(class(fpc), collapse = "/"), "; length = ", length(fpc)),
      fix = "Use whole finite population sizes; scalar values are recycled."
    )
  }
  fpc <- rep_len(as.numeric(fpc), length(n))
  invalid <- fpc < n
  if (any(invalid)) {
    first <- which(invalid)[[1L]]
    .sm_abort_argument(
      "Finite-population SRSWOR requires `fpc >= n`.",
      class = "sitemix_error_invalid_fpc",
      expected = "population_size >= sample size",
      actual = paste0("fpc = ", fpc[[first]], ", n = ", n[[first]]),
      fix = "Use the fixed population size; equality is a valid census."
    )
  }

  census <- fpc == n
  multiplier <- numeric(length(n))
  multiplier[!census] <- sqrt(
    (fpc[!census] - n[!census]) / (fpc[!census] - 1)
  )
  multiplier
}

.sm_fpc_variance_multiplier <- function(n, fpc = NULL) {
  .sm_fpc_multiplier(n, fpc = fpc)^2
}

.sm_fpc_design_variance_multiplier <- function(n, fpc = NULL) {
  .sm_check_positive_n(n)
  if (is.null(fpc)) {
    return(rep(1, length(n)))
  }
  # Reuse the full SRSWOR validation, including the explicit census branch.
  .sm_fpc_multiplier(n, fpc = fpc)
  fpc <- rep_len(as.numeric(fpc), length(n))
  (fpc - n) / fpc
}

.sm_apply_fpc <- function(se, n, fpc = NULL) {
  if (!is.numeric(se) || anyNA(se) || any(!is.finite(se)) || any(se < 0)) {
    .sm_abort_estimate(
      "`se` must contain non-negative finite standard errors.",
      class = "sitemix_error_estimate_var_method",
      expected = "non-negative finite standard errors",
      actual = paste(range(se, na.rm = TRUE), collapse = " to "),
      fix = "Check scalar variance construction before FPC adjustment."
    )
  }

  se * .sm_fpc_multiplier(n, fpc)
}

.sm_apply_fpc_variance <- function(v, n, fpc = NULL) {
  if (!is.numeric(v) || anyNA(v) || any(!is.finite(v))) {
    .sm_abort_estimate(
      "`v` must contain finite variance or covariance values.",
      class = "sitemix_error_estimate_var_method",
      expected = "finite numeric variance or covariance values",
      actual = paste(class(v), collapse = "/"),
      fix = "Check covariance construction before FPC adjustment."
    )
  }

  v * .sm_fpc_variance_multiplier(n, fpc = fpc)
}

.sm_validate_boundary_method <- function(boundary_method) {
  if (!is.character(boundary_method) || length(boundary_method) != 1L || is.na(boundary_method)) {
    .sm_abort_argument(
      "`boundary_method` must be a single boundary policy.",
      class = "sitemix_error_invalid_boundary",
      expected = c("wilson_floor", "agresti_coull", "none"),
      actual = paste(class(boundary_method), collapse = "/"),
      fix = "Use one of the locked boundary policies."
    )
  }
  if (!boundary_method %in% c("wilson_floor", "agresti_coull", "none")) {
    .sm_abort_argument(
      "`boundary_method` is not supported.",
      class = "sitemix_error_invalid_boundary",
      expected = c("wilson_floor", "agresti_coull", "none"),
      actual = boundary_method,
      fix = "Use one of the locked boundary policies."
    )
  }

  invisible(TRUE)
}

.sm_validate_bias_correction <- function(bias_correction) {
  if (is.null(bias_correction)) {
    return(invisible(TRUE))
  }
  if (!is.character(bias_correction) || length(bias_correction) != 1L || is.na(bias_correction)) {
    .sm_abort_argument(
      "`bias_correction` must be NULL or \"binomial_bc\".",
      class = "sitemix_error_invalid_bias",
      expected = c("NULL", "binomial_bc"),
      actual = paste(class(bias_correction), collapse = "/"),
      fix = "Use `NULL` for the plug-in SE or `\"binomial_bc\"`."
    )
  }
  if (!identical(bias_correction, "binomial_bc")) {
    .sm_abort_argument(
      "`bias_correction` is not supported.",
      class = "sitemix_error_invalid_bias",
      expected = c("NULL", "binomial_bc"),
      actual = bias_correction,
      fix = "Use `NULL` for the plug-in SE or `\"binomial_bc\"`."
    )
  }

  invisible(TRUE)
}

.sm_binomial_scalar_raw <- function(
  C,
  n,
  boundary_method = "wilson_floor",
  bias_correction = NULL,
  fpc = NULL,
  z = stats::qnorm(0.975)
) {
  .sm_validate_boundary_method(boundary_method)
  .sm_validate_bias_correction(bias_correction)
  counts <- .sm_check_counts(C, n)
  C <- counts$C
  n <- counts$n

  theta_raw <- C / n
  flag_zero_cell <- C == 0 | C == n
  se_raw <- .sm_binomial_se(theta_raw, n)
  var_method_raw <- rep("binomial", length(theta_raw))
  scalar_correction_rule <- rep("none", length(theta_raw))

  if (!is.null(bias_correction)) {
    interior <- !flag_zero_cell
    se_raw[interior] <- .sm_binomial_bc_se(theta_raw[interior], n[interior])
    var_method_raw[interior] <- "binomial_bc"
    scalar_correction_rule[interior] <- "binomial_bc"
  }

  if (any(flag_zero_cell) && identical(boundary_method, "wilson_floor")) {
    se_raw[flag_zero_cell] <- .sm_wilson_se(theta_raw[flag_zero_cell], n[flag_zero_cell], z = z)
    var_method_raw[flag_zero_cell] <- "wilson_boundary_surrogate"
    scalar_correction_rule[flag_zero_cell] <- "wilson_boundary_surrogate"
  } else if (any(flag_zero_cell) && identical(boundary_method, "agresti_coull")) {
    se_raw[flag_zero_cell] <- .sm_agresti_coull_se(
      C[flag_zero_cell],
      n[flag_zero_cell],
      z = z
    )
    var_method_raw[flag_zero_cell] <- "agresti_coull_boundary_surrogate"
    scalar_correction_rule[flag_zero_cell] <- "agresti_coull_boundary_surrogate"
  }

  if (is.null(fpc)) {
    fpc_multiplier <- rep(1, length(n))
  } else {
    fpc_multiplier <- .sm_fpc_multiplier(n, fpc = fpc)
    design_corrected <- var_method_raw == "binomial_bc"
    if (any(design_corrected)) {
      fpc_multiplier[design_corrected] <- sqrt(
        .sm_fpc_design_variance_multiplier(
          n[design_corrected],
          rep_len(fpc, length(n))[design_corrected]
        )
      )
    }
  }
  se_raw <- se_raw * fpc_multiplier

  data.frame(
    theta_raw = theta_raw,
    se_raw = se_raw,
    var_method_raw = var_method_raw,
    scalar_correction_rule = scalar_correction_rule,
    flag_zero_cell = flag_zero_cell,
    n_eff_raw = n,
    stringsAsFactors = FALSE
  )
}
