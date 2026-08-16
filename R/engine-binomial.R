# Scenario A binomial engine ------------------------------------------------

.sm_engine_binomial <- function(
  data,
  indicator,
  id_cols = c("site_id", "year"),
  vst = "arcsine",
  boundary_method = "wilson_floor",
  bias_correction = NULL,
  vjt = FALSE,
  min_n = 10L,
  fpc = NULL,
  anscombe = FALSE,
  from_counts = FALSE,
  na_action = "drop_rows",
  description = NULL,
  accountability_n = 30L
) {
  .sm_validate_arguments(
    data = data,
    family = "binomial",
    indicator = indicator,
    id_cols = id_cols,
    vst = vst,
    boundary_method = boundary_method,
    bias_correction = bias_correction,
    vjt = vjt,
    min_n = min_n,
    accountability_n = accountability_n,
    fpc = fpc,
    anscombe = anscombe,
    from_counts = from_counts,
    na_action = na_action,
    description = description
  )

  counts <- .sm_prepare_counts(
    data = data,
    family = "binomial",
    indicator = indicator,
    id_cols = id_cols,
    from_counts = from_counts,
    na_action = na_action
  )
  input_mode <- attr(counts, "input_mode", exact = TRUE)
  count_col <- attr(counts, "count_cols", exact = TRUE)[[1]]
  estimate_scale <- .sm_estimate_scale_from_vst(vst, anscombe = anscombe)
  .sm_validate_logit_boundary_support(
    C = counts[[count_col]],
    n = counts$n_jt,
    estimate_scale = estimate_scale
  )
  .sm_validate_binomial_boundary_vcov(
    C = counts[[count_col]],
    n = counts$n_jt,
    boundary_method = boundary_method,
    vjt = vjt
  )
  fpc_groups <- .sm_normalize_fpc_by_group(
    data = data,
    fpc = fpc,
    id_cols = id_cols,
    groups = counts,
    n = counts$n_jt
  )

  rows <- vector("list", nrow(counts))
  for (i in seq_len(nrow(counts))) {
    C <- counts[[count_col]][[i]]
    n <- counts$n_jt[[i]]
    population_size <- if (is.null(fpc_groups)) NULL else fpc_groups$population_size[[i]]
    raw <- .sm_binomial_scalar_raw(
      C = C,
      n = n,
      boundary_method = boundary_method,
      bias_correction = bias_correction,
      fpc = population_size
    )
    n_eff <- if (identical(estimate_scale, "arcsine_anscombe")) .sm_anscombe_n_eff(n) else as.numeric(n)

    row <- .sm_one_row(
      site_id = counts$site_id[[i]],
      year = counts$year[[i]],
      indicator = indicator,
      theta_raw = raw$theta_raw[[1]],
      se_raw = raw$se_raw[[1]],
      n = n,
      C = C,
      estimate_scale = estimate_scale,
      var_method_raw = raw$var_method_raw[[1]],
      n_eff = n_eff,
      min_n = min_n,
      accountability_n = accountability_n,
      flag_small_n = n < min_n,
      flag_zero_cell = raw$flag_zero_cell[[1]],
      input_mode = input_mode,
      flag_suppressed = FALSE,
      framing = NA_character_,
      fpc = population_size
    )

    if (isTRUE(vjt)) {
      row$V <- list(.sm_binomial_vcov_from_row(row, scalar_correction_rule = raw$scalar_correction_rule[[1]]))
    }
    rows[[i]] <- row
  }

  .sm_bind_sitemix_rows(
    rows,
    description = description,
    family = "binomial",
    sitemix_role = "summary_uncertainty"
  )
}

.sm_validate_binomial_boundary_vcov <- function(
  C,
  n,
  boundary_method,
  vjt
) {
  if (!isTRUE(vjt) || !identical(boundary_method, "agresti_coull")) {
    return(invisible(TRUE))
  }

  boundary <- C == 0L | C == n
  if (any(boundary)) {
    first <- which(boundary)[[1]]
    .sm_abort_estimate(
      "Agresti-Coull boundary regularization is not defined for matrix output.",
      class = "sitemix_error_estimate_vcov_invariant",
      expected = "no boundary cells, or boundary_method = \"wilson_floor\" / \"none\"",
      actual = paste0("C = ", C[[first]], ", n = ", n[[first]]),
      fix = "Use scalar-only output or a supported matrix boundary policy."
    )
  }

  invisible(TRUE)
}

.sm_validate_logit_boundary_support <- function(C, n, estimate_scale) {
  # Phase 1-4 fail-fast: logit + boundary cells errors regardless of
  # boundary_method. The Ch. 12 override "accept with warning when AC shifts
  # the boundary off (0,1) before transform" is deferred to Phase 5 when
  # warning emission is wired up.
  if (!identical(estimate_scale, "logit")) {
    return(invisible(TRUE))
  }
  boundary <- C == 0L | C == n
  if (any(boundary)) {
    first <- which(boundary)[[1]]
    .sm_abort_estimate(
      "Logit-scale binomial output requires interior site-year proportions.",
      class = "sitemix_error_estimate_var_method",
      expected = "0 < C < n for every retained cell",
      actual = paste0("C = ", C[[first]], ", n = ", n[[first]]),
      fix = "Use `vst = \"arcsine\"`, `vst = \"none\"`, or filter boundary cells before logit output."
    )
  }
  invisible(TRUE)
}

.sm_estimate_scale_from_vst <- function(vst, anscombe = FALSE) {
  .sm_validate_vst(vst)
  .sm_validate_anscombe_arg(anscombe, vst)
  if (identical(vst, "arcsine") && isTRUE(anscombe)) {
    "arcsine_anscombe"
  } else if (identical(vst, "arcsine")) {
    "arcsine"
  } else if (identical(vst, "logit")) {
    "logit"
  } else {
    "none"
  }
}

.sm_binomial_vcov_from_row <- function(row, scalar_correction_rule = "none") {
  mat <- matrix(
    row$se[[1]]^2,
    1L,
    1L,
    dimnames = list(row$indicator[[1]], row$indicator[[1]])
  )
  population_size <- if ("population_size" %in% names(row)) {
    row$population_size[[1]]
  } else {
    NULL
  }
  variance_rule <- if (identical(scalar_correction_rule, "binomial_bc")) {
    "design_corrected"
  } else {
    "plugin"
  }
  design <- .sm_vcov_fpc_metadata(
    n = row$n[[1]],
    fpc = population_size,
    variance_rule = variance_rule,
    K = 1L
  )
  sm_vcov(
    matrix = mat,
    site_id = row$site_id[[1]],
    year = row$year[[1]],
    indicator_order = row$indicator[[1]],
    family = "binomial",
    vcov_method = NA_character_,
    estimate_scale = row$estimate_scale[[1]],
    vcov_scale = .sm_vcov_scale_from_estimate_scale(row$estimate_scale[[1]]),
    matrix_boundary_rule = "none",
    scalar_correction_rule = scalar_correction_rule,
    n_jt = row$n[[1]],
    n_eff = row$n_eff[[1]],
    population_size = design$population_size,
    sampling_fraction = design$sampling_fraction,
    fpc_variance_multiplier = design$fpc_variance_multiplier,
    fpc_se_multiplier = design$fpc_se_multiplier,
    variance_multiplier_applied = design$variance_multiplier_applied,
    se_multiplier_applied = design$se_multiplier_applied,
    sampling_design = design$sampling_design,
    variance_rule = design$variance_rule,
    diag_contract = "row_se_squared"
  )
}

.sm_vcov_scale_from_estimate_scale <- function(estimate_scale) {
  # The arcsine VST variance 1/(4 n_eff) is the asymptotic delta-method
  # result on the arcsine scale, so the
  # 1x1 binomial V correctly carries vcov_scale = "arcsine_delta" even
  # though no literal delta computation runs in this function.
  switch(
    estimate_scale,
    none = "raw",
    arcsine = "arcsine_delta",
    arcsine_anscombe = "arcsine_delta",
    logit = "logit_delta",
    .sm_output_error(
      "`estimate_scale` is not supported for `sm_vcov` construction.",
      expected = c("none", "arcsine", "arcsine_anscombe", "logit"),
      actual = estimate_scale
    )
  )
}
