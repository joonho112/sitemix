# Scenario D0 aggregate-binomial engine ------------------------------------

.sm_engine_aggregate_d0 <- function(
  data,
  indicator = NULL,
  indicators = NULL,
  id_cols = c("site_id", "year"),
  numerator_col = NULL,
  denominator_col = NULL,
  indicator_col = NULL,
  subgroup_col = NULL,
  aggregate_case = "auto",
  framing = NA_character_,
  vst = "arcsine",
  boundary_method = "wilson_floor",
  bias_correction = NULL,
  vjt = FALSE,
  min_n = 10L,
  accountability_n = 30L,
  fpc = NULL,
  anscombe = FALSE,
  suppression = "drop",
  suppression_col = NULL,
  suppression_flag_value = "",
  suppression_when = NULL,
  suppressed_theta_hat = 0.5,
  suppression_sensitivity_acknowledge = FALSE,
  suppressed_n_strategy = "observed_n",
  suppressed_n_bound = NULL,
  description = NULL
) {
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
  .sm_validate_suppressed_theta_hat(suppressed_theta_hat)
  .sm_validate_suppression_sensitivity_acknowledge(suppression_sensitivity_acknowledge)
  .sm_validate_description(description)
  framing <- .sm_validate_aggregate_framing_arg(framing)

  aggregate <- .sm_prepare_aggregate_input(
    data = data,
    id_cols = id_cols,
    indicator = indicator,
    indicators = indicators,
    numerator_col = numerator_col,
    denominator_col = denominator_col,
    indicator_col = indicator_col,
    subgroup_col = subgroup_col,
    suppression_col = suppression_col,
    suppression_flag_value = suppression_flag_value,
    suppression_when = suppression_when,
    aggregate_case = aggregate_case,
    suppression = suppression,
    suppressed_n_strategy = suppressed_n_strategy,
    suppressed_n_bound = suppressed_n_bound,
    min_n = min_n
  )

  resolved_case <- attr(aggregate, "aggregate_case", exact = TRUE)
  if (!identical(resolved_case, "D0")) {
    .sm_abort_aggregate(
      "The aggregate-binomial engine accepts D0 inputs only.",
      class = "sitemix_error_ambiguous_dispatch",
      expected = "one retained indicator / aggregate_case D0",
      actual = resolved_case,
      fix = "Use a single indicator for D0, or use the multivariate aggregate path for D1 working-independence."
    )
  }
  if (any(!is.na(aggregate$subgroup)) ||
      (!is.na(framing) && !identical(framing, "subgroup_as_site"))) {
    .sm_abort_aggregate(
      "Direct D0 aggregate estimation does not pivot raw subgroup rows.",
      class = "sitemix_error_invalid_framing",
      expected = "already-pivoted subgroup-as-site rows or no subgroup framing",
      actual = if (any(!is.na(aggregate$subgroup))) "subgroup rows present" else framing,
      fix = "Use `sm_pivot_subgroups_to_sites()` before D0 estimation, or use `framing = NA`."
    )
  }
  if (!is.null(fpc) && any(aggregate$flag_suppressed)) {
    .sm_abort_argument(
      "Finite-population SRSWOR correction currently requires aggregate input with no Tier-1 suppression rows.",
      class = "sitemix_error_invalid_fpc",
      expected = "fully observed aggregate counts and denominators",
      actual = paste0(sum(aggregate$flag_suppressed), " suppressed row(s)"),
      fix = "Use `fpc = NULL` when retaining suppression audit rows, or provide fully observed fixed-population counts."
    )
  }
  .sm_require_suppression_sensitivity_acknowledgement(
    suppression = suppression,
    has_suppressed = any(aggregate$flag_suppressed),
    acknowledged = suppression_sensitivity_acknowledge,
    suppressed_theta_hat = suppressed_theta_hat
  )
  fpc_groups <- .sm_normalize_fpc_by_group(
    data = data,
    fpc = fpc,
    id_cols = id_cols,
    groups = aggregate,
    n = aggregate$n_jt
  )
  estimate_scale <- .sm_estimate_scale_from_vst(vst, anscombe = anscombe)
  if (any(aggregate$flag_suppressed) && identical(suppression, "upper_bound") && !identical(estimate_scale, "arcsine")) {
    .sm_abort_estimate(
      "`suppression = \"upper_bound\"` currently requires the default arcsine scale.",
      class = "sitemix_error_estimate_var_method",
      expected = "vst = \"arcsine\" and anscombe = FALSE",
      actual = estimate_scale,
      fix = "Use default arcsine output for suppressed upper-bound rows."
    )
  }
  if (any(aggregate$flag_suppressed) && isTRUE(vjt)) {
    .sm_abort_estimate(
      "`vjt = TRUE` is not available when suppressed rows are present.",
      class = "sitemix_error_estimate_vcov_invariant",
      expected = "ordinary covariance built only from identified rows",
      actual = paste0(suppression, " suppression rows present"),
      fix = "Use `vjt = FALSE`; suppression sensitivity is stored separately and never enters ordinary `V`."
    )
  }

  retained <- !aggregate$flag_suppressed
  if (any(retained)) {
    .sm_validate_logit_boundary_support(
      C = aggregate$c_jt[retained],
      n = aggregate$n_jt[retained],
      estimate_scale = estimate_scale
    )
    .sm_validate_binomial_boundary_vcov(
      C = aggregate$c_jt[retained],
      n = aggregate$n_jt[retained],
      boundary_method = boundary_method,
      vjt = vjt
    )
  }
  .sm_aggregate_d0_rows(
    aggregate = aggregate,
    vst = vst,
    boundary_method = boundary_method,
    bias_correction = bias_correction,
    vjt = vjt,
    min_n = min_n,
    accountability_n = accountability_n,
    fpc = if (is.null(fpc_groups)) NULL else fpc_groups$population_size,
    anscombe = anscombe,
    suppression = suppression,
    suppressed_theta_hat = suppressed_theta_hat,
    suppression_sensitivity_acknowledge = suppression_sensitivity_acknowledge,
    suppressed_n_strategy = suppressed_n_strategy,
    suppressed_n_bound = suppressed_n_bound,
    framing = framing,
    description = description
  )
}

.sm_aggregate_d0_rows <- function(
  aggregate,
  vst,
  boundary_method,
  bias_correction,
  vjt,
  min_n,
  accountability_n,
  fpc,
  anscombe,
  suppression,
  suppressed_theta_hat,
  suppression_sensitivity_acknowledge,
  suppressed_n_strategy,
  suppressed_n_bound,
  framing,
  description
) {
  estimate_scale <- .sm_estimate_scale_from_vst(vst, anscombe = anscombe)
  rows <- vector("list", nrow(aggregate))

  for (i in seq_len(nrow(aggregate))) {
    C <- aggregate$c_jt[[i]]
    n <- aggregate$n_jt[[i]]
    population_size <- if (is.null(fpc)) NULL else fpc[[i]]
    if (isTRUE(aggregate$flag_suppressed[[i]])) {
      if (identical(suppression, "drop")) {
        rows[[i]] <- .sm_suppressed_drop_row(
          site_id = aggregate$site_id[[i]],
          year = aggregate$year[[i]],
          indicator = aggregate$indicator[[i]],
          n = n,
          n_eff = if (identical(estimate_scale, "arcsine_anscombe")) .sm_anscombe_n_eff(n) else as.numeric(n),
          estimate_scale = estimate_scale,
          min_n = min_n,
          accountability_n = accountability_n,
          framing = framing
        )
      } else {
        n_upper <- .sm_suppressed_operational_n(
          aggregate = aggregate,
          row = i,
          suppressed_n_strategy = suppressed_n_strategy,
          suppressed_n_bound = suppressed_n_bound,
          min_n = min_n
        )
        row <- .sm_suppressed_upper_bound_row(
          site_id = aggregate$site_id[[i]],
          year = aggregate$year[[i]],
          indicator = aggregate$indicator[[i]],
          n = n_upper,
          suppressed_theta_hat = suppressed_theta_hat,
          denominator_observed = isTRUE(aggregate$denominator_observed[[i]]),
          min_n = min_n,
          accountability_n = accountability_n,
          framing = framing
        )
        rows[[i]] <- row
      }
      next
    }

    raw <- .sm_binomial_scalar_raw(
      C = C,
      n = n,
      boundary_method = boundary_method,
      bias_correction = bias_correction,
      fpc = population_size
    )
    n_eff <- if (identical(estimate_scale, "arcsine_anscombe")) {
      .sm_anscombe_n_eff(n)
    } else {
      as.numeric(n)
    }

    row <- .sm_one_row(
      site_id = aggregate$site_id[[i]],
      year = aggregate$year[[i]],
      indicator = aggregate$indicator[[i]],
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
      input_mode = "aggregate",
      flag_suppressed = FALSE,
      framing = framing,
      fpc = population_size
    )

    if (isTRUE(vjt)) {
      row$V <- list(.sm_binomial_vcov_from_row(row, scalar_correction_rule = raw$scalar_correction_rule[[1]]))
    }
    rows[[i]] <- row
  }

  if (any(aggregate$flag_suppressed)) {
    rows <- lapply(rows, .sm_add_aggregate_suppression_provenance)
  }
  out <- .sm_bind_sitemix_rows(
    rows,
    description = description,
    family = "binomial",
    sitemix_role = "summary_uncertainty"
  )
  attr(out, "aggregate_case") <- "D0"
  attr(out, "suppression") <- list(
    mode = suppression,
    detection_path = attr(aggregate, "suppression_detection_path", exact = TRUE),
    n_suppressed = attr(aggregate, "n_suppressed", exact = TRUE),
    denominator_observed_on_suppressed = attr(aggregate, "denominator_observed_on_suppressed", exact = TRUE),
    has_hidden_denominator = attr(aggregate, "has_hidden_denominator", exact = TRUE),
    suppressed_n_strategy = suppressed_n_strategy,
    suppressed_n_bound = suppressed_n_bound,
    suppressed_theta_hat = suppressed_theta_hat
  )
  has_sensitivity <- any(.sm_is_suppression_sensitivity_row(out))
  attr(out, "suppression")$sensitivity_acknowledgement_requested <- isTRUE(suppression_sensitivity_acknowledge)
  attr(out, "suppression")$sensitivity_acknowledged <- has_sensitivity && isTRUE(suppression_sensitivity_acknowledge)
  attr(out, "suppression")$sensitivity_role <- if (has_sensitivity) {
    "nonidentified_variance_sensitivity"
  } else {
    "none"
  }
  validate.sitemix_estimates(out)
  out
}

.sm_suppressed_operational_n <- function(
  aggregate,
  row,
  suppressed_n_strategy,
  suppressed_n_bound,
  min_n
) {
  n_observed <- aggregate$n_jt[[row]]
  if (identical(suppressed_n_strategy, "observed_n")) {
    if (is.na(n_observed)) {
      .sm_abort_aggregate(
        "Observed-denominator upper-bound mode requires observed `n_jt` on every suppressed row.",
        class = "sitemix_error_invalid_suppressed_n",
        expected = "observed positive `n_jt`",
        actual = "hidden denominator",
        row_identity = list(
          site_id = aggregate$site_id[[row]],
          year = aggregate$year[[row]],
          indicator = aggregate$indicator[[row]],
          row_index = row
        ),
        fix = "Use `suppressed_n_strategy = \"worst_case_bound\"` with a conservative `suppressed_n_bound`."
      )
    }
    return(n_observed)
  }

  .sm_validate_hidden_suppressed_n(
    suppression = "upper_bound",
    suppressed_n_strategy = "worst_case_bound",
    suppressed_n_bound = suppressed_n_bound,
    min_n = min_n
  )
  as.integer(suppressed_n_bound)
}
