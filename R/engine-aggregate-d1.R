# Scenario D1 aggregate working-independence engine -------------------------

.sm_engine_aggregate_d1 <- function(
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
  sampling_relation = "unknown",
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
  .sm_validate_suppressed_theta_hat(suppressed_theta_hat)
  .sm_validate_suppression_sensitivity_acknowledge(suppression_sensitivity_acknowledge)
  .sm_validate_description(description)
  framing <- .sm_validate_aggregate_framing_arg(framing)
  sampling_relation <- .sm_validate_sampling_relation_arg(sampling_relation)
  .sm_validate_aggregate_d1_indicator_args(indicator, indicators)

  if (identical(framing, "subgroup_as_site")) {
    .sm_abort_aggregate(
      "D1 aggregate estimation does not accept `framing = \"subgroup_as_site\"`.",
      class = "sitemix_error_invalid_framing",
      expected = c(NA_character_, "subgroup_as_indicator"),
      actual = framing,
      fix = "Use subgroup-as-site framing with D0, or use `framing = \"subgroup_as_indicator\"` for D1 rows."
    )
  }

  aggregate <- .sm_prepare_aggregate_input(
    data = data,
    id_cols = id_cols,
    indicator = NULL,
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
  aggregate <- .sm_aggregate_d1_restrict_indicators(aggregate, indicators)

  resolved_case <- attr(aggregate, "aggregate_case", exact = TRUE)
  if (!identical(resolved_case, "D1")) {
    .sm_abort_aggregate(
      "The aggregate working-independence engine accepts D1 inputs only.",
      class = "sitemix_error_ambiguous_dispatch",
      expected = "two or more retained marginal indicators / aggregate_case D1",
      actual = resolved_case,
      fix = "Use `family = \"binomial\"` or `aggregate_case = \"D0\"` for single-indicator aggregate input."
    )
  }
  if (any(!is.na(aggregate$subgroup))) {
    .sm_abort_aggregate(
      "Direct D1 aggregate estimation does not pivot subgroup rows.",
      class = "sitemix_error_invalid_framing",
      expected = "no raw `subgroup` column after aggregate normalization",
      actual = "subgroup rows present",
      fix = "Use the subgroup pivot helpers to convert subgroups into sites or indicators before D1 estimation."
    )
  }
  .sm_validate_aggregate_d1_groups(aggregate)
  .sm_require_suppression_sensitivity_acknowledgement(
    suppression = suppression,
    has_suppressed = any(aggregate$flag_suppressed),
    acknowledged = suppression_sensitivity_acknowledge,
    suppressed_theta_hat = suppressed_theta_hat
  )
  if (!is.null(fpc) && any(aggregate$flag_suppressed)) {
    .sm_abort_argument(
      "Finite-population SRSWOR correction currently requires aggregate input with no Tier-1 suppression rows.",
      class = "sitemix_error_invalid_fpc",
      expected = "fully observed aggregate counts and denominators",
      actual = paste0(sum(aggregate$flag_suppressed), " suppressed row(s)"),
      fix = "Use `fpc = NULL` when retaining suppression audit rows, or provide fully observed fixed-population counts."
    )
  }
  population_size_by_row <- .sm_normalize_d1_fpc_by_row(
    data = data,
    fpc = fpc,
    id_cols = id_cols,
    aggregate = aggregate
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
      "`vjt = TRUE` is not available for D1 groups containing suppressed rows.",
      class = "sitemix_error_estimate_vcov_invariant",
      expected = "ordinary covariance built only from identified marginals",
      actual = paste0(suppression, " suppression rows present"),
      fix = "Use `vjt = FALSE`; suppression sensitivity is stored separately and never enters ordinary D1 `V`."
    )
  }

  retained <- !aggregate$flag_suppressed
  if (any(retained)) {
    .sm_validate_logit_boundary_support(
      C = aggregate$c_jt[retained],
      n = aggregate$n_jt[retained],
      estimate_scale = estimate_scale
    )
  }

  out <- .sm_aggregate_d1_rows(
    aggregate = aggregate,
    vst = vst,
    boundary_method = boundary_method,
    bias_correction = bias_correction,
    vjt = vjt,
    min_n = min_n,
    accountability_n = accountability_n,
    fpc = population_size_by_row,
    anscombe = anscombe,
    suppression = suppression,
    suppressed_theta_hat = suppressed_theta_hat,
    suppression_sensitivity_acknowledge = suppression_sensitivity_acknowledge,
    suppressed_n_strategy = suppressed_n_strategy,
    suppressed_n_bound = suppressed_n_bound,
    framing = framing,
    sampling_relation = sampling_relation,
    description = description
  )
  .sm_warn_working_independence_default()
  out
}

.sm_normalize_d1_fpc_by_row <- function(data, fpc, id_cols, aggregate) {
  if (is.null(fpc)) {
    return(NULL)
  }
  key <- .sm_fpc_group_key(aggregate$site_id, aggregate$year)
  key_order <- unique(key)
  group_indices <- lapply(key_order, function(value) which(key == value))
  groups <- data.frame(
    site_id = vapply(group_indices, function(idx) aggregate$site_id[[idx[[1L]]]], character(1)),
    year = vapply(group_indices, function(idx) aggregate$year[[idx[[1L]]]], integer(1)),
    stringsAsFactors = FALSE
  )
  n_max <- vapply(group_indices, function(idx) max(aggregate$n_jt[idx]), numeric(1))
  normalized <- .sm_normalize_fpc_by_group(
    data = data,
    fpc = fpc,
    id_cols = id_cols,
    groups = groups,
    n = n_max
  )
  matched <- match(
    key,
    .sm_fpc_group_key(normalized$site_id, normalized$year)
  )
  if (anyNA(matched)) {
    .sm_abort_argument(
      "D1 finite-population sizes could not be aligned to normalized groups.",
      class = "sitemix_error_invalid_fpc",
      expected = "one keyed population size per D1 site-year",
      actual = unique(key[is.na(matched)]),
      fix = "Check D1 identifiers and input-row population-size alignment."
    )
  }
  unname(normalized$population_size[matched])
}

.sm_validate_aggregate_d1_indicator_args <- function(indicator, indicators) {
  if (!is.null(indicator)) {
    .sm_abort_argument(
      "`indicator` must be NULL for D1 aggregate multivariate input.",
      class = "sitemix_error_invalid_indicator",
      expected = "NULL",
      actual = as.character(indicator),
      fix = "Use `indicators` to select/order D1 marginal indicators."
    )
  }
  if (!is.null(indicators) &&
      (!is.character(indicators) || length(indicators) < 2L || anyNA(indicators) || any(indicators == "") || anyDuplicated(indicators))) {
    .sm_abort_argument(
      "`indicators` must be NULL or at least two distinct aggregate indicator labels.",
      class = "sitemix_error_invalid_indicators",
      expected = "NULL or two or more distinct labels",
      actual = as.character(indicators),
      fix = "Pass the D1 marginal labels in the intended order."
    )
  }
  invisible(TRUE)
}

.sm_aggregate_d1_restrict_indicators <- function(aggregate, indicators) {
  if (is.null(indicators)) {
    return(aggregate)
  }
  missing <- setdiff(indicators, unique(aggregate$indicator))
  if (length(missing) > 0L) {
    .sm_abort_argument(
      "`indicators` contains labels not present in the aggregate input.",
      class = "sitemix_error_invalid_indicators",
      expected = unique(aggregate$indicator),
      actual = indicators,
      fix = paste0("Missing: ", .sm_cli_collapse(missing, quote = TRUE), ".")
    )
  }
  out <- aggregate[aggregate$indicator %in% indicators, , drop = FALSE]
  out$indicator <- factor(out$indicator, levels = indicators)
  out <- out[order(out$site_id, out$year, out$indicator, out$subgroup, na.last = TRUE), , drop = FALSE]
  out$indicator <- as.character(out$indicator)
  attr(out, "indicator_order") <- indicators
  attr(out, "aggregate_case") <- "D1"
  attr(out, "family") <- "multivariate"
  attr(out, "n_suppressed") <- as.integer(sum(out$flag_suppressed))
  attr(out, "denominator_observed_on_suppressed") <- if (any(out$flag_suppressed)) {
    all(out$denominator_observed[out$flag_suppressed])
  } else {
    TRUE
  }
  attr(out, "has_hidden_denominator") <- any(is.na(out$c_jt) & is.na(out$n_jt))
  out
}

.sm_validate_aggregate_d1_groups <- function(aggregate) {
  expected_order <- attr(aggregate, "indicator_order", exact = TRUE)
  if (is.null(expected_order)) {
    expected_order <- unique(aggregate$indicator)
  }
  groups <- split(seq_len(nrow(aggregate)), paste(aggregate$site_id, aggregate$year, sep = "\r"))
  for (idx in groups) {
    actual_order <- aggregate$indicator[idx]
    if (!identical(actual_order, expected_order)) {
      first <- idx[[1L]]
      .sm_abort_aggregate(
        "D1 aggregate groups must share one complete ordered indicator set.",
        class = "sitemix_error_invalid_aggregate_schema",
        expected = expected_order,
        actual = actual_order,
        row_identity = list(
          site_id = aggregate$site_id[[first]],
          year = aggregate$year[[first]],
          indicator = aggregate$indicator[[first]],
          row_index = first
        ),
        fix = "Complete every site-year on the same indicators and pass `indicators` to lock their order.",
        expected_indicators = expected_order,
        actual_indicators = actual_order,
        missing_indicators = setdiff(expected_order, actual_order),
        extra_indicators = setdiff(actual_order, expected_order)
      )
    }
  }
  invisible(TRUE)
}

.sm_aggregate_d1_rows <- function(
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
  sampling_relation,
  description
) {
  estimate_scale <- .sm_estimate_scale_from_vst(vst, anscombe = anscombe)
  groups <- split(seq_len(nrow(aggregate)), paste(aggregate$site_id, aggregate$year, sep = "\r"))
  rows <- vector("list", nrow(aggregate))
  regimes <- vector("list", length(groups))
  row_i <- 1L
  group_i <- 1L

  for (idx in groups) {
    group <- aggregate[idx, , drop = FALSE]
    group_rows <- vector("list", length(idx))
    scalar_rules <- rep("none", length(idx))

    for (k in seq_along(idx)) {
      source_i <- idx[[k]]
      C <- aggregate$c_jt[[source_i]]
      n <- aggregate$n_jt[[source_i]]
      population_size <- if (is.null(fpc)) NULL else fpc[[source_i]]

      if (isTRUE(aggregate$flag_suppressed[[source_i]])) {
        if (identical(suppression, "drop")) {
          group_rows[[k]] <- .sm_suppressed_drop_row(
            site_id = aggregate$site_id[[source_i]],
            year = aggregate$year[[source_i]],
            indicator = aggregate$indicator[[source_i]],
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
            row = source_i,
            suppressed_n_strategy = suppressed_n_strategy,
            suppressed_n_bound = suppressed_n_bound,
            min_n = min_n
          )
          group_rows[[k]] <- .sm_suppressed_upper_bound_row(
            site_id = aggregate$site_id[[source_i]],
            year = aggregate$year[[source_i]],
            indicator = aggregate$indicator[[source_i]],
            n = n_upper,
            suppressed_theta_hat = suppressed_theta_hat,
            denominator_observed = isTRUE(aggregate$denominator_observed[[source_i]]),
            min_n = min_n,
            accountability_n = accountability_n,
            framing = framing
          )
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

      group_rows[[k]] <- .sm_one_row(
        site_id = aggregate$site_id[[source_i]],
        year = aggregate$year[[source_i]],
        indicator = aggregate$indicator[[source_i]],
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
      scalar_rules[[k]] <- raw$scalar_correction_rule[[1]]
    }

    if (isTRUE(vjt)) {
      v_obj <- .sm_aggregate_d1_vcov_from_rows(group_rows, scalar_correction_rule = scalar_rules)
      for (k in seq_along(group_rows)) {
        group_rows[[k]]$V <- list(v_obj)
        group_rows[[k]]$K <- length(group_rows)
      }
    }

    regimes[[group_i]] <- .sm_aggregate_d1_group_regime(
      group,
      sampling_relation = sampling_relation
    )
    group_i <- group_i + 1L
    for (row in group_rows) {
      rows[[row_i]] <- row
      row_i <- row_i + 1L
    }
  }

  if (any(aggregate$flag_suppressed)) {
    rows <- lapply(rows, .sm_add_aggregate_suppression_provenance)
  }
  out <- .sm_bind_sitemix_rows(
    rows,
    description = description,
    family = "multivariate",
    sitemix_role = "summary_uncertainty"
  )
  attr(out, "aggregate_case") <- "D1"
  attr(out, "sampling_relation") <- sampling_relation
  attr(out, "denominator_pattern") <- .sm_aggregate_d1_metadata_summary(regimes, "denominator_pattern")
  attr(out, "d1_regime") <- .sm_aggregate_d1_regime_summary(regimes)
  attr(out, "d1_regime_by_group") <- tibble::as_tibble(do.call(rbind, regimes))
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

.sm_aggregate_d1_vcov_from_rows <- function(rows, scalar_correction_rule) {
  group <- vctrs::vec_rbind(!!!rows)
  indicators <- group$indicator
  mat <- diag(group$se^2, nrow = length(indicators), ncol = length(indicators))
  dimnames(mat) <- list(indicators, indicators)

  n_jt <- if (length(unique(group$n)) == 1L) group$n[[1]] else NA_integer_
  n_eff <- if (length(unique(group$n_eff)) == 1L) group$n_eff[[1]] else NA_real_
  population_size <- if ("population_size" %in% names(group)) {
    unique(group$population_size)
  } else {
    NULL
  }
  if (length(population_size) > 1L) {
    .sm_abort_vcov(
      "D1 covariance rows must share one site-year population size.",
      class = "sitemix_error_vcov_invariant",
      expected = "one population_size per site-year",
      actual = population_size,
      fix = "Normalize input-row FPC values before working-independence assembly."
    )
  }
  variance_rule <- if ("variance_rule" %in% names(group)) {
    group$variance_rule
  } else {
    ifelse(scalar_correction_rule == "binomial_bc", "design_corrected", "plugin")
  }
  design <- .sm_vcov_fpc_metadata(
    n = group$n,
    fpc = population_size,
    variance_rule = variance_rule,
    K = length(indicators)
  )

  sm_vcov(
    matrix = mat,
    site_id = group$site_id[[1]],
    year = group$year[[1]],
    indicator_order = indicators,
    family = "multivariate",
    vcov_method = "working_independence",
    estimate_scale = group$estimate_scale[[1]],
    vcov_scale = .sm_vcov_scale_from_estimate_scale(group$estimate_scale[[1]]),
    matrix_boundary_rule = "none",
    scalar_correction_rule = scalar_correction_rule,
    psd_repair = "none",
    matrix_rank = .sm_matrix_rank(mat),
    positive_support = NA_integer_,
    n_jt = n_jt,
    n_eff = n_eff,
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

.sm_aggregate_d1_group_regime <- function(group, sampling_relation) {
  denominator_pattern <- if (anyNA(group$n_jt)) {
    "incomplete"
  } else if (length(unique(group$n_jt)) == 1L) {
    "common"
  } else {
    "varying"
  }
  d1_regime <- switch(
    sampling_relation,
    same_units = "D1a",
    different_units = "D1b",
    unknown = "unknown"
  )
  data.frame(
    site_id = group$site_id[[1]],
    year = group$year[[1]],
    K = as.integer(nrow(group)),
    sampling_relation = sampling_relation,
    denominator_pattern = denominator_pattern,
    d1_regime = d1_regime,
    stringsAsFactors = FALSE
  )
}

.sm_aggregate_d1_regime_summary <- function(regimes) {
  .sm_aggregate_d1_metadata_summary(regimes, "d1_regime")
}

.sm_aggregate_d1_metadata_summary <- function(regimes, column) {
  values <- unique(vapply(regimes, function(x) x[[column]][[1]], character(1)))
  if (length(values) == 1L) values[[1]] else "mixed"
}

.sm_warn_working_independence_default <- local({
  warned <- FALSE
  function() {
    if (isTRUE(warned)) {
      return(invisible(FALSE))
    }
    warned <<- TRUE
    .sm_warn(
      "Working-independence default selected for D1 aggregate input.",
      class = "sitemix_warning_working_independence_default",
      expected = "identified within-site covariance off-diagonals",
      actual = "off-diagonals structurally unidentified from marginal aggregates",
      fix = "Use `sm_frechet_envelope()` once sensitivity diagnostics are available."
    )
    invisible(TRUE)
  }
})
