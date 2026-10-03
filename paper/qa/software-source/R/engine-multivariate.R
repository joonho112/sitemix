# Scenario B multivariate engine -------------------------------------------

.sm_engine_multivariate <- function(
  data,
  indicator = NULL,
  indicators,
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
    family = "multivariate",
    indicator = indicator,
    indicators = indicators,
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
    family = "multivariate",
    indicators = indicators,
    id_cols = id_cols,
    from_counts = from_counts,
    na_action = na_action
  )
  input_mode <- attr(counts, "input_mode", exact = TRUE)
  indicators <- attr(counts, "indicator_order", exact = TRUE)
  count_cols <- attr(counts, "count_cols", exact = TRUE)
  pair_cols <- attr(counts, "pair_cols", exact = TRUE)
  estimate_scale <- .sm_estimate_scale_from_vst(vst, anscombe = anscombe)
  .sm_validate_multivariate_logit_boundary(counts, count_cols = count_cols, estimate_scale = estimate_scale)
  fpc_groups <- .sm_normalize_fpc_by_group(
    data = data,
    fpc = fpc,
    id_cols = id_cols,
    groups = counts,
    n = counts$n_jt
  )

  rows <- vector("list", nrow(counts) * length(indicators))
  row_i <- 1L
  for (i in seq_len(nrow(counts))) {
    n <- counts$n_jt[[i]]
    population_size <- if (is.null(fpc_groups)) NULL else fpc_groups$population_size[[i]]
    n_eff <- if (identical(estimate_scale, "arcsine_anscombe")) .sm_anscombe_n_eff(n) else as.numeric(n)
    scalar <- .sm_multivariate_scalar_raw(
      counts[i, ],
      indicators = indicators,
      count_cols = count_cols,
      boundary_method = boundary_method,
      bias_correction = bias_correction,
      fpc = population_size
    )
    v_obj <- NULL
    if (isTRUE(vjt)) {
      sur <- .sm_multivariate_sur_from_count_row(
        counts[i, ],
        indicators = indicators,
        count_cols = count_cols,
        pair_cols = pair_cols,
        boundary_method = boundary_method,
        bias_correction = bias_correction,
        fpc = population_size
      )
      sur$scalar_correction_rule <- scalar$scalar_correction_rule
      sur$n_eff <- n_eff
      v_obj <- .sm_multivariate_vcov_from_sur(
        sur,
        site_id = counts$site_id[[i]],
        year = counts$year[[i]],
        indicators = indicators,
        estimate_scale = estimate_scale,
        vcov_scale = "raw"
      )
    }

    # lintr cannot resolve this package-private cross-file helper.
    # nolint start: object_usage_linter.
    category_rows <- .sm_bc_category_rows(
      site_id = counts$site_id[[i]],
      year = counts$year[[i]],
      indicator_order = indicators,
      scalar = scalar,
      n = n,
      estimate_scale = estimate_scale,
      n_eff = n_eff,
      min_n = min_n,
      accountability_n = accountability_n,
      input_mode = input_mode,
      V = v_obj,
      K = if (isTRUE(vjt)) length(indicators) else NULL,
      fpc = population_size
    )
    # nolint end
    for (row in category_rows) {
      rows[[row_i]] <- row
      row_i <- row_i + 1L
    }
  }

  .sm_bind_sitemix_rows(
    rows,
    description = description,
    family = "multivariate",
    sitemix_role = "summary_uncertainty"
  )
}

.sm_multivariate_scalar_raw <- function(
  row,
  indicators,
  count_cols,
  boundary_method,
  bias_correction,
  fpc
) {
  n <- row$n_jt[[1]]
  C <- as.integer(unname(unlist(row[count_cols], use.names = FALSE)))
  raw <- lapply(C, function(c_k) {
    .sm_binomial_scalar_raw(
      C = c_k,
      n = n,
      boundary_method = boundary_method,
      bias_correction = bias_correction,
      fpc = fpc
    )
  })

  data.frame(
    indicator = indicators,
    C = C,
    theta_raw = C / n,
    se_raw = vapply(raw, function(x) x$se_raw[[1]], numeric(1)),
    var_method_raw = vapply(raw, function(x) x$var_method_raw[[1]], character(1)),
    scalar_correction_rule = vapply(raw, function(x) x$scalar_correction_rule[[1]], character(1)),
    flag_zero_cell = vapply(raw, function(x) x$flag_zero_cell[[1]], logical(1)),
    stringsAsFactors = FALSE
  )
}

.sm_validate_multivariate_logit_boundary <- function(counts, count_cols, estimate_scale) {
  if (!identical(estimate_scale, "logit")) {
    return(invisible(TRUE))
  }
  C <- as.vector(t(as.matrix(counts[count_cols])))
  n <- rep(counts$n_jt, each = length(count_cols))
  .sm_validate_logit_boundary_support(C = C, n = n, estimate_scale = estimate_scale)
}
