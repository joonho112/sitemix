regression_counts_path <- function(root = NULL) {
  if (!is.null(root)) {
    candidate <- file.path(root, "inst", "extdata", "alprek_subset_counts.rds")
    if (file.exists(candidate)) {
      return(candidate)
    }
  }

  local_candidates <- c(
    file.path("inst", "extdata", "alprek_subset_counts.rds"),
    file.path("..", "..", "inst", "extdata", "alprek_subset_counts.rds")
  )
  for (candidate in local_candidates) {
    if (file.exists(candidate)) {
      return(candidate)
    }
  }

  installed <- system.file("extdata", "alprek_subset_counts.rds", package = "sitemix")
  if (!nzchar(installed)) {
    stop("Could not find alprek_subset_counts.rds.", call. = FALSE)
  }
  installed
}

regression_quiet_working_independence <- function(expr) {
  withCallingHandlers(
    expr,
    sitemix_warning_working_independence_default = function(w) invokeRestart("muffleWarning")
  )
}

regression_core <- function(x) {
  cols <- intersect(
    c(
      "site_id", "year", "indicator", "theta_raw", "theta_hat", "se_raw",
      "se", "n", "n_eff", "estimate_scale", "transform", "var_method",
      "flag_small_n", "flag_zero_cell", "input_mode", "flag_suppressed",
      "framing", "flag_below_accountability", "K"
    ),
    names(x)
  )
  out <- as.data.frame(lapply(cols, function(col) x[[col]]), stringsAsFactors = FALSE)
  names(out) <- cols
  row.names(out) <- NULL
  out
}

regression_vcov <- function(x) {
  if (!"V" %in% names(x)) {
    return(list())
  }

  lapply(seq_along(x$V), function(i) {
    V <- x$V[[i]]
    mat <- as.matrix(V)
    offdiag <- mat[row(mat) != col(mat)]
    list(
      row = i,
      site_id = x$site_id[[i]],
      year = x$year[[i]],
      indicator = x$indicator[[i]],
      family = V$family,
      vcov_method = V$vcov_method,
      estimate_scale = V$estimate_scale,
      vcov_scale = V$vcov_scale,
      matrix_rank = V$matrix_rank,
      indicator_order = V$indicator_order,
      dim = dim(mat),
      diag = unname(diag(mat)),
      offdiag_sum = if (length(offdiag)) sum(offdiag) else 0,
      max_abs_row_sum = max(abs(rowSums(mat)))
    )
  })
}

regression_output <- function(x) {
  list(
    attrs = list(
      class = class(x),
      family = attr(x, "family", exact = TRUE),
      aggregate_case = attr(x, "aggregate_case", exact = TRUE),
      d1_regime = attr(x, "d1_regime", exact = TRUE),
      sitemix_role = attr(x, "sitemix_role", exact = TRUE)
    ),
    core = regression_core(x),
    vcov = regression_vcov(x)
  )
}

regression_vdiag <- function(x) {
  if (!"V" %in% names(x)) {
    return(rep(NA_real_, nrow(x)))
  }

  vapply(seq_len(nrow(x)), function(i) {
    mat <- as.matrix(x$V[[i]])
    indicator <- x$indicator[[i]]
    if (indicator %in% rownames(mat)) {
      return(unname(mat[indicator, indicator]))
    }
    unname(diag(mat)[[1]])
  }, numeric(1))
}

regression_core_with_vdiag <- function(x) {
  out <- regression_core(x)
  out$V_diag <- regression_vdiag(x)
  out
}

regression_alprek_content <- function(counts, indicator_cols, pair_cols) {
  probs <- c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1)

  list(
    n_by_year = aggregate(counts["n_jt"], list(year = counts$year), sum),
    n_jt_quantile_type1 = as.numeric(stats::quantile(counts$n_jt, probs = probs, type = 1, names = FALSE)),
    n_jt_quantile_probs = probs,
    indicator_totals_by_year = aggregate(counts[indicator_cols], list(year = counts$year), sum),
    pair_totals_by_year = aggregate(counts[pair_cols], list(year = counts$year), sum)
  )
}

regression_d0_2024_frpm <- function(counts_2024, sentinel_sites) {
  d0_input <- data.frame(
    site_id = counts_2024$site_id,
    year = counts_2024$year,
    indicator = "frpm",
    c_jt = counts_2024$c_jt_frpm,
    n_jt = counts_2024$n_jt,
    stringsAsFactors = FALSE
  )
  out <- sitemix::sm_estimate_from_aggregates(
    d0_input,
    family = "binomial",
    indicator = "frpm",
    vjt = TRUE,
    min_n = 1L
  )
  sentinel <- out[out$site_id %in% sentinel_sites, , drop = FALSE]

  list(
    attrs = list(
      family = attr(out, "family", exact = TRUE),
      aggregate_case = attr(out, "aggregate_case", exact = TRUE),
      nrow = nrow(out)
    ),
    summary = data.frame(
      theta_raw_sum = sum(out$theta_raw),
      theta_hat_sum = sum(out$theta_hat),
      se_sum = sum(out$se),
      below_accountability = sum(out$flag_below_accountability),
      zero_cell = sum(out$flag_zero_cell),
      small_n = sum(out$flag_small_n),
      min_n = min(out$n),
      max_n = max(out$n),
      stringsAsFactors = FALSE
    ),
    sentinel_rows = regression_core_with_vdiag(sentinel)
  )
}

regression_d1_2024_four_indicator <- function(counts_2024, indicators, sentinel_sites) {
  d1_input <- counts_2024[c("site_id", "year", paste0("c_jt_", indicators), "n_jt")]
  out <- regression_quiet_working_independence(
    sitemix::sm_estimate_from_aggregates(
      d1_input,
      family = "multivariate",
      sampling_relation = "same_units",
      vjt = TRUE,
      min_n = 1L
    )
  )
  sentinel <- out[out$site_id %in% sentinel_sites, , drop = FALSE]

  by_indicator <- do.call(
    rbind,
    lapply(indicators, function(indicator) {
      # Indicator-only summaries intentionally discard the joint transport
      # class before taking a partial covariance group.
      scalar_rows <- tibble::as_tibble(out)
      rows <- scalar_rows[scalar_rows$indicator == indicator, , drop = FALSE]
      data.frame(
        indicator = indicator,
        n_rows = nrow(rows),
        theta_raw_sum = sum(rows$theta_raw),
        theta_hat_sum = sum(rows$theta_hat),
        se_sum = sum(rows$se),
        zero_cell = sum(rows$flag_zero_cell),
        stringsAsFactors = FALSE
      )
    })
  )
  row.names(by_indicator) <- NULL

  vcov_pins <- do.call(
    rbind,
    lapply(sentinel_sites, function(site) {
      group <- out[out$site_id == site, , drop = FALSE]
      V <- group$V[[1]]
      mat <- as.matrix(V)
      offdiag <- mat[row(mat) != col(mat)]
      data.frame(
        site_id = site,
        year = group$year[[1]],
        vcov_method = V$vcov_method,
        vcov_scale = V$vcov_scale,
        indicator_order = paste(V$indicator_order, collapse = "|"),
        diag_frpm = unname(mat["frpm", "frpm"]),
        diag_snap = unname(mat["snap", "snap"]),
        diag_wic = unname(mat["wic", "wic"]),
        diag_tanf = unname(mat["tanf", "tanf"]),
        max_abs_offdiag = if (length(offdiag)) max(abs(offdiag)) else 0,
        stringsAsFactors = FALSE
      )
    })
  )
  row.names(vcov_pins) <- NULL

  list(
    attrs = list(
      family = attr(out, "family", exact = TRUE),
      aggregate_case = attr(out, "aggregate_case", exact = TRUE),
      d1_regime = attr(out, "d1_regime", exact = TRUE),
      nrow = nrow(out),
      K = unique(out$K)
    ),
    by_indicator = by_indicator,
    sentinel_rows = regression_core_with_vdiag(sentinel),
    vcov_pins = vcov_pins
  )
}

regression_build_baselines <- function(root = NULL) {
  counts_path <- regression_counts_path(root)
  counts <- readRDS(counts_path)
  alprek_slice <- counts[seq_len(6L), , drop = FALSE]
  mv_slice <- counts[seq_len(4L), , drop = FALSE]

  scenario_a <- sitemix::sm_estimate_from_counts(
    alprek_slice,
    family = "binomial",
    indicator = "frpm",
    vjt = TRUE,
    min_n = 1L
  )

  scenario_b <- sitemix::sm_estimate_from_counts(
    mv_slice,
    family = "multivariate",
    indicators = c("frpm", "snap", "wic"),
    vjt = TRUE,
    min_n = 1L
  )

  scenario_c_counts <- data.frame(
    site_id = c("M1", "M2", "M3"),
    year = c(2025L, 2025L, 2025L),
    n_jt = c(30L, 40L, 50L),
    c_jt_eng = c(10L, 14L, 25L),
    c_jt_spa = c(12L, 16L, 10L),
    c_jt_oth = c(8L, 10L, 15L),
    stringsAsFactors = FALSE
  )
  scenario_c <- sitemix::sm_estimate_from_counts(
    scenario_c_counts,
    family = "multinomial",
    indicators = c("eng", "spa", "oth"),
    vjt = TRUE,
    min_n = 1L
  )

  d0_input <- data.frame(
    site_id = alprek_slice$site_id,
    year = alprek_slice$year,
    indicator = "frpm",
    c_jt = alprek_slice$c_jt_frpm,
    n_jt = alprek_slice$n_jt,
    stringsAsFactors = FALSE
  )
  d0 <- sitemix::sm_estimate_from_aggregates(
    d0_input,
    family = "binomial",
    indicator = "frpm",
    vjt = TRUE,
    min_n = 1L
  )

  d1_input <- mv_slice[
    c("site_id", "year", "c_jt_frpm", "c_jt_snap", "c_jt_wic", "n_jt")
  ]
  d1 <- regression_quiet_working_independence(
    sitemix::sm_estimate_from_aggregates(
      d1_input,
      family = "multivariate",
      sampling_relation = "same_units",
      vjt = TRUE,
      min_n = 1L
    )
  )

  indicator_cols <- c("c_jt_frpm", "c_jt_snap", "c_jt_wic", "c_jt_tanf")
  pair_cols <- setdiff(names(counts), c("site_id", "year", "n_jt", indicator_cols))
  indicators <- sub("^c_jt_", "", indicator_cols)
  counts_2024 <- counts[counts$year == 2024L, , drop = FALSE]
  sentinel_sites <- c("S001", "S010", "S025", "S050")

  list(
    metadata = list(
      fixture_version = 2L,
      created_by = "Step 9.3 regression fixture generator",
      counts_file = basename(counts_path),
      counts_md5 = unname(tools::md5sum(counts_path))
    ),
    tolerances = list(
      scalar = 1e-12,
      matrix = 1e-10
    ),
    alprek_summary = list(
      dim = dim(counts),
      names = names(counts),
      total_n = sum(counts$n_jt),
      year_range = range(counts$year),
      n_sites = length(unique(counts$site_id)),
      n_site_years = nrow(counts),
      indicator_totals = as.list(colSums(counts[indicator_cols])),
      pair_totals = as.list(colSums(counts[pair_cols])),
      first_keys = paste(head(counts$site_id, 6L), head(counts$year, 6L), sep = "::")
    ),
    alprek_content = regression_alprek_content(counts, indicator_cols, pair_cols),
    scenario_a = regression_output(scenario_a),
    scenario_b = regression_output(scenario_b),
    scenario_c = regression_output(scenario_c),
    aggregate_d0 = regression_output(d0),
    aggregate_d1 = regression_output(d1),
    aggregate_d0_alprek_2024_frpm = regression_d0_2024_frpm(counts_2024, sentinel_sites),
    aggregate_d1_alprek_2024_four_indicator = regression_d1_2024_four_indicator(counts_2024, indicators, sentinel_sites)
  )
}

regression_flat_vcov <- function(baseline, names) {
  rows <- list()
  for (name in names) {
    vcovs <- baseline[[name]]$vcov
    for (i in seq_along(vcovs)) {
      item <- vcovs[[i]]
      rows[[length(rows) + 1L]] <- data.frame(
        scenario = name,
        row = item$row,
        site_id = item$site_id,
        year = item$year,
        indicator = item$indicator,
        family = item$family,
        vcov_method = if (is.null(item$vcov_method)) NA_character_ else item$vcov_method,
        estimate_scale = item$estimate_scale,
        vcov_scale = item$vcov_scale,
        matrix_rank = item$matrix_rank,
        indicator_order = paste(item$indicator_order, collapse = "|"),
        matrix_dim = paste(item$dim, collapse = "x"),
        diag = paste(format(item$diag, digits = 16), collapse = "|"),
        offdiag_sum = item$offdiag_sum,
        max_abs_row_sum = item$max_abs_row_sum,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

regression_bind_rows <- function(rows) {
  cols <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(row) {
    missing <- setdiff(cols, names(row))
    for (col in missing) {
      row[[col]] <- NA
    }
    row[cols]
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

regression_write_review_csvs <- function(baseline, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  small_names <- c("scenario_a", "scenario_b", "scenario_c", "aggregate_d0", "aggregate_d1")

  small_rows <- regression_bind_rows(
    lapply(small_names, function(name) {
      data.frame(scenario = name, baseline[[name]]$core, stringsAsFactors = FALSE)
    })
  )
  utils::write.csv(small_rows, file.path(out_dir, "small_cases_rows.csv"), row.names = FALSE)
  utils::write.csv(regression_flat_vcov(baseline, small_names), file.path(out_dir, "small_cases_vcov.csv"), row.names = FALSE)

  summary_rows <- rbind(
    data.frame(section = "n_by_year", key = baseline$alprek_content$n_by_year$year, value = baseline$alprek_content$n_by_year$n_jt),
    data.frame(section = "n_jt_quantile_type1", key = baseline$alprek_content$n_jt_quantile_probs, value = baseline$alprek_content$n_jt_quantile_type1),
    data.frame(section = "d0_2024_frpm", key = names(baseline$aggregate_d0_alprek_2024_frpm$summary), value = unlist(baseline$aggregate_d0_alprek_2024_frpm$summary, use.names = FALSE)),
    data.frame(section = "d1_2024_four_indicator", key = paste0(baseline$aggregate_d1_alprek_2024_four_indicator$by_indicator$indicator, "_theta_raw_sum"), value = baseline$aggregate_d1_alprek_2024_four_indicator$by_indicator$theta_raw_sum),
    data.frame(section = "d1_2024_four_indicator", key = paste0(baseline$aggregate_d1_alprek_2024_four_indicator$by_indicator$indicator, "_theta_hat_sum"), value = baseline$aggregate_d1_alprek_2024_four_indicator$by_indicator$theta_hat_sum),
    data.frame(section = "d1_2024_four_indicator", key = paste0(baseline$aggregate_d1_alprek_2024_four_indicator$by_indicator$indicator, "_zero_cell"), value = baseline$aggregate_d1_alprek_2024_four_indicator$by_indicator$zero_cell)
  )
  utils::write.csv(summary_rows, file.path(out_dir, "alprek_summary.csv"), row.names = FALSE)

  spot_rows <- regression_bind_rows(
    list(
      data.frame(scenario = "d0_2024_frpm", baseline$aggregate_d0_alprek_2024_frpm$sentinel_rows, stringsAsFactors = FALSE),
      data.frame(scenario = "d1_2024_four_indicator", baseline$aggregate_d1_alprek_2024_four_indicator$sentinel_rows, stringsAsFactors = FALSE)
    )
  )
  utils::write.csv(spot_rows, file.path(out_dir, "alprek_spotcheck_rows.csv"), row.names = FALSE)
  utils::write.csv(
    baseline$aggregate_d1_alprek_2024_four_indicator$vcov_pins,
    file.path(out_dir, "alprek_spotcheck_vcov.csv"),
    row.names = FALSE
  )
}
