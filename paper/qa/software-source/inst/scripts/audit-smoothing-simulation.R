#!/usr/bin/env Rscript

# Maintainer audit for the experimental GVF/log-variance smoother.  This file
# deliberately scores public sitemix output against separately coded binomial
# variance targets; it does not call internal variance helpers.

sm_smoothing_simulation_grid <- function() {
  n_values <- c(8L, 10L, 12L, 14L, 16L, 20L, 35L, 50L, 65L, 80L, 100L, 150L)
  p_values <- c(0.02, 0.10, 0.30, 0.50, 0.70, 0.90, 0.98)
  grid <- expand.grid(
    n = n_values,
    p = p_values,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- grid[order(grid$n, grid$p), , drop = FALSE]
  rownames(grid) <- NULL
  grid$site_id <- sprintf("SIM%03d", seq_len(nrow(grid)))
  grid$year <- 2025L
  grid$n_band <- ifelse(grid$n <= 20L, "small_n", "large_n")
  grid$p_band <- ifelse(
    grid$p <= 0.10 | grid$p >= 0.90,
    "near_boundary",
    "interior"
  )
  grid$stratum <- paste(grid$n_band, grid$p_band, sep = "/")
  grid
}

sm_smoothing_simulation_criteria <- function() {
  data.frame(
    criterion = c(
      "overall_relative_mse_ratio",
      "worst_stratum_relative_mse_ratio",
      "overall_coverage_error_increase",
      "worst_stratum_coverage_error_increase",
      "inverse_weight_tv_ratio"
    ),
    threshold = c(0.90, 1.10, 0.01, 0.03, 0.95),
    direction = c("<=", "<=", "<=", "<=", "<="),
    stringsAsFactors = FALSE
  )
}

.sm_simulation_preserve_rng <- function() {
  seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  list(
    kind = RNGkind(),
    seed_exists = seed_exists,
    seed = if (seed_exists) {
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
      NULL
    }
  )
}

.sm_simulation_restore_rng <- function(rng_state) {
  do.call(
    RNGkind,
    stats::setNames(as.list(rng_state$kind), c("kind", "normal.kind", "sample.kind"))
  )
  if (!isTRUE(rng_state$seed_exists)) {
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  } else {
    assign(".Random.seed", rng_state$seed, envir = .GlobalEnv)
  }
  invisible(NULL)
}

.sm_simulation_methods <- function(include_gam) {
  methods <- c("unsmoothed", "loglinear")
  if (isTRUE(include_gam)) {
    if (!requireNamespace("mgcv", quietly = TRUE)) {
      stop("`include_gam = TRUE` requires the suggested mgcv package.", call. = FALSE)
    }
    methods <- c(methods, "gam")
  }
  methods
}

.sm_simulation_fit_alternatives <- function(estimates, scale, methods, min_rows) {
  out <- list(unsmoothed = estimates[[scale]])
  fitted_methods <- setdiff(methods, "unsmoothed")
  for (method in fitted_methods) {
    smoothed <- suppressWarnings(sitemix::sm_smooth_variance(
      estimates,
      method = method,
      scale = scale,
      min_rows = min_rows,
      overwrite = FALSE,
      bias_correct = TRUE
    ))
    target <- if (identical(scale, "se")) "se_smoothed" else "se_raw_smoothed"
    out[[method]] <- smoothed[[target]]
  }
  out
}

.sm_simulation_long_rows <- function(
  grid,
  estimates,
  replicate_id,
  scale,
  methods,
  min_rows
) {
  alternatives <- .sm_simulation_fit_alternatives(
    estimates = estimates,
    scale = scale,
    methods = methods,
    min_rows = min_rows
  )
  if (identical(scale, "se")) {
    truth_variance <- 1 / (4 * grid$n)
    truth_estimate <- asin(sqrt(grid$p))
    point_estimate <- estimates$theta_hat
  } else {
    truth_variance <- grid$p * (1 - grid$p) / grid$n
    truth_estimate <- grid$p
    point_estimate <- estimates$theta_raw
  }

  rows <- lapply(methods, function(method) {
    estimated_variance <- alternatives[[method]]^2
    data.frame(
      replicate = as.integer(replicate_id),
      row_id = seq_len(nrow(grid)),
      scale = if (identical(scale, "se")) "transformed_se" else "raw_se",
      method = method,
      n = grid$n,
      p = grid$p,
      stratum = grid$stratum,
      estimate = point_estimate,
      truth_estimate = truth_estimate,
      estimated_variance = estimated_variance,
      truth_variance = truth_variance,
      relative_squared_error = (
        (estimated_variance - truth_variance) / truth_variance
      )^2,
      covered = abs(point_estimate - truth_estimate) <=
        stats::qnorm(0.975) * sqrt(estimated_variance),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.sm_simulation_weight_distortion <- function(rows) {
  split_rows <- split(rows, interaction(rows$replicate, rows$scale, rows$method, drop = TRUE))
  values <- lapply(split_rows, function(piece) {
    oracle <- (1 / piece$truth_variance) / sum(1 / piece$truth_variance)
    candidate <- (1 / piece$estimated_variance) / sum(1 / piece$estimated_variance)
    data.frame(
      replicate = piece$replicate[[1]],
      scale = piece$scale[[1]],
      method = piece$method[[1]],
      inverse_weight_tv = 0.5 * sum(abs(candidate - oracle)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, values)
  rownames(out) <- NULL
  out
}

sm_run_smoothing_simulation <- function(
  reps = 60L,
  seed = 20260712L,
  include_gam = TRUE
) {
  if (length(reps) != 1L || !is.finite(reps) || reps < 2 || reps != as.integer(reps)) {
    stop("`reps` must be an integer of at least 2.", call. = FALSE)
  }
  if (length(seed) != 1L || !is.finite(seed) || seed != as.integer(seed)) {
    stop("`seed` must be one finite integer.", call. = FALSE)
  }
  reps <- as.integer(reps)
  seed <- as.integer(seed)
  methods <- .sm_simulation_methods(include_gam)
  grid <- sm_smoothing_simulation_grid()
  min_rows <- 50L

  rng_state <- .sm_simulation_preserve_rng()
  on.exit(.sm_simulation_restore_rng(rng_state), add = TRUE)
  set.seed(
    seed,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )

  rows <- vector("list", reps * 2L)
  index <- 0L
  for (replicate_id in seq_len(reps)) {
    counts <- stats::rbinom(nrow(grid), size = grid$n, prob = grid$p)
    count_data <- data.frame(
      site_id = grid$site_id,
      year = grid$year,
      n_jt = grid$n,
      c_jt_absent = counts,
      stringsAsFactors = FALSE
    )
    estimates <- sitemix::sm_estimate_from_counts(
      count_data,
      family = "binomial",
      indicator = "absent",
      vst = "arcsine",
      boundary_method = "wilson_floor",
      min_n = 1L
    )
    for (scale in c("se", "se_raw")) {
      index <- index + 1L
      rows[[index]] <- .sm_simulation_long_rows(
        grid = grid,
        estimates = estimates,
        replicate_id = replicate_id,
        scale = scale,
        methods = methods,
        min_rows = min_rows
      )
    }
  }
  draws <- do.call(rbind, rows)
  rownames(draws) <- NULL
  weights <- .sm_simulation_weight_distortion(draws)

  list(
    metadata = list(
      seed = seed,
      reps = reps,
      grid_rows = nrow(grid),
      methods = methods,
      min_rows = min_rows,
      rng_kind = RNGkind()
    ),
    grid = grid,
    draws = draws,
    weights = weights
  )
}

.sm_simulation_metric_table <- function(draws, group_columns) {
  groups <- interaction(draws[group_columns], drop = TRUE, lex.order = TRUE)
  pieces <- split(draws, groups)
  out <- lapply(pieces, function(piece) {
    keys <- piece[1L, group_columns, drop = FALSE]
    cbind(
      keys,
      data.frame(
        relative_mse = mean(piece$relative_squared_error),
        coverage = mean(piece$covered),
        coverage_error = abs(mean(piece$covered) - 0.95),
        max_abs_variance_error = max(abs(
          piece$estimated_variance - piece$truth_variance
        )),
        stringsAsFactors = FALSE
      )
    )
  })
  result <- do.call(rbind, out)
  rownames(result) <- NULL
  result
}

sm_score_smoothing_simulation <- function(simulation) {
  draws <- simulation$draws
  overall <- .sm_simulation_metric_table(draws, c("scale", "method"))
  strata <- .sm_simulation_metric_table(draws, c("scale", "method", "stratum"))
  weight_groups <- interaction(
    simulation$weights$scale,
    simulation$weights$method,
    drop = TRUE,
    lex.order = TRUE
  )
  weight_tv <- do.call(rbind, lapply(split(simulation$weights, weight_groups), function(piece) {
    data.frame(
      scale = piece$scale[[1]],
      method = piece$method[[1]],
      inverse_weight_tv = mean(piece$inverse_weight_tv),
      stringsAsFactors = FALSE
    )
  }))
  rownames(weight_tv) <- NULL

  list(overall = overall, strata = strata, weights = weight_tv)
}

sm_decide_smoothing_simulation <- function(scores) {
  criteria <- sm_smoothing_simulation_criteria()
  candidate_rows <- scores$overall$method != "unsmoothed"
  candidates <- scores$overall[candidate_rows, c("scale", "method"), drop = FALSE]
  decisions <- lapply(seq_len(nrow(candidates)), function(i) {
    scale <- candidates$scale[[i]]
    method <- candidates$method[[i]]
    overall_candidate <- scores$overall[
      scores$overall$scale == scale & scores$overall$method == method,
      ,
      drop = FALSE
    ]
    overall_baseline <- scores$overall[
      scores$overall$scale == scale & scores$overall$method == "unsmoothed",
      ,
      drop = FALSE
    ]
    strata_candidate <- scores$strata[
      scores$strata$scale == scale & scores$strata$method == method,
      ,
      drop = FALSE
    ]
    strata_baseline <- scores$strata[
      scores$strata$scale == scale & scores$strata$method == "unsmoothed",
      ,
      drop = FALSE
    ]
    strata_baseline <- strata_baseline[match(strata_candidate$stratum, strata_baseline$stratum), , drop = FALSE]
    weight_candidate <- scores$weights$inverse_weight_tv[
      scores$weights$scale == scale & scores$weights$method == method
    ]
    weight_baseline <- scores$weights$inverse_weight_tv[
      scores$weights$scale == scale & scores$weights$method == "unsmoothed"
    ]

    zero_mse_baseline <- overall_baseline$relative_mse <= 1e-20
    overall_mse_ratio <- if (zero_mse_baseline) {
      if (overall_candidate$relative_mse <= 1e-20) 1 else Inf
    } else {
      overall_candidate$relative_mse / overall_baseline$relative_mse
    }
    stratum_mse_ratio <- ifelse(
      strata_baseline$relative_mse <= 1e-20,
      ifelse(strata_candidate$relative_mse <= 1e-20, 1, Inf),
      strata_candidate$relative_mse / strata_baseline$relative_mse
    )
    overall_coverage_increase <- overall_candidate$coverage_error - overall_baseline$coverage_error
    stratum_coverage_increase <- strata_candidate$coverage_error - strata_baseline$coverage_error
    weight_ratio <- if (weight_baseline <= 1e-12) {
      if (weight_candidate <= 1e-12) 1 else Inf
    } else {
      weight_candidate / weight_baseline
    }

    checks <- c(
      overall_mse_ratio <= criteria$threshold[criteria$criterion == "overall_relative_mse_ratio"],
      max(stratum_mse_ratio) <= criteria$threshold[criteria$criterion == "worst_stratum_relative_mse_ratio"],
      overall_coverage_increase <= criteria$threshold[criteria$criterion == "overall_coverage_error_increase"],
      max(stratum_coverage_increase) <= criteria$threshold[criteria$criterion == "worst_stratum_coverage_error_increase"],
      weight_ratio <= criteria$threshold[criteria$criterion == "inverse_weight_tv_ratio"]
    )
    data.frame(
      scale = scale,
      method = method,
      overall_relative_mse_ratio = overall_mse_ratio,
      worst_stratum_relative_mse_ratio = max(stratum_mse_ratio),
      overall_coverage_error_increase = overall_coverage_increase,
      worst_stratum_coverage_error_increase = max(stratum_coverage_increase),
      inverse_weight_tv_ratio = weight_ratio,
      checks_passed = sum(checks),
      checks_total = length(checks),
      decision = if (all(checks)) "GO" else "NO-GO",
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, decisions)
  rownames(out) <- NULL
  out
}

.sm_simulation_print <- function(simulation, scores, decisions) {
  cat("Smoothing simulation audit\n")
  cat("seed:", simulation$metadata$seed, "\n")
  cat("replicates:", simulation$metadata$reps, "\n")
  cat("grid rows per replicate:", simulation$metadata$grid_rows, "\n\n")
  print(scores$overall, row.names = FALSE)
  cat("\n")
  print(scores$strata, row.names = FALSE)
  cat("\n")
  print(scores$weights, row.names = FALSE)
  cat("\n")
  print(decisions, row.names = FALSE)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  reps_arg <- grep("^--reps=", args, value = TRUE)
  seed_arg <- grep("^--seed=", args, value = TRUE)
  reps <- if (length(reps_arg)) as.integer(sub("^--reps=", "", reps_arg[[1L]])) else 60L
  seed <- if (length(seed_arg)) as.integer(sub("^--seed=", "", seed_arg[[1L]])) else 20260712L
  include_gam <- !"--no-gam" %in% args

  command <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command, value = TRUE)
  script_path <- if (length(file_arg)) {
    sub("^--file=", "", file_arg[[1L]])
  } else {
    "inst/scripts/audit-smoothing-simulation.R"
  }
  root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
  if (requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(root, quiet = TRUE)
  }

  simulation <- sm_run_smoothing_simulation(
    reps = reps,
    seed = seed,
    include_gam = include_gam
  )
  scores <- sm_score_smoothing_simulation(simulation)
  decisions <- sm_decide_smoothing_simulation(scores)
  .sm_simulation_print(simulation, scores, decisions)
}
