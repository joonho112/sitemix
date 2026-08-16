smoothing_simulation_script <- system.file(
  "scripts",
  "audit-smoothing-simulation.R",
  package = "sitemix",
  mustWork = FALSE
)
if (!nzchar(smoothing_simulation_script) || !file.exists(smoothing_simulation_script)) {
  source_candidate <- testthat::test_path(
    "..", "..", "inst", "scripts", "audit-smoothing-simulation.R"
  )
  if (file.exists(source_candidate)) {
    smoothing_simulation_script <- source_candidate
  }
}
if (!nzchar(smoothing_simulation_script) || !file.exists(smoothing_simulation_script)) {
  stop(
    "Could not locate the packaged smoothing-simulation audit script.",
    call. = FALSE
  )
}
source(smoothing_simulation_script, local = TRUE)

smoothing_simulation_cache <- new.env(parent = emptyenv())

smoothing_fast_audit <- function() {
  if (is.null(smoothing_simulation_cache$result)) {
    simulation <- sm_run_smoothing_simulation(
      reps = 12L,
      seed = 20260712L,
      include_gam = FALSE
    )
    scores <- sm_score_smoothing_simulation(simulation)
    smoothing_simulation_cache$result <- list(
      simulation = simulation,
      scores = scores,
      decisions = sm_decide_smoothing_simulation(scores)
    )
  }
  smoothing_simulation_cache$result
}

smoothing_metric_value <- function(audit, scale, method, metric) {
  if (metric %in% names(audit$scores$overall)) {
    rows <- audit$scores$overall$scale == scale &
      audit$scores$overall$method == method
    return(audit$scores$overall[[metric]][rows])
  }
  if (metric %in% names(audit$scores$weights)) {
    rows <- audit$scores$weights$scale == scale &
      audit$scores$weights$method == method
    return(audit$scores$weights[[metric]][rows])
  }
  rows <- audit$decisions$scale == scale & audit$decisions$method == method
  audit$decisions[[metric]][rows]
}

test_that("smoothing simulation grid and decision criteria are prespecified", {
  grid <- sm_smoothing_simulation_grid()
  criteria <- sm_smoothing_simulation_criteria()

  expect_equal(nrow(grid), 84L)
  expect_setequal(unique(grid$n_band), c("small_n", "large_n"))
  expect_setequal(unique(grid$p_band), c("near_boundary", "interior"))
  expect_equal(length(unique(grid$n)), 12L)
  expect_equal(length(unique(grid$p)), 7L)
  expect_equal(
    criteria$criterion,
    c(
      "overall_relative_mse_ratio",
      "worst_stratum_relative_mse_ratio",
      "overall_coverage_error_increase",
      "worst_stratum_coverage_error_increase",
      "inverse_weight_tv_ratio"
    )
  )
  expect_equal(criteria$threshold, c(0.90, 1.10, 0.01, 0.03, 0.95))
})

test_that("fast smoothing simulation matches its tolerance fixture", {
  audit <- smoothing_fast_audit()
  reference <- utils::read.csv(testthat::test_path(
    "_data", "smoothing", "smoothing-simulation-fast-reference.csv"
  ))

  expect_equal(audit$simulation$metadata$seed, 20260712L)
  expect_equal(audit$simulation$metadata$reps, 12L)
  expect_equal(audit$simulation$metadata$grid_rows, 84L)
  expect_equal(audit$simulation$metadata$methods, c("unsmoothed", "loglinear"))

  for (i in seq_len(nrow(reference))) {
    actual <- smoothing_metric_value(
      audit,
      scale = reference$scale[[i]],
      method = reference$method[[i]],
      metric = reference$metric[[i]]
    )
    expect_equal(
      length(actual),
      1L,
      info = paste(reference[i, 1:3], collapse = "/")
    )
    expect_lte(
      abs(actual - reference$expected[[i]]),
      reference$tolerance[[i]]
    )
  }

  expect_true(all(audit$decisions$decision == "NO-GO"))
})

test_that("smoothing simulation preserves RNG kind and seed presence exactly", {
  original <- .sm_simulation_preserve_rng()
  on.exit(.sm_simulation_restore_rng(original), add = TRUE)

  RNGkind("L'Ecuyer-CMRG", "Box-Muller", "Rejection")
  set.seed(314159L)
  seeded_before <- .sm_simulation_preserve_rng()
  invisible(sm_run_smoothing_simulation(
    reps = 2L,
    seed = 20260712L,
    include_gam = FALSE
  ))
  expect_identical(.sm_simulation_preserve_rng(), seeded_before)

  RNGkind("L'Ecuyer-CMRG", "Box-Muller", "Rejection")
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  unseeded_before <- .sm_simulation_preserve_rng()
  invisible(sm_run_smoothing_simulation(
    reps = 2L,
    seed = 20260712L,
    include_gam = FALSE
  ))
  unseeded_after <- .sm_simulation_preserve_rng()

  expect_false(unseeded_before$seed_exists)
  expect_false(unseeded_after$seed_exists)
  expect_identical(unseeded_after$kind, unseeded_before$kind)
  expect_null(unseeded_after$seed)
})

test_that("simulation candidates remain append-only and cannot stale matching V", {
  grid <- sm_smoothing_simulation_grid()
  count_data <- data.frame(
    site_id = grid$site_id,
    year = grid$year,
    n_jt = grid$n,
    c_jt_absent = as.integer(round(grid$n * grid$p)),
    stringsAsFactors = FALSE
  )
  estimates <- sitemix::sm_estimate_from_counts(
    count_data,
    family = "binomial",
    indicator = "absent",
    vst = "arcsine",
    boundary_method = "wilson_floor",
    vjt = TRUE,
    min_n = 1L
  )

  appended <- sitemix::sm_smooth_variance(
    estimates,
    method = "loglinear",
    scale = "se",
    min_rows = 50L,
    overwrite = FALSE
  )
  expect_equal(appended$theta_raw, estimates$theta_raw)
  expect_equal(appended$theta_hat, estimates$theta_hat)
  expect_equal(appended$se, estimates$se)
  expect_equal(appended$se_raw, estimates$se_raw)
  expect_equal(appended$var_method, estimates$var_method)
  expect_equal(appended$V, estimates$V)
  expect_equal(attr(appended, "smoothing")$v$relation, "matching")

  expect_error(
    sitemix::sm_smooth_variance(
      estimates,
      method = "loglinear",
      scale = "se",
      min_rows = 50L,
      overwrite = TRUE
    ),
    class = "sitemix_error_smoothing_v_stale"
  )

  raw_appended <- suppressWarnings(sitemix::sm_smooth_variance(
    estimates,
    method = "loglinear",
    scale = "se_raw",
    min_rows = 50L,
    overwrite = FALSE
  ))
  expect_equal(raw_appended$theta_raw, estimates$theta_raw)
  expect_equal(raw_appended$theta_hat, estimates$theta_hat)
  expect_equal(raw_appended$se, estimates$se)
  expect_equal(raw_appended$se_raw, estimates$se_raw)
  expect_equal(raw_appended$var_method, estimates$var_method)
  expect_equal(raw_appended$V, estimates$V)
  expect_equal(attr(raw_appended, "smoothing")$v$relation, "incompatible")
})
