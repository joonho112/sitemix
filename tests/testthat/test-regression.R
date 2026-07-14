test_that("ALprek regression baselines remain stable", {
  baseline <- readRDS(testthat::test_path("_data", "regression", "regression-baselines.rds"))
  current <- regression_build_baselines()

  expect_equal(current$metadata, baseline$metadata)
  expect_equal(current$tolerances, baseline$tolerances)
  expect_equal(current$alprek_summary, baseline$alprek_summary)
  expect_equal(current$alprek_content, baseline$alprek_content)
})

test_that("A/B/C regression outputs match deterministic baselines", {
  baseline <- readRDS(testthat::test_path("_data", "regression", "regression-baselines.rds"))
  current <- regression_build_baselines()
  tol <- baseline$tolerances$scalar
  matrix_tol <- baseline$tolerances$matrix

  for (name in c("scenario_a", "scenario_b", "scenario_c")) {
    expect_equal(current[[name]]$attrs, baseline[[name]]$attrs)
    expect_equal(current[[name]]$core, baseline[[name]]$core, tolerance = tol)
    expect_equal(current[[name]]$vcov, baseline[[name]]$vcov, tolerance = matrix_tol)
  }
})

test_that("D0/D1 aggregate regression outputs match deterministic baselines", {
  baseline <- readRDS(testthat::test_path("_data", "regression", "regression-baselines.rds"))
  current <- regression_build_baselines()
  tol <- baseline$tolerances$scalar
  matrix_tol <- baseline$tolerances$matrix

  for (name in c("aggregate_d0", "aggregate_d1")) {
    expect_equal(current[[name]]$attrs, baseline[[name]]$attrs)
    expect_equal(current[[name]]$core, baseline[[name]]$core, tolerance = tol)
    expect_equal(current[[name]]$vcov, baseline[[name]]$vcov, tolerance = matrix_tol)
  }

  expect_equal(current$aggregate_d0$attrs$aggregate_case, "D0")
  expect_equal(current$aggregate_d1$attrs$aggregate_case, "D1")
  expect_equal(current$aggregate_d1$attrs$d1_regime, "D1a")
  expect_true(all(vapply(
    current$aggregate_d1$vcov,
    function(x) identical(x$vcov_method, "working_independence"),
    logical(1)
  )))
})

test_that("ALprek 2024 aggregate regression pins match deterministic baselines", {
  baseline <- readRDS(testthat::test_path("_data", "regression", "regression-baselines.rds"))
  current <- regression_build_baselines()
  tol <- baseline$tolerances$scalar
  matrix_tol <- baseline$tolerances$matrix

  expect_equal(
    current$aggregate_d0_alprek_2024_frpm$attrs,
    baseline$aggregate_d0_alprek_2024_frpm$attrs
  )
  expect_equal(
    current$aggregate_d0_alprek_2024_frpm$summary,
    baseline$aggregate_d0_alprek_2024_frpm$summary,
    tolerance = tol
  )
  expect_equal(
    current$aggregate_d0_alprek_2024_frpm$sentinel_rows,
    baseline$aggregate_d0_alprek_2024_frpm$sentinel_rows,
    tolerance = matrix_tol
  )

  expect_equal(
    current$aggregate_d1_alprek_2024_four_indicator$attrs,
    baseline$aggregate_d1_alprek_2024_four_indicator$attrs
  )
  expect_equal(
    current$aggregate_d1_alprek_2024_four_indicator$by_indicator,
    baseline$aggregate_d1_alprek_2024_four_indicator$by_indicator,
    tolerance = tol
  )
  expect_equal(
    current$aggregate_d1_alprek_2024_four_indicator$sentinel_rows,
    baseline$aggregate_d1_alprek_2024_four_indicator$sentinel_rows,
    tolerance = matrix_tol
  )
  expect_equal(
    current$aggregate_d1_alprek_2024_four_indicator$vcov_pins,
    baseline$aggregate_d1_alprek_2024_four_indicator$vcov_pins,
    tolerance = matrix_tol
  )
  expect_lte(max(current$aggregate_d1_alprek_2024_four_indicator$vcov_pins$max_abs_offdiag), 1e-12)
})

test_that("review CSVs are byte-exact projections of the protected baseline", {
  fixture_dir <- testthat::test_path("_data", "regression")
  baseline <- readRDS(file.path(fixture_dir, "regression-baselines.rds"))
  replay_dir <- tempfile("sitemix-regression-review-")
  on.exit(unlink(replay_dir, recursive = TRUE, force = TRUE), add = TRUE)

  regression_write_review_csvs(baseline, replay_dir)
  expected <- c(
    "alprek_summary.csv",
    "alprek_spotcheck_rows.csv",
    "alprek_spotcheck_vcov.csv",
    "small_cases_rows.csv",
    "small_cases_vcov.csv"
  )
  expect_identical(sort(list.files(replay_dir)), sort(expected))

  for (name in expected) {
    current_path <- file.path(fixture_dir, name)
    replay_path <- file.path(replay_dir, name)
    current_bytes <- readBin(current_path, what = "raw", n = file.size(current_path))
    replay_bytes <- readBin(replay_path, what = "raw", n = file.size(replay_path))
    expect_identical(replay_bytes, current_bytes, info = name)
  }
})
