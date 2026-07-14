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

test_that("review CSVs are value-exact projections of the protected baseline", {
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

  # Compare parsed values, not raw bytes. write.csv() renders doubles to
  # ~15 significant digits, so a last-digit (~1e-15, one-ULP) cross-platform
  # difference in a value breaks a byte-exact check even though the numbers
  # are identical to machine precision. Numeric columns are compared with a
  # tight tolerance; every other column must match exactly.
  read_review_csv <- function(path) {
    utils::read.csv(path, colClasses = "character", check.names = FALSE)
  }
  for (name in expected) {
    current_csv <- read_review_csv(file.path(fixture_dir, name))
    replay_csv <- read_review_csv(file.path(replay_dir, name))
    expect_identical(dim(replay_csv), dim(current_csv), info = name)
    expect_identical(names(replay_csv), names(current_csv), info = name)
    for (col in names(current_csv)) {
      current_col <- current_csv[[col]]
      replay_col <- replay_csv[[col]]
      current_num <- suppressWarnings(as.numeric(current_col))
      replay_num <- suppressWarnings(as.numeric(replay_col))
      numeric_col <- all(is.na(current_num) == is.na(current_col)) &&
        all(is.na(replay_num) == is.na(replay_col))
      if (numeric_col) {
        expect_equal(
          replay_num,
          current_num,
          tolerance = 1e-8,
          info = paste(name, col)
        )
      } else {
        expect_identical(replay_col, current_col, info = paste(name, col))
      }
    }
  }
})
