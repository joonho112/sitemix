test_that("prek_sim is documented and loadable", {
  env <- new.env(parent = emptyenv())
  data("prek_sim", package = "sitemix", envir = env)
  x <- env$prek_sim

  expect_s3_class(x, "tbl_df")
  expect_equal(names(x), c("student_id", "site_id", "year", "frpm", "snap", "wic", "tanf"))
  expect_equal(nrow(x), 7845L)
  expect_equal(ncol(x), 7L)
  expect_equal(length(unique(x$site_id)), 50L)
  expect_equal(sort(unique(x$year)), 2021:2025)
  expect_true(all(grepl("^S[0-9]{3}$", x$site_id)))
  expect_true(all(grepl("^ST[0-9]{5}$", x$student_id)))
  expect_false(anyDuplicated(x$student_id) > 0L)
  expect_true(all(vapply(x[c("frpm", "snap", "wic", "tanf")], function(col) {
    all(col %in% c(0L, 1L))
  }, logical(1))))

  info <- attr(x, "build_info")
  expect_type(info, "list")
  expect_equal(info$row_count, nrow(x))
  expect_equal(info$site_year_count, 250L)
  expect_equal(info$seed, 20260002L)
  expect_equal(info$size_seed, 909L)
  expect_identical(info$data_kind, "fully_simulated")
  expect_false(info$restricted_input_used)
  expect_lt(max(unlist(info$calibration_residual)), 0.01)
  expect_equal(info$panel_summary$sites, 50L)
  expect_true(info$panel_summary$max_cell_n >= 110L)
  expect_true(info$panel_summary$cells_at_least_100 >= 4L)
})

test_that("prek_sim extdata artifacts are portable", {
  counts_path <- system.file("extdata", "prek_sim_counts.rds", package = "sitemix", mustWork = TRUE)
  csv_path <- system.file("extdata", "prek_sim.csv", package = "sitemix", mustWork = TRUE)
  provenance_path <- system.file("extdata", "prek_sim_design.txt", package = "sitemix", mustWork = TRUE)

  expect_true(file.exists(counts_path))
  expect_true(file.exists(csv_path))
  expect_true(file.exists(provenance_path))

  env <- new.env(parent = emptyenv())
  data("prek_sim", package = "sitemix", envir = env)
  x <- env$prek_sim
  csv <- utils::read.csv(
    csv_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  counts <- readRDS(counts_path)
  expect_s3_class(counts, "tbl_df")
  expect_equal(dim(counts), c(250L, 13L))
  expect_equal(
    names(counts),
    c(
      "site_id",
      "year",
      "n_jt",
      "c_jt_frpm",
      "c_jt_snap",
      "c_jt_wic",
      "c_jt_tanf",
      "c_jt_frpm_snap",
      "c_jt_frpm_wic",
      "c_jt_frpm_tanf",
      "c_jt_snap_wic",
      "c_jt_snap_tanf",
      "c_jt_wic_tanf"
    )
  )
  expect_equal(attr(counts, "build_info")$row_count, 7845L)
  expect_equal(attr(counts, "build_info")$site_year_count, 250L)
  expect_identical(attr(counts, "build_info"), attr(x, "build_info"))

  plain_x <- as.data.frame(x, stringsAsFactors = FALSE)
  attr(plain_x, "build_info") <- NULL
  expect_identical(csv, plain_x)

  row_level <- data.frame(
    n_jt = rep.int(1L, nrow(x)),
    c_jt_frpm = x$frpm,
    c_jt_snap = x$snap,
    c_jt_wic = x$wic,
    c_jt_tanf = x$tanf,
    c_jt_frpm_snap = x$frpm * x$snap,
    c_jt_frpm_wic = x$frpm * x$wic,
    c_jt_frpm_tanf = x$frpm * x$tanf,
    c_jt_snap_wic = x$snap * x$wic,
    c_jt_snap_tanf = x$snap * x$tanf,
    c_jt_wic_tanf = x$wic * x$tanf
  )
  recomputed <- stats::aggregate(
    row_level,
    by = list(site_id = x$site_id, year = x$year),
    FUN = sum
  )
  recomputed <- recomputed[
    order(recomputed$site_id, recomputed$year),
    names(counts),
    drop = FALSE
  ]
  row.names(recomputed) <- NULL
  plain_counts <- as.data.frame(counts, stringsAsFactors = FALSE)
  attr(plain_counts, "build_info") <- NULL
  expect_identical(recomputed, plain_counts)

  design <- readLines(provenance_path, warn = FALSE)
  expect_match(
    design,
    "prek_sim is a fully simulated panel",
    all = FALSE
  )
  expect_false(any(grepl(
    "restricted|confidential|administrative source",
    design,
    ignore.case = TRUE
  )))
})

test_that("student-level and pre-aggregated artifacts agree", {
  env <- new.env(parent = emptyenv())
  data("prek_sim", package = "sitemix", envir = env)
  x <- env$prek_sim
  counts <- readRDS(system.file("extdata", "prek_sim_counts.rds", package = "sitemix", mustWork = TRUE))

  indicators <- c("frpm", "snap", "wic", "tanf")
  out_student <- sm_estimate(
    x,
    family = "multivariate",
    indicators = indicators,
    na_action = "error"
  )
  out_counts <- sm_estimate(
    counts,
    family = "multivariate",
    indicators = indicators,
    from_counts = TRUE
  )

  expect_s3_class(out_student, "sitemix_estimates")
  expect_s3_class(out_counts, "sitemix_estimates")
  expect_equal(nrow(out_student), 1000L)
  expect_equal(nrow(out_counts), 1000L)
  expect_equal(out_counts$site_id, out_student$site_id)
  expect_equal(out_counts$year, out_student$year)
  expect_equal(out_counts$indicator, out_student$indicator)
  expect_equal(out_counts$n, out_student$n)
  expect_equal(out_counts$theta_raw, out_student$theta_raw, tolerance = 1e-12)
  expect_equal(out_counts$theta_hat, out_student$theta_hat, tolerance = 1e-12)
  expect_equal(out_counts$se_raw, out_student$se_raw, tolerance = 1e-12)
  expect_equal(out_counts$se, out_student$se, tolerance = 1e-12)
  expect_true(validate.sitemix_estimates(out_student))
  expect_true(validate.sitemix_estimates(out_counts))
})
