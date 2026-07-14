subset_semantics_cache <- new.env(parent = emptyenv())

subset_fixture_b <- function() {
  if (is.null(subset_semantics_cache$b)) {
    data_env <- new.env(parent = emptyenv())
    utils::data("alprek_subset", package = "sitemix", envir = data_env)
    dat <- data_env$alprek_subset
    sites <- unique(dat$site_id[dat$year == 2024L])[1:2]
    subset_semantics_cache$b <- sm_estimate(
      dat[dat$year == 2024L & dat$site_id %in% sites, , drop = FALSE],
      family = "multivariate",
      indicators = c("frpm", "snap"),
      vjt = TRUE,
      description = "subset semantics fixture"
    )
  }
  subset_semantics_cache$b
}

subset_fixture_c <- function() {
  if (is.null(subset_semantics_cache$c)) {
    counts <- data.frame(
      site_id = c("M1", "M2"),
      year = c(2025L, 2025L),
      n_jt = c(30L, 40L),
      c_jt_eng = c(10L, 14L),
      c_jt_spa = c(12L, 16L),
      c_jt_oth = c(8L, 10L),
      stringsAsFactors = FALSE
    )
    subset_semantics_cache$c <- sm_estimate_from_counts(
      counts,
      family = "multinomial",
      indicators = c("eng", "spa", "oth"),
      vjt = TRUE
    )
  }
  subset_semantics_cache$c
}

subset_fixture_d1 <- function() {
  if (is.null(subset_semantics_cache$d1)) {
    aggregate <- data.frame(
      site_id = rep(c("D1", "D2"), each = 2L),
      year = rep(2025L, 4L),
      indicator = rep(c("a", "b"), 2L),
      c_jt = c(8L, 12L, 15L, 10L),
      n_jt = c(20L, 25L, 25L, 30L),
      stringsAsFactors = FALSE
    )
    subset_semantics_cache$d1 <- suppressWarnings(
      sm_estimate_from_aggregates(
        aggregate,
        family = "multivariate",
        sampling_relation = "same_units",
        vjt = TRUE,
        fpc = 100L,
        min_n = 1L
      )
    )
  }
  subset_semantics_cache$d1
}

subset_fixture_smoothed <- function() {
  if (is.null(subset_semantics_cache$smoothed)) {
    data_env <- new.env(parent = emptyenv())
    utils::data("alprek_subset", package = "sitemix", envir = data_env)
    dat <- data_env$alprek_subset
    sites <- unique(dat$site_id[dat$year == 2024L])[1:10]
    estimates <- sm_estimate(
      dat[dat$year == 2024L & dat$site_id %in% sites, , drop = FALSE],
      family = "binomial",
      indicator = "frpm"
    )
    subset_semantics_cache$smoothed <- suppressWarnings(
      sm_smooth_variance(estimates, min_rows = 5L, overwrite = FALSE)
    )
  }
  subset_semantics_cache$smoothed
}

test_that("base row subsets preserve complete matrix groups and reject partial groups", {
  x <- subset_fixture_b()
  first_site <- x$site_id[[1L]]
  idx <- which(x$site_id == first_site)

  complete <- x[idx, , drop = FALSE]
  expect_s3_class(complete, "sitemix_estimates")
  expect_true(validate.sitemix_estimates(complete))
  expect_identical(attr(complete, "family", exact = TRUE), "multivariate")
  expect_identical(
    attr(complete, "description", exact = TRUE),
    "subset semantics fixture"
  )
  expect_identical(
    attr(complete, "sitemix_role", exact = TRUE),
    "summary_uncertainty"
  )

  expect_error(
    x[idx[[1L]], , drop = FALSE],
    "partial covariance group",
    class = "sitemix_error_estimate_vcov_invariant"
  )

  empty <- x[0, , drop = FALSE]
  expect_s3_class(empty, "sitemix_estimates")
  expect_true(validate.sitemix_estimates(empty))
})

test_that("base row reorder realigns every repeated covariance coordinate", {
  x <- subset_fixture_b()
  first_site <- x$site_id[[1L]]
  group <- x[x$site_id == first_site, , drop = FALSE]
  expected_order <- rev(group$indicator)
  expected_matrix <- as.matrix(group$V[[1L]])[
    expected_order,
    expected_order,
    drop = FALSE
  ]

  reordered <- group[rev(seq_len(nrow(group))), , drop = FALSE]
  expect_s3_class(reordered, "sitemix_estimates")
  expect_true(validate.sitemix_estimates(reordered))
  expect_identical(reordered$V[[1L]]$indicator_order, expected_order)
  expect_equal(as.matrix(reordered$V[[1L]]), expected_matrix)
  expect_true(all(vapply(
    reordered$V,
    function(value) sitemix:::.sm_vcov_value_equal(value, reordered$V[[1L]]),
    logical(1)
  )))
  expect_identical(reordered$K, rep(2L, 2L))

  site_order <- rev(unique(x$site_id))
  rows <- unlist(lapply(site_order, function(site) which(x$site_id == site)))
  blocks <- x[rows, , drop = FALSE]
  expect_true(validate.sitemix_estimates(blocks))
  expect_identical(unique(blocks$site_id), site_order)
})

test_that("multinomial reorder preserves analytic rank and simplex covariance", {
  x <- subset_fixture_c()
  group <- x[x$site_id == "M1", , drop = FALSE]
  expected_order <- rev(group$indicator)
  expected <- as.matrix(group$V[[1L]])[
    expected_order,
    expected_order,
    drop = FALSE
  ]

  reordered <- group[rev(seq_len(nrow(group))), , drop = FALSE]
  expect_true(validate.sitemix_estimates(reordered))
  expect_identical(reordered$V[[1L]]$indicator_order, expected_order)
  expect_identical(reordered$V[[1L]]$matrix_rank, 2L)
  expect_identical(reordered$V[[1L]]$positive_support, 3L)
  expect_equal(as.matrix(reordered$V[[1L]]), expected)
  coordinate_fields <- c(
    "scalar_correction_rule", "sampling_fraction",
    "fpc_variance_multiplier", "fpc_se_multiplier",
    "variance_multiplier_applied", "se_multiplier_applied", "variance_rule"
  )
  for (field in coordinate_fields) {
    expect_identical(
      reordered$V[[1L]][[field]],
      rev(group$V[[1L]][[field]]),
      info = field
    )
  }
  expect_equal(
    as.vector(as.matrix(reordered$V[[1L]]) %*% rep(1, 3)),
    rep(0, 3),
    tolerance = 1e-12
  )
})

test_that("column selection drops or preserves the class deterministically", {
  x <- subset_fixture_b()

  missing_required <- x[, setdiff(names(x), "se"), drop = FALSE]
  expect_s3_class(missing_required, "tbl_df")
  expect_false(inherits(missing_required, "sitemix_estimates"))
  expect_null(attr(missing_required, "family", exact = TRUE))
  expect_null(attr(missing_required, "sitemix_role", exact = TRUE))

  scalar_transport <- x[
    ,
    setdiff(names(x), c("V", "K")),
    drop = FALSE
  ]
  expect_s3_class(scalar_transport, "sitemix_estimates")
  expect_true(validate.sitemix_estimates(scalar_transport))
  expect_false("V" %in% names(scalar_transport))
  expect_false("K" %in% names(scalar_transport))
  expect_identical(
    attr(scalar_transport, "family", exact = TRUE),
    attr(x, "family", exact = TRUE)
  )

  scalar_transport$audit_note <- seq_len(nrow(scalar_transport))
  expect_true(validate.sitemix_estimates(scalar_transport))
  expect_identical(scalar_transport$audit_note, seq_len(nrow(x)))

  reserved <- scalar_transport
  reserved$vcov_method <- "audit"
  expect_error(
    validate.sitemix_estimates(reserved),
    "reserved names",
    class = "sitemix_error_estimate_var_method"
  )

  audit_first <- scalar_transport[
    ,
    c("audit_note", setdiff(names(scalar_transport), "audit_note")),
    drop = FALSE
  ]
  expect_s3_class(audit_first, "tbl_df")
  expect_false(inherits(audit_first, "sitemix_estimates"))

  expect_error(
    scalar_transport[c(1L, 1L), , drop = FALSE],
    "unique by",
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("dplyr verbs share immediate subset and mutation semantics", {
  testthat::skip_if_not_installed("dplyr")
  x <- subset_fixture_b()
  first_site <- x$site_id[[1L]]

  complete <- dplyr::filter(x, site_id == first_site)
  expect_s3_class(complete, "sitemix_estimates")
  expect_true(validate.sitemix_estimates(complete))

  expect_error(
    dplyr::filter(x, site_id == first_site, indicator == "frpm"),
    "partial covariance group",
    class = "sitemix_error_estimate_vcov_invariant"
  )

  reordered <- dplyr::arrange(complete, dplyr::desc(indicator))
  expect_true(validate.sitemix_estimates(reordered))
  expect_identical(reordered$V[[1L]]$indicator_order, reordered$indicator)

  missing_required <- dplyr::select(x, -se)
  expect_s3_class(missing_required, "tbl_df")
  expect_false(inherits(missing_required, "sitemix_estimates"))

  scalar_transport <- dplyr::select(x, -V, -K)
  expect_s3_class(scalar_transport, "sitemix_estimates")
  expect_true(validate.sitemix_estimates(scalar_transport))

  with_audit <- dplyr::mutate(
    scalar_transport,
    audit_note = dplyr::row_number()
  )
  expect_s3_class(with_audit, "sitemix_estimates")
  expect_true(validate.sitemix_estimates(with_audit))
  expect_identical(
    attr(with_audit, "family", exact = TRUE),
    attr(x, "family", exact = TRUE)
  )

  relocated <- dplyr::relocate(with_audit, audit_note, .before = site_id)
  expect_s3_class(relocated, "tbl_df")
  expect_false(inherits(relocated, "sitemix_estimates"))

  expect_error(
    dplyr::mutate(scalar_transport, se = se + 0.1),
    "not reproducible",
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("D1 full-group subsets refresh group provenance and repeated V", {
  x <- subset_fixture_d1()
  one <- x[x$site_id == "D1" & x$year == 2025L, , drop = FALSE]
  by_group <- attr(one, "d1_regime_by_group", exact = TRUE)

  expect_s3_class(one, "sitemix_estimates")
  expect_true(validate.sitemix_estimates(one))
  expect_equal(nrow(by_group), 1L)
  expect_identical(by_group$site_id, "D1")
  expect_identical(attr(one, "sampling_relation", exact = TRUE), "same_units")
  expect_identical(attr(one, "denominator_pattern", exact = TRUE), "varying")
  expect_identical(attr(one, "d1_regime", exact = TRUE), "D1a")
  expect_identical(attr(one, "suppression", exact = TRUE)$n_suppressed, 0L)

  original_v <- one$V[[1L]]
  reordered <- one[rev(seq_len(nrow(one))), , drop = FALSE]
  expect_true(validate.sitemix_estimates(reordered))
  expect_identical(reordered$V[[1L]]$indicator_order, reordered$indicator)
  expect_identical(
    reordered$V[[1L]]$sampling_fraction,
    rev(original_v$sampling_fraction)
  )
  expect_identical(
    reordered$V[[1L]]$fpc_variance_multiplier,
    rev(original_v$fpc_variance_multiplier)
  )
  expect_identical(
    reordered$V[[1L]]$variance_multiplier_applied,
    rev(original_v$variance_multiplier_applied)
  )

  expect_error(
    one[1L, , drop = FALSE],
    "partial covariance group",
    class = "sitemix_error_estimate_vcov_invariant"
  )

  mixed <- x
  mixed_by_group <- attr(mixed, "d1_regime_by_group", exact = TRUE)
  mixed_by_group$denominator_pattern <- c("common", "varying")
  attr(mixed, "d1_regime_by_group") <- mixed_by_group
  mixed <- mixed[seq_len(nrow(mixed)), , drop = FALSE]
  expect_true(validate.sitemix_estimates(mixed))
  expect_identical(attr(mixed, "denominator_pattern", exact = TRUE), "mixed")

  misaligned <- x
  misaligned_by_group <- attr(misaligned, "d1_regime_by_group", exact = TRUE)
  misaligned_by_group$site_id <- paste0("missing-", misaligned_by_group$site_id)
  attr(misaligned, "d1_regime_by_group") <- misaligned_by_group
  condition <- expect_error(
    misaligned[seq_len(nrow(misaligned)), , drop = FALSE],
    "cannot be aligned after row selection",
    class = "sitemix_error_estimate_var_method"
  )
  expect_identical(condition$actual, unique(paste(x$site_id, x$year, sep = "\r")))
})

test_that("smoothing provenance remaps after row selection or optional-column removal", {
  x <- subset_fixture_smoothed()
  selected <- x[c(5L, 2L, 8L), , drop = FALSE]
  smoothing <- attr(selected, "smoothing", exact = TRUE)

  expect_true(validate.sitemix_estimates(selected))
  expect_identical(smoothing$eligible_rows, seq_len(3L))
  expect_identical(smoothing$n_eligible, 3L)
  expect_identical(smoothing$target_column, "se_smoothed")

  smoothing_columns <- intersect(
    names(x),
    c(
      "se_smoothed", "se_raw_smoothed", "var_method_smoothed",
      "se_pre_smoothing", "se_raw_pre_smoothing", "residual_log_var"
    )
  )
  canonical <- x[, setdiff(names(x), smoothing_columns), drop = FALSE]
  expect_s3_class(canonical, "sitemix_estimates")
  expect_true(validate.sitemix_estimates(canonical))
  expect_null(attr(canonical, "smoothing", exact = TRUE))
  expect_null(attr(canonical, "smoother_fit", exact = TRUE))
  expect_null(attr(canonical, "smoother_fit_summary", exact = TRUE))
})

test_that("suppression acknowledgement provenance follows retained rows", {
  aggregate <- data.frame(
    site_id = c("S1", "S2", "S3"),
    year = rep(2025L, 3L),
    indicator = rep("a", 3L),
    c_jt = c(NA_integer_, 4L, 6L),
    n_jt = c(10L, 10L, 10L),
    suppression_flag = c(TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  x <- sm_estimate_from_aggregates(
    aggregate,
    family = "binomial",
    suppression = "upper_bound",
    suppression_sensitivity_acknowledge = TRUE,
    vjt = FALSE,
    min_n = 1L
  )

  identified <- x[!x$flag_suppressed, , drop = FALSE]
  identified_suppression <- attr(identified, "suppression", exact = TRUE)
  expect_true(validate.sitemix_estimates(identified))
  expect_identical(identified_suppression$n_suppressed, 0L)
  expect_false(identified_suppression$sensitivity_acknowledged)
  expect_identical(identified_suppression$sensitivity_role, "none")
  expect_false(identified_suppression$has_hidden_denominator)

  sensitivity <- x[x$flag_suppressed, , drop = FALSE]
  sensitivity_suppression <- attr(sensitivity, "suppression", exact = TRUE)
  expect_true(validate.sitemix_estimates(sensitivity))
  expect_identical(sensitivity_suppression$n_suppressed, 1L)
  expect_true(sensitivity_suppression$sensitivity_acknowledged)
  expect_identical(
    sensitivity_suppression$sensitivity_role,
    "nonidentified_variance_sensitivity"
  )
})

test_that("empty aggregate subsets retain typed suppression provenance", {
  aggregate <- data.frame(
    site_id = c("S1", "S2"),
    year = rep(2025L, 2L),
    indicator = rep("a", 2L),
    c_jt = c(NA_integer_, 4L),
    n_jt = c(10L, 10L),
    suppression_flag = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  d0_drop <- sm_estimate_from_aggregates(
    aggregate,
    family = "binomial",
    suppression = "drop",
    vjt = FALSE,
    min_n = 1L
  )
  d0_upper <- sm_estimate_from_aggregates(
    aggregate,
    family = "binomial",
    suppression = "upper_bound",
    suppression_sensitivity_acknowledge = TRUE,
    vjt = FALSE,
    min_n = 1L
  )

  for (value in list(d0_drop, d0_upper, subset_fixture_d1())) {
    empty <- value[0, , drop = FALSE]
    suppression <- attr(empty, "suppression", exact = TRUE)
    expect_s3_class(empty, "sitemix_estimates")
    expect_true(validate.sitemix_estimates(empty))
    expect_identical(nrow(empty), 0L)
    expect_identical(suppression$n_suppressed, 0L)
    expect_false(suppression$sensitivity_acknowledged)
    expect_identical(suppression$sensitivity_role, "none")
    expect_false(suppression$has_hidden_denominator)
    expect_true(suppression$denominator_observed_on_suppressed)
  }
  expect_equal(
    nrow(attr(subset_fixture_d1()[0, , drop = FALSE], "d1_regime_by_group")),
    0L
  )
})

test_that("sensitivity-only subsets recover observed versus hidden denominators", {
  aggregate <- data.frame(
    site_id = c("H", "O", "Z"),
    year = rep(2025L, 3L),
    indicator = rep("a", 3L),
    c_jt = c(NA_integer_, NA_integer_, 5L),
    n_jt = c(NA_integer_, 10L, 10L),
    suppression_flag = c(TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  x <- sm_estimate_from_aggregates(
    aggregate,
    family = "binomial",
    suppression = "upper_bound",
    suppression_sensitivity_acknowledge = TRUE,
    suppressed_n_strategy = "worst_case_bound",
    suppressed_n_bound = 1L,
    vjt = FALSE,
    min_n = 1L
  )

  observed <- x[x$site_id == "O", , drop = FALSE]
  observed_suppression <- attr(observed, "suppression", exact = TRUE)
  expect_identical(
    observed$sensitivity_method,
    "worst_case_variance_observed_n"
  )
  expect_true(validate.sitemix_estimates(observed))
  expect_false(observed_suppression$has_hidden_denominator)
  expect_true(observed_suppression$denominator_observed_on_suppressed)
  expect_true(observed_suppression$sensitivity_acknowledged)

  hidden <- x[x$site_id == "H", , drop = FALSE]
  hidden_suppression <- attr(hidden, "suppression", exact = TRUE)
  expect_identical(
    hidden$sensitivity_method,
    "unquantified_hidden_denominator"
  )
  expect_true(validate.sitemix_estimates(hidden))
  expect_true(hidden_suppression$has_hidden_denominator)
  expect_false(hidden_suppression$denominator_observed_on_suppressed)
  expect_true(hidden_suppression$sensitivity_acknowledged)
})
