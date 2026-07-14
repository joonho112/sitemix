make_output_vcov <- function(
  site_id = "S001",
  year = 2024L,
  indicators = c("a", "b"),
  family = "multivariate",
  estimate_scale = "arcsine"
) {
  mat <- matrix(
    c(0.001875, 0.0002, 0.0002, 0.002304),
    2,
    2,
    dimnames = list(indicators, indicators)
  )
  sm_vcov(
    matrix = mat,
    site_id = site_id,
    year = year,
    indicator_order = indicators,
    family = family,
    vcov_method = if (identical(family, "multivariate")) "sur" else NA_character_,
    estimate_scale = estimate_scale,
    vcov_scale = "raw",
    scalar_correction_rule = rep("none", length(indicators)),
    n_jt = 100L,
    n_eff = 100,
    diag_contract = if (identical(family, "multivariate")) {
      "row_se_raw_squared"
    } else {
      "not_checked"
    }
  )
}

test_that("sm_one_row emits the locked default schema with v1.1 defaults", {
  row <- sitemix:::.sm_one_row(
    site_id = "S001",
    year = 2024,
    indicator = "absent",
    theta_raw = 0.25,
    se_raw = sqrt(0.25 * 0.75 / 100),
    n = 100L
  )

  expect_s3_class(row, "tbl_df")
  expect_identical(names(row), sitemix:::.sm_sitemix_columns)
  expect_equal(row$theta_hat, asin(sqrt(0.25)))
  expect_equal(row$se, 1 / (2 * sqrt(100)))
  expect_equal(row$var_method, "arcsine_vst")
  expect_equal(row$input_mode, "student_level")
  expect_false(row$flag_suppressed)
  expect_true(is.na(row$framing))
  expect_false(row$flag_below_accountability)

  x <- sitemix:::.sm_bind_sitemix_rows(row, family = "binomial")
  expect_s3_class(x, "sitemix_estimates")
  expect_true(validate.sitemix_estimates(x))
})

test_that("sm_one_row computes raw, logit, and anscombe scale rows", {
  raw <- sitemix:::.sm_one_row(
    site_id = "S001",
    year = 2024L,
    indicator = "raw",
    theta_raw = 0.25,
    se_raw = sqrt(0.25 * 0.75 / 100),
    n = 100L,
    estimate_scale = "none",
    var_method_raw = "binomial"
  )
  expect_equal(raw$theta_hat, raw$theta_raw)
  expect_equal(raw$se, raw$se_raw)
  expect_equal(raw$var_method, "binomial")

  logit <- sitemix:::.sm_one_row(
    site_id = "S001",
    year = 2024L,
    indicator = "logit",
    theta_raw = 0.25,
    se_raw = sqrt(0.25 * 0.75 / 100),
    n = 100L,
    estimate_scale = "logit"
  )
  expect_equal(logit$theta_hat, log(0.25 / 0.75))
  expect_equal(logit$se, 1 / sqrt(100 * 0.25 * 0.75))
  expect_equal(logit$var_method, "logit_delta")

  anscombe <- sitemix:::.sm_one_row(
    site_id = "S001",
    year = 2024L,
    indicator = "anscombe",
    theta_raw = 0.25,
    se_raw = sqrt(0.25 * 0.75 / 100),
    n = 100L,
    C = 25L,
    estimate_scale = "arcsine_anscombe"
  )
  expect_equal(anscombe$n_eff, 100.5)
  expect_equal(anscombe$theta_hat, asin(sqrt((25 + 3 / 8) / (100 + 3 / 4))))
  expect_equal(anscombe$var_method, "arcsine_anscombe")

  x <- sitemix:::.sm_bind_sitemix_rows(list(raw, logit, anscombe), family = "binomial")
  expect_true(validate.sitemix_estimates(x))
})

test_that("row binding preserves row order and rejects malformed row shapes", {
  row_a <- sitemix:::.sm_one_row("S002", 2024L, "a", 0.36, sqrt(0.36 * 0.64 / 100), 100L)
  row_b <- sitemix:::.sm_one_row("S001", 2024L, "a", 0.25, sqrt(0.25 * 0.75 / 100), 100L)

  x <- sitemix:::.sm_bind_sitemix_rows(
    list(row_a, row_b),
    description = "ordered rows",
    family = "binomial"
  )
  expect_equal(x$site_id, c("S002", "S001"))
  expect_equal(attr(x, "description"), "ordered rows")
  expect_type(x$year, "integer")
  expect_type(x$n, "integer")
  expect_type(x$n_eff, "double")

  missing <- row_a
  missing$se <- NULL
  expect_error(
    sitemix:::.sm_bind_sitemix_rows(list(missing), family = "binomial"),
    class = "sitemix_error_estimate_var_method"
  )

  extra <- row_a
  extra$vcov_method <- NA_character_
  expect_error(
    sitemix:::.sm_bind_sitemix_rows(list(extra), family = "binomial"),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("V list-column assembly preserves row count and K contract", {
  mat <- matrix(1 / (4 * 100), 1, 1, dimnames = list("absent", "absent"))
  V_bin <- sm_vcov(
    matrix = mat,
    site_id = "S001",
    year = 2024L,
    indicator_order = "absent",
    family = "binomial",
    vcov_method = NA_character_,
    estimate_scale = "arcsine",
    vcov_scale = "arcsine_delta",
    scalar_correction_rule = "none",
    n_jt = 100L,
    n_eff = 100
  )
  bin_row <- sitemix:::.sm_one_row(
    "S001", 2024L, "absent", 0.25, sqrt(0.25 * 0.75 / 100), 100L,
    V = V_bin
  )
  bin <- sitemix:::.sm_bind_sitemix_rows(bin_row, family = "binomial")
  expect_true("V" %in% names(bin))
  expect_false("K" %in% names(bin))
  expect_equal(nrow(bin), 1)

  V1 <- make_output_vcov()
  V2 <- make_output_vcov()
  row_a <- sitemix:::.sm_one_row("S001", 2024L, "a", 0.25, sqrt(0.25 * 0.75 / 100), 100L, V = V1, K = 2L)
  row_b <- sitemix:::.sm_one_row("S001", 2024L, "b", 0.36, sqrt(0.36 * 0.64 / 100), 100L, V = V2, K = 2L)
  mv <- sitemix:::.sm_bind_sitemix_rows(list(row_a, row_b), family = "multivariate")

  expect_equal(nrow(mv), 2)
  expect_equal(mv$K, c(2L, 2L))
  expect_true(sitemix:::.sm_vcov_value_equal(mv$V[[1]], mv$V[[2]]))
  expect_true(validate.sitemix_estimates(mv))
})

test_that("output assembly rejects mixed optional columns and V metadata drift", {
  row_no_v <- sitemix:::.sm_one_row("S001", 2024L, "a", 0.25, sqrt(0.25 * 0.75 / 100), 100L)
  row_v <- sitemix:::.sm_one_row(
    "S002", 2024L, "a", 0.25, sqrt(0.25 * 0.75 / 100), 100L,
    V = make_output_vcov(site_id = "S002", indicators = c("a", "b")),
    K = 2L
  )
  expect_error(
    sitemix:::.sm_bind_sitemix_rows(list(row_no_v, row_v), family = "multivariate"),
    class = "sitemix_error_estimate_vcov_invariant"
  )

  V_bad_site <- make_output_vcov(site_id = "OTHER")
  bad_a <- sitemix:::.sm_one_row("S001", 2024L, "a", 0.25, sqrt(0.25 * 0.75 / 100), 100L, V = V_bad_site, K = 2L)
  bad_b <- sitemix:::.sm_one_row("S001", 2024L, "b", 0.36, sqrt(0.36 * 0.64 / 100), 100L, V = V_bad_site, K = 2L)
  expect_error(
    sitemix:::.sm_bind_sitemix_rows(list(bad_a, bad_b), family = "multivariate"),
    class = "sitemix_error_estimate_vcov_invariant"
  )

  V_bad_family <- make_output_vcov(family = "multivariate")
  fam_a <- sitemix:::.sm_one_row("S001", 2024L, "a", 0.25, sqrt(0.25 * 0.75 / 100), 100L, V = V_bad_family, K = 2L)
  fam_b <- sitemix:::.sm_one_row("S001", 2024L, "b", 0.36, sqrt(0.36 * 0.64 / 100), 100L, V = V_bad_family, K = 2L)
  expect_error(
    sitemix:::.sm_bind_sitemix_rows(list(fam_a, fam_b), family = "binomial"),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})

test_that("group-local covariance reuse has exact Scenario B calls and outputs", {
  make_b <- function() {
    sm_estimate_from_counts(
      data.frame(
        site_id = "A", year = 2024L, n_jt = 10L,
        c_jt_a = 4L, c_jt_b = 6L, c_jt_a_b = 3L
      ),
      family = "multivariate",
      indicators = c("a", "b"),
      vjt = TRUE,
      min_n = 1L
    )
  }
  make_c <- function() {
    sm_estimate_from_counts(
      data.frame(
        site_id = "C", year = 2024L, n_jt = 10L,
        c_jt_x = 4L, c_jt_y = 3L, c_jt_z = 3L
      ),
      family = "multinomial",
      indicators = c("x", "y", "z"),
      vjt = TRUE,
      vst = "none",
      min_n = 1L
    )
  }
  make_d1 <- function() {
    withCallingHandlers(
      sm_estimate_from_aggregates(
        data.frame(
          site_id = "D", year = 2025L,
          c_jt_a = 20L, c_jt_b = 70L, n_jt = 100L
        ),
        family = "multivariate",
        sampling_relation = "same_units",
        vjt = TRUE,
        min_n = 1L
      ),
      sitemix_warning_working_independence_default = function(warning) {
        invokeRestart("muffleWarning")
      }
    )
  }

  reference <- list(B = make_b(), C = make_c(), D1 = make_d1())
  originals <- list(
    internal = sitemix:::.sm_validate_sm_vcov,
    alias = sitemix:::validate.sm_vcov,
    as_matrix = sitemix:::as.matrix.sm_vcov,
    psd = sitemix:::.sm_validate_vcov_psd,
    psd_tolerance = sitemix:::.sm_psd_tolerance,
    rank = sitemix:::.sm_matrix_rank,
    alignment = sitemix:::.sm_validate_output_vcov_alignment,
    equality = sitemix:::.sm_vcov_value_equal
  )
  calls <- new.env(parent = emptyenv())
  for (name in names(originals)) {
    calls[[name]] <- 0L
  }
  bump <- function(name) {
    calls[[name]] <- calls[[name]] + 1L
  }
  testthat::local_mocked_bindings(
    .sm_validate_sm_vcov = function(x) {
      bump("internal")
      originals$internal(x)
    },
    validate.sm_vcov = function(x) {
      bump("alias")
      originals$alias(x)
    },
    as.matrix.sm_vcov = function(x, ...) {
      bump("as_matrix")
      originals$as_matrix(x, ...)
    },
    .sm_validate_vcov_psd = function(x, ...) {
      bump("psd")
      originals$psd(x, ...)
    },
    .sm_psd_tolerance = function(x, ...) {
      bump("psd_tolerance")
      originals$psd_tolerance(x, ...)
    },
    .sm_matrix_rank = function(x, ...) {
      bump("rank")
      originals$rank(x, ...)
    },
    .sm_validate_output_vcov_alignment = function(rows, family = NULL) {
      bump("alignment")
      originals$alignment(rows, family = family)
    },
    .sm_vcov_value_equal = function(x, y) {
      bump("equality")
      originals$equality(x, y)
    },
    .package = "sitemix"
  )

  candidate_b <- make_b()
  expect_identical(candidate_b, reference$B)
  expect_identical(calls$internal, 1L)
  expect_identical(calls$alias, 2L)
  expect_identical(calls$as_matrix, 0L)
  expect_identical(calls$psd, 4L)
  expect_identical(calls$psd_tolerance, 4L)
  expect_identical(calls$rank, 4L)
  expect_identical(calls$alignment, 2L)
  expect_identical(calls$equality, 3L)

  expect_identical(make_c(), reference$C)
  expect_identical(make_d1(), reference$D1)
  expect_true(validate.sitemix_estimates(candidate_b))
})

test_that("covariance reuse falls back before malformed repeated V conditions", {
  make_v <- function() {
    indicators <- c("a", "b")
    sm_vcov(
      matrix = matrix(
        c(0.001875, 0.0002, 0.0002, 0.002304),
        2L,
        2L,
        dimnames = list(indicators, indicators)
      ),
      site_id = "S001",
      year = 2024L,
      indicator_order = indicators,
      family = "multivariate",
      vcov_method = "sur",
      estimate_scale = "arcsine",
      vcov_scale = "raw",
      scalar_correction_rule = c("none", "none"),
      n_jt = 100L,
      n_eff = 100,
      diag_contract = "row_se_raw_squared"
    )
  }
  rows_from <- function(first, second) {
    values <- list(first, second)
    indicators <- c("a", "b")
    theta <- c(0.25, 0.36)
    rows <- lapply(seq_along(indicators), function(i) {
      sitemix:::.sm_one_row(
        "S001", 2024L, indicators[[i]], theta[[i]],
        sqrt(theta[[i]] * (1 - theta[[i]]) / 100), 100L,
        estimate_scale = first$estimate_scale,
        var_method_raw = "binomial",
        V = values[[i]],
        K = 2L
      )
    })
    tibble::as_tibble(do.call(vctrs::vec_rbind, rows))
  }
  validator <- sitemix:::validate.sm_vcov
  validation_calls <- 0L
  testthat::local_mocked_bindings(
    validate.sm_vcov = function(x) {
      validation_calls <<- validation_calls + 1L
      validator(x)
    },
    .package = "sitemix"
  )

  first <- make_v()
  copy <- make_v()
  expect_identical(first, copy)
  expect_true(sitemix:::.sm_validate_output_vcov_alignment(
    rows_from(first, copy),
    family = "multivariate"
  ))
  expect_identical(validation_calls, 1L)

  near_first <- sm_vcov(
    matrix = diag(c(a = 1, b = 0)),
    site_id = "S001",
    year = 2024L,
    indicator_order = c("a", "b"),
    family = "multivariate",
    vcov_method = "sur",
    estimate_scale = "none",
    vcov_scale = "raw",
    scalar_correction_rule = c("none", "none"),
    n_jt = 100L,
    n_eff = 100,
    diag_contract = "not_checked"
  )
  near_invalid <- near_first
  near_invalid$matrix[[2L, 2L]] <- -1e-13
  expect_true(sitemix:::.sm_vcov_value_equal(near_first, near_invalid))
  expect_false(identical(near_first, near_invalid))
  validation_calls <- 0L
  expect_error(
    sitemix:::.sm_validate_output_vcov_alignment(
      rows_from(near_first, near_invalid),
      family = "multivariate"
    ),
    "positive semi-definite",
    class = "sitemix_error_vcov_invariant"
  )
  expect_identical(validation_calls, 2L)

  nonnumeric <- first
  storage.mode(nonnumeric$matrix) <- "character"
  validation_calls <- 0L
  expect_error(
    sitemix:::.sm_validate_output_vcov_alignment(
      rows_from(first, nonnumeric),
      family = "multivariate"
    ),
    "must be a numeric matrix",
    class = "sitemix_error_vcov_invariant"
  )
  expect_identical(validation_calls, 2L)
})

test_that("optimized validation preserves frozen malformed V condition leaves", {
  base <- sm_estimate_from_counts(
    data.frame(
      site_id = "A", year = 2024L, n_jt = 10L,
      c_jt_a = 4L, c_jt_b = 6L, c_jt_a_b = 3L
    ),
    family = "multivariate",
    indicators = c("a", "b"),
    vjt = TRUE,
    min_n = 1L
  )
  mutate_repeat <- function(fun) {
    out <- base
    out$V[[2L]] <- fun(out$V[[2L]])
    out
  }
  cases <- list(
    wrong_class = list(
      object = mutate_repeat(function(v) list()),
      class = "sitemix_error_estimate_vcov_invariant",
      message = "entries must be `sm_vcov` objects",
      has_identity = TRUE
    ),
    nonfinite = list(
      object = mutate_repeat(function(v) {
        v$matrix[[1L, 1L]] <- Inf
        v
      }),
      class = "sitemix_error_vcov_invariant",
      message = "must contain finite values",
      has_identity = FALSE
    ),
    non_psd = list(
      object = mutate_repeat(function(v) {
        v$matrix[[1L, 2L]] <- 0.1
        v$matrix[[2L, 1L]] <- 0.1
        v
      }),
      class = "sitemix_error_vcov_invariant",
      message = "positive semi-definite",
      has_identity = FALSE
    ),
    dimension = list(
      object = mutate_repeat(function(v) {
        v$matrix <- v$matrix[1L, 1L, drop = FALSE]
        v
      }),
      class = "sitemix_error_vcov_dimnames",
      message = "character vector of length K",
      has_identity = FALSE
    ),
    metadata = list(
      object = mutate_repeat(function(v) {
        v$n_jt <- -1L
        v
      }),
      class = "sitemix_error_vcov_invariant",
      message = "`n_jt` has an invalid value",
      has_identity = FALSE
    ),
    alignment = list(
      object = mutate_repeat(function(v) {
        v$site_id <- "OTHER"
        v
      }),
      class = "sitemix_error_estimate_vcov_invariant",
      message = "`V$site_id` must match the output row",
      has_identity = TRUE
    ),
    valid_different = list(
      object = mutate_repeat(function(v) {
        v$n_eff <- 99
        v
      }),
      class = "sitemix_error_estimate_vcov_invariant",
      message = "must be value-equal",
      has_identity = TRUE
    ),
    K_dimension = list(
      object = {
        out <- base
        out$K <- c(2L, 3L)
        out
      },
      class = "sitemix_error_estimate_vcov_invariant",
      message = "`K` must match the dimension",
      has_identity = FALSE
    )
  )

  for (name in names(cases)) {
    case <- cases[[name]]
    error <- tryCatch(
      sitemix:::validate.sitemix_estimates(case$object),
      error = identity
    )
    expect_s3_class(error, case$class)
    expect_match(conditionMessage(error), case$message, fixed = TRUE, info = name)
    expect_identical(!is.null(error$row_identity), case$has_identity, info = name)
  }

  reordered <- sitemix:::.sm_reorder_vcov_coordinates(base$V[[1L]], c("b", "a"))
  repeated_order <- base
  repeated_order$V[[2L]] <- reordered
  expect_error(
    sitemix:::validate.sitemix_estimates(repeated_order),
    "must be value-equal",
    class = "sitemix_error_estimate_vcov_invariant"
  )

  group_order <- base
  group_order$V <- rep(list(reordered), 2L)
  expect_error(
    sitemix:::validate.sitemix_estimates(group_order),
    "indicator order must match row order",
    class = "sitemix_error_estimate_vcov_invariant"
  )
})
