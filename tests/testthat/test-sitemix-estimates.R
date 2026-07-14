make_sitemix_rows <- function() {
  p <- c(0.25, 0.64)
  n <- c(100L, 100L)
  tibble::tibble(
    site_id = c("S001", "S001"),
    year = c(2024L, 2024L),
    indicator = c("a", "b"),
    theta_raw = p,
    theta_hat = asin(sqrt(p)),
    se_raw = sqrt(p * (1 - p) / n),
    se = 1 / (2 * sqrt(n)),
    n = n,
    n_eff = as.numeric(n),
    estimate_scale = c("arcsine", "arcsine"),
    transform = c("arcsine", "arcsine"),
    var_method = c("arcsine_vst", "arcsine_vst"),
    flag_small_n = c(FALSE, FALSE),
    flag_zero_cell = c(FALSE, FALSE),
    input_mode = c("student_level", "student_level"),
    flag_suppressed = c(FALSE, FALSE),
    framing = c(NA_character_, NA_character_),
    flag_below_accountability = c(FALSE, FALSE)
  )
}

make_test_vcov <- function() {
  mat <- matrix(
    c(0.001875, 0.0001, 0.0001, 0.002304),
    2,
    2,
    dimnames = list(c("a", "b"), c("a", "b"))
  )
  sm_vcov(
    matrix = mat,
    site_id = "S001",
    year = 2024L,
    indicator_order = c("a", "b"),
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

test_that("sitemix_estimates constructor validates 18-column schema and attributes", {
  x <- sitemix:::.sm_sitemix_estimates(
    make_sitemix_rows(),
    description = "synthetic rows",
    family = "multivariate",
    sitemix_role = "summary_uncertainty"
  )

  expect_s3_class(x, "sitemix_estimates")
  expect_s3_class(x, "tbl_df")
  expect_true(validate.sitemix_estimates(x))
  expect_identical(names(x)[1:18], sitemix:::.sm_sitemix_columns)
  expect_equal(attr(x, "description"), "synthetic rows")
  expect_equal(attr(x, "family"), "multivariate")
  expect_equal(attr(x, "sitemix_role"), "summary_uncertainty")
  expect_match(format(x), "<sitemix_estimates[2 x 18]>", fixed = TRUE)
})

test_that("sitemix_estimates supports optional V and K with repeated value equality", {
  rows <- make_sitemix_rows()
  V1 <- make_test_vcov()
  V2 <- make_test_vcov()
  expect_true(sitemix:::.sm_vcov_value_equal(V1, V2))
  rows$V <- list(V1, V2)
  rows$K <- c(2L, 2L)

  x <- sitemix:::.sm_sitemix_estimates(rows, family = "multivariate")

  expect_true(validate.sitemix_estimates(x))
  expect_s3_class(x$V[[1]], "sm_vcov")
  expect_equal(x$K, c(2L, 2L))
})

test_that("sitemix_estimates rejects missing default columns", {
  rows <- make_sitemix_rows()
  rows$se <- NULL

  expect_error(
    sitemix:::.sm_sitemix_estimates(rows),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("sitemix_estimates rejects unsupported lexicons", {
  rows <- make_sitemix_rows()
  rows$estimate_scale[[1]] <- "identity"
  rows$transform[[1]] <- "identity"

  expect_error(
    sitemix:::.sm_sitemix_estimates(rows),
    class = "sitemix_error_estimate_var_method"
  )

  rows <- make_sitemix_rows()
  rows$var_method[[1]] <- "sur"
  expect_error(
    sitemix:::.sm_sitemix_estimates(rows),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("sitemix_estimates rejects transform alias drift", {
  rows <- make_sitemix_rows()
  rows$transform[[1]] <- "none"

  expect_error(
    sitemix:::.sm_sitemix_estimates(rows),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("sitemix_estimates rejects impossible theta_hat and se values", {
  rows <- make_sitemix_rows()
  rows$theta_hat[[1]] <- rows$theta_hat[[1]] + 0.1

  expect_error(
    sitemix:::.sm_sitemix_estimates(rows),
    class = "sitemix_error_estimate_var_method"
  )

  rows <- make_sitemix_rows()
  rows$se[[1]] <- rows$se[[1]] + 0.1
  expect_error(
    sitemix:::.sm_sitemix_estimates(rows),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("sitemix_estimates allows only documented suppression NA exceptions with provenance", {
  suppressed <- sitemix:::.sm_suppressed_drop_row(
    site_id = "S009",
    year = 2025L,
    indicator = "absent",
    n = 8L,
    estimate_scale = "arcsine",
    min_n = 10L,
    accountability_n = 30L
  )
  suppressed <- sitemix:::.sm_add_aggregate_suppression_provenance(suppressed)
  x <- sitemix:::.sm_sitemix_estimates(suppressed, family = "binomial")
  expect_true(validate.sitemix_estimates(x))
  expect_true(all(is.na(unlist(x[c("theta_raw", "theta_hat", "se_raw", "se")]))))
  expect_true(is.na(x$flag_zero_cell))

  bad <- suppressed
  bad$flag_suppressed <- FALSE
  expect_error(
    sitemix:::.sm_sitemix_estimates(bad, family = "binomial"),
    class = "sitemix_error_estimate_var_method"
  )

  partial <- suppressed
  partial$theta_raw <- 0.5
  expect_error(
    sitemix:::.sm_sitemix_estimates(partial, family = "binomial"),
    class = "sitemix_error_estimate_var_method"
  )

  ordinary_na <- make_sitemix_rows()[1, ]
  ordinary_na$theta_raw <- NA_real_
  expect_error(
    sitemix:::.sm_sitemix_estimates(ordinary_na, family = "binomial"),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("sitemix_estimates validates raw, logit, and anscombe reproducibility", {
  raw <- make_sitemix_rows()[1, ]
  raw$theta_hat <- raw$theta_raw
  raw$se <- raw$se_raw
  raw$estimate_scale <- "none"
  raw$transform <- "none"
  raw$var_method <- "binomial"
  expect_true(validate.sitemix_estimates(sitemix:::.sm_sitemix_estimates(raw, family = "binomial")))

  logit <- make_sitemix_rows()[1, ]
  logit$theta_hat <- log(logit$theta_raw / (1 - logit$theta_raw))
  logit$se <- 1 / sqrt(logit$n * logit$theta_raw * (1 - logit$theta_raw))
  logit$estimate_scale <- "logit"
  logit$transform <- "logit"
  logit$var_method <- "logit_delta"
  expect_true(validate.sitemix_estimates(sitemix:::.sm_sitemix_estimates(logit, family = "binomial")))

  anscombe <- make_sitemix_rows()[1, ]
  C <- anscombe$theta_raw * anscombe$n
  anscombe$n_eff <- anscombe$n + 0.5
  anscombe$theta_hat <- asin(sqrt((C + 3 / 8) / (anscombe$n + 3 / 4)))
  anscombe$se <- 1 / (2 * sqrt(anscombe$n_eff))
  anscombe$estimate_scale <- "arcsine_anscombe"
  anscombe$transform <- "arcsine_anscombe"
  anscombe$var_method <- "arcsine_anscombe"
  expect_true(validate.sitemix_estimates(sitemix:::.sm_sitemix_estimates(anscombe, family = "binomial")))
})

test_that("sitemix_estimates rejects duplicate row identity", {
  rows <- make_sitemix_rows()
  rows$indicator[[2]] <- rows$indicator[[1]]

  expect_error(
    sitemix:::.sm_sitemix_estimates(rows),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("sitemix_estimates rejects repeated V mismatch and row-order mismatch", {
  rows <- make_sitemix_rows()
  rows$V <- list(make_test_vcov(), make_test_vcov())
  rows$V[[2]]$matrix[1, 1] <- rows$V[[2]]$matrix[1, 1] + 0.001
  rows$K <- c(2L, 2L)

  expect_error(
    sitemix:::.sm_sitemix_estimates(rows, family = "multivariate"),
    class = "sitemix_error_estimate_vcov_invariant"
  )

  rows <- make_sitemix_rows()
  V <- make_test_vcov()
  rows$indicator <- rev(rows$indicator)
  rows$theta_raw <- rev(rows$theta_raw)
  rows$theta_hat <- rev(rows$theta_hat)
  rows$se_raw <- rev(rows$se_raw)
  rows$se <- rev(rows$se)
  rows$V <- list(V, V)
  rows$K <- c(2L, 2L)

  expect_error(
    sitemix:::.sm_sitemix_estimates(rows, family = "multivariate"),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})

test_that("sitemix_estimates rejects bad K and bad attributes", {
  rows <- make_sitemix_rows()
  V <- make_test_vcov()
  rows$V <- list(V, V)
  rows$K <- c(3L, 3L)

  expect_error(
    sitemix:::.sm_sitemix_estimates(rows, family = "multivariate"),
    class = "sitemix_error_estimate_vcov_invariant"
  )

  expect_error(
    sitemix:::.sm_sitemix_estimates(make_sitemix_rows(), family = "mvbernoulli"),
    class = "sitemix_error_estimate_var_method"
  )
  expect_error(
    sitemix:::.sm_sitemix_estimates(make_sitemix_rows(), sitemix_role = "other"),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("print.sitemix_estimates includes compact metadata header", {
  x <- sitemix:::.sm_sitemix_estimates(
    make_sitemix_rows()[1, ],
    description = "one row",
    family = "binomial",
    sitemix_role = "descriptive"
  )

  printed <- utils::capture.output(print(x))
  expect_match(printed[[1]], "sitemix_estimates: 1 rows x 18 columns | family=binomial | role=descriptive | one row", fixed = TRUE)
  expect_match(printed[[2]], "groups=1 sites=1 years=1 indicators=1 V=FALSE K=FALSE", fixed = TRUE)
})

test_that("public validation locks output columns while allowing audit payload", {
  student <- data.frame(
    site_id = rep("S1", 6L),
    year = rep(2025L, 6L),
    a = c(0L, 1L, 0L, 1L, 0L, 1L),
    b = c(1L, 1L, 0L, 0L, 1L, 0L),
    stringsAsFactors = FALSE
  )
  output <- sm_estimate(
    student,
    family = "multivariate",
    indicators = c("a", "b"),
    min_n = 1L
  )
  expect_true(validate.sitemix_estimates(output))
  expect_false(any(c("V", "K") %in% names(output)))

  duplicate <- output
  names(duplicate)[[18L]] <- names(duplicate)[[17L]]
  duplicate_error <- rlang::catch_cnd(validate.sitemix_estimates(duplicate))
  expect_s3_class(duplicate_error, "sitemix_error_estimate_var_method")
  expect_match(conditionMessage(duplicate_error), "must use unique column names", fixed = TRUE)
  expect_identical(duplicate_error$expected, "unique column names")
  expect_identical(duplicate_error$actual, "framing")

  out_of_order <- output
  names(out_of_order)[1:2] <- rev(names(out_of_order)[1:2])
  order_error <- rlang::catch_cnd(validate.sitemix_estimates(out_of_order))
  expect_s3_class(order_error, "sitemix_error_estimate_var_method")
  expect_match(conditionMessage(order_error), "columns are out of order", fixed = TRUE)
  expect_identical(order_error$expected[1:2], c("site_id", "year"))
  expect_identical(order_error$actual[1:2], c("year", "site_id"))

  k_without_v <- output
  k_without_v$K <- rep(2L, nrow(k_without_v))
  k_error <- rlang::catch_cnd(validate.sitemix_estimates(k_without_v))
  expect_s3_class(k_error, "sitemix_error_estimate_vcov_invariant")
  expect_match(conditionMessage(k_error), "only be emitted alongside", fixed = TRUE)
  expect_identical(k_error$expected, "both `V` and `K`")
  expect_identical(k_error$actual, "`K` without `V`")

  audited <- output
  audited$review_note <- rep("checked", nrow(audited))
  expect_true(validate.sitemix_estimates(audited))
  expect_identical(audited$review_note, rep("checked", nrow(audited)))
})
