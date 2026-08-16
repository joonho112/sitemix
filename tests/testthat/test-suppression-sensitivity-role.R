suppression_role_d0 <- function(hidden = FALSE) {
  data.frame(
    site_id = c("S1", "S2"),
    year = c(2025L, 2025L),
    indicator = c("absent", "absent"),
    c_jt = c(NA_integer_, 4L),
    n_jt = c(if (hidden) NA_integer_ else 8L, 20L),
    suppression_flag = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )
}

suppression_role_d1 <- function() {
  data.frame(
    site_id = rep("S1", 2L),
    year = rep(2025L, 2L),
    indicator = c("absent", "suspend"),
    c_jt = c(NA_integer_, 2L),
    n_jt = c(8L, 8L),
    stringsAsFactors = FALSE
  )
}

test_that("drop retains canonical missing rows with explicit status", {
  out <- sm_estimate_from_aggregates(
    suppression_role_d0(),
    family = "binomial",
    indicator = "absent",
    suppression = "drop"
  )
  row <- out[out$flag_suppressed, ]

  expect_equal(row$estimate_status, "suppressed_missing")
  expect_equal(row$var_method, "suppressed_drop")
  expect_true(all(is.na(unlist(row[c("theta_raw", "theta_hat", "se_raw", "se")]))))
  expect_true(all(is.na(unlist(row[c(
    "sensitivity_probability", "sensitivity_var_raw", "sensitivity_var",
    "sensitivity_n", "sensitivity_method"
  )]))))
  expect_false(row$sensitivity_acknowledged)
  expect_equal(attr(out, "suppression")$sensitivity_role, "none")
  expect_false(attr(out, "suppression")$sensitivity_acknowledged)
})

test_that("object acknowledgement records requested versus applied state", {
  drop_requested <- sm_estimate_from_aggregates(
    suppression_role_d0(),
    family = "binomial",
    indicator = "absent",
    suppression = "drop",
    suppression_sensitivity_acknowledge = TRUE
  )
  expect_true(attr(drop_requested, "suppression")$sensitivity_acknowledgement_requested)
  expect_false(attr(drop_requested, "suppression")$sensitivity_acknowledged)
  expect_equal(attr(drop_requested, "suppression")$sensitivity_role, "none")

  fully_observed <- suppression_role_d0()
  fully_observed$c_jt[[1]] <- 1L
  fully_observed$suppression_flag[[1]] <- FALSE
  none_applied <- sm_estimate_from_aggregates(
    fully_observed,
    family = "binomial",
    indicator = "absent",
    suppression = "upper_bound",
    suppression_sensitivity_acknowledge = TRUE
  )
  expect_true(attr(none_applied, "suppression")$sensitivity_acknowledgement_requested)
  expect_false(attr(none_applied, "suppression")$sensitivity_acknowledged)
  expect_equal(attr(none_applied, "suppression")$sensitivity_role, "none")

  inconsistent <- drop_requested
  attr(inconsistent, "suppression")$sensitivity_acknowledged <- TRUE
  expect_error(
    validate.sitemix_estimates(inconsistent),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("upper_bound requires exact acknowledgement and probability 0.5", {
  expect_error(
    sm_estimate_from_aggregates(
      suppression_role_d0(),
      family = "binomial",
      indicator = "absent",
      suppression = "upper_bound"
    ),
    class = "sitemix_error_suppression_sensitivity_acknowledgement"
  )
  expect_error(
    sm_estimate_from_aggregates(
      suppression_role_d0(),
      family = "binomial",
      indicator = "absent",
      suppression = "upper_bound",
      suppression_sensitivity_acknowledge = TRUE,
      suppressed_theta_hat = 0.4
    ),
    class = "sitemix_error_invalid_suppressed_theta_hat"
  )
})

test_that("observed denominator upper_bound is separated variance sensitivity", {
  drop <- sm_estimate_from_aggregates(
    suppression_role_d0(),
    family = "binomial",
    indicator = "absent",
    suppression = "drop"
  )
  out <- sm_estimate_from_aggregates(
    suppression_role_d0(),
    family = "binomial",
    indicator = "absent",
    suppression = "upper_bound",
    suppression_sensitivity_acknowledge = TRUE
  )
  row <- out[out$flag_suppressed, ]

  expect_equal(row$estimate_status, "suppression_sensitivity")
  expect_equal(row$var_method, "suppression_sensitivity")
  expect_true(all(is.na(unlist(row[c("theta_raw", "theta_hat", "se_raw", "se")]))))
  expect_equal(row$sensitivity_probability, 0.5)
  expect_equal(row$sensitivity_n, 8L)
  expect_equal(row$sensitivity_var_raw, 0.25 / 8)
  expect_equal(row$sensitivity_var, 0.25 / 8)
  expect_equal(row$sensitivity_method, "worst_case_variance_observed_n")
  expect_true(row$sensitivity_acknowledged)
  expect_false("V" %in% names(out))
  expect_equal(attr(out, "suppression")$sensitivity_role, "nonidentified_variance_sensitivity")

  diagnosed <- sm_diagnose(out, level = "row", verbose = FALSE)
  diagnosed_row <- diagnosed[diagnosed$flag_suppressed, ]
  expect_equal(diagnosed_row$diag_errors[[1]], character())
  expect_true("sitemix_warning_suppression_sensitivity" %in% diagnosed_row$diag_warnings[[1]])
  expect_match(diagnosed_row$diag_notes, "nonidentified_variance_sensitivity", fixed = TRUE)
  summary_diagnosed <- sm_diagnose(out, verbose = FALSE)
  expect_equal(summary_diagnosed$n_flag_zero_cell, 0L)
  expect_equal(summary_diagnosed$n_flag_both, 0L)

  identified_drop <- drop[drop$estimate_status == "identified", c("theta_raw", "theta_hat", "se_raw", "se")]
  identified_upper <- out[out$estimate_status == "identified", c("theta_raw", "theta_hat", "se_raw", "se")]
  for (column in names(identified_upper)) {
    expect_equal(identified_upper[[column]], identified_drop[[column]])
  }

  expect_error(
    sm_estimate_from_aggregates(
      suppression_role_d0(),
      family = "binomial",
      indicator = "absent",
      suppression = "upper_bound",
      suppression_sensitivity_acknowledge = TRUE,
      vjt = TRUE
    ),
    class = "sitemix_error_estimate_vcov_invariant"
  )
})

test_that("hidden denominator makes no numeric sensitivity variance claim", {
  out <- sm_estimate_from_aggregates(
    suppression_role_d0(hidden = TRUE),
    family = "binomial",
    indicator = "absent",
    suppression = "upper_bound",
    suppression_sensitivity_acknowledge = TRUE,
    suppressed_n_strategy = "worst_case_bound",
    suppressed_n_bound = 5L
  )
  row <- out[out$flag_suppressed, ]

  expect_equal(row$n, 5L)
  expect_equal(row$sensitivity_method, "unquantified_hidden_denominator")
  expect_true(is.na(row$sensitivity_n))
  expect_true(is.na(row$sensitivity_var_raw))
  expect_true(is.na(row$sensitivity_var))
  expect_true(all(is.na(unlist(row[c("theta_raw", "theta_hat", "se_raw", "se")]))))
})

test_that("D1 sensitivity preserves state provenance but cannot build V or Frechet", {
  out <- suppressWarnings(sm_estimate_from_aggregates(
    suppression_role_d1(),
    family = "multivariate",
    sampling_relation = "same_units",
    suppression = "upper_bound",
    suppression_sensitivity_acknowledge = TRUE
  ))

  expect_equal(attr(out, "sampling_relation"), "same_units")
  expect_equal(attr(out, "denominator_pattern"), "common")
  expect_equal(attr(out, "d1_regime"), "D1a")
  expect_equal(out$estimate_status, c("suppression_sensitivity", "identified"))
  expect_false("V" %in% names(out))
  expect_error(
    suppressWarnings(sm_estimate_from_aggregates(
      suppression_role_d1(),
      family = "multivariate",
      sampling_relation = "same_units",
      suppression = "upper_bound",
      suppression_sensitivity_acknowledge = TRUE,
      vjt = TRUE
    )),
    class = "sitemix_error_estimate_vcov_invariant"
  )
  expect_error(
    sm_frechet_envelope(out, population_regime = "d1a"),
    class = "sitemix_error_suppression_sensitivity_excluded"
  )

  drop <- suppressWarnings(sm_estimate_from_aggregates(
    suppression_role_d1(),
    family = "multivariate",
    sampling_relation = "same_units",
    suppression = "drop"
  ))
  expect_error(
    sm_frechet_envelope(drop, population_regime = "d1a"),
    class = "sitemix_error_invalid_indicators"
  )
})

test_that("ordinary V injection is rejected on sensitivity rows", {
  out <- sm_estimate_from_aggregates(
    suppression_role_d0()[1, ],
    family = "binomial",
    indicator = "absent",
    suppression = "upper_bound",
    suppression_sensitivity_acknowledge = TRUE
  )
  out$V <- list(sm_vcov(
    matrix = matrix(1, 1, 1, dimnames = list("absent", "absent")),
    site_id = "S1",
    year = 2025L,
    indicator_order = "absent",
    family = "binomial",
    estimate_scale = "arcsine",
    vcov_scale = "arcsine_delta"
  ))
  expect_error(validate.sitemix_estimates(out), class = "sitemix_error_estimate_vcov_invariant")
})

test_that("sensitivity schema fails closed under adversarial provenance mutation", {
  out <- sm_estimate_from_aggregates(
    suppression_role_d0(),
    family = "binomial",
    indicator = "absent",
    suppression = "upper_bound",
    suppression_sensitivity_acknowledge = TRUE
  )
  i <- which(out$estimate_status == "suppression_sensitivity")

  missing_method <- out
  missing_method$sensitivity_method[i] <- NA_character_
  expect_error(
    validate.sitemix_estimates(missing_method),
    class = "sitemix_error_estimate_var_method"
  )

  mismatched_n <- out
  mismatched_n$sensitivity_n[i] <- 7L
  mismatched_n$sensitivity_var_raw[i] <- 0.25 / 7
  mismatched_n$sensitivity_var[i] <- 0.25 / 7
  expect_error(
    validate.sitemix_estimates(mismatched_n),
    class = "sitemix_error_estimate_var_method"
  )

  approximate_probability <- out
  approximate_probability$sensitivity_probability[i] <- 0.5 + 5e-13
  expect_error(
    validate.sitemix_estimates(approximate_probability),
    class = "sitemix_error_estimate_var_method"
  )
})

test_that("ordinary variance smoothing rejects sensitivity rows", {
  out <- sm_estimate_from_aggregates(
    suppression_role_d0(),
    family = "binomial",
    indicator = "absent",
    suppression = "upper_bound",
    suppression_sensitivity_acknowledge = TRUE
  )
  expect_error(
    sm_smooth_variance(out, min_rows = 1L),
    class = "sitemix_error_suppression_sensitivity_excluded"
  )
})

test_that("aggregate FPC fails closed when any suppression audit row is retained", {
  expect_error(
    sm_estimate_from_aggregates(
      suppression_role_d0(),
      family = "binomial",
      indicator = "absent",
      suppression = "drop",
      fpc = c(20L, 40L)
    ),
    class = "sitemix_error_invalid_fpc"
  )
})

test_that("suppression report labels sensitivity role and denominator limits", {
  observed <- sm_suppression_report(suppression_role_d0(), by = NULL)
  hidden <- sm_suppression_report(suppression_role_d0(hidden = TRUE), by = NULL)

  expect_equal(observed$upper_bound_role, "nonidentified_variance_sensitivity")
  expect_true(observed$upper_bound_numeric_variance_available)
  expect_true(observed$upper_bound_requires_acknowledgement)
  expect_equal(observed$recommended_action, "drop_or_acknowledge_variance_sensitivity")

  expect_equal(hidden$upper_bound_role, "nonidentified_variance_sensitivity")
  expect_false(hidden$upper_bound_numeric_variance_available)
  expect_true(hidden$upper_bound_requires_acknowledgement)
  expect_equal(hidden$recommended_action, "drop_or_acknowledge_unquantified_sensitivity")
})
