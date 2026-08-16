quiet_step313_d1 <- function(expr) {
  withCallingHandlers(
    expr,
    sitemix_warning_working_independence_default = function(w) invokeRestart("muffleWarning")
  )
}

step313_d1 <- function(
  K = 2L,
  sampling_relation = "same_units",
  vjt = TRUE,
  ...
) {
  indicators <- letters[seq_len(K)]
  data <- data.frame(
    site_id = rep("S1", K),
    year = rep(2025L, K),
    indicator = indicators,
    c_jt = c(20L, 70L, 40L)[seq_len(K)],
    n_jt = rep(100L, K),
    stringsAsFactors = FALSE
  )
  quiet_step313_d1(
    sitemix::sm_estimate_from_aggregates(
      data,
      family = "multivariate",
      sampling_relation = sampling_relation,
      vjt = vjt,
      vst = "none",
      boundary_method = "none",
      min_n = 1L,
      ...
    )
  )
}

test_that("formal K2 raw pairwise intervals match the independent /n oracle", {
  estimates <- step313_d1(K = 2L)
  envelope <- sitemix::sm_frechet_envelope(
    estimates,
    population_regime = "d1a",
    return_correlations = TRUE
  )
  interval <- envelope$raw_pairwise_intervals

  expect_equal(nrow(interval), 1L)
  expect_equal(interval$p_1, 0.2)
  expect_equal(interval$p_2, 0.7)
  expect_equal(interval$n_common, 100)
  expect_equal(interval$joint_probability_lower, 0)
  expect_equal(interval$joint_probability_upper, 0.2)
  expect_equal(interval$pairwise_covariance_lower, (0 - 0.2 * 0.7) / 100)
  expect_equal(interval$pairwise_covariance_upper, (0.2 - 0.2 * 0.7) / 100)
  expect_equal(interval$interval_scale, "raw_probability")
  expect_equal(interval$covariance_construction, "formal_iid_pairwise_covariance_over_n")
  expect_equal(interval$interval_scope, "formal_raw_pairwise_interval")

  expect_identical(
    envelope$unprojected_negative_dependence_corner,
    envelope$projected_negative_dependence_stress
  )
  expect_identical(
    envelope$unprojected_positive_dependence_corner,
    envelope$projected_positive_dependence_stress
  )
  expect_equal(unique(envelope$projection_diagnostics$projected_order_reversals), 0L)
  expect_equal(sum(envelope$projection_diagnostics$raw_interval_violations), 0L)
  expect_true(sitemix:::.sm_validate_frechet_envelope_object(envelope))
})

test_that("canonical fields and deprecated aliases have exact identity", {
  envelope <- sitemix::sm_frechet_envelope(
    step313_d1(K = 3L),
    population_regime = "d1a",
    return_correlations = TRUE
  )

  expect_identical(envelope$V_lower_raw, envelope$unprojected_negative_dependence_corner)
  expect_identical(envelope$V_upper_raw, envelope$unprojected_positive_dependence_corner)
  expect_identical(envelope$V_lower_psd, envelope$projected_negative_dependence_stress)
  expect_identical(envelope$V_upper_psd, envelope$projected_positive_dependence_stress)
  expect_identical(envelope$R_lower, envelope$pairwise_correlation_lower)
  expect_identical(envelope$R_upper, envelope$pairwise_correlation_upper)
  expect_equal(nrow(envelope$psd_diagnostics), 1L)
  expect_equal(nrow(envelope$projection_diagnostics), 2L)
  expect_true(all(c("L_iters", "U_iters") %in% names(envelope$psd_diagnostics)))
  expect_false(any(grepl("V_lower|V_upper|lower bound|upper bound", format(envelope))))

  alias_tamper <- envelope
  alias_tamper$V_lower_psd[[1L]][1L, 2L] <- 0
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(alias_tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )
})

test_that("formal D1a provenance fails closed outside the approved IID plugin oracle", {
  unknown <- step313_d1(K = 2L, sampling_relation = "unknown", vjt = FALSE)
  expect_error(
    sitemix::sm_frechet_envelope(unknown, population_regime = "d1a"),
    class = "sitemix_error_invalid_population_regime"
  )

  fpc <- step313_d1(K = 2L, fpc = 200)
  expect_error(
    sitemix::sm_frechet_envelope(fpc, population_regime = "d1a"),
    class = "sitemix_error_invalid_population_regime"
  )

  corrected <- step313_d1(K = 2L, bias_correction = "binomial_bc")
  expect_error(
    sitemix::sm_frechet_envelope(corrected, population_regime = "d1a"),
    class = "sitemix_error_invalid_population_regime"
  )
})

test_that("D1b unknown provenance remains explicit heuristic stress semantics", {
  estimates <- step313_d1(K = 2L, sampling_relation = "unknown", vjt = FALSE)
  expect_warning(
    envelope <- sitemix::sm_frechet_envelope(
      estimates,
      population_regime = "d1b",
      subgroup_conditional_action = "warn"
    ),
    class = "sitemix_warning_frechet_d1b_heuristic"
  )
  interval <- envelope$raw_pairwise_intervals

  expect_equal(envelope$frechet_scope, "heuristic_stress_test")
  expect_true(is.na(interval$n_common))
  expect_equal(interval$covariance_construction, "heuristic_se_scaled_pairwise_correlation")
  expect_equal(interval$interval_scope, "heuristic_pairwise_stress_range")
  expect_true(sitemix:::.sm_validate_frechet_envelope_object(envelope))

  boundary_data <- data.frame(
    site_id = c("S1", "S1"),
    year = c(2025L, 2025L),
    indicator = c("a", "b"),
    c_jt = c(0L, 50L),
    n_jt = c(100L, 100L)
  )
  boundary_estimates <- quiet_step313_d1(
    sitemix::sm_estimate_from_aggregates(
      boundary_data,
      family = "multivariate",
      sampling_relation = "unknown",
      vjt = FALSE,
      min_n = 1L
    )
  )
  boundary <- sitemix::sm_frechet_envelope(
    boundary_estimates,
    population_regime = "d1b",
    subgroup_conditional_action = "allow"
  )
  expect_true(is.na(boundary$raw_pairwise_intervals$pairwise_correlation_lower))
  expect_true(is.na(boundary$raw_pairwise_intervals$pairwise_correlation_upper))
  expect_equal(boundary$raw_pairwise_intervals$pairwise_covariance_lower, 0)
  expect_equal(boundary$raw_pairwise_intervals$pairwise_covariance_upper, 0)
  expect_true(sitemix:::.sm_validate_frechet_envelope_object(boundary))
})

test_that("K3 projection diagnostics expose sign changes without calling scenarios bounds", {
  result <- suppressWarnings(
    sitemix:::.sm_frechet_from_vectors(
      p = c(0.49089526406023648, 0.47698444876819845, 0.54220616049133241),
      s = c(0.012305699816638264, 0.070966493547329659, 0.138531664038827329),
      indicators = c("a", "b", "c"),
      psd_method = "higham",
      psd_tol = 1e-8,
      psd_max_iter = 1000L,
      return_correlations = FALSE,
      nearpd_args = list()
    )
  )

  expect_lt(result$unprojected_negative_dependence_corner["a", "b"], 0)
  expect_gt(result$projected_negative_dependence_stress["a", "b"], 0)
  expect_equal(
    result$projection_diagnostics$sign_changes[
      result$projection_diagnostics$scenario == "negative_dependence_stress"
    ],
    1L
  )
})

test_that("entrywise K3 diagnostics detect tiny real order and interval violations", {
  negative_raw <- diag(1, 3L)
  positive_raw <- diag(1, 3L)
  negative_projected <- negative_raw
  positive_projected <- positive_raw
  negative_raw[1L, 2L] <- negative_raw[2L, 1L] <- -1e-12
  positive_raw[1L, 2L] <- positive_raw[2L, 1L] <- 1e-12
  negative_projected[1L, 2L] <- negative_projected[2L, 1L] <- 2e-12
  positive_projected[1L, 2L] <- positive_projected[2L, 1L] <- 1e-12

  diagnostics <- sitemix:::.sm_frechet_projection_semantics(
    unprojected_negative = negative_raw,
    unprojected_positive = positive_raw,
    projected_negative = negative_projected,
    projected_positive = positive_projected
  )

  expect_equal(diagnostics$negative_sign_changes, 1L)
  expect_equal(diagnostics$projected_order_reversals, 1L)
  expect_equal(diagnostics$projected_negative_raw_interval_violations, 1L)
  expect_equal(diagnostics$projected_raw_interval_violations, 1L)
  expect_equal(diagnostics$projected_order_reversal_max, 1e-12)
  expect_equal(diagnostics$projected_negative_max_raw_interval_violation, 1e-12)
})

test_that("already-PSD K3 corners bypass both projection backends exactly", {
  zero_data <- data.frame(
    site_id = rep("S1", 3L),
    year = rep(2025L, 3L),
    indicator = c("a", "b", "c"),
    c_jt = c(0L, 2L, 0L),
    n_jt = rep(2L, 3L),
    stringsAsFactors = FALSE
  )
  zero_estimates <- quiet_step313_d1(
    sitemix::sm_estimate_from_aggregates(
      zero_data,
      family = "multivariate",
      sampling_relation = "same_units",
      vst = "none",
      boundary_method = "none",
      vjt = FALSE,
      min_n = 1L
    )
  )
  for (method in c("higham", "shrink")) {
    envelope <- sitemix::sm_frechet_envelope(
      zero_estimates,
      population_regime = "d1a",
      psd_method = method
    )
    zero <- matrix(0, 3L, 3L, dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
    expect_identical(envelope$unprojected_negative_dependence_corner[[1L]], zero)
    expect_identical(envelope$projected_negative_dependence_stress[[1L]], zero)
    expect_identical(envelope$unprojected_positive_dependence_corner[[1L]], zero)
    expect_identical(envelope$projected_positive_dependence_stress[[1L]], zero)
    expect_true(all(envelope$projection_diagnostics$iterations == 0L))
    expect_true(all(envelope$projection_diagnostics$raw_was_psd))
    expect_false(any(envelope$projection_diagnostics$projection_attempted))
    expect_true(all(is.na(envelope$projection_diagnostics$converged)))
  }

  nonzero <- matrix(
    c(1, 0.1, 0.2, 0.1, 1, 0.15, 0.2, 0.15, 1),
    nrow = 3L,
    dimnames = list(c("a", "b", "c"), c("a", "b", "c"))
  )
  for (method in c("higham", "shrink")) {
    projected <- sitemix:::.sm_frechet_psd_project(
      nonzero,
      method = method,
      psd_tol = 1e-8,
      psd_max_iter = 100L,
      shrink_alpha = NULL,
      nearpd_args = list()
    )
    expect_identical(projected$mat, nonzero)
    expect_identical(projected$iters, 0L)
    expect_true(projected$was_psd)
  }
})

test_that("object validation rejects pairwise and projected-diagnostic tampering", {
  envelope <- sitemix::sm_frechet_envelope(
    step313_d1(K = 3L),
    population_regime = "d1a"
  )

  pair_tamper <- envelope
  pair_tamper$raw_pairwise_intervals$pairwise_covariance_lower[[1L]] <- 0
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(pair_tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )

  diagnostics_tamper <- envelope
  diagnostics_tamper$projection_diagnostics$projected_order_reversals[[1L]] <- 99L
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(diagnostics_tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )

  projected_tamper <- envelope
  changed <- projected_tamper$projected_negative_dependence_stress
  changed[[1L]][1L, 2L] <- changed[[1L]][2L, 1L] <- 0.09
  projected_tamper$projected_negative_dependence_stress <- changed
  projected_tamper$V_lower_psd <- changed
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(projected_tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )

  scope_tamper <- sitemix::sm_frechet_envelope(
    step313_d1(K = 2L, sampling_relation = "unknown", vjt = FALSE),
    population_regime = "d1b",
    subgroup_conditional_action = "allow"
  )
  scope_tamper$frechet_scope <- "formal"
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(scope_tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )

  ghost_tamper <- envelope
  ghost <- ghost_tamper$raw_pairwise_intervals[1L, , drop = FALSE]
  ghost$site_id <- "GHOST"
  ghost$site_key <- "GHOST::2025"
  ghost_tamper$raw_pairwise_intervals <- vctrs::vec_rbind(
    ghost_tamper$raw_pairwise_intervals,
    ghost
  )
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(ghost_tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )

  heuristic_tamper <- sitemix::sm_frechet_envelope(
    step313_d1(K = 2L, sampling_relation = "unknown", vjt = FALSE),
    population_regime = "d1b",
    subgroup_conditional_action = "allow"
  )
  changed_value <- heuristic_tamper$raw_pairwise_intervals$pairwise_covariance_lower[[1L]] / 2
  heuristic_tamper$raw_pairwise_intervals$pairwise_covariance_lower[[1L]] <- changed_value
  changed <- heuristic_tamper$unprojected_negative_dependence_corner
  changed[[1L]][1L, 2L] <- changed[[1L]][2L, 1L] <- changed_value
  heuristic_tamper$unprojected_negative_dependence_corner <- changed
  heuristic_tamper$projected_negative_dependence_stress <- changed
  heuristic_tamper$V_lower_raw <- changed
  heuristic_tamper$V_lower_psd <- changed
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(heuristic_tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )

  pair_projected_tamper <- sitemix::sm_frechet_envelope(
    step313_d1(K = 2L),
    population_regime = "d1a"
  )
  changed <- pair_projected_tamper$projected_negative_dependence_stress
  changed[[1L]][1L, 2L] <- changed[[1L]][2L, 1L] <- changed[[1L]][1L, 2L] / 2
  pair_projected_tamper$projected_negative_dependence_stress <- changed
  pair_projected_tamper$V_lower_psd <- changed
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(pair_projected_tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )

  denominator_tamper <- envelope
  denominator_tamper$raw_pairwise_intervals$n_common[[2L]] <- 200
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(denominator_tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )

  marginal_tamper <- envelope
  marginal_tamper$raw_pairwise_intervals$p_1[[2L]] <-
    marginal_tamper$raw_pairwise_intervals$p_1[[2L]] + 0.01
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(marginal_tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )

  diagonal_tamper <- envelope
  canonical_matrix_fields <- c(
    "V_independence",
    "unprojected_negative_dependence_corner",
    "unprojected_positive_dependence_corner",
    "projected_negative_dependence_stress",
    "projected_positive_dependence_stress"
  )
  for (field in canonical_matrix_fields) {
    changed <- diagonal_tamper[[field]]
    changed[[1L]][1L, 1L] <- changed[[1L]][1L, 1L] + 1e-5
    diagonal_tamper[[field]] <- changed
  }
  diagonal_tamper$V_lower_raw <- diagonal_tamper$unprojected_negative_dependence_corner
  diagonal_tamper$V_upper_raw <- diagonal_tamper$unprojected_positive_dependence_corner
  diagonal_tamper$V_lower_psd <- diagonal_tamper$projected_negative_dependence_stress
  diagonal_tamper$V_upper_psd <- diagonal_tamper$projected_positive_dependence_stress
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(diagonal_tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )
})

test_that("public Frechet format rejects every stale replay component", {
  envelope <- sitemix::sm_frechet_envelope(
    step313_d1(K = 3L),
    population_regime = "d1a",
    return_correlations = TRUE,
    psd_method = "shrink",
    shrink_alpha = 0.4
  )

  cases <- list(
    site_keys = list(
      message = "Frechet site-key metadata is invalid.",
      mutate = function(x) {
        x$site_keys$site_key <- "wrong"
        x
      }
    ),
    matrix_lists = list(
      message = "Frechet matrix lists are not aligned with site keys.",
      mutate = function(x) {
        names(x$V_independence) <- "wrong"
        x
      }
    ),
    projection_diagnostics = list(
      message = "Projection diagnostics are not aligned with site keys.",
      mutate = function(x) {
        x$projection_diagnostics <- x$projection_diagnostics[2:1, , drop = FALSE]
        x
      }
    ),
    wide_diagnostics = list(
      message = "Deprecated wide PSD diagnostics are not aligned with site keys.",
      mutate = function(x) {
        x$psd_diagnostics$site_key <- "wrong"
        x
      }
    ),
    interval_schema = list(
      message = "Raw pairwise interval schema is incomplete.",
      mutate = function(x) {
        x$raw_pairwise_intervals$interval_scale <- NULL
        x
      }
    ),
    matrix_geometry = list(
      message = "A Frechet matrix failed dimension, name, finiteness, or symmetry validation.",
      mutate = function(x) {
        x$V_independence[[1L]][1L, 2L] <-
          x$V_independence[[1L]][1L, 2L] + 1e-5
        x
      }
    ),
    canonical_diagonal = list(
      message = "A projected or unprojected Frechet matrix changed the canonical diagonal.",
      mutate = function(x) {
        changed <- x$unprojected_negative_dependence_corner
        diag(changed[[1L]]) <- diag(changed[[1L]]) + 1e-5
        x$unprojected_negative_dependence_corner <- changed
        x$V_lower_raw <- changed
        x
      }
    ),
    interval_alignment = list(
      message = "Raw pairwise intervals failed pair alignment, ordering, or scale validation.",
      mutate = function(x) {
        x$raw_pairwise_intervals$interval_scale[[1L]] <- "wrong"
        x
      }
    ),
    corner_endpoint = list(
      message = "Raw pairwise interval endpoints do not match the unprojected corners.",
      mutate = function(x) {
        changed <- x$unprojected_negative_dependence_corner
        changed[[1L]][1L, 2L] <- changed[[1L]][2L, 1L] <-
          changed[[1L]][1L, 2L] + 1e-5
        x$unprojected_negative_dependence_corner <- changed
        x$V_lower_raw <- changed
        x
      }
    ),
    incomplete_correlations = list(
      message = "Pairwise correlation endpoint matrices are incomplete.",
      mutate = function(x) {
        x$pairwise_correlation_lower <- list()
        x$R_lower <- list()
        x
      }
    ),
    correlation_dimnames = list(
      message = "Pairwise correlation endpoints are not indicator-aligned.",
      mutate = function(x) {
        changed <- x$pairwise_correlation_lower
        dimnames(changed[[1L]]) <- list(c("wrong", "b", "c"), c("wrong", "b", "c"))
        x$pairwise_correlation_lower <- changed
        x$R_lower <- changed
        x
      }
    ),
    correlation_values = list(
      message = "Pairwise correlation matrices do not match raw interval rows.",
      mutate = function(x) {
        changed <- x$pairwise_correlation_lower
        changed[[1L]][1L, 2L] <- changed[[1L]][2L, 1L] <-
          changed[[1L]][1L, 2L] + 0.01
        x$pairwise_correlation_lower <- changed
        x$R_lower <- changed
        x
      }
    ),
    projection_config = list(
      message = "Frechet projection configuration is missing or malformed.",
      mutate = function(x) {
        x$projection_config <- 1
        x
      }
    ),
    diagnostic_schema = list(
      message = "Projected matrices or projection diagnostics do not match deterministic replay.",
      mutate = function(x) {
        x$projection_diagnostics$extra <- 1
        x
      }
    )
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    error <- expect_error(
      format(case$mutate(envelope)),
      class = "sitemix_error_frechet_envelope_missing",
      info = case_name
    )
    expect_match(
      conditionMessage(error),
      case$message,
      fixed = TRUE,
      info = case_name
    )
  }
})

test_that("public Frechet methods replay a K1 envelope without pair rows", {
  envelope <- sitemix::sm_frechet_envelope(
    step313_d1(K = 2L, vjt = FALSE),
    indicator = "a",
    population_regime = "d1a",
    psd_method = "shrink"
  )

  rendered <- format(envelope)
  summarized <- summary(envelope)

  expect_match(rendered[[1L]], "sm_frechet_envelope", fixed = TRUE)
  expect_s3_class(summarized, "summary.sm_frechet_envelope")
  expect_identical(unique(summarized$K), 1L)
  expect_identical(nrow(envelope$raw_pairwise_intervals), 0L)
})
