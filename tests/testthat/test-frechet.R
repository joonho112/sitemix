quiet_frechet_d1_warning <- function(expr) {
  withCallingHandlers(
    expr,
    sitemix_warning_working_independence_default = function(w) invokeRestart("muffleWarning")
  )
}

test_that("Frechet core handles K = 1 and K = 2 shortcuts", {
  one <- sitemix:::.sm_frechet_from_vectors(
    p = 0.3,
    s = 0.2,
    indicators = "a",
    psd_method = "higham",
    psd_tol = 1e-8,
    psd_max_iter = 100L,
    return_correlations = TRUE,
    nearpd_args = list()
  )
  expect_equal(one$V_independence, matrix(0.04, 1, 1, dimnames = list("a", "a")))
  expect_equal(one$V_lower_psd, one$V_independence)
  expect_equal(one$V_upper_psd, one$V_independence)
  expect_equal(one$psd_diagnostics$L_iters, 0L)
  expect_true(one$psd_diagnostics$L_was_PSD)

  two <- sitemix:::.sm_frechet_from_vectors(
    p = c(a = 0.1, b = 0.2),
    s = c(a = 0.05, b = 0.05),
    indicators = c("a", "b"),
    psd_method = "higham",
    psd_tol = 1e-8,
    psd_max_iter = 100L,
    return_correlations = TRUE,
    nearpd_args = list()
  )
  expect_equal(two$psd_diagnostics$L_iters, 0L)
  expect_equal(two$psd_diagnostics$U_iters, 0L)
  expect_equal(two$V_lower_psd, two$V_lower_raw)
  expect_equal(two$V_upper_psd, two$V_upper_raw)
  expect_lt(abs(as.numeric(two$V_lower_psd["a", "b"]) - (-0.0004166667)), 1e-8)
  expect_lt(abs(as.numeric(two$V_upper_psd["a", "b"]) - 0.001666667), 1e-8)
})

test_that("Frechet core projects K > 2 lower envelope while preserving diagonals", {
  res <- sitemix:::.sm_frechet_from_vectors(
    p = c(a = 0.5, b = 0.5, c = 0.5),
    s = c(a = 0.1, b = 0.1, c = 0.1),
    indicators = c("a", "b", "c"),
    psd_method = "higham",
    psd_tol = 1e-8,
    psd_max_iter = 100L,
    return_correlations = FALSE,
    nearpd_args = list()
  )

  expect_lt(res$psd_diagnostics$L_min_eig_before, 0)
  expect_gte(res$psd_diagnostics$L_min_eig_after, -1e-8)
  expect_equal(diag(res$V_lower_psd), diag(res$V_independence), tolerance = 1e-12)
  expect_equal(diag(res$V_upper_psd), diag(res$V_independence), tolerance = 1e-12)
  expect_equal(sum(diag(res$V_lower_psd)), sum(diag(res$V_independence)), tolerance = 1e-12)
  expect_equal(sum(diag(res$V_upper_psd)), sum(diag(res$V_independence)), tolerance = 1e-12)
})

test_that("Frechet shrink projection is PSD or errors on an invalid fixed alpha", {
  res <- sitemix:::.sm_frechet_from_vectors(
    p = c(a = 0.5, b = 0.5, c = 0.5),
    s = c(a = 0.1, b = 0.1, c = 0.1),
    indicators = c("a", "b", "c"),
    psd_method = "shrink",
    psd_tol = 1e-8,
    psd_max_iter = 100L,
    shrink_alpha = NULL,
    return_correlations = FALSE,
    nearpd_args = list()
  )
  expect_gte(res$psd_diagnostics$L_min_eig_after, -1e-8)
  expect_equal(unname(diag(res$V_lower_psd)), c(0.01, 0.01, 0.01), tolerance = 1e-12)
  expect_gt(res$psd_diagnostics$L_iters, 0L)

  expect_error(
    sitemix:::.sm_frechet_from_vectors(
      p = c(a = 0.5, b = 0.5, c = 0.5),
      s = c(a = 0.1, b = 0.1, c = 0.1),
      indicators = c("a", "b", "c"),
      psd_method = "shrink",
      psd_tol = 1e-8,
      psd_max_iter = 100L,
      shrink_alpha = 1,
      return_correlations = FALSE,
      nearpd_args = list()
    ),
    class = "sitemix_error_vcov_invariant"
  )
})

test_that("Frechet Higham and shrink projections give comparable Frobenius norms", {
  args <- list(
    p = c(a = 0.5, b = 0.5, c = 0.5),
    s = c(a = 0.1, b = 0.1, c = 0.1),
    indicators = c("a", "b", "c"),
    psd_tol = 1e-8,
    psd_max_iter = 100L,
    return_correlations = FALSE,
    nearpd_args = list()
  )
  higham <- do.call(
    sitemix:::.sm_frechet_from_vectors,
    c(args, list(psd_method = "higham"))
  )
  shrink <- do.call(
    sitemix:::.sm_frechet_from_vectors,
    c(args, list(psd_method = "shrink", shrink_alpha = NULL))
  )

  expect_equal(higham$V_lower_raw, shrink$V_lower_raw)
  expect_equal(higham$V_upper_raw, shrink$V_upper_raw)
  expect_equal(diag(higham$V_lower_psd), diag(higham$V_independence), tolerance = 1e-12)
  expect_equal(diag(shrink$V_lower_psd), diag(shrink$V_independence), tolerance = 1e-12)
  expect_gte(higham$psd_diagnostics$L_min_eig_after, -1e-8)
  expect_gte(shrink$psd_diagnostics$L_min_eig_after, -1e-8)
  expect_lt(higham$psd_diagnostics$frob_L_PSD, higham$psd_diagnostics$frob_L_raw)
  expect_lt(shrink$psd_diagnostics$frob_L_PSD, shrink$psd_diagnostics$frob_L_raw)
  expect_lt(
    abs(higham$psd_diagnostics$frob_L_PSD - shrink$psd_diagnostics$frob_L_PSD),
    1e-6
  )
})

test_that("Frechet core handles boundary marginals", {
  res <- sitemix:::.sm_frechet_from_vectors(
    p = c(zero = 0, mid = 0.5, one = 1),
    s = c(zero = 0.1, mid = 0.2, one = 0.3),
    indicators = c("zero", "mid", "one"),
    psd_method = "higham",
    psd_tol = 1e-8,
    psd_max_iter = 100L,
    return_correlations = TRUE,
    nearpd_args = list()
  )

  expect_true(is.na(res$R_lower["zero", "mid"]))
  expect_true(is.na(res$R_upper["mid", "one"]))
  expect_equal(res$V_lower_raw["zero", "mid"], 0)
  expect_equal(res$V_upper_raw["mid", "one"], 0)
  expect_equal(unname(diag(res$V_lower_psd)), c(0.01, 0.04, 0.09), tolerance = 1e-12)
  expect_equal(res$psd_diagnostics$boundary_marginal_indicators[[1]], c("zero", "one"))
})

test_that("Frechet core pins the AA HI sign-flip regression", {
  p <- c(
    AA = 0.1111, AI = 0.2727, ALL = 0.1009, AS = 0.0538, EL = 0.0951,
    FI = 0.0152, FOS = 0.3478, HI = 0.1112, HOM = 0.2403,
    LTEL = 0.1556, MR = 0.0294, PI = 0.0256, SED = 0.1182,
    SWD = 0.1248, WH = 0.0875
  )
  s <- c(
    AA = 0.01559, AI = 0.07524, ALL = 0.00518, AS = 0.02618, EL = 0.01551,
    FI = 0.02395, FOS = 0.09317, HI = 0.00700, HOM = 0.03746,
    LTEL = 0.03875, MR = 0.01703, PI = 0.03851, SED = 0.00642,
    SWD = 0.01423, WH = 0.01182
  )

  res <- sitemix:::.sm_frechet_from_vectors(
    p = p,
    s = s,
    indicators = names(p),
    site_id = "CVA",
    year = 2025L,
    psd_method = "higham",
    psd_tol = 1e-8,
    psd_max_iter = 100L,
    return_correlations = FALSE,
    nearpd_args = list()
  )

  expect_lt(res$V_lower_raw["AA", "HI"], 0)
  expect_gt(res$V_lower_psd["AA", "HI"], 0)
  expect_lt(abs(as.numeric(res$V_lower_psd["AA", "HI"]) - 2.70e-05), 1.35e-06)
})

test_that("sm_frechet_envelope integrates with D1 estimates without V", {
  d1 <- data.frame(
    site_id = c("S1", "S1", "S2", "S2"),
    year = c(2025L, 2025L, 2025L, 2025L),
    indicator = c("absent", "suspend", "absent", "suspend"),
    c_jt = c(10L, 2L, 15L, 3L),
    n_jt = c(100L, 100L, 80L, 80L),
    stringsAsFactors = FALSE
  )
  estimates <- quiet_frechet_d1_warning(
    sitemix::sm_estimate_from_aggregates(
      d1,
      family = "multivariate",
      sampling_relation = "same_units",
      vjt = FALSE,
      min_n = 1L
    )
  )
  env <- sitemix::sm_frechet_envelope(estimates, population_regime = "d1a")

  expect_s3_class(env, "sm_frechet_envelope")
  expect_length(env$V_independence, 2L)
  group <- estimates[estimates$site_id == "S1", ]
  expect_equal(unname(diag(env$V_independence[["S1::2025"]])), group$se_raw^2, tolerance = 1e-12)
  expect_true(all(env$V_independence[["S1::2025"]][row(env$V_independence[["S1::2025"]]) != col(env$V_independence[["S1::2025"]])] == 0))
  expect_equal(env$frechet_scope, "formal")
})

test_that("sm_frechet_envelope warns when D1 input V has non-diagonal entries", {
  d1 <- data.frame(
    site_id = c("S1", "S1"),
    year = c(2025L, 2025L),
    indicator = c("absent", "suspend"),
    c_jt = c(10L, 2L),
    n_jt = c(100L, 100L),
    stringsAsFactors = FALSE
  )
  estimates <- quiet_frechet_d1_warning(
    sitemix::sm_estimate_from_aggregates(
      d1,
      family = "multivariate",
      sampling_relation = "same_units",
      vjt = TRUE,
      min_n = 1L
    )
  )

  V <- estimates$V[[1]]
  mat <- as.matrix(V)
  mat["absent", "suspend"] <- 1e-4
  mat["suspend", "absent"] <- 1e-4
  V$matrix <- mat
  V$matrix_rank <- sitemix:::.sm_matrix_rank(mat)
  estimates$V <- rep(list(V), nrow(estimates))

  warning <- NULL
  env <- withCallingHandlers(
    sitemix::sm_frechet_envelope(estimates, population_regime = "d1a"),
    sitemix_warning_frechet_non_diagonal_v = function(w) {
      warning <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(warning, "sitemix_warning_frechet_non_diagonal_v")
  expect_match(conditionMessage(warning), "ignores non-diagonal entries", fixed = TRUE)
  expect_equal(warning$expected, "working-independence D1 diagonal `V`")
  expect_match(warning$actual, "max off-diagonal =", fixed = TRUE)
  expect_equal(warning$fix, "Use this diagnostic only for D1 marginal aggregate outputs.")
  expect_s3_class(env, "sm_frechet_envelope")
  expect_equal(unname(diag(env$V_independence[["S1::2025"]])), estimates$se_raw^2, tolerance = 1e-12)
})

test_that("sm_frechet_envelope S3 methods summarize provenance and spread", {
  d1 <- data.frame(
    site_id = c("S1", "S1", "S2", "S2"),
    year = c(2025L, 2025L, 2025L, 2025L),
    indicator = c("absent", "suspend", "absent", "suspend"),
    c_jt = c(10L, 2L, 15L, 3L),
    n_jt = c(100L, 100L, 80L, 80L),
    stringsAsFactors = FALSE
  )
  estimates <- quiet_frechet_d1_warning(
    sitemix::sm_estimate_from_aggregates(
      d1,
      family = "multivariate",
      sampling_relation = "same_units",
      vjt = FALSE,
      min_n = 1L
    )
  )
  env <- sitemix::sm_frechet_envelope(estimates, population_regime = "d1a")
  rendered <- format(env, n = 1L)

  expect_s3_class(summary(env), "summary.sm_frechet_envelope")
  expect_named(
    summary(env),
    c(
      "site_key", "site_id", "year", "K", "scenario",
      "population_regime", "frechet_scope",
      "estimate_scale", "vcov_scale", "projection_method", "projection_status",
      "relative_tolerance", "absolute_tolerance_before", "absolute_tolerance_after",
      "eigen_scale_before", "eigen_scale_after", "min_eigen_before", "min_eigen_after",
      "raw_was_psd", "projection_attempted", "converged", "iterations", "max_iterations",
      "shrink_alpha_requested", "shrink_alpha_applied", "frobenius_independence",
      "raw_frobenius_norm", "projected_frobenius_norm",
      "projection_distance_absolute", "projection_distance_relative",
      "diagonal_max_abs_change", "diagonal_preserved",
      "symmetry_max_abs_residual", "symmetry_preserved", "psd_preserved",
      "sign_changes", "raw_interval_violations", "max_raw_interval_violation",
      "projected_order_reversals", "projected_order_reversal_max"
    )
  )
  expect_match(rendered[[1]], "Frechet pairwise intervals and projected stress scenarios", fixed = TRUE)
  expect_true(any(grepl("Population regime: d1a", rendered, fixed = TRUE)))
  expect_true(any(grepl("PSD method:", rendered, fixed = TRUE)))
  expect_output(print(env, n = 1L), "Median relative projection distance")

  heuristic_estimates <- quiet_frechet_d1_warning(
    sitemix::sm_estimate_from_aggregates(
      d1,
      family = "multivariate",
      sampling_relation = "unknown",
      vjt = FALSE,
      min_n = 1L
    )
  )
  d1b <- suppressWarnings(
    sitemix::sm_frechet_envelope(
      heuristic_estimates,
      population_regime = "d1b",
      subgroup_conditional_action = "warn"
    )
  )
  expect_true(any(grepl("heuristic_stress_test", format(d1b), fixed = TRUE)))
  expect_equal(unique(summary(d1b)$frechet_scope), "heuristic_stress_test")
})

test_that("sm_frechet_envelope supports K = 1 identified subsets and rejects suppression", {
  d1 <- data.frame(
    site_id = c("S1", "S1"),
    year = c(2025L, 2025L),
    indicator = c("absent", "suspend"),
    c_jt = c(10L, 2L),
    n_jt = c(100L, 100L),
    stringsAsFactors = FALSE
  )
  estimates <- quiet_frechet_d1_warning(
    sitemix::sm_estimate_from_aggregates(
      d1,
      family = "multivariate",
      sampling_relation = "same_units",
      min_n = 1L
    )
  )
  subset_env <- sitemix::sm_frechet_envelope(
    estimates,
    indicator = "absent",
    population_regime = "d1a"
  )
  expect_equal(dim(subset_env$V_lower_psd[[1]]), c(1L, 1L))
  expect_equal(subset_env$psd_diagnostics$L_iters, 0L)

  suppressed <- d1
  suppressed$c_jt[suppressed$indicator == "absent"] <- NA_integer_
  suppressed$n_jt[suppressed$indicator == "absent"] <- 8L
  suppressed_estimates <- quiet_frechet_d1_warning(
    sitemix::sm_estimate_from_aggregates(
      suppressed,
      family = "multivariate",
      sampling_relation = "same_units",
      suppression = "drop",
      min_n = 1L
    )
  )
  expect_error(
    sitemix::sm_frechet_envelope(suppressed_estimates, population_regime = "d1a"),
    class = "sitemix_error_invalid_indicators"
  )
})

test_that("sm_frechet_envelope population regime gate is explicit", {
  d1b <- data.frame(
    site_id = c("S1", "S1"),
    year = c(2025L, 2025L),
    indicator = c("AI", "HI"),
    c_jt = c(2L, 15L),
    n_jt = c(10L, 60L),
    stringsAsFactors = FALSE
  )
  estimates <- quiet_frechet_d1_warning(
    sitemix::sm_estimate_from_aggregates(d1b, family = "multivariate", min_n = 1L)
  )

  expect_error(
    sitemix::sm_frechet_envelope(estimates),
    class = "sitemix_error_population_regime_required"
  )
  expect_warning(
    warned <- sitemix::sm_frechet_envelope(
      estimates,
      population_regime = "d1b",
      subgroup_conditional_action = "warn"
    ),
    class = "sitemix_warning_frechet_d1b_heuristic"
  )
  expect_equal(warned$frechet_scope, "heuristic_stress_test")
  expect_silent(
    allowed <- sitemix::sm_frechet_envelope(
      estimates,
      population_regime = "d1b",
      subgroup_conditional_action = "allow"
    )
  )
  expect_equal(allowed$population_regime, "d1b")
  expect_error(
    sitemix::sm_frechet_envelope(
      estimates,
      population_regime = "d1b",
      subgroup_conditional_action = "error"
    ),
    class = "sitemix_error_frechet_d1b_disallowed"
  )
})

test_that("sm_frechet_envelope rejects non-D1 multivariate estimates", {
  counts <- data.frame(
    site_id = "S1",
    year = 2025L,
    n_jt = 10L,
    c_jt_a = 3L,
    c_jt_b = 4L,
    c_jt_a_b = 2L
  )
  scenario_b <- sitemix::sm_estimate_from_counts(
    counts,
    family = "multivariate",
    indicators = c("a", "b"),
    min_n = 1L
  )

  expect_error(
    sitemix::sm_frechet_envelope(scenario_b, population_regime = "d1a"),
    class = "sitemix_error_invalid_aggregate_case"
  )
})

test_that("Frechet envelope object errors use the generic input branch", {
  err <- rlang::catch_cnd(
    sitemix:::.sm_validate_frechet_envelope_object(list())
  )

  expect_s3_class(err, "sitemix_error_frechet_envelope_missing")
  expect_s3_class(err, "sitemix_error_input")
  expect_false(inherits(err, "sitemix_error_adapter"))
})
