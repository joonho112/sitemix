step314_core <- function(
  scale = 1,
  psd_method = "shrink",
  psd_max_iter = 100L,
  shrink_alpha = NULL,
  s_weights = rep(1, 3L),
  nearpd_args = list()
) {
  sitemix:::.sm_frechet_from_vectors(
    p = rep(0.5, 3L),
    s = sqrt(scale) * s_weights,
    indicators = c("a", "b", "c"),
    site_id = "S1",
    year = 2025L,
    psd_method = psd_method,
    psd_tol = 1e-8,
    psd_max_iter = psd_max_iter,
    shrink_alpha = shrink_alpha,
    return_correlations = FALSE,
    nearpd_args = nearpd_args
  )
}

step314_d1 <- function(K = 3L) {
  data <- data.frame(
    site_id = rep("S1", K),
    year = rep(2025L, K),
    indicator = letters[seq_len(K)],
    c_jt = c(50L, 50L, 50L)[seq_len(K)],
    n_jt = rep(100L, K),
    stringsAsFactors = FALSE
  )
  withCallingHandlers(
    sitemix::sm_estimate_from_aggregates(
      data,
      family = "multivariate",
      sampling_relation = "same_units",
      vjt = FALSE,
      vst = "none",
      boundary_method = "none",
      min_n = 1L
    ),
    sitemix_warning_working_independence_default = function(w) {
      invokeRestart("muffleWarning")
    }
  )
}

test_that("canonical projection diagnostics are long and fully scale labelled", {
  envelope <- sitemix::sm_frechet_envelope(
    step314_d1(),
    population_regime = "d1a",
    psd_method = "higham"
  )
  diagnostics <- envelope$projection_diagnostics

  expect_equal(nrow(diagnostics), 2L)
  expect_identical(
    diagnostics$scenario,
    c("negative_dependence_stress", "positive_dependence_stress")
  )
  expect_named(
    diagnostics,
    c(
      "site_id", "year", "site_key", "K", "scenario",
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
  expect_true(all(diagnostics$estimate_scale == "raw_probability"))
  expect_true(all(diagnostics$vcov_scale == "raw"))
  expect_true(all(diagnostics$converged[diagnostics$projection_attempted]))
  expect_true(all(is.na(diagnostics$converged[!diagnostics$projection_attempted])))
  expect_true(all(diagnostics$diagonal_preserved))
  expect_true(all(diagnostics$symmetry_preserved))
  expect_true(all(diagnostics$psd_preserved))
  expect_equal(nrow(envelope$psd_diagnostics), 1L)
  expect_true(all(c("L_iters", "U_iters", "frob_L_PSD") %in% names(envelope$psd_diagnostics)))
  expect_equal(nrow(summary(envelope)), 2L)
  expect_true(any(grepl("scenario=negative_dependence_stress", format(envelope), fixed = TRUE)))
})

test_that("equicorrelation shrink oracle has the closed-form alpha frontier", {
  raw <- step314_core(shrink_alpha = 0.5)
  negative_raw <- raw$unprojected_negative_dependence_corner
  negative_projected <- raw$projected_negative_dependence_stress
  correlation_raw <- cov2cor(negative_raw)
  correlation_projected <- cov2cor(negative_projected)
  offdiag <- upper.tri(correlation_raw)

  expect_equal(correlation_raw[offdiag], rep(-1, 3L), tolerance = 1e-14)
  expect_equal(correlation_projected[offdiag], rep(-0.5, 3L), tolerance = 1e-14)
  expect_equal(
    negative_projected,
    0.5 * negative_raw + 0.5 * diag(diag(negative_raw)),
    tolerance = 1e-14
  )
  negative_diag <- raw$projection_diagnostics[
    raw$projection_diagnostics$scenario == "negative_dependence_stress",
    ,
    drop = FALSE
  ]
  expect_equal(negative_diag$shrink_alpha_requested, 0.5)
  expect_equal(negative_diag$shrink_alpha_applied, 0.5)
  expect_equal(negative_diag$projection_status, "fixed_alpha_applied")
  expect_true(negative_diag$converged)
})

test_that("fixed shrink alpha is exact for K greater than two and rejects infeasibility", {
  fixed <- step314_core(shrink_alpha = 0.4)
  raw <- fixed$unprojected_negative_dependence_corner
  expected <- 0.4 * raw + 0.6 * diag(diag(raw))
  expect_equal(fixed$projected_negative_dependence_stress, expected, tolerance = 1e-14)
  expect_equal(
    fixed$projection_diagnostics$shrink_alpha_applied,
    c(0.4, 0.4)
  )

  expect_error(
    step314_core(shrink_alpha = 0.75),
    class = "sitemix_error_vcov_invariant"
  )

  already_psd <- matrix(
    c(1, 0.1, 0.2, 0.1, 2, 0.3, 0.2, 0.3, 3),
    3L,
    dimnames = list(letters[1:3], letters[1:3])
  )
  projected <- sitemix:::.sm_frechet_psd_project(
    already_psd,
    method = "shrink",
    psd_tol = 1e-8,
    psd_max_iter = 100L,
    shrink_alpha = 0.4,
    nearpd_args = list()
  )
  expect_equal(
    projected$mat,
    0.4 * already_psd + 0.6 * diag(diag(already_psd)),
    tolerance = 1e-14
  )
  expect_true(projected$was_psd)
  expect_true(projected$attempted)
  expect_equal(projected$shrink_alpha_applied, 0.4)
})

test_that("Higham and shrink diagnostics are invariant under covariance rescaling", {
  scales <- c(1, 1e-6, 1e-12)
  for (method in c("higham", "shrink")) {
    results <- lapply(scales, function(scale) {
      step314_core(
        scale = scale,
        psd_method = method,
        s_weights = c(1, 2, 3)
      )
    })
    reference <- results[[1L]]
    for (i in seq_along(scales)) {
      result <- results[[i]]
      expect_equal(
        result$projected_negative_dependence_stress / scales[[i]],
        reference$projected_negative_dependence_stress,
        tolerance = 1e-11
      )
      expect_equal(
        unname(diag(result$projected_negative_dependence_stress)),
        scales[[i]] * c(1, 4, 9),
        tolerance = 1e-12
      )
      diagnostics <- result$projection_diagnostics
      expect_true(all(diagnostics$converged[diagnostics$projection_attempted]))
      expect_true(all(is.na(diagnostics$converged[!diagnostics$projection_attempted])))
      expect_true(all(diagnostics$diagonal_preserved))
      expect_true(all(diagnostics$symmetry_preserved))
      expect_true(all(diagnostics$psd_preserved))
      expect_lt(max(abs(
        diagnostics$min_eigen_after / scales[[i]] -
          reference$projection_diagnostics$min_eigen_after
      )), 1e-12)
      expect_identical(
        diagnostics$iterations,
        reference$projection_diagnostics$iterations
      )
    }
  }
})

test_that("K at most two is exact identity under every projection request", {
  for (method in c("higham", "shrink")) {
    result <- sitemix:::.sm_frechet_from_vectors(
      p = c(0.5, 0.5),
      s = c(1, 2),
      indicators = c("a", "b"),
      psd_method = method,
      psd_tol = 1e-8,
      psd_max_iter = 1L,
      shrink_alpha = if (method == "shrink") 0.4 else NULL,
      nearpd_args = list()
    )
    expect_identical(
      result$projected_negative_dependence_stress,
      result$unprojected_negative_dependence_corner
    )
    expect_identical(
      result$projected_positive_dependence_stress,
      result$unprojected_positive_dependence_corner
    )
    expect_true(all(result$projection_diagnostics$projection_status == "identity_k_le_2"))
    expect_false(any(result$projection_diagnostics$projection_attempted))
    expect_true(all(is.na(result$projection_diagnostics$converged)))
  }
})

test_that("both iterative projection backends fail closed on nonconvergence", {
  expect_error(
    step314_core(psd_method = "higham", psd_max_iter = 1L),
    class = "sitemix_error_vcov_projection_nonconvergence"
  )
  expect_error(
    step314_core(psd_method = "shrink", psd_max_iter = 1L),
    class = "sitemix_error_vcov_projection_nonconvergence"
  )
})

test_that("public PSD tolerance stays numerical and cannot relabel indefiniteness", {
  estimates <- step314_d1()
  for (invalid in c(1, 1e-6, Inf, 0)) {
    expect_error(
      sitemix::sm_frechet_envelope(
        estimates,
        population_regime = "d1a",
        psd_tol = invalid
      ),
      class = "sitemix_error_invalid_psd_tol"
    )
  }

  admissible <- c(1e-12, 1e-8, sqrt(.Machine$double.eps))
  for (tol in admissible) {
    result <- sitemix:::.sm_frechet_from_vectors(
      p = rep(0.5, 3L),
      s = rep(1, 3L),
      indicators = c("a", "b", "c"),
      psd_method = "higham",
      psd_tol = tol,
      psd_max_iter = 100L,
      nearpd_args = list()
    )
    negative <- result$projection_diagnostics[
      result$projection_diagnostics$scenario == "negative_dependence_stress",
      ,
      drop = FALSE
    ]
    expect_equal(
      sort(eigen(
        result$unprojected_negative_dependence_corner,
        symmetric = TRUE,
        only.values = TRUE
      )$values),
      c(-1, 2, 2),
      tolerance = 1e-14
    )
    expect_false(negative$raw_was_psd)
    expect_true(negative$projection_attempted)
    expect_true(negative$converged)
  }
})

test_that("projection diagnostics are recomputed after final diagonal reset and symmetry", {
  result <- step314_core(psd_method = "higham", s_weights = c(1, 2, 3))
  diagnostics <- result$projection_diagnostics
  raw <- list(
    result$unprojected_negative_dependence_corner,
    result$unprojected_positive_dependence_corner
  )
  projected <- list(
    result$projected_negative_dependence_stress,
    result$projected_positive_dependence_stress
  )
  for (i in 1:2) {
    expect_equal(diagnostics$min_eigen_after[[i]], min(eigen(
      projected[[i]], symmetric = TRUE, only.values = TRUE
    )$values))
    expect_equal(
      diagnostics$diagonal_max_abs_change[[i]],
      max(abs(diag(projected[[i]]) - diag(raw[[i]])))
    )
    expect_equal(
      diagnostics$symmetry_max_abs_residual[[i]],
      max(abs(projected[[i]] - t(projected[[i]])))
    )
    expect_equal(
      diagnostics$projection_distance_absolute[[i]],
      norm(projected[[i]] - raw[[i]], type = "F")
    )
  }
})

test_that("reserved nearPD arguments fail closed and supported settings are recorded", {
  estimates <- step314_d1()
  expect_error(
    sitemix::sm_frechet_envelope(
      estimates,
      population_regime = "d1a",
      maxit = 3L
    ),
    class = "sitemix_error_invalid_psd_method"
  )
  expect_error(
    sitemix::sm_frechet_envelope(
      estimates,
      population_regime = "d1a",
      keepDiag = FALSE
    ),
    class = "sitemix_error_invalid_psd_method"
  )
  expect_error(
    sitemix::sm_frechet_envelope(
      estimates,
      population_regime = "d1a",
      posd.tol = 1e-6
    ),
    class = "sitemix_error_invalid_psd_method"
  )

  envelope <- sitemix::sm_frechet_envelope(
    estimates,
    population_regime = "d1a",
    doDykstra = FALSE,
    conv.norm.type = "F"
  )
  expect_false(envelope$projection_config$nearpd_settings$doDykstra)
  expect_equal(envelope$projection_config$nearpd_settings$conv.norm.type, "F")
  expect_true(sitemix:::.sm_validate_frechet_envelope_object(envelope))
})

test_that("stored config replays K3 projections and rejects coordinated aliases tamper", {
  envelope <- sitemix::sm_frechet_envelope(
    step314_d1(),
    population_regime = "d1a",
    psd_method = "shrink",
    shrink_alpha = 0.4
  )
  expect_true(sitemix:::.sm_validate_frechet_envelope_object(envelope))

  projected_tamper <- envelope
  changed <- projected_tamper$projected_negative_dependence_stress
  changed[[1L]][1L, 2L] <- changed[[1L]][2L, 1L] <-
    changed[[1L]][1L, 2L] + 1e-7
  projected_tamper$projected_negative_dependence_stress <- changed
  projected_tamper$V_lower_psd <- changed
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(projected_tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )

  config_tamper <- envelope
  config_tamper$projection_config$fixed_alpha_policy <- "ignore_if_raw_psd"
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(config_tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )

  tolerance_tamper <- envelope
  tolerance_tamper$psd_tol <- 1
  tolerance_tamper$projection_config$relative_tolerance <- 1
  tolerance_tamper$projection_config$nearpd_settings$eig.tol <- 1
  tolerance_tamper$projection_config$nearpd_settings$conv.tol <- 1
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(tolerance_tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )

  diagnostics_tamper <- envelope
  diagnostics_tamper$projection_diagnostics$min_eigen_after[[1L]] <- 0
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(diagnostics_tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )
})

test_that("legacy boundary diagnostics replay exactly from K3 marginal probabilities", {
  data <- data.frame(
    site_id = rep("S1", 3L),
    year = rep(2025L, 3L),
    indicator = c("a", "b", "c"),
    c_jt = c(0L, 50L, 100L),
    n_jt = rep(100L, 3L),
    stringsAsFactors = FALSE
  )
  estimates <- withCallingHandlers(
    sitemix::sm_estimate_from_aggregates(
      data,
      family = "multivariate",
      sampling_relation = "same_units",
      vjt = FALSE,
      min_n = 1L
    ),
    sitemix_warning_working_independence_default = function(w) {
      invokeRestart("muffleWarning")
    }
  )
  envelope <- sitemix::sm_frechet_envelope(
    estimates,
    population_regime = "d1a"
  )
  expect_identical(
    envelope$psd_diagnostics$boundary_marginal_indicators[[1L]],
    c("a", "c")
  )

  tamper <- envelope
  tamper$psd_diagnostics$boundary_marginal_indicators[[1L]] <- character()
  expect_error(
    sitemix:::.sm_validate_frechet_envelope_object(tamper),
    class = "sitemix_error_frechet_envelope_missing"
  )
})

test_that("public Frechet controls reject malformed selectors and projection settings", {
  estimates <- step314_d1()
  complete_args <- list(
    x = estimates,
    indicator = NULL,
    population_regime = "d1a",
    subgroup_conditional_action = "allow",
    return_correlations = FALSE,
    psd_method = "higham",
    psd_tol = 1e-8,
    psd_max_iter = 100L,
    shrink_alpha = NULL
  )
  cases <- list(
    malformed_indicator = list(
      call = function() {
        sitemix::sm_frechet_envelope(
          estimates,
          indicator = 1,
          population_regime = "d1a"
        )
      },
      class = "sitemix_error_invalid_indicator",
      message = "must be NULL or one or more distinct D1 indicators"
    ),
    absent_indicator = list(
      call = function() {
        sitemix::sm_frechet_envelope(
          estimates,
          indicator = "ghost",
          population_regime = "d1a"
        )
      },
      class = "sitemix_error_invalid_indicator",
      message = "contains labels not present in `x`"
    ),
    invalid_max_iter = list(
      call = function() {
        sitemix::sm_frechet_envelope(
          estimates,
          population_regime = "d1a",
          psd_max_iter = 0
        )
      },
      class = "sitemix_error_invalid_psd_max_iter",
      message = "must be a positive integer scalar"
    ),
    invalid_shrink_alpha = list(
      call = function() {
        sitemix::sm_frechet_envelope(
          estimates,
          population_regime = "d1a",
          psd_method = "shrink",
          shrink_alpha = 0
        )
      },
      class = "sitemix_error_invalid_shrink_alpha",
      message = "must be NULL or a finite scalar in (0, 1]"
    ),
    higham_shrink_alpha = list(
      call = function() {
        sitemix::sm_frechet_envelope(
          estimates,
          population_regime = "d1a",
          shrink_alpha = 0.5
        )
      },
      class = "sitemix_error_invalid_shrink_alpha",
      message = "only meaningful when `psd_method = \"shrink\"`"
    ),
    shrink_nearpd_dots = list(
      call = function() {
        sitemix::sm_frechet_envelope(
          estimates,
          population_regime = "d1a",
          psd_method = "shrink",
          doSym = FALSE
        )
      },
      class = "sitemix_error_invalid_psd_method",
      message = "only available for the Higham method"
    ),
    unnamed_nearpd_dot = list(
      call = function() {
        do.call(
          sitemix::sm_frechet_envelope,
          c(complete_args, list(FALSE))
        )
      },
      class = "sitemix_error_invalid_psd_method",
      message = "must be named"
    ),
    duplicate_nearpd_dot = list(
      call = function() {
        do.call(
          sitemix::sm_frechet_envelope,
          c(
            complete_args,
            structure(list(FALSE, TRUE), names = c("doSym", "doSym"))
          )
        )
      },
      class = "sitemix_error_invalid_psd_method",
      message = "names must be unique"
    ),
    unsupported_nearpd_dot = list(
      call = function() {
        do.call(
          sitemix::sm_frechet_envelope,
          c(complete_args, list(foo = 1))
        )
      },
      class = "sitemix_error_invalid_psd_method",
      message = "Unsupported `nearPD()` settings"
    )
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    error <- expect_error(
      case$call(),
      class = case$class,
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

test_that("public deterministic shrink projection fails closed on nonconvergence", {
  error <- expect_error(
    sitemix::sm_frechet_envelope(
      step314_d1(),
      population_regime = "d1a",
      psd_method = "shrink",
      psd_max_iter = 1L
    ),
    class = "sitemix_error_vcov_projection_nonconvergence"
  )

  expect_match(
    conditionMessage(error),
    "did not converge within `psd_max_iter`",
    fixed = TRUE
  )
  expect_match(error$expected, "alpha interval width", fixed = TRUE)
})
