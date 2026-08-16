jui_stop <- function(message) {
  stop(message, call. = FALSE)
}

jui_quiet_working_independence <- function(expr) {
  withCallingHandlers(
    expr,
    sitemix_warning_working_independence_default = function(w) {
      invokeRestart("muffleWarning")
    }
  )
}

jui_b_counts <- function(two_sites = FALSE) {
  out <- data.frame(
    site_id = if (two_sites) c("B", "A") else "A",
    year = if (two_sites) c(2025L, 2024L) else 2024L,
    n_jt = if (two_sites) c(8L, 10L) else 10L,
    c_jt_a = if (two_sites) c(3L, 4L) else 4L,
    c_jt_b = if (two_sites) c(5L, 6L) else 6L,
    c_jt_a_b = if (two_sites) c(2L, 3L) else 3L,
    stringsAsFactors = FALSE
  )
  out
}

jui_b_estimate <- function(vst = "arcsine", two_sites = FALSE, fpc = NULL) {
  sm_estimate_from_counts(
    jui_b_counts(two_sites = two_sites),
    family = "multivariate",
    indicators = c("a", "b"),
    vst = vst,
    vjt = TRUE,
    min_n = 1L,
    fpc = fpc
  )
}

jui_c_estimate <- function(fpc = NULL) {
  sm_estimate_from_counts(
    data.frame(
      site_id = "C",
      year = 2024L,
      n_jt = 10L,
      c_jt_x = 4L,
      c_jt_y = 3L,
      c_jt_z = 3L,
      stringsAsFactors = FALSE
    ),
    family = "multinomial",
    indicators = c("x", "y", "z"),
    vst = "none",
    vjt = TRUE,
    min_n = 1L,
    fpc = fpc
  )
}

jui_d1_estimate <- function(
  K = 2L,
  sampling_relation = "same_units",
  vjt = TRUE,
  vst = "arcsine"
) {
  indicators <- letters[seq_len(K)]
  counts <- c(20L, 70L, 40L)[seq_len(K)]
  denominators <- if (identical(sampling_relation, "same_units")) {
    rep(100L, K)
  } else {
    c(100L, 80L, 60L)[seq_len(K)]
  }
  data <- data.frame(
    site_id = rep("D", K),
    year = rep(2025L, K),
    indicator = indicators,
    c_jt = counts,
    n_jt = denominators,
    stringsAsFactors = FALSE
  )
  jui_quiet_working_independence(
    sm_estimate_from_aggregates(
      data,
      family = "multivariate",
      sampling_relation = sampling_relation,
      vst = vst,
      boundary_method = "none",
      vjt = vjt,
      min_n = 1L
    )
  )
}

jui_stamp_smoothing <- function(x, overwrite = FALSE, status = "fit") {
  eligible <- rep(TRUE, nrow(x))
  x$se_smoothed <- x$se
  x$var_method_smoothed <- x$var_method
  if (isTRUE(overwrite) && identical(status, "fit")) {
    x$se_pre_smoothing <- x$se
  }
  v_fact <- sitemix:::.sm_smoothing_v_fact(x, "se", eligible)
  attr(x, "smoothing") <- sitemix:::.sm_smoothing_provenance(
    method = "loglinear",
    scale = "se",
    scope = "all",
    by = NULL,
    min_n = NULL,
    min_rows = 2L,
    bias_correct = TRUE,
    overwrite = overwrite,
    model_formula = log_var ~ log_n,
    eligible = eligible,
    target_column = "se_smoothed",
    v_fact = v_fact,
    status = status
  )
  x
}

jui_empty_joint_result <- function() {
  list(
    groups = list(),
    results = data.frame(
      site_id = character(),
      year = integer(),
      K = integer(),
      point_scale = character(),
      vcov_scale = character(),
      vcov_method = character(),
      diag_contract = character(),
      contrast_estimate = numeric(),
      contrast_variance = numeric(),
      contrast_se = numeric(),
      stringsAsFactors = FALSE
    )
  )
}

jui_consume_joint <- function(
  x,
  contrast,
  analysis_scale = c("reported", "raw"),
  uncertainty_source = "canonical"
) {
  analysis_scale <- match.arg(analysis_scale)
  if (!identical(uncertainty_source, "canonical")) {
    jui_stop("joint consumption requires canonical uncertainty; smoothed alternatives need a rebuilt covariance")
  }

  summary_diag <- as.data.frame(sm_diagnose(x, level = "summary", verbose = FALSE))
  row_diag <- as.data.frame(sm_diagnose(x, level = "row", verbose = FALSE))
  status <- if ("estimate_status" %in% names(row_diag)) {
    row_diag$estimate_status
  } else {
    rep("identified", nrow(row_diag))
  }

  if (any(status == "suppression_sensitivity") ||
      any(row_diag$suppression_sensitivity_role != "none")) {
    jui_stop("suppression sensitivity is not ordinary joint uncertainty")
  }
  if (any(status != "identified") || any(row_diag$flag_suppressed)) {
    jui_stop("suppressed or unavailable coordinates cannot be dropped from a covariance group")
  }
  if (identical(summary_diag$smoothing_provenance_valid[[1L]], FALSE)) {
    jui_stop("invalid smoothing provenance blocks joint consumption")
  }
  if (!identical(summary_diag$v_stale[[1L]], FALSE)) {
    jui_stop("stale or unknown covariance state blocks joint consumption")
  }
  if (any(row_diag$diag_severity == "error")) {
    jui_stop("row diagnostic error blocks joint consumption")
  }
  if (!all(c("V", "K") %in% names(x))) {
    jui_stop("strict joint consumption requires both V and K")
  }
  if (nrow(x) == 0L) {
    return(jui_empty_joint_result())
  }
  if (!isTRUE(summary_diag$v_valid[[1L]])) {
    jui_stop("validated joint covariance is required")
  }

  vcov_diag <- as.data.frame(sm_diagnose(x, level = "vcov", verbose = FALSE))
  if (any(!vcov_diag$v_valid) || any(!vcov_diag$psd_ok) ||
      any(!vcov_diag$repeated_v_equal) ||
      any(vcov_diag$row_sum_zero_ok %in% FALSE) ||
      any(vcov_diag$diag_severity == "error") ||
      any(vcov_diag$v_stale %in% TRUE) || anyNA(vcov_diag$v_stale) ||
      any(vcov_diag$smoothing_provenance_valid %in% FALSE)) {
    jui_stop("covariance diagnostics failed a hard joint-consumption gate")
  }

  if (!is.numeric(contrast) || is.null(names(contrast)) || anyNA(contrast) ||
      any(!is.finite(contrast)) || any(names(contrast) == "") ||
      anyDuplicated(names(contrast))) {
    jui_stop("contrast must be a finite uniquely named numeric vector")
  }

  keys <- unique(data.frame(
    site_id = x$site_id,
    year = x$year,
    stringsAsFactors = FALSE
  ))
  groups <- vector("list", nrow(keys))
  results <- vector("list", nrow(keys))

  for (g in seq_len(nrow(keys))) {
    key_site <- keys$site_id[[g]]
    key_year <- keys$year[[g]]
    idx <- which(x$site_id == key_site & x$year == key_year)
    d_idx <- which(vcov_diag$site_id == key_site & vcov_diag$year == key_year)
    if (length(idx) == 0L || length(d_idx) != 1L) {
      jui_stop("tuple-key alignment failed for covariance diagnostics")
    }

    V <- x$V[[idx[[1L]]]]
    order <- V$indicator_order
    matrix <- as.matrix(V)
    group_indicators <- x$indicator[idx]
    if (!identical(V$site_id, key_site) || !identical(V$year, key_year)) {
      jui_stop("V site/year metadata does not match its tuple key")
    }
    if (anyDuplicated(group_indicators) || anyDuplicated(order) ||
        !setequal(group_indicators, order)) {
      jui_stop("group indicators and matrix coordinates must match exactly")
    }
    row_order <- match(order, group_indicators)
    if (anyNA(row_order) || length(row_order) != length(idx)) {
      jui_stop("partial, missing, or extra covariance coordinates are not allowed")
    }
    if (length(unique(x$K[idx])) != 1L ||
        x$K[[idx[[1L]]]] != length(idx) ||
        nrow(matrix) != length(idx) || ncol(matrix) != length(idx) ||
        length(order) != length(idx) ||
        !identical(rownames(matrix), order) ||
        !identical(colnames(matrix), order)) {
      jui_stop("K, group rows, matrix dimensions, and matrix names must agree")
    }
    if (!setequal(names(contrast), order) || length(contrast) != length(order)) {
      jui_stop("contrast names must equal the complete indicator_order")
    }

    ordered_rows <- idx[row_order]
    if (identical(V$diag_contract, "not_checked")) {
      jui_stop("diag_contract not_checked has no automatic row companion")
    }
    if (identical(analysis_scale, "reported")) {
      if (!isTRUE(vcov_diag$estimate_vcov_scale_compatible[[d_idx]]) ||
          !all(row_diag$estimate_vcov_scale_compatible[ordered_rows])) {
        jui_stop("reported estimates and V have incompatible scales")
      }
      theta <- x$theta_hat[ordered_rows]
      point_scale <- unique(x$estimate_scale[ordered_rows])
      if (length(point_scale) != 1L) {
        jui_stop("reported point scale must be constant within a covariance group")
      }
    } else {
      raw_contract <- V$diag_contract %in% c(
        "row_se_raw_squared",
        "row_se_raw_squared_except_boundary_surrogates"
      ) || (
        identical(V$diag_contract, "row_se_squared") &&
          all(x$estimate_scale[ordered_rows] == "none")
      )
      if (!identical(V$vcov_scale, "raw") || !isTRUE(raw_contract)) {
        jui_stop("raw analysis requires raw V and an explicit raw row companion")
      }
      theta <- x$theta_raw[ordered_rows]
      point_scale <- "raw_probability"
    }
    if (any(!is.finite(theta))) {
      jui_stop("joint point coordinates must be finite and identified")
    }

    a <- unname(contrast[order])
    names(a) <- order
    estimate <- sum(a * theta)
    variance <- drop(crossprod(a, matrix %*% a))
    tolerance <- 128 * length(a) * .Machine$double.eps *
      max(abs(matrix), .Machine$double.xmin)
    if (!is.finite(variance) || variance < -tolerance) {
      jui_stop("contrast variance is materially negative or non-finite")
    }
    variance <- max(0, variance)

    groups[[g]] <- list(
      key = data.frame(site_id = key_site, year = key_year),
      indicator_order = order,
      point = stats::setNames(theta, order),
      contrast = a,
      V = matrix,
      provenance = list(
        family = V$family,
        vcov_method = V$vcov_method,
        estimate_scale = V$estimate_scale,
        vcov_scale = V$vcov_scale,
        diag_contract = V$diag_contract,
        psd_repair = V$psd_repair,
        matrix_rank = V$matrix_rank,
        matrix_boundary_rule = V$matrix_boundary_rule,
        sampling_design = V$sampling_design,
        variance_rule = V$variance_rule
      )
    )
    results[[g]] <- data.frame(
      site_id = key_site,
      year = key_year,
      K = as.integer(length(order)),
      point_scale = point_scale,
      vcov_scale = V$vcov_scale,
      vcov_method = if (is.na(V$vcov_method)) NA_character_ else V$vcov_method,
      diag_contract = V$diag_contract,
      contrast_estimate = estimate,
      contrast_variance = variance,
      contrast_se = sqrt(variance),
      stringsAsFactors = FALSE
    )
  }

  list(groups = groups, results = do.call(rbind, results))
}

jui_consume_frechet_stress <- function(envelope, scenario, contrast) {
  allowed <- c("negative_dependence_stress", "positive_dependence_stress")
  if (!is.character(scenario) || length(scenario) != 1L || !scenario %in% allowed) {
    jui_stop("use one canonical projected stress-scenario name")
  }
  if (!is.numeric(contrast) || is.null(names(contrast)) || anyNA(contrast) ||
      any(!is.finite(contrast)) || any(names(contrast) == "") ||
      anyDuplicated(names(contrast))) {
    jui_stop("contrast must be a finite uniquely named numeric vector")
  }
  diagnostics <- as.data.frame(summary(envelope))
  if (!identical(envelope$projected_scenario_role, "stress_scenario_not_bound") ||
      !identical(envelope$covariance_scale, "raw")) {
    jui_stop("projected Frechet matrices must remain raw-scale stress scenarios, not bounds")
  }
  field <- if (identical(scenario, allowed[[1L]])) {
    "projected_negative_dependence_stress"
  } else {
    "projected_positive_dependence_stress"
  }
  keys <- envelope$site_keys
  output <- vector("list", nrow(keys))
  for (i in seq_len(nrow(keys))) {
    key <- keys$site_key[[i]]
    d <- diagnostics[
      diagnostics$site_key == key & diagnostics$scenario == scenario,
      ,
      drop = FALSE
    ]
    if (nrow(d) != 1L || !isTRUE(d$diagonal_preserved) ||
        !isTRUE(d$symmetry_preserved) || !isTRUE(d$psd_preserved) ||
        (isTRUE(d$projection_attempted) && !isTRUE(d$converged)) ||
        (!isTRUE(d$projection_attempted) && !is.na(d$converged))) {
      jui_stop("projected stress diagnostics failed convergence or invariant gates")
    }
    matrix <- envelope[[field]][[key]]
    order <- rownames(matrix)
    if (!is.matrix(matrix) || inherits(matrix, "sm_vcov") ||
        !identical(colnames(matrix), order) ||
        !setequal(names(contrast), order) || length(contrast) != length(order)) {
      jui_stop("stress matrix and named contrast coordinates do not align")
    }
    a <- unname(contrast[order])
    variance <- drop(crossprod(a, matrix %*% a))
    output[[i]] <- data.frame(
      site_id = keys$site_id[[i]],
      year = keys$year[[i]],
      site_key = key,
      scenario = scenario,
      scenario_role = envelope$projected_scenario_role,
      population_regime = envelope$population_regime,
      frechet_scope = envelope$frechet_scope,
      estimate_scale = d$estimate_scale,
      vcov_scale = d$vcov_scale,
      projection_method = d$projection_method,
      projection_status = d$projection_status,
      projection_distance_relative = d$projection_distance_relative,
      sign_changes = d$sign_changes,
      raw_interval_violations = d$raw_interval_violations,
      projected_order_reversals = d$projected_order_reversals,
      contrast_variance = max(0, variance),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, output)
}

test_that("reported and raw joint scale branches are explicit", {
  x <- jui_b_estimate(vst = "arcsine")
  contrast <- c(a = 1, b = -1)

  expect_false(sm_diagnose(x, verbose = FALSE)$estimate_vcov_scale_compatible)
  expect_error(
    jui_consume_joint(x, contrast, analysis_scale = "reported"),
    "incompatible scales"
  )

  consumed <- jui_consume_joint(x, contrast, analysis_scale = "raw")
  V <- as.matrix(x$V[[1L]])
  expected_point <- x$theta_raw[match(x$V[[1L]]$indicator_order, x$indicator)]
  expected_variance <- drop(crossprod(unname(contrast), V %*% unname(contrast)))

  expect_equal(consumed$results$point_scale, "raw_probability")
  expect_equal(consumed$results$vcov_scale, "raw")
  expect_equal(consumed$results$contrast_estimate, sum(contrast * expected_point))
  expect_equal(consumed$results$contrast_variance, expected_variance, tolerance = 1e-14)
  expect_identical(consumed$groups[[1L]]$indicator_order, c("a", "b"))
  expect_identical(consumed$groups[[1L]]$provenance$diag_contract, "row_se_raw_squared")
})

test_that("compatible transformed D1 output supports the reported branch", {
  x <- jui_d1_estimate(K = 2L, vst = "arcsine")
  contrast <- c(a = 0.25, b = 0.75)
  diagnosed <- sm_diagnose(x, level = "vcov", verbose = FALSE)

  expect_true(diagnosed$estimate_vcov_scale_compatible)
  expect_equal(diagnosed$vcov_scale, "arcsine_delta")
  consumed <- jui_consume_joint(x, contrast, analysis_scale = "reported")
  order <- x$V[[1L]]$indicator_order
  theta <- x$theta_hat[match(order, x$indicator)]
  oracle <- drop(crossprod(unname(contrast[order]), as.matrix(x$V[[1L]]) %*% unname(contrast[order])))

  expect_equal(consumed$results$point_scale, "arcsine")
  expect_equal(consumed$results$contrast_estimate, sum(contrast[order] * theta))
  expect_equal(consumed$results$contrast_variance, oracle, tolerance = 1e-14)
  expect_equal(consumed$results$vcov_method, "working_independence")
  expect_equal(consumed$results$diag_contract, "row_se_squared")
})

test_that("tuple keys and matrix coordinates survive complete row scrambling", {
  x <- jui_b_estimate(vst = "none", two_sites = TRUE)
  contrast <- c(a = 1, b = -0.5)
  original <- jui_consume_joint(x, contrast, analysis_scale = "reported")
  scrambled <- x[c(4L, 3L, 2L, 1L), ]
  reordered <- jui_consume_joint(scrambled, contrast, analysis_scale = "reported")

  sort_results <- function(value) {
    out <- value[order(value$site_id, value$year), , drop = FALSE]
    row.names(out) <- NULL
    out
  }
  expect_equal(sort_results(reordered$results), sort_results(original$results))
  expect_identical(reordered$groups[[1L]]$indicator_order, c("b", "a"))
  expect_identical(rownames(reordered$groups[[1L]]$V), c("b", "a"))
  expect_identical(names(reordered$groups[[1L]]$point), c("b", "a"))
  expect_equal(
    reordered$groups[[1L]]$point,
    stats::setNames(
      scrambled$theta_hat[1:2],
      scrambled$indicator[1:2]
    )
  )
})

test_that("partial, malformed, and unchecked matrix coordinates fail closed", {
  x <- jui_b_estimate(vst = "none")
  contrast <- c(a = 1, b = -1)

  no_k <- x
  no_k$K <- NULL
  expect_error(jui_consume_joint(no_k, contrast), "both V and K")

  partial <- as.data.frame(x)[1L, , drop = FALSE]
  attr(partial, "family") <- attr(x, "family", exact = TRUE)
  attr(partial, "sitemix_role") <- attr(x, "sitemix_role", exact = TRUE)
  class(partial) <- c("sitemix_estimates", "data.frame")
  expect_error(
    jui_consume_joint(partial, contrast),
    class = "sitemix_error_estimate_vcov_invariant"
  )

  bad_key <- x
  changed <- bad_key$V[[1L]]
  changed$site_id <- "OTHER"
  bad_key$V <- rep(list(changed), nrow(bad_key))
  expect_error(
    jui_consume_joint(bad_key, contrast),
    class = "sitemix_error_estimate_vcov_invariant"
  )

  unchecked <- x
  changed <- unchecked$V[[1L]]
  changed$diag_contract <- "not_checked"
  unchecked$V <- rep(list(changed), nrow(unchecked))
  expect_error(jui_consume_joint(unchecked, contrast), "not_checked")

  expect_error(jui_consume_joint(x, c(a = 1)), "complete indicator_order")
  duplicate_contrast <- structure(c(1, -1), names = c("a", "a"))
  expect_error(jui_consume_joint(x, duplicate_contrast), "uniquely named")
})

test_that("singular multinomial and exact-census matrices need no inverse", {
  x <- jui_c_estimate()
  contrast <- c(x = 1, y = -1, z = 0)
  consumed <- jui_consume_joint(x, contrast, analysis_scale = "reported")
  V <- as.matrix(x$V[[1L]])
  oracle <- drop(crossprod(unname(contrast), V %*% unname(contrast)))

  expect_equal(x$V[[1L]]$matrix_rank, 2L)
  expect_equal(as.vector(V %*% rep(1, 3)), rep(0, 3), tolerance = 1e-14)
  expect_equal(consumed$results$contrast_variance, oracle, tolerance = 1e-14)
  expect_gt(consumed$results$contrast_variance, 0)

  census <- jui_c_estimate(fpc = 10)
  diagnosed <- sm_diagnose(census, level = "vcov", verbose = FALSE)
  census_result <- jui_consume_joint(census, contrast, analysis_scale = "reported")
  expect_true(diagnosed$zero_uncertainty_census)
  expect_equal(diagnosed$diag_severity, "note")
  expect_equal(as.matrix(census$V[[1L]]), matrix(
    0,
    3,
    3,
    dimnames = list(c("x", "y", "z"), c("x", "y", "z"))
  ))
  expect_equal(census_result$results$contrast_variance, 0)
  expect_equal(census_result$results$contrast_se, 0)
})

test_that("stale and invalid smoothing fail while canonical append-only output works", {
  x <- jui_b_estimate(vst = "none")
  contrast <- c(a = 1, b = -1)

  append_only <- jui_stamp_smoothing(x, overwrite = FALSE, status = "fit")
  append_diag <- sm_diagnose(append_only, verbose = FALSE)
  expect_true(append_diag$smoothing_provenance_valid)
  expect_false(append_diag$v_stale)
  expect_no_error(jui_consume_joint(append_only, contrast))
  expect_error(
    jui_consume_joint(append_only, contrast, uncertainty_source = "smoothed"),
    "canonical uncertainty"
  )

  stale <- jui_stamp_smoothing(x, overwrite = TRUE, status = "fit")
  expect_true(sm_diagnose(stale, verbose = FALSE)$v_stale)
  expect_error(jui_consume_joint(stale, contrast), "stale or unknown")

  invalid <- append_only
  smoothing <- attr(invalid, "smoothing", exact = TRUE)
  smoothing$n_eligible <- 999L
  attr(invalid, "smoothing") <- smoothing
  invalid_diag <- sm_diagnose(invalid, verbose = FALSE)
  expect_false(invalid_diag$smoothing_provenance_valid)
  expect_true(is.na(invalid_diag$v_stale))
  expect_error(jui_consume_joint(invalid, contrast), "invalid smoothing provenance")

  skipped <- jui_stamp_smoothing(x, overwrite = TRUE, status = "skipped")
  expect_false(sm_diagnose(skipped, verbose = FALSE)$v_stale)
  expect_no_error(jui_consume_joint(skipped, contrast))
})

test_that("suppressed and sensitivity rows never become joint coordinates", {
  data <- data.frame(
    site_id = c("S", "I"),
    year = c(2025L, 2025L),
    indicator = c("a", "a"),
    c_jt = c(NA_integer_, 10L),
    n_jt = c(8L, 40L),
    stringsAsFactors = FALSE
  )
  dropped <- sm_estimate_from_aggregates(
    data,
    family = "binomial",
    indicator = "a",
    suppression = "drop",
    min_n = 1L
  )
  sensitivity <- sm_estimate_from_aggregates(
    data,
    family = "binomial",
    indicator = "a",
    suppression = "upper_bound",
    suppression_sensitivity_acknowledge = TRUE,
    min_n = 1L
  )

  expect_identical(dropped$estimate_status, c("identified", "suppressed_missing"))
  expect_identical(sensitivity$estimate_status, c("identified", "suppression_sensitivity"))
  expect_error(
    jui_consume_joint(dropped, c(a = 1)),
    "suppressed or unavailable"
  )
  expect_error(
    jui_consume_joint(sensitivity, c(a = 1)),
    "suppression sensitivity"
  )
  sensitivity_row <- sensitivity$estimate_status == "suppression_sensitivity"
  expect_true(is.finite(sensitivity$sensitivity_var[sensitivity_row]))
  expect_true(is.na(sensitivity$se[sensitivity_row]))
  expect_false("V" %in% names(sensitivity))
})

test_that("formal K2 intervals stay distinct from projected stress fields", {
  estimates <- jui_d1_estimate(K = 2L, vjt = FALSE, vst = "none")
  envelope <- sm_frechet_envelope(
    estimates,
    population_regime = "d1a",
    return_correlations = TRUE
  )
  interval <- envelope$raw_pairwise_intervals
  stress <- jui_consume_frechet_stress(
    envelope,
    "negative_dependence_stress",
    c(a = 1, b = -1)
  )

  expect_equal(interval$interval_scope, "formal_raw_pairwise_interval")
  expect_equal(interval$pairwise_covariance_lower, (0 - 0.2 * 0.7) / 100)
  expect_equal(interval$pairwise_covariance_upper, (0.2 - 0.2 * 0.7) / 100)
  expect_identical(
    envelope$unprojected_negative_dependence_corner,
    envelope$projected_negative_dependence_stress
  )
  expect_equal(stress$scenario_role, "stress_scenario_not_bound")
  expect_equal(stress$frechet_scope, "formal")
  expect_equal(stress$scenario, "negative_dependence_stress")
  expect_false(inherits(envelope$projected_negative_dependence_stress[[1L]], "sm_vcov"))
  expect_true(is.matrix(envelope$projected_negative_dependence_stress[[1L]]))
  expect_false(stress$scenario %in% c("lower_bound", "upper_bound"))
  expect_error(
    jui_consume_frechet_stress(
      envelope,
      "negative_dependence_stress",
      c(a = Inf, b = -1)
    ),
    "finite uniquely named"
  )
  expect_error(
    jui_consume_frechet_stress(
      envelope,
      "negative_dependence_stress",
      c(a = NaN, b = -1)
    ),
    "finite uniquely named"
  )
  expect_error(
    jui_consume_frechet_stress(
      envelope,
      "negative_dependence_stress",
      stats::setNames(c(1, -1), c("a", "a"))
    ),
    "finite uniquely named"
  )
})

test_that("K3 projected matrices remain labelled stress scenarios across regimes", {
  formal <- sm_frechet_envelope(
    jui_d1_estimate(K = 3L, vjt = FALSE, vst = "none"),
    population_regime = "d1a",
    psd_method = "higham"
  )
  negative <- jui_consume_frechet_stress(
    formal,
    "negative_dependence_stress",
    c(a = 1, b = -1, c = 0)
  )
  positive <- jui_consume_frechet_stress(
    formal,
    "positive_dependence_stress",
    c(a = 1, b = -1, c = 0)
  )

  expect_equal(negative$scenario_role, "stress_scenario_not_bound")
  expect_equal(positive$scenario_role, "stress_scenario_not_bound")
  expect_equal(negative$vcov_scale, "raw")
  expect_equal(negative$estimate_scale, "raw_probability")
  expect_true(all(summary(formal)$diagonal_preserved))
  expect_true(all(summary(formal)$symmetry_preserved))
  expect_true(all(summary(formal)$psd_preserved))
  expect_true(all(
    summary(formal)$converged[summary(formal)$projection_attempted]
  ))
  expect_true(all(is.na(
    summary(formal)$converged[!summary(formal)$projection_attempted]
  )))
  expect_true(all(negative$raw_interval_violations >= 0L))
  expect_true(all(negative$projected_order_reversals >= 0L))

  heuristic <- sm_frechet_envelope(
    jui_d1_estimate(
      K = 3L,
      sampling_relation = "unknown",
      vjt = FALSE,
      vst = "none"
    ),
    population_regime = "d1b",
    subgroup_conditional_action = "allow"
  )
  heuristic_stress <- jui_consume_frechet_stress(
    heuristic,
    "negative_dependence_stress",
    c(a = 1, b = -1, c = 0)
  )
  expect_equal(heuristic_stress$frechet_scope, "heuristic_stress_test")
  expect_equal(
    unique(heuristic$raw_pairwise_intervals$interval_scope),
    "heuristic_pairwise_stress_range"
  )
  expect_true(all(is.na(heuristic$raw_pairwise_intervals$n_common)))
})

test_that("conversion validates envelopes, keeps empty types, and adds no API", {
  envelope <- sm_frechet_envelope(
    jui_d1_estimate(K = 3L, vjt = FALSE, vst = "none"),
    population_regime = "d1a"
  )
  tampered <- envelope
  changed <- tampered$projected_negative_dependence_stress
  changed[[1L]][1L, 2L] <- changed[[1L]][2L, 1L] <- 0
  tampered$projected_negative_dependence_stress <- changed
  tampered$V_lower_psd <- changed
  expect_error(
    jui_consume_frechet_stress(
      tampered,
      "negative_dependence_stress",
      c(a = 1, b = -1, c = 0)
    ),
    class = "sitemix_error_frechet_envelope_missing"
  )

  x <- jui_b_estimate(vst = "none")
  empty <- x[0L, ]
  consumed <- jui_consume_joint(
    empty,
    c(a = 1, b = -1),
    analysis_scale = "reported"
  )
  expect_length(consumed$groups, 0L)
  expect_equal(nrow(consumed$results), 0L)
  expect_type(consumed$results$site_id, "character")
  expect_type(consumed$results$year, "integer")
  expect_type(consumed$results$contrast_variance, "double")

  exports <- getNamespaceExports("sitemix")
  expect_false(any(c(
    "sm_as_joint_data",
    "sm_as_summary_data",
    "sm_select_indicators"
  ) %in% exports))
})
