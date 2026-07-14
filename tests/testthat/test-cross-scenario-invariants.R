cross_default_columns <- function() {
  c(
    "site_id",
    "year",
    "indicator",
    "theta_raw",
    "theta_hat",
    "se_raw",
    "se",
    "n",
    "n_eff",
    "estimate_scale",
    "transform",
    "var_method",
    "flag_small_n",
    "flag_zero_cell",
    "input_mode",
    "flag_suppressed",
    "framing",
    "flag_below_accountability"
  )
}

cross_binomial_counts <- function() {
  data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    n_jt = c(10L, 12L),
    c_jt_absent = c(4L, 6L)
  )
}

cross_multivariate_counts <- function() {
  data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 10L,
    c_jt_snap = 4L,
    c_jt_frpm = 7L,
    c_jt_snap_frpm = 3L
  )
}

cross_multinomial_counts <- function() {
  data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 10L,
    c_jt_eng = 4L,
    c_jt_spa = 3L,
    c_jt_oth = 3L
  )
}

cross_v_outputs <- function() {
  list(
    binomial = sitemix::sm_estimate_from_counts(
      cross_binomial_counts(),
      family = "binomial",
      indicator = "absent",
      vjt = TRUE,
      min_n = 2L,
      description = "cross binomial"
    ),
    multivariate = sitemix::sm_estimate_from_counts(
      cross_multivariate_counts(),
      family = "multivariate",
      indicators = c("snap", "frpm"),
      vjt = TRUE,
      min_n = 2L,
      description = "cross multivariate"
    ),
    multinomial = sitemix::sm_estimate_from_counts(
      cross_multinomial_counts(),
      family = "multinomial",
      indicators = c("eng", "spa", "oth"),
      vjt = TRUE,
      min_n = 2L,
      description = "cross multinomial"
    )
  )
}

cross_expect_psd <- function(V, tol = 1e-10) {
  mat <- as.matrix(V)
  eigenvalues <- eigen((mat + t(mat)) / 2, symmetric = TRUE, only.values = TRUE)$values
  expect_gte(min(eigenvalues), -tol)
}

cross_property_students <- function() {
  data.frame(
    site_id = rep(c("S1", "S2"), each = 12L),
    year = rep(c(2024L, 2025L), each = 12L),
    absent = rep(c(0L, 1L, 0L, 1L, 1L, 0L), 4L),
    a = rep(c(0L, 1L, 1L, 0L, 1L, 0L, 1L, 0L), 3L),
    b = rep(c(1L, 1L, 0L, 0L, 1L, 0L), 4L),
    c = rep(c(0L, 1L, 0L, 1L, 0L, 1L, 1L, 0L), 3L),
    language = factor(
      rep(c("eng", "spa", "oth", "eng", "oth", "spa"), 4L),
      levels = c("eng", "spa", "oth")
    ),
    stringsAsFactors = FALSE
  )
}

cross_property_outputs <- function(
  data,
  binomial_indicator = "absent",
  multivariate_indicators = c("a", "b", "c"),
  multinomial_indicator = "language"
) {
  list(
    binomial = sm_estimate(
      data,
      family = "binomial",
      indicator = binomial_indicator,
      vst = "none",
      boundary_method = "none",
      vjt = TRUE,
      min_n = 1L
    ),
    multivariate = sm_estimate(
      data,
      family = "multivariate",
      indicators = multivariate_indicators,
      vst = "none",
      boundary_method = "none",
      vjt = TRUE,
      min_n = 1L
    ),
    multinomial = sm_estimate(
      data,
      family = "multinomial",
      indicator = multinomial_indicator,
      vst = "none",
      boundary_method = "none",
      vjt = TRUE,
      min_n = 1L
    )
  )
}

cross_canonical_output <- function(x) {
  rows <- order(x$site_id, x$year, x$indicator, method = "radix")
  x[rows, , drop = FALSE]
}

cross_expect_same_output <- function(x, y, ignore_input_mode = FALSE) {
  x <- cross_canonical_output(x)
  y <- cross_canonical_output(y)
  if (isTRUE(ignore_input_mode)) {
    y$input_mode <- x$input_mode
  }

  expect_equal(y, x, tolerance = 1e-12)
  if ("V" %in% names(x)) {
    expect_identical(length(y$V), length(x$V))
    for (i in seq_along(x$V)) {
      expect_true(
        sitemix:::.sm_vcov_value_equal(y$V[[i]], x$V[[i]]),
        info = paste("covariance row", i)
      )
    }
  }
}

cross_restore_labels <- function(x, site_inverse, indicator_inverse) {
  x$site_id <- unname(site_inverse[as.character(x$site_id)])
  x$indicator <- unname(indicator_inverse[as.character(x$indicator)])
  if ("V" %in% names(x)) {
    x$V <- lapply(x$V, function(V) {
      matrix <- V$matrix
      V$site_id <- unname(site_inverse[as.character(V$site_id)])
      V$indicator_order <- unname(indicator_inverse[V$indicator_order])
      dimnames(matrix) <- list(
        unname(indicator_inverse[rownames(matrix)]),
        unname(indicator_inverse[colnames(matrix)])
      )
      V$matrix <- matrix
      V
    })
  }
  x
}

test_that("A/B/C estimates are invariant to a nontrivial student-row permutation", {
  students <- cross_property_students()
  permutation <- c(13:24, seq(1L, 11L, by = 2L), seq(2L, 12L, by = 2L))
  reference <- cross_property_outputs(students)
  permuted <- cross_property_outputs(students[permutation, , drop = FALSE])

  for (family in names(reference)) {
    cross_expect_same_output(reference[[family]], permuted[[family]])
  }
})

test_that("A/B/C estimates are equivariant under bijective label changes", {
  students <- cross_property_students()
  reference <- cross_property_outputs(students)
  renamed <- students

  site_forward <- c(S1 = "North", S2 = "South")
  renamed$site_id <- unname(site_forward[renamed$site_id])
  names(renamed)[names(renamed) == "absent"] <- "missed"
  names(renamed)[match(c("a", "b", "c"), names(renamed))] <- c(
    "alpha", "beta", "gamma"
  )
  language_forward <- c(eng = "English", spa = "Spanish", oth = "Other")
  renamed$language <- factor(
    unname(language_forward[as.character(renamed$language)]),
    levels = unname(language_forward[c("eng", "spa", "oth")])
  )

  relabeled <- cross_property_outputs(
    renamed,
    binomial_indicator = "missed",
    multivariate_indicators = c("alpha", "beta", "gamma")
  )
  site_inverse <- c(North = "S1", South = "S2")
  indicator_inverse <- list(
    binomial = c(missed = "absent"),
    multivariate = c(alpha = "a", beta = "b", gamma = "c"),
    multinomial = c(English = "eng", Spanish = "spa", Other = "oth")
  )

  for (family in names(reference)) {
    restored <- cross_restore_labels(
      relabeled[[family]],
      site_inverse = site_inverse,
      indicator_inverse = indicator_inverse[[family]]
    )
    cross_expect_same_output(reference[[family]], restored)
  }
})

test_that("bounded random student rows equal independently counted A/B/C inputs", {
  rng_kind <- RNGkind()
  rng_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  rng_seed <- if (rng_seed_exists) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  on.exit({
    do.call(RNGkind, as.list(rng_kind))
    if (rng_seed_exists) {
      assign(".Random.seed", rng_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(
    20260713L,
    kind = "Mersenne-Twister",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )

  for (case_id in seq_len(6L)) {
    n <- sample(8:24, size = 1L)
    site_id <- sprintf("P%02d", case_id)
    year <- 2020L + case_id
    language <- c(
      "eng", "spa", "oth",
      sample(c("eng", "spa", "oth"), size = n - 3L, replace = TRUE)
    )
    language <- sample(language, size = n, replace = FALSE)
    students <- data.frame(
      site_id = rep(site_id, n),
      year = rep(year, n),
      absent = stats::rbinom(n, size = 1L, prob = 0.35),
      a = stats::rbinom(n, size = 1L, prob = 0.30),
      b = stats::rbinom(n, size = 1L, prob = 0.50),
      c = stats::rbinom(n, size = 1L, prob = 0.70),
      language = factor(language, levels = c("eng", "spa", "oth")),
      stringsAsFactors = FALSE
    )

    binomial_counts <- data.frame(
      site_id = site_id,
      year = year,
      n_jt = n,
      c_jt_absent = sum(students$absent)
    )
    multivariate_counts <- data.frame(
      site_id = site_id,
      year = year,
      n_jt = n,
      c_jt_a = sum(students$a),
      c_jt_b = sum(students$b),
      c_jt_c = sum(students$c),
      c_jt_a_b = sum(students$a * students$b),
      c_jt_a_c = sum(students$a * students$c),
      c_jt_b_c = sum(students$b * students$c)
    )
    category_counts <- tabulate(
      match(students$language, c("eng", "spa", "oth")),
      nbins = 3L
    )
    multinomial_counts <- data.frame(
      site_id = site_id,
      year = year,
      n_jt = n,
      c_jt_eng = category_counts[[1L]],
      c_jt_spa = category_counts[[2L]],
      c_jt_oth = category_counts[[3L]]
    )

    student_outputs <- cross_property_outputs(students)
    count_outputs <- list(
      binomial = sm_estimate_from_counts(
        binomial_counts,
        family = "binomial",
        indicator = "absent",
        vst = "none",
        boundary_method = "none",
        vjt = TRUE,
        min_n = 1L
      ),
      multivariate = sm_estimate_from_counts(
        multivariate_counts,
        family = "multivariate",
        indicators = c("a", "b", "c"),
        vst = "none",
        boundary_method = "none",
        vjt = TRUE,
        min_n = 1L
      ),
      multinomial = sm_estimate_from_counts(
        multinomial_counts,
        family = "multinomial",
        indicators = c("eng", "spa", "oth"),
        vst = "none",
        boundary_method = "none",
        vjt = TRUE,
        min_n = 1L
      )
    )

    for (family in names(student_outputs)) {
      cross_expect_same_output(
        student_outputs[[family]],
        count_outputs[[family]],
        ignore_input_mode = TRUE
      )
      for (V in count_outputs[[family]]$V) {
        cross_expect_psd(V)
      }
    }

    multinomial_V <- count_outputs$multinomial$V[[1L]]
    expect_equal(
      unname(rowSums(as.matrix(multinomial_V))),
      rep(0, 3L),
      tolerance = 1e-12
    )
    expect_identical(multinomial_V$matrix_rank, 2L)
    expect_identical(multinomial_V$positive_support, 3L)
  }
})

test_that("A/B/C public outputs share the locked default schema", {
  outputs <- list(
    binomial = sitemix::sm_estimate_from_counts(
      cross_binomial_counts(),
      family = "binomial",
      indicator = "absent",
      min_n = 2L,
      description = "schema A"
    ),
    multivariate = sitemix::sm_estimate_from_counts(
      cross_multivariate_counts(),
      family = "multivariate",
      indicators = c("snap", "frpm"),
      min_n = 2L,
      description = "schema B"
    ),
    multinomial = sitemix::sm_estimate_from_counts(
      cross_multinomial_counts(),
      family = "multinomial",
      indicators = c("eng", "spa", "oth"),
      min_n = 2L,
      description = "schema C"
    )
  )

  expect_equal(vapply(outputs, ncol, integer(1)), c(binomial = 18L, multivariate = 18L, multinomial = 18L))
  for (family in names(outputs)) {
    out <- outputs[[family]]
    expect_s3_class(out, "sitemix_estimates")
    expect_equal(names(out), cross_default_columns(), info = family)
    expect_equal(attr(out, "family"), family)
    expect_equal(out$input_mode, rep("counts_full_suff", nrow(out)), info = family)
    expect_equal(out$var_method, rep("arcsine_vst", nrow(out)), info = family)
    expect_equal(out$transform, out$estimate_scale, info = family)
    expect_false("description" %in% names(out), info = family)
    expect_false("V" %in% names(out), info = family)
    expect_false("K" %in% names(out), info = family)
    expect_false("vcov_method" %in% names(out), info = family)
    expect_true(validate.sitemix_estimates(out), info = family)
  }
})

test_that("A/B/C vjt outputs append only the contracted covariance columns", {
  outputs <- cross_v_outputs()

  expect_equal(names(outputs$binomial), c(cross_default_columns(), "V"))
  expect_equal(names(outputs$multivariate), c(cross_default_columns(), "V", "K"))
  expect_equal(names(outputs$multinomial), c(cross_default_columns(), "V", "K"))

  expect_false("K" %in% names(outputs$binomial))
  for (family in names(outputs)) {
    out <- outputs[[family]]
    expect_false("vcov_method" %in% names(out), info = family)
    expect_type(out$V, "list")
    expect_true(validate.sitemix_estimates(out), info = family)
  }

  expect_equal(outputs$multivariate$K, rep(2L, 2))
  expect_equal(outputs$multinomial$K, rep(3L, 3))
  expect_true(sitemix:::.sm_vcov_value_equal(outputs$multivariate$V[[1]], outputs$multivariate$V[[2]]))
  expect_true(sitemix:::.sm_vcov_value_equal(outputs$multinomial$V[[1]], outputs$multinomial$V[[2]]))
  expect_true(sitemix:::.sm_vcov_value_equal(outputs$multinomial$V[[2]], outputs$multinomial$V[[3]]))
})

test_that("row var_method and matrix vcov_method remain separate across scenarios", {
  outputs <- cross_v_outputs()

  expect_equal(outputs$binomial$var_method, rep("arcsine_vst", 2))
  expect_equal(outputs$multivariate$var_method, rep("arcsine_vst", 2))
  expect_equal(outputs$multinomial$var_method, rep("arcsine_vst", 3))

  expect_true(is.na(outputs$binomial$V[[1]]$vcov_method))
  expect_equal(outputs$multivariate$V[[1]]$vcov_method, "sur")
  expect_equal(outputs$multinomial$V[[1]]$vcov_method, "multinomial")

  expect_equal(outputs$binomial$V[[1]]$vcov_scale, "arcsine_delta")
  expect_equal(outputs$multivariate$V[[1]]$vcov_scale, "raw")
  expect_equal(outputs$multinomial$V[[1]]$vcov_scale, "raw")
  expect_false("vcov_method" %in% names(outputs$binomial))
  expect_false("vcov_method" %in% names(outputs$multivariate))
  expect_false("vcov_method" %in% names(outputs$multinomial))
})

test_that("A/B/C vjt matrices satisfy PSD, rank, and family-specific invariants", {
  outputs <- cross_v_outputs()

  for (out in outputs) {
    for (V in out$V) {
      expect_true(validate.sm_vcov(V))
      cross_expect_psd(V)
      expect_equal(V$matrix_rank, sitemix:::.sm_matrix_rank(as.matrix(V)))
    }
  }

  expect_equal(outputs$binomial$V[[1]]$family, "binomial")
  expect_equal(outputs$binomial$V[[1]]$matrix_rank, 1L)
  expect_equal(as.matrix(outputs$binomial$V[[1]])[1, 1], outputs$binomial$se[[1]]^2, tolerance = 1e-12)

  expect_equal(outputs$multivariate$V[[1]]$family, "multivariate")
  expect_equal(outputs$multivariate$V[[1]]$matrix_rank, 2L)
  expect_equal(outputs$multivariate$V[[1]]$indicator_order, c("snap", "frpm"))
  expect_equal(unname(diag(as.matrix(outputs$multivariate$V[[1]]))), outputs$multivariate$se_raw^2, tolerance = 1e-12)

  multinomial_v <- outputs$multinomial$V[[1]]
  multinomial_mat <- as.matrix(multinomial_v)
  expect_equal(multinomial_v$family, "multinomial")
  expect_equal(multinomial_v$matrix_rank, 2L)
  expect_equal(multinomial_v$positive_support, 3L)
  expect_equal(multinomial_v$indicator_order, c("eng", "spa", "oth"))
  expect_equal(as.vector(multinomial_mat %*% rep(1, 3)), rep(0, 3), tolerance = 1e-12)
  expect_lte(max(multinomial_mat[row(multinomial_mat) != col(multinomial_mat)]), 0)
  expect_equal(unname(diag(multinomial_mat)), outputs$multinomial$se_raw^2, tolerance = 1e-12)
})

test_that("binomial vjt supports raw zero-variance boundary matrices", {
  out <- sitemix::sm_estimate_from_counts(
    data.frame(site_id = "S1", year = 2024L, n_jt = 1L, c_jt_absent = 0L),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    boundary_method = "none",
    vjt = TRUE,
    min_n = 2L
  )

  expect_equal(out$theta_raw, 0)
  expect_equal(out$se, 0)
  expect_equal(out$var_method, "binomial")
  expect_equal(as.matrix(out$V[[1]]), matrix(0, 1, 1, dimnames = list("absent", "absent")))
  expect_equal(out$V[[1]]$matrix_rank, 0L)
  cross_expect_psd(out$V[[1]])
  expect_true(validate.sitemix_estimates(out))
})

test_that("public multivariate vjt rejects globally infeasible K=3 counts before covariance", {
  err <- rlang::catch_cnd(
    sitemix::sm_estimate_from_counts(
      data.frame(
        site_id = "S1",
        year = 2024L,
        n_jt = 10L,
        c_jt_a = 5L,
        c_jt_b = 5L,
        c_jt_c = 5L,
        c_jt_a_b = 0L,
        c_jt_a_c = 0L,
        c_jt_b_c = 0L
      ),
      family = "multivariate",
      indicators = c("a", "b", "c"),
      boundary_method = "none",
      vjt = TRUE
    )
  )
  expect_s3_class(err, "sitemix_error_input_indicator_count")
  expect_equal(err$triple_count_lower, 0L)
  expect_equal(err$triple_count_upper, -5L)
  expect_equal(err$joint_feasibility, "infeasible")
})

test_that("public multivariate vjt applies FPC to the full SUR matrix", {
  counts <- data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 4L,
    c_jt_a = 2L,
    c_jt_b = 2L,
    c_jt_c = 2L,
    c_jt_a_b = 2L,
    c_jt_a_c = 0L,
    c_jt_b_c = 0L
  )
  base <- sitemix::sm_estimate_from_counts(
    counts,
    family = "multivariate",
    indicators = c("a", "b", "c"),
    boundary_method = "none",
    vjt = TRUE,
    min_n = 2L
  )
  fpc <- sitemix::sm_estimate_from_counts(
    counts,
    family = "multivariate",
    indicators = c("a", "b", "c"),
    boundary_method = "none",
    fpc = 10,
    vjt = TRUE,
    min_n = 2L
  )

  expect_equal(as.matrix(fpc$V[[1]]), as.matrix(base$V[[1]]) * ((10 - 4) / (10 - 1)), tolerance = 1e-12)
  expect_equal(fpc$se_raw^2, unname(diag(as.matrix(fpc$V[[1]]))), tolerance = 1e-12)
  cross_expect_psd(fpc$V[[1]])
})

test_that("public multinomial vjt locks simplex rank at boundary support sizes", {
  zero_category <- sitemix::sm_estimate_from_counts(
    data.frame(site_id = "S1", year = 2024L, n_jt = 5L, c_jt_eng = 0L, c_jt_spa = 2L, c_jt_oth = 3L),
    family = "multinomial",
    indicators = c("eng", "spa", "oth"),
    vst = "none",
    vjt = TRUE,
    min_n = 2L
  )
  zero_mat <- as.matrix(zero_category$V[[1]])

  expect_equal(zero_category$V[[1]]$positive_support, 2L)
  expect_equal(zero_category$V[[1]]$matrix_rank, 1L)
  expect_equal(
    zero_category$var_method,
    c("wilson_boundary_surrogate", "binomial", "binomial")
  )
  expect_equal(
    zero_category$V[[1]]$scalar_correction_rule,
    c("wilson_boundary_surrogate", "none", "none")
  )
  expect_equal(zero_mat["eng", "eng"], 0)
  expect_gt(zero_category$se_raw[[1]], 0)
  expect_equal(as.vector(zero_mat %*% rep(1, 3)), rep(0, 3), tolerance = 1e-12)
  cross_expect_psd(zero_category$V[[1]])

  single_support <- sitemix::sm_estimate_from_counts(
    data.frame(site_id = "S1", year = 2024L, n_jt = 5L, c_jt_eng = 5L, c_jt_spa = 0L, c_jt_oth = 0L),
    family = "multinomial",
    indicators = c("eng", "spa", "oth"),
    vst = "none",
    vjt = TRUE,
    min_n = 2L
  )
  single_mat <- as.matrix(single_support$V[[1]])

  expect_equal(single_support$V[[1]]$positive_support, 1L)
  expect_equal(single_support$V[[1]]$matrix_rank, 0L)
  expect_equal(single_mat, matrix(0, 3, 3, dimnames = list(c("eng", "spa", "oth"), c("eng", "spa", "oth"))))
  expect_true(all(single_support$se_raw > 0))
  cross_expect_psd(single_support$V[[1]])
})

test_that("public multinomial vjt supports K = 2 simplex output", {
  out <- sitemix::sm_estimate_from_counts(
    data.frame(site_id = "S1", year = 2024L, n_jt = 8L, c_jt_a = 3L, c_jt_b = 5L),
    family = "multinomial",
    indicators = c("a", "b"),
    vjt = TRUE,
    min_n = 2L
  )
  mat <- as.matrix(out$V[[1]])

  expect_equal(out$indicator, c("a", "b"))
  expect_equal(out$K, c(2L, 2L))
  expect_equal(out$V[[1]]$positive_support, 2L)
  expect_equal(out$V[[1]]$matrix_rank, 1L)
  expect_equal(as.vector(mat %*% c(1, 1)), c(0, 0), tolerance = 1e-12)
  expect_true(sitemix:::.sm_vcov_value_equal(out$V[[1]], out$V[[2]]))
  cross_expect_psd(out$V[[1]])
})

test_that("row var_method never contains matrix-level labels across (family, vst, boundary_method, vjt)", {
  forbidden <- c("sur", "multinomial", "working_independence")

  bin_fixture <- data.frame(
    site_id = c("A", "B"),
    year = c(2024L, 2024L),
    n_jt = c(20L, 30L),
    c_jt_absent = c(7L, 12L)
  )
  mv_fixture <- data.frame(
    site_id = "A", year = 2024L,
    n_jt = 30L,
    c_jt_a = 10L, c_jt_b = 12L, c_jt_a_b = 5L
  )
  mn_fixture <- data.frame(
    site_id = "A", year = 2024L,
    n_jt = 30L,
    c_jt_x = 10L, c_jt_y = 12L, c_jt_z = 8L
  )

  # Binomial: all vst + boundary + vjt combinations are valid on interior data.
  for (vst in c("arcsine", "logit", "none")) {
    for (bm in c("wilson_floor", "agresti_coull", "none")) {
      for (vjt in c(TRUE, FALSE)) {
        out <- sm_estimate(
          bin_fixture, family = "binomial", indicator = "absent",
          from_counts = TRUE, vst = vst, boundary_method = bm, vjt = vjt
        )
        expect_false(
          any(out$var_method %in% forbidden),
          info = sprintf("binomial / vst=%s / boundary=%s / vjt=%s", vst, bm, vjt)
        )
      }
    }
  }

  # Multivariate: AC + vjt=TRUE errors per contract; keep AC restricted to vjt=FALSE.
  # Use arcsine and none only for the matrix path; pair with WF/none boundary.
  for (vst in c("arcsine", "none")) {
    for (bm in c("wilson_floor", "none")) {
      for (vjt in c(TRUE, FALSE)) {
        out <- sm_estimate(
          mv_fixture, family = "multivariate", indicators = c("a", "b"),
          from_counts = TRUE, vst = vst, boundary_method = bm, vjt = vjt
        )
        expect_false(
          any(out$var_method %in% forbidden),
          info = sprintf("multivariate / vst=%s / boundary=%s / vjt=%s", vst, bm, vjt)
        )
      }
    }
  }

  # Multinomial: same constraints.
  for (vst in c("arcsine", "none")) {
    for (bm in c("wilson_floor", "none")) {
      for (vjt in c(TRUE, FALSE)) {
        out <- sm_estimate(
          mn_fixture, family = "multinomial", indicators = c("x", "y", "z"),
          from_counts = TRUE, vst = vst, boundary_method = bm, vjt = vjt
        )
        expect_false(
          any(out$var_method %in% forbidden),
          info = sprintf("multinomial / vst=%s / boundary=%s / vjt=%s", vst, bm, vjt)
        )
      }
    }
  }
})

test_that("multinomial V matrices use the analytic positive-support rank", {
  # Site A: all 3 categories nonzero (positive_support=3, rank=2).
  # Site B: 2 of 3 categories nonzero (positive_support=2, rank=1).
  counts <- data.frame(
    site_id = c("A", "B"),
    year = c(2024L, 2024L),
    n_jt = c(20L, 20L),
    c_jt_x = c(8L, 10L),
    c_jt_y = c(7L, 10L),
    c_jt_z = c(5L, 0L)
  )

  out <- sm_estimate(
    counts, family = "multinomial", indicators = c("x", "y", "z"),
    from_counts = TRUE, vjt = TRUE,
    min_n = 1L
  )

  for (i in seq_along(out$V)) {
    V <- out$V[[i]]
    expect_equal(
      V$matrix_rank, V$positive_support - 1L,
      label = sprintf("row %d (site=%s): rank=%d vs positive_support-1=%d",
                      i, out$site_id[[i]], V$matrix_rank, V$positive_support - 1L)
    )
  }
})

test_that("B/C shared category rows preserve group order, FPC, and exact V repeats", {
  multivariate_counts <- data.frame(
    site_id = c("B", "A"),
    year = c(2025L, 2024L),
    n_jt = c(8L, 6L),
    c_jt_c = c(4L, 2L),
    c_jt_a = c(3L, 3L),
    c_jt_b = c(5L, 3L),
    c_jt_c_a = c(2L, 2L),
    c_jt_c_b = c(2L, 1L),
    c_jt_a_b = c(2L, 2L)
  )
  multinomial_counts <- data.frame(
    site_id = c("B", "A"),
    year = c(2025L, 2024L),
    n_jt = c(8L, 10L),
    c_jt_z = c(2L, 4L),
    c_jt_x = c(3L, 3L),
    c_jt_y = c(3L, 3L)
  )
  specs <- list(
    multivariate = list(
      counts = multivariate_counts,
      indicator_order = c("c", "a", "b"),
      fpc = c(20, 12),
      expected_n = rep(c(6L, 8L), each = 3L),
      expected_N = rep(c(12, 20), each = 3L)
    ),
    multinomial = list(
      counts = multinomial_counts,
      indicator_order = c("z", "x", "y"),
      fpc = c(20, 25),
      expected_n = rep(c(10L, 8L), each = 3L),
      expected_N = rep(c(25, 20), each = 3L)
    )
  )
  fpc_columns <- c(
    "population_size",
    "sampling_fraction",
    "fpc_variance_multiplier",
    "fpc_se_multiplier",
    "variance_multiplier_applied",
    "se_multiplier_applied",
    "sampling_design",
    "variance_rule"
  )

  for (family in names(specs)) {
    spec <- specs[[family]]
    out <- sitemix::sm_estimate_from_counts(
      spec$counts,
      family = family,
      indicators = spec$indicator_order,
      vst = "none",
      boundary_method = "none",
      vjt = TRUE,
      min_n = 1L,
      fpc = spec$fpc
    )
    expected_fpc_variance <- (spec$expected_N - spec$expected_n) / (spec$expected_N - 1)
    expected_fpc_se <- sqrt(expected_fpc_variance)

    expect_identical(class(out), c("sitemix_estimates", "tbl_df", "tbl", "data.frame"))
    expect_identical(attr(out, "family", exact = TRUE), family)
    expect_identical(attr(out, "sitemix_role", exact = TRUE), "summary_uncertainty")
    expect_identical(names(out), c(cross_default_columns(), "V", "K", fpc_columns))
    expect_true(validate.sitemix_estimates(out))
    expect_identical(out$site_id, rep(c("A", "B"), each = 3L))
    expect_identical(out$year, rep(c(2024L, 2025L), each = 3L))
    expect_identical(out$indicator, rep(spec$indicator_order, 2L))
    expect_identical(out$n, spec$expected_n)
    expect_identical(out$K, rep(3L, 6L))
    expect_equal(out$population_size, spec$expected_N)
    expect_equal(out$sampling_fraction, spec$expected_n / spec$expected_N)
    expect_equal(out$fpc_variance_multiplier, expected_fpc_variance)
    expect_equal(out$fpc_se_multiplier, expected_fpc_se)
    expect_equal(out$variance_multiplier_applied, expected_fpc_variance)
    expect_equal(out$se_multiplier_applied, expected_fpc_se)
    expect_identical(out$sampling_design, rep("SRSWOR", 6L))
    expect_identical(out$variance_rule, rep("plugin", 6L))

    for (group_rows in list(1:3, 4:6)) {
      expect_identical(out$V[[group_rows[[1L]]]], out$V[[group_rows[[2L]]]])
      expect_identical(out$V[[group_rows[[1L]]]], out$V[[group_rows[[3L]]]])
      expect_equal(out$V[[group_rows[[1L]]]]$population_size, out$population_size[[group_rows[[1L]]]])
    }
    expect_false(identical(out$V[[1L]], out$V[[4L]]))
  }
})
