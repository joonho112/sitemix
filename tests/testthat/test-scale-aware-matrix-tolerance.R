step310_labels <- c("a", "b", "c")

step310_named <- function(mat) {
  dimnames(mat) <- list(step310_labels, step310_labels)
  mat
}

step310_sur <- function(mat, matrix_rank = NULL) {
  sm_vcov(
    matrix = step310_named(mat),
    site_id = "scale",
    year = 2026L,
    indicator_order = step310_labels,
    family = "multivariate",
    vcov_method = "sur",
    estimate_scale = "none",
    vcov_scale = "raw",
    scalar_correction_rule = rep("none", 3),
    matrix_rank = matrix_rank,
    n_jt = 20L,
    n_eff = 20
  )
}

step310_multinomial <- function(mat, positive_support) {
  sm_vcov(
    matrix = step310_named(mat),
    site_id = "scale",
    year = 2026L,
    indicator_order = step310_labels,
    family = "multinomial",
    vcov_method = "multinomial",
    estimate_scale = "none",
    vcov_scale = "raw",
    matrix_boundary_rule = "simplex_preserve",
    scalar_correction_rule = rep("none", 3),
    positive_support = positive_support,
    n_jt = 20L,
    n_eff = 20
  )
}

test_that("matrix validity and rank decisions are invariant under rescaling", {
  scales <- c(1, 1e-6, 1e-12)
  psd <- diag(c(1, 1e-10, 0))
  indefinite <- diag(c(1, 0.5, -1e-4))

  ranks <- vapply(scales, function(scale) {
    V <- step310_sur(scale * psd)
    expect_true(validate.sm_vcov(V))
    expect_error(
      step310_sur(scale * indefinite),
      class = "sitemix_error_vcov_invariant"
    )
    V$matrix_rank
  }, integer(1))

  expect_identical(ranks, rep(2L, length(scales)))
})

test_that("PSD validity and numerical-rank tolerances are distinct oracles", {
  scales <- c(1, 1e-6, 1e-12)
  ratios <- vapply(scales, function(scale) {
    mat <- scale * diag(c(1, 1e-10, 0))
    psd_tol <- sitemix:::.sm_psd_tolerance(mat)
    rank_tol <- sitemix:::.sm_rank_tolerance(mat)
    expect_gt(psd_tol, rank_tol)
    expect_equal(
      psd_tol / scale,
      sitemix:::.sm_psd_tolerance(diag(c(1, 1e-10, 0))),
      tolerance = 1e-12
    )
    expect_equal(
      rank_tol / scale,
      sitemix:::.sm_rank_tolerance(diag(c(1, 1e-10, 0))),
      tolerance = 1e-12
    )
    psd_tol / rank_tol
  }, numeric(1))

  expect_equal(ratios, rep(8, length(scales)), tolerance = 1e-12)
})

test_that("symmetry decisions are scale invariant", {
  scales <- c(1, 1e-6, 1e-12)
  symmetric <- matrix(c(1, 0.2, 0.1, 0.2, 0.8, 0.05, 0.1, 0.05, 0.6), 3, 3)
  asymmetric <- symmetric
  asymmetric[1, 2] <- asymmetric[1, 2] + 1e-4

  for (scale in scales) {
    expect_true(validate.sm_vcov(step310_sur(scale * symmetric)))
    expect_error(
      step310_sur(scale * asymmetric),
      class = "sitemix_error_vcov_invariant"
    )
    expect_identical(sitemix:::.sm_matrix_rank(scale * asymmetric), NA_integer_)
  }
})

test_that("simplex decisions and analytic support rank are scale invariant", {
  scales <- c(1, 1e-6, 1e-12)
  p <- c(0.7, 0.3 - 1e-12, 1e-12)
  simplex <- diag(p) - tcrossprod(p)
  violated <- simplex + diag(c(1e-4, 0, 0))

  ranks <- vapply(scales, function(scale) {
    V <- step310_multinomial(scale * simplex, positive_support = 3L)
    expect_true(validate.sm_vcov(V))
    expect_error(
      step310_multinomial(scale * violated, positive_support = 3L),
      class = "sitemix_error_vcov_invariant"
    )
    V$matrix_rank
  }, integer(1))

  expect_identical(ranks, rep(2L, length(scales)))
})

test_that("zero matrices have finite machine-absolute tolerances", {
  zero <- matrix(0, 3, 3)

  expect_gt(sitemix:::.sm_psd_tolerance(zero), 0)
  expect_gt(sitemix:::.sm_rank_tolerance(zero), 0)
  expect_gt(sitemix:::.sm_symmetry_tolerance(zero), 0)
  expect_gt(sitemix:::.sm_simplex_tolerance(zero), 0)
  expect_identical(sitemix:::.sm_matrix_rank(zero), 0L)
  expect_true(validate.sm_vcov(step310_sur(zero)))
})

test_that("Frechet eigenvalue checks interpret psd_tol relatively", {
  scales <- c(1, 1e-6, 1e-12)
  nearly_psd <- diag(c(1, 0.5, -5e-9))
  indefinite <- diag(c(1, 0.5, -1e-4))

  for (scale in scales) {
    expect_true(sitemix:::.sm_frechet_is_psd(scale * nearly_psd, psd_tol = 1e-8))
    expect_false(sitemix:::.sm_frechet_is_psd(scale * indefinite, psd_tol = 1e-8))
    expect_silent(
      sitemix:::.sm_frechet_validate_psd_result(scale * nearly_psd, psd_tol = 1e-8)
    )
    expect_error(
      sitemix:::.sm_frechet_validate_psd_result(scale * indefinite, psd_tol = 1e-8),
      class = "sitemix_error_vcov_invariant"
    )
  }
})

test_that("Frechet Higham and shrink projection consumers are scale invariant", {
  scales <- c(1, 1e-6, 1e-12)
  psd_tol <- 1e-8

  for (method in c("higham", "shrink")) {
    projected <- lapply(scales, function(scale) {
      out <- sitemix:::.sm_frechet_from_vectors(
        p = c(0.5, 0.5, 0.5),
        s = sqrt(scale) * c(0.1, 0.1, 0.1),
        indicators = c("a", "b", "c"),
        psd_method = method,
        psd_tol = psd_tol,
        psd_max_iter = 100L,
        return_correlations = FALSE,
        nearpd_args = list()
      )

      # The unprojected lower corner has eigenvalues
      # scale * c(0.02, 0.02, -0.01), so it is materially indefinite.
      expect_equal(
        sort(eigen(out$V_lower_raw, symmetric = TRUE, only.values = TRUE)$values / scale),
        c(-0.01, 0.02, 0.02),
        tolerance = 1e-12
      )
      expect_false(out$psd_diagnostics$L_was_PSD)
      expect_gt(out$psd_diagnostics$L_iters, 0L)
      expect_equal(
        unname(diag(out$V_lower_psd)),
        rep(0.01 * scale, 3L),
        tolerance = 1e-12
      )

      eig <- eigen(out$V_lower_psd, symmetric = TRUE, only.values = TRUE)$values
      independent_tol <- psd_tol * max(abs(eig)) +
        nrow(out$V_lower_psd) * .Machine$double.xmin
      expect_gte(min(eig), -independent_tol)

      out
    })

    reference <- projected[[1L]]$V_lower_psd
    for (i in seq_along(scales)) {
      expect_equal(projected[[i]]$V_lower_psd / scales[[i]], reference, tolerance = 1e-12)
      expect_identical(
        projected[[i]]$psd_diagnostics$L_iters,
        projected[[1L]]$psd_diagnostics$L_iters
      )
    }
  }
})

test_that("Frechet fixed shrink rejects a materially indefinite corner at every scale", {
  scales <- c(1, 1e-6, 1e-12)

  for (scale in scales) {
    expect_error(
      sitemix:::.sm_frechet_from_vectors(
        p = c(0.5, 0.5, 0.5),
        s = sqrt(scale) * c(0.1, 0.1, 0.1),
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
  }
})
