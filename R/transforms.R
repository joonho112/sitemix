# Scale transform helpers ---------------------------------------------------

.sm_check_probability <- function(p, arg = "p", allow_boundary = TRUE) {
  if (!is.numeric(p) || anyNA(p) || any(!is.finite(p))) {
    .sm_abort_estimate(
      paste0("`", arg, "` must be finite numeric probabilities."),
      class = "sitemix_error_estimate_var_method",
      expected = if (allow_boundary) "finite values in [0, 1]" else "finite values in (0, 1)",
      actual = paste(class(p), collapse = "/"),
      fix = "Check count aggregation before applying scale transforms."
    )
  }

  ok <- if (allow_boundary) p >= 0 & p <= 1 else p > 0 & p < 1
  if (!all(ok)) {
    .sm_abort_estimate(
      paste0("`", arg, "` contains probabilities outside the supported range."),
      class = "sitemix_error_estimate_var_method",
      expected = if (allow_boundary) "[0, 1]" else "(0, 1)",
      actual = paste(range(p, na.rm = TRUE), collapse = " to "),
      fix = "Use a compatible boundary method before this transform."
    )
  }

  invisible(TRUE)
}

.sm_check_positive_n <- function(n, arg = "n") {
  if (!is.numeric(n) || anyNA(n) || any(!is.finite(n)) || any(n <= 0)) {
    .sm_abort_estimate(
      paste0("`", arg, "` must contain positive finite cell sizes."),
      class = "sitemix_error_estimate_zero_n",
      expected = "positive finite cell sizes",
      actual = paste(range(n, na.rm = TRUE), collapse = " to "),
      fix = "Drop or diagnose zero-size cells before estimating."
    )
  }

  invisible(TRUE)
}

.sm_check_counts <- function(C, n) {
  .sm_check_positive_n(n)
  if (!is.numeric(C) || anyNA(C) || any(!is.finite(C))) {
    .sm_abort_estimate(
      "`C` must contain finite numeric counts.",
      class = "sitemix_error_estimate_var_method",
      expected = "finite counts",
      actual = paste(class(C), collapse = "/"),
      fix = "Validate count columns before estimation."
    )
  }
  if (length(C) != length(n) && length(C) != 1L && length(n) != 1L) {
    .sm_abort_estimate(
      "`C` and `n` must have compatible lengths.",
      class = "sitemix_error_estimate_var_method",
      expected = "equal lengths, or one scalar input",
      actual = paste0("length(C) = ", length(C), ", length(n) = ", length(n)),
      fix = "Recycle explicitly before calling scalar helpers."
    )
  }

  common <- max(length(C), length(n))
  C <- rep(C, length.out = common)
  n <- rep(n, length.out = common)

  if (any(C < 0 | C > n)) {
    .sm_abort_estimate(
      "`C` must satisfy 0 <= C <= n.",
      class = "sitemix_error_estimate_var_method",
      expected = "0 <= C <= n",
      actual = paste0("range(C - n) = ", paste(range(C - n), collapse = " to ")),
      fix = "Check count aggregation and count-column naming."
    )
  }

  list(C = C, n = n)
}

.sm_transform_arcsine <- function(p) {
  .sm_check_probability(p, allow_boundary = TRUE)
  asin(sqrt(p))
}

.sm_backtransform_arcsine <- function(x) {
  sin(x)^2
}

.sm_anscombe_p <- function(C, n) {
  counts <- .sm_check_counts(C, n)
  (counts$C + 3 / 8) / (counts$n + 3 / 4)
}

.sm_anscombe_n_eff <- function(n) {
  .sm_check_positive_n(n)
  n + 1 / 2
}

.sm_transform_arcsine_anscombe <- function(C, n) {
  .sm_transform_arcsine(.sm_anscombe_p(C, n))
}

.sm_transform_logit <- function(p) {
  .sm_check_probability(p, allow_boundary = FALSE)
  log(p / (1 - p))
}

.sm_backtransform_logit <- function(x) {
  stats::plogis(x)
}

.sm_transform_none <- function(p) {
  .sm_check_probability(p, allow_boundary = TRUE)
  p
}

.sm_transform_probability <- function(
  theta_raw,
  n,
  C = NULL,
  vst = c("arcsine", "logit", "none"),
  anscombe = FALSE
) {
  if (!is.character(vst) || length(vst) != 1L || is.na(vst)) {
    .sm_abort_argument(
      "`vst` must be a single transform name.",
      class = "sitemix_error_invalid_vst",
      expected = c("arcsine", "logit", "none"),
      actual = vst,
      fix = "Use one of the locked transform names."
    )
  }
  if (!vst %in% c("arcsine", "logit", "none")) {
    .sm_abort_argument(
      "`vst` is not supported.",
      class = "sitemix_error_invalid_vst",
      expected = c("arcsine", "logit", "none"),
      actual = vst,
      fix = "Use one of the locked transform names."
    )
  }
  if (!is.logical(anscombe) || length(anscombe) != 1L || is.na(anscombe)) {
    .sm_abort_argument(
      "`anscombe` must be TRUE or FALSE.",
      class = "sitemix_error_invalid_anscombe",
      expected = c("TRUE", "FALSE"),
      actual = paste(class(anscombe), collapse = "/"),
      fix = "Pass a scalar logical value."
    )
  }
  if (anscombe && !identical(vst, "arcsine")) {
    .sm_abort_argument(
      "`anscombe = TRUE` requires `vst = \"arcsine\"`.",
      class = "sitemix_error_anscombe_requires_arcsine",
      expected = "vst = \"arcsine\"",
      actual = paste0("vst = \"", vst, "\""),
      fix = "Set `anscombe = FALSE` or use the arcsine transform."
    )
  }

  if (identical(vst, "arcsine") && anscombe) {
    if (is.null(C)) {
      .sm_abort_estimate(
        "Anscombe transform requires the count numerator `C`.",
        class = "sitemix_error_estimate_var_method",
        expected = "count numerator `C`",
        actual = "NULL",
        fix = "Pass counts to the transform helper."
      )
    }
    n_eff <- .sm_anscombe_n_eff(n)
    theta_hat <- .sm_transform_arcsine_anscombe(C, n)
    estimate_scale <- "arcsine_anscombe"
  } else if (identical(vst, "arcsine")) {
    .sm_check_positive_n(n)
    n_eff <- n
    theta_hat <- .sm_transform_arcsine(theta_raw)
    estimate_scale <- "arcsine"
  } else if (identical(vst, "logit")) {
    .sm_check_positive_n(n)
    n_eff <- n
    theta_hat <- .sm_transform_logit(theta_raw)
    estimate_scale <- "logit"
  } else {
    .sm_check_positive_n(n)
    n_eff <- n
    theta_hat <- .sm_transform_none(theta_raw)
    estimate_scale <- "none"
  }

  list(
    theta_hat = theta_hat,
    n_eff = n_eff,
    estimate_scale = estimate_scale,
    transform = estimate_scale
  )
}
