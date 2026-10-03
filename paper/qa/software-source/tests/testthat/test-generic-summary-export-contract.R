generic_export_contract_table <- function() {
  utils::read.csv(
    testthat::test_path(
      "_data", "api", "generic-summary-export-contract.csv"
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

generic_export_provenance <- function(x, diagnostics) {
  attribute_names <- c(
    "description", "family", "sitemix_role", "aggregate_case",
    "sampling_relation", "denominator_pattern", "d1_regime",
    "d1_regime_by_group", "suppression", "smoothing"
  )
  values <- lapply(attribute_names, function(name) attr(x, name, exact = TRUE))
  names(values) <- attribute_names
  values$diagnostic_severity <- diagnostics$diag_severity[[1L]]
  values
}

generic_export_scalar <- function(x) {
  diagnostics <- sm_diagnose(x, verbose = FALSE)
  if (identical(diagnostics$diag_severity[[1L]], "error")) {
    stop("diagnostic error blocks scalar export", call. = FALSE)
  }

  rows <- as.data.frame(x)
  required <- c(
    "site_id", "year", "indicator", "theta_raw", "theta_hat", "se_raw",
    "se", "n", "estimate_scale", "var_method", "flag_small_n",
    "flag_zero_cell", "flag_suppressed", "flag_below_accountability"
  )
  stopifnot(all(required %in% names(rows)))
  if (any(rows$flag_suppressed) && !"estimate_status" %in% names(rows)) {
    stop("suppressed rows require estimate_status", call. = FALSE)
  }

  status <- if ("estimate_status" %in% names(rows)) {
    rows$estimate_status
  } else {
    rep("identified", nrow(rows))
  }
  stopifnot(all(status %in% c(
    "identified", "suppressed_missing", "suppression_sensitivity"
  )))

  census <- rep(FALSE, nrow(rows))
  census_fields <- c("sampling_design", "population_size")
  if (all(census_fields %in% names(rows))) {
    census <- status == "identified" &
      rows$sampling_design == "SRSWOR" &
      is.finite(rows$population_size) &
      rows$population_size == as.numeric(rows$n) &
      is.finite(rows$se_raw) & rows$se_raw == 0 &
      is.finite(rows$se) & rows$se == 0
  }
  finite_pair <- is.finite(rows$theta_hat) & is.finite(rows$se)
  role <- rep("identified_positive_se", nrow(rows))
  role[status == "suppressed_missing"] <- "suppressed_missing"
  role[status == "suppression_sensitivity"] <- "suppression_sensitivity"
  role[status == "identified" & census] <- "census_zero_uncertainty"
  role[status == "identified" & !finite_pair] <- "invalid_nonfinite_scalar"
  role[
    status == "identified" & finite_pair & rows$se <= 0 & !census
  ] <- "invalid_nonpositive_se"

  eligible <- role == "identified_positive_se"
  audit <- rows[setdiff(names(rows), c("V", "K"))]
  audit$export_role <- role
  audit$inverse_variance_eligible <- eligible
  audit$inverse_variance <- rep(NA_real_, nrow(audit))
  audit$inverse_variance[eligible] <- 1 / rows$se[eligible]^2

  minimal <- c(
    "site_id", "year", "indicator", "theta_hat", "se",
    "estimate_scale", "var_method"
  )
  analysis_input <- audit[eligible, minimal, drop = FALSE]
  analysis_input$inverse_variance <- audit$inverse_variance[eligible]

  list(
    audit = audit,
    analysis_input = analysis_input,
    excluded_audit = audit[!eligible, , drop = FALSE],
    provenance = generic_export_provenance(x, diagnostics)
  )
}

generic_export_grouped_v_from_rows <- function(rows) {
  required <- c("site_id", "year", "indicator", "V")
  if (!all(required %in% names(rows))) {
    stop("grouped covariance export requires V and row keys", call. = FALSE)
  }
  if (anyDuplicated(rows[c("site_id", "year", "indicator")])) {
    stop("duplicate row coordinate", call. = FALSE)
  }

  keys <- unique(rows[c("site_id", "year")])
  groups <- keys
  groups$indicator_order <- I(vector("list", nrow(keys)))
  groups$row_keys <- I(vector("list", nrow(keys)))
  groups$V <- I(vector("list", nrow(keys)))
  groups$estimate_scale <- rep(NA_character_, nrow(keys))
  groups$vcov_scale <- rep(NA_character_, nrow(keys))
  groups$vcov_method <- rep(NA_character_, nrow(keys))
  groups$diag_contract <- rep(NA_character_, nrow(keys))
  groups$combine_without_transform <- rep(NA, nrow(keys))

  for (i in seq_len(nrow(keys))) {
    idx <- which(
      rows$site_id == keys$site_id[[i]] & rows$year == keys$year[[i]]
    )
    block <- rows[idx, , drop = FALSE]
    first <- block$V[[1L]]
    if (!all(vapply(block$V, identical, logical(1), first))) {
      stop("V must be repeated exactly within each group", call. = FALSE)
    }
    if (!identical(first$site_id, as.character(keys$site_id[[i]])) ||
        !identical(first$year, as.integer(keys$year[[i]]))) {
      stop("matrix key does not match row group", call. = FALSE)
    }

    order <- first$indicator_order
    coordinate <- match(order, block$indicator)
    if (anyNA(coordinate) || anyDuplicated(block$indicator) ||
        length(coordinate) != nrow(block)) {
      stop("incomplete or duplicate matrix coordinate", call. = FALSE)
    }
    aligned <- block[coordinate, , drop = FALSE]
    matrix <- as.matrix(first)
    if (!identical(aligned$indicator, order) ||
        !identical(rownames(matrix), order) ||
        !identical(colnames(matrix), order) ||
        nrow(matrix) != length(order) || ncol(matrix) != length(order)) {
      stop("matrix coordinates do not align", call. = FALSE)
    }
    if ("K" %in% names(block) && !all(block$K == length(order))) {
      stop("K does not align with indicator_order", call. = FALSE)
    }

    groups$indicator_order[[i]] <- order
    groups$row_keys[[i]] <- aligned[c("site_id", "year", "indicator")]
    groups$V[[i]] <- first
    groups$estimate_scale[[i]] <- first$estimate_scale
    groups$vcov_scale[[i]] <- first$vcov_scale
    groups$vcov_method[[i]] <- first$vcov_method
    groups$diag_contract[[i]] <- first$diag_contract
    groups$combine_without_transform[[i]] <- all(
      aligned$estimate_scale == first$estimate_scale
    ) && all(c(
      none = "raw", arcsine = "arcsine_delta",
      arcsine_anscombe = "arcsine_delta", logit = "logit_delta"
    )[aligned$estimate_scale] == first$vcov_scale)
  }
  groups
}

generic_export_grouped_v <- function(x) {
  summary <- sm_diagnose(x, verbose = FALSE)
  matrices <- sm_diagnose(x, level = "vcov", verbose = FALSE)
  if (identical(summary$diag_severity[[1L]], "error") ||
      any(matrices$diag_severity == "error")) {
    stop("diagnostic error blocks grouped covariance export", call. = FALSE)
  }
  list(
    rows = as.data.frame(x),
    groups = generic_export_grouped_v_from_rows(as.data.frame(x)),
    provenance = generic_export_provenance(x, summary)
  )
}

generic_export_multivariate <- function(vst = "none") {
  sm_estimate_from_counts(
    data.frame(
      site_id = c("B", "A"), year = c(2025L, 2025L),
      n_jt = c(30L, 20L), c_jt_a = c(12L, 8L),
      c_jt_b = c(15L, 12L), c_jt_a_b = c(7L, 5L)
    ),
    family = "multivariate", indicators = c("a", "b"),
    vst = vst, vjt = TRUE, min_n = 2L, accountability_n = 2L
  )
}

generic_export_suppression <- function(mode = "drop", hidden = FALSE) {
  sm_estimate_from_aggregates(
    data.frame(
      site_id = c("S", "I"), year = 2025L, indicator = "p",
      c_jt = c(NA_integer_, 5L),
      n_jt = c(if (hidden) NA_integer_ else 8L, 20L),
      suppression_flag = c(TRUE, FALSE)
    ),
    family = "binomial", indicator = "p", suppression = mode,
    suppression_sensitivity_acknowledge = identical(mode, "upper_bound"),
    suppressed_n_strategy = if (hidden) "worst_case_bound" else "observed_n",
    suppressed_n_bound = if (hidden) 2L else NULL,
    min_n = 2L
  )
}

test_that("the generic export fixture locks canonical columns and metadata", {
  contract <- generic_export_contract_table()
  expect_identical(nrow(contract), 45L)
  expect_identical(anyDuplicated(contract[c("layer", "field")]), 0L)
  expect_setequal(
    contract$field[contract$layer == "row_key"],
    c("site_id", "year", "indicator")
  )
  expect_setequal(
    contract$field[contract$layer == "scalar_transformed"],
    c("theta_hat", "se", "estimate_scale", "var_method")
  )
  expect_setequal(
    contract$field[contract$layer == "joint"], c("V", "K")
  )
  expect_identical(
    contract$requirement[contract$layer == "joint" & contract$field == "V"],
    "conditional_vjt"
  )
  expect_identical(
    contract$requirement[contract$layer == "joint" & contract$field == "K"],
    "conditional_multivariate_joint"
  )
  expect_setequal(
    contract$field[contract$layer == "V_metadata"],
    c(
      "site_id", "year", "indicator_order", "estimate_scale",
      "vcov_scale", "vcov_method", "diag_contract"
    )
  )
  expect_setequal(
    contract$field[contract$layer == "object_attribute"],
    c(
      "description", "family", "sitemix_role", "aggregate_case",
      "sampling_relation", "denominator_pattern", "d1_regime",
      "d1_regime_by_group", "suppression", "smoothing"
    )
  )
  expect_false(any(grepl("adapter|consumer|ebrecipe", contract$field)))
})

test_that("base scalar exchange preserves all rows and builds finite positive-SE input", {
  x <- sm_estimate_from_counts(
    data.frame(
      site_id = c("B", "A"), year = 2025L,
      n_jt = c(40L, 20L), c_jt_p = c(20L, 5L)
    ),
    family = "binomial", indicator = "p", min_n = 2L,
    accountability_n = 2L
  )
  out <- generic_export_scalar(x)

  expect_s3_class(out$audit, "data.frame")
  expect_false(inherits(out$audit, "sitemix_estimates"))
  expect_identical(nrow(out$audit), nrow(x))
  expect_identical(out$audit$export_role, rep("identified_positive_se", 2L))
  expect_true(all(out$audit$inverse_variance_eligible))
  expect_equal(out$analysis_input$inverse_variance, 1 / x$se^2)
  expect_identical(nrow(out$excluded_audit), 0L)
  expect_identical(out$provenance$family, "binomial")
  expect_identical(out$provenance$sitemix_role, "summary_uncertainty")
  expect_identical(out$provenance$diagnostic_severity, "ok")
  expect_setequal(
    names(out$provenance),
    c(
      "description", "family", "sitemix_role", "aggregate_case",
      "sampling_relation", "denominator_pattern", "d1_regime",
      "d1_regime_by_group", "suppression", "smoothing",
      "diagnostic_severity"
    )
  )
})

test_that("object-attribute sidecar preserves aggregate D1 provenance", {
  x <- withCallingHandlers(
    sm_estimate_from_aggregates(
      data.frame(
        site_id = "D1", year = 2025L,
        c_jt_a = 12L, c_jt_b = 20L, n_jt = 100L
      ),
      family = "multivariate", sampling_relation = "same_units",
      vjt = TRUE, min_n = 1L
    ),
    sitemix_warning_working_independence_default = function(w) {
      invokeRestart("muffleWarning")
    }
  )
  out <- generic_export_grouped_v(x)

  expect_identical(out$provenance$family, "multivariate")
  expect_identical(out$provenance$sitemix_role, "summary_uncertainty")
  expect_identical(out$provenance$aggregate_case, "D1")
  expect_identical(out$provenance$sampling_relation, "same_units")
  expect_identical(out$provenance$denominator_pattern, "common")
  expect_identical(out$provenance$d1_regime, "D1a")
  expect_s3_class(out$provenance$d1_regime_by_group, "data.frame")
  expect_identical(out$provenance$d1_regime_by_group$d1_regime, "D1a")
  expect_identical(out$provenance$suppression$sensitivity_role, "none")
  expect_null(out$provenance$smoothing)
})

test_that("suppressed and sensitivity rows remain explicit and never become weights", {
  drop <- generic_export_scalar(generic_export_suppression("drop"))
  sensitivity <- generic_export_scalar(generic_export_suppression("upper_bound"))
  hidden <- generic_export_scalar(generic_export_suppression(
    "upper_bound", hidden = TRUE
  ))

  expect_identical(nrow(drop$audit), 2L)
  expect_setequal(drop$audit$export_role, c(
    "identified_positive_se", "suppressed_missing"
  ))
  expect_identical(nrow(drop$analysis_input), 1L)
  expect_identical(nrow(drop$excluded_audit), 1L)
  expect_true(is.na(drop$excluded_audit$inverse_variance))

  sensitivity_row <- sensitivity$audit$export_role ==
    "suppression_sensitivity"
  expect_true(is.finite(sensitivity$audit$sensitivity_var[sensitivity_row]))
  expect_true(is.na(sensitivity$audit$theta_hat[sensitivity_row]))
  expect_true(is.na(sensitivity$audit$se[sensitivity_row]))
  expect_false(sensitivity$audit$inverse_variance_eligible[sensitivity_row])
  expect_true(is.na(sensitivity$audit$inverse_variance[sensitivity_row]))
  expect_identical(nrow(sensitivity$analysis_input), 1L)

  hidden_row <- hidden$audit$export_role == "suppression_sensitivity"
  expect_true(is.na(hidden$audit$sensitivity_var[hidden_row]))
  expect_false(hidden$audit$inverse_variance_eligible[hidden_row])
  expect_identical(
    hidden$provenance$suppression$sensitivity_role,
    "nonidentified_variance_sensitivity"
  )
})

test_that("census zero uncertainty is explicit and non-census zero blocks export", {
  census <- sm_estimate_from_counts(
    data.frame(site_id = "C", year = 2025L, n_jt = 10L, c_jt_p = 5L),
    family = "binomial", indicator = "p", fpc = 10L,
    vjt = TRUE, min_n = 2L
  )
  out <- generic_export_scalar(census)
  expect_identical(out$audit$export_role, "census_zero_uncertainty")
  expect_false(out$audit$inverse_variance_eligible)
  expect_true(is.na(out$audit$inverse_variance))
  expect_identical(nrow(out$analysis_input), 0L)
  expect_identical(nrow(out$excluded_audit), 1L)
  expect_identical(out$provenance$diagnostic_severity, "note")

  boundary <- sm_estimate_from_counts(
    data.frame(site_id = "Z", year = 2025L, n_jt = 10L, c_jt_p = 0L),
    family = "binomial", indicator = "p", vst = "none",
    boundary_method = "none", min_n = 2L
  )
  expect_error(
    generic_export_scalar(boundary),
    "diagnostic error blocks scalar export"
  )
})

test_that("as.data.frame is a non-validating boundary and no smoother is substituted", {
  x <- sm_estimate_from_counts(
    data.frame(site_id = "A", year = 2025L, n_jt = 20L, c_jt_p = 8L),
    family = "binomial", indicator = "p", min_n = 2L
  )
  x$se_smoothed <- x$se * 2
  x$var_method_smoothed <- paste0(x$var_method, " + gvf_smooth_loglinear")
  expect_true(validate.sitemix_estimates(x))
  out <- generic_export_scalar(x)
  expect_identical(out$analysis_input$se, x$se)
  expect_false("se_smoothed" %in% names(out$analysis_input))
  expect_identical(out$audit$se_smoothed, x$se_smoothed)

  tampered <- x
  tampered$se[[1L]] <- -1
  expect_no_error(plain <- as.data.frame(tampered))
  expect_s3_class(plain, "data.frame")
  expect_identical(plain$se[[1L]], -1)
  expect_error(generic_export_scalar(tampered), class = "sitemix_error")
})

test_that("full and partial class behavior keeps scalar and joint intent explicit", {
  x <- generic_export_multivariate()
  plain <- as.data.frame(x)
  tibble <- tibble::as_tibble(x)
  expect_s3_class(plain, "data.frame")
  expect_false(inherits(plain, "sitemix_estimates"))
  expect_s3_class(tibble, "tbl_df")
  expect_false(inherits(tibble, "sitemix_estimates"))
  expect_identical(names(plain), names(x))
  expect_true(all(vapply(plain$V, inherits, logical(1), "sm_vcov")))

  expect_error(
    x[x$indicator == "a", , drop = FALSE],
    class = "sitemix_error_estimate_vcov_invariant"
  )
  scalar_columns <- c(
    "site_id", "year", "indicator", "theta_hat", "se",
    "estimate_scale", "var_method"
  )
  scalar <- x[, scalar_columns, drop = FALSE]
  expect_false(inherits(scalar, "sitemix_estimates"))
  expect_false("V" %in% names(scalar))
  expect_false("K" %in% names(scalar))
  expect_null(attr(scalar, "sitemix_role", exact = TRUE))

  full_group <- x[x$site_id == "A", , drop = FALSE]
  expect_s3_class(full_group, "sitemix_estimates")
  expect_true(validate.sitemix_estimates(full_group))
})

test_that("grouped V export aligns tuple keys and indicator coordinates", {
  x <- generic_export_multivariate()
  out <- generic_export_grouped_v(x)
  groups <- out$groups

  expect_identical(nrow(out$rows), nrow(x))
  expect_identical(nrow(groups), 2L)
  expect_identical(names(groups)[1:2], c("site_id", "year"))
  expect_true(all(groups$combine_without_transform))
  expect_identical(groups$vcov_scale, rep("raw", 2L))
  expect_identical(groups$vcov_method, rep("sur", 2L))
  expect_identical(groups$diag_contract, rep("row_se_raw_squared", 2L))
  for (i in seq_len(nrow(groups))) {
    order <- groups$indicator_order[[i]]
    keys <- groups$row_keys[[i]]
    V <- groups$V[[i]]
    expect_identical(keys$indicator, order)
    expect_identical(rownames(as.matrix(V)), order)
    expect_identical(colnames(as.matrix(V)), order)
    expect_identical(V$site_id, groups$site_id[[i]])
    expect_identical(V$year, groups$year[[i]])
  }
})

test_that("scalar vjt exports a one-by-one V without requiring K", {
  x <- sm_estimate_from_counts(
    data.frame(site_id = "A", year = 2025L, n_jt = 20L, c_jt_p = 8L),
    family = "binomial", indicator = "p", vjt = TRUE, min_n = 2L
  )
  expect_true("V" %in% names(x))
  expect_false("K" %in% names(x))

  out <- generic_export_grouped_v(x)
  expect_identical(nrow(out$groups), 1L)
  expect_identical(out$groups$indicator_order[[1L]], "p")
  expect_identical(dim(as.matrix(out$groups$V[[1L]])), c(1L, 1L))
  expect_identical(rownames(as.matrix(out$groups$V[[1L]])), "p")
  expect_identical(colnames(as.matrix(out$groups$V[[1L]])), "p")
  expect_identical(out$groups$V[[1L]]$site_id, "A")
  expect_identical(out$groups$V[[1L]]$year, 2025L)
})

test_that("grouped V matching ignores incidental plain-row order and fails partial", {
  x <- generic_export_multivariate()
  plain <- as.data.frame(x)
  reversed <- plain[rev(seq_len(nrow(plain))), , drop = FALSE]
  groups <- generic_export_grouped_v_from_rows(reversed)
  expect_identical(nrow(groups), 2L)
  expect_true(all(vapply(
    groups$row_keys,
    function(key) identical(key$indicator, c("a", "b")),
    logical(1)
  )))

  partial <- plain[plain$indicator == "a", , drop = FALSE]
  expect_error(
    generic_export_grouped_v_from_rows(partial),
    "incomplete or duplicate matrix coordinate"
  )
  duplicate <- rbind(plain, plain[1L, , drop = FALSE])
  expect_error(
    generic_export_grouped_v_from_rows(duplicate),
    "duplicate row coordinate"
  )
  inconsistent <- plain
  inconsistent$V[[2L]] <- inconsistent$V[[3L]]
  expect_error(
    generic_export_grouped_v_from_rows(inconsistent),
    "V must be repeated exactly"
  )
})

test_that("scale mismatch is transported and typed empty exports remain stable", {
  transformed <- generic_export_grouped_v(generic_export_multivariate("arcsine"))
  expect_false(any(transformed$groups$combine_without_transform))
  expect_identical(transformed$groups$estimate_scale, rep("arcsine", 2L))
  expect_identical(transformed$groups$vcov_scale, rep("raw", 2L))
  expect_identical(
    transformed$provenance$diagnostic_severity, "warning"
  )

  empty_scalar <- generic_export_scalar(
    sm_estimate_from_counts(
      data.frame(site_id = "E", year = 2025L, n_jt = 20L, c_jt_p = 8L),
      family = "binomial", indicator = "p", min_n = 2L
    )[0, ]
  )
  expect_identical(nrow(empty_scalar$audit), 0L)
  expect_identical(nrow(empty_scalar$analysis_input), 0L)
  expect_type(empty_scalar$audit$export_role, "character")
  expect_type(empty_scalar$audit$inverse_variance_eligible, "logical")
  expect_type(empty_scalar$audit$inverse_variance, "double")

  empty_joint <- generic_export_grouped_v(generic_export_multivariate()[0, ])
  expect_identical(nrow(empty_joint$rows), 0L)
  expect_identical(nrow(empty_joint$groups), 0L)
  expect_type(empty_joint$groups$indicator_order, "list")
  expect_type(empty_joint$groups$V, "list")
  expect_type(empty_joint$groups$combine_without_transform, "logical")
})

test_that("the base fixtures add no public helper or external consumer contract", {
  exports <- getNamespaceExports("sitemix")
  expect_false(any(c(
    "sm_as_summary_data", "sm_select_indicators", "as_eb_input"
  ) %in% exports))
  bodies <- paste(
    deparse(generic_export_scalar),
    deparse(generic_export_grouped_v_from_rows),
    deparse(generic_export_grouped_v),
    collapse = "\n"
  )
  expect_false(grepl("requireNamespace", bodies, fixed = TRUE))
  expect_false(grepl("formals(", bodies, fixed = TRUE))
  expect_false(grepl("ebrecipe|adapter_ready", bodies))
})
