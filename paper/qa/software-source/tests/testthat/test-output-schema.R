sitemix_default_columns <- function() {
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

schema_count_data <- function() {
  data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    n_jt = c(4L, 40L),
    c_jt_absent = c(1L, 20L)
  )
}

schema_multivariate_count_data <- function() {
  data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 20L,
    c_jt_snap = 8L,
    c_jt_frpm = 12L,
    c_jt_snap_frpm = 6L
  )
}

schema_multinomial_count_data <- function() {
  data.frame(
    site_id = "S1",
    year = 2024L,
    n_jt = 20L,
    c_jt_eng = 7L,
    c_jt_spa = 8L,
    c_jt_oth = 5L
  )
}

schema_d0_aggregate_data <- function() {
  data.frame(
    site_id = c("S1", "S2"),
    year = c(2024L, 2024L),
    indicator = c("absent", "absent"),
    c_jt = c(3L, 12L),
    n_jt = c(10L, 40L),
    stringsAsFactors = FALSE
  )
}

schema_d1_aggregate_data <- function() {
  data.frame(
    site_id = c("S1", "S2"),
    year = c(2025L, 2025L),
    c_jt_snap = c(12L, 8L),
    c_jt_frpm = c(20L, 30L),
    n_jt = c(100L, 90L),
    stringsAsFactors = FALSE
  )
}

schema_smoothing_count_data <- function(n = 16L) {
  data.frame(
    site_id = sprintf("S%03d", seq_len(n)),
    year = rep(2025L, n),
    n_jt = as.integer(seq(12L, 12L + n - 1L)),
    c_jt_absent = as.integer(round(seq(12L, 12L + n - 1L) * 0.25)),
    stringsAsFactors = FALSE
  )
}

schema_suppression_aggregate_data <- function() {
  data.frame(
    site_id = c("S1", "S2", "S3"),
    year = c(2025L, 2025L, 2025L),
    indicator = c("absent", "absent", "absent"),
    c_jt = c(NA_integer_, 0L, 7L),
    n_jt = c(8L, 10L, 40L),
    stringsAsFactors = FALSE
  )
}

schema_quiet_working_independence <- function(expr) {
  withCallingHandlers(
    expr,
    sitemix_warning_working_independence_default = function(w) invokeRestart("muffleWarning")
  )
}

schema_value_set <- function(x) {
  values <- unique(x)
  if (is.logical(values)) {
    values <- ifelse(is.na(values), "NA", as.character(values))
  } else {
    values <- as.character(values)
    values[is.na(values)] <- "NA"
  }
  paste(sort(values), collapse = ", ")
}

schema_column_summary <- function(x) {
  data.frame(
    column = names(x),
    typeof = vapply(x, typeof, character(1)),
    class = vapply(x, function(col) paste(class(col), collapse = "/"), character(1)),
    stringsAsFactors = FALSE
  )
}

schema_default <- function(x, default) {
  if (is.null(x)) {
    default
  } else {
    x
  }
}

schema_attr_summary <- function(x) {
  attr_value <- function(name) {
    value <- attr(x, name, exact = TRUE)
    if (is.null(value)) {
      return("NULL")
    }
    paste(as.character(value), collapse = "|")
  }

  data.frame(
    rows = nrow(x),
    columns = ncol(x),
    object_class = paste(class(x), collapse = "/"),
    family = attr_value("family"),
    aggregate_case = attr_value("aggregate_case"),
    sampling_relation = attr_value("sampling_relation"),
    denominator_pattern = attr_value("denominator_pattern"),
    d1_regime = attr_value("d1_regime"),
    sitemix_role = attr_value("sitemix_role"),
    has_V = "V" %in% names(x),
    has_K = "K" %in% names(x),
    valid = validate.sitemix_estimates(x),
    stringsAsFactors = FALSE
  )
}

schema_lexicon_summary <- function(x) {
  lexicon_cols <- intersect(
    c(
      "indicator", "estimate_scale", "transform", "var_method",
      "input_mode", "framing", "flag_small_n", "flag_zero_cell",
      "flag_suppressed", "flag_below_accountability"
    ),
    names(x)
  )

  data.frame(
    column = lexicon_cols,
    values = vapply(x[lexicon_cols], schema_value_set, character(1)),
    stringsAsFactors = FALSE
  )
}

schema_vcov_summary <- function(x) {
  if (!"V" %in% names(x)) {
    return(NULL)
  }

  data.frame(
    row = seq_along(x$V),
    V_class = vapply(x$V, function(v) paste(class(v), collapse = "/"), character(1)),
    family = vapply(x$V, function(v) as.character(schema_default(v$family, NA_character_)), character(1)),
    vcov_method = vapply(x$V, function(v) as.character(schema_default(v$vcov_method, NA_character_)), character(1)),
    vcov_scale = vapply(x$V, function(v) as.character(schema_default(v$vcov_scale, NA_character_)), character(1)),
    matrix_dim = vapply(x$V, function(v) paste(dim(as.matrix(v)), collapse = "x"), character(1)),
    matrix_rank = vapply(x$V, function(v) as.integer(schema_default(v$matrix_rank, NA_integer_)), integer(1)),
    indicator_order = vapply(x$V, function(v) paste(schema_default(v$indicator_order, NA_character_), collapse = "|"), character(1)),
    stringsAsFactors = FALSE
  )
}

snapshot_schema <- function(label, x) {
  old_width <- getOption("width")
  options(width = 180)
  on.exit(options(width = old_width), add = TRUE)

  cat("## ", label, "\n", sep = "")
  cat("Attributes\n")
  attr_lines <- utils::capture.output(
    print(schema_attr_summary(x), row.names = FALSE, right = FALSE)
  )
  cat(sub("[[:space:]]+$", "", attr_lines), sep = "\n")
  cat("Columns\n")
  column_lines <- utils::capture.output(
    print(schema_column_summary(x), row.names = FALSE, right = FALSE)
  )
  cat(sub("[[:space:]]+$", "", column_lines), sep = "\n")
  cat("Lexicon\n")
  lexicon_lines <- utils::capture.output(
    print(schema_lexicon_summary(x), row.names = FALSE, right = FALSE)
  )
  cat(sub("[[:space:]]+$", "", lexicon_lines), sep = "\n")

  vcov <- schema_vcov_summary(x)
  if (!is.null(vcov)) {
    cat("V metadata\n")
    vcov_lines <- utils::capture.output(
      print(vcov, row.names = FALSE, right = FALSE)
    )
    cat(sub("[[:space:]]+$", "", vcov_lines), sep = "\n")
  }
}

test_that("public binomial output keeps the locked 18-column schema", {
  out <- sitemix::sm_estimate_from_counts(
    schema_count_data(),
    family = "binomial",
    indicator = "absent",
    min_n = 2L
  )

  expect_s3_class(out, "sitemix_estimates")
  expect_equal(names(out), sitemix_default_columns())
  expect_equal(attr(out, "family"), "binomial")
  expect_equal(attr(out, "sitemix_role"), "summary_uncertainty")
  expect_false("vcov_method" %in% names(out))
  expect_false("V" %in% names(out))
  expect_false("K" %in% names(out))

  expect_type(out$site_id, "character")
  expect_type(out$year, "integer")
  expect_type(out$indicator, "character")
  expect_type(out$theta_raw, "double")
  expect_type(out$theta_hat, "double")
  expect_type(out$se_raw, "double")
  expect_type(out$se, "double")
  expect_type(out$n, "integer")
  expect_type(out$n_eff, "double")
  expect_type(out$estimate_scale, "character")
  expect_type(out$transform, "character")
  expect_type(out$var_method, "character")
  expect_type(out$flag_small_n, "logical")
  expect_type(out$flag_zero_cell, "logical")
  expect_type(out$input_mode, "character")
  expect_type(out$flag_suppressed, "logical")
  expect_type(out$framing, "character")
  expect_type(out$flag_below_accountability, "logical")
  expect_equal(out$transform, out$estimate_scale)
  expect_equal(out$flag_suppressed, rep(FALSE, nrow(out)))
  expect_true(all(is.na(out$framing)))
  expect_equal(out$flag_below_accountability, c(TRUE, FALSE))
  expect_true(validate.sitemix_estimates(out))
})

test_that("binomial vjt output appends V without K", {
  out <- sitemix::sm_estimate_from_counts(
    schema_count_data(),
    family = "binomial",
    indicator = "absent",
    vjt = TRUE,
    min_n = 1L
  )

  expect_equal(names(out), c(sitemix_default_columns(), "V"))
  expect_false("K" %in% names(out))
  expect_false("vcov_method" %in% names(out))
  expect_type(out$V, "list")
  expect_s3_class(out$V[[1]], "sm_vcov")
  expect_equal(as.matrix(out$V[[1]]), matrix(out$se[[1]]^2, 1, 1, dimnames = list("absent", "absent")))
  expect_true(is.na(out$V[[1]]$vcov_method))
  expect_equal(out$V[[1]]$estimate_scale, "arcsine")
  expect_equal(out$V[[1]]$vcov_scale, "arcsine_delta")
  expect_true(validate.sitemix_estimates(out))
})

test_that("binomial 1x1 V metadata follows the row scale", {
  raw <- sitemix::sm_estimate_from_counts(
    schema_count_data(),
    family = "binomial",
    indicator = "absent",
    vst = "none",
    vjt = TRUE,
    min_n = 1L
  )
  expect_equal(raw$V[[1]]$estimate_scale, "none")
  expect_equal(raw$V[[1]]$vcov_scale, "raw")
  expect_equal(as.matrix(raw$V[[1]]), matrix(raw$se[[1]]^2, 1, 1, dimnames = list("absent", "absent")))

  logit <- sitemix::sm_estimate_from_counts(
    schema_count_data(),
    family = "binomial",
    indicator = "absent",
    vst = "logit",
    vjt = TRUE,
    min_n = 1L
  )
  expect_equal(logit$V[[1]]$estimate_scale, "logit")
  expect_equal(logit$V[[1]]$vcov_scale, "logit_delta")
  expect_equal(as.matrix(logit$V[[1]]), matrix(logit$se[[1]]^2, 1, 1, dimnames = list("absent", "absent")))

  anscombe <- sitemix::sm_estimate_from_counts(
    schema_count_data(),
    family = "binomial",
    indicator = "absent",
    anscombe = TRUE,
    vjt = TRUE,
    min_n = 1L
  )
  expect_equal(anscombe$V[[1]]$estimate_scale, "arcsine_anscombe")
  expect_equal(anscombe$V[[1]]$vcov_scale, "arcsine_delta")
})

test_that("public output schema snapshots cover supported families and variants", {
  a <- sitemix::sm_estimate_from_counts(
    schema_count_data(),
    family = "binomial",
    indicator = "absent",
    min_n = 2L
  )
  b <- sitemix::sm_estimate_from_counts(
    schema_multivariate_count_data(),
    family = "multivariate",
    indicators = c("snap", "frpm"),
    vjt = TRUE,
    min_n = 2L
  )
  c <- sitemix::sm_estimate_from_counts(
    schema_multinomial_count_data(),
    family = "multinomial",
    indicators = c("eng", "spa", "oth"),
    vjt = TRUE,
    min_n = 2L
  )
  d0 <- sitemix::sm_estimate_from_aggregates(
    schema_d0_aggregate_data(),
    family = "binomial",
    indicator = "absent",
    vjt = TRUE,
    min_n = 2L
  )
  d1 <- schema_quiet_working_independence(
    sitemix::sm_estimate_from_aggregates(
      schema_d1_aggregate_data(),
      family = "multivariate",
      vjt = TRUE,
      min_n = 2L
    )
  )
  smoothed <- sitemix::sm_smooth_variance(
    sitemix::sm_estimate_from_counts(
      schema_smoothing_count_data(),
      family = "binomial",
      indicator = "absent",
      min_n = 1L
    ),
    min_rows = 4L,
    overwrite = TRUE
  )
  suppressed_drop <- sitemix::sm_estimate_from_aggregates(
    schema_suppression_aggregate_data(),
    family = "binomial",
    indicator = "absent",
    suppression = "drop",
    min_n = 10L,
    accountability_n = 30L
  )
  suppressed_upper <- sitemix::sm_estimate_from_aggregates(
    schema_suppression_aggregate_data(),
    family = "binomial",
    indicator = "absent",
    suppression = "upper_bound",
    suppressed_theta_hat = 0.5,
    suppression_sensitivity_acknowledge = TRUE,
    min_n = 10L,
    accountability_n = 30L
  )

  expect_snapshot({
    snapshot_schema("A: binomial counts", a)
    snapshot_schema("B: multivariate counts with V/K", b)
    snapshot_schema("C: multinomial counts with V/K", c)
    snapshot_schema("D0: aggregate binomial with V", d0)
    snapshot_schema("D1: aggregate marginals with working-independence V/K", d1)
    snapshot_schema("smoothing: overwrite audit trail", smoothed)
    snapshot_schema("suppression: drop", suppressed_drop)
    snapshot_schema("suppression: acknowledged variance sensitivity", suppressed_upper)
  })
})
