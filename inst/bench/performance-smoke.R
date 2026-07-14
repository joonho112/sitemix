#!/usr/bin/env Rscript

.bench_budget_md5 <- "d05b2ad43afb3c83b829100f9ceba1e9"
.bench_runtime_md5 <- "d3cc4583d99417b96a3595353dcc85d8"
.bench_calibration_id <- "step68_initial_calibration_20260713"
.bench_threshold_basis <- "post_phase6_future_regression_calibration"

bench_arg_value <- function(args, name, default = NULL) {
  prefix <- paste0("--", name, "=")
  hit <- grep(paste0("^", prefix), args, value = TRUE)
  if (length(hit)) {
    return(sub(prefix, "", hit[[1L]], fixed = TRUE))
  }
  default
}

bench_has_flag <- function(args, name) {
  paste0("--", name) %in% args
}

bench_validate_args <- function(args) {
  value_names <- c(
    "profile", "reps", "warmup", "memory-reps", "seed", "out-dir", "budget"
  )
  flag_names <- "enforce"
  valid <- vapply(args, function(arg) {
    if (identical(arg, "--enforce")) {
      return(TRUE)
    }
    any(startsWith(arg, paste0("--", value_names, "=")))
  }, logical(1))
  if (any(!valid)) {
    stop("Unknown or malformed argument: ", args[which(!valid)[[1L]]], call. = FALSE)
  }
  names <- sub("^--([^=]+).*$", "\\1", args)
  if (anyDuplicated(names)) {
    stop("Duplicate argument: --", names[duplicated(names)][[1L]], call. = FALSE)
  }
  if (any(!names %in% c(value_names, flag_names))) {
    stop("Argument name is not allowed.", call. = FALSE)
  }
  invisible(TRUE)
}

bench_int_arg <- function(args, name, default, minimum = 0L) {
  text <- bench_arg_value(args, name, as.character(default))
  if (length(text) != 1L || !grepl("^[0-9]+$", text)) {
    stop("`--", name, "` must be an integer >= ", minimum, ".", call. = FALSE)
  }
  value <- suppressWarnings(as.numeric(text))
  if (!is.finite(value) || value < minimum || value > .Machine$integer.max) {
    stop("`--", name, "` must be an integer >= ", minimum, ".", call. = FALSE)
  }
  as.integer(value)
}

bench_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script_path <- if (length(file_arg)) {
    sub("^--file=", "", file_arg[[1L]])
  } else {
    file.path("inst", "bench", "performance-smoke.R")
  }
  normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)
}

bench_required_threads <- function() {
  c(
    OMP_NUM_THREADS = "1",
    OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1",
    VECLIB_MAXIMUM_THREADS = "1",
    BLAS_NUM_THREADS = "1"
  )
}

bench_thread_state <- function() {
  required <- bench_required_threads()
  observed <- Sys.getenv(names(required), unset = "")
  if (!identical(unname(observed), unname(required))) {
    detail <- paste0(names(required), "=", observed, collapse = ", ")
    stop(
      "Thread controls must equal 1 before R starts; observed: ",
      detail,
      call. = FALSE
    )
  }
  observed
}

bench_load_package <- function(root) {
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    stop("Performance smoke requires a sitemix source tree.", call. = FALSE)
  }
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("Performance smoke requires pkgload for source-tree loading.", call. = FALSE)
  }
  pkgload::load_all(root, quiet = TRUE)
  invisible(TRUE)
}

bench_counts_path <- function(root) {
  path <- file.path(root, "inst", "extdata", "alprek_subset_counts.rds")
  if (!file.exists(path)) {
    stop("Could not find source-tree alprek_subset_counts.rds.", call. = FALSE)
  }
  path
}

bench_expected_names <- function(include_K = TRUE) {
  names <- c(
    "site_id", "year", "indicator", "theta_raw", "theta_hat", "se_raw", "se",
    "n", "n_eff", "estimate_scale", "transform", "var_method", "flag_small_n",
    "flag_zero_cell", "input_mode", "flag_suppressed", "framing",
    "flag_below_accountability", "V"
  )
  if (include_K) {
    names <- c(names, "K")
  }
  paste(names, collapse = "|")
}

bench_expected_case_specs <- function() {
  class <- "sitemix_estimates|tbl_df|tbl|data.frame"
  data.frame(
    case = c(
      "synthetic_c_small",
      "synthetic_c_medium",
      "synthetic_c_large",
      "alprek_scenario_b_counts_vjt",
      "alprek_aggregate_d0_frpm_vjt",
      "alprek_aggregate_d1_four_indicator_vjt"
    ),
    kind = c(rep("synthetic_c", 3L), rep("alprek", 3L)),
    K = c(3L, 8L, 16L, 3L, 1L, 4L),
    S = c(5L, 25L, 100L, 250L, 250L, 250L),
    n = c(64L, 128L, 128L, 0L, 0L, 0L),
    seed = c(20260723L, 20260724L, 20260725L, 0L, 0L, 0L),
    iterations = c(5L, 2L, 1L, 1L, 1L, 1L),
    expected_rows = c(15L, 200L, 1600L, 750L, 250L, 1000L),
    expected_columns = c(20L, 20L, 20L, 20L, 19L, 20L),
    expected_has_V = rep(TRUE, 6L),
    expected_has_K = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),
    expected_class = rep(class, 6L),
    expected_family = c(
      "multinomial", "multinomial", "multinomial",
      "multivariate", "binomial", "multivariate"
    ),
    expected_role = rep("summary_uncertainty", 6L),
    expected_names = c(
      rep(bench_expected_names(TRUE), 4L),
      bench_expected_names(FALSE),
      bench_expected_names(TRUE)
    ),
    expected_result_md5 = c(
      "6a8177fb1bc8fbcd8465cacf1eff6d2d",
      "8ae31d140c7b3057b9097524816c8615",
      "1437a5b74f238c342e49cfd885af5210",
      "7401958552398047196908b3a65b8ef3",
      "f2efed9c2f3b6a995e5969c15921496f",
      "78fceaf2638f33bf5036608488fda599"
    ),
    threshold_basis = rep(.bench_threshold_basis, 6L),
    calibration_run_id = rep(.bench_calibration_id, 6L),
    stringsAsFactors = FALSE
  )
}

bench_budget_columns <- function() {
  c(
    names(bench_expected_case_specs()),
    "timing_advisory_seconds", "timing_hard_seconds",
    "object_advisory_bytes", "object_hard_bytes",
    "serialized_advisory_bytes", "serialized_hard_bytes",
    "robust_cv_limit", "max_median_ratio_limit", "allocation_blocking"
  )
}

bench_parse_logical <- function(values, column) {
  text <- toupper(trimws(as.character(values)))
  if (any(!text %in% c("TRUE", "FALSE"))) {
    stop("Performance budget contains invalid `", column, "` values.", call. = FALSE)
  }
  text == "TRUE"
}

bench_compare_spec_column <- function(observed, expected, column) {
  equal <- if (is.numeric(expected)) {
    is.numeric(observed) && length(observed) == length(expected) &&
      all(observed == expected)
  } else {
    identical(observed, expected)
  }
  if (!isTRUE(equal)) {
    stop("Performance budget disagrees with canonical `", column, "`.", call. = FALSE)
  }
  invisible(TRUE)
}

bench_validate_budget <- function(budget) {
  expected <- bench_expected_case_specs()
  if (!identical(names(budget), bench_budget_columns()) || nrow(budget) != nrow(expected)) {
    stop("Performance budget schema or case count is invalid.", call. = FALSE)
  }
  logical_columns <- c("expected_has_V", "expected_has_K", "allocation_blocking")
  for (column in logical_columns) {
    budget[[column]] <- bench_parse_logical(budget[[column]], column)
  }
  integer_columns <- c(
    "K", "S", "n", "seed", "iterations", "expected_rows", "expected_columns",
    "object_advisory_bytes", "object_hard_bytes", "serialized_advisory_bytes",
    "serialized_hard_bytes"
  )
  numeric_columns <- c(
    integer_columns, "timing_advisory_seconds", "timing_hard_seconds",
    "robust_cv_limit", "max_median_ratio_limit"
  )
  for (column in numeric_columns) {
    values <- suppressWarnings(as.numeric(budget[[column]]))
    if (anyNA(values) || any(!is.finite(values)) || any(values < 0)) {
      stop("Performance budget contains invalid `", column, "` values.", call. = FALSE)
    }
    if (column %in% integer_columns && any(values != floor(values))) {
      stop("Performance budget requires integer `", column, "` values.", call. = FALSE)
    }
    budget[[column]] <- if (column %in% integer_columns) as.integer(values) else values
  }
  if (!identical(budget$case, expected$case) || anyDuplicated(budget$case)) {
    stop("Performance budget case order or identity is invalid.", call. = FALSE)
  }
  for (column in names(expected)) {
    bench_compare_spec_column(budget[[column]], expected[[column]], column)
  }
  if (any(!budget$kind %in% c("synthetic_c", "alprek"))) {
    stop("Performance budget case kinds are invalid.", call. = FALSE)
  }
  if (any(budget$K <= 0L) || any(budget$S <= 0L) ||
        any(budget$iterations <= 0L) || any(budget$expected_columns <= 0L)) {
    stop("Performance budget contains nonpositive structural counts.", call. = FALSE)
  }
  if (any(budget$expected_rows != budget$K * budget$S)) {
    stop("Performance budget rows must equal K times S.", call. = FALSE)
  }
  synthetic <- budget$kind == "synthetic_c"
  if (any(budget$n[synthetic] <= budget$K[synthetic]) ||
        any(budget$seed[synthetic] <= 0L)) {
    stop("Synthetic case n and seed values are invalid.", call. = FALSE)
  }
  if (any(budget$n[!synthetic] != 0L) || any(budget$seed[!synthetic] != 0L)) {
    stop("AL Pre-K cases must use n=0 and seed=0 sentinels.", call. = FALSE)
  }
  limit_pairs <- list(
    c("timing_advisory_seconds", "timing_hard_seconds"),
    c("object_advisory_bytes", "object_hard_bytes"),
    c("serialized_advisory_bytes", "serialized_hard_bytes")
  )
  for (pair in limit_pairs) {
    advisory <- budget[[pair[[1L]]]]
    hard <- budget[[pair[[2L]]]]
    if (any(advisory <= 0) || any(hard <= advisory)) {
      stop("Performance budget requires 0 < advisory < hard limits.", call. = FALSE)
    }
  }
  if (any(budget$robust_cv_limit <= 0 | budget$robust_cv_limit > 1)) {
    stop("Performance budget robust CV limits must be in (0, 1].", call. = FALSE)
  }
  if (any(budget$max_median_ratio_limit <= 1 |
            budget$max_median_ratio_limit > 10)) {
    stop("Performance max/median limits must be in (1, 10].", call. = FALSE)
  }
  if (any(budget$allocation_blocking)) {
    stop("Blocking allocation thresholds are not calibrated.", call. = FALSE)
  }
  budget
}

bench_read_budget <- function(path) {
  if (!file.exists(path)) {
    stop("Performance budget is unavailable: ", path, call. = FALSE)
  }
  budget <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  bench_validate_budget(budget)
}

bench_runtime_columns <- function() {
  c(
    "calibration_run_id", "calibrated_utc", "threshold_basis",
    "calibration_source", "calibration_evidence_sha256",
    "calibration_gate_summary_sha256", "benchmark_profile", "timing_reps",
    "warmup", "memory_reps", "r_major_minor", "r_full", "platform", "sysname",
    "machine", "blas", "lapack", "calibration_package_version",
    "enforced_package_version", "dependency_versions",
    "counts_md5", names(bench_required_threads())
  )
}

bench_read_runtime_reference <- function(path) {
  if (!file.exists(path)) {
    stop("Performance runtime reference is unavailable: ", path, call. = FALSE)
  }
  reference <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  if (nrow(reference) != 1L || !identical(names(reference), bench_runtime_columns())) {
    stop("Performance runtime reference schema is invalid.", call. = FALSE)
  }
  if (!identical(reference$calibration_run_id, .bench_calibration_id) ||
        !identical(reference$threshold_basis, .bench_threshold_basis)) {
    stop("Performance runtime provenance disagrees with the budget.", call. = FALSE)
  }
  if (!nzchar(reference$calibration_package_version) ||
        !nzchar(reference$enforced_package_version)) {
    stop("Performance package-version provenance is incomplete.", call. = FALSE)
  }
  hashes <- c(
    reference$calibration_evidence_sha256,
    reference$calibration_gate_summary_sha256
  )
  if (any(!grepl("^[0-9a-f]{64}$", hashes))) {
    stop("Performance runtime provenance hashes are invalid.", call. = FALSE)
  }
  integer_columns <- c("timing_reps", "warmup", "memory_reps")
  for (column in integer_columns) {
    value <- suppressWarnings(as.numeric(reference[[column]]))
    if (!is.finite(value) || value < 0 || value != floor(value)) {
      stop("Performance runtime reference has invalid run counts.", call. = FALSE)
    }
    reference[[column]] <- as.integer(value)
  }
  if (reference$timing_reps < 5L || reference$warmup < 2L ||
        reference$memory_reps < 3L) {
    stop("Performance runtime calibration is underpowered.", call. = FALSE)
  }
  reference
}

bench_dependency_versions <- function() {
  packages <- c("Matrix", "cli", "pkgload", "rlang", "tibble", "vctrs")
  versions <- vapply(packages, function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Required benchmark dependency is unavailable: ", package, call. = FALSE)
    }
    as.character(utils::packageVersion(package))
  }, character(1))
  paste0(packages, "=", versions, collapse = ";")
}

bench_r_major_minor <- function() {
  minor <- strsplit(R.version$minor, ".", fixed = TRUE)[[1L]][[1L]]
  paste(R.version$major, minor, sep = ".")
}

bench_current_runtime <- function(counts_path, threads) {
  info <- utils::sessionInfo()
  data.frame(
    r_major_minor = bench_r_major_minor(),
    r_full = as.character(getRversion()),
    platform = R.version$platform,
    sysname = unname(Sys.info()[["sysname"]]),
    machine = unname(Sys.info()[["machine"]]),
    blas = info$BLAS,
    lapack = info$LAPACK,
    enforced_package_version = as.character(utils::packageVersion("sitemix")),
    dependency_versions = bench_dependency_versions(),
    counts_md5 = unname(tools::md5sum(counts_path)),
    as.list(unname(threads)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  ) |> stats::setNames(c(
    "r_major_minor", "r_full", "platform", "sysname", "machine", "blas",
    "lapack", "enforced_package_version", "dependency_versions", "counts_md5",
    names(threads)
  ))
}

bench_runtime_compatibility_columns <- function() {
  c(
    "r_major_minor", "r_full", "platform", "sysname", "machine", "blas",
    "lapack", "enforced_package_version", "dependency_versions", "counts_md5",
    names(bench_required_threads())
  )
}

bench_runtime_compatible <- function(current, reference) {
  columns <- bench_runtime_compatibility_columns()
  all(vapply(columns, function(column) {
    identical(as.character(current[[column]]), as.character(reference[[column]]))
  }, logical(1)))
}

bench_quiet_working_independence <- function(expr) {
  withCallingHandlers(
    expr,
    sitemix_warning_working_independence_default = function(w) invokeRestart("muffleWarning")
  )
}

bench_make_multinomial_counts <- function(K, S, n, seed) {
  set.seed(seed)
  indicators <- sprintf("c%02d", seq_len(K))
  counts <- t(vapply(seq_len(S), function(i) {
    probabilities <- stats::runif(K, min = 0.5, max = 1.5)
    as.integer(1L + stats::rmultinom(1L, n - K, prob = probabilities)[, 1L])
  }, integer(K)))
  data <- data.frame(
    site_id = sprintf("S%03d", seq_len(S)),
    year = 2024L,
    n_jt = as.integer(n),
    stringsAsFactors = FALSE
  )
  for (k in seq_len(K)) {
    data[[paste0("c_jt_", indicators[[k]])]] <- counts[, k]
  }
  list(data = data, indicators = indicators)
}

bench_build_cases <- function(counts, budget) {
  alprek_rows <- unique(budget$S[budget$kind == "alprek"])
  if (length(alprek_rows) != 1L || nrow(counts) != alprek_rows) {
    stop("AL Pre-K fixture rows disagree with the canonical case specs.", call. = FALSE)
  }
  cases <- list()
  synthetic <- budget[budget$kind == "synthetic_c", , drop = FALSE]
  for (i in seq_len(nrow(synthetic))) {
    fixture <- bench_make_multinomial_counts(
      K = synthetic$K[[i]],
      S = synthetic$S[[i]],
      n = synthetic$n[[i]],
      seed = synthetic$seed[[i]]
    )
    cases[[synthetic$case[[i]]]] <- local({
      current <- fixture
      function() {
        sitemix::sm_estimate_from_counts(
          current$data,
          family = "multinomial",
          indicators = current$indicators,
          vst = "none",
          boundary_method = "none",
          vjt = TRUE,
          min_n = 1L
        )
      }
    })
  }

  indicators <- c("frpm", "snap", "wic", "tanf")
  cases$alprek_scenario_b_counts_vjt <- function() {
    sitemix::sm_estimate_from_counts(
      counts,
      family = "multivariate",
      indicators = indicators[1:3],
      vjt = TRUE,
      min_n = 1L
    )
  }
  cases$alprek_aggregate_d0_frpm_vjt <- function() {
    sitemix::sm_estimate_from_aggregates(
      counts[c("site_id", "year", "c_jt_frpm", "n_jt")],
      family = "binomial",
      indicator = "frpm",
      numerator_col = "c_jt_frpm",
      denominator_col = "n_jt",
      vjt = TRUE,
      min_n = 1L
    )
  }
  cases$alprek_aggregate_d1_four_indicator_vjt <- function() {
    bench_quiet_working_independence(
      sitemix::sm_estimate_from_aggregates(
        counts[c("site_id", "year", paste0("c_jt_", indicators), "n_jt")],
        family = "multivariate",
        vjt = TRUE,
        min_n = 1L
      )
    )
  }
  if (!identical(names(cases), budget$case)) {
    stop("Performance case registry disagrees with canonical case order.", call. = FALSE)
  }
  cases
}

bench_signature <- function(value) {
  path <- tempfile("sitemix-performance-result-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(value, path, version = 3)
  list(
    md5 = unname(tools::md5sum(path)),
    object_size_bytes = as.numeric(object.size(value)),
    serialized_size_bytes = unname(file.info(path)$size)
  )
}

bench_scalar_attr <- function(value, name) {
  attribute <- attr(value, name, exact = TRUE)
  if (length(attribute) == 1L && !is.na(attribute)) as.character(attribute) else NA_character_
}

bench_v_shape_ok <- function(value, K) {
  if (!"V" %in% names(value) || length(value$V) != nrow(value)) {
    return(FALSE)
  }
  all(vapply(value$V, function(vcov) {
    inherits(vcov, "sm_vcov") && is.matrix(vcov$matrix) &&
      identical(dim(vcov$matrix), c(K, K)) &&
      length(vcov$indicator_order) == K
  }, logical(1)))
}

bench_k_values_ok <- function(value, K, expected_has_K) {
  if (!isTRUE(expected_has_K)) {
    return(!"K" %in% names(value))
  }
  "K" %in% names(value) && length(value$K) == nrow(value) &&
    all(!is.na(value$K)) && all(value$K == K)
}

bench_result_contract <- function(value, K, expected_has_K) {
  data.frame(
    rows = nrow(value),
    columns = ncol(value),
    has_V = "V" %in% names(value),
    has_K = "K" %in% names(value),
    class = paste(class(value), collapse = "|"),
    family = bench_scalar_attr(value, "family"),
    role = bench_scalar_attr(value, "sitemix_role"),
    names = paste(names(value), collapse = "|"),
    v_shape_ok = bench_v_shape_ok(value, K),
    k_values_ok = bench_k_values_ok(value, K, expected_has_K),
    stringsAsFactors = FALSE
  )
}

bench_run_timing_case <- function(case, fun, warmup, reps, iterations, K, has_K) {
  for (i in seq_len(warmup)) {
    invisible(fun())
  }
  rows <- vector("list", reps)
  for (replicate in seq_len(reps)) {
    gc()
    value <- NULL
    timing <- system.time({
      for (iteration in seq_len(iterations)) {
        value <- fun()
      }
    })
    signature <- bench_signature(value)
    contract <- bench_result_contract(value, K, has_K)
    rows[[replicate]] <- cbind(
      data.frame(
        case = case,
        replicate = replicate,
        iterations = iterations,
        batch_user_seconds = unname(timing[["user.self"]]),
        batch_system_seconds = unname(timing[["sys.self"]]),
        batch_elapsed_seconds = unname(timing[["elapsed"]]),
        elapsed_seconds = unname(timing[["elapsed"]]) / iterations,
        stringsAsFactors = FALSE
      ),
      contract,
      data.frame(
        result_md5 = signature$md5,
        object_size_bytes = signature$object_size_bytes,
        serialized_size_bytes = signature$serialized_size_bytes,
        stringsAsFactors = FALSE
      )
    )
  }
  do.call(rbind, rows)
}

bench_rprofmem_bytes <- function(path) {
  lines <- readLines(path, warn = FALSE)
  matched <- grepl("^[0-9]+", lines)
  bytes <- suppressWarnings(as.numeric(sub("^([0-9]+).*$", "\\1", lines[matched])))
  bytes[is.finite(bytes)]
}

bench_gc_value <- function(gc_state, row, column) {
  if (!row %in% rownames(gc_state) || !column %in% colnames(gc_state)) {
    return(NA_real_)
  }
  unname(gc_state[row, column])
}

bench_run_memory_case <- function(case, fun, reps, K, has_K) {
  if (!isTRUE(capabilities("profmem"))) {
    stop("Rprofmem capability is required for the calibration pass.", call. = FALSE)
  }
  rows <- vector("list", reps)
  for (replicate in seq_len(reps)) {
    profile_path <- tempfile("sitemix-rprofmem-", fileext = ".out")
    on.exit(unlink(profile_path), add = TRUE)
    gc(reset = TRUE)
    value <- NULL
    utils::Rprofmem(profile_path)
    tryCatch(
      value <- fun(),
      finally = utils::Rprofmem(NULL)
    )
    gc_state <- gc()
    allocations <- bench_rprofmem_bytes(profile_path)
    unlink(profile_path)
    if (!length(allocations)) {
      stop("Rprofmem produced no parseable allocation events.", call. = FALSE)
    }
    signature <- bench_signature(value)
    contract <- bench_result_contract(value, K, has_K)
    rows[[replicate]] <- cbind(
      data.frame(
        case = case,
        replicate = replicate,
        allocation_events = length(allocations),
        allocation_total_bytes = sum(allocations),
        allocation_max_bytes = max(allocations),
        gc_ncells_used = bench_gc_value(gc_state, "Ncells", "used"),
        gc_vcells_used = bench_gc_value(gc_state, "Vcells", "used"),
        gc_ncells_max_used = bench_gc_value(gc_state, "Ncells", "max used"),
        gc_vcells_max_used = bench_gc_value(gc_state, "Vcells", "max used"),
        stringsAsFactors = FALSE
      ),
      contract,
      data.frame(
        result_md5 = signature$md5,
        object_size_bytes = signature$object_size_bytes,
        serialized_size_bytes = signature$serialized_size_bytes,
        stringsAsFactors = FALSE
      )
    )
  }
  do.call(rbind, rows)
}

bench_one_value <- function(x, column) {
  values <- unique(x[[column]])
  if (length(values) != 1L) {
    stop("Benchmark result is unstable for `", column, "`.", call. = FALSE)
  }
  values[[1L]]
}

bench_timing_summary <- function(results) {
  rows <- lapply(split(results, results$case), function(x) {
    elapsed <- x$elapsed_seconds
    median_elapsed <- stats::median(elapsed)
    data.frame(
      case = x$case[[1L]],
      timing_reps = nrow(x),
      observed_iterations = bench_one_value(x, "iterations"),
      min_elapsed_seconds = min(elapsed),
      median_elapsed_seconds = median_elapsed,
      max_elapsed_seconds = max(elapsed),
      mad_elapsed_seconds = stats::mad(elapsed),
      iqr_elapsed_seconds = stats::IQR(elapsed),
      robust_cv = if (median_elapsed > 0) stats::mad(elapsed) / median_elapsed else NA_real_,
      max_median_ratio = if (median_elapsed > 0) max(elapsed) / median_elapsed else NA_real_,
      rows = bench_one_value(x, "rows"),
      columns = bench_one_value(x, "columns"),
      has_V = bench_one_value(x, "has_V"),
      has_K = bench_one_value(x, "has_K"),
      class = bench_one_value(x, "class"),
      family = bench_one_value(x, "family"),
      role = bench_one_value(x, "role"),
      names = bench_one_value(x, "names"),
      v_shape_ok = all(x$v_shape_ok),
      k_values_ok = all(x$k_values_ok),
      result_md5 = paste(unique(x$result_md5), collapse = "|"),
      result_signature_stable = length(unique(x$result_md5)) == 1L,
      object_size_bytes = bench_one_value(x, "object_size_bytes"),
      object_size_stable = length(unique(x$object_size_bytes)) == 1L,
      serialized_size_bytes = bench_one_value(x, "serialized_size_bytes"),
      serialized_size_stable = length(unique(x$serialized_size_bytes)) == 1L,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

bench_memory_summary <- function(results) {
  rows <- lapply(split(results, results$case), function(x) {
    data.frame(
      case = x$case[[1L]],
      memory_reps = nrow(x),
      median_allocation_events = stats::median(x$allocation_events),
      median_allocation_total_bytes = stats::median(x$allocation_total_bytes),
      mad_allocation_total_bytes = stats::mad(x$allocation_total_bytes),
      median_allocation_max_bytes = stats::median(x$allocation_max_bytes),
      max_gc_ncells_used = max(x$gc_ncells_used),
      max_gc_vcells_used = max(x$gc_vcells_used),
      max_gc_ncells_max_used = max(x$gc_ncells_max_used),
      max_gc_vcells_max_used = max(x$gc_vcells_max_used),
      rows_memory = bench_one_value(x, "rows"),
      columns_memory = bench_one_value(x, "columns"),
      has_V_memory = bench_one_value(x, "has_V"),
      has_K_memory = bench_one_value(x, "has_K"),
      class_memory = bench_one_value(x, "class"),
      family_memory = bench_one_value(x, "family"),
      role_memory = bench_one_value(x, "role"),
      names_memory = bench_one_value(x, "names"),
      v_shape_ok_memory = all(x$v_shape_ok),
      k_values_ok_memory = all(x$k_values_ok),
      result_md5_memory = paste(unique(x$result_md5), collapse = "|"),
      result_signature_stable_memory = length(unique(x$result_md5)) == 1L,
      object_size_bytes_memory = bench_one_value(x, "object_size_bytes"),
      object_size_stable_memory = length(unique(x$object_size_bytes)) == 1L,
      serialized_size_bytes_memory = bench_one_value(x, "serialized_size_bytes"),
      serialized_size_stable_memory = length(unique(x$serialized_size_bytes)) == 1L,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  row.names(out) <- NULL
  out
}

bench_limit_status <- function(value, advisory, hard) {
  if (value > hard) {
    "HARD"
  } else if (value > advisory) {
    "ADVISORY"
  } else {
    "PASS"
  }
}

bench_is_unstable <- function(
  robust_cv,
  max_median_ratio,
  robust_cv_limit,
  max_median_ratio_limit
) {
  is.na(robust_cv) |
    robust_cv > robust_cv_limit |
    is.na(max_median_ratio) |
    max_median_ratio > max_median_ratio_limit
}

bench_gate_decisions <- function(
  timing,
  memory,
  budget,
  numeric_blocking,
  reference_signature_checked
) {
  merged <- merge(budget, timing, by = "case", all = TRUE, sort = FALSE)
  merged <- merge(merged, memory, by = "case", all = TRUE, sort = FALSE)
  merged <- merged[match(budget$case, merged$case), , drop = FALSE]
  if (anyNA(merged$kind) || nrow(merged) != nrow(budget)) {
    stop("Performance results do not match the exact budget case set.", call. = FALSE)
  }
  merged$iterations_ok <- merged$observed_iterations == merged$iterations
  merged$dimensions_ok <- merged$rows == merged$expected_rows &
    merged$columns == merged$expected_columns &
    merged$rows_memory == merged$expected_rows &
    merged$columns_memory == merged$expected_columns
  merged$schema_ok <- merged$has_V == merged$expected_has_V &
    merged$has_K == merged$expected_has_K &
    merged$has_V_memory == merged$expected_has_V &
    merged$has_K_memory == merged$expected_has_K &
    merged$class == merged$expected_class &
    merged$class_memory == merged$expected_class &
    merged$family == merged$expected_family &
    merged$family_memory == merged$expected_family &
    merged$role == merged$expected_role &
    merged$role_memory == merged$expected_role &
    merged$names == merged$expected_names &
    merged$names_memory == merged$expected_names &
    merged$v_shape_ok & merged$v_shape_ok_memory &
    merged$k_values_ok & merged$k_values_ok_memory
  merged$signature_ok <- merged$result_signature_stable &
    merged$result_signature_stable_memory &
    merged$result_md5 == merged$result_md5_memory
  merged$reference_signature_checked <- reference_signature_checked
  merged$reference_signature_ok <- if (isTRUE(reference_signature_checked)) {
    merged$result_md5 == merged$expected_result_md5
  } else {
    NA
  }
  merged$size_stable <- merged$object_size_stable &
    merged$object_size_stable_memory &
    merged$serialized_size_stable &
    merged$serialized_size_stable_memory &
    merged$object_size_bytes == merged$object_size_bytes_memory &
    merged$serialized_size_bytes == merged$serialized_size_bytes_memory
  merged$timing_status <- mapply(
    bench_limit_status,
    merged$median_elapsed_seconds,
    merged$timing_advisory_seconds,
    merged$timing_hard_seconds,
    USE.NAMES = FALSE
  )
  merged$object_status <- mapply(
    bench_limit_status,
    merged$object_size_bytes,
    merged$object_advisory_bytes,
    merged$object_hard_bytes,
    USE.NAMES = FALSE
  )
  merged$serialized_status <- mapply(
    bench_limit_status,
    merged$serialized_size_bytes,
    merged$serialized_advisory_bytes,
    merged$serialized_hard_bytes,
    USE.NAMES = FALSE
  )
  merged$allocation_status <- "CALIBRATION_ONLY"
  merged$unstable <- bench_is_unstable(
    merged$robust_cv,
    merged$max_median_ratio,
    merged$robust_cv_limit,
    merged$max_median_ratio_limit
  )
  merged$numeric_blocking <- numeric_blocking
  merged$hard_numeric <- merged$timing_status == "HARD" |
    merged$object_status == "HARD" |
    merged$serialized_status == "HARD"
  merged$structural_failure <- !merged$iterations_ok |
    !merged$dimensions_ok |
    !merged$schema_ok |
    !merged$signature_ok |
    !merged$size_stable |
    (merged$reference_signature_checked & !merged$reference_signature_ok)
  merged$case_status <- ifelse(
    merged$structural_failure,
    "FAIL",
    ifelse(
      merged$numeric_blocking & merged$unstable,
      "UNSTABLE",
      ifelse(
        merged$numeric_blocking & merged$hard_numeric,
        "FAIL",
        ifelse(
          merged$unstable | merged$hard_numeric |
            merged$timing_status == "ADVISORY" |
            merged$object_status == "ADVISORY" |
            merged$serialized_status == "ADVISORY",
          "ADVISORY",
          "PASS"
        )
      )
    )
  )
  merged
}

bench_metadata <- function(
  profile,
  warmup,
  reps,
  memory_reps,
  seed,
  budget_path,
  runtime_path,
  runtime_current,
  runtime_reference,
  runtime_compatible,
  budget_authoritative,
  started_utc,
  completed_utc
) {
  keys <- c(
    "profile", "warmup", "timing_reps", "memory_reps", "seed",
    "runtime_compatible", "budget_authoritative", "budget_md5", "runtime_md5",
    "calibration_run_id", "calibrated_utc", "threshold_basis",
    "calibration_source", "calibration_package_version",
    "calibration_evidence_sha256",
    "calibration_gate_summary_sha256", names(runtime_current),
    "started_utc", "completed_utc"
  )
  values <- c(
    profile, warmup, reps, memory_reps, seed,
    runtime_compatible, budget_authoritative,
    unname(tools::md5sum(budget_path)), unname(tools::md5sum(runtime_path)),
    runtime_reference$calibration_run_id,
    runtime_reference$calibrated_utc,
    runtime_reference$threshold_basis,
    runtime_reference$calibration_source,
    runtime_reference$calibration_package_version,
    runtime_reference$calibration_evidence_sha256,
    runtime_reference$calibration_gate_summary_sha256,
    unname(unlist(runtime_current[1L, ], use.names = FALSE)),
    started_utc, completed_utc
  )
  data.frame(key = keys, value = as.character(values), stringsAsFactors = FALSE)
}

bench_source_hashes <- function(root, budget_path, runtime_path, counts_path) {
  paths <- c(
    file.path(root, "inst", "bench", "performance-smoke.R"),
    file.path(root, "inst", "bench", "performance-contract-self-test.R"),
    budget_path,
    runtime_path,
    counts_path,
    file.path(root, "DESCRIPTION"),
    file.path(root, "NAMESPACE"),
    sort(list.files(file.path(root, "R"), pattern = "[.]R$", full.names = TRUE))
  )
  if (any(!file.exists(paths))) {
    stop("Performance source manifest contains a missing file.", call. = FALSE)
  }
  data.frame(
    path = vapply(paths, function(path) {
      normalized <- normalizePath(path, mustWork = TRUE)
      if (startsWith(normalized, paste0(root, .Platform$file.sep))) {
        substring(normalized, nchar(root) + 2L)
      } else {
        normalized
      }
    }, character(1)),
    md5 = unname(tools::md5sum(paths)),
    stringsAsFactors = FALSE
  )
}

bench_observed_case_specs <- function(decisions) {
  decisions[c(
    "case", "kind", "K", "S", "n", "seed", "iterations",
    "observed_iterations", "expected_rows", "rows", "rows_memory",
    "expected_columns", "columns", "columns_memory", "expected_has_V",
    "has_V", "has_V_memory", "expected_has_K", "has_K", "has_K_memory",
    "expected_class", "class", "class_memory", "expected_family", "family",
    "family_memory", "expected_role", "role", "role_memory", "expected_names",
    "names", "names_memory", "expected_result_md5", "result_md5",
    "reference_signature_checked", "reference_signature_ok", "v_shape_ok",
    "v_shape_ok_memory", "k_values_ok", "k_values_ok_memory"
  )]
}

bench_output_target <- function(out_dir) {
  if (!is.character(out_dir) || length(out_dir) != 1L || !nzchar(out_dir)) {
    stop("`--out-dir` must be one nonempty path.", call. = FALSE)
  }
  expanded <- path.expand(out_dir)
  if (!startsWith(expanded, .Platform$file.sep)) {
    expanded <- file.path(getwd(), expanded)
  }
  parent <- dirname(expanded)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  parent <- normalizePath(parent, mustWork = TRUE)
  target <- file.path(parent, basename(expanded))
  if (file.exists(target) || dir.exists(target)) {
    stop("Output directory must be fresh: ", target, call. = FALSE)
  }
  target
}

bench_write_outputs <- function(
  out_dir,
  timing_results,
  timing_summary,
  memory_results,
  memory_summary,
  decisions,
  metadata,
  source_hashes,
  budget,
  runtime_current,
  runtime_reference,
  gate_summary
) {
  target <- bench_output_target(out_dir)
  staging <- paste0(target, ".tmp-", Sys.getpid())
  if (file.exists(staging) || dir.exists(staging)) {
    stop("Staging directory already exists: ", staging, call. = FALSE)
  }
  dir.create(staging, recursive = FALSE, showWarnings = FALSE)
  committed <- FALSE
  on.exit(if (!committed) unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  utils::write.csv(timing_results, file.path(staging, "timings.csv"), row.names = FALSE)
  utils::write.csv(timing_summary, file.path(staging, "timing-summary.csv"), row.names = FALSE)
  utils::write.csv(memory_results, file.path(staging, "allocations.csv"), row.names = FALSE)
  utils::write.csv(memory_summary, file.path(staging, "memory-summary.csv"), row.names = FALSE)
  utils::write.csv(decisions, file.path(staging, "gate-decisions.csv"), row.names = FALSE)
  utils::write.csv(metadata, file.path(staging, "run-metadata.csv"), row.names = FALSE)
  utils::write.csv(source_hashes, file.path(staging, "source-hashes.csv"), row.names = FALSE)
  utils::write.csv(budget, file.path(staging, "budget-snapshot.csv"), row.names = FALSE)
  utils::write.csv(
    bench_observed_case_specs(decisions),
    file.path(staging, "case-specs.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    runtime_current,
    file.path(staging, "runtime-current.csv"),
    row.names = FALSE
  )
  utils::write.csv(
    runtime_reference,
    file.path(staging, "runtime-reference.csv"),
    row.names = FALSE
  )
  utils::write.csv(gate_summary, file.path(staging, "gate-summary.csv"), row.names = FALSE)
  saveRDS(
    list(
      timings = timing_results,
      timing_summary = timing_summary,
      allocations = memory_results,
      memory_summary = memory_summary,
      decisions = decisions,
      metadata = metadata,
      source_hashes = source_hashes,
      budget = budget,
      observed_case_specs = bench_observed_case_specs(decisions),
      runtime_current = runtime_current,
      runtime_reference = runtime_reference,
      gate_summary = gate_summary
    ),
    file.path(staging, "performance-evidence.rds"),
    version = 3
  )
  writeLines(
    capture.output(utils::sessionInfo()),
    file.path(staging, "session-info.txt"),
    useBytes = TRUE
  )
  if (!file.rename(staging, target)) {
    stop("Could not atomically commit performance artifacts.", call. = FALSE)
  }
  committed <- TRUE
  normalizePath(target, mustWork = TRUE)
}

bench_same_path <- function(left, right) {
  identical(
    normalizePath(left, mustWork = TRUE),
    normalizePath(right, mustWork = TRUE)
  )
}

bench_validate_authority <- function(
  profile,
  enforce,
  reps,
  warmup,
  memory_reps,
  seed,
  budget_path,
  canonical_budget_path,
  runtime_path,
  runtime_compatible
) {
  budget_authoritative <- bench_same_path(budget_path, canonical_budget_path) &&
    identical(unname(tools::md5sum(budget_path)), .bench_budget_md5)
  runtime_authoritative <- identical(
    unname(tools::md5sum(runtime_path)),
    .bench_runtime_md5
  )
  if (!runtime_authoritative) {
    stop("Canonical performance runtime hash is invalid.", call. = FALSE)
  }
  if (isTRUE(enforce) && !identical(profile, "closeout")) {
    stop("`--enforce` is only valid with `--profile=closeout`.", call. = FALSE)
  }
  if (isTRUE(enforce) && !budget_authoritative) {
    stop("Enforced closeout requires the canonical budget path and hash.", call. = FALSE)
  }
  if (isTRUE(enforce) && !runtime_compatible) {
    stop("Enforced closeout requires the exact calibrated runtime.", call. = FALSE)
  }
  if (isTRUE(enforce) && (reps < 5L || warmup < 2L || memory_reps < 3L)) {
    stop("Enforced closeout requires reps>=5, warmup>=2, memory-reps>=3.", call. = FALSE)
  }
  if (seed != 20260723L) {
    stop("`--seed` must match the canonical first synthetic seed 20260723.", call. = FALSE)
  }
  list(
    budget_authoritative = budget_authoritative,
    runtime_authoritative = runtime_authoritative,
    numeric_blocking = isTRUE(enforce) && identical(profile, "closeout") &&
      budget_authoritative && runtime_compatible
  )
}

bench_exit_status <- function(
  structural_failures,
  blocking_unstable_cases,
  blocking_hard_failures
) {
  if (structural_failures || blocking_unstable_cases) {
    return(2L)
  }
  if (blocking_hard_failures) {
    return(1L)
  }
  0L
}

bench_main <- function() {
  started_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  args <- commandArgs(trailingOnly = TRUE)
  bench_validate_args(args)
  profile <- bench_arg_value(args, "profile", "ci-smoke")
  if (!profile %in% c("ci-smoke", "closeout")) {
    stop("`--profile` must be `ci-smoke` or `closeout`.", call. = FALSE)
  }
  default_reps <- if (identical(profile, "closeout")) 5L else 3L
  default_warmup <- if (identical(profile, "closeout")) 2L else 1L
  default_memory_reps <- if (identical(profile, "closeout")) 3L else 1L
  reps <- bench_int_arg(args, "reps", default_reps, minimum = 1L)
  warmup <- bench_int_arg(args, "warmup", default_warmup, minimum = 0L)
  memory_reps <- bench_int_arg(args, "memory-reps", default_memory_reps, minimum = 1L)
  seed <- bench_int_arg(args, "seed", 20260723L, minimum = 1L)
  enforce <- bench_has_flag(args, "enforce") ||
    identical(tolower(Sys.getenv("SITEMIX_BENCH_ENFORCE")), "true")
  root <- bench_root()
  out_dir <- bench_arg_value(
    args,
    "out-dir",
    file.path(tempdir(), paste0("sitemix-performance-", profile))
  )
  out_dir <- bench_output_target(out_dir)
  threads <- bench_thread_state()
  bench_load_package(root)

  canonical_budget_path <- file.path(root, "inst", "gates", "performance-budget.csv")
  budget_path <- bench_arg_value(args, "budget", canonical_budget_path)
  budget <- bench_read_budget(budget_path)
  runtime_path <- file.path(root, "inst", "gates", "performance-runtime.csv")
  runtime_reference <- bench_read_runtime_reference(runtime_path)
  counts_path <- bench_counts_path(root)
  counts <- readRDS(counts_path)
  runtime_current <- bench_current_runtime(counts_path, threads)
  runtime_compatible <- bench_runtime_compatible(runtime_current, runtime_reference)
  authority <- bench_validate_authority(
    profile,
    enforce,
    reps,
    warmup,
    memory_reps,
    seed,
    budget_path,
    canonical_budget_path,
    runtime_path,
    runtime_compatible
  )
  cases <- bench_build_cases(counts, budget)
  set.seed(seed)

  timing_results <- do.call(rbind, lapply(budget$case, function(case) {
    row <- budget[budget$case == case, , drop = FALSE]
    bench_run_timing_case(
      case = case,
      fun = cases[[case]],
      warmup = warmup,
      reps = reps,
      iterations = row$iterations[[1L]],
      K = row$K[[1L]],
      has_K = row$expected_has_K[[1L]]
    )
  }))
  row.names(timing_results) <- NULL
  timing_summary <- bench_timing_summary(timing_results)

  memory_results <- do.call(rbind, lapply(budget$case, function(case) {
    row <- budget[budget$case == case, , drop = FALSE]
    bench_run_memory_case(
      case,
      cases[[case]],
      reps = memory_reps,
      K = row$K[[1L]],
      has_K = row$expected_has_K[[1L]]
    )
  }))
  row.names(memory_results) <- NULL
  memory_summary <- bench_memory_summary(memory_results)
  reference_signature_checked <- runtime_compatible && authority$budget_authoritative
  decisions <- bench_gate_decisions(
    timing_summary,
    memory_summary,
    budget,
    numeric_blocking = authority$numeric_blocking,
    reference_signature_checked = reference_signature_checked
  )
  structural_failures <- sum(decisions$structural_failure)
  numeric_hard_breaches <- sum(decisions$hard_numeric)
  unstable_observations <- sum(decisions$unstable)
  blocking_hard_failures <- sum(decisions$numeric_blocking & decisions$hard_numeric)
  blocking_unstable_cases <- sum(decisions$numeric_blocking & decisions$unstable)
  advisory_cases <- sum(decisions$case_status == "ADVISORY")
  status <- if (structural_failures) {
    "FAIL"
  } else if (blocking_unstable_cases) {
    "UNSTABLE"
  } else if (blocking_hard_failures) {
    "FAIL"
  } else if (advisory_cases) {
    "ADVISORY"
  } else {
    "PASS"
  }
  gate_summary <- data.frame(
    profile = profile,
    status = status,
    numeric_blocking = authority$numeric_blocking,
    runtime_compatible = runtime_compatible,
    budget_authoritative = authority$budget_authoritative,
    reference_signature_checked = reference_signature_checked,
    threshold_basis = .bench_threshold_basis,
    cases = nrow(decisions),
    structural_failures = structural_failures,
    numeric_hard_breaches = numeric_hard_breaches,
    unstable_observations = unstable_observations,
    blocking_hard_failures = blocking_hard_failures,
    blocking_unstable_cases = blocking_unstable_cases,
    advisory_cases = advisory_cases,
    allocation_calibration_cases = sum(decisions$allocation_status == "CALIBRATION_ONLY"),
    gc_heap_counters_descriptive_only = TRUE,
    peak_rss_blocking = FALSE,
    peak_rss_measured = FALSE,
    peak_rss_calibration_required = TRUE,
    stringsAsFactors = FALSE
  )
  completed_utc <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  metadata <- bench_metadata(
    profile,
    warmup,
    reps,
    memory_reps,
    seed,
    budget_path,
    runtime_path,
    runtime_current,
    runtime_reference,
    runtime_compatible,
    authority$budget_authoritative,
    started_utc,
    completed_utc
  )
  source_hashes <- bench_source_hashes(root, budget_path, runtime_path, counts_path)
  artifact_dir <- bench_write_outputs(
    out_dir,
    timing_results,
    timing_summary,
    memory_results,
    memory_summary,
    decisions,
    metadata,
    source_hashes,
    budget,
    runtime_current,
    runtime_reference,
    gate_summary
  )

  cat("sitemix performance and memory smoke\n")
  print(gate_summary, row.names = FALSE)
  print(decisions[c(
    "case", "median_elapsed_seconds", "robust_cv", "max_median_ratio",
    "timing_status", "object_status", "serialized_status", "allocation_status",
    "case_status"
  )], row.names = FALSE)
  cat("Artifacts: ", artifact_dir, "\n", sep = "")

  bench_exit_status(
    structural_failures,
    blocking_unstable_cases,
    blocking_hard_failures
  )
}

bench_entry <- function() {
  tryCatch(
    bench_main(),
    error = function(e) {
      message("performance smoke contract error: ", conditionMessage(e))
      2L
    }
  )
}

if (sys.nframe() == 0L) {
  quit(save = "no", status = bench_entry(), runLast = FALSE)
}
