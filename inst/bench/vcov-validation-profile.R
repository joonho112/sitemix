#!/usr/bin/env Rscript

# Profile repeated Scenario B covariance validation without changing package
# behavior. Timing, tracing, and Rprof sampling run in separate passes.

sm_vp_root <- function() {
  command <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command, value = TRUE)
  candidates <- character()
  if (length(file_arg)) {
    script <- sub("^--file=", "", file_arg[[1L]])
    candidates <- c(file.path(dirname(script), "..", ".."), getwd())
  } else {
    candidates <- getwd()
  }
  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "DESCRIPTION")) &&
          dir.exists(file.path(candidate, "R"))) {
      return(normalizePath(candidate, mustWork = TRUE))
    }
  }
  stop("Could not locate the sitemix source tree.", call. = FALSE)
}

sm_vp_parse_integer <- function(value, name, lower, upper) {
  number <- suppressWarnings(as.numeric(value))
  integer <- suppressWarnings(as.integer(number))
  if (length(number) != 1L || is.na(number) || !is.finite(number) ||
        is.na(integer) || number != integer || integer < lower || integer > upper) {
    stop("`", name, "` must be an integer in [", lower, ", ", upper, "].", call. = FALSE)
  }
  integer
}

sm_vp_parse_number <- function(value, name, lower, upper) {
  number <- suppressWarnings(as.numeric(value))
  if (length(number) != 1L || is.na(number) || !is.finite(number) ||
        number < lower || number > upper) {
    stop("`", name, "` must be in [", lower, ", ", upper, "].", call. = FALSE)
  }
  number
}

sm_vp_parse_args <- function(args) {
  out <- list(
    out_dir = file.path(tempdir(), "sitemix-step64-profile"),
    warmup = 1L,
    reps = 2L,
    seed = 20260713L,
    rprof_interval = 0.001,
    profile = NULL,
    before_dir = NULL
  )
  seen <- character()
  for (arg in args) {
    key <- sub("=.*$", "", arg)
    if (key %in% seen) {
      stop("Duplicate argument: ", key, call. = FALSE)
    }
    seen <- c(seen, key)
    if (startsWith(arg, "--out-dir=")) {
      out$out_dir <- sub("^--out-dir=", "", arg)
    } else if (startsWith(arg, "--warmup=")) {
      out$warmup <- sm_vp_parse_integer(sub("^--warmup=", "", arg), "warmup", 0L, 2L)
    } else if (startsWith(arg, "--reps=")) {
      out$reps <- sm_vp_parse_integer(sub("^--reps=", "", arg), "reps", 1L, 5L)
    } else if (startsWith(arg, "--seed=")) {
      out$seed <- sm_vp_parse_integer(sub("^--seed=", "", arg), "seed", 1L, 2147483644L)
    } else if (startsWith(arg, "--rprof-interval=")) {
      out$rprof_interval <- sm_vp_parse_number(
        sub("^--rprof-interval=", "", arg),
        "rprof-interval",
        0.0005,
        0.1
      )
    } else if (startsWith(arg, "--profile=")) {
      out$profile <- sub("^--profile=", "", arg)
    } else if (startsWith(arg, "--before-dir=")) {
      out$before_dir <- sub("^--before-dir=", "", arg)
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
  }
  if (!nzchar(out$out_dir)) {
    stop("`out-dir` must not be empty.", call. = FALSE)
  }
  if (is.null(out$profile) || !out$profile %in% c("before", "optimized")) {
    stop("`profile` must be explicitly set to `before` or `optimized`.", call. = FALSE)
  }
  if (identical(out$profile, "optimized") &&
        (is.null(out$before_dir) || !nzchar(out$before_dir))) {
    stop("`before-dir` is required for the optimized profile.", call. = FALSE)
  }
  out
}

sm_vp_require_runtime <- function(root, out_dir, before_dir = NULL) {
  if (getRversion() < "4.1.0") {
    stop("R >= 4.1.0 is required.", call. = FALSE)
  }
  if ("sitemix" %in% loadedNamespaces()) {
    stop("Run the profiler in a fresh R process.", call. = FALSE)
  }
  required <- c("pkgload", "Matrix", "tibble", "vctrs")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing profiling dependencies: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  root <- paste0(normalizePath(root, mustWork = TRUE), .Platform$file.sep)
  output <- paste0(normalizePath(out_dir, mustWork = TRUE), .Platform$file.sep)
  if (startsWith(output, root)) {
    stop("Profiling artifacts must be outside the source tree.", call. = FALSE)
  }
  if (!is.null(before_dir) && !dir.exists(before_dir)) {
    stop("The before-profile directory does not exist: ", before_dir, call. = FALSE)
  }
  invisible(TRUE)
}

sm_vp_case_specs <- function() {
  data.frame(
    case = c("small", "medium", "large"),
    K = c(2L, 8L, 16L),
    S = c(5L, 25L, 100L),
    n = c(64L, 128L, 128L),
    stringsAsFactors = FALSE
  )
}

sm_vp_step64_source_manifest <- function() {
  data.frame(
    path = c(
      "R/engine-multivariate.R", "R/output-assembly.R",
      "R/sitemix_estimates.R", "R/sm_estimate.R", "R/sm_vcov.R",
      "R/sur-covariance.R", "R/validate-input.R",
      "inst/bench/vcov-validation-profile.R"
    ),
    checksum = c(
      "e7e0d542198a6821155f45d630667144",
      "bf1c33c2f440fad890fc87b1bbd83d95",
      "9d2daea1d27bb3a7f3fa78d21aa642ac",
      "eeea66e3bc64069c0ab143740bad857b",
      "4685565816452754cec21df54d1b45ce",
      "1cf8f7d81972af36f0901ace5715e8db",
      "a954f6ae9651921bdb9828267b62db34",
      "498c85aad0803a9e3ae415fb1eb9455b"
    ),
    stringsAsFactors = FALSE
  )
}

sm_vp_make_case <- function(spec, seed) {
  set.seed(seed)
  indicators <- sprintf("x%02d", seq_len(spec$K))
  site_ids <- sprintf("site-%03d", seq_len(spec$S))
  data <- data.frame(
    site_id = rep(site_ids, each = spec$n),
    year = rep(2024L, spec$S * spec$n),
    stringsAsFactors = FALSE
  )
  counts <- matrix(0L, nrow = spec$S, ncol = spec$K)
  for (site in seq_len(spec$S)) {
    rows <- ((site - 1L) * spec$n + 1L):(site * spec$n)
    for (coordinate in seq_len(spec$K)) {
      probability <- 0.15 + 0.7 * coordinate / (spec$K + 1L)
      probability <- min(0.9, max(0.1, probability + 0.04 * sin(site + coordinate)))
      count <- max(1L, min(spec$n - 1L, as.integer(round(spec$n * probability))))
      values <- integer(spec$n)
      values[sample.int(spec$n, count)] <- 1L
      if (!indicators[[coordinate]] %in% names(data)) {
        data[[indicators[[coordinate]]]] <- integer(nrow(data))
      }
      data[[indicators[[coordinate]]]][rows] <- values
      counts[site, coordinate] <- count
    }
  }
  stopifnot(all(counts > 0L), all(counts < spec$n))
  list(data = data, indicators = indicators, counts = counts, spec = spec)
}

sm_vp_package_function <- function(name) {
  get(name, envir = asNamespace("sitemix"), inherits = FALSE)
}

sm_vp_public_estimate <- function(case) {
  estimate <- getExportedValue("sitemix", "sm_estimate")
  estimate(
    case$data,
    family = "multivariate",
    indicators = case$indicators,
    vjt = TRUE,
    vst = "none",
    boundary_method = "none",
    min_n = 1L,
    accountability_n = 1L
  )
}

sm_vp_construct_unique <- function(case) {
  sur_from_matrix <- sm_vp_package_function(".sm_multivariate_sur_from_matrix")
  vcov_from_sur <- sm_vp_package_function(".sm_multivariate_vcov_from_sur")
  groups <- split(case$data, case$data$site_id)
  lapply(groups, function(group) {
    sur <- sur_from_matrix(
      group[case$indicators],
      indicators = case$indicators,
      boundary_method = "none"
    )
    vcov_from_sur(
      sur,
      site_id = group$site_id[[1L]],
      year = group$year[[1L]],
      indicators = case$indicators,
      estimate_scale = "none"
    )
  })
}

sm_vp_prepare_context <- function(case) {
  output <- sm_vp_public_estimate(case)
  group_key <- paste(output$site_id, output$year, sep = "\r")
  first <- !duplicated(group_key)
  unique_v <- output$V[first]
  plain <- tibble::as_tibble(output)
  row_list <- lapply(seq_len(nrow(plain)), function(i) plain[i, , drop = FALSE])
  list(case = case, output = output, unique_v = unique_v, row_list = row_list)
}

sm_vp_stage_functions <- function(context) {
  list(
    construct_unique = function() sm_vp_construct_unique(context$case),
    assemble_repeated = function() {
      sm_vp_package_function(".sm_bind_sitemix_rows")(
        context$row_list,
        family = "multivariate",
        sitemix_role = "summary_uncertainty"
      )
    },
    validate_output = function() {
      sm_vp_package_function("validate.sitemix_estimates")(context$output)
    },
    as_matrix_unique = function() {
      lapply(context$unique_v, sm_vp_package_function("as.matrix.sm_vcov"))
    },
    as_matrix_repeated = function() {
      lapply(context$output$V, sm_vp_package_function("as.matrix.sm_vcov"))
    },
    end_to_end_public = function() sm_vp_public_estimate(context$case)
  )
}

sm_vp_md5_object <- function(object) {
  path <- tempfile("sitemix-vp-result-", fileext = ".rds")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  saveRDS(object, path, version = 2L, compress = FALSE)
  unname(tools::md5sum(path))
}

sm_vp_serialized_size <- function(object) {
  length(serialize(object, NULL, version = 2L))
}

sm_vp_gc_value <- function(gc_state, row, column) {
  index <- match(column, colnames(gc_state))
  if (is.na(index)) {
    return(NA_real_)
  }
  unname(gc_state[row, index])
}

sm_vp_measure_stage <- function(case_name, stage, fun, warmup, reps) {
  if (warmup > 0L) {
    for (i in seq_len(warmup)) {
      invisible(fun())
    }
  }
  timing_rows <- vector("list", reps)
  gc_rows <- vector("list", reps)
  last_result <- NULL
  for (replicate in seq_len(reps)) {
    invisible(gc(reset = TRUE))
    timing <- system.time(last_result <- fun())
    gc_state <- gc()
    timing_rows[[replicate]] <- data.frame(
      case = case_name,
      stage = stage,
      replicate = replicate,
      user_seconds = unname(timing[["user.self"]]),
      system_seconds = unname(timing[["sys.self"]]),
      elapsed_seconds = unname(timing[["elapsed"]]),
      stringsAsFactors = FALSE
    )
    gc_rows[[replicate]] <- data.frame(
      case = case_name,
      stage = stage,
      replicate = replicate,
      ncells_used = sm_vp_gc_value(gc_state, 1L, "used"),
      vcells_used = sm_vp_gc_value(gc_state, 2L, "used"),
      ncells_gc_trigger = sm_vp_gc_value(gc_state, 1L, "gc trigger"),
      vcells_gc_trigger = sm_vp_gc_value(gc_state, 2L, "gc trigger"),
      ncells_limit_mb = sm_vp_gc_value(gc_state, 1L, "limit (Mb)"),
      vcells_limit_mb = sm_vp_gc_value(gc_state, 2L, "limit (Mb)"),
      ncells_max_used = sm_vp_gc_value(gc_state, 1L, "max used"),
      vcells_max_used = sm_vp_gc_value(gc_state, 2L, "max used"),
      stringsAsFactors = FALSE
    )
  }
  list(
    timing = do.call(rbind, timing_rows),
    gc = do.call(rbind, gc_rows),
    result = last_result
  )
}

sm_vp_trace_targets <- function() {
  data.frame(
    target = c(
      "validate_internal", "validate_alias", "as_matrix", "psd",
      "psd_tolerance", "rank", "eigen", "alignment", "repeated_v",
      "equality", "bind", "vec_rbind", "rank_matrix", "validate_output",
      "validate_vcov_column", "constructor"
    ),
    function_name = c(
      ".sm_validate_sm_vcov", "validate.sm_vcov", "as.matrix.sm_vcov",
      ".sm_validate_vcov_psd", ".sm_psd_tolerance", ".sm_matrix_rank",
      "eigen", ".sm_validate_output_vcov_alignment",
      ".sm_validate_repeated_v", ".sm_vcov_value_equal",
      ".sm_bind_sitemix_rows", "vec_rbind", "rankMatrix",
      "validate.sitemix_estimates", ".sm_validate_sitemix_vcov", "sm_vcov"
    ),
    namespace = c(
      rep("sitemix", 6L), "base", rep("sitemix", 4L), "vctrs", "Matrix",
      rep("sitemix", 3L)
    ),
    stringsAsFactors = FALSE
  )
}

sm_vp_trace_expression <- function(key) {
  substitute(
    assign(
      KEY,
      get(KEY, envir = get(".sitemix_vp_counter", envir = .GlobalEnv)) + 1L,
      envir = get(".sitemix_vp_counter", envir = .GlobalEnv)
    ),
    list(KEY = key)
  )
}

sm_vp_untrace <- function(targets, installed) {
  for (i in rev(installed)) {
    invisible(utils::capture.output(try(
      untrace(
        targets$function_name[[i]],
        where = asNamespace(targets$namespace[[i]])
      ),
      silent = TRUE
    )))
  }
  invisible(TRUE)
}

sm_vp_trace_stage <- function(fun, targets) {
  counter <- new.env(parent = emptyenv())
  for (key in targets$target) {
    counter[[key]] <- 0L
  }
  assign(".sitemix_vp_counter", counter, envir = .GlobalEnv)
  on.exit(rm(".sitemix_vp_counter", envir = .GlobalEnv), add = TRUE)
  installed <- integer()
  on.exit(sm_vp_untrace(targets, installed), add = TRUE)
  for (i in seq_len(nrow(targets))) {
    invisible(utils::capture.output(trace(
      targets$function_name[[i]],
      tracer = sm_vp_trace_expression(targets$target[[i]]),
      where = asNamespace(targets$namespace[[i]]),
      print = FALSE
    )))
    installed <- c(installed, i)
  }
  result <- fun()
  counts <- vapply(targets$target, function(key) counter[[key]], integer(1))
  list(result = result, counts = counts)
}

sm_vp_zero_counts <- function(targets) {
  stats::setNames(integer(nrow(targets)), targets$target)
}

sm_vp_expected_counts_before <- function(spec, stage, targets) {
  S <- spec$S
  K <- spec$K
  SK <- S * K
  expected <- sm_vp_zero_counts(targets)
  if (stage == "construct_unique") {
    expected[c("validate_internal", "constructor")] <- S
    expected[c("psd", "psd_tolerance", "rank")] <- 2L * S
    expected[["eigen"]] <- 6L * S
  } else if (stage == "assemble_repeated") {
    expected[["validate_internal"]] <- SK
    expected[["validate_alias"]] <- 2L * SK
    expected[["as_matrix"]] <- SK
    expected[c("psd", "psd_tolerance", "rank")] <- 3L * SK
    expected[["eigen"]] <- 9L * SK
    expected[c("alignment", "bind")] <- c(2L, 1L)
    expected[c("repeated_v", "equality")] <- c(1L, S * (K - 1L))
    expected[c("vec_rbind", "validate_output", "validate_vcov_column")] <- 1L
  } else if (stage == "validate_output") {
    expected[c("validate_internal", "validate_alias", "as_matrix")] <- SK
    expected[c("psd", "psd_tolerance", "rank")] <- 2L * SK
    expected[["eigen"]] <- 6L * SK
    expected[c("alignment", "repeated_v")] <- 1L
    expected[["equality"]] <- S * (K - 1L)
    expected[c("validate_output", "validate_vcov_column")] <- 1L
  } else if (stage == "as_matrix_unique") {
    expected[c("validate_internal", "as_matrix", "psd", "psd_tolerance", "rank")] <- S
    expected[["eigen"]] <- 3L * S
  } else if (stage == "as_matrix_repeated") {
    expected[c("validate_internal", "as_matrix", "psd", "psd_tolerance", "rank")] <- SK
    expected[["eigen"]] <- 3L * SK
  } else if (stage == "end_to_end_public") {
    expected[["validate_internal"]] <- S + SK
    expected[["validate_alias"]] <- 2L * SK
    expected[["as_matrix"]] <- SK
    expected[c("psd", "psd_tolerance", "rank")] <- 2L * S + 3L * SK
    expected[["eigen"]] <- 6L * S + 9L * SK
    expected[c("alignment", "bind", "vec_rbind")] <- c(2L, 1L, 1L)
    expected[c("repeated_v", "equality")] <- c(1L, S * (K - 1L))
    expected[c("validate_output", "validate_vcov_column", "constructor")] <- c(1L, 1L, S)
  } else {
    stop("Unknown stage: ", stage, call. = FALSE)
  }
  expected
}

sm_vp_expected_counts_optimized <- function(spec, stage, targets) {
  S <- spec$S
  K <- spec$K
  SK <- S * K
  expected <- sm_vp_zero_counts(targets)
  if (stage == "construct_unique") {
    expected[c("validate_internal", "constructor")] <- S
    expected[c("psd", "psd_tolerance", "rank")] <- 2L * S
    expected[["eigen"]] <- 6L * S
  } else if (stage == "assemble_repeated") {
    expected[["validate_alias"]] <- 2L * S
    expected[c("psd", "psd_tolerance", "rank")] <- 2L * S
    expected[["eigen"]] <- 6L * S
    expected[c("alignment", "bind")] <- c(2L, 1L)
    expected[c("repeated_v", "equality")] <- c(1L, 3L * S * (K - 1L))
    expected[c("vec_rbind", "validate_output", "validate_vcov_column")] <- 1L
  } else if (stage == "validate_output") {
    expected[["validate_alias"]] <- S
    expected[c("psd", "psd_tolerance", "rank")] <- S
    expected[["eigen"]] <- 3L * S
    expected[c("alignment", "repeated_v")] <- 1L
    expected[["equality"]] <- 2L * S * (K - 1L)
    expected[c("validate_output", "validate_vcov_column")] <- 1L
  } else if (stage == "as_matrix_unique") {
    expected[c("validate_internal", "as_matrix", "psd", "psd_tolerance", "rank")] <- S
    expected[["eigen"]] <- 3L * S
  } else if (stage == "as_matrix_repeated") {
    expected[c("validate_internal", "as_matrix", "psd", "psd_tolerance", "rank")] <- SK
    expected[["eigen"]] <- 3L * SK
  } else if (stage == "end_to_end_public") {
    expected[["validate_internal"]] <- S
    expected[["validate_alias"]] <- 2L * S
    expected[c("psd", "psd_tolerance", "rank")] <- 4L * S
    expected[["eigen"]] <- 12L * S
    expected[c("alignment", "bind", "vec_rbind")] <- c(2L, 1L, 1L)
    expected[c("repeated_v", "equality")] <- c(1L, 3L * S * (K - 1L))
    expected[c("validate_output", "validate_vcov_column", "constructor")] <- c(1L, 1L, S)
  } else {
    stop("Unknown stage: ", stage, call. = FALSE)
  }
  expected
}

sm_vp_expected_counts <- function(spec, stage, targets, profile) {
  if (identical(profile, "before")) {
    return(sm_vp_expected_counts_before(spec, stage, targets))
  }
  sm_vp_expected_counts_optimized(spec, stage, targets)
}

sm_vp_trace_rows <- function(case, stage, traced, targets, profile) {
  expected <- sm_vp_expected_counts(case$spec, stage, targets, profile)
  data.frame(
    expectation_profile = profile,
    case = case$spec$case,
    K = case$spec$K,
    S = case$spec$S,
    n = case$spec$n,
    stage = stage,
    target = names(expected),
    observed = unname(traced$counts[names(expected)]),
    expected = unname(expected),
    formula_match = unname(traced$counts[names(expected)]) == unname(expected),
    stringsAsFactors = FALSE
  )
}

sm_vp_formula_rows <- function(case, stage, trace_rows, profile) {
  if (stage != "end_to_end_public") {
    return(NULL)
  }
  S <- case$spec$S
  K <- case$spec$K
  observed <- stats::setNames(trace_rows$observed, trace_rows$target)
  metrics <- c("validation", "eigen", "as_matrix", "psd", "rank", "equality")
  values <- c(
    sum(observed[c("validate_internal", "validate_alias")]),
    observed[["eigen"]], observed[["as_matrix"]], observed[["psd"]],
    observed[["rank"]], observed[["equality"]]
  )
  if (identical(profile, "before")) {
    formulas <- c(
      "S + 3*S*K", "6*S + 9*S*K", "S*K", "2*S + 3*S*K",
      "2*S + 3*S*K", "S*(K - 1)"
    )
    expected <- c(
      S + 3L * S * K, 6L * S + 9L * S * K, S * K,
      2L * S + 3L * S * K, 2L * S + 3L * S * K, S * (K - 1L)
    )
  } else {
    formulas <- c("3*S", "12*S", "0", "4*S", "4*S", "3*S*(K - 1)")
    expected <- c(3L * S, 12L * S, 0L, 4L * S, 4L * S, 3L * S * (K - 1L))
  }
  data.frame(
    expectation_profile = profile,
    case = case$spec$case,
    metric = metrics,
    formula = formulas,
    observed = unname(values),
    expected = unname(expected),
    formula_match = unname(values) == unname(expected),
    stringsAsFactors = FALSE
  )
}

sm_vp_timing_summary <- function(timings) {
  groups <- split(timings, paste(timings$case, timings$stage, sep = "\r"))
  rows <- lapply(groups, function(group) {
    data.frame(
      case = group$case[[1L]],
      stage = group$stage[[1L]],
      reps = nrow(group),
      median_elapsed_seconds = stats::median(group$elapsed_seconds),
      min_elapsed_seconds = min(group$elapsed_seconds),
      max_elapsed_seconds = max(group$elapsed_seconds),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[order(out$case, out$stage), , drop = FALSE]
}

sm_vp_object_sizes <- function(case, context) {
  objects <- list(
    input_data = case$data,
    unique_v = context$unique_v,
    repeated_output = context$output,
    one_matrix = context$unique_v[[1L]]$matrix
  )
  rows <- lapply(names(objects), function(name) {
    object <- objects[[name]]
    data.frame(
      case = case$spec$case,
      object = name,
      object_size_bytes = as.numeric(utils::object.size(object)),
      serialized_size_bytes = sm_vp_serialized_size(object),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

sm_vp_profile_rprof <- function(case_name, fun, out_dir, interval) {
  path <- file.path(out_dir, paste0("rprof-", case_name, ".out"))
  Rprof(path, interval = interval, memory.profiling = TRUE)
  on.exit(Rprof(NULL), add = TRUE)
  invisible(fun())
  Rprof(NULL)
  summary <- summaryRprof(path, memory = "both")$by.total
  if (is.null(summary) || !nrow(summary)) {
    return(data.frame(
      case = character(), rank = integer(), function_name = character(),
      total_time = numeric(), total_percent = numeric(),
      self_time = numeric(), self_percent = numeric()
    ))
  }
  summary <- summary[seq_len(min(25L, nrow(summary))), , drop = FALSE]
  data.frame(
    case = case_name,
    rank = seq_len(nrow(summary)),
    function_name = rownames(summary),
    total_time = summary$total.time,
    total_percent = summary$total.pct,
    self_time = summary$self.time,
    self_percent = summary$self.pct,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

sm_vp_source_hashes <- function(root) {
  relative <- c(
    "R/engine-multivariate.R", "R/output-assembly.R",
    "R/sitemix_estimates.R", "R/sm_estimate.R", "R/sm_vcov.R",
    "R/sur-covariance.R", "R/validate-input.R",
    "inst/bench/vcov-validation-profile.R"
  )
  paths <- file.path(root, relative)
  stopifnot(all(file.exists(paths)))
  data.frame(
    path = relative,
    algorithm = "MD5",
    checksum = unname(tools::md5sum(paths)),
    stringsAsFactors = FALSE
  )
}

sm_vp_runtime_environment <- function() {
  session <- utils::sessionInfo()
  thread_vars <- c(
    "OMP_NUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS",
    "VECLIB_MAXIMUM_THREADS", "BLIS_NUM_THREADS",
    "RCPP_PARALLEL_NUM_THREADS"
  )
  values <- Sys.getenv(thread_vars, unset = "<unset>")
  data.frame(
    key = c(
      "R.version", "platform", "BLAS", "LAPACK", "detected_cores",
      thread_vars
    ),
    value = c(
      R.version.string,
      R.version$platform,
      if (is.null(session$BLAS)) "<unknown>" else session$BLAS,
      if (is.null(session$LAPACK)) "<unknown>" else session$LAPACK,
      as.character(parallel::detectCores()),
      unname(values)
    ),
    stringsAsFactors = FALSE
  )
}

sm_vp_write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, na = "")
}

sm_vp_collect_case <- function(case, options, targets, out_dir) {
  context <- sm_vp_prepare_context(case)
  stages <- sm_vp_stage_functions(context)
  timing_rows <- list()
  gc_rows <- list()
  trace_rows <- list()
  signature_rows <- list()
  formula_rows <- list()
  index <- 0L
  for (stage in names(stages)) {
    index <- index + 1L
    measured <- sm_vp_measure_stage(
      case$spec$case,
      stage,
      stages[[stage]],
      options$warmup,
      options$reps
    )
    traced <- sm_vp_trace_stage(stages[[stage]], targets)
    counts <- sm_vp_trace_rows(case, stage, traced, targets, options$profile)
    reference_md5 <- sm_vp_md5_object(measured$result)
    traced_md5 <- sm_vp_md5_object(traced$result)
    timing_rows[[index]] <- measured$timing
    gc_rows[[index]] <- measured$gc
    trace_rows[[index]] <- counts
    signature_rows[[index]] <- data.frame(
      case = case$spec$case,
      stage = stage,
      reference_md5 = reference_md5,
      traced_md5 = traced_md5,
      identical = identical(measured$result, traced$result),
      stringsAsFactors = FALSE
    )
    formula_rows[[index]] <- sm_vp_formula_rows(
      case,
      stage,
      counts,
      options$profile
    )
  }
  rprof <- sm_vp_profile_rprof(
    case$spec$case,
    stages$end_to_end_public,
    out_dir,
    options$rprof_interval
  )
  list(
    timings = do.call(rbind, timing_rows),
    gc = do.call(rbind, gc_rows),
    trace = do.call(rbind, trace_rows),
    signatures = do.call(rbind, signature_rows),
    formulas = do.call(rbind, formula_rows),
    sizes = sm_vp_object_sizes(case, context),
    rprof = rprof,
    min_count = min(case$counts),
    max_count = max(case$counts),
    all_interior = all(case$counts > 0L & case$counts < case$spec$n)
  )
}

sm_vp_bind_component <- function(results, name) {
  do.call(rbind, lapply(results, `[[`, name))
}

sm_vp_read_before_profile <- function(directory, options, specs) {
  required <- c(
    "profile-evidence.rds", "profile-summary.csv", "timing-summary.csv",
    "trace-counts.csv", "source-hashes.csv"
  )
  paths <- file.path(directory, required)
  if (!all(file.exists(paths))) {
    stop("The before profile is incomplete: ", directory, call. = FALSE)
  }
  before <- readRDS(paths[[1L]])
  manifest <- sm_vp_step64_source_manifest()
  matched <- match(manifest$path, before$source_hashes$path)
  source_match <- !anyNA(matched) && identical(
    before$source_hashes$checksum[matched],
    manifest$checksum
  )
  before_specs <- before$case_specs[c("case", "K", "S", "n", "seed")]
  after_specs <- specs[c("case", "K", "S", "n", "seed")]
  checks <- data.frame(
    check = c(
      "status", "formula_failures", "signature_failures", "case_specs",
      "warmup", "reps", "seed", "timing_tracing_separated",
      "step64_source_manifest"
    ),
    pass = c(
      identical(before$summary$status, "PASS"),
      identical(before$summary$formula_failures, 0L),
      identical(before$summary$signature_failures, 0L),
      identical(before_specs, after_specs),
      identical(before$metadata$warmup, options$warmup),
      identical(before$metadata$reps, options$reps),
      identical(before$metadata$seed, options$seed),
      isTRUE(before$metadata$timing_tracing_separated),
      source_match
    ),
    stringsAsFactors = FALSE
  )
  if (!all(checks$pass)) {
    failed <- paste(checks$check[!checks$pass], collapse = ", ")
    stop("The before profile failed compatibility checks: ", failed, call. = FALSE)
  }
  list(
    evidence = before,
    checks = checks,
    source_hashes = before$source_hashes,
    artifacts = data.frame(
      file = required,
      bytes = file.info(paths)$size,
      md5 = unname(tools::md5sum(paths)),
      stringsAsFactors = FALSE
    )
  )
}

sm_vp_ratio <- function(after, before) {
  ifelse(before == 0, ifelse(after == 0, 1, Inf), after / before)
}

sm_vp_compare_timing <- function(before, after) {
  old <- before[c("case", "stage", "reps", "median_elapsed_seconds")]
  names(old)[3:4] <- c("before_reps", "before_median_seconds")
  new <- after[c("case", "stage", "reps", "median_elapsed_seconds")]
  names(new)[3:4] <- c("optimized_reps", "optimized_median_seconds")
  out <- merge(old, new, by = c("case", "stage"), sort = TRUE)
  out$optimized_to_before_ratio <- sm_vp_ratio(
    out$optimized_median_seconds,
    out$before_median_seconds
  )
  out$percent_change <- 100 * (out$optimized_to_before_ratio - 1)
  out
}

sm_vp_end_to_end_metrics <- function(trace, profile) {
  trace <- trace[trace$stage == "end_to_end_public", , drop = FALSE]
  groups <- split(trace, trace$case)
  rows <- lapply(groups, function(group) {
    observed <- stats::setNames(group$observed, group$target)
    data.frame(
      profile = profile,
      case = group$case[[1L]],
      K = group$K[[1L]],
      S = group$S[[1L]],
      metric = c("validation", "eigen", "as_matrix", "psd", "rank", "equality"),
      observed = c(
        sum(observed[c("validate_internal", "validate_alias")]),
        observed[["eigen"]], observed[["as_matrix"]], observed[["psd"]],
        observed[["rank"]], observed[["equality"]]
      ),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

sm_vp_compare_counts <- function(before, after) {
  keys <- c("case", "K", "S", "n", "stage", "target")
  old <- before[c(keys, "observed")]
  names(old)[ncol(old)] <- "before_observed"
  new <- after[c(keys, "observed")]
  names(new)[ncol(new)] <- "optimized_observed"
  out <- merge(old, new, by = keys, sort = TRUE)
  out$optimized_to_before_ratio <- sm_vp_ratio(
    out$optimized_observed,
    out$before_observed
  )
  out$percent_reduction <- 100 * (1 - out$optimized_to_before_ratio)
  out
}

sm_vp_compare_end_to_end <- function(before, after) {
  old <- sm_vp_end_to_end_metrics(before, "before")
  new <- sm_vp_end_to_end_metrics(after, "optimized")
  keys <- c("case", "K", "S", "metric")
  old <- old[c(keys, "observed")]
  new <- new[c(keys, "observed")]
  names(old)[ncol(old)] <- "before_observed"
  names(new)[ncol(new)] <- "optimized_observed"
  out <- merge(old, new, by = keys, sort = TRUE)
  out$optimized_to_before_ratio <- sm_vp_ratio(
    out$optimized_observed,
    out$before_observed
  )
  out$percent_reduction <- 100 * (1 - out$optimized_to_before_ratio)
  out
}

sm_vp_write_artifacts <- function(evidence, out_dir) {
  sm_vp_write_csv(evidence$case_specs, file.path(out_dir, "case-specs.csv"))
  sm_vp_write_csv(evidence$metadata, file.path(out_dir, "run-metadata.csv"))
  sm_vp_write_csv(evidence$timings, file.path(out_dir, "timings.csv"))
  sm_vp_write_csv(evidence$timing_summary, file.path(out_dir, "timing-summary.csv"))
  sm_vp_write_csv(evidence$gc, file.path(out_dir, "gc-summary.csv"))
  sm_vp_write_csv(evidence$trace, file.path(out_dir, "trace-counts.csv"))
  sm_vp_write_csv(evidence$formulas, file.path(out_dir, "formula-checks.csv"))
  sm_vp_write_csv(evidence$signatures, file.path(out_dir, "result-signatures.csv"))
  sm_vp_write_csv(evidence$sizes, file.path(out_dir, "object-sizes.csv"))
  sm_vp_write_csv(evidence$rprof, file.path(out_dir, "rprof-summary.csv"))
  sm_vp_write_csv(evidence$source_hashes, file.path(out_dir, "source-hashes.csv"))
  sm_vp_write_csv(evidence$runtime, file.path(out_dir, "runtime-environment.csv"))
  sm_vp_write_csv(evidence$summary, file.path(out_dir, "profile-summary.csv"))
  sm_vp_write_csv(
    evidence$before_checks,
    file.path(out_dir, "before-compatibility-checks.csv")
  )
  sm_vp_write_csv(
    evidence$before_artifacts,
    file.path(out_dir, "before-artifacts.csv")
  )
  sm_vp_write_csv(
    evidence$before_source_hashes,
    file.path(out_dir, "before-source-hashes.csv")
  )
  sm_vp_write_csv(
    evidence$timing_comparison,
    file.path(out_dir, "before-after-timing.csv")
  )
  sm_vp_write_csv(
    evidence$count_comparison,
    file.path(out_dir, "before-after-trace-counts.csv")
  )
  sm_vp_write_csv(
    evidence$end_to_end_comparison,
    file.path(out_dir, "before-after-end-to-end.csv")
  )
  saveRDS(evidence, file.path(out_dir, "profile-evidence.rds"), version = 2L)
  writeLines(
    capture.output(utils::sessionInfo()),
    file.path(out_dir, "session-info.txt"),
    useBytes = TRUE
  )
}

sm_vp_run <- function(options) {
  root <- sm_vp_root()
  sm_vp_require_runtime(root, options$out_dir, options$before_dir)
  started <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  pkgload::load_all(root, quiet = TRUE, export_all = FALSE)
  specs <- sm_vp_case_specs()
  specs$seed <- options$seed + seq_len(nrow(specs)) - 1L
  before <- if (identical(options$profile, "optimized")) {
    sm_vp_read_before_profile(options$before_dir, options, specs)
  } else {
    NULL
  }
  targets <- sm_vp_trace_targets()
  results <- vector("list", nrow(specs))
  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, , drop = FALSE]
    case <- sm_vp_make_case(spec, options$seed + i - 1L)
    results[[i]] <- sm_vp_collect_case(case, options, targets, options$out_dir)
  }
  specs$min_marginal_count <- vapply(results, `[[`, integer(1), "min_count")
  specs$max_marginal_count <- vapply(results, `[[`, integer(1), "max_count")
  specs$all_interior <- vapply(results, `[[`, logical(1), "all_interior")
  timings <- sm_vp_bind_component(results, "timings")
  trace <- sm_vp_bind_component(results, "trace")
  signatures <- sm_vp_bind_component(results, "signatures")
  formulas <- sm_vp_bind_component(results, "formulas")
  timing_summary <- sm_vp_timing_summary(timings)
  all_formula <- all(trace$formula_match) && all(formulas$formula_match)
  all_identical <- all(signatures$identical)
  status <- if (all_formula && all_identical && all(specs$all_interior)) "PASS" else "FAIL"
  metadata <- data.frame(
    started_utc = started,
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    pid = Sys.getpid(),
    expectation_profile = options$profile,
    before_dir = if (is.null(options$before_dir)) {
      NA_character_
    } else {
      normalizePath(options$before_dir, mustWork = TRUE)
    },
    seed = options$seed,
    warmup = options$warmup,
    reps = options$reps,
    rprof_interval_seconds = options$rprof_interval,
    timing_tracing_separated = TRUE,
    stringsAsFactors = FALSE
  )
  summary <- data.frame(
    status = status,
    expectation_profile = options$profile,
    before_compatible = if (is.null(before)) NA else all(before$checks$pass),
    cases = nrow(specs),
    stages = length(unique(timings$stage)),
    formula_checks = nrow(trace) + nrow(formulas),
    formula_failures = sum(!trace$formula_match) + sum(!formulas$formula_match),
    signature_checks = nrow(signatures),
    signature_failures = sum(!signatures$identical),
    all_interior = all(specs$all_interior),
    stringsAsFactors = FALSE
  )
  evidence <- list(
    case_specs = specs,
    metadata = metadata,
    timings = timings,
    timing_summary = timing_summary,
    gc = sm_vp_bind_component(results, "gc"),
    trace = trace,
    formulas = formulas,
    signatures = signatures,
    sizes = sm_vp_bind_component(results, "sizes"),
    rprof = sm_vp_bind_component(results, "rprof"),
    source_hashes = sm_vp_source_hashes(root),
    runtime = sm_vp_runtime_environment(),
    summary = summary,
    before_checks = if (is.null(before)) data.frame() else before$checks,
    before_artifacts = if (is.null(before)) data.frame() else before$artifacts,
    before_source_hashes = if (is.null(before)) data.frame() else before$source_hashes,
    timing_comparison = if (is.null(before)) {
      data.frame()
    } else {
      sm_vp_compare_timing(before$evidence$timing_summary, timing_summary)
    },
    count_comparison = if (is.null(before)) {
      data.frame()
    } else {
      sm_vp_compare_counts(before$evidence$trace, trace)
    },
    end_to_end_comparison = if (is.null(before)) {
      data.frame()
    } else {
      sm_vp_compare_end_to_end(before$evidence$trace, trace)
    }
  )
  sm_vp_write_artifacts(evidence, options$out_dir)
  cat(
    "vcov validation profile ", status,
    "; expectation_profile=", options$profile,
    "; cases=", nrow(specs),
    "; formula_failures=", summary$formula_failures,
    "; signature_failures=", summary$signature_failures,
    "; artifacts=", normalizePath(options$out_dir),
    "\n",
    sep = ""
  )
  if (status == "PASS") 0L else 1L
}

sm_vp_main <- function() {
  options <- sm_vp_parse_args(commandArgs(trailingOnly = TRUE))
  sm_vp_run(options)
}

if (sys.nframe() == 0L) {
  status <- tryCatch(
    sm_vp_main(),
    error = function(error) {
      message("vcov validation profile error: ", conditionMessage(error))
      2L
    }
  )
  quit(save = "no", status = status)
}
