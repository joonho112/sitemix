#!/usr/bin/env Rscript

# Enforce the package line-coverage contract with exact integer arithmetic.
# Percentages are presentation-only: the decision is covered * denominator >=
# total * numerator, so rounding can never turn a failing run into a pass.

sm_coverage_gate_root <- function() {
  command <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command, value = TRUE)
  candidates <- character()
  if (length(file_arg)) {
    script <- sub("^--file=", "", file_arg[[1L]])
    candidates <- c(
      file.path(dirname(script), "..", ".."),
      file.path(dirname(script), "..")
    )
  }
  candidates <- c(candidates, getwd())
  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "DESCRIPTION"))) {
      return(normalizePath(candidate, mustWork = TRUE))
    }
  }
  stop("Could not locate a package root containing DESCRIPTION.", call. = FALSE)
}

sm_coverage_gate_budget_path <- function(root) {
  installed_gate <- if (!file.exists(file.path(root, "DESCRIPTION"))) {
    tryCatch(
      system.file("gates", "coverage-gate-budget.csv", package = "sitemix"),
      error = function(condition) ""
    )
  } else {
    ""
  }
  candidates <- c(
    file.path(root, "inst", "gates", "coverage-gate-budget.csv"),
    file.path(root, "gates", "coverage-gate-budget.csv"),
    installed_gate[nzchar(installed_gate)]
  )
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    stop("coverage-gate-budget.csv is unavailable.", call. = FALSE)
  }
  existing[[1L]]
}

sm_coverage_gate_read_budget <- function(root) {
  budget <- utils::read.csv(
    sm_coverage_gate_budget_path(root),
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  columns <- c(
    "gate_id", "ci_profile", "metric", "threshold_numerator",
    "threshold_denominator", "threshold_percent", "blocking",
    "gate_wired", "notes"
  )
  if (!identical(names(budget), columns) || nrow(budget) != 1L) {
    stop("Coverage budget must have the frozen one-row schema.", call. = FALSE)
  }
  if (anyNA(budget) || any(!vapply(budget, function(x) {
    all(nzchar(as.character(x)))
  }, logical(1)))) {
    stop("Coverage budget contains a missing value.", call. = FALSE)
  }
  numerator <- suppressWarnings(as.numeric(budget$threshold_numerator[[1L]]))
  denominator <- suppressWarnings(as.numeric(budget$threshold_denominator[[1L]]))
  percent <- suppressWarnings(as.numeric(budget$threshold_percent[[1L]]))
  integer_fields <- c(numerator, denominator)
  if (any(!is.finite(integer_fields)) || any(integer_fields != as.integer(integer_fields)) ||
      numerator < 1 || denominator < 1 || numerator > denominator) {
    stop("Coverage numerator and denominator must be positive bounded integers.", call. = FALSE)
  }
  if (!is.finite(percent) ||
      !isTRUE(all.equal(percent, 100 * numerator / denominator, tolerance = 1e-12))) {
    stop("Coverage percent disagrees with its exact ratio.", call. = FALSE)
  }
  if (!identical(budget$metric[[1L]], "covered_executable_lines") ||
      !identical(budget$blocking[[1L]], TRUE) ||
      !identical(budget$gate_wired[[1L]], TRUE)) {
    stop("Coverage budget is not wired as a blocking executable-line gate.", call. = FALSE)
  }
  budget
}

sm_coverage_gate_count <- function(value, name, allow_zero = TRUE) {
  number <- suppressWarnings(as.numeric(value))
  integer <- suppressWarnings(as.integer(number))
  lower <- if (allow_zero) 0L else 1L
  if (length(number) != 1L || is.na(number) || !is.finite(number) ||
      is.na(integer) || number != integer || number < lower) {
    stop("`", name, "` must be one ", if (allow_zero) "non-negative" else "positive",
         " integer.", call. = FALSE)
  }
  integer
}

sm_coverage_gate_evaluate <- function(covered, total, numerator = 9L, denominator = 10L) {
  covered <- sm_coverage_gate_count(covered, "covered")
  total <- sm_coverage_gate_count(total, "total", allow_zero = FALSE)
  numerator <- sm_coverage_gate_count(numerator, "numerator", allow_zero = FALSE)
  denominator <- sm_coverage_gate_count(denominator, "denominator", allow_zero = FALSE)
  if (covered > total) {
    stop("`covered` cannot exceed `total`.", call. = FALSE)
  }
  if (numerator > denominator) {
    stop("`numerator` cannot exceed `denominator`.", call. = FALSE)
  }
  required <- as.integer(ceiling(total * numerator / denominator))
  passed <- covered * denominator >= total * numerator
  list(
    passed = isTRUE(passed),
    status = if (isTRUE(passed)) "PASS" else "FAIL",
    covered = covered,
    total = total,
    required = required,
    shortfall = max(0L, required - covered),
    percent = 100 * covered / total,
    numerator = numerator,
    denominator = denominator
  )
}

sm_coverage_gate_relative <- function(path, root) {
  path <- gsub("\\\\", "/", path)
  root <- paste0(gsub("\\\\", "/", normalizePath(root, mustWork = TRUE)), "/")
  if (startsWith(path, root)) substring(path, nchar(root) + 1L) else path
}

sm_coverage_gate_measure <- function(root, coverage_rds = NULL) {
  if (!requireNamespace("covr", quietly = TRUE)) {
    stop("covr is required for the coverage gate.", call. = FALSE)
  }
  coverage <- if (is.null(coverage_rds)) {
    covr::package_coverage(
      path = root,
      type = "tests",
      relative_path = TRUE,
      quiet = TRUE,
      clean = TRUE,
      pre_clean = FALSE
    )
  } else {
    if (!file.exists(coverage_rds)) {
      stop("The supplied coverage RDS does not exist.", call. = FALSE)
    }
    readRDS(coverage_rds)
  }
  tally <- covr::tally_coverage(coverage, by = "line")
  required <- c("filename", "line", "value")
  if (!is.data.frame(tally) || !all(required %in% names(tally)) || !nrow(tally)) {
    stop("covr returned no executable-line tally.", call. = FALSE)
  }
  if (anyNA(tally$value) || any(tally$value < 0)) {
    stop("covr returned an invalid execution count.", call. = FALSE)
  }
  tally$filename <- vapply(
    tally$filename,
    sm_coverage_gate_relative,
    character(1),
    root = root
  )
  list(
    coverage = coverage,
    tally = tally,
    covered = sum(tally$value > 0),
    total = nrow(tally)
  )
}

sm_coverage_gate_by_file <- function(tally) {
  files <- sort(unique(tally$filename))
  rows <- lapply(files, function(file) {
    values <- tally$value[tally$filename == file]
    covered <- sum(values > 0)
    total <- length(values)
    data.frame(
      file = file,
      covered_executable_lines = as.integer(covered),
      total_executable_lines = as.integer(total),
      uncovered_executable_lines = as.integer(total - covered),
      coverage_percent = 100 * covered / total,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

sm_coverage_gate_write_session <- function(path) {
  output <- capture.output(utils::sessionInfo())
  writeLines(output, path, useBytes = TRUE)
}

sm_coverage_gate_audit <- function(
  root,
  out_dir = file.path(root, "ci-artifacts", "coverage"),
  coverage_rds = NULL
) {
  started <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  budget <- sm_coverage_gate_read_budget(root)
  measured <- sm_coverage_gate_measure(root, coverage_rds = coverage_rds)
  decision <- sm_coverage_gate_evaluate(
    covered = measured$covered,
    total = measured$total,
    numerator = budget$threshold_numerator[[1L]],
    denominator = budget$threshold_denominator[[1L]]
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(measured$coverage, file.path(out_dir, "coverage.rds"), version = 2)
  coverage_html <- file.path(out_dir, "coverage.html")
  covr::report(measured$coverage, file = coverage_html, browse = FALSE)
  if (!file.exists(coverage_html) || is.na(file.info(coverage_html)$size) ||
      file.info(coverage_html)$size <= 0) {
    stop("covr did not create a non-empty HTML coverage report.", call. = FALSE)
  }
  utils::write.csv(
    sm_coverage_gate_by_file(measured$tally),
    file.path(out_dir, "coverage-by-file.csv"),
    row.names = FALSE,
    na = ""
  )
  summary <- data.frame(
    gate_id = budget$gate_id[[1L]],
    ci_profile = budget$ci_profile[[1L]],
    metric = budget$metric[[1L]],
    started_utc = started,
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    status = decision$status,
    covered_executable_lines = decision$covered,
    total_executable_lines = decision$total,
    uncovered_executable_lines = decision$total - decision$covered,
    required_covered_lines = decision$required,
    shortfall_lines = decision$shortfall,
    threshold_numerator = decision$numerator,
    threshold_denominator = decision$denominator,
    threshold_percent = budget$threshold_percent[[1L]],
    coverage_percent = decision$percent,
    blocking = budget$blocking[[1L]],
    gate_wired = budget$gate_wired[[1L]],
    source = if (is.null(coverage_rds)) "fresh_covr_run" else "supplied_covr_rds",
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    summary,
    file.path(out_dir, "coverage-summary.csv"),
    row.names = FALSE,
    na = ""
  )
  sm_coverage_gate_write_session(file.path(out_dir, "coverage-session-info.txt"))
  list(status = decision$status, summary = summary, out_dir = out_dir)
}

sm_coverage_gate_arg <- function(args, name, default = NULL) {
  prefix <- paste0(name, "=")
  values <- args[startsWith(args, prefix)]
  if (!length(values)) default else sub(paste0("^", prefix), "", values[[1L]])
}

sm_coverage_gate_parse_args <- function(args) {
  options <- c("--out-dir", "--coverage-rds")
  known <- rep(FALSE, length(args))
  for (option in options) {
    matches <- startsWith(args, paste0(option, "="))
    if (sum(matches) > 1L) {
      stop("Duplicate argument: ", option, call. = FALSE)
    }
    if (any(matches) && !nzchar(sm_coverage_gate_arg(args, option, ""))) {
      stop("Argument requires a value: ", option, call. = FALSE)
    }
    known <- known | matches
  }
  if (any(!known)) {
    stop("Unknown arguments: ", paste(args[!known], collapse = ", "), call. = FALSE)
  }
  list(
    out_dir = sm_coverage_gate_arg(args, "--out-dir", NULL),
    coverage_rds = sm_coverage_gate_arg(args, "--coverage-rds", NULL)
  )
}

sm_coverage_gate_main <- function() {
  parsed <- sm_coverage_gate_parse_args(commandArgs(trailingOnly = TRUE))
  root <- sm_coverage_gate_root()
  out_dir <- if (is.null(parsed$out_dir)) {
    file.path(root, "ci-artifacts", "coverage")
  } else {
    parsed$out_dir
  }
  result <- sm_coverage_gate_audit(
    root = root,
    out_dir = out_dir,
    coverage_rds = parsed$coverage_rds
  )
  cat(
    "coverage gate ", result$status,
    "; covered=", result$summary$covered_executable_lines,
    "; total=", result$summary$total_executable_lines,
    "; required=", result$summary$required_covered_lines,
    "; artifacts=", normalizePath(out_dir, mustWork = TRUE), "\n",
    sep = ""
  )
  if (identical(result$status, "PASS")) 0L else 1L
}

if (sys.nframe() == 0L) {
  exit_status <- tryCatch(
    sm_coverage_gate_main(),
    error = function(error) {
      message("coverage gate ERROR: ", conditionMessage(error))
      2L
    }
  )
  quit(save = "no", status = exit_status, runLast = FALSE)
}
