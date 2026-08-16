#!/usr/bin/env Rscript

self_test_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script <- sub("^--file=", "", file_arg[[1L]])
  normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE)
}

self_test_error <- function(expr, pattern) {
  error <- tryCatch(
    {
      force(expr)
      NULL
    },
    error = identity
  )
  stopifnot(inherits(error, "error"), grepl(pattern, conditionMessage(error)))
  invisible(error)
}

root <- self_test_root()
runner <- file.path(root, "inst", "bench", "performance-smoke.R")
environment <- new.env(parent = globalenv())
sys.source(runner, envir = environment)

environment$bench_validate_args(c(
  "--profile=ci-smoke", "--reps=3", "--warmup=1", "--memory-reps=1",
  "--seed=20260723", "--out-dir=/tmp/example"
))
self_test_error(
  environment$bench_validate_args(c("--profile=ci-smoke", "--unknown")),
  "Unknown"
)
self_test_error(
  environment$bench_validate_args(c("--profile=ci-smoke", "--profile=closeout")),
  "Duplicate"
)
self_test_error(
  environment$bench_int_arg("--reps=1.9", "reps", 3L, 1L),
  "integer"
)

budget_path <- file.path(root, "inst", "gates", "performance-budget.csv")
runtime_path <- file.path(root, "inst", "gates", "performance-runtime.csv")
budget <- environment$bench_read_budget(budget_path)
runtime <- environment$bench_read_runtime_reference(runtime_path)
stopifnot(
  identical(budget$case, environment$bench_expected_case_specs()$case),
  nrow(runtime) == 1L,
  identical(unname(tools::md5sum(budget_path)), environment$.bench_budget_md5),
  identical(unname(tools::md5sum(runtime_path)), environment$.bench_runtime_md5)
)

tampered <- budget
tampered$K[[1L]] <- 99L
self_test_error(environment$bench_validate_budget(tampered), "canonical `K`")
tampered <- budget
tampered$timing_advisory_seconds[[1L]] <- tampered$timing_hard_seconds[[1L]]
self_test_error(environment$bench_validate_budget(tampered), "advisory < hard")
tampered <- budget
tampered$allocation_blocking[[1L]] <- TRUE
self_test_error(environment$bench_validate_budget(tampered), "not calibrated")

custom_budget <- tempfile("sitemix-custom-budget-", fileext = ".csv")
on.exit(unlink(custom_budget), add = TRUE)
invisible(file.copy(budget_path, custom_budget, overwrite = TRUE))
self_test_error(
  environment$bench_validate_authority(
    "closeout", TRUE, 5L, 2L, 3L, 20260723L,
    custom_budget, budget_path, runtime_path, TRUE
  ),
  "canonical budget"
)
self_test_error(
  environment$bench_validate_authority(
    "closeout", TRUE, 1L, 0L, 1L, 20260723L,
    budget_path, budget_path, runtime_path, TRUE
  ),
  "reps>=5"
)
self_test_error(
  environment$bench_validate_authority(
    "ci-smoke", TRUE, 5L, 2L, 3L, 20260723L,
    budget_path, budget_path, runtime_path, TRUE
  ),
  "only valid"
)
self_test_error(
  environment$bench_validate_authority(
    "closeout", TRUE, 5L, 2L, 3L, 20260723L,
    budget_path, budget_path, runtime_path, FALSE
  ),
  "exact calibrated runtime"
)
self_test_error(
  environment$bench_validate_authority(
    "closeout", FALSE, 5L, 2L, 3L, 1L,
    budget_path, budget_path, runtime_path, TRUE
  ),
  "canonical first synthetic seed"
)

stopifnot(
  environment$bench_is_unstable(0, 10, 0.15, 2),
  !environment$bench_is_unstable(0.1, 1.5, 0.15, 2),
  environment$bench_exit_status(1L, 0L, 0L) == 2L,
  environment$bench_exit_status(0L, 1L, 0L) == 2L,
  environment$bench_exit_status(0L, 0L, 1L) == 1L,
  environment$bench_exit_status(0L, 0L, 0L) == 0L
)

existing <- tempfile("sitemix-existing-output-")
dir.create(existing)
on.exit(unlink(existing, recursive = TRUE), add = TRUE)
self_test_error(environment$bench_output_target(existing), "must be fresh")

workflow <- readLines(
  file.path(root, ".github", "workflows", "R-CMD-check.yaml"),
  warn = FALSE
)
start <- grep("- name: Performance smoke benchmark", workflow, fixed = TRUE)
stopifnot(length(start) == 1L)
tail <- workflow[seq.int(start, length(workflow))]
next_step <- grep("^      - ", tail[-1L])
end <- if (length(next_step)) start + next_step[[1L]] - 1L else length(workflow)
block <- paste(workflow[seq.int(start, end)], collapse = "\n")
stopifnot(
  grepl("timeout-minutes: 20", block, fixed = TRUE),
  grepl("--profile=ci-smoke", block, fixed = TRUE),
  grepl("--warmup=1", block, fixed = TRUE),
  grepl("--reps=3", block, fixed = TRUE),
  grepl("--memory-reps=1", block, fixed = TRUE),
  !grepl("--enforce", block, fixed = TRUE)
)
for (variable in names(environment$bench_required_threads())) {
  stopifnot(grepl(paste0(variable, ': "1"'), block, fixed = TRUE))
}

cat("performance smoke contract self-test passed\n")
