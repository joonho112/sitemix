#!/usr/bin/env Rscript

# Enforce zero correctness lint and a fingerprinted no-new style policy while
# preserving object-usage findings as a separately reviewed advisory inventory.

sm_lint_gate_root <- function() {
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

sm_lint_gate_paths <- function(root) {
  gate_dir <- file.path(root, "inst", "gates")
  list(
    policy = file.path(gate_dir, "lint-policy.csv"),
    style = file.path(gate_dir, "lint-style-baseline.csv"),
    usage = file.path(gate_dir, "lint-object-usage-baseline.csv")
  )
}

sm_lint_gate_require_source_tree <- function(root) {
  required <- c(
    file.path(root, "R"),
    file.path(root, "tests", "testthat"),
    file.path(root, "inst", "gates")
  )
  if (!all(dir.exists(required)) || !file.exists(file.path(root, ".lintr"))) {
    stop("A complete package source tree is required for the lint gate.", call. = FALSE)
  }
  invisible(TRUE)
}

sm_lint_gate_exclusions <- function() {
  list(
    ".Rproj.user", ".quarto", "_book", "ci-artifacts", "dev", "doc",
    "docs", "log", "log-v2", "man", "Meta", "pkgdown",
    "R/RcppExports.R", "sitemix.Rcheck"
  )
}

sm_lint_gate_linters <- function() {
  lintr::linters_with_defaults(
    line_length_linter = lintr::line_length_linter(120L),
    object_length_linter = lintr::object_length_linter(60L),
    object_name_linter = lintr::object_name_linter(
      styles = c("snake_case", "symbols"),
      regexes = c(
        "^[A-Z][A-Za-z0-9]*(?:_[A-Za-z0-9]+)*$",
        "^[a-z][a-z0-9_]*_[A-Z][A-Za-z0-9]*$",
        "^validate[.](?:sitemix_estimates|sm_vcov)$",
        "^[.]Random[.]seed$"
      )
    ),
    object_usage_linter = NULL,
    all_equal_linter = lintr::all_equal_linter(),
    any_duplicated_linter = lintr::any_duplicated_linter(),
    any_is_na_linter = lintr::any_is_na_linter(),
    class_equals_linter = lintr::class_equals_linter(),
    condition_call_linter = lintr::condition_call_linter(),
    condition_message_linter = lintr::condition_message_linter(),
    duplicate_argument_linter = lintr::duplicate_argument_linter(),
    empty_assignment_linter = lintr::empty_assignment_linter(),
    length_levels_linter = lintr::length_levels_linter(),
    list_comparison_linter = lintr::list_comparison_linter(),
    missing_argument_linter = lintr::missing_argument_linter(),
    package_hooks_linter = lintr::package_hooks_linter(),
    routine_registration_linter = lintr::routine_registration_linter(),
    terminal_close_linter = lintr::terminal_close_linter(),
    unreachable_code_linter = lintr::unreachable_code_linter()
  )
}

sm_lint_gate_assert_active <- function(linters, policy) {
  active <- names(linters)
  expected <- policy$linter[policy$track != "object_usage"]
  if ("object_usage_linter" %in% active) {
    stop("object_usage_linter must be absent from the blocking pass.", call. = FALSE)
  }
  if (anyDuplicated(active) || !identical(sort(active), sort(expected))) {
    stop("Active linters disagree with the exact 40-linter policy.", call. = FALSE)
  }
  invisible(TRUE)
}

sm_lint_gate_assert_developer_config <- function(root) {
  path <- file.path(root, ".lintr")
  if (!file.exists(path)) {
    return(invisible(FALSE))
  }
  contents <- paste(readLines(path, warn = FALSE), collapse = "\n")
  observed <- sm_lint_gate_md5(contents)
  expected <- "f6a76974eed88750b923e433bb486250"
  if (!identical(observed, expected)) {
    stop(".lintr has drifted from the script-owned policy.", call. = FALSE)
  }
  invisible(TRUE)
}

sm_lint_gate_expected_policy <- function() {
  list(
    correctness = c(
      "all_equal_linter", "any_duplicated_linter", "any_is_na_linter",
      "class_equals_linter", "condition_call_linter",
      "condition_message_linter", "duplicate_argument_linter",
      "empty_assignment_linter", "equals_na_linter",
      "length_levels_linter", "list_comparison_linter",
      "missing_argument_linter", "package_hooks_linter",
      "routine_registration_linter", "T_and_F_symbol_linter",
      "terminal_close_linter", "unreachable_code_linter",
      "vector_logic_linter"
    ),
    style = c(
      "assignment_linter", "brace_linter", "commas_linter",
      "commented_code_linter", "function_left_parentheses_linter",
      "indentation_linter", "infix_spaces_linter", "line_length_linter",
      "object_length_linter", "object_name_linter", "paren_body_linter",
      "pipe_consistency_linter", "pipe_continuation_linter",
      "quotes_linter", "return_linter", "semicolon_linter", "seq_linter",
      "spaces_inside_linter", "spaces_left_parentheses_linter",
      "trailing_blank_lines_linter", "trailing_whitespace_linter",
      "whitespace_linter"
    ),
    object_usage = "object_usage_linter"
  )
}

sm_lint_gate_read_policy <- function(path) {
  if (!file.exists(path)) {
    stop("lint-policy.csv is unavailable.", call. = FALSE)
  }
  policy <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  columns <- c("linter", "track", "blocking", "baseline_mode", "rationale")
  if (!identical(names(policy), columns) || nrow(policy) != 41L) {
    stop("Lint policy must have the frozen 41-row schema.", call. = FALSE)
  }
  if (anyNA(policy) || any(!vapply(policy, function(x) {
    all(nzchar(as.character(x)))
  }, logical(1)))) {
    stop("Lint policy contains a missing value.", call. = FALSE)
  }
  if (anyDuplicated(policy$linter)) {
    stop("Lint policy contains a duplicate linter.", call. = FALSE)
  }
  sm_lint_gate_validate_policy(policy)
}

sm_lint_gate_validate_policy <- function(policy) {
  expected <- sm_lint_gate_expected_policy()
  for (track in names(expected)) {
    observed <- policy$linter[policy$track == track]
    if (!setequal(observed, expected[[track]])) {
      stop("Lint policy disagrees with the frozen ", track, " set.", call. = FALSE)
    }
  }
  blocking <- as.character(policy$blocking)
  if (!all(blocking %in% c("TRUE", "FALSE"))) {
    stop("Lint policy blocking values must be logical.", call. = FALSE)
  }
  policy$blocking <- blocking == "TRUE"
  expected_mode <- c(
    correctness = "zero",
    style = "no_new",
    object_usage = "advisory_no_new"
  )
  if (any(policy$baseline_mode != expected_mode[policy$track])) {
    stop("Lint policy baseline modes are inconsistent.", call. = FALSE)
  }
  if (any(policy$blocking != (policy$track != "object_usage"))) {
    stop("Only object-usage findings may be nonblocking.", call. = FALSE)
  }
  policy
}

sm_lint_gate_relative <- function(path, root) {
  path <- gsub("\\\\", "/", path)
  prefix <- paste0(gsub("\\\\", "/", normalizePath(root)), "/")
  inside <- startsWith(path, prefix)
  path[inside] <- substring(path[inside], nchar(prefix) + 1L)
  path
}

sm_lint_gate_normalize_message <- function(x) {
  x[is.na(x)] <- "<NA>"
  trimws(gsub("[\r\n]+", " ", enc2utf8(x)))
}

sm_lint_gate_normalize_source <- function(x) {
  x[is.na(x)] <- "<NA>"
  gsub("\r", "", enc2utf8(x), fixed = TRUE)
}

sm_lint_gate_md5 <- function(keys) {
  if (!length(keys)) {
    return(character())
  }
  paths <- tempfile(rep("sitemix-lint-key-", length(keys)))
  on.exit(unlink(paths, force = TRUE), add = TRUE)
  for (i in seq_along(keys)) {
    writeBin(charToRaw(enc2utf8(keys[[i]])), paths[[i]])
  }
  unname(tools::md5sum(paths))
}

sm_lint_gate_occurrence <- function(keys) {
  if (!length(keys)) {
    return(integer())
  }
  groups <- match(keys, unique(keys))
  ave(seq_along(keys), groups, FUN = seq_along)
}

sm_lint_gate_normalize <- function(lints, root) {
  out <- as.data.frame(lints, stringsAsFactors = FALSE)
  names(out)[names(out) == "line"] <- "source"
  if (!nrow(out)) {
    out$source <- character()
  }
  out$filename <- sm_lint_gate_relative(out$filename, root)
  out$message <- sm_lint_gate_normalize_message(out$message)
  out$source <- sm_lint_gate_normalize_source(out$source)
  out <- out[order(
    out$filename, out$linter, out$message, out$source,
    out$line_number, out$column_number
  ), , drop = FALSE]
  base_key <- paste(
    out$filename, out$linter, out$message, out$source,
    sep = "\u001f"
  )
  out$occurrence <- sm_lint_gate_occurrence(base_key)
  out$fingerprint <- sm_lint_gate_md5(paste(
    base_key, out$occurrence,
    sep = "\u001f"
  ))
  out$signature <- sm_lint_gate_md5(base_key)
  rownames(out) <- NULL
  out
}

sm_lint_gate_usage_only <- function(findings) {
  collection_linters <- c(
    "object_usage_linter",
    "duplicate_argument_linter"
  )
  if (any(!findings$linter %in% collection_linters)) {
    stop("The advisory pass produced an unexpected linter.", call. = FALSE)
  }
  findings <- findings[
    findings$linter == "object_usage_linter",
    ,
    drop = FALSE
  ]
  if (any(findings$linter != "object_usage_linter")) {
    stop("Only object_usage_linter rows may enter the advisory inventory.", call. = FALSE)
  }
  findings
}

sm_lint_gate_require_lintr <- function() {
  if (!requireNamespace("lintr", quietly = TRUE)) {
    stop("lintr is required for the lint gate.", call. = FALSE)
  }
  version <- utils::packageVersion("lintr")
  if (version < "3.3.0") {
    stop("lintr >= 3.3.0 is required for the lint gate.", call. = FALSE)
  }
  invisible(version)
}

sm_lint_gate_collect <- function(root, policy) {
  active_linters <- sm_lint_gate_linters()
  sm_lint_gate_assert_active(active_linters, policy)
  sm_lint_gate_assert_developer_config(root)
  configured <- lintr::lint_package(
    path = root,
    linters = active_linters,
    relative_path = TRUE,
    exclusions = sm_lint_gate_exclusions(),
    parse_settings = FALSE,
    show_progress = FALSE,
    cache = FALSE
  )
  usage <- lintr::lint_package(
    path = root,
    linters = list(
      object_usage_linter = lintr::object_usage_linter(),
      duplicate_argument_linter = lintr::duplicate_argument_linter()
    ),
    relative_path = TRUE,
    exclusions = sm_lint_gate_exclusions(),
    parse_settings = FALSE,
    show_progress = FALSE,
    cache = FALSE
  )
  configured <- sm_lint_gate_normalize(configured, root)
  usage <- sm_lint_gate_normalize(usage, root)
  usage <- sm_lint_gate_usage_only(usage)
  configured <- sm_lint_gate_classify(configured, policy)
  usage$track <- rep("object_usage", nrow(usage))
  usage$blocking <- rep(FALSE, nrow(usage))
  list(configured = configured, usage = usage)
}

sm_lint_gate_classify <- function(findings, policy) {
  index <- match(findings$linter, policy$linter)
  if (anyNA(index)) {
    unknown <- unique(findings$linter[is.na(index)])
    stop("Unclassified linter findings: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  findings$track <- policy$track[index]
  findings$blocking <- policy$blocking[index]
  if (any(findings$track == "object_usage")) {
    stop("object_usage_linter leaked into the blocking pass.", call. = FALSE)
  }
  findings
}

sm_lint_gate_baseline_columns <- function() {
  c(
    "fingerprint", "signature", "linter", "filename", "message", "source",
    "occurrence", "baseline_line", "baseline_column", "review_step",
    "disposition"
  )
}

sm_lint_gate_baseline <- function(findings, review_step, disposition) {
  data.frame(
    fingerprint = findings$fingerprint,
    signature = findings$signature,
    linter = findings$linter,
    filename = findings$filename,
    message = findings$message,
    source = findings$source,
    occurrence = findings$occurrence,
    baseline_line = findings$line_number,
    baseline_column = findings$column_number,
    review_step = rep(review_step, nrow(findings)),
    disposition = rep(disposition, nrow(findings)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

sm_lint_gate_read_baseline <- function(path, track, policy) {
  if (!file.exists(path)) {
    stop(basename(path), " is unavailable.", call. = FALSE)
  }
  baseline <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character(),
    colClasses = "character"
  )
  if (!identical(names(baseline), sm_lint_gate_baseline_columns())) {
    stop(basename(path), " has an invalid schema.", call. = FALSE)
  }
  if (anyNA(baseline) || any(!vapply(baseline, function(x) {
    all(nzchar(x))
  }, logical(1)))) {
    stop(basename(path), " contains a missing value.", call. = FALSE)
  }
  if (anyDuplicated(baseline$fingerprint) ||
        any(!grepl("^[0-9a-f]{32}$", baseline$fingerprint)) ||
        any(!grepl("^[0-9a-f]{32}$", baseline$signature))) {
    stop(basename(path), " contains an invalid fingerprint.", call. = FALSE)
  }
  numeric <- suppressWarnings(as.numeric(c(
    baseline$occurrence, baseline$baseline_line, baseline$baseline_column
  )))
  integer <- suppressWarnings(as.integer(numeric))
  if (anyNA(numeric) || any(!is.finite(numeric)) ||
        any(numeric != integer) || any(integer < 1L)) {
    stop(basename(path), " contains invalid location metadata.", call. = FALSE)
  }
  base_keys <- paste(
    baseline$filename, baseline$linter, baseline$message, baseline$source,
    sep = "\u001f"
  )
  fingerprint_keys <- paste(base_keys, baseline$occurrence, sep = "\u001f")
  if (!identical(sm_lint_gate_md5(base_keys), baseline$signature) ||
        !identical(sm_lint_gate_md5(fingerprint_keys), baseline$fingerprint)) {
    stop(basename(path), " contains a fingerprint mismatch.", call. = FALSE)
  }
  allowed <- policy$linter[policy$track == track]
  if (any(!baseline$linter %in% allowed)) {
    stop(basename(path), " contains a linter from the wrong track.", call. = FALSE)
  }
  disposition <- if (track == "style") {
    "approved_style_debt"
  } else {
    "reviewed_static_false_positive"
  }
  if (any(baseline$disposition != disposition)) {
    stop(basename(path), " contains an invalid disposition.", call. = FALSE)
  }
  if (any(!grepl("^[0-9]{3}$", baseline$review_step))) {
    stop(basename(path), " contains an invalid review step.", call. = FALSE)
  }
  baseline
}

sm_lint_gate_compare <- function(findings, baseline, zero_track = FALSE) {
  if (zero_track) {
    findings$baseline_state <- rep("zero_required", nrow(findings))
    return(list(current = findings, new = findings, resolved = baseline[0L, ]))
  }
  is_new <- !findings$fingerprint %in% baseline$fingerprint
  findings$baseline_state <- ifelse(is_new, "new", "reviewed")
  resolved <- baseline[!baseline$fingerprint %in% findings$fingerprint, , drop = FALSE]
  list(current = findings, new = findings[is_new, , drop = FALSE], resolved = resolved)
}

sm_lint_gate_evaluate <- function(configured, usage, style_baseline, usage_baseline) {
  correctness <- configured[configured$track == "correctness", , drop = FALSE]
  style <- configured[configured$track == "style", , drop = FALSE]
  correctness_result <- sm_lint_gate_compare(
    correctness,
    style_baseline[0L, ],
    zero_track = TRUE
  )
  style_result <- sm_lint_gate_compare(style, style_baseline)
  usage_result <- sm_lint_gate_compare(usage, usage_baseline)
  configured_result <- rbind(
    correctness_result$current,
    style_result$current
  )
  configured_new <- rbind(correctness_result$new, style_result$new)
  list(
    configured = configured_result,
    new = configured_new,
    style_resolved = style_result$resolved,
    usage = usage_result$current,
    usage_new = usage_result$new,
    usage_resolved = usage_result$resolved
  )
}

sm_lint_gate_by_linter <- function(results, policy) {
  policy <- policy[policy$track != "object_usage", , drop = FALSE]
  count <- tabulate(match(results$configured$linter, policy$linter), nrow(policy))
  new_count <- tabulate(match(results$new$linter, policy$linter), nrow(policy))
  data.frame(
    linter = policy$linter,
    track = policy$track,
    blocking = policy$blocking,
    count = count,
    new = new_count,
    stringsAsFactors = FALSE
  )
}

sm_lint_gate_by_file <- function(findings) {
  files <- sort(unique(findings$filename))
  rows <- lapply(files, function(path) {
    selected <- findings[findings$filename == path, , drop = FALSE]
    data.frame(
      filename = path,
      correctness = sum(selected$track == "correctness"),
      style = sum(selected$track == "style"),
      total = nrow(selected),
      stringsAsFactors = FALSE
    )
  })
  if (!length(rows)) {
    return(data.frame(
      filename = character(), correctness = integer(),
      style = integer(), total = integer()
    ))
  }
  do.call(rbind, rows)
}

sm_lint_gate_source_files <- function(root) {
  roots <- file.path(root, c(
    "R", "tests", "inst", "vignettes", "data-raw", "demo", "exec"
  ))
  roots <- roots[dir.exists(roots)]
  files <- unlist(lapply(roots, function(path) {
    list.files(
      path,
      pattern = "[.](r|rmd|qmd|rnw|rhtml|rrst|rtex|rtxt)$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  }), use.names = FALSE)
  relative <- sm_lint_gate_relative(files, root)
  length(setdiff(relative, "R/RcppExports.R"))
}

sm_lint_gate_decision <- function(results, mode) {
  correctness <- sum(results$configured$track == "correctness")
  style <- sum(results$configured$track == "style")
  style_new <- sum(results$new$track == "style")
  passed <- correctness == 0L && style_new == 0L
  status <- if (passed) {
    "PASS"
  } else if (correctness > 0L) {
    "PENDING_CORRECTNESS"
  } else {
    "PENDING_STYLE"
  }
  list(
    status = status,
    blocking_status = if (passed) "PASS" else "FAIL",
    exit_code = if (mode == "gate" && !passed) 1L else 0L,
    correctness = correctness,
    style = style,
    style_new = style_new
  )
}

sm_lint_gate_unique_locations <- function(findings) {
  keys <- paste(
    findings$filename,
    findings$line_number,
    findings$column_number,
    findings$linter,
    findings$message,
    sep = "\u001f"
  )
  length(unique(keys))
}

sm_lint_gate_summary <- function(results, decision, root, mode) {
  data.frame(
    mode = mode,
    status = decision$status,
    blocking_status = decision$blocking_status,
    correctness_count = decision$correctness,
    style_count = decision$style,
    style_new = decision$style_new,
    style_resolved = nrow(results$style_resolved),
    object_usage_count = nrow(results$usage),
    object_usage_unique = sm_lint_gate_unique_locations(results$usage),
    object_usage_new = nrow(results$usage_new),
    object_usage_resolved = nrow(results$usage_resolved),
    object_usage_status = if (nrow(results$usage_new)) "ADVISORY_DRIFT" else "CLEAN",
    lintr_version = as.character(utils::packageVersion("lintr")),
    source_files = sm_lint_gate_source_files(root),
    stringsAsFactors = FALSE
  )
}

sm_lint_gate_write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, na = "")
}

sm_lint_gate_write_outputs <- function(results, summary, policy, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  sm_lint_gate_write_csv(results$configured, file.path(out_dir, "lint-results.csv"))
  sm_lint_gate_write_csv(
    sm_lint_gate_by_linter(results, policy),
    file.path(out_dir, "lint-by-linter.csv")
  )
  sm_lint_gate_write_csv(
    sm_lint_gate_by_file(results$configured),
    file.path(out_dir, "lint-by-file.csv")
  )
  sm_lint_gate_write_csv(summary, file.path(out_dir, "lint-summary.csv"))
  sm_lint_gate_write_csv(results$new, file.path(out_dir, "lint-new.csv"))
  sm_lint_gate_write_csv(
    results$style_resolved,
    file.path(out_dir, "lint-resolved.csv")
  )
  sm_lint_gate_write_csv(
    results$usage,
    file.path(out_dir, "object-usage-advisory.csv")
  )
  usage_summary <- summary[c(
    "object_usage_count", "object_usage_unique", "object_usage_new",
    "object_usage_resolved", "object_usage_status"
  )]
  sm_lint_gate_write_csv(
    usage_summary,
    file.path(out_dir, "object-usage-summary.csv")
  )
  session <- c(
    paste0("lintr: ", utils::packageVersion("lintr")),
    capture.output(utils::sessionInfo())
  )
  writeLines(session, file.path(out_dir, "lint-session-info.txt"), useBytes = TRUE)
}

sm_lint_gate_parse_args <- function(args) {
  out <- list(
    mode = "gate", out_dir = NULL, self_test = FALSE,
    write_baselines = FALSE, review_step = NULL,
    expect_correctness = NULL, expect_style = NULL, expect_usage = NULL
  )
  for (arg in args) {
    if (arg == "--self-test") {
      out$self_test <- TRUE
    } else if (arg == "--write-baselines") {
      out$write_baselines <- TRUE
    } else if (startsWith(arg, "--mode=")) {
      out$mode <- sub("^--mode=", "", arg)
    } else if (startsWith(arg, "--out-dir=")) {
      out$out_dir <- sub("^--out-dir=", "", arg)
    } else if (startsWith(arg, "--review-step=")) {
      out$review_step <- sub("^--review-step=", "", arg)
    } else if (startsWith(arg, "--expect-style=")) {
      out$expect_style <- sub("^--expect-style=", "", arg)
    } else if (startsWith(arg, "--expect-correctness=")) {
      out$expect_correctness <- sub("^--expect-correctness=", "", arg)
    } else if (startsWith(arg, "--expect-object-usage=")) {
      out$expect_usage <- sub("^--expect-object-usage=", "", arg)
    } else {
      stop("Unknown argument: ", arg, call. = FALSE)
    }
  }
  if (!out$mode %in% c("inventory", "gate")) {
    stop("--mode must be inventory or gate.", call. = FALSE)
  }
  out
}

sm_lint_gate_expected_count <- function(value, label) {
  number <- suppressWarnings(as.integer(value))
  if (length(number) != 1L || is.na(number) || number < 0L ||
        !identical(as.character(number), value)) {
    stop(label, " must be a non-negative integer.", call. = FALSE)
  }
  number
}

sm_lint_gate_install_baselines <- function(staged, targets) {
  moved <- logical(length(targets))
  for (i in seq_along(targets)) {
    moved[[i]] <- file.rename(staged[[i]], targets[[i]])
    if (!moved[[i]]) {
      unlink(targets[moved], force = TRUE)
      stop("Could not atomically install both lint baselines.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

sm_lint_gate_write_baselines <- function(options, findings, paths, policy) {
  if (nzchar(Sys.getenv("CI")) || nzchar(Sys.getenv("GITHUB_ACTIONS"))) {
    stop("Baseline creation is forbidden in CI.", call. = FALSE)
  }
  required <- c(
    options$review_step, options$expect_correctness,
    options$expect_style, options$expect_usage
  )
  if (any(vapply(required, is.null, logical(1)))) {
    stop("Baseline creation requires review step and all three expected counts.", call. = FALSE)
  }
  if (!grepl("^[0-9]{3}$", options$review_step)) {
    stop("--review-step must be a three-digit log number.", call. = FALSE)
  }
  expected_correctness <- sm_lint_gate_expected_count(
    options$expect_correctness,
    "expected correctness"
  )
  expected_style <- sm_lint_gate_expected_count(options$expect_style, "expected style")
  expected_usage <- sm_lint_gate_expected_count(options$expect_usage, "expected usage")
  correctness <- findings$configured[
    findings$configured$track == "correctness",
    ,
    drop = FALSE
  ]
  style <- findings$configured[findings$configured$track == "style", , drop = FALSE]
  if (nrow(correctness) != expected_correctness ||
        nrow(style) != expected_style || nrow(findings$usage) != expected_usage) {
    stop("Observed lint counts disagree with the reviewed baseline counts.", call. = FALSE)
  }
  targets <- c(paths$style, paths$usage)
  if (any(file.exists(targets))) {
    stop("Refusing to overwrite an existing lint baseline.", call. = FALSE)
  }
  style_baseline <- sm_lint_gate_baseline(style, options$review_step, "approved_style_debt")
  usage_baseline <- sm_lint_gate_baseline(
    findings$usage,
    options$review_step,
    "reviewed_static_false_positive"
  )
  staged <- c(
    tempfile("lint-style-", tmpdir = dirname(paths$style), fileext = ".csv"),
    tempfile("lint-usage-", tmpdir = dirname(paths$usage), fileext = ".csv")
  )
  on.exit(unlink(staged, force = TRUE), add = TRUE)
  sm_lint_gate_write_csv(style_baseline, staged[[1L]])
  sm_lint_gate_write_csv(usage_baseline, staged[[2L]])
  sm_lint_gate_read_baseline(staged[[1L]], "style", policy)
  sm_lint_gate_read_baseline(staged[[2L]], "object_usage", policy)
  sm_lint_gate_install_baselines(staged, targets)
}

sm_lint_gate_test_finding <- function(line_number, source = "x <- 1") {
  structure(list(
    structure(list(
      filename = "R/example.R", line_number = line_number,
      column_number = 1L, type = "style", message = "example",
      line = source, ranges = list(c(1L, 2L)), linter = "example_linter"
    ), class = "lint")
  ), class = c("lints", "list"))
}

sm_lint_gate_self_test_contract <- function() {
  package_root <- sm_lint_gate_root()
  sm_lint_gate_require_source_tree(package_root)
  paths <- sm_lint_gate_paths(package_root)
  policy <- sm_lint_gate_read_policy(paths$policy)
  active <- sm_lint_gate_linters()
  sm_lint_gate_assert_active(active, policy)
  sm_lint_gate_assert_developer_config(package_root)
  style <- sm_lint_gate_read_baseline(paths$style, "style", policy)
  usage <- sm_lint_gate_read_baseline(paths$usage, "object_usage", policy)
  stopifnot(nrow(style) > 0L, nrow(usage) > 0L)
  installed_like <- tempfile("sitemix-installed-layout-")
  dir.create(installed_like)
  on.exit(unlink(installed_like, recursive = TRUE, force = TRUE), add = TRUE)
  rejected <- try(sm_lint_gate_require_source_tree(installed_like), silent = TRUE)
  stopifnot(inherits(rejected, "try-error"))
  invisible(TRUE)
}

sm_lint_gate_self_test_bootstrap_guard <- function() {
  directory <- tempfile("sitemix-lint-bootstrap-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE, force = TRUE), add = TRUE)
  paths <- list(
    style = file.path(directory, "style.csv"),
    usage = file.path(directory, "usage.csv")
  )
  options <- list(
    review_step = "049", expect_correctness = NULL,
    expect_style = "0", expect_usage = "0"
  )
  findings <- list(
    configured = data.frame(track = "correctness"),
    usage = data.frame()
  )
  rejected <- try(
    sm_lint_gate_write_baselines(options, findings, paths, data.frame()),
    silent = TRUE
  )
  stopifnot(
    inherits(rejected, "try-error"),
    !file.exists(paths$style),
    !file.exists(paths$usage)
  )
  invisible(TRUE)
}

sm_lint_gate_self_test <- function() {
  sm_lint_gate_require_lintr()
  sm_lint_gate_self_test_contract()
  sm_lint_gate_self_test_bootstrap_guard()
  root <- normalizePath(tempdir(), mustWork = TRUE)
  first <- sm_lint_gate_normalize(sm_lint_gate_test_finding(10L), root)
  shifted <- sm_lint_gate_normalize(sm_lint_gate_test_finding(20L), root)
  changed <- sm_lint_gate_normalize(
    sm_lint_gate_test_finding(10L, "x <- 2"),
    root
  )
  stopifnot(
    identical(first$fingerprint, shifted$fingerprint),
    !identical(first$fingerprint, changed$fingerprint)
  )
  duplicated <- c(
    sm_lint_gate_test_finding(10L),
    sm_lint_gate_test_finding(20L)
  )
  class(duplicated) <- c("lints", "list")
  duplicated <- sm_lint_gate_normalize(duplicated, root)
  stopifnot(
    identical(duplicated$occurrence, c(1L, 2L)),
    length(unique(duplicated$fingerprint)) == 2L
  )
  usage_mock <- rbind(
    transform(first, linter = "object_usage_linter"),
    transform(first, linter = "duplicate_argument_linter")
  )
  usage_only <- sm_lint_gate_usage_only(usage_mock)
  unexpected <- usage_mock
  unexpected$linter[[2L]] <- "brace_linter"
  rejected <- try(sm_lint_gate_usage_only(unexpected), silent = TRUE)
  stopifnot(
    nrow(usage_only) == 1L,
    identical(usage_only$linter, "object_usage_linter"),
    inherits(rejected, "try-error")
  )
  mock <- list(
    configured = first[0L, ], new = first[0L, ],
    style_resolved = data.frame(), usage = first,
    usage_new = first, usage_resolved = data.frame()
  )
  mock$configured$track <- character()
  mock$new$track <- character()
  decision <- sm_lint_gate_decision(mock, "gate")
  stopifnot(decision$exit_code == 0L, nrow(mock$usage_new) == 1L)
  cat("lint gate self-test: PASS\n")
  invisible(TRUE)
}

sm_lint_gate_run <- function(options) {
  sm_lint_gate_require_lintr()
  root <- sm_lint_gate_root()
  sm_lint_gate_require_source_tree(root)
  paths <- sm_lint_gate_paths(root)
  policy <- sm_lint_gate_read_policy(paths$policy)
  findings <- sm_lint_gate_collect(root, policy)
  if (options$write_baselines) {
    sm_lint_gate_write_baselines(options, findings, paths, policy)
  }
  style_baseline <- sm_lint_gate_read_baseline(paths$style, "style", policy)
  usage_baseline <- sm_lint_gate_read_baseline(paths$usage, "object_usage", policy)
  results <- sm_lint_gate_evaluate(
    findings$configured,
    findings$usage,
    style_baseline,
    usage_baseline
  )
  decision <- sm_lint_gate_decision(results, options$mode)
  summary <- sm_lint_gate_summary(results, decision, root, options$mode)
  out_dir <- options$out_dir
  if (is.null(out_dir)) {
    out_dir <- file.path(root, "ci-artifacts", "lint")
  }
  sm_lint_gate_write_outputs(results, summary, policy, out_dir)
  cat(
    "lint gate: ", decision$status,
    " | correctness=", decision$correctness,
    " | style=", decision$style,
    " | style_new=", decision$style_new,
    " | object_usage=", nrow(results$usage),
    " | object_usage_new=", nrow(results$usage_new),
    "\n",
    sep = ""
  )
  decision$exit_code
}

sm_lint_gate_main <- function() {
  options <- sm_lint_gate_parse_args(commandArgs(trailingOnly = TRUE))
  if (options$self_test) {
    sm_lint_gate_self_test()
    return(0L)
  }
  sm_lint_gate_run(options)
}

if (sys.nframe() == 0L) {
  status <- tryCatch(
    sm_lint_gate_main(),
    error = function(error) {
      message("lint gate error: ", conditionMessage(error))
      2L
    }
  )
  quit(save = "no", status = status)
}
