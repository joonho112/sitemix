#!/usr/bin/env Rscript

# Audit the package test taxonomy and enforce an exact per-profile skip budget.
# Step 5.7 owns CI wiring; this script is intentionally dependency-light so the
# same gate can later run in full and deliberately optional-negative jobs.

sm_test_arch_set <- function(x) {
  if (length(x) == 0L || is.na(x) || !nzchar(x) || identical(x, "none")) {
    return(character())
  }
  values <- trimws(strsplit(x, "|", fixed = TRUE)[[1L]])
  unique(values[nzchar(values) & values != "none"])
}

sm_test_arch_join <- function(x) {
  x <- unique(x[nzchar(x)])
  if (length(x) == 0L) "none" else paste(x, collapse = "|")
}

sm_test_arch_root <- function() {
  command <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command, value = TRUE)
  if (length(file_arg) > 0L) {
    script <- sub("^--file=", "", file_arg[[1L]])
    candidates <- c(
      file.path(dirname(script), "..", ".."),
      file.path(dirname(script), "..")
    )
    for (candidate in candidates) {
      if (file.exists(file.path(candidate, "DESCRIPTION")) &&
          dir.exists(file.path(candidate, "tests", "testthat"))) {
        return(normalizePath(candidate, mustWork = TRUE))
      }
    }
  }
  root <- normalizePath(getwd(), mustWork = TRUE)
  if (!file.exists(file.path(root, "DESCRIPTION")) ||
      !dir.exists(file.path(root, "tests", "testthat"))) {
    stop("Could not locate a package root containing DESCRIPTION and tests/testthat.", call. = FALSE)
  }
  root
}

sm_test_arch_paths <- function(root) {
  base <- file.path(root, "tests", "testthat", "_data", "test-architecture")
  installed_gate_dir <- if (!file.exists(file.path(root, "DESCRIPTION"))) {
    tryCatch(
      system.file("gates", package = "sitemix"),
      error = function(condition) ""
    )
  } else {
    ""
  }
  gate_candidates <- c(
    file.path(root, "inst", "gates"),
    file.path(root, "gates"),
    installed_gate_dir[nzchar(installed_gate_dir)]
  )
  gate_dir <- gate_candidates[dir.exists(gate_candidates)]
  if (!length(gate_dir)) {
    gate_dir <- gate_candidates[[1L]]
  } else {
    gate_dir <- gate_dir[[1L]]
  }
  list(
    taxonomy = file.path(base, "test-file-taxonomy.csv"),
    budget = file.path(base, "job-skip-budget.csv"),
    timing_budget = file.path(gate_dir, "test-timing-budget.csv"),
    timing_baseline = file.path(gate_dir, "test-timing-baseline.csv"),
    tests = file.path(root, "tests", "testthat")
  )
}

sm_test_arch_read_csv <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
}

sm_test_arch_match <- function(pattern, text) {
  hit <- regexec(pattern, text, perl = TRUE)
  value <- regmatches(text, hit)[[1L]]
  if (length(value) < 2L) NA_character_ else value[[2L]]
}

sm_test_arch_scan_skips <- function(root, test_paths) {
  skip_rows <- list()
  broad_rows <- list()
  skip_i <- 0L
  broad_i <- 0L

  for (relative in test_paths) {
    lines <- readLines(file.path(root, relative), warn = FALSE)
    current_test <- NA_character_
    for (line_no in seq_along(lines)) {
      line <- lines[[line_no]]
      named_test <- sm_test_arch_match('test_that\\("([^"]+)"', line)
      if (!is.na(named_test)) {
        current_test <- named_test
      }
      has_skip_call <- grepl(
        "skip(_[[:alnum:]_]+)?[[:space:]]*\\(",
        line,
        perl = TRUE
      )
      if (!has_skip_call) {
        next
      }

      dependency <- sm_test_arch_match(
        'skip_if_not_installed\\("([^"]+)"',
        line
      )
      identity <- if (is.na(current_test)) {
        paste0(relative, "::<unnamed-test>")
      } else {
        paste0(relative, "::", current_test)
      }

      if (!is.na(dependency)) {
        skip_i <- skip_i + 1L
        skip_rows[[skip_i]] <- data.frame(
          path = relative,
          line = as.integer(line_no),
          test = current_test,
          dependency = dependency,
          identity = identity,
          stringsAsFactors = FALSE
        )
      } else {
        broad_i <- broad_i + 1L
        broad_rows[[broad_i]] <- data.frame(
          path = relative,
          line = as.integer(line_no),
          test = current_test,
          source = trimws(line),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  empty_skips <- data.frame(
    path = character(), line = integer(), test = character(),
    dependency = character(), identity = character(),
    stringsAsFactors = FALSE
  )
  empty_broad <- data.frame(
    path = character(), line = integer(), test = character(),
    source = character(), stringsAsFactors = FALSE
  )
  list(
    skip_sites = if (length(skip_rows)) do.call(rbind, skip_rows) else empty_skips,
    broad_skips = if (length(broad_rows)) do.call(rbind, broad_rows) else empty_broad
  )
}

sm_test_arch_validate_static <- function(root) {
  paths <- sm_test_arch_paths(root)
  taxonomy <- sm_test_arch_read_csv(paths$taxonomy)
  budget <- sm_test_arch_read_csv(paths$budget)
  timing_budget <- sm_test_arch_read_csv(paths$timing_budget)
  timing_baseline <- sm_test_arch_read_csv(paths$timing_baseline)
  issues <- character()
  add_issue <- function(message) {
    issues <<- c(issues, message)
  }

  taxonomy_columns <- c(
    "path", "primary_category", "secondary_tags", "optional_dependency",
    "fixture_kind", "global_state_risk"
  )
  budget_columns <- c(
    "profile", "ci_job", "os", "r_version", "dependency_mode",
    "test_scope", "required_present", "required_absent", "expected_skips",
    "allowed_skip_ids", "blocking", "gate_wired", "notes"
  )
  timing_budget_columns <- c(
    "profile", "suite_budget_seconds", "max_block_seconds",
    "slow_block_threshold_seconds", "max_slow_blocks", "blocking",
    "gate_wired", "notes"
  )
  timing_baseline_columns <- c(
    "path", "baseline_real_seconds", "baseline_test_blocks",
    "baseline_expectations", "source_run", "recorded_utc"
  )
  if (!identical(names(taxonomy), taxonomy_columns)) {
    add_issue("taxonomy columns do not match the frozen schema")
  }
  if (!identical(names(budget), budget_columns)) {
    add_issue("skip-budget columns do not match the frozen schema")
  }
  if (!identical(names(timing_budget), timing_budget_columns)) {
    add_issue("timing-budget columns do not match the frozen schema")
  }
  if (!identical(names(timing_baseline), timing_baseline_columns)) {
    add_issue("timing-baseline columns do not match the frozen schema")
  }

  required_taxonomy <- intersect(taxonomy_columns, names(taxonomy))
  for (column in required_taxonomy) {
    if (anyNA(taxonomy[[column]]) || any(!nzchar(taxonomy[[column]]))) {
      add_issue(paste0("taxonomy has missing values in `", column, "`"))
    }
  }
  required_budget <- intersect(budget_columns, names(budget))
  for (column in required_budget) {
    if (anyNA(budget[[column]]) || any(!nzchar(as.character(budget[[column]])))) {
      add_issue(paste0("skip budget has missing values in `", column, "`"))
    }
  }
  required_timing_budget <- intersect(timing_budget_columns, names(timing_budget))
  for (column in required_timing_budget) {
    if (anyNA(timing_budget[[column]]) ||
        any(!nzchar(as.character(timing_budget[[column]])))) {
      add_issue(paste0("timing budget has missing values in `", column, "`"))
    }
  }
  required_timing_baseline <- intersect(timing_baseline_columns, names(timing_baseline))
  for (column in required_timing_baseline) {
    if (anyNA(timing_baseline[[column]]) ||
        any(!nzchar(as.character(timing_baseline[[column]])))) {
      add_issue(paste0("timing baseline has missing values in `", column, "`"))
    }
  }

  actual <- sort(file.path(
    "tests", "testthat",
    list.files(paths$tests, pattern = "^test-.*[.]R$", full.names = FALSE)
  ))
  declared <- sort(taxonomy$path)
  if (anyDuplicated(taxonomy$path)) {
    add_issue("taxonomy assigns at least one test file more than once")
  }
  if (!identical(actual, declared)) {
    missing <- setdiff(actual, declared)
    stale <- setdiff(declared, actual)
    if (length(missing)) {
      add_issue(paste0("taxonomy is missing: ", paste(missing, collapse = "; ")))
    }
    if (length(stale)) {
      add_issue(paste0("taxonomy has stale paths: ", paste(stale, collapse = "; ")))
    }
  }

  categories <- c(
    "unit", "oracle", "invariant", "regression", "snapshot",
    "integration", "release"
  )
  invalid_categories <- setdiff(unique(taxonomy$primary_category), categories)
  if (length(invalid_categories)) {
    add_issue(paste0(
      "taxonomy has invalid primary categories: ",
      paste(invalid_categories, collapse = "; ")
    ))
  }
  invalid_optional <- setdiff(
    unique(taxonomy$optional_dependency),
    c("none", "dplyr", "mgcv")
  )
  if (length(invalid_optional)) {
    add_issue(paste0(
      "taxonomy has undeclared optional dependencies: ",
      paste(invalid_optional, collapse = "; ")
    ))
  }

  if (anyDuplicated(budget$profile)) {
    add_issue("skip-budget profile names are not unique")
  }
  numeric_skips <- suppressWarnings(as.integer(budget$expected_skips))
  if (anyNA(numeric_skips) || any(numeric_skips < 0L) ||
      any(numeric_skips != as.numeric(budget$expected_skips))) {
    add_issue("expected_skips must contain non-negative integers")
  }
  if (any(!budget$test_scope %in% c("full", "none"))) {
    add_issue("test_scope must be `full` or `none`")
  }
  if (any(!budget$blocking %in% c(TRUE, FALSE)) ||
      any(!budget$gate_wired %in% c(TRUE, FALSE))) {
    add_issue("blocking and gate_wired must be logical")
  }

  if (!identical(sort(timing_budget$profile), sort(budget$profile)) ||
      anyDuplicated(timing_budget$profile)) {
    add_issue("timing-budget profiles must exactly match skip-budget profiles")
  }
  timing_numeric <- c(
    "suite_budget_seconds", "max_block_seconds",
    "slow_block_threshold_seconds", "max_slow_blocks"
  )
  for (column in intersect(timing_numeric, names(timing_budget))) {
    value <- suppressWarnings(as.numeric(timing_budget[[column]]))
    if (anyNA(value) || any(!is.finite(value)) || any(value <= 0)) {
      add_issue(paste0("timing budget has invalid positive values in `", column, "`"))
    }
  }
  max_slow <- suppressWarnings(as.numeric(timing_budget$max_slow_blocks))
  if (any(!is.na(max_slow) & max_slow != as.integer(max_slow))) {
    add_issue("max_slow_blocks must contain positive integers")
  }
  if (any(!timing_budget$blocking %in% c(TRUE, FALSE)) ||
      any(!timing_budget$gate_wired %in% c(TRUE, FALSE))) {
    add_issue("timing blocking and gate_wired must be logical")
  }
  wired_order <- match(budget$profile, timing_budget$profile)
  if (!anyNA(wired_order) &&
      !identical(budget$gate_wired, timing_budget$gate_wired[wired_order])) {
    add_issue("timing and skip budgets disagree on gate_wired")
  }

  if (anyDuplicated(timing_baseline$path) ||
      !identical(sort(timing_baseline$path), sort(taxonomy$path))) {
    add_issue("timing baseline must contain each taxonomy path exactly once")
  }
  baseline_real <- suppressWarnings(as.numeric(timing_baseline$baseline_real_seconds))
  baseline_blocks <- suppressWarnings(as.numeric(timing_baseline$baseline_test_blocks))
  baseline_expectations <- suppressWarnings(as.numeric(timing_baseline$baseline_expectations))
  if (anyNA(baseline_real) || any(!is.finite(baseline_real)) || any(baseline_real < 0)) {
    add_issue("timing baseline seconds must be finite and non-negative")
  }
  if (anyNA(baseline_blocks) || any(baseline_blocks < 1) ||
      any(baseline_blocks != as.integer(baseline_blocks))) {
    add_issue("timing baseline blocks must be positive integers")
  }
  if (anyNA(baseline_expectations) || any(baseline_expectations < 1) ||
      any(baseline_expectations != as.integer(baseline_expectations))) {
    add_issue("timing baseline expectations must be positive integers")
  }

  ordinary <- budget$dependency_mode == "full"
  if (any(numeric_skips[ordinary] != 0L) ||
      any(budget$allowed_skip_ids[ordinary] != "none")) {
    add_issue("ordinary full profiles must have expected_skips=0 and no exceptions")
  }
  ordinary_required <- lapply(budget$required_present[ordinary], sm_test_arch_set)
  if (any(!vapply(
    ordinary_required,
    function(x) identical(sort(x), c("dplyr", "mgcv")),
    logical(1)
  ))) {
    add_issue("ordinary full profiles must require dplyr and mgcv explicitly")
  }

  for (i in seq_len(nrow(budget))) {
    present <- sm_test_arch_set(budget$required_present[[i]])
    absent <- sm_test_arch_set(budget$required_absent[[i]])
    allowed <- sm_test_arch_set(budget$allowed_skip_ids[[i]])
    if (length(intersect(present, absent))) {
      add_issue(paste0("profile `", budget$profile[[i]], "` requires a dependency both present and absent"))
    }
    if (budget$test_scope[[i]] == "full" && length(allowed) != numeric_skips[[i]]) {
      add_issue(paste0("profile `", budget$profile[[i]], "` skip identities do not match its count"))
    }
    if (budget$test_scope[[i]] == "none" &&
        (numeric_skips[[i]] != 0L || length(allowed))) {
      add_issue(paste0("non-test profile `", budget$profile[[i]], "` has a skip budget"))
    }
  }

  scanned <- sm_test_arch_scan_skips(root, actual)
  if (nrow(scanned$broad_skips)) {
    add_issue("broad or non-dependency skip calls are present")
  }
  allowed_union <- unique(unlist(
    lapply(budget$allowed_skip_ids, sm_test_arch_set),
    use.names = FALSE
  ))
  actual_skip_ids <- unique(scanned$skip_sites$identity)
  if (!identical(sort(allowed_union), sort(actual_skip_ids))) {
    add_issue("approved skip identities do not exactly match source skip sites")
  }
  if (nrow(scanned$skip_sites)) {
    for (i in seq_len(nrow(scanned$skip_sites))) {
      row <- scanned$skip_sites[i, , drop = FALSE]
      taxonomy_row <- taxonomy[taxonomy$path == row$path, , drop = FALSE]
      if (nrow(taxonomy_row) != 1L ||
          !identical(taxonomy_row$optional_dependency[[1L]], row$dependency[[1L]])) {
        add_issue(paste0("optional dependency taxonomy disagrees at `", row$identity[[1L]], "`"))
      }
    }
  }

  list(
    taxonomy = taxonomy,
    budget = budget,
    timing_budget = timing_budget,
    timing_baseline = timing_baseline,
    actual_files = actual,
    skip_sites = scanned$skip_sites,
    broad_skips = scanned$broad_skips,
    issues = unique(issues)
  )
}

sm_test_arch_package_available <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    return(FALSE)
  }
  if (identical(package, "mgcv")) {
    return(utils::packageVersion(package) >= package_version("1.9.0"))
  }
  TRUE
}

sm_test_arch_dependency_state <- function(profile_row) {
  present <- sm_test_arch_set(profile_row$required_present[[1L]])
  absent <- sm_test_arch_set(profile_row$required_absent[[1L]])
  packages <- sort(unique(c(present, absent)))
  availability <- if (length(packages)) {
    vapply(packages, sm_test_arch_package_available, logical(1))
  } else {
    stats::setNames(logical(), character())
  }
  missing_present <- present[!vapply(present, sm_test_arch_package_available, logical(1))]
  unexpected_present <- absent[vapply(absent, sm_test_arch_package_available, logical(1))]
  list(
    ok = length(missing_present) == 0L && length(unexpected_present) == 0L,
    availability = availability,
    missing_present = missing_present,
    unexpected_present = unexpected_present
  )
}

sm_test_arch_result_path <- function(file) {
  file <- gsub("\\\\", "/", file)
  if (startsWith(file, "tests/testthat/")) {
    return(file)
  }
  if (startsWith(file, "test-")) {
    return(file.path("tests", "testthat", file))
  }
  file
}

sm_test_arch_skip_reason <- function(results) {
  skipped <- Filter(function(x) inherits(x, "expectation_skip"), results)
  if (!length(skipped)) {
    return("")
  }
  paste(unique(vapply(skipped, conditionMessage, character(1))), collapse = " | ")
}

sm_test_arch_empty_results <- function() {
  data.frame(
    run_id = integer(), seed = integer(), shuffle = logical(),
    parallel = logical(), file = character(), context = character(),
    test = character(),
    nb = integer(), failed = integer(), skipped = logical(),
    error = logical(), warning = integer(), user = numeric(),
    system = numeric(), real = numeric(), passed = integer(),
    stringsAsFactors = FALSE
  )
}

sm_test_arch_empty_skips <- function() {
  data.frame(
    run_id = integer(), seed = integer(), shuffle = logical(),
    parallel = logical(), file = character(), test = character(),
    identity = character(), reason = character(),
    stringsAsFactors = FALSE
  )
}

sm_test_arch_empty_runs <- function() {
  data.frame(
    run_id = integer(), seed = integer(), shuffle = logical(),
    shuffle_scope = character(), parallel = logical(), testthat_cpus = integer(),
    testthat_cpus_source = character(), parallel_workers = integer(),
    execution_engine = character(), load_package = character(),
    file_order_signature = character(),
    test_blocks = integer(), expectations = integer(), passed = integer(),
    failed = integer(), errors = integer(), warnings = integer(),
    skips = integer(), real_seconds = numeric(), wall_seconds = numeric(),
    suite_budget_seconds = numeric(), max_block_seconds = numeric(),
    slow_block_threshold_seconds = numeric(), max_slow_blocks = integer(),
    observed_max_block_seconds = numeric(), observed_slow_blocks = integer(),
    timing_status = character(), result_signature = character(), status = character(),
    stringsAsFactors = FALSE
  )
}

sm_test_arch_empty_timing <- function() {
  data.frame(
    run_id = integer(), file = character(), test_blocks = integer(),
    expectations = integer(), real_seconds = numeric(),
    baseline_real_seconds = numeric(), baseline_ratio = numeric(),
    slow_blocks = integer(), max_block_seconds = numeric(),
    stringsAsFactors = FALSE
  )
}

sm_test_arch_empty_timing_summary <- function() {
  data.frame(
    profile = character(), run_id = integer(), wall_seconds = numeric(),
    suite_budget_seconds = numeric(), observed_max_block_seconds = numeric(),
    max_block_seconds = numeric(), observed_slow_blocks = integer(),
    slow_block_threshold_seconds = numeric(), max_slow_blocks = integer(),
    timing_status = character(),
    stringsAsFactors = FALSE
  )
}

sm_test_arch_empty_slow_tests <- function() {
  data.frame(
    run_id = integer(), file = character(), test = character(),
    real_seconds = numeric(), slow_block_threshold_seconds = numeric(),
    stringsAsFactors = FALSE
  )
}

sm_test_arch_timing_summary <- function(runs, profile) {
  if (!nrow(runs)) {
    return(sm_test_arch_empty_timing_summary())
  }
  data.frame(
    profile = rep(profile, nrow(runs)),
    run_id = runs$run_id,
    wall_seconds = runs$wall_seconds,
    suite_budget_seconds = runs$suite_budget_seconds,
    observed_max_block_seconds = runs$observed_max_block_seconds,
    max_block_seconds = runs$max_block_seconds,
    observed_slow_blocks = runs$observed_slow_blocks,
    slow_block_threshold_seconds = runs$slow_block_threshold_seconds,
    max_slow_blocks = runs$max_slow_blocks,
    timing_status = runs$timing_status,
    stringsAsFactors = FALSE
  )
}

sm_test_arch_slow_tests <- function(results, threshold) {
  if (!nrow(results)) {
    return(sm_test_arch_empty_slow_tests())
  }
  selected <- !is.na(results$real) & results$real > threshold
  if (!any(selected)) {
    return(sm_test_arch_empty_slow_tests())
  }
  data.frame(
    run_id = results$run_id[selected],
    file = vapply(
      results$file[selected],
      sm_test_arch_result_path,
      character(1)
    ),
    test = results$test[selected],
    real_seconds = results$real[selected],
    slow_block_threshold_seconds = rep(threshold, sum(selected)),
    stringsAsFactors = FALSE
  )
}

sm_test_arch_timing_policy <- function(static, profile) {
  rows <- static$timing_budget[
    static$timing_budget$profile == profile,
    ,
    drop = FALSE
  ]
  if (nrow(rows) != 1L) {
    stop("`profile` must name exactly one timing-budget row.", call. = FALSE)
  }
  rows[1L, , drop = FALSE]
}

sm_test_arch_timing_evaluate <- function(wall_seconds, block_seconds, policy) {
  wall_seconds <- suppressWarnings(as.numeric(wall_seconds))
  block_seconds <- suppressWarnings(as.numeric(block_seconds))
  if (length(wall_seconds) != 1L || is.na(wall_seconds) ||
      !is.finite(wall_seconds) || wall_seconds < 0 ||
      anyNA(block_seconds) || any(!is.finite(block_seconds)) ||
      any(block_seconds < 0)) {
    stop("Observed test timing values must be finite and non-negative.", call. = FALSE)
  }
  suite_budget <- as.numeric(policy$suite_budget_seconds[[1L]])
  block_budget <- as.numeric(policy$max_block_seconds[[1L]])
  slow_threshold <- as.numeric(policy$slow_block_threshold_seconds[[1L]])
  slow_budget <- as.integer(policy$max_slow_blocks[[1L]])
  observed_max <- if (length(block_seconds)) max(block_seconds) else 0
  observed_slow <- sum(block_seconds > slow_threshold)
  suite_ok <- wall_seconds <= suite_budget
  block_ok <- observed_max <= block_budget
  slow_ok <- observed_slow <= slow_budget
  list(
    passed = isTRUE(suite_ok && block_ok && slow_ok),
    status = if (isTRUE(suite_ok && block_ok && slow_ok)) "PASS" else "FAIL",
    suite_ok = suite_ok,
    block_ok = block_ok,
    slow_ok = slow_ok,
    observed_max_block_seconds = observed_max,
    observed_slow_blocks = as.integer(observed_slow),
    violation_count = sum(!c(suite_ok, block_ok, slow_ok))
  )
}

sm_test_arch_timing_by_file <- function(raw, run_id, static, policy) {
  files <- vapply(raw$file, sm_test_arch_result_path, character(1))
  paths <- sort(unique(files))
  rows <- lapply(paths, function(path) {
    selected <- files == path
    baseline <- static$timing_baseline[
      static$timing_baseline$path == path,
      ,
      drop = FALSE
    ]
    baseline_seconds <- if (nrow(baseline) == 1L) {
      as.numeric(baseline$baseline_real_seconds[[1L]])
    } else {
      NA_real_
    }
    observed <- sum(raw$real[selected])
    data.frame(
      run_id = as.integer(run_id),
      file = path,
      test_blocks = sum(selected),
      expectations = sum(raw$nb[selected]),
      real_seconds = observed,
      baseline_real_seconds = baseline_seconds,
      baseline_ratio = if (is.finite(baseline_seconds) && baseline_seconds > 0) {
        observed / baseline_seconds
      } else {
        NA_real_
      },
      slow_blocks = sum(
        raw$real[selected] > as.numeric(policy$slow_block_threshold_seconds[[1L]])
      ),
      max_block_seconds = max(raw$real[selected]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

sm_test_arch_testthat_cpu_config <- function(parallel) {
  if (!isTRUE(parallel)) {
    return(list(value = NA_integer_, source = "not_requested"))
  }
  option_value <- getOption("Ncpus", NULL)
  if (!is.null(option_value)) {
    cpus <- suppressWarnings(as.integer(option_value))
    if (length(cpus) != 1L || is.na(cpus) || cpus < 1L) {
      stop("`getOption(\"Ncpus\")` must be one positive integer.", call. = FALSE)
    }
    return(list(value = cpus, source = "option:Ncpus"))
  }
  value <- Sys.getenv("TESTTHAT_CPUS", "")
  if (!nzchar(value)) {
    return(list(value = 2L, source = "testthat_default"))
  }
  cpus <- suppressWarnings(as.integer(value))
  if (length(cpus) != 1L || is.na(cpus) || cpus < 1L) {
    stop("`TESTTHAT_CPUS` must be one positive integer.", call. = FALSE)
  }
  list(value = cpus, source = "env:TESTTHAT_CPUS")
}

sm_test_arch_validate_run_controls <- function(
  shuffle = FALSE,
  parallel = FALSE,
  seed = NULL,
  repeats = 1L
) {
  if (!is.logical(shuffle) || length(shuffle) != 1L || is.na(shuffle)) {
    stop("`shuffle` must be one non-missing logical value.", call. = FALSE)
  }
  if (!is.logical(parallel) || length(parallel) != 1L || is.na(parallel)) {
    stop("`parallel` must be one non-missing logical value.", call. = FALSE)
  }

  repeats_number <- suppressWarnings(as.numeric(repeats))
  repeats_integer <- suppressWarnings(as.integer(repeats_number))
  if (length(repeats_number) != 1L || is.na(repeats_number) ||
      !is.finite(repeats_number) || is.na(repeats_integer) ||
      repeats_number != repeats_integer ||
      repeats_number < 1L || repeats_number > 3L) {
    stop("`repeats` must be one integer from 1 through 3.", call. = FALSE)
  }
  repeats <- as.integer(repeats_number)

  if (is.null(seed)) {
    if (isTRUE(shuffle) || isTRUE(parallel) || repeats > 1L) {
      stop(
        "`seed` is required when shuffle, parallel, or repeated runs are requested.",
        call. = FALSE
      )
    }
    seeds <- rep(NA_integer_, repeats)
  } else {
    seed_number <- suppressWarnings(as.numeric(seed))
    seed_integer <- suppressWarnings(as.integer(seed_number))
    max_seed <- .Machine$integer.max - repeats + 1
    if (length(seed_number) != 1L || is.na(seed_number) ||
        !is.finite(seed_number) || is.na(seed_integer) ||
        seed_number != seed_integer ||
        seed_number < 0 || seed_number > max_seed) {
      stop(
        "`seed` must be one non-negative integer that leaves room for all repeated-run seeds.",
        call. = FALSE
      )
    }
    seeds <- as.integer(seed_number + seq_len(repeats) - 1L)
  }

  cpu_config <- sm_test_arch_testthat_cpu_config(parallel)
  list(
    shuffle = isTRUE(shuffle),
    shuffle_scope = if (isTRUE(shuffle)) "file_order" else "none",
    parallel = isTRUE(parallel),
    testthat_cpus = cpu_config$value,
    testthat_cpus_source = cpu_config$source,
    repeats = repeats,
    seeds = seeds
  )
}

sm_test_arch_order_test_paths <- function(test_paths, shuffle) {
  if (isTRUE(shuffle)) {
    sample(test_paths, length(test_paths), replace = FALSE)
  } else {
    test_paths
  }
}

sm_test_arch_file_order_signature <- function(test_paths) {
  path <- tempfile("sitemix-test-file-order-", fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  writeLines(enc2utf8(test_paths), path, useBytes = TRUE)
  unname(tools::md5sum(path))
}

sm_test_arch_load_mode <- function(root) {
  description_path <- file.path(root, "DESCRIPTION")
  if (!file.exists(description_path)) {
    description_path <- tryCatch(
      system.file("DESCRIPTION", package = "sitemix"),
      error = function(condition) ""
    )
  }
  if (!nzchar(description_path) || !file.exists(description_path)) {
    stop("Package DESCRIPTION is unavailable for the test architecture.", call. = FALSE)
  }
  description <- read.dcf(description_path)
  built <- "Built" %in% colnames(description) &&
    nzchar(description[[1L, "Built"]])
  if (built) "installed" else "source"
}

sm_test_arch_test_files_api <- function() {
  test_files <- utils::getFromNamespace("test_files", "testthat")
  required_formals <- c(
    "test_dir", "test_package", "test_paths", "reporter",
    "stop_on_failure", "stop_on_warning", "load_package", "parallel",
    "shuffle"
  )
  missing_formals <- setdiff(required_formals, names(formals(test_files)))
  if (length(missing_formals)) {
    stop(
      "Installed testthat has an incompatible internal `test_files()` interface; missing: ",
      paste(missing_formals, collapse = ", "),
      call. = FALSE
    )
  }
  test_files
}

sm_test_arch_find_scripts_api <- function() {
  find_scripts <- utils::getFromNamespace("find_test_scripts", "testthat")
  required_formals <- c("path", "full.names")
  missing_formals <- setdiff(required_formals, names(formals(find_scripts)))
  if (length(missing_formals)) {
    stop(
      "Installed testthat has an incompatible `find_test_scripts()` interface; missing: ",
      paste(missing_formals, collapse = ", "),
      call. = FALSE
    )
  }
  find_scripts
}

sm_test_arch_run_once <- function(root, shuffle, parallel) {
  test_dir <- file.path(root, "tests", "testthat")
  find_scripts <- sm_test_arch_find_scripts_api()
  test_paths <- find_scripts(
    test_dir,
    full.names = FALSE
  )
  if (!length(test_paths)) {
    stop("No test files were found for the audit run.", call. = FALSE)
  }
  test_paths <- sm_test_arch_order_test_paths(test_paths, shuffle)
  test_files <- sm_test_arch_test_files_api()

  description_path <- file.path(root, "DESCRIPTION")
  if (!file.exists(description_path)) {
    description_path <- system.file("DESCRIPTION", package = "sitemix")
  }
  package <- unname(read.dcf(description_path, fields = "Package")[[1L]])
  result <- test_files(
    test_dir = test_dir,
    test_package = package,
    test_paths = test_paths,
    reporter = testthat::SilentReporter$new(),
    stop_on_failure = FALSE,
    stop_on_warning = FALSE,
    load_package = sm_test_arch_load_mode(root),
    parallel = isTRUE(parallel),
    shuffle = FALSE
  )
  attr(result, "sitemix_file_order_signature") <-
    sm_test_arch_file_order_signature(test_paths)
  attr(result, "sitemix_test_file_count") <- length(test_paths)
  result
}

sm_test_arch_capture_rng <- function() {
  seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  list(
    kind = RNGkind(),
    seed_exists = seed_exists,
    seed = if (seed_exists) {
      get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
      NULL
    }
  )
}

sm_test_arch_restore_rng <- function(state) {
  RNGkind(
    kind = state$kind[[1L]],
    normal.kind = state$kind[[2L]],
    sample.kind = state$kind[[3L]]
  )
  if (isTRUE(state$seed_exists)) {
    assign(".Random.seed", state$seed, envir = .GlobalEnv)
  } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  invisible(NULL)
}

sm_test_arch_capture_env <- function(name) {
  value <- Sys.getenv(name, unset = NA_character_)
  list(name = name, existed = !is.na(value), value = value)
}

sm_test_arch_restore_env <- function(state) {
  if (isTRUE(state$existed)) {
    value <- state$value
    names(value) <- state$name
    do.call(Sys.setenv, as.list(value))
  } else {
    Sys.unsetenv(state$name)
  }
  invisible(NULL)
}

sm_test_arch_result_signature <- function(results) {
  columns <- intersect(
    c(
      "file", "context", "test", "nb", "failed", "skipped", "error",
      "warning", "passed"
    ),
    names(results)
  )
  normalized <- results[columns]
  if (nrow(normalized)) {
    normalized$file <- vapply(
      normalized$file,
      sm_test_arch_result_path,
      character(1)
    )
    ordering <- do.call(order, c(normalized, list(method = "radix")))
    normalized <- normalized[ordering, , drop = FALSE]
    row.names(normalized) <- NULL
  }
  path <- tempfile("sitemix-test-signature-", fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(normalized, path, version = 2)
  unname(tools::md5sum(path))
}

sm_test_arch_write_csv <- function(x, path) {
  utils::write.csv(x, path, row.names = FALSE, na = "")
}

sm_test_arch_audit <- function(
  root,
  profile = "local-full",
  out_dir = file.path(root, "ci-artifacts", "test-budget", profile),
  static_only = FALSE,
  shuffle = FALSE,
  parallel = FALSE,
  seed = NULL,
  repeats = 1L
) {
  started <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  controls <- sm_test_arch_validate_run_controls(
    shuffle = shuffle,
    parallel = parallel,
    seed = seed,
    repeats = repeats
  )
  static <- sm_test_arch_validate_static(root)
  profile_rows <- static$budget[static$budget$profile == profile, , drop = FALSE]
  if (nrow(profile_rows) != 1L) {
    stop("`profile` must name exactly one row in job-skip-budget.csv.", call. = FALSE)
  }
  profile_row <- profile_rows[1L, , drop = FALSE]
  timing_policy <- sm_test_arch_timing_policy(static, profile)
  dependency <- sm_test_arch_dependency_state(profile_row)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  issues_frame <- data.frame(issue = static$issues, stringsAsFactors = FALSE)
  sm_test_arch_write_csv(issues_frame, file.path(out_dir, "static-issues.csv"))
  sm_test_arch_write_csv(static$skip_sites, file.path(out_dir, "static-skip-sites.csv"))

  category_counts <- as.data.frame(table(
    factor(
      static$taxonomy$primary_category,
      levels = c("unit", "oracle", "invariant", "regression", "snapshot", "integration", "release")
    )
  ), stringsAsFactors = FALSE)
  names(category_counts) <- c("primary_category", "n_files")
  sm_test_arch_write_csv(category_counts, file.path(out_dir, "taxonomy-summary.csv"))

  results_frame <- sm_test_arch_empty_results()
  skips_frame <- sm_test_arch_empty_skips()
  runs_frame <- sm_test_arch_empty_runs()
  timing_frame <- sm_test_arch_empty_timing()
  run_errors <- character()
  should_run <- !isTRUE(static_only) && identical(profile_row$test_scope[[1L]], "full")
  not_cran_state <- sm_test_arch_capture_env("NOT_CRAN")
  if (should_run) {
    Sys.setenv(NOT_CRAN = "true")
    on.exit(sm_test_arch_restore_env(not_cran_state), add = TRUE)
  }
  expected_ids <- sm_test_arch_set(profile_row$allowed_skip_ids[[1L]])
  execution_engine <- "testthat:::test_files"
  load_package <- sm_test_arch_load_mode(root)
  parallel_workers <- if (controls$parallel) {
    min(controls$testthat_cpus, length(static$actual_files))
  } else {
    1L
  }

  if (should_run) {
    if (!requireNamespace("testthat", quietly = TRUE)) {
      run_errors <- "testthat is unavailable"
    } else {
      rng_state <- sm_test_arch_capture_rng()
      on.exit(sm_test_arch_restore_rng(rng_state), add = TRUE)
      result_rows <- vector("list", controls$repeats)
      skip_rows <- vector("list", controls$repeats)
      run_rows <- vector("list", controls$repeats)
      timing_rows <- vector("list", controls$repeats)

      for (run_id in seq_len(controls$repeats)) {
        run_seed <- controls$seeds[[run_id]]
        if (!is.na(run_seed)) {
          set.seed(
            run_seed,
            kind = "Mersenne-Twister",
            normal.kind = "Inversion",
            sample.kind = "Rejection"
          )
        }
        run_started_elapsed <- proc.time()[["elapsed"]]
        run <- tryCatch(
          sm_test_arch_run_once(
            root = root,
            shuffle = controls$shuffle,
            parallel = controls$parallel
          ),
          error = function(error) error
        )
        wall_seconds <- proc.time()[["elapsed"]] - run_started_elapsed
        if (inherits(run, "error")) {
          message <- conditionMessage(run)
          run_errors <- c(
            run_errors,
            paste0("run ", run_id, ": ", message)
          )
          run_rows[[run_id]] <- data.frame(
            run_id = as.integer(run_id),
            seed = run_seed,
            shuffle = controls$shuffle,
            shuffle_scope = controls$shuffle_scope,
            parallel = controls$parallel,
            testthat_cpus = controls$testthat_cpus,
            testthat_cpus_source = controls$testthat_cpus_source,
            parallel_workers = parallel_workers,
            execution_engine = execution_engine,
            load_package = load_package,
            file_order_signature = "",
            test_blocks = NA_integer_,
            expectations = NA_integer_,
            passed = NA_integer_,
            failed = NA_integer_,
            errors = NA_integer_,
            warnings = NA_integer_,
            skips = NA_integer_,
            real_seconds = NA_real_,
            wall_seconds = wall_seconds,
            suite_budget_seconds = as.numeric(timing_policy$suite_budget_seconds[[1L]]),
            max_block_seconds = as.numeric(timing_policy$max_block_seconds[[1L]]),
            slow_block_threshold_seconds = as.numeric(
              timing_policy$slow_block_threshold_seconds[[1L]]
            ),
            max_slow_blocks = as.integer(timing_policy$max_slow_blocks[[1L]]),
            observed_max_block_seconds = NA_real_,
            observed_slow_blocks = NA_integer_,
            timing_status = "ERROR",
            result_signature = "",
            status = "ERROR",
            stringsAsFactors = FALSE
          )
          next
        }

        raw <- as.data.frame(run)
        timing_rows[[run_id]] <- sm_test_arch_timing_by_file(
          raw = raw,
          run_id = run_id,
          static = static,
          policy = timing_policy
        )
        result_piece <- raw[setdiff(names(raw), "result")]
        result_piece <- cbind(
          data.frame(
            run_id = rep(as.integer(run_id), nrow(result_piece)),
            seed = rep(run_seed, nrow(result_piece)),
            shuffle = rep(controls$shuffle, nrow(result_piece)),
            parallel = rep(controls$parallel, nrow(result_piece)),
            stringsAsFactors = FALSE
          ),
          result_piece
        )
        result_rows[[run_id]] <- result_piece

        skipped_index <- which(raw$skipped)
        if (length(skipped_index)) {
          skip_file <- vapply(raw$file[skipped_index], sm_test_arch_result_path, character(1))
          skip_test <- raw$test[skipped_index]
          skip_rows[[run_id]] <- data.frame(
            run_id = rep(as.integer(run_id), length(skipped_index)),
            seed = rep(run_seed, length(skipped_index)),
            shuffle = rep(controls$shuffle, length(skipped_index)),
            parallel = rep(controls$parallel, length(skipped_index)),
            file = skip_file,
            test = skip_test,
            identity = paste0(skip_file, "::", skip_test),
            reason = vapply(raw$result[skipped_index], sm_test_arch_skip_reason, character(1)),
            stringsAsFactors = FALSE
          )
        }

        actual_run_ids <- if (length(skipped_index)) {
          skip_rows[[run_id]]$identity
        } else {
          character()
        }
        run_skip_match <- identical(sort(actual_run_ids), sort(expected_ids))
        timing_decision <- sm_test_arch_timing_evaluate(
          wall_seconds = wall_seconds,
          block_seconds = raw$real,
          policy = timing_policy
        )
        run_ok <- sum(raw$failed) == 0L &&
          sum(raw$error) == 0L &&
          sum(raw$warning) == 0L &&
          isTRUE(run_skip_match) &&
          isTRUE(timing_decision$passed)
        run_rows[[run_id]] <- data.frame(
          run_id = as.integer(run_id),
          seed = run_seed,
          shuffle = controls$shuffle,
          shuffle_scope = controls$shuffle_scope,
          parallel = controls$parallel,
          testthat_cpus = controls$testthat_cpus,
          testthat_cpus_source = controls$testthat_cpus_source,
          parallel_workers = parallel_workers,
          execution_engine = execution_engine,
          load_package = load_package,
          file_order_signature = attr(run, "sitemix_file_order_signature"),
          test_blocks = nrow(raw),
          expectations = sum(raw$nb),
          passed = sum(raw$passed),
          failed = sum(raw$failed),
          errors = sum(raw$error),
          warnings = sum(raw$warning),
          skips = sum(raw$skipped),
          real_seconds = sum(raw$real),
          wall_seconds = wall_seconds,
          suite_budget_seconds = as.numeric(timing_policy$suite_budget_seconds[[1L]]),
          max_block_seconds = as.numeric(timing_policy$max_block_seconds[[1L]]),
          slow_block_threshold_seconds = as.numeric(
            timing_policy$slow_block_threshold_seconds[[1L]]
          ),
          max_slow_blocks = as.integer(timing_policy$max_slow_blocks[[1L]]),
          observed_max_block_seconds = timing_decision$observed_max_block_seconds,
          observed_slow_blocks = timing_decision$observed_slow_blocks,
          timing_status = timing_decision$status,
          result_signature = sm_test_arch_result_signature(raw),
          status = if (run_ok) "PASS" else "FAIL",
          stringsAsFactors = FALSE
        )
      }

      nonempty_results <- Filter(Negate(is.null), result_rows)
      if (length(nonempty_results)) {
        results_frame <- do.call(rbind, nonempty_results)
        row.names(results_frame) <- NULL
      }
      nonempty_skips <- Filter(Negate(is.null), skip_rows)
      if (length(nonempty_skips)) {
        skips_frame <- do.call(rbind, nonempty_skips)
        row.names(skips_frame) <- NULL
      }
      nonempty_runs <- Filter(Negate(is.null), run_rows)
      if (length(nonempty_runs)) {
        runs_frame <- do.call(rbind, nonempty_runs)
        row.names(runs_frame) <- NULL
      }
      nonempty_timing <- Filter(Negate(is.null), timing_rows)
      if (length(nonempty_timing)) {
        timing_frame <- do.call(rbind, nonempty_timing)
        row.names(timing_frame) <- NULL
      }
    }
  }

  sm_test_arch_write_csv(results_frame, file.path(out_dir, "test-results.csv"))
  sm_test_arch_write_csv(skips_frame, file.path(out_dir, "test-skips.csv"))
  sm_test_arch_write_csv(runs_frame, file.path(out_dir, "test-runs.csv"))
  sm_test_arch_write_csv(timing_frame, file.path(out_dir, "test-timing.csv"))
  sm_test_arch_write_csv(timing_frame, file.path(out_dir, "test-timing-by-file.csv"))
  timing_summary_frame <- sm_test_arch_timing_summary(runs_frame, profile)
  sm_test_arch_write_csv(
    timing_summary_frame,
    file.path(out_dir, "test-timing-summary.csv")
  )
  slow_tests_frame <- sm_test_arch_slow_tests(
    results_frame,
    threshold = as.numeric(timing_policy$slow_block_threshold_seconds[[1L]])
  )
  sm_test_arch_write_csv(slow_tests_frame, file.path(out_dir, "slow-tests.csv"))

  actual_ids <- unique(skips_frame$identity)
  skip_match <- if (should_run && !length(run_errors) && nrow(runs_frame)) {
    all(vapply(seq_len(nrow(runs_frame)), function(run_id) {
      ids <- skips_frame$identity[skips_frame$run_id == run_id]
      identical(sort(ids), sort(expected_ids))
    }, logical(1)))
  } else {
    NA
  }
  signature_match <- if (should_run && !length(run_errors) && nrow(runs_frame)) {
    length(unique(runs_frame$result_signature)) == 1L
  } else {
    NA
  }
  file_orders_distinct <- if (
    should_run && !length(run_errors) && nrow(runs_frame) &&
      controls$shuffle && controls$repeats > 1L
  ) {
    length(unique(runs_frame$file_order_signature)) == controls$repeats
  } else if (should_run && !length(run_errors) && nrow(runs_frame)) {
    TRUE
  } else {
    NA
  }
  timing_match <- if (should_run && !length(run_errors) && nrow(runs_frame)) {
    all(runs_frame$timing_status == "PASS")
  } else {
    NA
  }
  slowest_timing <- if (nrow(timing_frame)) {
    timing_frame[which.max(timing_frame$real_seconds), , drop = FALSE]
  } else {
    timing_frame
  }
  timing_violation_count <- if (should_run && nrow(runs_frame)) {
    sum(runs_frame$wall_seconds > runs_frame$suite_budget_seconds, na.rm = TRUE) +
      sum(
        runs_frame$observed_max_block_seconds > runs_frame$max_block_seconds,
        na.rm = TRUE
      ) +
      sum(runs_frame$observed_slow_blocks > runs_frame$max_slow_blocks, na.rm = TRUE)
  } else {
    NA_integer_
  }
  test_ok <- if (should_run && !length(run_errors)) {
    nrow(runs_frame) == controls$repeats &&
      all(runs_frame$status == "PASS") &&
      isTRUE(skip_match) &&
      isTRUE(signature_match) &&
      isTRUE(file_orders_distinct) &&
      isTRUE(timing_match)
  } else if (should_run) {
    FALSE
  } else {
    TRUE
  }
  static_ok <- length(static$issues) == 0L
  status <- if (static_ok && dependency$ok && test_ok) "PASS" else "FAIL"

  summary <- data.frame(
    profile = profile,
    started_utc = started,
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    status = status,
    static_only = isTRUE(static_only),
    taxonomy_files = nrow(static$taxonomy),
    actual_test_files = length(static$actual_files),
    static_skip_sites = nrow(static$skip_sites),
    static_broad_skips = nrow(static$broad_skips),
    static_issue_count = length(static$issues),
    dependency_profile_ok = dependency$ok,
    missing_required_dependencies = sm_test_arch_join(dependency$missing_present),
    unexpectedly_present_dependencies = sm_test_arch_join(dependency$unexpected_present),
    shuffle = controls$shuffle,
    shuffle_scope = controls$shuffle_scope,
    parallel = controls$parallel,
    testthat_cpus = controls$testthat_cpus,
    testthat_cpus_source = controls$testthat_cpus_source,
    parallel_workers = parallel_workers,
    execution_engine = execution_engine,
    load_package = load_package,
    repeats = controls$repeats,
    seed_first = if (all(is.na(controls$seeds))) NA_integer_ else controls$seeds[[1L]],
    seed_last = if (all(is.na(controls$seeds))) NA_integer_ else controls$seeds[[controls$repeats]],
    runs_passed = if (should_run && nrow(runs_frame)) sum(runs_frame$status == "PASS") else NA_integer_,
    runs_failed = if (should_run && nrow(runs_frame)) sum(runs_frame$status != "PASS") else NA_integer_,
    result_signature_match = signature_match,
    file_orders_distinct = file_orders_distinct,
    test_blocks = if (should_run && !length(run_errors)) nrow(results_frame) else NA_integer_,
    expectations = if (should_run && !length(run_errors)) sum(results_frame$nb) else NA_integer_,
    passed = if (should_run && !length(run_errors)) sum(results_frame$passed) else NA_integer_,
    failed = if (should_run && !length(run_errors)) sum(results_frame$failed) else NA_integer_,
    errors = if (should_run && !length(run_errors)) sum(results_frame$error) else NA_integer_,
    warnings = if (should_run && !length(run_errors)) sum(results_frame$warning) else NA_integer_,
    expected_skips = as.integer(profile_row$expected_skips[[1L]]),
    actual_skips = if (should_run && !length(run_errors)) nrow(skips_frame) else NA_integer_,
    exact_skip_identity_match = skip_match,
    expected_skip_ids = sm_test_arch_join(expected_ids),
    actual_skip_ids = sm_test_arch_join(actual_ids),
    not_cran_scoped = should_run,
    not_cran_prior = if (isTRUE(not_cran_state$existed)) {
      not_cran_state$value
    } else {
      "<unset>"
    },
    timing_gate_applied = should_run,
    timing_budget_ok = timing_match,
    suite_budget_seconds = as.numeric(timing_policy$suite_budget_seconds[[1L]]),
    max_block_budget_seconds = as.numeric(timing_policy$max_block_seconds[[1L]]),
    slow_block_threshold_seconds = as.numeric(
      timing_policy$slow_block_threshold_seconds[[1L]]
    ),
    max_slow_blocks = as.integer(timing_policy$max_slow_blocks[[1L]]),
    max_wall_seconds = if (should_run && nrow(runs_frame)) {
      max(runs_frame$wall_seconds, na.rm = TRUE)
    } else {
      NA_real_
    },
    observed_max_block_seconds = if (should_run && nrow(runs_frame)) {
      max(runs_frame$observed_max_block_seconds, na.rm = TRUE)
    } else {
      NA_real_
    },
    observed_slow_blocks = if (should_run && nrow(runs_frame)) {
      max(runs_frame$observed_slow_blocks, na.rm = TRUE)
    } else {
      NA_integer_
    },
    slowest_file = if (nrow(slowest_timing)) slowest_timing$file[[1L]] else "",
    slowest_file_seconds = if (nrow(slowest_timing)) {
      slowest_timing$real_seconds[[1L]]
    } else {
      NA_real_
    },
    timing_violation_count = timing_violation_count,
    timing_baseline_source = paste(unique(static$timing_baseline$source_run), collapse = "|"),
    test_run_error = paste(run_errors, collapse = " | "),
    stringsAsFactors = FALSE
  )
  sm_test_arch_write_csv(summary, file.path(out_dir, "test-summary.csv"))

  list(
    status = status,
    summary = summary,
    runs = runs_frame,
    timing = timing_frame,
    timing_summary = timing_summary_frame,
    slow_tests = slow_tests_frame,
    static = static,
    out_dir = out_dir
  )
}

sm_test_arch_arg <- function(args, name, default = NULL) {
  prefix <- paste0(name, "=")
  value <- args[startsWith(args, prefix)]
  if (!length(value)) default else sub(paste0("^", prefix), "", value[[1L]])
}

sm_test_arch_parse_args <- function(args) {
  value_options <- c("--profile", "--out-dir", "--seed", "--repeats")
  flag_options <- c("--static-only", "--shuffle", "--parallel")
  known <- args %in% flag_options
  for (option in value_options) {
    known <- known | startsWith(args, paste0(option, "="))
  }
  if (any(!known)) {
    stop("Unknown arguments: ", paste(args[!known], collapse = ", "), call. = FALSE)
  }
  for (option in flag_options) {
    if (sum(args == option) > 1L) {
      stop("Duplicate argument: ", option, call. = FALSE)
    }
  }
  for (option in value_options) {
    matches <- startsWith(args, paste0(option, "="))
    if (sum(matches) > 1L) {
      stop("Duplicate argument: ", option, call. = FALSE)
    }
    if (any(matches) && !nzchar(sm_test_arch_arg(args, option, ""))) {
      stop("Argument requires a value: ", option, call. = FALSE)
    }
  }

  list(
    static_only = "--static-only" %in% args,
    shuffle = "--shuffle" %in% args,
    parallel = "--parallel" %in% args,
    profile = sm_test_arch_arg(args, "--profile", "local-full"),
    out_dir = sm_test_arch_arg(args, "--out-dir", NULL),
    seed = sm_test_arch_arg(args, "--seed", NULL),
    repeats = sm_test_arch_arg(args, "--repeats", "1")
  )
}

sm_test_arch_main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  parsed <- sm_test_arch_parse_args(args)
  root <- sm_test_arch_root()
  profile <- parsed$profile
  out_dir <- if (is.null(parsed$out_dir)) {
    file.path(root, "ci-artifacts", "test-budget", profile)
  } else {
    parsed$out_dir
  }
  result <- sm_test_arch_audit(
    root = root,
    profile = profile,
    out_dir = out_dir,
    static_only = parsed$static_only,
    shuffle = parsed$shuffle,
    parallel = parsed$parallel,
    seed = parsed$seed,
    repeats = parsed$repeats
  )
  cat(
    "test-budget audit ", result$status,
    "; profile=", profile,
    "; shuffle=", parsed$shuffle,
    "; parallel=", parsed$parallel,
    "; repeats=", result$summary$repeats,
    "; artifacts=", normalizePath(result$out_dir, mustWork = TRUE),
    "\n",
    sep = ""
  )
  if (identical(result$status, "PASS")) 0L else 1L
}

if (sys.nframe() == 0L) {
  exit_status <- tryCatch(
    sm_test_arch_main(),
    error = function(error) {
      message("test-budget audit ERROR: ", conditionMessage(error))
      2L
    }
  )
  quit(save = "no", status = exit_status, runLast = FALSE)
}
