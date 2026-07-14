test_architecture_root <- normalizePath(
  testthat::test_path("..", ".."),
  mustWork = TRUE
)
test_architecture_split_check_layout <- !file.exists(
  file.path(test_architecture_root, "DESCRIPTION")
)
test_architecture_installed_root <- if (test_architecture_split_check_layout) {
  tryCatch({
    root <- find.package("sitemix", quiet = TRUE)
    if (nzchar(root)) normalizePath(root, mustWork = TRUE) else ""
  }, error = function(condition) "")
} else {
  ""
}

test_architecture_package_candidates <- function(...) {
  relative <- file.path(...)
  installed <- if (nzchar(test_architecture_installed_root)) {
    file.path(test_architecture_installed_root, relative)
  } else {
    character()
  }
  unique(c(
    file.path(test_architecture_root, "inst", relative),
    file.path(test_architecture_root, relative),
    installed
  ))
}

test_architecture_script_candidates <- test_architecture_package_candidates(
  "scripts", "audit-test-budget.R"
)
test_architecture_script <- test_architecture_script_candidates[
  file.exists(test_architecture_script_candidates)
][[1L]]
test_architecture_env <- new.env(parent = baseenv())
sys.source(test_architecture_script, envir = test_architecture_env)

test_coverage_script_candidates <- test_architecture_package_candidates(
  "scripts", "audit-coverage-gate.R"
)
test_coverage_script <- test_coverage_script_candidates[
  file.exists(test_coverage_script_candidates)
][[1L]]
test_coverage_env <- new.env(parent = baseenv())
sys.source(test_coverage_script, envir = test_coverage_env)

test_architecture_manifest_path <- file.path(
  test_architecture_root,
  "tests", "testthat", "_data", "test-architecture",
  "fixture-provenance.csv"
)

test_architecture_read_manifest <- function() {
  utils::read.csv(
    test_architecture_manifest_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
}

test_architecture_source_artifacts <- function() {
  roots <- c(
    file.path(test_architecture_root, "data"),
    file.path(test_architecture_root, "inst", "extdata"),
    file.path(test_architecture_root, "tests", "testthat", "_data"),
    file.path(test_architecture_root, "tests", "testthat", "_snaps")
  )
  paths <- unlist(lapply(roots[dir.exists(roots)], function(path) {
    list.files(path, recursive = TRUE, full.names = TRUE)
  }), use.names = FALSE)
  paths <- paths[!file.info(paths)$isdir]
  root_length <- nchar(normalizePath(test_architecture_root)) + 2L
  relative <- substring(normalizePath(paths), root_length)
  relative <- gsub("\\\\", "/", relative)
  sort(setdiff(
    relative,
    "tests/testthat/_data/test-architecture/fixture-provenance.csv"
  ))
}

test_architecture_resolve_artifact <- function(path) {
  source_path <- file.path(test_architecture_root, path)
  if (file.exists(source_path)) {
    return(source_path)
  }
  if (startsWith(path, "inst/extdata/")) {
    relative <- sub("^inst/", "", path)
    installed_paths <- c(
      file.path(test_architecture_root, relative),
      if (nzchar(test_architecture_installed_root)) {
        file.path(test_architecture_installed_root, relative)
      } else {
        character()
      }
    )
    existing <- installed_paths[file.exists(installed_paths)]
    if (length(existing)) {
      return(existing[[1L]])
    }
  }
  NA_character_
}

test_architecture_resolve_inst <- function(...) {
  relative <- file.path(...)
  candidates <- test_architecture_package_candidates(relative)
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    stop("Missing installed architecture artifact: ", relative, call. = FALSE)
  }
  existing[[1L]]
}

test_architecture_job_blocks <- function(lines) {
  jobs_line <- which(lines == "jobs:")
  if (length(jobs_line) != 1L) {
    stop("A workflow must contain exactly one top-level `jobs:` key.", call. = FALSE)
  }
  starts <- grep("^  [A-Za-z0-9][A-Za-z0-9_-]*:$", lines)
  starts <- starts[starts > jobs_line]
  if (!length(starts)) {
    stop("A workflow must contain at least one job.", call. = FALSE)
  }
  ids <- sub("^  ([A-Za-z0-9][A-Za-z0-9_-]*):$", "\\1", lines[starts])
  ends <- c(starts[-1L] - 1L, length(lines))
  blocks <- Map(function(first, last) {
    lines[seq.int(first, last)]
  }, starts, ends)
  stats::setNames(blocks, ids)
}

test_architecture_action_uses <- function(lines) {
  raw <- grep(
    "^[[:space:]]*-[[:space:]]+uses:[[:space:]]+",
    lines,
    value = TRUE
  )
  reference <- trimws(sub(
    "^[[:space:]]*-[[:space:]]+uses:[[:space:]]+",
    "",
    sub("[[:space:]]+#.*$", "", raw)
  ))
  action_path <- sub("@.*$", "", reference)
  parts <- strsplit(action_path, "/", fixed = TRUE)
  action <- vapply(parts, function(value) {
    paste(value[seq_len(min(2L, length(value)))], collapse = "/")
  }, character(1))
  subaction <- vapply(parts, function(value) {
    if (length(value) > 2L) {
      paste(value[-c(1L, 2L)], collapse = "/")
    } else {
      value[[2L]]
    }
  }, character(1))
  data.frame(
    raw = raw,
    reference = reference,
    action = action,
    subaction = subaction,
    sha = sub("^.*@", "", reference),
    stringsAsFactors = FALSE
  )
}

test_that("every current test file has exactly one primary taxonomy", {
  audited <- test_architecture_env$sm_test_arch_validate_static(
    test_architecture_root
  )
  taxonomy <- audited$taxonomy

  expect_length(audited$issues, 0L)
  expect_identical(nrow(taxonomy), 53L)
  expect_identical(anyDuplicated(taxonomy$path), 0L)
  expect_identical(sort(taxonomy$path), audited$actual_files)
  expect_false(any(grepl(";", taxonomy$primary_category, fixed = TRUE)))
  expect_setequal(
    unique(taxonomy$primary_category),
    c(
      "unit", "oracle", "invariant", "regression", "snapshot",
      "integration", "release"
    )
  )
  expect_identical(
    as.integer(table(factor(
      taxonomy$primary_category,
      levels = c(
        "unit", "oracle", "invariant", "regression", "snapshot",
        "integration", "release"
      )
    ))),
    c(10L, 11L, 5L, 2L, 1L, 17L, 7L)
  )
})

test_that("durable fixtures have an exact checksum and provenance inventory", {
  manifest <- test_architecture_read_manifest()
  expected_columns <- c(
    "path", "artifact_kind", "producer", "source_inputs",
    "deterministic_seed", "checksum_algorithm", "checksum",
    "review_step", "review_disposition"
  )
  expected_kinds <- c(
    "package_data", "extdata", "api_contract", "condition_contract",
    "coverage_evidence", "dispatch_contract", "regression_fixture",
    "smoothing_reference", "test_architecture", "snapshot"
  )

  expect_identical(names(manifest), expected_columns)
  expect_identical(nrow(manifest), 26L)
  expect_identical(anyDuplicated(manifest$path), 0L)
  expect_false(anyNA(manifest))
  expect_true(all(vapply(manifest, function(column) {
    all(nzchar(as.character(column)))
  }, logical(1))))
  expect_true(all(!grepl("\\\\", manifest$path)))
  expect_setequal(unique(manifest$artifact_kind), expected_kinds)
  expect_identical(unique(manifest$checksum_algorithm), "MD5")
  expect_true(all(grepl("^[0-9a-f]{32}$", manifest$checksum)))

  if (dir.exists(file.path(test_architecture_root, "inst"))) {
    expect_identical(sort(manifest$path), test_architecture_source_artifacts())
  }

  resolved <- vapply(
    manifest$path,
    test_architecture_resolve_artifact,
    character(1)
  )
  verifiable <- !is.na(resolved)
  expect_gte(sum(verifiable), 25L)
  expect_identical(
    unname(tools::md5sum(resolved[verifiable])),
    manifest$checksum[verifiable]
  )

  snapshot <- manifest[
    manifest$path == "tests/testthat/_snaps/output-schema.md",
    ,
    drop = FALSE
  ]
  expect_identical(nrow(snapshot), 1L)
  expect_identical(snapshot$review_disposition, "reviewed-no-change")
  expect_match(snapshot$review_step, "047", fixed = TRUE)
})

test_that("data and regression fixtures have a fail-closed focused manifest", {
  candidates <- test_architecture_package_candidates(
    "gates", "data-fixture-provenance.csv"
  )
  manifest_path <- candidates[file.exists(candidates)][[1L]]
  manifest <- utils::read.csv(
    manifest_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  expected_columns <- c(
    "path", "artifact_role", "producer", "source_inputs",
    "deterministic_seed", "format", "rows", "columns", "schema", "md5",
    "sha256", "regeneration_mode", "restricted_input_required",
    "repository_disposition", "source_package_disposition", "claim_boundary"
  )
  expected_roles <- c(
    "restricted_canonical_input", "public_package_data",
    "public_portable_data", "public_count_data", "public_provenance",
    "public_data_builder", "protected_numeric_baseline",
    "regression_review_projection", "protected_numeric_builder"
  )

  expect_identical(names(manifest), expected_columns)
  expect_identical(nrow(manifest), 13L)
  expect_identical(anyDuplicated(manifest$path), 0L)
  expect_false(anyNA(manifest))
  expect_true(all(vapply(manifest, function(column) {
    all(nzchar(as.character(column)))
  }, logical(1))))
  expect_setequal(unique(manifest$artifact_role), expected_roles)
  expect_identical(
    sum(manifest$artifact_role == "regression_review_projection"),
    5L
  )
  expect_true(all(grepl("^[0-9a-f]{32}$", manifest$md5)))
  expect_true(all(grepl("^[0-9a-f]{64}$", manifest$sha256)))
  expect_setequal(
    unique(manifest$source_package_disposition),
    c("included", "excluded")
  )
  expected_excluded <- c(
    "dev/data-ALprek-example/student_panel_2021-2026.rds",
    "inst/scripts/build-regression-baselines.R"
  )
  expect_identical(
    sort(manifest$path[
      manifest$source_package_disposition == "excluded"
    ]),
    sort(expected_excluded)
  )

  resolve <- function(path) {
    if (identical(
      path,
      "dev/data-ALprek-example/student_panel_2021-2026.rds"
    )) {
      return(NA_character_)
    }
    source_path <- file.path(test_architecture_root, path)
    if (file.exists(source_path)) {
      return(source_path)
    }
    if (startsWith(path, "inst/")) {
      relative <- sub("^inst/", "", path)
      installed_paths <- c(
        file.path(test_architecture_root, relative),
        if (nzchar(test_architecture_installed_root)) {
          file.path(test_architecture_installed_root, relative)
        } else {
          character()
        }
      )
      existing <- installed_paths[file.exists(installed_paths)]
      if (length(existing)) {
        return(existing[[1L]])
      }
    }
    NA_character_
  }
  resolved <- vapply(manifest$path, resolve, character(1))
  present <- !is.na(resolved)
  source_layout <- dir.exists(file.path(test_architecture_root, "inst"))
  included <- manifest$source_package_disposition == "included"
  package_data <- manifest$artifact_role == "public_package_data"
  if (source_layout) {
    expect_true(all(present[included]))
  } else {
    expect_true(all(present[included & !package_data]))
    expect_false(any(present[package_data]))

    data_env <- new.env(parent = emptyenv())
    utils::data("alprek_subset", package = "sitemix", envir = data_env)
    expect_true(exists("alprek_subset", envir = data_env, inherits = FALSE))
    installed_data <- get(
      "alprek_subset",
      envir = data_env,
      inherits = FALSE
    )
    data_row <- manifest[package_data, , drop = FALSE]
    expected_schema <- strsplit(
      data_row$schema,
      "|",
      fixed = TRUE
    )[[1L]]
    expect_identical(
      dim(installed_data),
      c(as.integer(data_row$rows), as.integer(data_row$columns))
    )
    expect_identical(names(installed_data), expected_schema)

    csv_row <- manifest$artifact_role == "public_portable_data"
    expect_identical(sum(csv_row), 1L)
    expect_true(present[csv_row])
    portable_data <- utils::read.csv(
      resolved[csv_row],
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    installed_plain <- as.data.frame(
      installed_data,
      stringsAsFactors = FALSE
    )
    attr(installed_plain, "build_info") <- NULL
    expect_identical(installed_plain, portable_data)
  }
  expect_identical(
    unname(tools::md5sum(resolved[present])),
    manifest$md5[present]
  )
  sha256sum <- get0(
    "sha256sum",
    envir = asNamespace("tools"),
    mode = "function",
    inherits = FALSE
  )
  if (is.function(sha256sum)) {
    expect_identical(
      unname(sha256sum(resolved[present])),
      manifest$sha256[present]
    )
  }

  protected_builder <- manifest[
    manifest$artifact_role == "protected_numeric_builder",
    ,
    drop = FALSE
  ]
  expect_identical(protected_builder$regeneration_mode, "never_execute_in_automated_audit")
  expect_identical(protected_builder$source_package_disposition, "excluded")
})

test_that("documentation QA has a nonempty fail-closed budget and stable CLI", {
  script_candidates <- test_architecture_package_candidates(
    "scripts", "audit-documentation-qa.R"
  )
  script_path <- script_candidates[file.exists(script_candidates)][[1L]]
  budget_candidates <- test_architecture_package_candidates(
    "gates", "documentation-qa-budget.csv"
  )
  budget_path <- budget_candidates[file.exists(budget_candidates)][[1L]]
  wordlist_candidates <- test_architecture_package_candidates("WORDLIST")
  wordlist_path <- wordlist_candidates[file.exists(wordlist_candidates)][[1L]]

  expect_no_error(parse(file = script_path))
  source <- paste(readLines(script_path, warn = FALSE), collapse = "\n")
  qa_env <- new.env(parent = baseenv())
  expect_no_error(sys.source(script_path, envir = qa_env))
  source_occurrences <- function(pattern) {
    as.integer(sum(gregexpr(pattern, source, fixed = TRUE)[[1L]] > 0L))
  }
  budget <- utils::read.csv(
    budget_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  expected_columns <- c("metric", "comparison", "threshold", "rationale")
  expected_metrics <- c(
    "wordlist_entries", "spelling_findings", "url_occurrences", "url_unique",
    "https_occurrences", "https_unique", "online_https_checked",
    "url_parent_files", "url_failures",
    "rd_files", "rd_syntax_diagnostics", "rd_content_diagnostics", "rd_xrefs",
    "rd_xref_files", "rd_xref_failures", "required_site_pages",
    "required_site_page_failures", "site_html_files", "site_local_links",
    "site_link_failures", "site_overlay_failures", "pkgdown_warnings",
    "pkgdown_errors", "doc_generated_drift", "doc_active_coupling",
    "vignette_source_failures", "vignette_render_failures",
    "vignette_link_failures", "canonical_vignettes", "protected_failures"
  )

  expect_identical(names(budget), expected_columns)
  expect_identical(nrow(budget), 30L)
  expect_identical(budget$metric, expected_metrics)
  expect_identical(anyDuplicated(budget$metric), 0L)
  expect_false(anyNA(budget))
  expect_true(all(nzchar(budget$rationale)))
  expect_setequal(
    unique(budget$comparison),
    c("at_least", "at_most", "exact")
  )
  expect_gt(
    budget$threshold[budget$metric == "https_unique"],
    0
  )
  expect_identical(
    budget$threshold[budget$metric == "required_site_pages"],
    35L
  )
  expect_identical(
    budget$threshold[budget$metric == "canonical_vignettes"],
    17L
  )

  wordlist <- readLines(wordlist_path, warn = FALSE)
  expect_gte(length(wordlist), 281L)
  expect_identical(anyDuplicated(wordlist), 0L)
  expect_false(any(wordlist %in% c("ebrecipe", "as_eb_input", "Joonho")))

  expect_match(source, '"gate",', fixed = TRUE)
  expect_match(source, '"negative-spelling"', fixed = TRUE)
  expect_match(source, '"negative-link"', fixed = TRUE)
  expect_match(source, '"negative-generated-drift"', fixed = TRUE)
  expect_match(source, 'c("offline", "online")', fixed = TRUE)
  expect_match(source, '"--no-build-vignettes", "--no-manual"', fixed = TRUE)
  expect_match(source, '"audit-documentation-drift.R"', fixed = TRUE)
  expect_match(source, '"audit-vignette-reproducibility.R"', fixed = TRUE)
  expect_match(source, "Documentation QA staging directory", fixed = TRUE)

  default_online <- paste0(
    'documentation_qa_arg_value(args, "url-mode", ',
    '"online")'
  )
  default_offline <- paste0(
    'documentation_qa_arg_value(args, "url-mode", ',
    '"offline")'
  )
  expect_identical(source_occurrences(default_online), 2L)
  expect_identical(source_occurrences(default_offline), 0L)
  expect_identical(source_occurrences("new_process = FALSE"), 1L)
  expect_identical(
    source_occurrences(
      'validation_boundary = "source_tar_plus_tracked_site_overlay"'
    ),
    1L
  )

  expected_overlay_paths <- c(
    "_pkgdown.yml",
    "pkgdown/favicon/apple-touch-icon.png",
    "pkgdown/favicon/favicon-96x96.png",
    "pkgdown/favicon/favicon.ico",
    "pkgdown/favicon/favicon.svg",
    "pkgdown/favicon/site.webmanifest",
    "pkgdown/favicon/web-app-manifest-192x192.png",
    "pkgdown/favicon/web-app-manifest-512x512.png"
  )
  expect_identical(
    qa_env$.documentation_qa_site_overlay_paths,
    expected_overlay_paths
  )
  expect_identical(
    source_occurrences(
      '"documentation-site-overlay-manifest.csv" = built$site_overlay'
    ),
    1L
  )

  expect_match(
    source,
    "authored_db <- documentation_qa_authored_url_rows(package_dir)",
    fixed = TRUE
  )
  expect_match(
    source,
    'file.path(package_dir, "_pkgdown.yml")',
    fixed = TRUE
  )
  expect_match(
    source,
    'file.path(package_dir, "vignettes", "references.bib")',
    fixed = TRUE
  )
  expect_match(source, 'bib_dois <- field("doi")', fixed = TRUE)
  expect_match(
    source,
    'paste0("https://doi.org/", bib_dois[nzchar(bib_dois)])',
    fixed = TRUE
  )
  expect_match(source, '"doi_resolver_no_follow"', fixed = TRUE)
  expect_match(
    source,
    "doi_results <- lapply(targets$url[doi_targets], documentation_qa_doi_result)",
    fixed = TRUE
  )
  expect_identical(
    source_occurrences(
      '"documentation-url-online-targets.csv" = url_online_targets'
    ),
    1L
  )
})

test_that("ordinary jobs have zero skips and negative profiles are exact", {
  audited <- test_architecture_env$sm_test_arch_validate_static(
    test_architecture_root
  )
  budget <- audited$budget

  expect_identical(nrow(budget), 13L)
  expect_identical(anyDuplicated(budget$profile), 0L)
  expect_setequal(
    budget$profile,
    c(
      "local-full", "architecture-static", "rcmd-ubuntu-release",
      "rcmd-ubuntu-devel", "rcmd-ubuntu-oldrel", "rcmd-macos-release",
      "rcmd-windows-release", "package-quality-release",
      "minimum-r-hard-only", "optional-no-gam", "optional-no-dplyr",
      "optional-none", "pkgdown-docs"
    )
  )

  ordinary <- budget$dependency_mode == "full"
  expect_true(all(budget$expected_skips[ordinary] == 0L))
  expect_true(all(budget$allowed_skip_ids[ordinary] == "none"))
  expect_true(all(budget$required_present[ordinary] == "dplyr|mgcv"))

  negative <- budget[budget$dependency_mode == "optional-negative", ]
  expect_setequal(
    negative$profile,
    c(
      "minimum-r-hard-only", "optional-no-gam",
      "optional-no-dplyr", "optional-none"
    )
  )
  expect_identical(
    negative$expected_skips[match(
      c(
        "minimum-r-hard-only", "optional-no-gam",
        "optional-no-dplyr", "optional-none"
      ),
      negative$profile
    )],
    c(2L, 1L, 1L, 2L)
  )
  expect_setequal(
    budget$profile[budget$gate_wired],
    c(
      "rcmd-ubuntu-release", "rcmd-ubuntu-devel", "rcmd-ubuntu-oldrel",
      "rcmd-macos-release", "rcmd-windows-release",
      "package-quality-release", "minimum-r-hard-only", "optional-no-gam"
    )
  )
})

test_that("the source tree has exactly two approved dependency skip sites", {
  audited <- test_architecture_env$sm_test_arch_validate_static(
    test_architecture_root
  )
  sites <- audited$skip_sites[order(audited$skip_sites$dependency), ]

  expect_identical(nrow(sites), 2L)
  expect_identical(nrow(audited$broad_skips), 0L)
  expect_identical(sites$dependency, c("dplyr", "mgcv"))
  expect_identical(
    sites$identity,
    c(
      paste0(
        "tests/testthat/test-output-subset-semantics.R::",
        "dplyr verbs share immediate subset and mutation semantics"
      ),
      paste0(
        "tests/testthat/test-smooth-variance.R::",
        "valid GAM fits satisfy rank, convergence, and prediction gates"
      )
    )
  )
})

test_that("the audit writes machine-readable static evidence", {
  out_dir <- tempfile("sitemix-test-budget-")
  on.exit(unlink(out_dir, recursive = TRUE, force = TRUE), add = TRUE)
  audited <- test_architecture_env$sm_test_arch_audit(
    root = test_architecture_root,
    profile = "architecture-static",
    out_dir = out_dir,
    static_only = TRUE
  )

  expect_identical(audited$status, "PASS")
  expected_files <- c(
    "static-issues.csv", "static-skip-sites.csv", "taxonomy-summary.csv",
    "test-results.csv", "test-skips.csv", "test-runs.csv", "test-timing.csv",
    "test-timing-by-file.csv", "test-timing-summary.csv", "slow-tests.csv",
    "test-summary.csv"
  )
  expect_true(all(file.exists(file.path(out_dir, expected_files))))
  summary <- utils::read.csv(
    file.path(out_dir, "test-summary.csv"),
    stringsAsFactors = FALSE
  )
  expect_identical(summary$status, "PASS")
  expect_identical(summary$taxonomy_files, 53L)
  expect_identical(summary$actual_test_files, 53L)
  expect_identical(summary$static_skip_sites, 2L)
  expect_identical(summary$static_broad_skips, 0L)
  expect_identical(summary$static_issue_count, 0L)
  expect_identical(summary$shuffle_scope, "none")
  expect_identical(summary$execution_engine, "testthat:::test_files")
  expect_identical(
    summary$load_package,
    test_architecture_env$sm_test_arch_load_mode(test_architecture_root)
  )
  expect_identical(summary$parallel_workers, 1L)
  expect_false(summary$timing_gate_applied)
  expect_true(is.na(summary$timing_budget_ok))
  expect_identical(summary$suite_budget_seconds, 180L)
  expect_identical(summary$max_block_budget_seconds, 15L)
  expect_false(summary$not_cran_scoped)
  expect_true(nzchar(summary$not_cran_prior))
  runs <- utils::read.csv(
    file.path(out_dir, "test-runs.csv"),
    stringsAsFactors = FALSE
  )
  expect_identical(nrow(runs), 0L)
  timing <- utils::read.csv(
    file.path(out_dir, "test-timing.csv"),
    stringsAsFactors = FALSE
  )
  expect_identical(nrow(timing), 0L)
  timing_by_file <- utils::read.csv(
    file.path(out_dir, "test-timing-by-file.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_identical(names(timing_by_file), c(
    "run_id", "file", "test_blocks", "expectations", "real_seconds",
    "baseline_real_seconds", "baseline_ratio", "slow_blocks",
    "max_block_seconds"
  ))
  expect_identical(nrow(timing_by_file), 0L)
  timing_summary <- utils::read.csv(
    file.path(out_dir, "test-timing-summary.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_identical(names(timing_summary), c(
    "profile", "run_id", "wall_seconds", "suite_budget_seconds",
    "observed_max_block_seconds", "max_block_seconds",
    "observed_slow_blocks", "slow_block_threshold_seconds",
    "max_slow_blocks", "timing_status"
  ))
  expect_identical(nrow(timing_summary), 0L)
  slow_tests <- utils::read.csv(
    file.path(out_dir, "slow-tests.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  expect_identical(names(slow_tests), c(
    "run_id", "file", "test", "real_seconds",
    "slow_block_threshold_seconds"
  ))
  expect_identical(nrow(slow_tests), 0L)
})

test_that("coverage and timing budgets are exact, complete, and wired", {
  audited <- test_architecture_env$sm_test_arch_validate_static(
    test_architecture_root
  )
  timing <- audited$timing_budget
  baseline <- audited$timing_baseline
  coverage <- test_coverage_env$sm_coverage_gate_read_budget(
    test_architecture_root
  )

  expect_length(audited$issues, 0L)
  expect_identical(nrow(timing), 13L)
  expect_identical(sort(timing$profile), sort(audited$budget$profile))
  expect_identical(timing$gate_wired, audited$budget$gate_wired)
  expect_identical(nrow(baseline), 53L)
  expect_identical(sort(baseline$path), sort(audited$taxonomy$path))
  expect_equal(sum(baseline$baseline_real_seconds), 36.717, tolerance = 1e-12)
  expect_identical(sum(baseline$baseline_test_blocks), 496L)
  expect_identical(sum(baseline$baseline_expectations), 6597L)
  expect_identical(
    unique(baseline$source_run),
    c("048-final-local-full", "054-step66-local-full", "053-step65-local-full")
  )

  ordinary_cap <- !timing$profile %in% c(
    "rcmd-ubuntu-oldrel", "rcmd-windows-release", "minimum-r-hard-only"
  )
  expect_true(all(timing$suite_budget_seconds[ordinary_cap] == 180L))
  expect_true(all(timing$max_block_seconds[ordinary_cap] == 15L))
  expect_true(all(timing$slow_block_threshold_seconds[ordinary_cap] == 5L))
  expect_true(all(timing$max_slow_blocks == 5L))
  expanded <- !ordinary_cap
  expect_true(all(timing$suite_budget_seconds[expanded] == 300L))
  expect_true(all(timing$max_block_seconds[expanded] == 30L))
  expect_true(all(timing$slow_block_threshold_seconds[expanded] == 10L))

  expect_identical(coverage$threshold_numerator, 9L)
  expect_identical(coverage$threshold_denominator, 10L)
  expect_identical(coverage$threshold_percent, 90L)
  expect_true(coverage$blocking)
  expect_true(coverage$gate_wired)
})

test_that("coverage gate uses the exact integer boundary", {
  pass <- test_coverage_env$sm_coverage_gate_evaluate(9043L, 10047L)
  fail <- test_coverage_env$sm_coverage_gate_evaluate(9042L, 10047L)

  expect_identical(pass$status, "PASS")
  expect_true(pass$passed)
  expect_identical(pass$required, 9043L)
  expect_identical(pass$shortfall, 0L)
  expect_identical(fail$status, "FAIL")
  expect_false(fail$passed)
  expect_identical(fail$required, 9043L)
  expect_identical(fail$shortfall, 1L)
  expect_gt(pass$percent, 90)
  expect_lt(fail$percent, 90)
  expect_error(
    test_coverage_env$sm_coverage_gate_evaluate(10048L, 10047L),
    "cannot exceed",
    fixed = TRUE
  )

  by_file <- test_coverage_env$sm_coverage_gate_by_file(data.frame(
    filename = c("R/a.R", "R/a.R", "R/b.R"),
    value = c(1L, 0L, 3L),
    stringsAsFactors = FALSE
  ))
  expect_identical(by_file$file, c("R/a.R", "R/b.R"))
  expect_identical(by_file$covered_executable_lines, c(1L, 1L))
  expect_identical(by_file$total_executable_lines, c(2L, 1L))
})

test_that("timing gate preserves blocks and aggregates files separately", {
  audited <- test_architecture_env$sm_test_arch_validate_static(
    test_architecture_root
  )
  policy <- test_architecture_env$sm_test_arch_timing_policy(
    audited,
    "local-full"
  )
  raw <- data.frame(
    file = c("test-a.R", "test-a.R", "test-b.R"),
    nb = c(1L, 2L, 3L),
    real = c(1, 6, 2),
    stringsAsFactors = FALSE
  )
  timing <- test_architecture_env$sm_test_arch_timing_by_file(
    raw = raw,
    run_id = 1L,
    static = audited,
    policy = policy
  )
  expect_identical(timing$file, c(
    "tests/testthat/test-a.R", "tests/testthat/test-b.R"
  ))
  expect_identical(timing$test_blocks, c(2L, 1L))
  expect_identical(timing$expectations, c(3L, 3L))
  expect_identical(timing$real_seconds, c(7, 2))
  expect_identical(timing$slow_blocks, c(1L, 0L))

  passing <- test_architecture_env$sm_test_arch_timing_evaluate(
    wall_seconds = 180,
    block_seconds = c(rep(5.01, 4L), 15),
    policy = policy
  )
  expect_true(passing$passed)
  expect_identical(passing$observed_slow_blocks, 5L)
  expect_identical(passing$observed_max_block_seconds, 15)
  expect_false(test_architecture_env$sm_test_arch_timing_evaluate(
    wall_seconds = 180.01,
    block_seconds = 1,
    policy = policy
  )$passed)
  expect_false(test_architecture_env$sm_test_arch_timing_evaluate(
    wall_seconds = 1,
    block_seconds = 15.01,
    policy = policy
  )$passed)
  expect_false(test_architecture_env$sm_test_arch_timing_evaluate(
    wall_seconds = 1,
    block_seconds = rep(5.01, 6L),
    policy = policy
  )$passed)

  raw_results <- data.frame(
    run_id = c(1L, 1L), file = c("test-a.R", "test-b.R"),
    test = c("fast", "slow"), real = c(5, 5.01),
    stringsAsFactors = FALSE
  )
  slow <- test_architecture_env$sm_test_arch_slow_tests(raw_results, 5)
  expect_identical(nrow(slow), 1L)
  expect_identical(slow$file, "tests/testthat/test-b.R")
  expect_identical(slow$test, "slow")
  expect_identical(slow$real_seconds, 5.01)
})

test_that("audit run controls and CLI parsing are bounded and explicit", {
  controls <- test_architecture_env$sm_test_arch_validate_run_controls(
    shuffle = TRUE,
    parallel = TRUE,
    seed = "20260713",
    repeats = "3"
  )
  expect_identical(controls$repeats, 3L)
  expect_identical(controls$seeds, 20260713:20260715)
  expect_true(controls$shuffle)
  expect_identical(controls$shuffle_scope, "file_order")
  expect_true(controls$parallel)
  expect_gte(controls$testthat_cpus, 1L)
  expect_true(controls$testthat_cpus_source %in% c(
    "option:Ncpus", "env:TESTTHAT_CPUS", "testthat_default"
  ))

  expect_error(
    test_architecture_env$sm_test_arch_validate_run_controls(shuffle = TRUE),
    "`seed` is required",
    fixed = TRUE
  )
  for (invalid in list(0L, 4L, 1.5, NA_integer_, "bad")) {
    expect_error(
      test_architecture_env$sm_test_arch_validate_run_controls(
        seed = 1L,
        repeats = invalid
      ),
      "`repeats`",
      fixed = TRUE
    )
  }
  expect_error(
    test_architecture_env$sm_test_arch_validate_run_controls(
      seed = .Machine$integer.max,
      repeats = 2L
    ),
    "leaves room",
    fixed = TRUE
  )
  expect_error(
    test_architecture_env$sm_test_arch_validate_run_controls(shuffle = 1, seed = 1L),
    "`shuffle`",
    fixed = TRUE
  )

  parsed <- test_architecture_env$sm_test_arch_parse_args(c(
    "--profile=local-full", "--out-dir=/tmp/sitemix-audit",
    "--shuffle", "--parallel", "--seed=20260713", "--repeats=2"
  ))
  expect_identical(parsed$profile, "local-full")
  expect_identical(parsed$out_dir, "/tmp/sitemix-audit")
  expect_identical(parsed$seed, "20260713")
  expect_identical(parsed$repeats, "2")
  expect_true(parsed$shuffle)
  expect_true(parsed$parallel)
  expect_error(
    test_architecture_env$sm_test_arch_parse_args("--unknown"),
    "Unknown arguments",
    fixed = TRUE
  )
  expect_error(
    test_architecture_env$sm_test_arch_parse_args(c("--seed=1", "--seed=2")),
    "Duplicate argument",
    fixed = TRUE
  )
  expect_error(
    test_architecture_env$sm_test_arch_parse_args("--seed="),
    "requires a value",
    fixed = TRUE
  )

  test_files <- test_architecture_env$sm_test_arch_test_files_api()
  expect_true(is.function(test_files))
  expect_true(all(
    c("test_paths", "parallel", "shuffle") %in% names(formals(test_files))
  ))
  find_scripts <- test_architecture_env$sm_test_arch_find_scripts_api()
  expect_true(is.function(find_scripts))
  expect_true(all(c("path", "full.names") %in% names(formals(find_scripts))))
  expect_identical(
    test_architecture_env$sm_test_arch_load_mode(test_architecture_root),
    if (dir.exists(file.path(test_architecture_root, "inst"))) {
      "source"
    } else {
      "installed"
    }
  )

  rng_state <- test_architecture_env$sm_test_arch_capture_rng()
  on.exit(
    test_architecture_env$sm_test_arch_restore_rng(rng_state),
    add = TRUE
  )
  paths <- sprintf("test-%02d.R", 1:10)
  set.seed(314159L)
  first <- test_architecture_env$sm_test_arch_order_test_paths(paths, TRUE)
  set.seed(314159L)
  second <- test_architecture_env$sm_test_arch_order_test_paths(paths, TRUE)
  expect_identical(first, second)
  expect_setequal(first, paths)
  expect_false(identical(first, paths))
  expect_identical(
    test_architecture_env$sm_test_arch_file_order_signature(first),
    test_architecture_env$sm_test_arch_file_order_signature(second)
  )
  expect_false(identical(
    test_architecture_env$sm_test_arch_file_order_signature(first),
    test_architecture_env$sm_test_arch_file_order_signature(paths)
  ))
  expect_identical(
    test_architecture_env$sm_test_arch_order_test_paths(paths, FALSE),
    paths
  )
})

test_that("audit result signatures ignore order and RNG restoration is exact", {
  rows <- data.frame(
    file = c("test-b.R", "test-a.R"),
    context = c("", ""),
    test = c("second", "first"),
    nb = c(2L, 1L),
    failed = c(0L, 0L),
    skipped = c(FALSE, FALSE),
    error = c(FALSE, FALSE),
    warning = c(0L, 0L),
    passed = c(2L, 1L),
    stringsAsFactors = FALSE
  )
  expect_identical(
    test_architecture_env$sm_test_arch_result_signature(rows),
    test_architecture_env$sm_test_arch_result_signature(rows[2:1, ])
  )

  outer_state <- test_architecture_env$sm_test_arch_capture_rng()
  on.exit(
    test_architecture_env$sm_test_arch_restore_rng(outer_state),
    add = TRUE
  )

  RNGkind("L'Ecuyer-CMRG", "Box-Muller", "Rejection")
  set.seed(90210L)
  seeded_state <- test_architecture_env$sm_test_arch_capture_rng()
  set.seed(7L, kind = "Mersenne-Twister", normal.kind = "Inversion")
  test_architecture_env$sm_test_arch_restore_rng(seeded_state)
  expect_identical(RNGkind(), seeded_state$kind)
  expect_identical(
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
    seeded_state$seed
  )

  RNGkind("L'Ecuyer-CMRG", "Box-Muller", "Rejection")
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  seedless_state <- test_architecture_env$sm_test_arch_capture_rng()
  set.seed(8L, kind = "Mersenne-Twister", normal.kind = "Inversion")
  test_architecture_env$sm_test_arch_restore_rng(seedless_state)
  expect_identical(RNGkind(), seedless_state$kind)
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))

  not_cran_outer <- test_architecture_env$sm_test_arch_capture_env("NOT_CRAN")
  on.exit(
    test_architecture_env$sm_test_arch_restore_env(not_cran_outer),
    add = TRUE
  )
  Sys.unsetenv("NOT_CRAN")
  absent <- test_architecture_env$sm_test_arch_capture_env("NOT_CRAN")
  Sys.setenv(NOT_CRAN = "true")
  test_architecture_env$sm_test_arch_restore_env(absent)
  expect_false(nzchar(Sys.getenv("NOT_CRAN")))
  Sys.setenv(NOT_CRAN = "original-value")
  present <- test_architecture_env$sm_test_arch_capture_env("NOT_CRAN")
  Sys.setenv(NOT_CRAN = "true")
  test_architecture_env$sm_test_arch_restore_env(present)
  expect_identical(Sys.getenv("NOT_CRAN"), "original-value")
})

test_that("CI contracts are exact and portable across package boundaries", {
  job_path <- test_architecture_resolve_inst("gates", "ci-job-contract.csv")
  pin_path <- test_architecture_resolve_inst("gates", "ci-action-pins.csv")
  jobs <- utils::read.csv(
    job_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  pins <- utils::read.csv(
    pin_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )

  expect_identical(names(jobs), c(
    "workflow", "job_id", "role", "runner_policy", "r_policy",
    "blocking_policy", "timeout_minutes", "session_required",
    "artifact_required", "artifact_retention_days", "required_markers"
  ))
  expect_identical(nrow(jobs), 8L)
  expect_identical(anyDuplicated(jobs[c("workflow", "job_id")]), 0L)
  expect_false(anyNA(jobs))
  expect_true(all(vapply(jobs, function(value) {
    all(nzchar(as.character(value)))
  }, logical(1))))

  expected_keys <- paste(
    c(rep(".github/workflows/R-CMD-check.yaml", 7L),
      ".github/workflows/pkgdown.yaml"),
    c(
      "R-CMD-check", "minimum-R", "dependency-floor", "optional-negative",
      "package-quality", "coverage", "performance", "documentation"
    ),
    sep = "::"
  )
  expect_identical(paste(jobs$workflow, jobs$job_id, sep = "::"), expected_keys)
  expect_identical(jobs$role, c(
    "check-matrix", "minimum-r", "dependency-floor",
    "optional-dependency-negative", "full-suite-lint-schema-export",
    "line-coverage", "performance-structure-and-advisory-numerics",
    "documentation-and-pkgdown-build"
  ))
  expect_identical(jobs$runner_policy, c(
    "matrix.config.os", "ubuntu-22.04", rep("ubuntu-latest", 6L)
  ))
  expect_identical(jobs$r_policy, c(
    "release-devel-oldrel", "4.1.3", "4.5.1", rep("release", 5L)
  ))
  expect_identical(jobs$blocking_policy, c(
    "devel-step-advisory", rep("blocking", 7L)
  ))
  expect_identical(
    as.integer(jobs$timeout_minutes),
    c(60L, 45L, 30L, 30L, 30L, 45L, 30L, 45L)
  )
  expect_true(all(as.logical(jobs$session_required)))
  expect_true(all(as.logical(jobs$artifact_required)))
  expect_identical(as.integer(jobs$artifact_retention_days), rep(14L, 8L))

  expect_identical(names(pins), c(
    "action", "sha", "release", "allowed_subactions", "approved_utc"
  ))
  expect_identical(nrow(pins), 3L)
  expect_identical(anyDuplicated(pins$action), 0L)
  expect_identical(pins$action, c(
    "actions/checkout", "actions/upload-artifact", "r-lib/actions"
  ))
  expect_identical(pins$sha, c(
    "9c091bb21b7c1c1d1991bb908d89e4e9dddfe3e0",
    "043fb46d1a93c77aae656e7c1c64a875d1fc6a0a",
    "d3c5be51b12e724e68f33216ca3c148b66d5f0b6"
  ))
  expect_true(all(grepl("^[0-9a-f]{40}$", pins$sha)))
  expect_identical(pins$release, c("v7.0.0", "v7.0.1", "v2.12.1"))
  expect_identical(pins$allowed_subactions, c(
    "checkout", "upload-artifact",
    "setup-pandoc|setup-r|setup-r-dependencies|setup-tinytex"
  ))
  expect_identical(pins$approved_utc, rep("2026-07-13", 3L))
})

test_that("CI audit scripts parse without loading optional YAML support", {
  ci_script <- test_architecture_resolve_inst(
    "scripts", "audit-ci-semantics.R"
  )
  floor_script <- test_architecture_resolve_inst(
    "scripts", "audit-dependency-floor.R"
  )
  expect_no_error(parse(file = ci_script))
  expect_no_error(parse(file = floor_script))

  ci_source <- paste(readLines(ci_script, warn = FALSE), collapse = "\n")
  floor_source <- paste(readLines(floor_script, warn = FALSE), collapse = "\n")
  expect_true(all(vapply(c(
    "negative-threshold", "negative-deploy", "negative-floor",
    "negative-matrix", "negative-yaml", "negative-shell", "negative-r",
    "self-test"
  ), grepl, logical(1), x = ci_source, fixed = TRUE)))
  expect_true(all(vapply(c(
    "ci-job-contract.csv", "ci-action-pins.csv", "dependency-floor.csv",
    "audit-dependency-floor.R", "performance-contract-self-test.R",
    "performance-smoke.R", ".github/dependabot.yml"
  ), grepl, logical(1), x = ci_source, fixed = TRUE)))
  expect_match(
    ci_source,
    "yaml::yaml.load(text, eval.expr = FALSE)",
    fixed = TRUE
  )
  expect_match(
    ci_source,
    '"ebrecipe|as_eb_input|eb_handoff" = "consumer_coupling"',
    fixed = TRUE
  )

  expect_true(all(vapply(c(
    ".dep_floor_columns", "dep_floor_expected_policy",
    'c("gate", "negative-version")', 'find.package("sitemix"',
    'getExportedValue("sitemix", "sm_estimate")',
    'getExportedValue("sitemix", "sm_diagnose")',
    'getExportedValue("Matrix", "nearPD")',
    "dep_floor_matrix_smoke",
    "dep_floor_source_manifest", "dep_floor_protected",
    "inst/scripts/build-regression-baselines.R"
  ), grepl, logical(1), x = floor_source, fixed = TRUE)))
})

test_that("dependency floors and Matrix policy are frozen exactly", {
  floor_path <- test_architecture_resolve_inst("gates", "dependency-floor.csv")
  floor <- utils::read.csv(
    floor_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  expect_identical(names(floor), c(
    "package", "description_field", "description_constraint",
    "installed_version_policy", "expected_installed_version",
    "runtime_profile", "rationale"
  ))
  expect_identical(nrow(floor), 5L)
  expect_identical(anyDuplicated(floor$package), 0L)
  expect_false(anyNA(floor))
  expect_true(all(vapply(floor, function(value) {
    all(nzchar(as.character(value)))
  }, logical(1))))

  expected <- data.frame(
    package = c("rlang", "tibble", "vctrs", "cli", "Matrix"),
    description_field = rep("Imports", 5L),
    description_constraint = c(
      ">= 1.1.0", ">= 3.2.0", ">= 0.6.0", ">= 3.6.0", "unversioned"
    ),
    installed_version_policy = c(rep("exact", 4L), "compatible"),
    expected_installed_version = c(
      "1.1.0", "3.2.0", "0.6.0", "3.6.0", "r-4.5.1-compatible"
    ),
    runtime_profile = rep("r-4.5.1", 5L),
    stringsAsFactors = FALSE
  )
  ordered <- floor[match(expected$package, floor$package), names(expected)]
  rownames(ordered) <- NULL
  expect_identical(ordered, expected)

  description_candidates <- test_architecture_package_candidates("DESCRIPTION")
  description <- read.dcf(
    description_candidates[file.exists(description_candidates)][[1L]]
  )
  imports <- trimws(strsplit(
    gsub("[[:space:]]+", " ", description[[1L, "Imports"]]),
    ",",
    fixed = TRUE
  )[[1L]])
  expect_setequal(imports, c(
    "rlang (>= 1.1.0)", "tibble (>= 3.2.0)", "vctrs (>= 0.6.0)",
    "cli (>= 3.6.0)", "Matrix"
  ))

  workflow <- file.path(
    test_architecture_root,
    ".github", "workflows", "R-CMD-check.yaml"
  )
  if (file.exists(workflow)) {
    source <- paste(readLines(workflow, warn = FALSE), collapse = "\n")
    expect_true(all(vapply(c(
      '"cli@3.6.0"', '"rlang@1.1.0"', '"vctrs@0.6.0"',
      '"tibble@3.2.0"', "direct dependency floors (R 4.5.1)",
      "r-version: '4.5.1'", "audit-dependency-floor.R --self-test",
      "--profile=gate"
    ), grepl, logical(1), x = source, fixed = TRUE)))
    expect_false(grepl("Matrix@", source, fixed = TRUE))
  }
})

test_that("final workflow topology and job evidence match the contract", {
  workflow_paths <- c(
    file.path(
      test_architecture_root,
      ".github", "workflows", "R-CMD-check.yaml"
    ),
    file.path(
      test_architecture_root,
      ".github", "workflows", "pkgdown.yaml"
    )
  )
  present <- file.exists(workflow_paths)
  expect_true(all(present) || !any(present))
  if (all(present)) {
    workflow_lines <- lapply(workflow_paths, readLines, warn = FALSE)
    names(workflow_lines) <- c(
      ".github/workflows/R-CMD-check.yaml",
      ".github/workflows/pkgdown.yaml"
    )
    blocks <- lapply(workflow_lines, test_architecture_job_blocks)
    expect_identical(names(blocks[[1L]]), c(
      "R-CMD-check", "minimum-R", "dependency-floor", "optional-negative",
      "package-quality", "coverage", "performance"
    ))
    expect_identical(names(blocks[[2L]]), "documentation")
    expect_identical(sum(lengths(blocks)), 8L)

    job_path <- test_architecture_resolve_inst("gates", "ci-job-contract.csv")
    jobs <- utils::read.csv(
      job_path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = character()
    )
    for (row in seq_len(nrow(jobs))) {
      block <- blocks[[jobs$workflow[[row]]]][[jobs$job_id[[row]]]]
      block_source <- paste(block, collapse = "\n")
      timeout <- grep("^    timeout-minutes: [0-9]+$", block, value = TRUE)
      expect_identical(length(timeout), 1L)
      expect_identical(
        as.integer(sub("^    timeout-minutes: ", "", timeout)),
        as.integer(jobs$timeout_minutes[[row]])
      )
      expect_identical(
        sum(block == "      - name: Session snapshot"),
        as.integer(as.logical(jobs$session_required[[row]]))
      )
      expect_identical(
        sum(grepl("actions/upload-artifact@", block, fixed = TRUE)),
        as.integer(as.logical(jobs$artifact_required[[row]]))
      )
      expect_match(
        block_source,
        paste0("retention-days: ", jobs$artifact_retention_days[[row]]),
        fixed = TRUE
      )
      markers <- strsplit(
        jobs$required_markers[[row]],
        "|",
        fixed = TRUE
      )[[1L]]
      expect_true(all(vapply(
        markers,
        grepl,
        logical(1),
        x = block_source,
        fixed = TRUE
      )))
    }

    all_lines <- unlist(workflow_lines, use.names = FALSE)
    expect_identical(
      sum(grepl("cancel-in-progress: true", all_lines, fixed = TRUE)),
      2L
    )
    expect_identical(
      sum(grepl("github.workflow", all_lines, fixed = TRUE)),
      2L
    )
    expect_identical(sum(trimws(all_lines) == "contents: read"), 2L)
    expect_false(any(grepl(
      "contents:[[:space:]]*write|permissions:[[:space:]]*write",
      all_lines,
      ignore.case = TRUE,
      perl = TRUE
    )))
    expect_identical(
      sum(all_lines == "      - name: Session snapshot"),
      8L
    )
    expect_identical(sum(trimws(all_lines) == "if-no-files-found: error"), 8L)
    expect_identical(sum(trimws(all_lines) == "retention-days: 14"), 8L)

    quality <- paste(blocks[[1L]][["package-quality"]], collapse = "\n")
    coverage <- paste(blocks[[1L]][["coverage"]], collapse = "\n")
    performance <- paste(blocks[[1L]][["performance"]], collapse = "\n")
    expect_true(all(vapply(c(
      "audit-lint-gate.R", "audit-ci-semantics.R --self-test",
      "--profile=gate", "--profile=package-quality-release"
    ), grepl, logical(1), x = gsub("[[:space:]]+", " ", quality),
    fixed = TRUE)))
    expect_false(grepl("audit-coverage-gate.R", quality, fixed = TRUE))
    expect_false(grepl("performance-smoke.R", quality, fixed = TRUE))
    expect_match(coverage, "audit-coverage-gate.R", fixed = TRUE)
    expect_false(grepl("audit-test-budget.R", coverage, fixed = TRUE))
    expect_match(performance, "performance-contract-self-test.R", fixed = TRUE)
    expect_match(performance, "performance-smoke.R", fixed = TRUE)
    expect_match(performance, "--profile=ci-smoke", fixed = TRUE)
    expect_false(grepl("audit-coverage-gate.R", performance, fixed = TRUE))

    quality_lines <- blocks[[1L]][["package-quality"]]
    positions <- c(
      dependency = which(trimws(quality_lines) == "any::lintr"),
      lint = grep("audit-lint-gate.R", quality_lines, fixed = TRUE),
      semantics_self_test = grep(
        "audit-ci-semantics.R --self-test", quality_lines, fixed = TRUE
      ),
      semantics_gate = grep("--out-dir=ci-artifacts/ci-semantics", quality_lines,
        fixed = TRUE
      ),
      package_budget = grep(
        "--profile=package-quality-release", quality_lines, fixed = TRUE
      )
    )
    expect_identical(length(positions), 5L)
    expect_true(all(diff(positions) > 0L))

    workflow_text <- paste(workflow_lines[[1L]], collapse = "\n")
    budget <- test_architecture_env$sm_test_arch_validate_static(
      test_architecture_root
    )$budget
    wired <- budget$profile[budget$gate_wired]
    expect_identical(length(wired), 8L)
    expect_true(all(vapply(
      wired,
      grepl,
      logical(1),
      x = workflow_text,
      fixed = TRUE
    )))
    expect_false(grepl("optional-no-dplyr", workflow_text, fixed = TRUE))
    expect_false(grepl("optional-none", workflow_text, fixed = TRUE))
    expect_identical(sum(grepl(
      'sudo -E "$(command -v Rscript)"',
      workflow_lines[[1L]],
      fixed = TRUE
    )), 2L)
  }
})

test_that("every workflow action is approved and pinned to a full SHA", {
  workflow_paths <- c(
    file.path(
      test_architecture_root,
      ".github", "workflows", "R-CMD-check.yaml"
    ),
    file.path(
      test_architecture_root,
      ".github", "workflows", "pkgdown.yaml"
    )
  )
  present <- file.exists(workflow_paths)
  expect_true(all(present) || !any(present))
  if (all(present)) {
    lines <- unlist(lapply(workflow_paths, readLines, warn = FALSE),
      use.names = FALSE
    )
    uses <- test_architecture_action_uses(lines)
    pins <- utils::read.csv(
      test_architecture_resolve_inst("gates", "ci-action-pins.csv"),
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = character()
    )
    expect_identical(nrow(uses), 35L)
    expect_true(all(grepl("^[^@[:space:]]+@[0-9a-f]{40}$", uses$reference)))

    pin_index <- match(uses$action, pins$action)
    expect_false(anyNA(pin_index))
    expect_identical(uses$sha, pins$sha[pin_index])
    allowed <- strsplit(pins$allowed_subactions[pin_index], "|", fixed = TRUE)
    expect_true(all(mapply(
      function(subaction, choices) subaction %in% choices,
      uses$subaction,
      allowed,
      USE.NAMES = FALSE
    )))
    expect_true(all(mapply(
      function(raw, release) grepl(paste0("# ", release), raw, fixed = TRUE),
      uses$raw,
      pins$release[pin_index],
      USE.NAMES = FALSE
    )))

    expect_identical(sum(uses$action == "actions/checkout"), 8L)
    expect_identical(sum(uses$action == "actions/upload-artifact"), 8L)
    expect_identical(sum(uses$action == "r-lib/actions"), 19L)
    expect_identical(sum(uses$subaction == "setup-r"), 8L)
    expect_identical(sum(uses$subaction == "setup-r-dependencies"), 8L)
    expect_identical(sum(uses$subaction == "setup-pandoc"), 2L)
    expect_identical(sum(uses$subaction == "setup-tinytex"), 1L)
    expect_identical(sum(trimws(lines) == "persist-credentials: false"), 8L)
    expect_identical(sum(trimws(lines) == "install-quarto: false"), 8L)
    expect_false(any(trimws(lines) == "install-quarto: true"))
    expect_identical(sum(trimws(lines) == "pandoc-version: '3.8.3'"), 2L)
  }
})

test_that("R CMD check is strict with only step-level R-devel advice", {
  workflow <- file.path(
    test_architecture_root,
    ".github", "workflows", "R-CMD-check.yaml"
  )
  expect_true(
    file.exists(workflow) ||
      !dir.exists(file.path(test_architecture_root, ".github", "workflows"))
  )
  if (file.exists(workflow)) {
    lines <- readLines(workflow, warn = FALSE)
    matrix <- grep("profile: rcmd-", lines, value = TRUE, fixed = TRUE)
    expect_identical(length(matrix), 5L)
    expect_identical(trimws(matrix), c(
      "- {os: ubuntu-latest, r: release, profile: rcmd-ubuntu-release, advisory: false, manual: true}",
      "- {os: ubuntu-latest, r: devel, profile: rcmd-ubuntu-devel, advisory: true, manual: false}",
      "- {os: ubuntu-latest, r: oldrel-1, profile: rcmd-ubuntu-oldrel, advisory: false, manual: false}",
      "- {os: macos-latest, r: release, profile: rcmd-macos-release, advisory: false, manual: false}",
      "- {os: windows-latest, r: release, profile: rcmd-windows-release, advisory: false, manual: false}"
    ))
    expect_identical(sum(grepl("manual: true", matrix, fixed = TRUE)), 1L)
    expect_identical(sum(grepl("advisory: true", matrix, fixed = TRUE)), 1L)
    expect_match(
      matrix[grepl("manual: true", matrix, fixed = TRUE)],
      "ubuntu-latest, r: release",
      fixed = TRUE
    )
    expect_match(
      matrix[grepl("advisory: true", matrix, fixed = TRUE)],
      "ubuntu-latest, r: devel",
      fixed = TRUE
    )

    expect_identical(
      sum(grepl("r-lib/actions/setup-tinytex@", lines, fixed = TRUE)),
      1L
    )
    expect_identical(
      sum(trimws(lines) == "if: matrix.config.manual == true"),
      1L
    )
    continue <- grep("continue-on-error:", lines, value = TRUE, fixed = TRUE)
    expect_identical(length(continue), 2L)
    expect_true(all(startsWith(continue, "        continue-on-error:")))
    expect_true(all(trimws(continue) == paste(
      "continue-on-error:",
      "${{ matrix.config.advisory == true }}"
    )))
    expect_false(any(grepl("^    continue-on-error:", lines)))
    expect_false(any(trimws(continue) == "continue-on-error: true"))

    error_on <- grep("error_on =", lines, value = TRUE, fixed = TRUE)
    expect_identical(trimws(error_on), 'error_on = "note",')
    expect_identical(
      sum(grepl("Record advisory R-devel outcomes", lines, fixed = TRUE)),
      1L
    )
  }
})

test_that("documentation CI is unified online build-only automation", {
  workflow_paths <- c(
    file.path(
      test_architecture_root,
      ".github", "workflows", "R-CMD-check.yaml"
    ),
    file.path(
      test_architecture_root,
      ".github", "workflows", "pkgdown.yaml"
    )
  )
  present <- file.exists(workflow_paths)
  expect_true(all(present) || !any(present))
  if (all(present)) {
    check_source <- paste(readLines(workflow_paths[[1L]], warn = FALSE),
      collapse = "\n"
    )
    docs_lines <- readLines(workflow_paths[[2L]], warn = FALSE)
    docs_source <- paste(docs_lines, collapse = "\n")
    docs_normalized <- gsub("[[:space:]]+", " ", docs_source)
    expect_match(
      docs_normalized,
      paste(
        "Rscript --vanilla inst/scripts/audit-documentation-qa.R",
        "--profile=gate --url-mode=online",
        "--out-dir=ci-artifacts/documentation"
      ),
      fixed = TRUE
    )
    dependencies <- trimws(grep(
      "^[[:space:]]+any::",
      docs_lines,
      value = TRUE
    ))
    expect_setequal(dependencies, c(
      "any::curl", "any::knitr", "any::mgcv", "any::pkgdown",
      "any::pkgload", "any::rmarkdown", "any::roxygen2@8.0.0",
      "any::spelling", "any::urlchecker", "any::xml2", "any::yaml"
    ))
    expect_true(all(vapply(
      c("push:", "pull_request:", "workflow_dispatch:"),
      grepl,
      logical(1),
      x = docs_source,
      fixed = TRUE
    )))
    expect_false(grepl(
      paste(
        "contents:[[:space:]]*write|gh-pages|github-pages-deploy|JamesIves|",
        "build_site_github_pages|pkgdown::build_site|pkgdown::deploy"
      ),
      docs_source,
      ignore.case = TRUE,
      perl = TRUE
    ))

    active <- paste(check_source, docs_source, sep = "\n")
    expect_false(grepl(
      "ebrecipe|as_eb_input|eb_handoff",
      active,
      ignore.case = TRUE,
      perl = TRUE
    ))
    contracts <- paste(
      readLines(
        test_architecture_resolve_inst("gates", "ci-job-contract.csv"),
        warn = FALSE
      ),
      collapse = "\n"
    )
    expect_false(grepl(
      "ebrecipe|as_eb_input|eb_handoff",
      contracts,
      ignore.case = TRUE,
      perl = TRUE
    ))
  }
})

test_that("Dependabot and schema-export coverage stay explicitly wired", {
  dependabot <- file.path(test_architecture_root, ".github", "dependabot.yml")
  if (file.exists(dependabot)) {
    lines <- readLines(dependabot, warn = FALSE)
    expect_identical(sum(trimws(lines) == "version: 2"), 1L)
    expect_identical(
      sum(trimws(lines) == "- package-ecosystem: github-actions"),
      1L
    )
    expect_identical(sum(trimws(lines) == 'directory: "/"'), 1L)
    expect_identical(sum(trimws(lines) == "interval: weekly"), 1L)
    expect_identical(sum(trimws(lines) == "day: monday"), 1L)
    expect_identical(sum(trimws(lines) == 'time: "09:00"'), 1L)
    expect_identical(
      sum(trimws(lines) == "open-pull-requests-limit: 5"),
      1L
    )
    expect_false(any(grepl(
      "interval:[[:space:]]*(daily|monthly)",
      lines,
      perl = TRUE
    )))
  }

  required_tests <- c(
    "test-output-schema.R", "test-fpc-vcov-schema.R",
    "test-generic-summary-export-contract.R", "test-vcov-api-surface.R"
  )
  expect_true(all(file.exists(file.path(
    test_architecture_root,
    "tests", "testthat", required_tests
  ))))
})

test_that("optional isolation is CI-only, fresh-process, and restore-safe", {
  candidates <- test_architecture_package_candidates(
    "scripts", "isolate-optional-dependencies.R"
  )
  script <- candidates[file.exists(candidates)][[1L]]
  lines <- readLines(script, warn = FALSE)
  source <- paste(lines, collapse = "\n")

  expect_match(source, 'Sys.getenv("CI")', fixed = TRUE)
  expect_match(source, 'Sys.getenv("GITHUB_ACTIONS")', fixed = TRUE)
  expect_match(source, 'args == "--dry-run"', fixed = TRUE)
  expect_match(source, "file.access(dirname(installations$path)", fixed = TRUE)
  expect_match(source, "on.exit(restore(), add = TRUE)", fixed = TRUE)
  expect_match(source, "system2(", fixed = TRUE)
  expect_match(source, "loadedNamespaces()", fixed = TRUE)
  expect_match(source, "optional-namespace-proof.csv", fixed = TRUE)
  expect_match(source, "isolation-summary.csv", fixed = TRUE)
  expect_match(source, "package = character(), source = character()", fixed = TRUE)
  expect_match(source, "isolated = logical(), restored = logical(), ok = logical()", fixed = TRUE)
  expect_match(source, "pid = integer(), profile = character()", fixed = TRUE)
  expect_match(source, "Optional package libraries are not writable", fixed = TRUE)
})
