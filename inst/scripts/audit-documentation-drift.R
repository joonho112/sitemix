#!/usr/bin/env Rscript

.doc_protected_sha256 <- c(
  "inst/scripts/build-regression-baselines.R" =
    "29e8909b541af31ff47042591b462bd745c8b172bf6574a4ee6a90ced050acb1",
  "tests/testthat/_data/regression/regression-baselines.rds" =
    "be0527f9357aa7cbb0c014a9b0ce8e60e15252b5270fad5bb99113106f9e075b",
  "tests/testthat/_snaps/output-schema.md" =
    "ed838cde596fba9618627826af12e5e5b286fa633076474bc9e47f6824885c8e"
)

doc_arg_value <- function(args, name, default = NULL) {
  prefix <- paste0("--", name, "=")
  hit <- grep(paste0("^", prefix), args, value = TRUE)
  if (length(hit)) sub(prefix, "", hit[[1L]], fixed = TRUE) else default
}

doc_validate_args <- function(args) {
  valid <- startsWith(args, "--out-dir=")
  if (any(!valid)) {
    stop("Unknown or malformed argument: ", args[which(!valid)[[1L]]], call. = FALSE)
  }
  names <- sub("^--([^=]+).*$", "\\1", args)
  if (anyDuplicated(names)) {
    stop("Duplicate argument: --", names[duplicated(names)][[1L]], call. = FALSE)
  }
  invisible(TRUE)
}

doc_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  script <- if (length(file_arg)) {
    sub("^--file=", "", file_arg[[1L]])
  } else {
    file.path("inst", "scripts", "audit-documentation-drift.R")
  }
  normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE)
}

doc_output_target <- function(path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`--out-dir` must be one nonempty path.", call. = FALSE)
  }
  expanded <- path.expand(path)
  if (!startsWith(expanded, .Platform$file.sep)) {
    expanded <- file.path(getwd(), expanded)
  }
  dir.create(dirname(expanded), recursive = TRUE, showWarnings = FALSE)
  target <- file.path(
    normalizePath(dirname(expanded), mustWork = TRUE),
    basename(expanded)
  )
  if (file.exists(target) || dir.exists(target)) {
    stop("Output directory must be fresh: ", target, call. = FALSE)
  }
  target
}

doc_sha256 <- function(paths) {
  paths <- as.character(paths)
  if (any(!file.exists(paths))) {
    stop("Cannot hash a missing documentation file.", call. = FALSE)
  }
  unname(tools::sha256sum(paths))
}

doc_require_packages <- function(root) {
  packages <- c("knitr", "pkgload", "rmarkdown", "roxygen2")
  versions <- vapply(packages, function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Documentation drift gate requires package `", package, "`.", call. = FALSE)
    }
    as.character(utils::packageVersion(package))
  }, character(1))
  description <- read.dcf(file.path(root, "DESCRIPTION"))
  configured <- unname(description[1L, "Config/roxygen2/version"])
  if (!identical(versions[["roxygen2"]], configured)) {
    stop(
      "Installed roxygen2 ", versions[["roxygen2"]],
      " disagrees with configured version ", configured, ".",
      call. = FALSE
    )
  }
  suggests <- unname(description[1L, "Suggests"])
  if (!grepl("(^|[,[:space:]])pkgload([[:space:](,]|$)", suggests)) {
    stop("pkgload must be declared in Suggests for README regeneration.", call. = FALSE)
  }
  data.frame(
    tool = c(
      packages,
      "R", "Pandoc", "configured_roxygen2"
    ),
    version = c(
      unname(versions),
      as.character(getRversion()),
      as.character(rmarkdown::pandoc_version()),
      configured
    ),
    stringsAsFactors = FALSE
  )
}

doc_build_source <- function(root, work_dir, build_log) {
  build_dir <- file.path(work_dir, "build")
  extract_dir <- file.path(work_dir, "extract")
  dir.create(build_dir, recursive = TRUE)
  dir.create(extract_dir, recursive = TRUE)
  old <- setwd(build_dir)
  on.exit(setwd(old), add = TRUE)
  status <- system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD", "build", "--no-build-vignettes", "--no-manual",
      shQuote(root)
    ),
    stdout = build_log,
    stderr = build_log
  )
  if (!identical(status, 0L)) {
    stop("R CMD build failed; see build log.", call. = FALSE)
  }
  tarballs <- list.files(build_dir, pattern = "[.]tar[.]gz$", full.names = TRUE)
  if (length(tarballs) != 1L) {
    stop("R CMD build did not produce exactly one source tarball.", call. = FALSE)
  }
  utils::untar(tarballs[[1L]], exdir = extract_dir)
  packages <- list.dirs(extract_dir, recursive = FALSE, full.names = TRUE)
  if (length(packages) != 1L || !file.exists(file.path(packages, "DESCRIPTION"))) {
    stop("Could not identify the extracted source package.", call. = FALSE)
  }
  package_dir <- normalizePath(packages[[1L]], mustWork = TRUE)
  for (file in c("README.Rmd", "_pkgdown.yml")) {
    source <- file.path(root, file)
    if (file.exists(source) && !file.copy(source, file.path(package_dir, file))) {
      stop("Could not copy excluded source file `", file, "`.", call. = FALSE)
    }
  }
  list(package_dir = package_dir, tarball = tarballs[[1L]])
}

doc_vignette_manifest <- function(root, package_dir) {
  canonical <- sort(list.files(
    file.path(package_dir, "vignettes"),
    pattern = "^[am][0-9].*[.]Rmd$",
    full.names = FALSE
  ))
  ids <- sub("-.*$", "", canonical)
  expected_ids <- c(paste0("a", 1:9), paste0("m", 1:8))
  canonical_ok <- length(canonical) == 17L &&
    identical(sort(ids), sort(expected_ids)) && !anyDuplicated(ids)
  support <- c("apa.csl", "references.bib")
  support_ok <- all(file.exists(file.path(package_dir, "vignettes", support)))
  protected <- names(.doc_protected_sha256)[startsWith(
    names(.doc_protected_sha256),
    "vignettes/"
  )]
  protected_excluded <- all(!file.exists(file.path(package_dir, protected)))
  data.frame(
    kind = c(
      rep("canonical_vignette", length(canonical)),
      rep("support", length(support)),
      rep("protected_legacy", length(protected))
    ),
    path = c(
      file.path("vignettes", canonical),
      file.path("vignettes", support),
      protected
    ),
    source_exists = c(
      rep(TRUE, length(canonical) + length(support)),
      file.exists(file.path(root, protected))
    ),
    source_tar_included = c(
      rep(TRUE, length(canonical) + length(support)),
      rep(FALSE, length(protected))
    ),
    contract_ok = c(
      rep(canonical_ok, length(canonical)),
      rep(support_ok, length(support)),
      rep(protected_excluded, length(protected))
    ),
    stringsAsFactors = FALSE
  )
}

doc_read_text_files <- function(paths, root, scope) {
  rows <- lapply(paths[file.exists(paths)], function(path) {
    lines <- readLines(path, warn = FALSE)
    matched <- grepl("ebrecipe|as_eb_input|eb_handoff", lines, ignore.case = TRUE)
    if (!any(matched)) {
      return(NULL)
    }
    data.frame(
      scope = scope,
      path = substring(normalizePath(path), nchar(normalizePath(root)) + 2L),
      line = which(matched),
      text = trimws(lines[matched]),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) {
    return(data.frame(
      scope = character(), path = character(), line = integer(), text = character(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

doc_active_coupling <- function(root, package_dir) {
  canonical_vignettes <- list.files(
    file.path(package_dir, "vignettes"),
    pattern = "^[am][0-9].*[.]Rmd$",
    full.names = TRUE
  )
  package_paths <- c(
    file.path(package_dir, c(
      "DESCRIPTION", "NAMESPACE", "README.Rmd", "README.md", "_pkgdown.yml",
      file.path("inst", "CITATION")
    )),
    list.files(file.path(package_dir, "R"), pattern = "[.]R$", full.names = TRUE),
    list.files(file.path(package_dir, "man"), pattern = "[.]Rd$", full.names = TRUE),
    canonical_vignettes
  )
  active <- doc_read_text_files(package_paths, package_dir, "active_package_surface")
  approved_redirects <- c(
    '- ["articles/a8-eb-handoff.html", "articles/a8-downstream-workflows.html"]',
    paste0(
      '- ["articles/m8-eb-handoff-walters-2024.html", ',
      '"articles/m8-output-contract.html"]'
    )
  )
  active <- active[!(
    active$path == "_pkgdown.yml" & active$text %in% approved_redirects
  ), , drop = FALSE]
  approved_readme_migration <- active$path %in% c("README.Rmd", "README.md") &
    grepl(
      "Optional `ebrecipe` dependency|`as_eb_input\\(\\)`|`sitemix_role = \\\"eb_handoff\\\"`",
      active$text
    )
  active <- active[!approved_readme_migration, , drop = FALSE]
  workflow_paths <- list.files(
    file.path(root, ".github", "workflows"),
    pattern = "[.](yml|yaml)$",
    full.names = TRUE
  )
  workflows <- doc_read_text_files(workflow_paths, root, "active_workflow")
  rbind(active, workflows)
}

doc_allowlisted_coupling <- function(root) {
  news <- doc_read_text_files(file.path(root, "NEWS.md"), root, "historical_NEWS")
  tests <- c(
    list.files(file.path(root, "tests"), pattern = "[.]R$", recursive = TRUE, full.names = TRUE),
    list.files(file.path(root, "tests"), pattern = "[.]csv$", recursive = TRUE, full.names = TRUE)
  )
  tests <- doc_read_text_files(tests, root, "retirement_test_or_fixture")
  protected <- names(.doc_protected_sha256)[startsWith(
    names(.doc_protected_sha256),
    "vignettes/"
  )]
  protected <- doc_read_text_files(
    file.path(root, protected),
    root,
    "protected_legacy_vignette"
  )
  rbind(news, tests, protected)
}

doc_protected_manifest <- function(root) {
  paths <- file.path(root, names(.doc_protected_sha256))
  observed <- rep(NA_character_, length(paths))
  present <- file.exists(paths)
  observed[present] <- doc_sha256(paths[present])
  data.frame(
    path = names(.doc_protected_sha256),
    expected_sha256 = unname(.doc_protected_sha256),
    observed_sha256 = observed,
    exact = present & observed == unname(.doc_protected_sha256),
    stringsAsFactors = FALSE
  )
}

doc_regenerate <- function(package_dir, log_path) {
  messages <- character()
  warnings <- character()
  output <- capture.output(
    withCallingHandlers(
      {
        old <- setwd(package_dir)
        on.exit(setwd(old), add = TRUE)
        roxygen2::roxygenise(
          ".",
          roclets = c("rd", "namespace"),
          load_code = "pkgload",
          clean = TRUE
        )
        rmarkdown::render(
          "README.Rmd",
          output_format = "github_document",
          output_file = "README.md",
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
      },
      message = function(message) {
        messages <<- c(messages, conditionMessage(message))
        invokeRestart("muffleMessage")
      },
      warning = function(warning) {
        warnings <<- c(warnings, conditionMessage(warning))
        invokeRestart("muffleWarning")
      }
    ),
    type = "output"
  )
  messages <- messages[nzchar(trimws(messages))]
  warnings <- warnings[nzchar(trimws(warnings))]
  writeLines(
    c(output, paste0("MESSAGE: ", messages), paste0("WARNING: ", warnings)),
    log_path,
    useBytes = TRUE
  )
  list(messages = length(messages), warnings = length(warnings))
}

doc_generated_comparison <- function(root, package_dir) {
  relative <- c(
    "NAMESPACE",
    "README.md",
    file.path("man", sort(unique(c(
      list.files(file.path(root, "man"), pattern = "[.]Rd$"),
      list.files(file.path(package_dir, "man"), pattern = "[.]Rd$")
    ))))
  )
  current <- file.path(root, relative)
  regenerated <- file.path(package_dir, relative)
  current_exists <- file.exists(current)
  regenerated_exists <- file.exists(regenerated)
  current_sha <- rep(NA_character_, length(relative))
  regenerated_sha <- rep(NA_character_, length(relative))
  current_sha[current_exists] <- doc_sha256(current[current_exists])
  regenerated_sha[regenerated_exists] <- doc_sha256(regenerated[regenerated_exists])
  data.frame(
    path = relative,
    current_exists = current_exists,
    regenerated_exists = regenerated_exists,
    current_sha256 = current_sha,
    regenerated_sha256 = regenerated_sha,
    exact = current_exists & regenerated_exists & current_sha == regenerated_sha,
    stringsAsFactors = FALSE
  )
}

doc_write_outputs <- function(
  target,
  summary,
  generated,
  active,
  allowlisted,
  vignettes,
  protected,
  toolchain,
  build_log,
  regeneration_log,
  tarball
) {
  staging <- paste0(target, ".tmp-", Sys.getpid())
  if (file.exists(staging) || dir.exists(staging)) {
    stop("Documentation staging directory already exists.", call. = FALSE)
  }
  dir.create(staging)
  committed <- FALSE
  on.exit(if (!committed) unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  tables <- list(
    "documentation-summary.csv" = summary,
    "documentation-drift-files.csv" = generated,
    "documentation-coupling-active.csv" = active,
    "documentation-coupling-allowlist.csv" = allowlisted,
    "documentation-vignette-manifest.csv" = vignettes,
    "documentation-protected.csv" = protected,
    "documentation-toolchain.csv" = toolchain
  )
  for (name in names(tables)) {
    utils::write.csv(tables[[name]], file.path(staging, name), row.names = FALSE)
  }
  copied <- file.copy(
    c(build_log, regeneration_log),
    file.path(staging, c("source-build.log", "regeneration.log"))
  )
  if (!all(copied)) {
    stop("Could not copy documentation execution logs.", call. = FALSE)
  }
  writeLines(
    capture.output(utils::sessionInfo()),
    file.path(staging, "session-info.txt"),
    useBytes = TRUE
  )
  saveRDS(
    list(
      summary = summary,
      generated = generated,
      active_coupling = active,
      allowlisted_coupling = allowlisted,
      vignettes = vignettes,
      protected = protected,
      toolchain = toolchain,
      source_tarball_sha256 = doc_sha256(tarball)
    ),
    file.path(staging, "documentation-evidence.rds"),
    version = 3
  )
  if (!file.rename(staging, target)) {
    stop("Could not atomically commit documentation artifacts.", call. = FALSE)
  }
  committed <- TRUE
  normalizePath(target, mustWork = TRUE)
}

doc_main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  doc_validate_args(args)
  root <- doc_root()
  package <- unname(read.dcf(file.path(root, "DESCRIPTION"))[1L, "Package"])
  if (!identical(package, "sitemix")) {
    stop("Documentation drift gate requires the sitemix source tree.", call. = FALSE)
  }
  target <- doc_output_target(doc_arg_value(
    args,
    "out-dir",
    file.path(tempdir(), "sitemix-documentation-drift")
  ))
  toolchain <- doc_require_packages(root)
  protected_before <- doc_protected_manifest(root)
  work_dir <- tempfile("sitemix-documentation-drift-work-")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
  build_log <- file.path(work_dir, "source-build.log")
  regeneration_log <- file.path(work_dir, "regeneration.log")
  built <- doc_build_source(root, work_dir, build_log)
  vignettes <- doc_vignette_manifest(root, built$package_dir)
  active <- doc_active_coupling(root, built$package_dir)
  allowlisted <- doc_allowlisted_coupling(root)
  regeneration <- doc_regenerate(built$package_dir, regeneration_log)
  generated <- doc_generated_comparison(root, built$package_dir)
  protected_after <- doc_protected_manifest(root)
  protected <- protected_after
  protected$before_sha256 <- protected_before$observed_sha256
  protected$unchanged_during_gate <-
    protected_before$observed_sha256 == protected_after$observed_sha256

  generated_drift <- sum(!generated$exact)
  active_coupling <- nrow(active)
  canonical_count <- sum(vignettes$kind == "canonical_vignette")
  manifest_failures <- sum(!vignettes$contract_ok) + as.integer(canonical_count != 17L)
  protected_failures <- sum(!protected$exact | !protected$unchanged_during_gate)
  status <- if (
    generated_drift || active_coupling || manifest_failures || protected_failures ||
      regeneration$warnings
  ) "FAIL" else "PASS"
  summary <- data.frame(
    status = status,
    generated_files = nrow(generated),
    generated_drift = generated_drift,
    active_coupling = active_coupling,
    canonical_vignettes = canonical_count,
    support_files = sum(vignettes$kind == "support"),
    protected_legacy_vignettes = sum(vignettes$kind == "protected_legacy"),
    vignette_manifest_failures = manifest_failures,
    historical_news_matches = sum(allowlisted$scope == "historical_NEWS"),
    retirement_test_fixture_matches =
      sum(allowlisted$scope == "retirement_test_or_fixture"),
    protected_legacy_matches =
      sum(allowlisted$scope == "protected_legacy_vignette"),
    regeneration_messages = regeneration$messages,
    regeneration_warnings = regeneration$warnings,
    protected_failures = protected_failures,
    source_tarball_sha256 = doc_sha256(built$tarball),
    stringsAsFactors = FALSE
  )
  artifact_dir <- doc_write_outputs(
    target,
    summary,
    generated,
    active,
    allowlisted,
    vignettes,
    protected,
    toolchain,
    build_log,
    regeneration_log,
    built$tarball
  )
  print(summary, row.names = FALSE)
  cat("Artifacts: ", artifact_dir, "\n", sep = "")
  if (identical(status, "PASS")) 0L else 1L
}

doc_entry <- function() {
  tryCatch(
    doc_main(),
    error = function(error) {
      message("documentation drift contract error: ", conditionMessage(error))
      2L
    }
  )
}

if (sys.nframe() == 0L) {
  quit(save = "no", status = doc_entry(), runLast = FALSE)
}
