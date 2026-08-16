#!/usr/bin/env Rscript

.vignette_fixed_date <- "2026-07-12"

.vignette_expected_files <- c(
  "a1-getting-started.Rmd",
  "a2-input-formats.Rmd",
  "a3-scenario-binomial.Rmd",
  "a4-multivariate-multinomial.Rmd",
  "a5-published-aggregates.Rmd",
  "a6-diagnostics-and-suppression.Rmd",
  "a7-variance-smoothing-and-frechet.Rmd",
  "a8-downstream-workflows.Rmd",
  "a9-case-study-alabama-prek.Rmd",
  "m1-statistical-foundations.Rmd",
  "m2-scalar-se-binomial.Rmd",
  "m3-multivariate-sur-covariance.Rmd",
  "m4-multinomial-simplex.Rmd",
  "m5-aggregate-engines.Rmd",
  "m6-variance-smoothing-theory.Rmd",
  "m7-frechet-envelope-theory.Rmd",
  "m8-output-contract.Rmd"
)

.vignette_expected_titles <- c(
  "a1-getting-started.Rmd" =
    "A1 · Getting started — your first site-level estimate",
  "a2-input-formats.Rmd" =
    "A2 · Input formats — student rows, counts, or aggregates?",
  "a3-scenario-binomial.Rmd" =
    "A3 · Scenario A — binomial estimates",
  "a4-multivariate-multinomial.Rmd" =
    "A4 · Scenarios B / C — multivariate and multinomial",
  "a5-published-aggregates.Rmd" =
    "A5 · Published aggregates D0 / D1",
  "a6-diagnostics-and-suppression.Rmd" =
    "A6 · Diagnostics and suppression",
  "a7-variance-smoothing-and-frechet.Rmd" =
    "A7 · Variance smoothing and Fréchet stress scenarios",
  "a8-downstream-workflows.Rmd" =
    "A8 · Downstream workflows",
  "a9-case-study-alabama-prek.Rmd" =
    "A9 · Case study — Alabama Pre-K",
  "m1-statistical-foundations.Rmd" =
    "M1 · Statistical foundations — sampling uncertainty",
  "m2-scalar-se-binomial.Rmd" =
    "M2 · Scalar SE — binomial pipeline",
  "m3-multivariate-sur-covariance.Rmd" =
    "M3 · Multivariate SUR covariance",
  "m4-multinomial-simplex.Rmd" =
    "M4 · Multinomial simplex covariance",
  "m5-aggregate-engines.Rmd" =
    "M5 · Aggregate engines D0 / D1",
  "m6-variance-smoothing-theory.Rmd" =
    "M6 · Variance smoothing theory",
  "m7-frechet-envelope-theory.Rmd" =
    "M7 · Fréchet pairwise intervals and projected stress theory",
  "m8-output-contract.Rmd" =
    "M8 · Output contract"
)

.vignette_warning_expectations <- c(
  "a5-published-aggregates.Rmd" =
    "sitemix_warning_working_independence_default",
  "a7-variance-smoothing-and-frechet.Rmd" =
    "sitemix_warning_working_independence_default",
  "m5-aggregate-engines.Rmd" =
    "sitemix_warning_working_independence_default",
  "m6-variance-smoothing-theory.Rmd" =
    "sitemix_warning_raw_scale_smoothing",
  "m7-frechet-envelope-theory.Rmd" =
    "sitemix_warning_working_independence_default",
  "m8-output-contract.Rmd" =
    "sitemix_warning_working_independence_default"
)

.vignette_redirects <- c(
  "articles/a8-eb-handoff.html" =
    "articles/a8-downstream-workflows.html",
  "articles/m8-eb-handoff-walters-2024.html" =
    "articles/m8-output-contract.html"
)

.vignette_protected_sha256 <- c(
  "inst/scripts/build-regression-baselines.R" =
    "29e8909b541af31ff47042591b462bd745c8b172bf6574a4ee6a90ced050acb1",
  "tests/testthat/_data/regression/regression-baselines.rds" =
    "be0527f9357aa7cbb0c014a9b0ce8e60e15252b5270fad5bb99113106f9e075b",
  "tests/testthat/_snaps/output-schema.md" =
    "ed838cde596fba9618627826af12e5e5b286fa633076474bc9e47f6824885c8e"
)

vignette_arg_value <- function(args, name, default = NULL) {
  prefix <- paste0("--", name, "=")
  hit <- grep(paste0("^", prefix), args, value = TRUE)
  if (length(hit)) {
    sub(prefix, "", hit[[1L]], fixed = TRUE)
  } else {
    default
  }
}

vignette_validate_args <- function(args, names) {
  prefixes <- paste0("--", names, "=")
  valid <- vapply(args, function(arg) {
    any(startsWith(arg, prefixes))
  }, logical(1))
  if (any(!valid)) {
    stop("Unknown or malformed argument: ", args[which(!valid)[[1L]]], call. = FALSE)
  }
  observed <- sub("^--([^=]+).*$", "\\1", args)
  if (anyDuplicated(observed)) {
    stop("Duplicate argument: --", observed[duplicated(observed)][[1L]], call. = FALSE)
  }
  if (!setequal(observed, names)) {
    stop("Required arguments do not match the execution mode.", call. = FALSE)
  }
  invisible(TRUE)
}

vignette_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (!length(file_arg)) {
    stop("The vignette audit must be run with Rscript.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
}

vignette_root <- function() {
  script <- vignette_script_path()
  root <- normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE)
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    stop("Could not locate the package root.", call. = FALSE)
  }
  root
}

vignette_output_target <- function(path) {
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

vignette_sha256 <- function(paths) {
  paths <- as.character(paths)
  if (any(!file.exists(paths))) {
    stop("Cannot hash a missing file.", call. = FALSE)
  }
  unname(tools::sha256sum(paths))
}

vignette_require_packages <- function() {
  packages <- c("knitr", "pkgdown", "pkgload", "rmarkdown", "yaml")
  versions <- vapply(packages, function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop("Vignette audit requires package `", package, "`.", call. = FALSE)
    }
    as.character(utils::packageVersion(package))
  }, character(1))
  data.frame(
    tool = c(packages, "R", "Pandoc"),
    version = c(
      unname(versions),
      as.character(getRversion()),
      as.character(rmarkdown::pandoc_version())
    ),
    stringsAsFactors = FALSE
  )
}

vignette_expected_warning <- function(file) {
  if (!file %in% names(.vignette_warning_expectations)) {
    ""
  } else {
    unname(.vignette_warning_expectations[[file]])
  }
}

vignette_source_contract <- function(root) {
  vignette_dir <- file.path(root, "vignettes")
  observed <- sort(list.files(
    vignette_dir,
    pattern = "^[am][0-9].*[.]Rmd$",
    full.names = FALSE
  ))
  expected <- sort(.vignette_expected_files)
  manifest_exact <- identical(observed, expected)
  files <- sort(unique(c(observed, expected)))
  rows <- lapply(files, function(file) {
    path <- file.path(vignette_dir, file)
    exists <- file.exists(path)
    lines <- if (exists) readLines(path, warn = FALSE) else character()
    text <- paste(lines, collapse = "\n")
    if (!file %in% names(.vignette_expected_titles)) {
      expected_title <- ""
    } else {
      expected_title <- unname(.vignette_expected_titles[[file]])
    }
    expected_warning <- vignette_expected_warning(file)
    helper_definition <- grepl(
      "capture_expected_sitemix_warning <- function",
      text,
      fixed = TRUE
    )
    helper_calls <- sum(grepl(
      "capture_expected_sitemix_warning(",
      lines,
      fixed = TRUE
    ))
    warning_contract <- if (nzchar(expected_warning)) {
      helper_definition && helper_calls == 1L &&
        grepl(expected_warning, text, fixed = TRUE)
    } else {
      !helper_definition && helper_calls == 0L
    }
    contract <- data.frame(
      file = file,
      exists = exists,
      expected_manifest = file %in% .vignette_expected_files,
      manifest_exact = manifest_exact,
      title_exact = sum(lines == paste0("title: \"", expected_title, "\"")) == 1L,
      index_exact = sum(lines == paste0(
        "  %\\VignetteIndexEntry{", expected_title, "}"
      )) == 1L,
      fixed_date = sum(lines == paste0("date: \"", .vignette_fixed_date, "\"")) == 1L,
      no_dynamic_date = !grepl("Sys.Date(", text, fixed = TRUE),
      html_vignette = grepl("rmarkdown::html_vignette:", text, fixed = TRUE),
      bibliography = sum(lines == "bibliography: references.bib") == 1L,
      csl = sum(lines == "csl: apa.csl") == 1L,
      setup_fail_fast = sum(
        lines == "```{r setup, include = FALSE, error = FALSE}"
      ) == 1L,
      global_error_false = grepl("error[[:space:]]*=[[:space:]]*FALSE", text),
      global_warning_hidden = grepl(
        "warning[[:space:]]*=[[:space:]]*FALSE",
        text
      ),
      warnings_are_errors = sum(lines == "options(warn = 2)") == 1L,
      seed_once = sum(grepl("set.seed(", lines, fixed = TRUE)) == 1L,
      rng_kind_fixed = all(vapply(c(
        "1L, kind = \"Mersenne-Twister\"",
        "normal.kind = \"Inversion\"",
        "sample.kind = \"Rejection\""
      ), grepl, logical(1), x = text, fixed = TRUE)),
      figures_fixed = all(vapply(c(
        "fig[.]width[[:space:]]*=[[:space:]]*7",
        "fig[.]height[[:space:]]*=[[:space:]]*4[.]5",
        "fig[.]retina[[:space:]]*=[[:space:]]*2",
        "dpi[[:space:]]*=[[:space:]]*144",
        "fig[.]align[[:space:]]*=",
        "out[.]width[[:space:]]*=[[:space:]]*\"100%\""
      ), grepl, logical(1), x = text)),
      no_blanket_warning_suppression = !grepl("suppressWarnings(", text, fixed = TRUE),
      warning_contract = warning_contract,
      package_neutral = !grepl(
        "ebrecipe|as_eb_input|eb_handoff",
        text,
        ignore.case = TRUE
      ),
      stringsAsFactors = FALSE
    )
    checks <- setdiff(names(contract), c("file", "expected_manifest"))
    contract$contract_ok <- exists && all(unlist(contract[checks], use.names = FALSE))
    contract
  })
  do.call(rbind, rows)
}

vignette_extract_links <- function(text) {
  pattern <- "\\]\\(([^)]+[.]html(?:#[^)]*)?)\\)"
  matches <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1L]]
  if (!length(matches) || identical(matches, "")) {
    character()
  } else {
    sub("^.*\\]\\(", "", sub("\\)$", "", matches))
  }
}

vignette_link_contract <- function(root) {
  vignette_dir <- file.path(root, "vignettes")
  rows <- list()
  index <- 0L
  for (file in .vignette_expected_files) {
    source <- file.path(vignette_dir, file)
    text <- paste(readLines(source, warn = FALSE), collapse = "\n")
    links <- vignette_extract_links(text)
    links <- links[!grepl("^[[:alpha:]][[:alnum:]+.-]*://", links)]
    for (link in links) {
      index <- index + 1L
      target <- sub("[#?].*$", "", link)
      reference <- startsWith(target, "../reference/")
      if (reference) {
        resolved <- file.path(
          root,
          "man",
          sub("[.]html$", ".Rd", basename(target))
        )
      } else {
        resolved <- file.path(
          dirname(source),
          sub("[.]html$", ".Rmd", target)
        )
      }
      resolved <- normalizePath(resolved, mustWork = FALSE)
      rows[[index]] <- data.frame(
        source = file,
        link = link,
        target_kind = if (reference) "reference" else "article",
        resolved_source = basename(resolved),
        exists = file.exists(resolved),
        old_slug = grepl(
          "a8-eb-handoff|m8-eb-handoff-walters-2024",
          link
        ),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    data.frame(
      source = character(), link = character(), target_kind = character(),
      resolved_source = character(), exists = logical(), old_slug = logical(),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, rows)
  }
}

vignette_redirect_config <- function(root) {
  path <- file.path(root, "_pkgdown.yml")
  config <- yaml::read_yaml(path)
  redirects <- config$redirects
  rows <- lapply(redirects, function(redirect) {
    data.frame(
      from = as.character(redirect[[1L]]),
      to = as.character(redirect[[2L]]),
      stringsAsFactors = FALSE
    )
  })
  observed <- if (length(rows)) {
    do.call(rbind, rows)
  } else {
    data.frame(from = character(), to = character(), stringsAsFactors = FALSE)
  }
  expected <- data.frame(
    from = names(.vignette_redirects),
    to = unname(.vignette_redirects),
    stringsAsFactors = FALSE
  )
  observed$key <- paste(observed$from, observed$to, sep = " -> ")
  expected$key <- paste(expected$from, expected$to, sep = " -> ")
  observed$expected <- observed$key %in% expected$key
  lines <- trimws(readLines(path, warn = FALSE))
  approved_lines <- paste0(
    "- [\"", expected$from, "\", \"", expected$to, "\"]"
  )
  exact <- nrow(observed) == nrow(expected) &&
    !anyDuplicated(observed$from) && setequal(observed$key, expected$key) &&
    all(vapply(approved_lines, function(line) sum(lines == line) == 1L, logical(1)))
  list(table = observed, exact = exact)
}

vignette_protected_manifest <- function(root) {
  paths <- file.path(root, names(.vignette_protected_sha256))
  present <- file.exists(paths)
  observed <- rep(NA_character_, length(paths))
  observed[present] <- vignette_sha256(paths[present])
  data.frame(
    path = names(.vignette_protected_sha256),
    expected_sha256 = unname(.vignette_protected_sha256),
    observed_sha256 = observed,
    exact = present & observed == unname(.vignette_protected_sha256),
    stringsAsFactors = FALSE
  )
}

vignette_worker <- function(args) {
  names <- c("worker-root", "worker-input", "worker-out", "worker-result")
  vignette_validate_args(args, names)
  root <- normalizePath(vignette_arg_value(args, "worker-root"), mustWork = TRUE)
  input <- normalizePath(vignette_arg_value(args, "worker-input"), mustWork = TRUE)
  out_dir <- vignette_arg_value(args, "worker-out")
  result_path <- vignette_arg_value(args, "worker-result")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(result_path), recursive = TRUE, showWarnings = FALSE)
  options(
    warn = 2,
    sitemix.vignette.warning_observations = list()
  )
  unexpected <- list()
  rendered <- NULL
  error_message <- NULL
  started <- Sys.time()
  rendered <- tryCatch(
    withCallingHandlers(
      {
        pkgload::load_all(
          root,
          reset = TRUE,
          attach = TRUE,
          export_all = FALSE,
          export_imports = FALSE,
          helpers = FALSE,
          attach_testthat = FALSE,
          quiet = TRUE,
          warn_conflicts = FALSE,
          debug = FALSE
        )
        rmarkdown::render(
          input,
          output_file = paste0(tools::file_path_sans_ext(basename(input)), ".html"),
          output_dir = out_dir,
          clean = TRUE,
          envir = new.env(parent = globalenv()),
          quiet = TRUE
        )
      },
      warning = function(condition) {
        unexpected[[length(unexpected) + 1L]] <<- list(
          class = class(condition),
          message = conditionMessage(condition)
        )
        stop("Unexpected vignette warning: ", conditionMessage(condition), call. = FALSE)
      }
    ),
    error = function(condition) {
      error_message <<- conditionMessage(condition)
      NULL
    }
  )
  finished <- Sys.time()
  observations <- getOption("sitemix.vignette.warning_observations", list())
  file <- basename(input)
  expected_class <- vignette_expected_warning(file)
  observed_classes <- vapply(observations, function(observation) {
    as.character(observation$expected_class)
  }, character(1))
  observed_counts <- vapply(observations, function(observation) {
    as.integer(observation$count)
  }, integer(1))
  warning_contract <- if (nzchar(expected_class)) {
    length(observations) == 1L && identical(observed_classes, expected_class) &&
      identical(observed_counts, 1L)
  } else {
    length(observations) == 0L
  }
  output_exists <- !is.null(rendered) && file.exists(rendered) &&
    isTRUE(file.info(rendered)$size > 0)
  if (is.null(error_message) && !warning_contract) {
    error_message <- "Expected-warning observation contract failed."
  }
  if (is.null(error_message) && !output_exists) {
    error_message <- "Render did not create one nonempty HTML file."
  }
  status <- if (is.null(error_message)) "PASS" else "FAIL"
  result <- list(
    vignette = file,
    status = status,
    pid = Sys.getpid(),
    started_utc = format(started, tz = "UTC", usetz = TRUE),
    duration_seconds = as.numeric(difftime(finished, started, units = "secs")),
    expected_warning_class = expected_class,
    observed_warning_classes = observed_classes,
    observed_warning_counts = observed_counts,
    unexpected_warnings = unexpected,
    output = if (output_exists) normalizePath(rendered) else "",
    output_sha256 = if (output_exists) vignette_sha256(rendered) else "",
    error = if (is.null(error_message)) "" else error_message,
    r_version = as.character(getRversion())
  )
  saveRDS(result, result_path, version = 3)
  cat(
    file, status,
    paste0("expected_warning=", expected_class),
    paste0("observed_count=", sum(observed_counts)),
    paste0("pid=", result$pid),
    sep = " | "
  )
  cat("\n")
  if (identical(status, "PASS")) 0L else 1L
}

vignette_redirect_worker <- function(args) {
  vignette_validate_args(args, "redirect-probe")
  probe <- normalizePath(vignette_arg_value(args, "redirect-probe"), mustWork = TRUE)
  options(warn = 2)
  pkgdown::build_redirects(probe)
  0L
}

vignette_run_workers <- function(root, work_dir) {
  script <- vignette_script_path()
  rscript <- file.path(R.home("bin"), "Rscript")
  render_root <- file.path(work_dir, "renders")
  result_root <- file.path(work_dir, "worker-results")
  log_root <- file.path(work_dir, "logs")
  dir.create(render_root, recursive = TRUE)
  dir.create(result_root, recursive = TRUE)
  dir.create(log_root, recursive = TRUE)
  results <- vector("list", length(.vignette_expected_files))
  for (index in seq_along(.vignette_expected_files)) {
    file <- .vignette_expected_files[[index]]
    id <- tools::file_path_sans_ext(file)
    output <- file.path(render_root, id)
    result_path <- file.path(result_root, paste0(id, ".rds"))
    log_path <- file.path(log_root, paste0(id, ".log"))
    dir.create(output, recursive = TRUE)
    status <- system2(
      rscript,
      c(
        "--vanilla",
        shQuote(script),
        shQuote(paste0("--worker-root=", root)),
        shQuote(paste0(
          "--worker-input=", file.path(root, "vignettes", file)
        )),
        shQuote(paste0("--worker-out=", output)),
        shQuote(paste0("--worker-result=", result_path))
      ),
      stdout = log_path,
      stderr = log_path
    )
    if (file.exists(result_path)) {
      result <- readRDS(result_path)
    } else {
      result <- list(
        vignette = file,
        status = "FAIL",
        pid = NA_integer_,
        started_utc = "",
        duration_seconds = NA_real_,
        expected_warning_class = vignette_expected_warning(file),
        observed_warning_classes = character(),
        observed_warning_counts = integer(),
        unexpected_warnings = list(),
        output = "",
        output_sha256 = "",
        error = "Worker did not write its result artifact.",
        r_version = ""
      )
    }
    result$process_exit_status <- as.integer(status)
    result$log <- log_path
    results[[index]] <- result
  }
  results
}

vignette_render_table <- function(results) {
  rows <- lapply(results, function(result) {
    observed_classes <- result$observed_warning_classes
    observed_counts <- result$observed_warning_counts
    data.frame(
      vignette = result$vignette,
      status = result$status,
      process_exit_status = result$process_exit_status,
      pid = result$pid,
      started_utc = result$started_utc,
      duration_seconds = result$duration_seconds,
      expected_warning_class = result$expected_warning_class,
      observed_warning_class = paste(observed_classes, collapse = "|"),
      observed_warning_count = sum(observed_counts),
      unexpected_warning_count = length(result$unexpected_warnings),
      output = result$output,
      output_sha256 = result$output_sha256,
      r_version = result$r_version,
      error = result$error,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

vignette_build_redirects <- function(root, work_dir) {
  probe <- file.path(work_dir, "redirect-probe")
  dir.create(probe)
  copied <- file.copy(
    file.path(root, c("DESCRIPTION", "_pkgdown.yml")),
    probe
  )
  if (!all(copied)) {
    stop("Could not create the isolated redirect probe.", call. = FALSE)
  }
  log_path <- file.path(work_dir, "redirect-build.log")
  status <- system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      "--vanilla",
      shQuote(vignette_script_path()),
      shQuote(paste0("--redirect-probe=", probe))
    ),
    stdout = log_path,
    stderr = log_path
  )
  base_url <- "https://joonho112.github.io/sitemix/"
  rows <- lapply(names(.vignette_redirects), function(from) {
    to <- unname(.vignette_redirects[[from]])
    path <- file.path(probe, "docs", from)
    exists <- file.exists(path)
    text <- if (exists) paste(readLines(path, warn = FALSE), collapse = "\n") else ""
    target <- paste0(base_url, to)
    data.frame(
      from = from,
      to = to,
      exists = exists,
      nonempty = exists && isTRUE(file.info(path)$size > 0),
      meta_refresh_exact = grepl(
        paste0("content=\"0;URL=", target, "\""),
        text,
        fixed = TRUE
      ),
      canonical_exact = grepl(
        paste0("rel=\"canonical\" href=\"", target, "\""),
        text,
        fixed = TRUE
      ),
      sha256 = if (exists) vignette_sha256(path) else "",
      stringsAsFactors = FALSE
    )
  })
  list(
    table = do.call(rbind, rows),
    status = as.integer(status),
    log = log_path,
    probe = probe
  )
}

vignette_source_manifest <- function(root) {
  relative <- c(
    file.path("vignettes", .vignette_expected_files),
    "vignettes/references.bib",
    "NEWS.md",
    "_pkgdown.yml",
    "R/sm_estimate.R",
    "inst/scripts/audit-documentation-drift.R",
    "inst/scripts/audit-vignette-reproducibility.R"
  )
  paths <- file.path(root, relative)
  data.frame(
    path = relative,
    size_bytes = file.info(paths)$size,
    sha256 = vignette_sha256(paths),
    stringsAsFactors = FALSE
  )
}

vignette_summary_markdown <- function(summary, render) {
  c(
    "# Vignette reproducibility summary",
    "",
    paste0("- Status: **", summary$status, "**"),
    paste0("- Canonical source files: ", summary$canonical_vignettes),
    paste0("- Source-contract failures: ", summary$source_contract_failures),
    paste0("- Internal article links: ", summary$internal_links),
    paste0("- Link failures: ", summary$link_failures),
    paste0("- Fresh-process renders: ", summary$render_passes, "/17 PASS"),
    paste0("- Expected classed warnings captured: ", summary$expected_warnings),
    paste0("- Unexpected warnings: ", summary$unexpected_warnings),
    paste0("- Redirect pages: ", summary$redirect_passes, "/2 PASS"),
    paste0("- Protected failures: ", summary$protected_failures),
    "",
    "## Render matrix",
    "",
    "| Vignette | Status | Expected warning | Observed | Seconds |",
    "|:--|:--:|:--|--:|--:|",
    vapply(seq_len(nrow(render)), function(index) {
      row <- render[index, ]
      sprintf(
        "| `%s` | %s | `%s` | %d | %.2f |",
        row$vignette,
        row$status,
        row$expected_warning_class,
        row$observed_warning_count,
        row$duration_seconds
      )
    }, character(1))
  )
}

vignette_write_outputs <- function(
  target,
  summary,
  source,
  links,
  redirect_config,
  redirect_build,
  render,
  protected,
  toolchain,
  manifest,
  results,
  work_dir
) {
  staging <- paste0(target, ".tmp-", Sys.getpid())
  if (file.exists(staging) || dir.exists(staging)) {
    stop("Vignette staging directory already exists.", call. = FALSE)
  }
  dir.create(staging)
  committed <- FALSE
  on.exit(if (!committed) unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  tables <- list(
    "vignette-summary.csv" = summary,
    "vignette-source-contract.csv" = source,
    "vignette-link-contract.csv" = links,
    "vignette-redirect-config.csv" = redirect_config,
    "vignette-redirect-pages.csv" = redirect_build$table,
    "vignette-render-matrix.csv" = render,
    "vignette-protected.csv" = protected,
    "vignette-toolchain.csv" = toolchain,
    "vignette-source-manifest.csv" = manifest
  )
  for (name in names(tables)) {
    utils::write.csv(tables[[name]], file.path(staging, name), row.names = FALSE)
  }
  writeLines(
    vignette_summary_markdown(summary, render),
    file.path(staging, "vignette-summary.md"),
    useBytes = TRUE
  )
  writeLines(
    capture.output(utils::sessionInfo()),
    file.path(staging, "session-info.txt"),
    useBytes = TRUE
  )
  saveRDS(
    list(
      summary = summary,
      source_contract = source,
      link_contract = links,
      redirect_config = redirect_config,
      redirect_pages = redirect_build$table,
      render_matrix = render,
      protected = protected,
      toolchain = toolchain,
      source_manifest = manifest,
      worker_results = results
    ),
    file.path(staging, "vignette-evidence.rds"),
    version = 3
  )
  copied <- file.copy(
    c(
      file.path(work_dir, "logs"),
      file.path(work_dir, "renders"),
      file.path(work_dir, "worker-results")
    ),
    staging,
    recursive = TRUE
  )
  if (!all(copied)) {
    stop("Could not copy fresh-render evidence.", call. = FALSE)
  }
  if (!file.copy(redirect_build$log, file.path(staging, "redirect-build.log"))) {
    stop("Could not copy the redirect build log.", call. = FALSE)
  }
  redirect_dir <- file.path(staging, "redirect-pages")
  dir.create(redirect_dir)
  for (from in names(.vignette_redirects)) {
    source_path <- file.path(redirect_build$probe, "docs", from)
    destination <- file.path(redirect_dir, from)
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    if (!file.copy(source_path, destination)) {
      stop("Could not copy a redirect page.", call. = FALSE)
    }
  }
  if (!file.rename(staging, target)) {
    stop("Could not atomically commit vignette artifacts.", call. = FALSE)
  }
  committed <- TRUE
  normalizePath(target, mustWork = TRUE)
}

vignette_main <- function(args) {
  vignette_validate_args(args, "out-dir")
  root <- vignette_root()
  package <- unname(read.dcf(file.path(root, "DESCRIPTION"))[1L, "Package"])
  if (!identical(package, "sitemix")) {
    stop("Vignette audit requires the sitemix source tree.", call. = FALSE)
  }
  target <- vignette_output_target(vignette_arg_value(args, "out-dir"))
  toolchain <- vignette_require_packages()
  protected_before <- vignette_protected_manifest(root)
  work_dir <- tempfile("sitemix-vignette-audit-work-")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

  source <- vignette_source_contract(root)
  links <- vignette_link_contract(root)
  redirect_config <- vignette_redirect_config(root)
  results <- vignette_run_workers(root, work_dir)
  render <- vignette_render_table(results)
  redirect_build <- vignette_build_redirects(root, work_dir)
  protected_after <- vignette_protected_manifest(root)
  protected <- protected_after
  protected$before_sha256 <- protected_before$observed_sha256
  protected$unchanged_during_gate <-
    protected_before$observed_sha256 == protected_after$observed_sha256
  manifest <- vignette_source_manifest(root)

  source_failures <- sum(!source$contract_ok)
  link_failures <- sum(!links$exists | links$old_slug)
  render_failures <- sum(
    render$status != "PASS" | render$process_exit_status != 0L
  )
  redirect_page_ok <- with(redirect_build$table, {
    exists & nonempty & meta_refresh_exact & canonical_exact
  })
  redirect_failures <- as.integer(!redirect_config$exact) +
    as.integer(redirect_build$status != 0L) + sum(!redirect_page_ok)
  protected_failures <- sum(
    !protected$exact | !protected$unchanged_during_gate
  )
  status <- if (
    source_failures || link_failures || render_failures ||
      redirect_failures || protected_failures || nrow(links) == 0L
  ) "FAIL" else "PASS"
  summary <- data.frame(
    status = status,
    canonical_vignettes = sum(source$exists),
    source_contract_failures = source_failures,
    internal_links = nrow(links),
    link_failures = link_failures,
    render_passes = sum(render$status == "PASS"),
    render_failures = render_failures,
    expected_warnings = sum(render$observed_warning_count),
    unexpected_warnings = sum(render$unexpected_warning_count),
    redirect_config_exact = redirect_config$exact,
    redirect_passes = sum(redirect_page_ok),
    redirect_failures = redirect_failures,
    protected_failures = protected_failures,
    stringsAsFactors = FALSE
  )
  artifact_dir <- vignette_write_outputs(
    target,
    summary,
    source,
    links,
    redirect_config$table,
    redirect_build,
    render,
    protected,
    toolchain,
    manifest,
    results,
    work_dir
  )
  print(summary, row.names = FALSE)
  cat("Artifacts: ", artifact_dir, "\n", sep = "")
  if (identical(status, "PASS")) 0L else 1L
}

vignette_entry <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  tryCatch(
    {
      if (any(startsWith(args, "--worker-"))) {
        vignette_worker(args)
      } else if (any(startsWith(args, "--redirect-probe="))) {
        vignette_redirect_worker(args)
      } else {
        vignette_main(args)
      }
    },
    error = function(error) {
      message("vignette reproducibility contract error: ", conditionMessage(error))
      2L
    }
  )
}

if (sys.nframe() == 0L) {
  quit(save = "no", status = vignette_entry(), runLast = FALSE)
}
