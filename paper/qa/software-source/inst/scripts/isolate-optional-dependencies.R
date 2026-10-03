#!/usr/bin/env Rscript

# Run an exact optional-negative test profile in fresh R processes. Package
# directories are renamed only on GitHub Actions and are restored on every exit.

sm_optional_set <- function(x) {
  if (!length(x) || is.na(x) || !nzchar(x) || identical(x, "none")) {
    return(character())
  }
  strsplit(x, "|", fixed = TRUE)[[1L]]
}

sm_optional_join <- function(x) {
  if (!length(x)) "none" else paste(x, collapse = "|")
}

sm_optional_root <- function() {
  command <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command, value = TRUE)
  if (length(file_arg)) {
    script <- sub("^--file=", "", file_arg[[1L]])
    candidate <- file.path(dirname(script), "..", "..")
    if (file.exists(file.path(candidate, "DESCRIPTION"))) {
      return(normalizePath(candidate, mustWork = TRUE))
    }
  }
  root <- normalizePath(getwd(), mustWork = TRUE)
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    stop("Could not locate the sitemix package root.", call. = FALSE)
  }
  root
}

sm_optional_profile <- function(profile) {
  switch(
    profile,
    "minimum-r-hard-only" = list(present = character(), absent = c("dplyr", "mgcv")),
    "optional-no-gam" = list(present = "dplyr", absent = "mgcv"),
    "optional-no-dplyr" = list(present = "mgcv", absent = "dplyr"),
    "optional-none" = list(present = character(), absent = c("dplyr", "mgcv")),
    stop(
      "`profile` must be one of minimum-r-hard-only, optional-no-gam, ",
      "optional-no-dplyr, or optional-none.",
      call. = FALSE
    )
  )
}

sm_optional_arg <- function(args, name, default = NULL) {
  prefix <- paste0(name, "=")
  value <- args[startsWith(args, prefix)]
  if (!length(value)) default else sub(paste0("^", prefix), "", value[[1L]])
}

sm_optional_parse_args <- function(args) {
  value_options <- c("--profile", "--out-dir")
  known <- args == "--dry-run"
  for (option in value_options) {
    known <- known | startsWith(args, paste0(option, "="))
  }
  if (any(!known)) {
    stop("Unknown arguments: ", paste(args[!known], collapse = ", "), call. = FALSE)
  }
  if (sum(args == "--dry-run") > 1L) {
    stop("Duplicate argument: --dry-run", call. = FALSE)
  }
  for (option in value_options) {
    matches <- startsWith(args, paste0(option, "="))
    if (sum(matches) > 1L) {
      stop("Duplicate argument: ", option, call. = FALSE)
    }
    if (any(matches) && !nzchar(sm_optional_arg(args, option, ""))) {
      stop("Argument requires a value: ", option, call. = FALSE)
    }
  }
  profile <- sm_optional_arg(args, "--profile", NULL)
  if (is.null(profile)) {
    stop("`--profile=<name>` is required.", call. = FALSE)
  }
  list(
    profile = profile,
    out_dir = sm_optional_arg(args, "--out-dir", NULL),
    dry_run = "--dry-run" %in% args
  )
}

sm_optional_installations <- function(packages) {
  libraries <- unique(normalizePath(.libPaths(), mustWork = TRUE))
  rows <- lapply(packages, function(package) {
    paths <- file.path(libraries, package)
    paths <- paths[dir.exists(paths)]
    if (!length(paths)) {
      return(NULL)
    }
    data.frame(package = package, path = paths, stringsAsFactors = FALSE)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    return(data.frame(package = character(), path = character()))
  }
  do.call(rbind, rows)
}

sm_optional_proof_script <- function() {
  path <- tempfile("sitemix-optional-proof-", fileext = ".R")
  writeLines(
    c(
      "args <- commandArgs(trailingOnly = TRUE)",
      "profile <- args[[1L]]",
      "out_dir <- args[[2L]]",
      "split_set <- function(x) if (identical(x, 'none')) character() else strsplit(x, '|', fixed = TRUE)[[1L]]",
      "present <- split_set(args[[3L]])",
      "absent <- split_set(args[[4L]])",
      "packages <- c(present, absent)",
      "expected <- c(rep(TRUE, length(present)), rep(FALSE, length(absent)))",
      "loaded_before <- packages %in% loadedNamespaces()",
      "available <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)",
      "proof <- data.frame(",
      "  profile = rep(profile, length(packages)),",
      "  package = packages,",
      "  expected_available = expected,",
      "  available = unname(available),",
      "  namespace_loaded_before = loaded_before,",
      "  child_pid = rep(Sys.getpid(), length(packages)),",
      "  stringsAsFactors = FALSE",
      ")",
      "proof$ok <- proof$expected_available == proof$available & !proof$namespace_loaded_before",
      "dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)",
      "utils::write.csv(proof, file.path(out_dir, 'optional-namespace-proof.csv'), row.names = FALSE)",
      "if (!all(proof$ok)) quit(save = 'no', status = 3L, runLast = FALSE)"
    ),
    path,
    useBytes = TRUE
  )
  path
}

sm_optional_run <- function(parsed) {
  root <- sm_optional_root()
  spec <- sm_optional_profile(parsed$profile)
  out_dir <- if (is.null(parsed$out_dir)) {
    file.path("ci-artifacts", "test-budget", parsed$profile)
  } else {
    parsed$out_dir
  }
  installations <- sm_optional_installations(spec$absent)

  if (isTRUE(parsed$dry_run)) {
    cat(
      "optional-dependency isolation DRY RUN; profile=", parsed$profile,
      "; required_present=", sm_optional_join(spec$present),
      "; required_absent=", sm_optional_join(spec$absent),
      "; installations_to_rename=", nrow(installations),
      "; out_dir=", out_dir, "\n",
      sep = ""
    )
    if (nrow(installations)) {
      print(installations, row.names = FALSE)
    }
    return(0L)
  }

  ci_guard <- identical(tolower(Sys.getenv("CI")), "true") &&
    identical(tolower(Sys.getenv("GITHUB_ACTIONS")), "true")
  if (!ci_guard) {
    stop(
      "Optional-package isolation is restricted to GitHub Actions. ",
      "Use --dry-run for local validation; no local package will be moved.",
      call. = FALSE
    )
  }
  already_loaded <- intersect(spec$absent, loadedNamespaces())
  if (length(already_loaded)) {
    stop(
      "Refusing to move loaded namespaces: ",
      paste(already_loaded, collapse = ", "),
      call. = FALSE
    )
  }
  missing_present <- spec$present[
    !vapply(spec$present, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(missing_present)) {
    stop(
      "Required optional packages are missing before isolation: ",
      paste(missing_present, collapse = ", "),
      call. = FALSE
    )
  }
  non_writable <- installations$path[
    file.access(dirname(installations$path), mode = 2L) != 0L
  ]
  if (length(non_writable)) {
    stop(
      "Optional package libraries are not writable: ",
      paste(non_writable, collapse = ", "),
      ". Invoke this CI-only wrapper with `sudo -E Rscript` so every ",
      "visible installation can be restored safely.",
      call. = FALSE
    )
  }

  moves <- data.frame(
    package = character(), source = character(), quarantine = character(),
    stringsAsFactors = FALSE
  )
  isolation_summary <- data.frame(
    package = character(), source = character(), quarantine = character(),
    isolated = logical(), restored = logical(), ok = logical(),
    pid = integer(), profile = character(), stringsAsFactors = FALSE
  )
  absolute_out_dir <- if (grepl("^(/|[A-Za-z]:[/\\\\])", out_dir)) {
    out_dir
  } else {
    file.path(root, out_dir)
  }
  summary_path <- file.path(absolute_out_dir, "isolation-summary.csv")
  write_summary <- function() {
    dir.create(dirname(summary_path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(isolation_summary, summary_path, row.names = FALSE)
  }
  restore <- function() {
    if (!nrow(moves)) {
      write_summary()
      return(invisible(TRUE))
    }
    failures <- character()
    for (i in rev(seq_len(nrow(moves)))) {
      source <- moves$source[[i]]
      quarantine <- moves$quarantine[[i]]
      if (file.exists(quarantine) && !file.exists(source)) {
        if (!file.rename(quarantine, source)) {
          failures <- c(failures, paste0(quarantine, " -> ", source))
        } else {
          isolation_summary$restored[isolation_summary$source == source] <<- TRUE
        }
      } else if (file.exists(quarantine) || !file.exists(source)) {
        failures <- c(failures, paste0("ambiguous restore state: ", source))
      } else {
        isolation_summary$restored[isolation_summary$source == source] <<- TRUE
      }
    }
    isolation_summary$ok <<-
      isolation_summary$isolated & isolation_summary$restored
    write_summary()
    if (length(failures)) {
      stop("Failed to restore isolated packages: ", paste(failures, collapse = "; "), call. = FALSE)
    }
    invisible(TRUE)
  }
  on.exit(restore(), add = TRUE)

  for (i in seq_len(nrow(installations))) {
    source <- installations$path[[i]]
    quarantine <- file.path(
      dirname(source),
      paste0(".", basename(source), "-sitemix-isolated-", Sys.getpid())
    )
    if (file.exists(quarantine)) {
      stop("Isolation target already exists: ", quarantine, call. = FALSE)
    }
    if (!file.rename(source, quarantine)) {
      stop("Could not isolate optional package directory: ", source, call. = FALSE)
    }
    moves <- rbind(
      moves,
      data.frame(
        package = installations$package[[i]],
        source = source,
        quarantine = quarantine,
        stringsAsFactors = FALSE
      )
    )
    isolation_summary <- rbind(
      isolation_summary,
      data.frame(
        package = installations$package[[i]],
        source = source,
        quarantine = quarantine,
        isolated = TRUE,
        restored = FALSE,
        ok = TRUE,
        pid = as.integer(Sys.getpid()),
        profile = parsed$profile,
        stringsAsFactors = FALSE
      )
    )
    write_summary()
  }
  if (!nrow(installations)) {
    write_summary()
  }

  unexpectedly_present <- spec$absent[
    vapply(spec$absent, requireNamespace, logical(1), quietly = TRUE)
  ]
  if (length(unexpectedly_present)) {
    stop(
      "Isolation did not hide all optional packages: ",
      paste(unexpectedly_present, collapse = ", "),
      call. = FALSE
    )
  }

  old_wd <- setwd(root)
  on.exit(setwd(old_wd), add = TRUE)
  proof_script <- sm_optional_proof_script()
  on.exit(unlink(proof_script, force = TRUE), add = TRUE)
  rscript <- file.path(R.home("bin"), "Rscript")
  proof_status <- system2(
    rscript,
    c(
      "--vanilla", shQuote(proof_script), shQuote(parsed$profile),
      shQuote(out_dir), shQuote(sm_optional_join(spec$present)),
      shQuote(sm_optional_join(spec$absent))
    )
  )
  if (!identical(proof_status, 0L)) {
    stop("Fresh-process optional namespace proof failed with status ", proof_status, ".", call. = FALSE)
  }

  audit_script <- file.path(root, "inst", "scripts", "audit-test-budget.R")
  audit_status <- system2(
    rscript,
    c(
      "--vanilla", shQuote(audit_script),
      shQuote(paste0("--profile=", parsed$profile)),
      shQuote(paste0("--out-dir=", out_dir))
    )
  )
  if (!identical(audit_status, 0L)) {
    stop("Optional-negative test budget failed with status ", audit_status, ".", call. = FALSE)
  }

  cat(
    "optional-dependency isolation PASS; profile=", parsed$profile,
    "; proof=", file.path(out_dir, "optional-namespace-proof.csv"),
    "; audit=", file.path(out_dir, "test-summary.csv"), "\n",
    sep = ""
  )
  0L
}

sm_optional_main <- function() {
  parsed <- sm_optional_parse_args(commandArgs(trailingOnly = TRUE))
  sm_optional_run(parsed)
}

exit_status <- tryCatch(
  sm_optional_main(),
  error = function(error) {
    message("optional-dependency isolation ERROR: ", conditionMessage(error))
    2L
  }
)
quit(save = "no", status = exit_status, runLast = FALSE)
