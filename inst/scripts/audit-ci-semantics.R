#!/usr/bin/env Rscript

# Audit GitHub Actions semantics against the frozen Step 7.6 contracts.
# This script is read-only with respect to the repository. Evidence is assembled
# in a sibling staging directory and atomically renamed into a fresh target.

.ci_semantics_profiles <- c(
  "gate", "negative-threshold", "negative-deploy", "negative-floor",
  "negative-matrix", "negative-yaml", "negative-shell", "negative-r",
  "self-test"
)

.ci_semantics_protected_sha256 <- c(
  "inst/scripts/build-regression-baselines.R" =
    "29e8909b541af31ff47042591b462bd745c8b172bf6574a4ee6a90ced050acb1",
  "tests/testthat/_data/regression/regression-baselines.rds" =
    "be0527f9357aa7cbb0c014a9b0ce8e60e15252b5270fad5bb99113106f9e075b",
  "tests/testthat/_snaps/output-schema.md" =
    "ed838cde596fba9618627826af12e5e5b286fa633076474bc9e47f6824885c8e"
)

ci_semantics_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (!length(file_arg)) {
    return(normalizePath(".", mustWork = TRUE))
  }
  script <- sub("^--file=", "", file_arg[[1L]])
  normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE)
}

ci_semantics_arg_value <- function(args, key, default = NULL) {
  prefix <- paste0("--", key, "=")
  found <- startsWith(args, prefix)
  if (sum(found) > 1L) {
    stop("Duplicate argument `--", key, "`.", call. = FALSE)
  }
  if (!any(found)) {
    return(default)
  }
  value <- substring(args[found], nchar(prefix) + 1L)
  if (!nzchar(value)) {
    stop("Argument `--", key, "` requires a value.", call. = FALSE)
  }
  value
}

ci_semantics_validate_args <- function(args) {
  known <- args == "--self-test" | grepl("^--(profile|out-dir)=.+$", args)
  if (any(!known)) {
    stop(
      "Unknown or malformed arguments: ", paste(args[!known], collapse = ", "),
      call. = FALSE
    )
  }
  if (sum(args == "--self-test") > 1L ||
        (any(args == "--self-test") && any(startsWith(args, "--profile=")))) {
    stop("`--self-test` and `--profile` are mutually exclusive.", call. = FALSE)
  }
  profile <- ci_semantics_profile(args)
  if (!profile %in% .ci_semantics_profiles) {
    stop(
      "`--profile` must be one of: ",
      paste(.ci_semantics_profiles, collapse = ", "), ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

ci_semantics_profile <- function(args) {
  if (any(args == "--self-test")) {
    "self-test"
  } else {
    ci_semantics_arg_value(args, "profile", "gate")
  }
}

ci_semantics_read_csv <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
}

ci_semantics_sha256 <- function(paths) {
  paths <- as.character(paths)
  sha256sum <- get0(
    "sha256sum",
    envir = asNamespace("tools"),
    mode = "function",
    inherits = FALSE
  )
  if (is.function(sha256sum)) {
    return(unname(sha256sum(paths)))
  }
  vapply(paths, function(path) {
    output <- system2(
      "shasum", c("-a", "256", path), stdout = TRUE, stderr = TRUE
    )
    status <- attr(output, "status")
    if (!is.null(status) && status != 0L) {
      stop("Could not calculate SHA-256 for `", path, "`.", call. = FALSE)
    }
    strsplit(output[[1L]], "[[:space:]]+")[[1L]][[1L]]
  }, character(1))
}

ci_semantics_empty_issues <- function() {
  data.frame(
    component = character(), severity = character(), code = character(),
    path = character(), detail = character(), stringsAsFactors = FALSE
  )
}

ci_semantics_empty_syntax <- function() {
  data.frame(
    workflow = character(), job_id = character(), step = character(),
    syntax = character(), status = character(), detail = character(),
    stringsAsFactors = FALSE
  )
}

ci_semantics_empty_budget <- function() {
  data.frame(
    gate = character(), source = character(), expected = character(),
    observed = character(), pass = logical(), stringsAsFactors = FALSE
  )
}

ci_semantics_empty_self_tests <- function() {
  data.frame(
    profile = character(), target_code = character(), detected = logical(),
    baseline_clean = logical(), repository_modified = logical(),
    pass = logical(), stringsAsFactors = FALSE
  )
}

ci_semantics_add_issue <- function(issues, component, code, path, detail,
                                   severity = "P1") {
  rbind(
    issues,
    data.frame(
      component = component, severity = severity, code = code,
      path = path, detail = detail, stringsAsFactors = FALSE
    )
  )
}

ci_semantics_split_markers <- function(value) {
  if (!length(value) || is.na(value) || !nzchar(value)) {
    return(character())
  }
  strsplit(value, "|", fixed = TRUE)[[1L]]
}

ci_semantics_contracts <- function(root) {
  job_path <- file.path(root, "inst", "gates", "ci-job-contract.csv")
  pin_path <- file.path(root, "inst", "gates", "ci-action-pins.csv")
  jobs <- ci_semantics_read_csv(job_path)
  pins <- ci_semantics_read_csv(pin_path)
  job_columns <- c(
    "workflow", "job_id", "role", "runner_policy", "r_policy",
    "blocking_policy", "timeout_minutes", "session_required",
    "artifact_required", "artifact_retention_days", "required_markers"
  )
  pin_columns <- c(
    "action", "sha", "release", "allowed_subactions", "approved_utc"
  )
  if (!identical(names(jobs), job_columns) || nrow(jobs) != 8L) {
    stop("CI job contract schema or row count drifted.", call. = FALSE)
  }
  if (!identical(names(pins), pin_columns) || nrow(pins) != 3L) {
    stop("CI action pin schema or row count drifted.", call. = FALSE)
  }
  if (anyDuplicated(jobs[c("workflow", "job_id")]) ||
        anyDuplicated(pins$action)) {
    stop("CI contracts contain duplicate identities.", call. = FALSE)
  }
  if (anyNA(jobs) || anyNA(pins) ||
        any(!vapply(jobs, function(x) all(nzchar(as.character(x))), logical(1))) ||
        any(!vapply(pins, function(x) all(nzchar(as.character(x))), logical(1)))) {
    stop("CI contracts contain missing or empty fields.", call. = FALSE)
  }
  expected_jobs <- c(
    "R-CMD-check", "minimum-R", "dependency-floor", "optional-negative",
    "package-quality", "coverage", "performance", "documentation"
  )
  if (!setequal(jobs$job_id, expected_jobs) ||
    !identical(sum(jobs$workflow == ".github/workflows/pkgdown.yaml"), 1L) ||
    !identical(
      jobs$job_id[jobs$workflow == ".github/workflows/pkgdown.yaml"],
      "documentation"
    )) {
    stop("CI job identities disagree with the frozen eight-job set.", call. = FALSE)
  }
  if (any(jobs$timeout_minutes <= 0L) ||
        any(jobs$artifact_retention_days != 14L) ||
        any(!jobs$session_required) || any(!jobs$artifact_required)) {
    stop("CI timeout/session/artifact contract drifted.", call. = FALSE)
  }
  if (any(!grepl("^[0-9a-f]{40}$", pins$sha))) {
    stop("Every CI action must have one full lower-case commit SHA.", call. = FALSE)
  }
  list(jobs = jobs, pins = pins, job_path = job_path, pin_path = pin_path)
}

ci_semantics_workflow <- function(root, relative, override = NULL) {
  path <- file.path(root, relative)
  text <- if (is.null(override)) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  } else {
    override
  }
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  error <- NULL
  parsed <- tryCatch(
    yaml::yaml.load(text, eval.expr = FALSE),
    error = function(condition) {
      error <<- conditionMessage(condition)
      NULL
    }
  )
  list(path = relative, text = text, lines = lines, parsed = parsed, error = error)
}

ci_semantics_job_blocks <- function(lines) {
  jobs_line <- which(trimws(lines) == "jobs:")
  if (length(jobs_line) != 1L) {
    return(list())
  }
  candidate <- grep("^  [A-Za-z0-9][A-Za-z0-9_-]*:[[:space:]]*$", lines)
  candidate <- candidate[candidate > jobs_line]
  if (!length(candidate)) {
    return(list())
  }
  ids <- sub("^  ([A-Za-z0-9][A-Za-z0-9_-]*):[[:space:]]*$", "\\1", lines[candidate])
  out <- vector("list", length(candidate))
  names(out) <- ids
  for (i in seq_along(candidate)) {
    end <- if (i < length(candidate)) candidate[[i + 1L]] - 1L else length(lines)
    out[[i]] <- paste(lines[seq.int(candidate[[i]], end)], collapse = "\n")
  }
  out
}

ci_semantics_jobs <- function(workflow) {
  if (is.null(workflow$parsed) || is.null(workflow$parsed$jobs)) {
    return(list())
  }
  workflow$parsed$jobs
}

ci_semantics_steps <- function(job) {
  if (is.null(job) || is.null(job$steps)) list() else job$steps
}

ci_semantics_chr <- function(value) {
  if (is.null(value) || !length(value)) "" else paste(as.character(value), collapse = "|")
}

ci_semantics_normalize_expressions <- function(text) {
  gsub("[$][{][{][^}]*[}][}]", "GITHUB_EXPRESSION", text, perl = TRUE)
}

ci_semantics_check_script <- function(text, kind) {
  normalized <- ci_semantics_normalize_expressions(text)
  if (identical(kind, "R")) {
    error <- tryCatch({
      parse(text = normalized)
      NULL
    }, error = identity)
    if (is.null(error)) {
      return(list(pass = TRUE, detail = "R parse passed"))
    }
    return(list(pass = FALSE, detail = conditionMessage(error)))
  }
  path <- tempfile("sitemix-ci-shell-", fileext = ".sh")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  writeLines(normalized, path, useBytes = TRUE)
  output <- suppressWarnings(system2("bash", c("-n", path), stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) status <- 0L
  list(
    pass = identical(as.integer(status), 0L),
    detail = if (!length(output)) "bash -n passed" else paste(output, collapse = " | ")
  )
}

ci_semantics_action_identity <- function(value) {
  if (!grepl("@", value, fixed = TRUE)) {
    return(list(action = value, subaction = "", sha = ""))
  }
  pieces <- strsplit(value, "@", fixed = TRUE)[[1L]]
  path <- pieces[[1L]]
  sha <- paste(pieces[-1L], collapse = "@")
  path_parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  action <- paste(path_parts[seq_len(min(2L, length(path_parts)))], collapse = "/")
  subaction <- if (identical(action, "r-lib/actions") && length(path_parts) >= 3L) {
    paste(path_parts[-c(1L, 2L)], collapse = "/")
  } else if (length(path_parts) >= 2L) {
    path_parts[[2L]]
  } else {
    ""
  }
  list(action = action, subaction = subaction, sha = sha)
}

ci_semantics_active_text <- function(workflows) {
  lines <- unlist(lapply(workflows, function(workflow) workflow$lines), use.names = FALSE)
  lines <- lines[!grepl("^[[:space:]]*#", lines)]
  lines <- sub("[[:space:]]+#.*$", "", lines)
  paste(lines, collapse = "\n")
}

ci_semantics_source_manifest <- function(root) {
  candidates <- c(
    ".github/workflows/R-CMD-check.yaml", ".github/workflows/pkgdown.yaml",
    ".github/dependabot.yml",
    "inst/gates/ci-job-contract.csv", "inst/gates/ci-action-pins.csv",
    "inst/gates/coverage-gate-budget.csv",
    "inst/gates/documentation-qa-budget.csv",
    "inst/gates/performance-budget.csv", "inst/gates/performance-runtime.csv",
    "inst/gates/test-timing-budget.csv",
    "tests/testthat/_data/test-architecture/job-skip-budget.csv",
    "inst/scripts/audit-ci-semantics.R", "inst/scripts/audit-test-budget.R",
    "inst/scripts/audit-coverage-gate.R",
    "inst/scripts/audit-documentation-qa.R",
    "inst/bench/performance-contract-self-test.R",
    "inst/bench/performance-smoke.R",
    "inst/gates/dependency-floor.csv",
    "inst/scripts/audit-dependency-floor.R",
    "tests/testthat/test-test-architecture.R",
    names(.ci_semantics_protected_sha256)
  )
  paths <- file.path(root, candidates)
  exists <- file.exists(paths)
  paths <- paths[exists]
  relative <- candidates[exists]
  data.frame(
    path = relative,
    size_bytes = unname(file.info(paths)$size),
    md5 = unname(tools::md5sum(paths)),
    sha256 = ci_semantics_sha256(paths),
    stringsAsFactors = FALSE
  )
}

ci_semantics_audit <- function(root, overrides = list(), floor_override = NULL,
                               synthetic = list()) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("CI semantics audit requires package `yaml`.", call. = FALSE)
  }
  contracts <- ci_semantics_contracts(root)
  workflow_paths <- unique(contracts$jobs$workflow)
  workflows <- setNames(lapply(workflow_paths, function(relative) {
    ci_semantics_workflow(root, relative, overrides[[relative]])
  }), workflow_paths)
  issues <- ci_semantics_empty_issues()
  syntax <- ci_semantics_empty_syntax()
  budget_wiring <- ci_semantics_empty_budget()

  add_issue <- function(component, code, path, detail, severity = "P1") {
    issues <<- ci_semantics_add_issue(
      issues, component, code, path, detail, severity
    )
  }
  add_budget <- function(gate, source, expected, observed, pass) {
    budget_wiring <<- rbind(
      budget_wiring,
      data.frame(
        gate = gate, source = source, expected = as.character(expected),
        observed = as.character(observed), pass = isTRUE(pass),
        stringsAsFactors = FALSE
      )
    )
    if (!isTRUE(pass)) {
      add_issue("budget_wiring", "budget_wiring", source, paste0(
        gate, ": expected `", expected, "`, observed `", observed, "`."
      ))
    }
  }

  for (relative in names(workflows)) {
    workflow <- workflows[[relative]]
    pass <- is.null(workflow$error)
    syntax <- rbind(
      syntax,
      data.frame(
        workflow = relative, job_id = "", step = "", syntax = "YAML",
        status = if (pass) "PASS" else "FAIL",
        detail = if (pass) "yaml::yaml.load passed" else workflow$error,
        stringsAsFactors = FALSE
      )
    )
    if (!pass) {
      add_issue("syntax", "yaml_parse", relative, workflow$error, "P0")
    }
  }

  dependabot_path <- file.path(root, ".github", "dependabot.yml")
  dependabot <- tryCatch(
    yaml::read_yaml(dependabot_path),
    error = identity
  )
  dependabot_pass <- !inherits(dependabot, "error") &&
    identical(dependabot$version, 2L) &&
    is.list(dependabot$updates) && length(dependabot$updates) == 1L &&
    identical(
      dependabot$updates[[1L]][["package-ecosystem"]],
      "github-actions"
    ) &&
    identical(dependabot$updates[[1L]]$directory, "/") &&
    identical(dependabot$updates[[1L]]$schedule$interval, "weekly")
  if (!dependabot_pass) {
    add_issue(
      "dependabot", "actions_update_policy", ".github/dependabot.yml",
      "Dependabot must define one weekly GitHub Actions update policy."
    )
  }

  job_rows <- vector("list", nrow(contracts$jobs))
  for (i in seq_len(nrow(contracts$jobs))) {
    contract <- contracts$jobs[i, , drop = FALSE]
    workflow <- workflows[[contract$workflow]]
    parsed_jobs <- ci_semantics_jobs(workflow)
    blocks <- ci_semantics_job_blocks(workflow$lines)
    present <- contract$job_id %in% names(parsed_jobs) &&
      contract$job_id %in% names(blocks)
    job <- if (present) parsed_jobs[[contract$job_id]] else NULL
    block <- if (present) blocks[[contract$job_id]] else ""
    runner <- if (present) ci_semantics_chr(job[["runs-on"]]) else ""
    timeout <- if (present) suppressWarnings(as.integer(job[["timeout-minutes"]])) else NA_integer_
    steps <- ci_semantics_steps(job)
    setup_r_versions <- unique(vapply(steps, function(step) {
      identity <- ci_semantics_action_identity(ci_semantics_chr(step$uses))
      if (identical(identity$action, "r-lib/actions") &&
            identical(identity$subaction, "setup-r")) {
        ci_semantics_chr(step$with[["r-version"]])
      } else {
        ""
      }
    }, character(1)))
    setup_r_versions <- setup_r_versions[nzchar(setup_r_versions)]
    observed_r_policy <- paste(setup_r_versions, collapse = "|")
    r_policy_pass <- if (identical(contract$r_policy, "release-devel-oldrel")) {
      length(setup_r_versions) == 1L &&
        grepl("matrix.config.r", setup_r_versions, fixed = TRUE)
    } else {
      identical(setup_r_versions, contract$r_policy)
    }
    step_continue <- vapply(steps, function(step) {
      value <- step[["continue-on-error"]]
      !is.null(value) && !identical(value, FALSE) &&
        !identical(as.character(value), "false")
    }, logical(1))
    allows_advisory <- identical(
      contract$blocking_policy, "devel-step-advisory"
    )
    blocking_pass <- is.null(job[["continue-on-error"]]) &&
      (allows_advisory || !any(step_continue))
    runner_pass <- if (identical(contract$runner_policy, "matrix.config.os")) {
      grepl("matrix.config.os", runner, fixed = TRUE)
    } else {
      identical(runner, contract$runner_policy)
    }
    timeout_pass <- identical(timeout, as.integer(contract$timeout_minutes))
    markers <- ci_semantics_split_markers(contract$required_markers)
    missing_markers <- markers[!vapply(markers, grepl, logical(1), x = block, fixed = TRUE)]
    session_pass <- !isTRUE(contract$session_required) ||
      grepl("session-info|sessionInfo[(]", block, perl = TRUE)
    artifact_pass <- !isTRUE(contract$artifact_required) ||
      (grepl("actions/upload-artifact@", block, fixed = TRUE) &&
        grepl("always[(][)]", block, perl = TRUE) &&
        grepl(
          paste0("retention-days:[[:space:]]*", contract$artifact_retention_days),
          block, perl = TRUE
        ))
    job_pass <- present && runner_pass && r_policy_pass && blocking_pass &&
      timeout_pass &&
      !length(missing_markers) && session_pass && artifact_pass
    if (!present) {
      add_issue("jobs", "job_set", contract$workflow, paste0(
        "Missing contracted job `", contract$job_id, "`."
      ), "P0")
    }
    if (present && !runner_pass) {
      add_issue("jobs", "runner_policy", contract$job_id, paste0(
        "Expected runner policy `", contract$runner_policy,
        "`, observed `", runner, "`."
      ))
    }
    if (present && !r_policy_pass) {
      add_issue("jobs", "r_policy", contract$job_id, paste0(
        "Expected R policy `", contract$r_policy,
        "`, observed `", observed_r_policy, "`."
      ))
    }
    if (present && !blocking_pass) {
      add_issue("jobs", "blocking_policy", contract$job_id,
                "Job or step-level continue-on-error violates its blocking policy.",
                "P0")
    }
    if (present && !timeout_pass) {
      add_issue("jobs", "timeout_policy", contract$job_id, paste0(
        "Expected timeout ", contract$timeout_minutes,
        ", observed ", ifelse(is.na(timeout), "missing", timeout), "."
      ))
    }
    if (length(missing_markers)) {
      add_issue("jobs", "required_marker", contract$job_id, paste0(
        "Missing markers: ", paste(missing_markers, collapse = "; ")
      ))
    }
    if (present && !session_pass) {
      add_issue("artifacts", "session_evidence", contract$job_id,
                "Job does not retain session information.")
    }
    if (present && !artifact_pass) {
      add_issue("artifacts", "artifact_policy", contract$job_id,
                "Job lacks always-uploaded 14-day artifacts.")
    }
    job_rows[[i]] <- cbind(
      contract,
      data.frame(
        present = present, observed_runner = runner,
        observed_r_policy = observed_r_policy,
        r_policy_pass = r_policy_pass, blocking_pass = blocking_pass,
        observed_timeout_minutes = timeout,
        missing_markers = paste(missing_markers, collapse = "|"),
        session_pass = session_pass, artifact_pass = artifact_pass,
        pass = job_pass, stringsAsFactors = FALSE
      )
    )
  }
  jobs <- do.call(rbind, job_rows)
  row.names(jobs) <- NULL

  for (relative in names(workflows)) {
    observed <- names(ci_semantics_jobs(workflows[[relative]]))
    expected <- contracts$jobs$job_id[contracts$jobs$workflow == relative]
    extras <- setdiff(observed, expected)
    if (length(extras)) {
      add_issue("jobs", "job_set", relative, paste0(
        "Uncontracted jobs: ", paste(extras, collapse = ", ")
      ), "P0")
    }
  }

  action_rows <- list()
  action_i <- 0L
  for (relative in names(workflows)) {
    for (job_id in names(ci_semantics_jobs(workflows[[relative]]))) {
      steps <- ci_semantics_steps(ci_semantics_jobs(workflows[[relative]])[[job_id]])
      for (step_i in seq_along(steps)) {
        uses <- ci_semantics_chr(steps[[step_i]]$uses)
        if (!nzchar(uses)) next
        action_i <- action_i + 1L
        identity <- ci_semantics_action_identity(uses)
        pin_index <- match(identity$action, contracts$pins$action)
        known <- !is.na(pin_index)
        allowed <- if (known) {
          identity$subaction %in% ci_semantics_split_markers(
            contracts$pins$allowed_subactions[[pin_index]]
          )
        } else {
          FALSE
        }
        sha_pass <- known && identical(
          identity$sha, contracts$pins$sha[[pin_index]]
        )
        checkout_security <- !identical(identity$action, "actions/checkout") ||
          identical(steps[[step_i]]$with[["persist-credentials"]], FALSE) ||
          identical(
            as.character(steps[[step_i]]$with[["persist-credentials"]]),
            "false"
          )
        pass <- known && allowed && sha_pass && checkout_security
        if (!known) {
          add_issue("actions", "action_pin", relative, paste0(
            "Unapproved action `", uses, "`."
          ), "P0")
        } else if (!allowed) {
          add_issue("actions", "action_subaction", relative, paste0(
            "Unapproved subaction `", identity$subaction, "` for `",
            identity$action, "`."
          ))
        } else if (!sha_pass) {
          add_issue("actions", "action_pin", relative, paste0(
            "Action `", identity$action, "` is not pinned to `",
            contracts$pins$sha[[pin_index]], "`."
          ), "P0")
        } else if (!checkout_security) {
          add_issue("actions", "checkout_credentials", relative,
                    "Checkout must set `persist-credentials: false`.", "P0")
        }
        action_rows[[action_i]] <- data.frame(
          workflow = relative, job_id = job_id,
          step = ci_semantics_chr(steps[[step_i]]$name), uses = uses,
          action = identity$action, subaction = identity$subaction,
          observed_sha = identity$sha,
          expected_sha = if (known) contracts$pins$sha[[pin_index]] else "",
          known = known, allowed_subaction = allowed, pin_exact = sha_pass,
          checkout_security = checkout_security,
          pass = pass, stringsAsFactors = FALSE
        )
      }
    }
  }
  actions <- if (length(action_rows)) do.call(rbind, action_rows) else data.frame(
    workflow = character(), job_id = character(), step = character(),
    uses = character(), action = character(), subaction = character(),
    observed_sha = character(), expected_sha = character(), known = logical(),
    allowed_subaction = logical(), pin_exact = logical(),
    checkout_security = logical(), pass = logical(),
    stringsAsFactors = FALSE
  )
  for (action in contracts$pins$action) {
    if (!action %in% actions$action) {
      add_issue("actions", "action_pin_unused", contracts$pin_path, paste0(
        "Approved action `", action, "` is not used."
      ))
    }
  }

  for (relative in names(workflows)) {
    parsed_jobs <- ci_semantics_jobs(workflows[[relative]])
    for (job_id in names(parsed_jobs)) {
      steps <- ci_semantics_steps(parsed_jobs[[job_id]])
      for (step_i in seq_along(steps)) {
        run <- ci_semantics_chr(steps[[step_i]]$run)
        shell <- ci_semantics_chr(steps[[step_i]]$shell)
        name <- ci_semantics_chr(steps[[step_i]]$name)
        kind <- if (startsWith(shell, "Rscript")) {
          "R"
        } else if (startsWith(shell, "bash")) {
          "bash"
        } else if (!nzchar(shell)) {
          "bash"
        } else {
          ""
        }
        if (!nzchar(run) || !nzchar(kind)) next
        checked <- ci_semantics_check_script(run, kind)
        syntax <- rbind(
          syntax,
          data.frame(
            workflow = relative, job_id = job_id, step = name,
            syntax = kind, status = if (checked$pass) "PASS" else "FAIL",
            detail = checked$detail, stringsAsFactors = FALSE
          )
        )
        if (!checked$pass) {
          add_issue(
            "syntax", if (identical(kind, "R")) "r_parse" else "shell_parse",
            paste0(relative, "::", job_id, "::", name), checked$detail, "P0"
          )
        }
      }
    }
  }
  for (kind in intersect(names(synthetic), c("shell", "r"))) {
    syntax_kind <- if (identical(kind, "r")) "R" else "bash"
    checked <- ci_semantics_check_script(synthetic[[kind]], syntax_kind)
    syntax <- rbind(
      syntax,
      data.frame(
        workflow = "<negative-fixture>", job_id = "fixture",
        step = paste0("negative-", kind), syntax = syntax_kind,
        status = if (checked$pass) "PASS" else "FAIL",
        detail = checked$detail, stringsAsFactors = FALSE
      )
    )
    if (!checked$pass) {
      add_issue(
        "syntax", if (identical(kind, "r")) "r_parse" else "shell_parse",
        paste0("negative-", kind), checked$detail, "P0"
      )
    }
  }

  active <- ci_semantics_active_text(workflows)
  check_workflow <- workflows[[".github/workflows/R-CMD-check.yaml"]]
  docs_workflow <- workflows[[".github/workflows/pkgdown.yaml"]]
  for (relative in names(workflows)) {
    parsed <- workflows[[relative]]$parsed
    permissions <- if (is.null(parsed)) NULL else parsed$permissions
    permission_values <- if (is.list(permissions)) {
      unlist(permissions, use.names = TRUE)
    } else {
      character()
    }
    permissions_pass <- length(permission_values) &&
      identical(unname(permission_values[["contents"]]), "read") &&
      all(unname(permission_values) %in% c("read", "none"))
    if (!permissions_pass) {
      add_issue("permissions", "write_permission", relative,
                "Workflow permissions are not explicitly read-only.", "P0")
    }
    concurrency <- if (is.null(parsed)) NULL else parsed$concurrency
    concurrency_pass <- is.list(concurrency) &&
      isTRUE(concurrency[["cancel-in-progress"]]) &&
      grepl("github.workflow", ci_semantics_chr(concurrency$group), fixed = TRUE)
    if (!concurrency_pass) {
      add_issue("concurrency", "concurrency_policy", relative,
                "Workflow lacks workflow-scoped concurrency with cancellation.")
    }
  }

  forbidden <- c(
    "contents:[[:space:]]*write" = "write_permission",
    "gh-pages" = "deployment_surface",
    "github-pages-deploy" = "deployment_surface",
    "JamesIves" = "deployment_surface",
    "build_site_github_pages" = "deployment_surface",
    "pkgdown::build_site" = "uncertified_pkgdown_build",
    "ebrecipe|as_eb_input|eb_handoff" = "consumer_coupling",
    "build-regression-baselines[.]R" = "numeric_builder"
  )
  for (pattern in names(forbidden)) {
    if (grepl(pattern, active, ignore.case = TRUE, perl = TRUE)) {
      add_issue(
        "forbidden_surface", forbidden[[pattern]], ".github/workflows",
        paste0("Active workflow text matches forbidden pattern `", pattern, "`."),
        "P0"
      )
    }
  }

  check_text <- check_workflow$text
  strict_count <- sum(gregexpr('error_on = "note"', check_text, fixed = TRUE)[[1L]] > 0L)
  if (!identical(strict_count, 1L) ||
        grepl('error_on = "error"|error_on = "warning"|error_on = "never"',
              check_text, perl = TRUE)) {
    add_issue("check", "check_threshold", check_workflow$path,
              "Blocking check policy must contain exactly one `error_on = \"note\"`.", "P0")
  }
  check_jobs <- ci_semantics_jobs(check_workflow)
  check_job <- check_jobs[["R-CMD-check"]]
  if (!is.null(check_job) && !is.null(check_job[["continue-on-error"]])) {
    add_issue("check", "job_advisory", "R-CMD-check",
              "Advisory behavior must not be set at job level.", "P0")
  }
  check_steps <- ci_semantics_steps(check_job)
  rcmd_index <- which(vapply(check_steps, function(step) {
    grepl("rcmdcheck::rcmdcheck", ci_semantics_chr(step$run), fixed = TRUE)
  }, logical(1)))
  advisory_pass <- length(rcmd_index) == 1L &&
    grepl("matrix.config.advisory", ci_semantics_chr(
      check_steps[[rcmd_index]] [["continue-on-error"]]
    ), fixed = TRUE)
  outcome_pass <- if (length(rcmd_index) == 1L) {
    step_id <- ci_semantics_chr(check_steps[[rcmd_index]]$id)
    nzchar(step_id) && grepl(
      paste0("steps.", step_id, ".outcome"), check_text, fixed = TRUE
    ) && grepl("advisory", check_text, fixed = TRUE)
  } else {
    FALSE
  }
  if (!advisory_pass || !outcome_pass) {
    add_issue("check", "step_advisory", "R-CMD-check",
              "R-devel must use step-only advisory handling and retain its outcome.", "P0")
  }
  matrix <- if (is.null(check_job)) NULL else check_job$strategy$matrix$config
  expected_matrix <- data.frame(
    os = c(
      "ubuntu-latest", "ubuntu-latest", "ubuntu-latest",
      "macos-latest", "windows-latest"
    ),
    r = c("release", "devel", "oldrel-1", "release", "release"),
    profile = c(
      "rcmd-ubuntu-release", "rcmd-ubuntu-devel", "rcmd-ubuntu-oldrel",
      "rcmd-macos-release", "rcmd-windows-release"
    ),
    advisory = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    manual = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  observed_matrix <- if (is.list(matrix) && length(matrix) == 5L) {
    do.call(rbind, lapply(matrix, function(row) {
      data.frame(
        os = ci_semantics_chr(row$os),
        r = ci_semantics_chr(row$r),
        profile = ci_semantics_chr(row$profile),
        advisory = isTRUE(row$advisory),
        manual = isTRUE(row$manual),
        stringsAsFactors = FALSE
      )
    }))
  } else {
    NULL
  }
  if (!is.null(observed_matrix)) {
    row.names(observed_matrix) <- NULL
  }
  matrix_pass <- identical(observed_matrix, expected_matrix)
  if (!matrix_pass) {
    add_issue(
      "check", "check_matrix", "R-CMD-check",
      "The five OS/R/profile/advisory/manual tuples must match the frozen matrix.",
      "P0"
    )
  }
  tinytex_pass <- grepl("setup-tinytex@", check_text, fixed = TRUE) &&
    grepl("matrix.config.manual", check_text, fixed = TRUE) &&
    grepl("--no-manual", check_text, fixed = TRUE)
  if (!tinytex_pass) {
    add_issue("check", "manual_policy", "R-CMD-check",
              "Exactly one current-Ubuntu release cell must build the PDF manual.", "P0")
  }

  all_setup_dependencies <- actions[
    actions$action == "r-lib/actions" &
      actions$subaction == "setup-r-dependencies",
    ,
    drop = FALSE
  ]
  install_quarto_failures <- 0L
  pandoc_checks <- 0L
  pandoc_failures <- 0L
  for (relative in names(workflows)) {
    for (job_id in names(ci_semantics_jobs(workflows[[relative]]))) {
      for (step in ci_semantics_steps(ci_semantics_jobs(workflows[[relative]])[[job_id]])) {
        identity <- ci_semantics_action_identity(ci_semantics_chr(step$uses))
        if (identical(identity$action, "r-lib/actions") &&
              identical(identity$subaction, "setup-r-dependencies")) {
          value <- step$with[["install-quarto"]]
          if (!identical(value, FALSE) && !identical(as.character(value), "false")) {
            install_quarto_failures <- install_quarto_failures + 1L
          }
        }
        if (identical(identity$action, "r-lib/actions") &&
              identical(identity$subaction, "setup-pandoc")) {
          pandoc_checks <- pandoc_checks + 1L
          if (!identical(as.character(step$with[["pandoc-version"]]), "3.8.3")) {
            pandoc_failures <- pandoc_failures + 1L
          }
        }
      }
    }
  }
  if (nrow(all_setup_dependencies) == 0L || install_quarto_failures > 0L) {
    add_issue("toolchain", "quarto_policy", ".github/workflows",
              "Every setup-r-dependencies step must set `install-quarto: false`.")
  }
  if (pandoc_checks == 0L || pandoc_failures > 0L) {
    add_issue("toolchain", "pandoc_policy", ".github/workflows",
              "Every setup-pandoc step must pin Pandoc 3.8.3.")
  }

  docs_text <- docs_workflow$text
  docs_normalized <- gsub("[[:space:]]+", " ", docs_text)
  docs_command_pass <- grepl(
    "audit-documentation-qa.R --profile=gate --url-mode=online",
    docs_normalized, fixed = TRUE
  )
  docs_dependencies <- c(
    "any::curl", "any::knitr", "any::pkgdown", "any::pkgload",
    "any::rmarkdown", "any::roxygen2", "any::spelling",
    "any::urlchecker", "any::xml2", "any::yaml", "any::mgcv"
  )
  docs_dependency_pass <- all(vapply(
    docs_dependencies, grepl, logical(1), x = docs_text, fixed = TRUE
  ))
  docs_trigger_pass <- all(vapply(
    c("push:", "pull_request:", "workflow_dispatch:"),
    grepl, logical(1), x = docs_text, fixed = TRUE
  ))
  if (!docs_command_pass || !docs_dependency_pass || !docs_trigger_pass) {
    add_issue("documentation", "documentation_wiring", docs_workflow$path,
              "Documentation must use the online unified gate with explicit dependencies on push/PR/manual.", "P0")
  }

  role_exclusions <- list(
    "package-quality" = c("audit-coverage-gate.R", "performance-smoke.R", "audit-documentation-qa.R"),
    "coverage" = c("performance-smoke.R", "audit-documentation-qa.R"),
    "performance" = c("audit-coverage-gate.R", "audit-documentation-qa.R")
  )
  check_blocks <- ci_semantics_job_blocks(check_workflow$lines)
  for (job_id in names(role_exclusions)) {
    block <- check_blocks[[job_id]]
    if (!is.null(block)) {
      hits <- role_exclusions[[job_id]][vapply(
        role_exclusions[[job_id]], grepl, logical(1), x = block, fixed = TRUE
      )]
      if (length(hits)) {
        add_issue("jobs", "role_overlap", job_id, paste0(
          "Role contains commands owned by another job: ", paste(hits, collapse = ", ")
        ))
      }
    }
  }

  skip_path <- file.path(
    root, "tests", "testthat", "_data", "test-architecture",
    "job-skip-budget.csv"
  )
  skip_budget <- ci_semantics_read_csv(skip_path)
  wired <- skip_budget$profile[skip_budget$gate_wired]
  observed_wired <- wired[vapply(wired, grepl, logical(1), x = check_text, fixed = TRUE)]
  add_budget(
    "test_skip_profiles", "job-skip-budget.csv",
    paste(sort(wired), collapse = "|"), paste(sort(observed_wired), collapse = "|"),
    setequal(wired, observed_wired)
  )
  ordinary <- skip_budget$dependency_mode == "full"
  ordinary_pass <- all(skip_budget$expected_skips[ordinary] == 0L) &&
    all(skip_budget$allowed_skip_ids[ordinary] == "none")
  add_budget("ordinary_expected_skips", "job-skip-budget.csv", "0", paste(
    unique(skip_budget$expected_skips[ordinary]), collapse = "|"
  ), ordinary_pass)

  timing_path <- file.path(root, "inst", "gates", "test-timing-budget.csv")
  timing <- ci_semantics_read_csv(timing_path)
  timing_wired <- timing$profile[timing$gate_wired]
  add_budget(
    "timing_profiles", "test-timing-budget.csv",
    paste(sort(wired), collapse = "|"), paste(sort(timing_wired), collapse = "|"),
    setequal(wired, timing_wired)
  )

  coverage_path <- file.path(root, "inst", "gates", "coverage-gate-budget.csv")
  coverage <- ci_semantics_read_csv(coverage_path)
  coverage_pass <- nrow(coverage) == 1L && isTRUE(coverage$blocking[[1L]]) &&
    isTRUE(coverage$gate_wired[[1L]]) &&
    grepl("audit-coverage-gate.R", check_blocks[["coverage"]], fixed = TRUE)
  add_budget("coverage", "coverage-gate-budget.csv", "blocking+wired", paste(
    coverage$blocking[[1L]], coverage$gate_wired[[1L]], sep = "|"
  ), coverage_pass)

  docs_budget_path <- file.path(root, "inst", "gates", "documentation-qa-budget.csv")
  docs_budget <- ci_semantics_read_csv(docs_budget_path)
  add_budget(
    "documentation", "documentation-qa-budget.csv", "30 metrics + online gate",
    paste0(nrow(docs_budget), " metrics"),
    nrow(docs_budget) == 30L && docs_command_pass
  )

  performance_path <- file.path(root, "inst", "gates", "performance-budget.csv")
  performance <- ci_semantics_read_csv(performance_path)
  perf_block <- check_blocks[["performance"]]
  perf_pass <- nrow(performance) == 6L &&
    grepl("performance-contract-self-test.R", perf_block, fixed = TRUE) &&
    grepl("--profile=ci-smoke", perf_block, fixed = TRUE)
  add_budget("performance", "performance-budget.csv", "6 cases + ci-smoke", paste0(
    nrow(performance), " cases"
  ), perf_pass)

  floor_path <- file.path(root, "inst", "gates", "dependency-floor.csv")
  floor <- if (!is.null(floor_override)) {
    floor_override
  } else if (file.exists(floor_path)) {
    ci_semantics_read_csv(floor_path)
  } else {
    NULL
  }
  if (!is.null(floor)) {
    floor_columns <- c(
      "package", "description_field", "description_constraint",
      "installed_version_policy", "expected_installed_version",
      "runtime_profile", "rationale"
    )
    exact <- if (identical(names(floor), floor_columns)) {
      floor$installed_version_policy == "exact"
    } else {
      rep(FALSE, nrow(floor))
    }
    exact_versions_pass <- any(exact) && all(vapply(
      floor$expected_installed_version[exact],
      function(version) {
        !inherits(try(package_version(version), silent = TRUE), "try-error")
      },
      logical(1)
    ))
    compatible_pass <- identical(names(floor), floor_columns) &&
      identical(
        floor$package[floor$installed_version_policy == "compatible"],
        "Matrix"
      ) &&
      identical(
        floor$expected_installed_version[
          floor$installed_version_policy == "compatible"
        ],
        "r-4.5.1-compatible"
      )
    runtime_pass <- identical(names(floor), floor_columns) &&
      all(floor$runtime_profile == "r-4.5.1")
    floor_pass <- identical(names(floor), floor_columns) &&
      nrow(floor) == 5L && !anyDuplicated(floor$package) &&
      all(vapply(floor, function(column) all(nzchar(column)), logical(1))) &&
      exact_versions_pass && compatible_pass && runtime_pass
    add_budget(
      "dependency_floor", "dependency-floor.csv", "nonempty valid unique versions",
      paste0(nrow(floor), " rows"), floor_pass
    )
    if (!floor_pass) {
      add_issue("dependency_floor", "dependency_floor", "dependency-floor.csv",
                "Dependency-floor schema or version values are invalid.", "P0")
    }
  } else {
    add_budget(
      "dependency_floor", "dependency-floor.csv", "optional contract absent",
      "absent", TRUE
    )
  }

  protected_paths <- file.path(root, names(.ci_semantics_protected_sha256))
  protected <- data.frame(
    path = names(.ci_semantics_protected_sha256),
    expected_sha256 = unname(.ci_semantics_protected_sha256),
    exists = file.exists(protected_paths),
    observed_sha256 = rep("", length(protected_paths)),
    pass = FALSE,
    stringsAsFactors = FALSE
  )
  protected$observed_sha256[protected$exists] <- ci_semantics_sha256(
    protected_paths[protected$exists]
  )
  protected$pass <- protected$exists &
    protected$observed_sha256 == protected$expected_sha256
  if (any(!protected$pass)) {
    add_issue("protected", "protected_hash", "protected artifacts",
              "At least one protected artifact hash changed.", "P0")
  }

  component_names <- c(
    "jobs", "actions", "syntax", "permissions", "concurrency", "check",
    "toolchain", "documentation", "budget_wiring", "protected",
    "forbidden_surface", "dependabot"
  )
  components <- do.call(rbind, lapply(component_names, function(component) {
    count <- sum(issues$component == component)
    data.frame(
      component = component,
      status = if (count == 0L) "PASS" else "FAIL",
      detail = paste0(count, " issue(s)"), stringsAsFactors = FALSE
    )
  }))
  row.names(components) <- NULL
  list(
    issues = issues, components = components, jobs = jobs, actions = actions,
    syntax = syntax, budget_wiring = budget_wiring, protected = protected,
    contracts = contracts, workflows = workflows
  )
}

ci_semantics_negative_fixture <- function(root, profile) {
  check_path <- ".github/workflows/R-CMD-check.yaml"
  docs_path <- ".github/workflows/pkgdown.yaml"
  check_text <- paste(readLines(file.path(root, check_path), warn = FALSE), collapse = "\n")
  docs_text <- paste(readLines(file.path(root, docs_path), warn = FALSE), collapse = "\n")
  overrides <- list()
  floor_override <- NULL
  synthetic <- list()
  target <- switch(
    profile,
    "negative-threshold" = {
      overrides[[check_path]] <- sub(
        'error_on = "note"', 'error_on = "error"', check_text, fixed = TRUE
      )
      "check_threshold"
    },
    "negative-deploy" = {
      overrides[[docs_path]] <- sub(
        "contents: read", "contents: write", docs_text, fixed = TRUE
      )
      "write_permission"
    },
    "negative-floor" = {
      floor_override <- data.frame(
        package = "rlang", minimum_version = "not-a-version",
        stringsAsFactors = FALSE
      )
      "dependency_floor"
    },
    "negative-matrix" = {
      overrides[[check_path]] <- sub(
        "- {os: windows-latest, r: release, profile: rcmd-windows-release, advisory: false, manual: false}",
        "- {os: ubuntu-latest, r: release, profile: rcmd-windows-release, advisory: false, manual: false}",
        check_text,
        fixed = TRUE
      )
      "check_matrix"
    },
    "negative-yaml" = {
      overrides[[check_path]] <- paste0(check_text, "\ninvalid-fixture: [\n")
      "yaml_parse"
    },
    "negative-shell" = {
      synthetic$shell <- "if then\n  echo broken\nfi"
      "shell_parse"
    },
    "negative-r" = {
      synthetic$r <- "x <- function("
      "r_parse"
    },
    stop("Unsupported negative profile.", call. = FALSE)
  )
  list(
    target = target, overrides = overrides,
    floor_override = floor_override, synthetic = synthetic
  )
}

ci_semantics_append_source_stability <- function(result, source_before, source_after) {
  if (!identical(source_before$path, source_after$path)) {
    unchanged <- FALSE
    source <- merge(
      source_before, source_after, by = "path", all = TRUE,
      suffixes = c("_before", "_after"), sort = FALSE
    )
    source$unchanged_during_gate <- FALSE
  } else {
    source <- source_before
    names(source)[-1L] <- paste0(names(source)[-1L], "_before")
    source$size_bytes_after <- source_after$size_bytes
    source$md5_after <- source_after$md5
    source$sha256_after <- source_after$sha256
    source$unchanged_during_gate <-
      source$size_bytes_before == source$size_bytes_after &
      source$md5_before == source$md5_after &
      source$sha256_before == source$sha256_after
    unchanged <- all(source$unchanged_during_gate)
  }
  if (!unchanged) {
    result$issues <- ci_semantics_add_issue(
      result$issues, "source_stability", "source_drift", "repository",
      "CI audit inputs changed while the gate was running.", "P0"
    )
  }
  result$components <- rbind(
    result$components[result$components$component != "source_stability", , drop = FALSE],
    data.frame(
      component = "source_stability",
      status = if (unchanged) "PASS" else "FAIL",
      detail = paste0(sum(!source$unchanged_during_gate), " changed file(s)"),
      stringsAsFactors = FALSE
    )
  )
  result$source_manifest <- source
  result$repository_modified <- !unchanged
  result
}

ci_semantics_output_target <- function(root, value, profile) {
  target <- if (is.null(value)) {
    file.path(root, "ci-artifacts", paste0("ci-semantics-", profile))
  } else if (grepl("^/", value)) {
    value
  } else {
    file.path(root, value)
  }
  if (file.exists(target) || dir.exists(target)) {
    stop("CI semantics output directory must be fresh: ", target, call. = FALSE)
  }
  target
}

ci_semantics_write_csv <- function(value, path) {
  utils::write.csv(value, path, row.names = FALSE, na = "", quote = TRUE)
}

ci_semantics_write_outputs <- function(target, summary, result, self_tests) {
  parent <- dirname(target)
  dir.create(parent, recursive = TRUE, showWarnings = FALSE)
  parent <- normalizePath(parent, mustWork = TRUE)
  target <- file.path(parent, basename(target))
  staging <- file.path(
    parent,
    paste0(".", basename(target), ".staging-", Sys.getpid())
  )
  if (file.exists(staging) || dir.exists(staging)) {
    unlink(staging, recursive = TRUE, force = TRUE)
  }
  dir.create(staging, recursive = FALSE, showWarnings = FALSE)
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  outputs <- list(
    "ci-semantics-summary.csv" = summary,
    "ci-semantics-components.csv" = result$components,
    "ci-semantics-issues.csv" = result$issues,
    "ci-semantics-jobs.csv" = result$jobs,
    "ci-semantics-actions.csv" = result$actions,
    "ci-semantics-syntax.csv" = result$syntax,
    "ci-semantics-budget-wiring.csv" = result$budget_wiring,
    "ci-semantics-protected.csv" = result$protected,
    "ci-semantics-source-manifest.csv" = result$source_manifest,
    "ci-semantics-self-tests.csv" = self_tests
  )
  for (name in names(outputs)) {
    ci_semantics_write_csv(outputs[[name]], file.path(staging, name))
  }
  evidence <- list(
    summary = summary, components = result$components, issues = result$issues,
    jobs = result$jobs, actions = result$actions, syntax = result$syntax,
    budget_wiring = result$budget_wiring, protected = result$protected,
    source_manifest = result$source_manifest, self_tests = self_tests
  )
  saveRDS(evidence, file.path(staging, "ci-semantics-evidence.rds"), version = 2)
  writeLines(
    c(
      paste0("profile=", summary$profile),
      paste0("status=", summary$status),
      capture.output(utils::sessionInfo())
    ),
    file.path(staging, "session-info.txt"), useBytes = TRUE
  )
  expected <- c(names(outputs), "ci-semantics-evidence.rds", "session-info.txt")
  if (!all(file.exists(file.path(staging, expected))) ||
        any(file.info(file.path(staging, expected))$size <= 0L)) {
    stop("CI semantics staging evidence is incomplete.", call. = FALSE)
  }
  if (!file.rename(staging, target)) {
    stop("Could not atomically commit CI semantics evidence.", call. = FALSE)
  }
  target
}

ci_semantics_main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  ci_semantics_validate_args(args)
  root <- ci_semantics_root()
  profile <- ci_semantics_profile(args)
  target <- ci_semantics_output_target(
    root, ci_semantics_arg_value(args, "out-dir"), profile
  )
  started <- format(Sys.time(), tz = "UTC", usetz = TRUE)
  source_before <- ci_semantics_source_manifest(root)
  baseline <- ci_semantics_audit(root)
  self_tests <- ci_semantics_empty_self_tests()
  result <- baseline
  detector_pass <- NA

  if (identical(profile, "self-test")) {
    rows <- lapply(setdiff(.ci_semantics_profiles, c("gate", "self-test")), function(item) {
      fixture <- ci_semantics_negative_fixture(root, item)
      mutated <- ci_semantics_audit(
        root, fixture$overrides, fixture$floor_override, fixture$synthetic
      )
      detected <- fixture$target %in% mutated$issues$code
      data.frame(
        profile = item, target_code = fixture$target, detected = detected,
        baseline_clean = nrow(baseline$issues) == 0L,
        repository_modified = FALSE,
        pass = detected && nrow(baseline$issues) == 0L,
        stringsAsFactors = FALSE
      )
    })
    self_tests <- do.call(rbind, rows)
    detector_pass <- all(self_tests$pass)
    if (!detector_pass) {
      result$issues <- ci_semantics_add_issue(
        result$issues, "self_tests", "negative_self_test", "self-test",
        "At least one deliberate CI defect was not detected.", "P0"
      )
    }
  } else if (!identical(profile, "gate")) {
    fixture <- ci_semantics_negative_fixture(root, profile)
    result <- ci_semantics_audit(
      root, fixture$overrides, fixture$floor_override, fixture$synthetic
    )
    detected <- fixture$target %in% result$issues$code
    self_tests <- data.frame(
      profile = profile, target_code = fixture$target, detected = detected,
      baseline_clean = nrow(baseline$issues) == 0L,
      repository_modified = FALSE,
      pass = detected && nrow(baseline$issues) == 0L,
      stringsAsFactors = FALSE
    )
    detector_pass <- isTRUE(self_tests$pass)
  }

  source_after <- ci_semantics_source_manifest(root)
  result <- ci_semantics_append_source_stability(result, source_before, source_after)
  if (nrow(self_tests)) {
    self_tests$repository_modified <- result$repository_modified
    self_tests$pass <- self_tests$pass & !self_tests$repository_modified
    detector_pass <- all(self_tests$pass)
  }
  gate_pass <- nrow(result$issues) == 0L
  status <- if (identical(profile, "gate")) {
    if (gate_pass) "PASS" else "FAIL"
  } else {
    if (isTRUE(detector_pass)) "PASS" else "FAIL"
  }
  summary <- data.frame(
    status = status, profile = profile,
    workflow_files = 2L,
    contract_jobs = nrow(result$jobs),
    observed_jobs = sum(result$jobs$present),
    observed_actions = nrow(result$actions),
    syntax_checks = nrow(result$syntax),
    budget_checks = nrow(result$budget_wiring),
    protected_files = nrow(result$protected),
    issue_count = nrow(result$issues),
    detector_findings = if (nrow(self_tests)) sum(self_tests$detected) else 0L,
    repository_modified = result$repository_modified,
    started_utc = started,
    completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    stringsAsFactors = FALSE
  )
  artifact_dir <- ci_semantics_write_outputs(
    target, summary, result, self_tests
  )
  print(summary, row.names = FALSE)
  cat("Artifacts: ", artifact_dir, "\n", sep = "")
  if (identical(status, "PASS")) 0L else 1L
}

if (sys.nframe() == 0L) {
  status <- tryCatch(
    ci_semantics_main(),
    error = function(condition) {
      message("CI semantics audit error: ", conditionMessage(condition))
      2L
    }
  )
  quit(save = "no", status = status, runLast = FALSE)
}
