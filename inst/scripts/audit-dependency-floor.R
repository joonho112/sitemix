#!/usr/bin/env Rscript

# Verify the direct dependency floor on the exact R 4.5.1 compatibility profile.
# This gate intentionally exercises only public sitemix APIs with base R. It
# never sources or executes the protected numeric-baseline builder.

.dep_floor_columns <- c(
  "package", "description_field", "description_constraint",
  "installed_version_policy", "expected_installed_version",
  "runtime_profile", "rationale"
)

.dep_floor_runtime_version <- "4.5.1"

.dep_floor_output_schema <- c(
  "site_id", "year", "indicator", "theta_raw", "theta_hat", "se_raw",
  "se", "n", "n_eff", "estimate_scale", "transform", "var_method",
  "flag_small_n", "flag_zero_cell", "input_mode", "flag_suppressed",
  "framing", "flag_below_accountability", "V"
)

.dep_floor_protected_sha256 <- c(
  "inst/scripts/build-regression-baselines.R" =
    "29e8909b541af31ff47042591b462bd745c8b172bf6574a4ee6a90ced050acb1",
  "tests/testthat/_data/regression/regression-baselines.rds" =
    "be0527f9357aa7cbb0c014a9b0ce8e60e15252b5270fad5bb99113106f9e075b",
  "tests/testthat/_snaps/output-schema.md" =
    "ed838cde596fba9618627826af12e5e5b286fa633076474bc9e47f6824885c8e"
)

dep_floor_expected_policy <- function() {
  data.frame(
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
    rationale = c(
      rep(
        "Direct runtime floor verified exactly on the frozen pre-R-4.6 profile",
        4L
      ),
      paste(
        "R-recommended Matrix remains unversioned and must be available",
        "in an R 4.5.1-compatible release"
      )
    ),
    stringsAsFactors = FALSE
  )
}

dep_floor_parse_args <- function(args) {
  values <- list(out_dir = NULL, profile = "gate", self_test = FALSE)
  observed <- character()
  for (arg in args) {
    if (identical(arg, "--self-test")) {
      name <- "self-test"
      value <- TRUE
    } else if (startsWith(arg, "--out-dir=")) {
      name <- "out-dir"
      value <- sub("^--out-dir=", "", arg)
    } else if (startsWith(arg, "--profile=")) {
      name <- "profile"
      value <- sub("^--profile=", "", arg)
    } else {
      stop("Unknown or malformed argument: ", arg, call. = FALSE)
    }
    if (name %in% observed) {
      stop("Duplicate argument: --", name, call. = FALSE)
    }
    observed <- c(observed, name)
    if (identical(name, "self-test")) {
      values$self_test <- value
    } else if (identical(name, "out-dir")) {
      values$out_dir <- value
    } else {
      values$profile <- value
    }
  }
  if (!values$profile %in% c("gate", "negative-version")) {
    stop("`--profile` must be `gate` or `negative-version`.", call. = FALSE)
  }
  if (!values$self_test && (is.null(values$out_dir) || !nzchar(values$out_dir))) {
    stop("Missing required argument: --out-dir=PATH", call. = FALSE)
  }
  values
}

dep_floor_script_path <- function() {
  command <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command, value = TRUE)
  if (length(file_arg) != 1L) {
    stop("The dependency-floor audit must be run with Rscript.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
}

dep_floor_root <- function() {
  script <- dep_floor_script_path()
  root <- normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE)
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    stop("Could not locate the package root.", call. = FALSE)
  }
  root
}

dep_floor_output_target <- function(path) {
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

dep_floor_read_policy <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character(),
    colClasses = "character"
  )
}

dep_floor_policy_checks <- function(policy) {
  expected <- dep_floor_expected_policy()
  checks <- c(
    identical(names(policy), .dep_floor_columns),
    identical(nrow(policy), nrow(expected)),
    !anyNA(policy),
    all(vapply(policy, function(column) all(nzchar(column)), logical(1))),
    !anyDuplicated(policy$package),
    identical(policy, expected)
  )
  data.frame(
    component = c(
      "policy_schema", "policy_row_count", "policy_missing_values",
      "policy_nonempty_values", "policy_unique_packages", "policy_exact_content"
    ),
    pass = checks,
    detail = c(
      paste(names(policy), collapse = "|"),
      paste0(nrow(policy), " rows"),
      if (checks[[3L]]) "none" else "missing values found",
      if (checks[[4L]]) "all values nonempty" else "empty values found",
      if (checks[[5L]]) "unique" else "duplicate package rows found",
      if (checks[[6L]]) "matches frozen policy" else "policy content drift"
    ),
    stringsAsFactors = FALSE
  )
}

dep_floor_parse_imports <- function(value) {
  entries <- trimws(strsplit(value, ",", fixed = TRUE)[[1L]])
  rows <- lapply(entries, function(entry) {
    matched <- regexec(
      "^([A-Za-z][A-Za-z0-9.]*)[[:space:]]*(?:[(]([^)]*)[)])?$",
      entry,
      perl = TRUE
    )
    parts <- regmatches(entry, matched)[[1L]]
    if (!length(parts)) {
      return(data.frame(
        package = NA_character_, constraint = NA_character_,
        source = entry, stringsAsFactors = FALSE
      ))
    }
    constraint <- if (length(parts) >= 3L && nzchar(parts[[3L]])) {
      trimws(parts[[3L]])
    } else {
      "unversioned"
    }
    data.frame(
      package = parts[[2L]], constraint = constraint,
      source = entry, stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

dep_floor_description_checks <- function(root, policy) {
  description <- read.dcf(file.path(root, "DESCRIPTION"))
  parsed <- dep_floor_parse_imports(description[[1L, "Imports"]])
  usable <- identical(names(policy), .dep_floor_columns) &&
    identical(nrow(policy), 5L) && !anyNA(policy$package)
  if (!usable) {
    parsed$expected_constraint <- NA_character_
    parsed$pass <- FALSE
    parsed$detail <- "policy is not usable"
    return(parsed)
  }
  expected <- policy[, c("package", "description_constraint"), drop = FALSE]
  names(expected)[[2L]] <- "expected_constraint"
  parsed$expected_constraint <- expected$expected_constraint[
    match(parsed$package, expected$package)
  ]
  parsed$pass <- !is.na(parsed$package) & !is.na(parsed$expected_constraint) &
    parsed$constraint == parsed$expected_constraint
  parsed$detail <- ifelse(
    parsed$pass,
    "DESCRIPTION constraint matches",
    "DESCRIPTION constraint mismatch"
  )
  exact_order <- identical(parsed$package, expected$package)
  extra <- data.frame(
    package = "<Imports-order>", constraint = paste(parsed$package, collapse = "|"),
    source = description[[1L, "Imports"]],
    expected_constraint = paste(expected$package, collapse = "|"),
    pass = exact_order,
    detail = if (exact_order) "Imports package set and order match" else "Imports package set or order drift",
    stringsAsFactors = FALSE
  )
  rbind(parsed, extra)
}

dep_floor_safe_version <- function(package) {
  tryCatch(
    as.character(utils::packageVersion(package)),
    error = function(condition) NA_character_
  )
}

dep_floor_collect_versions <- function(policy) {
  packages <- if (identical(names(policy), .dep_floor_columns)) {
    policy$package
  } else {
    dep_floor_expected_policy()$package
  }
  data.frame(
    package = packages,
    observed_version = vapply(packages, dep_floor_safe_version, character(1)),
    stringsAsFactors = FALSE
  )
}

dep_floor_evaluate_versions <- function(policy, observed) {
  required <- c(
    "package", "installed_version_policy", "expected_installed_version"
  )
  if (!all(required %in% names(policy))) {
    stop("Version policy is missing required columns.", call. = FALSE)
  }
  actual <- observed$observed_version[match(policy$package, observed$package)]
  available <- !is.na(actual) & nzchar(actual)
  valid_version <- vapply(actual, function(value) {
    if (is.na(value) || !nzchar(value)) {
      return(FALSE)
    }
    tryCatch({
      numeric_version(value)
      TRUE
    }, error = function(condition) FALSE)
  }, logical(1))
  exact <- policy$installed_version_policy == "exact"
  compatible <- policy$installed_version_policy == "compatible"
  pass <- available & valid_version &
    ((exact & actual == policy$expected_installed_version) | compatible)
  data.frame(
    package = policy$package,
    version_policy = policy$installed_version_policy,
    expected_version = policy$expected_installed_version,
    observed_version = actual,
    available = available,
    valid_version = valid_version,
    pass = pass,
    detail = ifelse(
      pass,
      ifelse(exact, "exact installed floor matches", "R-profile-compatible version is available"),
      ifelse(available, "installed version violates policy", "package is not installed")
    ),
    stringsAsFactors = FALSE
  )
}

dep_floor_normalize_dcf <- function(value) {
  paste(strsplit(trimws(value), "[[:space:]]+")[[1L]], collapse = " ")
}

dep_floor_dcf_value <- function(dcf, field) {
  if (!field %in% colnames(dcf)) {
    return(NA_character_)
  }
  dep_floor_normalize_dcf(dcf[[1L, field]])
}

dep_floor_sitemix_checks <- function(root) {
  source_dcf <- read.dcf(file.path(root, "DESCRIPTION"))
  installed_path <- tryCatch(
    find.package("sitemix", quiet = TRUE),
    error = function(condition) ""
  )
  available <- length(installed_path) == 1L && nzchar(installed_path)
  if (available) {
    installed_path <- normalizePath(installed_path, mustWork = TRUE)
  } else {
    installed_path <- NA_character_
  }
  source_path <- normalizePath(root, mustWork = TRUE)
  distinct <- available && !identical(installed_path, source_path)
  installed_description <- if (available) {
    file.path(installed_path, "DESCRIPTION")
  } else {
    NA_character_
  }
  installed_dcf <- if (available && file.exists(installed_description)) {
    read.dcf(installed_description)
  } else {
    matrix(character(), nrow = 0L, ncol = 0L)
  }
  fields <- c("Package", "Version", "Depends", "Imports")
  source_values <- vapply(fields, function(field) {
    dep_floor_dcf_value(source_dcf, field)
  }, character(1))
  installed_values <- vapply(fields, function(field) {
    if (!nrow(installed_dcf)) NA_character_ else dep_floor_dcf_value(installed_dcf, field)
  }, character(1))
  metadata <- data.frame(
    component = paste0("installed_metadata_", tolower(fields)),
    expected = source_values,
    observed = installed_values,
    pass = !is.na(installed_values) & source_values == installed_values,
    detail = ifelse(
      !is.na(installed_values) & source_values == installed_values,
      "installed metadata matches source", "installed metadata mismatch"
    ),
    stringsAsFactors = FALSE
  )
  built <- if (nrow(installed_dcf)) dep_floor_dcf_value(installed_dcf, "Built") else NA_character_
  built_r <- if (!is.na(built) && grepl("^R [^;]+", built)) {
    sub("^R ([^;]+).*$", "\\1", built)
  } else {
    NA_character_
  }
  current_r <- as.character(getRversion())
  header <- data.frame(
    component = c(
      "runtime_version_exact", "installed_package_available",
      "installed_path_distinct", "installed_built_current_r"
    ),
    expected = c(.dep_floor_runtime_version, "TRUE", source_path, current_r),
    observed = c(current_r, as.character(available), installed_path, built_r),
    pass = c(
      identical(current_r, .dep_floor_runtime_version), available, distinct,
      !is.na(built_r) && identical(built_r, current_r)
    ),
    detail = c(
      if (identical(current_r, .dep_floor_runtime_version)) {
        "runtime matches the exact dependency-floor profile"
      } else {
        "runtime does not match the exact dependency-floor profile"
      },
      if (available) "installed sitemix found" else "installed sitemix not found",
      if (distinct) "installed library path differs from source" else "installed path resolves to source or is absent",
      if (!is.na(built_r) && identical(built_r, current_r)) {
        "installed under current R"
      } else {
        "Built R differs from current R"
      }
    ),
    stringsAsFactors = FALSE
  )
  list(checks = rbind(header, metadata), path = installed_path)
}

dep_floor_hash <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  tryCatch(
    unname(tools::sha256sum(path)),
    error = function(condition) NA_character_
  )
}

dep_floor_source_manifest <- function(root, installed_path) {
  source_paths <- c(
    "NAMESPACE", "inst/gates/dependency-floor.csv",
    "inst/scripts/audit-dependency-floor.R"
  )
  installed_paths <- c(
    "NAMESPACE", "gates/dependency-floor.csv",
    "scripts/audit-dependency-floor.R"
  )
  source_full <- file.path(root, source_paths)
  installed_full <- if (length(installed_path) == 1L && !is.na(installed_path)) {
    file.path(installed_path, installed_paths)
  } else {
    rep(NA_character_, length(installed_paths))
  }
  source_hash <- vapply(source_full, dep_floor_hash, character(1))
  installed_hash <- vapply(installed_full, dep_floor_hash, character(1))
  pass <- !is.na(source_hash) & !is.na(installed_hash) & source_hash == installed_hash
  data.frame(
    component = c("namespace", "dependency_floor_policy", "dependency_floor_audit"),
    source_path = source_paths,
    installed_path = installed_paths,
    source_sha256 = source_hash,
    installed_sha256 = installed_hash,
    pass = pass,
    detail = ifelse(pass, "installed artifact matches source", "installed artifact differs from source"),
    stringsAsFactors = FALSE
  )
}

dep_floor_smoke_row <- function(component, pass, detail) {
  if (!length(detail)) {
    detail <- "<missing>"
  }
  data.frame(
    component = component,
    pass = isTRUE(pass),
    detail = paste(as.character(detail), collapse = "|"),
    stringsAsFactors = FALSE
  )
}

dep_floor_matrix_smoke <- function() {
  result <- tryCatch({
    if (!requireNamespace("Matrix", quietly = TRUE)) {
      stop("Matrix namespace is unavailable", call. = FALSE)
    }
    near_pd <- getExportedValue("Matrix", "nearPD")
    candidate <- matrix(c(1, 1.2, 1.2, 1), nrow = 2L)
    projection <- near_pd(
      candidate,
      corr = TRUE,
      do2eigen = TRUE,
      base.matrix = TRUE,
      maxit = 100L
    )
    projected <- as.matrix(projection$mat)
    eigenvalues <- eigen(
      (projected + t(projected)) / 2,
      symmetric = TRUE,
      only.values = TRUE
    )$values
    list(
      version = as.character(utils::packageVersion("Matrix")),
      projected = projected,
      converged = isTRUE(projection$converged),
      eigenvalues = eigenvalues
    )
  }, error = function(condition) condition)
  if (inherits(result, "condition")) {
    return(dep_floor_smoke_row(
      "matrix_namespace_nearpd", FALSE, conditionMessage(result)
    ))
  }
  projected <- result$projected
  rbind(
    dep_floor_smoke_row(
      "matrix_namespace_nearpd", TRUE,
      paste0("Matrix ", result$version, " loaded and nearPD completed")
    ),
    dep_floor_smoke_row(
      "matrix_nearpd_shape_symmetry",
      identical(dim(projected), c(2L, 2L)) &&
        isTRUE(all.equal(projected, t(projected), tolerance = 1e-10)),
      paste(dim(projected), collapse = "x")
    ),
    dep_floor_smoke_row(
      "matrix_nearpd_finite",
      is.numeric(projected) && all(is.finite(projected)),
      "projected entries are finite"
    ),
    dep_floor_smoke_row(
      "matrix_nearpd_converged", result$converged,
      paste0("converged=", result$converged)
    ),
    dep_floor_smoke_row(
      "matrix_nearpd_psd",
      all(is.finite(result$eigenvalues)) && min(result$eigenvalues) >= -1e-8,
      paste0("min_eigenvalue=", format(min(result$eigenvalues), digits = 17L))
    )
  )
}

dep_floor_all_v_valid <- function(values) {
  is.list(values) && length(values) > 0L && all(vapply(values, function(value) {
    if (!inherits(value, "sm_vcov") || !is.list(value)) {
      return(FALSE)
    }
    matrix <- value[["matrix"]]
    is.matrix(matrix) &&
      identical(dim(matrix), c(1L, 1L)) && is.numeric(matrix) &&
      all(is.finite(matrix)) &&
      identical(value[["vcov_scale"]], "arcsine_delta")
  }, logical(1)))
}

dep_floor_numeric_column <- function(value, column, positive = FALSE) {
  observed <- value[[column]]
  is.numeric(observed) && length(observed) == nrow(value) &&
    length(observed) > 0L && all(is.finite(observed)) &&
    (!positive || all(observed > 0))
}

dep_floor_first_value <- function(value, column) {
  if (!column %in% names(value) || nrow(value) < 1L) {
    return(NULL)
  }
  value[[column]][[1L]]
}

dep_floor_smoke <- function() {
  result <- tryCatch({
    estimate <- getExportedValue("sitemix", "sm_estimate")
    diagnose <- getExportedValue("sitemix", "sm_diagnose")
    data_env <- new.env(parent = emptyenv())
    suppressWarnings(utils::data("alprek_subset", package = "sitemix", envir = data_env))
    if (!exists("alprek_subset", envir = data_env, inherits = FALSE)) {
      stop("installed alprek_subset data is unavailable", call. = FALSE)
    }
    data <- get("alprek_subset", envir = data_env, inherits = FALSE)
    work <- data[data$year == 2024, , drop = FALSE]
    output <- estimate(
      work,
      family = "binomial",
      indicator = "frpm",
      min_n = 1L,
      vjt = TRUE
    )
    diagnostics <- diagnose(output, verbose = FALSE)
    list(output = output, diagnostics = diagnostics)
  }, error = function(condition) condition)
  if (inherits(result, "condition")) {
    return(dep_floor_smoke_row("public_api_execution", FALSE, conditionMessage(result)))
  }
  output <- result$output
  diagnostics <- result$diagnostics
  required_diag <- c(
    "family", "sitemix_role", "n_cells", "scalar_uncertainty_finite",
    "scalar_se_positive", "v_present", "v_valid"
  )
  diag_family <- dep_floor_first_value(diagnostics, "family")
  diag_role <- dep_floor_first_value(diagnostics, "sitemix_role")
  diag_n_cells <- dep_floor_first_value(diagnostics, "n_cells")
  diag_scalar_finite <- dep_floor_first_value(
    diagnostics, "scalar_uncertainty_finite"
  )
  diag_se_positive <- dep_floor_first_value(
    diagnostics, "scalar_se_positive"
  )
  diag_v_present <- dep_floor_first_value(diagnostics, "v_present")
  diag_v_valid <- dep_floor_first_value(diagnostics, "v_valid")
  execution_row <- dep_floor_smoke_row(
    "public_api_execution", TRUE, "sm_estimate and sm_diagnose completed"
  )
  rows <- tryCatch(list(
    execution_row,
    dep_floor_smoke_row("estimate_class", inherits(output, "sitemix_estimates"), paste(class(output), collapse = "|")),
    dep_floor_smoke_row("estimate_rows", nrow(output) == 50L, paste0(nrow(output), " rows")),
    dep_floor_smoke_row(
      "estimate_schema",
      identical(names(output), .dep_floor_output_schema),
      paste(names(output), collapse = "|")
    ),
    dep_floor_smoke_row(
      "estimate_family",
      identical(attr(output, "family"), "binomial"),
      as.character(attr(output, "family"))
    ),
    dep_floor_smoke_row(
      "estimate_role",
      identical(attr(output, "sitemix_role"), "summary_uncertainty"),
      as.character(attr(output, "sitemix_role"))
    ),
    dep_floor_smoke_row(
      "estimate_finite",
      dep_floor_numeric_column(output, "theta_hat"),
      "theta_hat values are finite"
    ),
    dep_floor_smoke_row(
      "standard_error_finite",
      dep_floor_numeric_column(output, "se"),
      "SE values are finite"
    ),
    dep_floor_smoke_row(
      "standard_error_positive",
      dep_floor_numeric_column(output, "se", positive = TRUE),
      "SE values are positive"
    ),
    dep_floor_smoke_row(
      "vcov_contract",
      dep_floor_all_v_valid(output[["V"]]),
      "all V entries satisfy the 1 x 1 sm_vcov contract"
    ),
    dep_floor_smoke_row(
      "diagnostics_class",
      inherits(diagnostics, "sitemix_diagnostics_summary"),
      paste(class(diagnostics), collapse = "|")
    ),
    dep_floor_smoke_row("diagnostics_rows", nrow(diagnostics) == 1L, paste0(nrow(diagnostics), " rows")),
    dep_floor_smoke_row(
      "diagnostics_schema",
      all(required_diag %in% names(diagnostics)),
      paste(names(diagnostics), collapse = "|")
    ),
    dep_floor_smoke_row(
      "diagnostics_family",
      identical(diag_family, "binomial"),
      as.character(diag_family)
    ),
    dep_floor_smoke_row(
      "diagnostics_role",
      identical(diag_role, "summary_uncertainty"),
      as.character(diag_role)
    ),
    dep_floor_smoke_row(
      "diagnostics_n_cells",
      identical(diag_n_cells, 50L),
      as.character(diag_n_cells)
    ),
    dep_floor_smoke_row(
      "diagnostics_scalar_finite",
      isTRUE(diag_scalar_finite),
      as.character(diag_scalar_finite)
    ),
    dep_floor_smoke_row(
      "diagnostics_se_positive",
      isTRUE(diag_se_positive),
      as.character(diag_se_positive)
    ),
    dep_floor_smoke_row(
      "diagnostics_v_present",
      isTRUE(diag_v_present),
      as.character(diag_v_present)
    ),
    dep_floor_smoke_row(
      "diagnostics_v_valid",
      isTRUE(diag_v_valid),
      as.character(diag_v_valid)
    )
  ), error = function(condition) condition)
  if (inherits(rows, "condition")) {
    return(rbind(
      execution_row,
      dep_floor_smoke_row(
        "public_api_contract_checks", FALSE, conditionMessage(rows)
      )
    ))
  }
  do.call(rbind, rows)
}

dep_floor_protected <- function(root, before) {
  paths <- names(.dep_floor_protected_sha256)
  after <- vapply(file.path(root, paths), dep_floor_hash, character(1))
  expected <- unname(.dep_floor_protected_sha256)
  pass <- !is.na(before) & !is.na(after) &
    before == expected & after == expected & before == after
  data.frame(
    path = paths,
    expected_sha256 = expected,
    before_sha256 = before,
    after_sha256 = after,
    expected_before = !is.na(before) & before == expected,
    expected_after = !is.na(after) & after == expected,
    unchanged = !is.na(before) & !is.na(after) & before == after,
    numeric_builder_executed = FALSE,
    pass = pass,
    stringsAsFactors = FALSE
  )
}

dep_floor_bind_installed <- function(policy, before, evaluated, after) {
  before_check <- dep_floor_evaluate_versions(policy, before)
  evaluated_check <- dep_floor_evaluate_versions(policy, evaluated)
  after_check <- dep_floor_evaluate_versions(policy, after)
  unchanged <- before_check$observed_version == after_check$observed_version
  unchanged[is.na(unchanged)] <- FALSE
  data.frame(
    package = policy$package,
    version_policy = policy$installed_version_policy,
    expected_version = policy$expected_installed_version,
    observed_before = before_check$observed_version,
    observed_evaluated = evaluated_check$observed_version,
    observed_after = after_check$observed_version,
    policy_pass_before = before_check$pass,
    policy_pass_evaluated = evaluated_check$pass,
    policy_pass_after = after_check$pass,
    unchanged_after_smoke = unchanged,
    pass = evaluated_check$pass & after_check$pass & unchanged,
    detail = evaluated_check$detail,
    stringsAsFactors = FALSE
  )
}

dep_floor_write_csv <- function(value, path) {
  utils::write.csv(value, path, row.names = FALSE, na = "")
}

dep_floor_write_evidence <- function(target, evidence) {
  stage <- tempfile(pattern = paste0(".", basename(target), "-"), tmpdir = dirname(target))
  if (!dir.create(stage, recursive = FALSE, showWarnings = FALSE)) {
    stop("Could not create evidence staging directory.", call. = FALSE)
  }
  committed <- FALSE
  on.exit({
    if (!committed && dir.exists(stage)) {
      unlink(stage, recursive = TRUE, force = TRUE)
    }
  }, add = TRUE)
  dep_floor_write_csv(evidence$summary, file.path(stage, "dependency-floor-summary.csv"))
  dep_floor_write_csv(evidence$policy, file.path(stage, "dependency-floor-policy.csv"))
  dep_floor_write_csv(evidence$description, file.path(stage, "dependency-floor-description.csv"))
  dep_floor_write_csv(evidence$installed, file.path(stage, "dependency-floor-installed.csv"))
  dep_floor_write_csv(evidence$sitemix, file.path(stage, "dependency-floor-sitemix.csv"))
  dep_floor_write_csv(evidence$source_manifest, file.path(stage, "dependency-floor-source-manifest.csv"))
  dep_floor_write_csv(evidence$smoke, file.path(stage, "dependency-floor-smoke.csv"))
  dep_floor_write_csv(evidence$protected, file.path(stage, "dependency-floor-protected.csv"))
  writeLines(evidence$session_info, file.path(stage, "dependency-floor-session-info.txt"), useBytes = TRUE)
  saveRDS(evidence, file.path(stage, "dependency-floor-evidence.rds"), version = 2L)
  if (!file.rename(stage, target)) {
    stop("Could not atomically commit the evidence directory.", call. = FALSE)
  }
  committed <- TRUE
  invisible(target)
}

dep_floor_all_pass <- function(value) {
  is.data.frame(value) && "pass" %in% names(value) &&
    length(value$pass) > 0L && all(value$pass)
}

dep_floor_summary <- function(profile, evidence, negative_detected) {
  components <- c(
    dep_floor_all_pass(evidence$policy_checks),
    dep_floor_all_pass(evidence$description),
    dep_floor_all_pass(evidence$installed),
    dep_floor_all_pass(evidence$sitemix),
    dep_floor_all_pass(evidence$source_manifest),
    dep_floor_all_pass(evidence$smoke),
    dep_floor_all_pass(evidence$protected)
  )
  gate_pass <- all(components)
  status <- if (identical(profile, "negative-version")) "FAIL" else if (gate_pass) "PASS" else "FAIL"
  matrix_row <- evidence$installed$package == "Matrix"
  data.frame(
    status = status,
    profile = profile,
    current_r = as.character(getRversion()),
    platform = R.version$platform,
    direct_floor_count = sum(evidence$installed$version_policy == "exact"),
    direct_floor_failures = sum(!evidence$installed$policy_pass_evaluated),
    matrix_version = evidence$installed$observed_evaluated[matrix_row][[1L]],
    policy_failures = sum(!evidence$policy_checks$pass),
    description_failures = sum(!evidence$description$pass),
    installed_source_failures = sum(!evidence$sitemix$pass) + sum(!evidence$source_manifest$pass),
    smoke_failures = sum(!evidence$smoke$pass),
    protected_failures = sum(!evidence$protected$pass),
    negative_version_detected = isTRUE(negative_detected),
    numeric_builder_executed = FALSE,
    gate_pass = gate_pass,
    stringsAsFactors = FALSE
  )
}

dep_floor_run_gate <- function(args) {
  root <- dep_floor_root()
  target <- dep_floor_output_target(args$out_dir)
  policy_path <- file.path(root, "inst", "gates", "dependency-floor.csv")
  policy <- dep_floor_read_policy(policy_path)
  policy_checks <- dep_floor_policy_checks(policy)
  runtime_policy <- dep_floor_expected_policy()
  protected_before <- vapply(
    file.path(root, names(.dep_floor_protected_sha256)),
    dep_floor_hash,
    character(1)
  )
  description <- dep_floor_description_checks(root, runtime_policy)
  installed_before <- dep_floor_collect_versions(runtime_policy)
  sitemix <- dep_floor_sitemix_checks(root)
  source_manifest <- dep_floor_source_manifest(root, sitemix$path)
  smoke <- rbind(dep_floor_matrix_smoke(), dep_floor_smoke())
  installed_after <- dep_floor_collect_versions(runtime_policy)
  protected <- dep_floor_protected(root, protected_before)
  evaluated <- installed_before
  if (identical(args$profile, "negative-version")) {
    evaluated$observed_version[evaluated$package == "cli"] <- "3.6.1"
  }
  installed <- dep_floor_bind_installed(
    runtime_policy, installed_before, evaluated, installed_after
  )
  evidence <- list(
    policy = policy,
    policy_checks = policy_checks,
    description = description,
    installed = installed,
    sitemix = sitemix$checks,
    source_manifest = source_manifest,
    smoke = smoke,
    protected = protected,
    session_info = capture.output(utils::sessionInfo())
  )
  baseline_pass <- all(c(
    dep_floor_all_pass(policy_checks),
    dep_floor_all_pass(description),
    all(installed$policy_pass_before),
    all(installed$policy_pass_after),
    all(installed$unchanged_after_smoke),
    dep_floor_all_pass(sitemix$checks),
    dep_floor_all_pass(source_manifest),
    dep_floor_all_pass(smoke),
    dep_floor_all_pass(protected)
  ))
  cli_row <- installed$package == "cli"
  negative_detected <- identical(args$profile, "negative-version") &&
    baseline_pass && sum(!installed$policy_pass_evaluated) == 1L &&
    !installed$policy_pass_evaluated[cli_row][[1L]]
  evidence$summary <- dep_floor_summary(
    args$profile, evidence, negative_detected
  )
  dep_floor_write_evidence(target, evidence)
  if (identical(args$profile, "negative-version")) {
    if (negative_detected) 1L else 2L
  } else if (isTRUE(evidence$summary$gate_pass[[1L]])) {
    0L
  } else {
    1L
  }
}

dep_floor_self_test <- function() {
  policy <- dep_floor_expected_policy()
  exact <- data.frame(
    package = policy$package,
    observed_version = c("1.1.0", "3.2.0", "0.6.0", "3.6.0", "1.7-4"),
    stringsAsFactors = FALSE
  )
  exact_result <- dep_floor_evaluate_versions(policy, exact)
  mismatch <- exact
  mismatch$observed_version[mismatch$package == "cli"] <- "3.6.1"
  mismatch_result <- dep_floor_evaluate_versions(policy, mismatch)
  mismatch_row <- mismatch_result$package == "cli"
  valid_vcov <- structure(
    list(matrix = matrix(0.01, nrow = 1L), vcov_scale = "arcsine_delta"),
    class = "sm_vcov"
  )
  wrong_scale <- valid_vcov
  wrong_scale$vcov_scale <- "raw"
  obsolete_matrix_shape <- structure(matrix(0.01, nrow = 1L), class = "sm_vcov")
  matrix_smoke <- dep_floor_matrix_smoke()
  pass <- all(exact_result$pass) &&
    sum(!mismatch_result$pass) == 1L &&
    !mismatch_result$pass[mismatch_row][[1L]] &&
    isTRUE(mismatch_result$pass[mismatch_result$package == "Matrix"][[1L]]) &&
    dep_floor_all_v_valid(list(valid_vcov)) &&
    !dep_floor_all_v_valid(list(wrong_scale)) &&
    !dep_floor_all_v_valid(list(obsolete_matrix_shape)) &&
    dep_floor_all_pass(matrix_smoke)
  if (!pass) {
    stop("Dependency-floor evaluator self-test failed.", call. = FALSE)
  }
  message("dependency-floor self-test: PASS")
  0L
}

dep_floor_main <- function(args) {
  parsed <- dep_floor_parse_args(args)
  if (parsed$self_test) {
    return(dep_floor_self_test())
  }
  dep_floor_run_gate(parsed)
}

if (sys.nframe() == 0L) {
  status <- tryCatch(
    dep_floor_main(commandArgs(trailingOnly = TRUE)),
    error = function(condition) {
      message("dependency-floor audit error: ", conditionMessage(condition))
      2L
    }
  )
  quit(save = "no", status = status, runLast = FALSE)
}
