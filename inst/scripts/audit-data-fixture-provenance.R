#!/usr/bin/env Rscript

.data_fixture_protected_sha256 <- c(
  "inst/scripts/build-regression-baselines.R" =
    "29e8909b541af31ff47042591b462bd745c8b172bf6574a4ee6a90ced050acb1",
  "tests/testthat/_data/regression/regression-baselines.rds" =
    "be0527f9357aa7cbb0c014a9b0ce8e60e15252b5270fad5bb99113106f9e075b",
  "tests/testthat/_snaps/output-schema.md" =
    "ed838cde596fba9618627826af12e5e5b286fa633076474bc9e47f6824885c8e"
)

.data_fixture_manifest_columns <- c(
  "path", "artifact_role", "producer", "source_inputs",
  "deterministic_seed", "format", "rows", "columns", "schema", "md5",
  "sha256", "regeneration_mode", "restricted_input_required",
  "repository_disposition", "source_package_disposition", "claim_boundary"
)

.data_fixture_public_paths <- c(
  "data/alprek_subset.rda",
  "inst/extdata/alprek_subset.csv",
  "inst/extdata/alprek_subset_counts.rds",
  "inst/extdata/alprek_subset_provenance.txt"
)

.data_fixture_review_paths <- file.path(
  "tests", "testthat", "_data", "regression",
  c(
    "alprek_summary.csv",
    "alprek_spotcheck_rows.csv",
    "alprek_spotcheck_vcov.csv",
    "small_cases_rows.csv",
    "small_cases_vcov.csv"
  )
)

.data_fixture_public_schema <- c(
  "student_id", "site_id", "year", "frpm", "snap", "wic", "tanf"
)

.data_fixture_count_schema <- c(
  "site_id", "year", "n_jt", "c_jt_frpm", "c_jt_snap", "c_jt_wic",
  "c_jt_tanf", "c_jt_frpm_snap", "c_jt_frpm_wic", "c_jt_frpm_tanf",
  "c_jt_snap_wic", "c_jt_snap_tanf", "c_jt_wic_tanf"
)

.data_fixture_baseline_schema <- c(
  "metadata", "tolerances", "alprek_summary", "alprek_content",
  "scenario_a", "scenario_b", "scenario_c", "aggregate_d0", "aggregate_d1",
  "aggregate_d0_alprek_2024_frpm",
  "aggregate_d1_alprek_2024_four_indicator"
)

.data_fixture_expected_paths <- c(
  "dev/data-ALprek-example/student_panel_2021-2026.rds",
  .data_fixture_public_paths,
  "inst/scripts/build-alprek-subset.R",
  "tests/testthat/_data/regression/regression-baselines.rds",
  .data_fixture_review_paths,
  "inst/scripts/build-regression-baselines.R"
)

.data_fixture_expected_roles <- c(
  "restricted_canonical_input",
  "public_package_data",
  "public_portable_data",
  "public_count_data",
  "public_provenance",
  "public_data_builder",
  "protected_numeric_baseline",
  rep("regression_review_projection", 5L),
  "protected_numeric_builder"
)

.data_fixture_expected_metadata <- data.frame(
  producer = c(
    "restricted_administrative_source",
    rep("inst/scripts/build-alprek-subset.R", 4L),
    "maintainer_source",
    "inst/scripts/build-regression-baselines.R",
    rep(
      "tests/testthat/helper-regression.R::regression_write_review_csvs",
      5L
    ),
    "maintainer_source"
  ),
  deterministic_seed = c(
    "not_applicable",
    rep("2026", 5L),
    rep("none", 7L)
  ),
  format = c(
    "rds_v3_xdr", "rda_xz", "csv", "rds_v2_xdr", "plain_text",
    "R_script", "rds_v2_xdr", rep("csv", 5L), "R_script"
  ),
  regeneration_mode = c(
    "never_ship_or_copy",
    rep("explicit_restricted_source_content_replay", 4L),
    "explicit_restricted_source_to_fresh_temp_only",
    "never_regenerate_in_automated_audit",
    rep("replay_review_csv_from_protected_baseline_only", 5L),
    "never_execute_in_automated_audit"
  ),
  restricted_input_required = c(rep("TRUE", 6L), rep("FALSE", 7L)),
  repository_disposition = c(
    "ignored_local_only",
    rep("repository_artifact", 4L),
    "maintainer_repository_file",
    rep("repository_artifact", 6L),
    "maintainer_repository_file"
  ),
  source_package_disposition = c(
    "excluded",
    rep("included", 11L),
    "excluded"
  ),
  stringsAsFactors = FALSE
)

.data_fixture_helper_sha256 <-
  "d126f24127a77b3ac1235f268e126bb68ce290288f036cdeb2c33ff722daf00d"

.data_fixture_companion_md5 <- "882cb68041ad78faae94d738b0cce928"
.data_fixture_companion_sha256 <-
  "4af83ba9469a25ac021248850136274b6d8037f431198a23001b5939aa9ac26e"
.data_fixture_companion_rows <- 116899L

data_fixture_arg_value <- function(args, name, default = NULL) {
  prefix <- paste0("--", name, "=")
  hit <- grep(paste0("^", prefix), args, value = TRUE)
  if (length(hit)) {
    sub(prefix, "", hit[[1L]], fixed = TRUE)
  } else {
    default
  }
}

data_fixture_validate_args <- function(args) {
  allowed <- c("out-dir", "source", "replay-public-data")
  valid <- vapply(args, function(arg) {
    any(startsWith(arg, paste0("--", allowed, "=")))
  }, logical(1))
  if (any(!valid)) {
    stop("Unknown or malformed argument: ", args[which(!valid)[[1L]]], call. = FALSE)
  }
  observed <- sub("^--([^=]+).*$", "\\1", args)
  if (anyDuplicated(observed)) {
    stop("Duplicate argument: --", observed[duplicated(observed)][[1L]], call. = FALSE)
  }
  if (!"out-dir" %in% observed) {
    stop("Missing required argument: --out-dir=PATH", call. = FALSE)
  }
  invisible(TRUE)
}

data_fixture_bool <- function(value, name) {
  normalized <- toupper(value)
  if (!normalized %in% c("TRUE", "FALSE")) {
    stop("`--", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  identical(normalized, "TRUE")
}

data_fixture_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (!length(file_arg)) {
    stop("The data-fixture audit must be run with Rscript.", call. = FALSE)
  }
  normalizePath(sub("^--file=", "", file_arg[[1L]]), mustWork = TRUE)
}

data_fixture_root <- function() {
  script <- data_fixture_script_path()
  root <- normalizePath(file.path(dirname(script), "..", ".."), mustWork = TRUE)
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    stop("Could not locate the package root.", call. = FALSE)
  }
  root
}

data_fixture_output_target <- function(path) {
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

data_fixture_md5 <- function(paths) {
  paths <- as.character(paths)
  if (any(!file.exists(paths))) {
    stop("Cannot hash a missing file with MD5.", call. = FALSE)
  }
  unname(tools::md5sum(paths))
}

data_fixture_sha256 <- function(paths) {
  paths <- as.character(paths)
  if (any(!file.exists(paths))) {
    stop("Cannot hash a missing file with SHA-256.", call. = FALSE)
  }
  unname(tools::sha256sum(paths))
}

data_fixture_count_lines <- function(path) {
  connection <- file(path, open = "rt")
  on.exit(close(connection), add = TRUE)
  count <- 0L
  repeat {
    lines <- readLines(connection, n = 100000L, warn = FALSE)
    if (!length(lines)) {
      break
    }
    count <- count + length(lines)
  }
  count
}

data_fixture_read_csv <- function(path) {
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character(),
    colClasses = "character"
  )
}

data_fixture_load_rda <- function(path) {
  env <- new.env(parent = emptyenv())
  names <- load(path, envir = env)
  if (!identical(names, "alprek_subset")) {
    stop("Package-data RDA must contain exactly `alprek_subset`.", call. = FALSE)
  }
  env$alprek_subset
}

data_fixture_plain_frame <- function(x) {
  out <- as.data.frame(x, stringsAsFactors = FALSE)
  attr(out, "build_info") <- NULL
  out
}

data_fixture_source_data <- function(path) {
  raw <- readRDS(path)
  if (is.list(raw) && !is.data.frame(raw) && "data" %in% names(raw)) {
    raw$data
  } else {
    raw
  }
}

data_fixture_inspect <- function(path, role) {
  if (role %in% c("public_data_builder", "protected_numeric_builder")) {
    values <- c(rows = "not_applicable", columns = "not_applicable", schema = "R_script")
  } else if (identical(role, "restricted_canonical_input")) {
    x <- data_fixture_source_data(path)
    required <- c(
      "adece_id", "site_code", "year", "free_reduced_lunch", "snap", "wic", "tanf"
    )
    schema <- if (all(required %in% names(x))) {
      paste0("required_columns_only:", paste(required, collapse = "|"))
    } else {
      "required_columns_missing"
    }
    values <- c(
      rows = as.character(nrow(x)),
      columns = as.character(ncol(x)),
      schema = schema
    )
  } else if (identical(role, "public_package_data")) {
    x <- data_fixture_load_rda(path)
    values <- c(
      rows = as.character(nrow(x)),
      columns = as.character(ncol(x)),
      schema = paste(names(x), collapse = "|")
    )
  } else if (role %in% c("public_portable_data", "regression_review_projection")) {
    x <- utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      na.strings = character()
    )
    values <- c(
      rows = as.character(nrow(x)),
      columns = as.character(ncol(x)),
      schema = paste(names(x), collapse = "|")
    )
  } else if (identical(role, "public_count_data")) {
    x <- readRDS(path)
    values <- c(
      rows = as.character(nrow(x)),
      columns = as.character(ncol(x)),
      schema = paste(names(x), collapse = "|")
    )
  } else if (identical(role, "public_provenance")) {
    values <- c(
      rows = as.character(length(readLines(path, warn = FALSE))),
      columns = "not_applicable",
      schema = "plain_text"
    )
  } else if (identical(role, "protected_numeric_baseline")) {
    x <- readRDS(path)
    values <- c(
      rows = as.character(length(x)),
      columns = "not_applicable",
      schema = paste(names(x), collapse = "|")
    )
  } else {
    stop("Unsupported data-fixture artifact role: ", role, call. = FALSE)
  }
  values
}

data_fixture_manifest <- function(root, source_arg) {
  path <- file.path(root, "inst", "gates", "data-fixture-provenance.csv")
  manifest <- data_fixture_read_csv(path)
  issues <- character()
  if (!identical(names(manifest), .data_fixture_manifest_columns)) {
    issues <- c(issues, "manifest columns disagree with the frozen schema")
  }
  if (nrow(manifest) != 13L) {
    issues <- c(issues, "manifest must contain exactly 13 rows")
  }
  if (!identical(manifest$path, .data_fixture_expected_paths)) {
    issues <- c(issues, "manifest paths or canonical row order drifted")
  }
  if (!identical(manifest$artifact_role, .data_fixture_expected_roles)) {
    issues <- c(issues, "manifest roles or canonical row order drifted")
  }
  metadata_columns <- names(.data_fixture_expected_metadata)
  if (!identical(
    manifest[metadata_columns],
    .data_fixture_expected_metadata
  )) {
    issues <- c(issues, "manifest producer or disposition metadata drifted")
  }
  if (anyDuplicated(manifest$path)) {
    issues <- c(issues, "manifest paths are duplicated")
  }
  if (any(!vapply(manifest, function(column) all(nzchar(column)), logical(1)))) {
    issues <- c(issues, "manifest contains an empty field")
  }
  if (sum(manifest$artifact_role == "restricted_canonical_input") != 1L) {
    issues <- c(issues, "manifest must contain one restricted input")
  }
  if (sum(manifest$artifact_role == "public_data_builder") != 1L ||
        sum(manifest$artifact_role == "protected_numeric_builder") != 1L) {
    issues <- c(issues, "manifest must contain exactly two builder dispositions")
  }
  if (sum(manifest$artifact_role == "regression_review_projection") != 5L) {
    issues <- c(issues, "manifest must contain five regression review projections")
  }
  if (any(!grepl("^[0-9a-f]{32}$", manifest$md5)) ||
        any(!grepl("^[0-9a-f]{64}$", manifest$sha256))) {
    issues <- c(issues, "manifest checksums are malformed")
  }
  if (!setequal(
    unique(manifest$source_package_disposition),
    c("included", "excluded")
  )) {
    issues <- c(issues, "manifest source-package dispositions drifted")
  }
  helper_rows <- manifest$artifact_role %in% c(
    "regression_review_projection",
    "protected_numeric_builder"
  )
  if (any(!grepl(
    .data_fixture_helper_sha256,
    manifest$source_inputs[helper_rows],
    fixed = TRUE
  ))) {
    issues <- c(issues, "manifest regression-helper identity drifted")
  }

  observed_path <- file.path(root, manifest$path)
  restricted <- manifest$artifact_role == "restricted_canonical_input"
  if (nzchar(source_arg)) {
    observed_path[restricted] <- path.expand(source_arg)
  } else {
    observed_path[restricted] <- NA_character_
  }
  present <- !is.na(observed_path) & file.exists(observed_path)
  allowed_missing <- restricted & !nzchar(source_arg)
  if (any(!present & !allowed_missing)) {
    issues <- c(issues, "one or more required manifest paths are missing")
  }

  observed_md5 <- rep(NA_character_, nrow(manifest))
  observed_sha256 <- rep(NA_character_, nrow(manifest))
  observed_md5[present] <- data_fixture_md5(observed_path[present])
  observed_sha256[present] <- data_fixture_sha256(observed_path[present])
  md5_exact <- present & observed_md5 == manifest$md5
  sha256_exact <- present & observed_sha256 == manifest$sha256
  checksum_exact <- md5_exact & sha256_exact
  checksum_status <- ifelse(
    present,
    ifelse(checksum_exact, "EXACT", "DRIFT"),
    ifelse(allowed_missing, "NOT_CHECKED_ALLOWED_ABSENCE", "MISSING")
  )

  observed_rows <- rep(NA_character_, nrow(manifest))
  observed_columns <- rep(NA_character_, nrow(manifest))
  observed_schema <- rep(NA_character_, nrow(manifest))
  for (index in which(present)) {
    inspected <- data_fixture_inspect(observed_path[[index]], manifest$artifact_role[[index]])
    observed_rows[[index]] <- inspected[["rows"]]
    observed_columns[[index]] <- inspected[["columns"]]
    observed_schema[[index]] <- inspected[["schema"]]
  }
  schema_exact <- present &
    observed_rows == manifest$rows &
    observed_columns == manifest$columns &
    observed_schema == manifest$schema
  schema_status <- ifelse(
    present,
    ifelse(schema_exact, "EXACT", "DRIFT"),
    ifelse(allowed_missing, "NOT_CHECKED_ALLOWED_ABSENCE", "MISSING")
  )
  required <- !allowed_missing
  if (any(required & (!checksum_exact | !schema_exact))) {
    issues <- c(issues, "manifest checksum or schema contract failed")
  }

  manifest$observed_path_kind <- ifelse(
    restricted & nzchar(source_arg),
    "explicit_restricted_source",
    ifelse(restricted, "not_supplied", "source_root_relative")
  )
  manifest$present <- present
  manifest$observed_md5 <- observed_md5
  manifest$observed_sha256 <- observed_sha256
  manifest$checksum_status <- checksum_status
  manifest$observed_rows <- observed_rows
  manifest$observed_columns <- observed_columns
  manifest$observed_schema <- observed_schema
  manifest$schema_status <- schema_status
  list(table = manifest, issues = unique(issues), observed_paths = observed_path)
}

data_fixture_broad_inventory <- function(root) {
  path <- file.path(
    root,
    "tests", "testthat", "_data", "test-architecture", "fixture-provenance.csv"
  )
  manifest <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = character()
  )
  expected_columns <- c(
    "path", "artifact_kind", "producer", "source_inputs",
    "deterministic_seed", "checksum_algorithm", "checksum",
    "review_step", "review_disposition"
  )
  issues <- character()
  if (!identical(names(manifest), expected_columns)) {
    issues <- c(issues, "broad fixture manifest columns drifted")
  }
  if (nrow(manifest) != 26L || anyDuplicated(manifest$path)) {
    issues <- c(issues, "broad fixture manifest must have 26 unique paths")
  }
  if (anyNA(manifest) || any(!vapply(
    manifest,
    function(column) all(nzchar(as.character(column))),
    logical(1)
  ))) {
    issues <- c(issues, "broad fixture manifest contains an empty field")
  }
  if (!identical(unique(manifest$checksum_algorithm), "MD5")) {
    issues <- c(issues, "broad fixture manifest checksum algorithm drifted")
  }
  artifact_roots <- c(
    file.path(root, "data"),
    file.path(root, "inst", "extdata"),
    file.path(root, "tests", "testthat", "_data"),
    file.path(root, "tests", "testthat", "_snaps")
  )
  artifact_paths <- unlist(lapply(
    artifact_roots[dir.exists(artifact_roots)],
    function(directory) {
      list.files(directory, recursive = TRUE, full.names = TRUE)
    }
  ), use.names = FALSE)
  artifact_paths <- artifact_paths[!file.info(artifact_paths)$isdir]
  root_length <- nchar(normalizePath(root)) + 2L
  current_paths <- substring(normalizePath(artifact_paths), root_length)
  current_paths <- gsub("\\\\", "/", current_paths)
  current_paths <- sort(setdiff(
    current_paths,
    "tests/testthat/_data/test-architecture/fixture-provenance.csv"
  ))
  if (!identical(sort(manifest$path), current_paths)) {
    issues <- c(issues, "broad fixture manifest path set is incomplete")
  }
  paths <- file.path(root, manifest$path)
  present <- file.exists(paths)
  observed_md5 <- rep(NA_character_, nrow(manifest))
  observed_sha256 <- rep(NA_character_, nrow(manifest))
  observed_md5[present] <- data_fixture_md5(paths[present])
  observed_sha256[present] <- data_fixture_sha256(paths[present])
  exact <- present & observed_md5 == manifest$checksum
  if (any(!exact)) {
    issues <- c(issues, "broad fixture manifest has a missing or drifted artifact")
  }
  manifest$present <- present
  manifest$observed_md5 <- observed_md5
  manifest$observed_sha256 <- observed_sha256
  manifest$exact <- exact
  list(table = manifest, issues = unique(issues))
}

data_fixture_recompute_counts <- function(x) {
  indicators <- c("frpm", "snap", "wic", "tanf")
  groups <- split(
    seq_len(nrow(x)),
    paste(x$site_id, x$year, sep = "\r")
  )
  groups <- groups[order(names(groups))]
  pairs <- utils::combn(indicators, 2L, simplify = FALSE)
  rows <- lapply(groups, function(index) {
    row <- data.frame(
      site_id = x$site_id[[index[[1L]]]],
      year = as.integer(x$year[[index[[1L]]]]),
      n_jt = as.integer(length(index)),
      stringsAsFactors = FALSE
    )
    for (indicator in indicators) {
      row[[paste0("c_jt_", indicator)]] <- as.integer(sum(x[[indicator]][index]))
    }
    for (pair in pairs) {
      row[[paste0("c_jt_", pair[[1L]], "_", pair[[2L]])]] <- as.integer(sum(
        x[[pair[[1L]]]][index] * x[[pair[[2L]]]][index]
      ))
    }
    row
  })
  out <- do.call(rbind, rows)
  out <- out[order(out$site_id, out$year), .data_fixture_count_schema, drop = FALSE]
  row.names(out) <- NULL
  out
}

data_fixture_contract_row <- function(check, expected, actual, pass) {
  data.frame(
    check = check,
    expected = as.character(expected),
    actual = as.character(actual),
    pass = isTRUE(pass),
    stringsAsFactors = FALSE
  )
}

data_fixture_public_contract <- function(root) {
  x <- data_fixture_load_rda(file.path(root, "data", "alprek_subset.rda"))
  csv <- utils::read.csv(
    file.path(root, "inst", "extdata", "alprek_subset.csv"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  counts <- readRDS(file.path(root, "inst", "extdata", "alprek_subset_counts.rds"))
  provenance <- readLines(
    file.path(root, "inst", "extdata", "alprek_subset_provenance.txt"),
    warn = FALSE
  )
  info <- attr(x, "build_info", exact = TRUE)
  count_info <- attr(counts, "build_info", exact = TRUE)
  plain_x <- data_fixture_plain_frame(x)
  plain_counts <- data_fixture_plain_frame(counts)
  recomputed <- data_fixture_recompute_counts(x)
  indicator_values <- unlist(x[c("frpm", "snap", "wic", "tanf")], use.names = FALSE)
  package_schema <- paste(.data_fixture_public_schema, collapse = "|")
  observed_package_schema <- paste(names(x), collapse = "|")
  count_schema <- paste(.data_fixture_count_schema, collapse = "|")
  observed_count_schema <- paste(names(counts), collapse = "|")
  csv_exact <- identical(plain_x, csv)
  counts_exact <- identical(plain_counts, recomputed)
  build_info_exact <- identical(info, count_info)
  source_md5 <- "fd18882ba1a7ddc287300a7e5bafe84d"
  candidate_strata <- unname(unlist(info$complete_no_missing_candidate_sites))
  sampled_strata <- unname(unlist(info$sampled_strata))
  original_ids_absent <- !any(c("adece_id", "site_code") %in% names(x))
  site_ids_synthetic <- all(grepl("^S[0-9]{3}$", x$site_id))
  student_ids_synthetic <- all(grepl("^ST[0-9]{5}$", x$student_id))
  restricted_note_present <- any(grepl(
    "Restricted source panel: not shipped",
    provenance,
    fixed = TRUE
  ))
  rows <- list(
    data_fixture_contract_row(
      "package_data_dimensions",
      "7312x7",
      paste(dim(x), collapse = "x"),
      identical(dim(x), c(7312L, 7L))
    ),
    data_fixture_contract_row(
      "package_data_schema",
      package_schema,
      observed_package_schema,
      identical(names(x), .data_fixture_public_schema)
    ),
    data_fixture_contract_row(
      "portable_csv_values",
      "identical",
      if (csv_exact) "identical" else "different",
      csv_exact
    ),
    data_fixture_contract_row(
      "count_data_dimensions",
      "250x13",
      paste(dim(counts), collapse = "x"),
      identical(dim(counts), c(250L, 13L))
    ),
    data_fixture_contract_row(
      "count_data_schema",
      count_schema,
      observed_count_schema,
      identical(names(counts), .data_fixture_count_schema)
    ),
    data_fixture_contract_row(
      "count_recomputation",
      "identical",
      if (counts_exact) "identical" else "different",
      counts_exact
    ),
    data_fixture_contract_row(
      "build_info_shared",
      "identical",
      if (build_info_exact) "identical" else "different",
      build_info_exact
    ),
    data_fixture_contract_row(
      "builder_seed",
      "2026",
      info$seed,
      identical(info$seed, 2026L)
    ),
    data_fixture_contract_row(
      "source_digest",
      source_md5,
      info$source_digest,
      identical(info$source_digest, source_md5)
    ),
    data_fixture_contract_row(
      "candidate_strata",
      "49|327|290",
      paste(candidate_strata, collapse = "|"),
      identical(candidate_strata, c(49L, 327L, 290L))
    ),
    data_fixture_contract_row(
      "sampled_strata",
      "10|20|20",
      paste(sampled_strata, collapse = "|"),
      identical(sampled_strata, c(10L, 20L, 20L))
    ),
    data_fixture_contract_row(
      "public_indicator_values",
      "0|1",
      paste(sort(unique(indicator_values)), collapse = "|"),
      all(indicator_values %in% c(0L, 1L))
    ),
    data_fixture_contract_row(
      "original_identifiers_absent",
      "TRUE",
      original_ids_absent,
      original_ids_absent
    ),
    data_fixture_contract_row(
      "synthetic_site_ids",
      "^S[0-9]{3}$",
      site_ids_synthetic,
      site_ids_synthetic
    ),
    data_fixture_contract_row(
      "synthetic_student_ids",
      "^ST[0-9]{5}$",
      student_ids_synthetic,
      student_ids_synthetic
    ),
    data_fixture_contract_row(
      "restricted_source_not_shipped_note",
      "present",
      restricted_note_present,
      restricted_note_present
    ),
    data_fixture_contract_row(
      "sibling_csv_disposition",
      "differs_from_rds",
      info$sibling_csv_note,
      identical(info$sibling_csv_note, "differs_from_rds")
    )
  )
  do.call(rbind, rows)
}

data_fixture_restricted_status <- function(manifest_result, source_arg, replay_requested) {
  row <- manifest_result$table[
    manifest_result$table$artifact_role == "restricted_canonical_input",
    ,
    drop = FALSE
  ]
  supplied <- nzchar(source_arg)
  present <- supplied && file.exists(path.expand(source_arg))
  exact <- present && identical(row$checksum_status, "EXACT") &&
    identical(row$schema_status, "EXACT")
  source_status <- if (!supplied) {
    "NOT_SUPPLIED"
  } else if (!present) {
    "SUPPLIED_PATH_MISSING"
  } else if (!exact) {
    "SUPPLIED_SOURCE_CONTRACT_FAILED"
  } else {
    "SUPPLIED_SOURCE_VALIDATED"
  }
  companion_path <- if (supplied) {
    sub("[.]rds$", ".csv", path.expand(source_arg))
  } else {
    NA_character_
  }
  companion_present <- supplied && file.exists(companion_path)
  companion_md5 <- if (companion_present) {
    data_fixture_md5(companion_path)
  } else {
    NA_character_
  }
  companion_sha256 <- if (companion_present) {
    data_fixture_sha256(companion_path)
  } else {
    NA_character_
  }
  companion_rows <- if (companion_present) {
    data_fixture_count_lines(companion_path) - 1L
  } else {
    NA_integer_
  }
  companion_exact <- companion_present &&
    identical(companion_md5, .data_fixture_companion_md5) &&
    identical(companion_sha256, .data_fixture_companion_sha256) &&
    identical(companion_rows, .data_fixture_companion_rows)
  companion_status <- if (!supplied) {
    "NOT_CHECKED"
  } else if (!companion_present) {
    "COMPANION_CSV_MISSING"
  } else if (!companion_exact) {
    "COMPANION_CSV_CONTRACT_FAILED"
  } else {
    "COMPANION_CSV_VALIDATED_EXPECTED_RDS_ROW_DIFFERENCE"
  }
  data.frame(
    source_supplied = supplied,
    source_available = present,
    source_basename = if (supplied) basename(source_arg) else "not_supplied",
    source_status = source_status,
    recorded_md5 = row$md5,
    observed_md5 = if (present) row$observed_md5 else NA_character_,
    recorded_sha256 = row$sha256,
    observed_sha256 = if (present) row$observed_sha256 else NA_character_,
    identity_and_schema_exact = if (present) exact else NA,
    companion_csv_status = companion_status,
    companion_csv_available = companion_present,
    companion_csv_rows = companion_rows,
    companion_csv_recorded_md5 = .data_fixture_companion_md5,
    companion_csv_observed_md5 = companion_md5,
    companion_csv_recorded_sha256 = .data_fixture_companion_sha256,
    companion_csv_observed_sha256 = companion_sha256,
    replay_source_ready = exact && companion_exact,
    replay_requested = replay_requested,
    stringsAsFactors = FALSE
  )
}

data_fixture_public_replay <- function(
  root,
  source_arg,
  source_status,
  replay_requested,
  work_dir
) {
  replay_root <- file.path(work_dir, "public-replay")
  data_dir <- file.path(replay_root, "data")
  extdata_dir <- file.path(replay_root, "inst", "extdata")
  audit_dir <- file.path(replay_root, "audit")
  dir.create(data_dir, recursive = TRUE)
  dir.create(extdata_dir, recursive = TRUE)
  dir.create(audit_dir, recursive = TRUE)
  log_path <- file.path(work_dir, "public-replay.log")
  current <- file.path(root, .data_fixture_public_paths)
  replay <- c(
    file.path(data_dir, "alprek_subset.rda"),
    file.path(extdata_dir, "alprek_subset.csv"),
    file.path(extdata_dir, "alprek_subset_counts.rds"),
    file.path(extdata_dir, "alprek_subset_provenance.txt")
  )
  current_writer <- c(
    "not_available_for_rda",
    "not_applicable",
    as.character(infoRDS(current[[3L]])$writer_version),
    "not_applicable"
  )
  if (!replay_requested) {
    writeLines(
      "Public-data replay was not requested; no rebuild claim is made.",
      log_path,
      useBytes = TRUE
    )
    table <- data.frame(
      path = .data_fixture_public_paths,
      replay_status = "NOT_ATTEMPTED",
      content_exact = NA,
      byte_exact = NA,
      current_md5 = data_fixture_md5(current),
      replay_md5 = NA_character_,
      current_sha256 = data_fixture_sha256(current),
      replay_sha256 = NA_character_,
      current_writer = current_writer,
      replay_writer = NA_character_,
      stringsAsFactors = FALSE
    )
    result <- list(
      table = table,
      pass = TRUE,
      status = "NOT_ATTEMPTED",
      claim = "PUBLIC_VALIDATION_ONLY_NO_REBUILD_CLAIM",
      log = log_path,
      replay_root = replay_root,
      expected_warning = NA
    )
  } else if (!isTRUE(source_status$replay_source_ready)) {
    writeLines(
      "Requested replay was blocked because the explicit source did not validate.",
      log_path,
      useBytes = TRUE
    )
    table <- data.frame(
      path = .data_fixture_public_paths,
      replay_status = "BLOCKED_SOURCE_CONTRACT",
      content_exact = NA,
      byte_exact = NA,
      current_md5 = data_fixture_md5(current),
      replay_md5 = NA_character_,
      current_sha256 = data_fixture_sha256(current),
      replay_sha256 = NA_character_,
      current_writer = current_writer,
      replay_writer = NA_character_,
      stringsAsFactors = FALSE
    )
    result <- list(
      table = table,
      pass = FALSE,
      status = "BLOCKED_SOURCE_CONTRACT",
      claim = "NO_REBUILD_CLAIM",
      log = log_path,
      replay_root = replay_root,
      expected_warning = FALSE
    )
  } else {
    status <- system2(
      file.path(R.home("bin"), "Rscript"),
      c(
        "--vanilla",
        shQuote(file.path(root, "inst", "scripts", "build-alprek-subset.R")),
        shQuote(paste0("--source=", normalizePath(source_arg, mustWork = TRUE))),
        shQuote(paste0("--out-data=", replay[[1L]])),
        shQuote(paste0("--out-extdata=", extdata_dir)),
        shQuote(paste0("--audit-dir=", audit_dir))
      ),
      stdout = log_path,
      stderr = log_path
    )
    log_lines <- readLines(log_path, warn = FALSE)
    warning_match <- grepl(
      "Canonical RDS has 116689 rows, but sibling CSV has 116899 data rows.",
      log_lines,
      fixed = TRUE
    )
    expected_warning <- sum(warning_match) == 1L &&
      sum(log_lines == "Warning message:") == 1L &&
      !any(log_lines == "Warning messages:")
    output_exists <- file.exists(replay)
    content_exact <- rep(FALSE, length(replay))
    if (all(output_exists)) {
      current_rda <- data_fixture_load_rda(current[[1L]])
      replay_rda <- data_fixture_load_rda(replay[[1L]])
      current_csv <- utils::read.csv(current[[2L]], stringsAsFactors = FALSE, check.names = FALSE)
      replay_csv <- utils::read.csv(replay[[2L]], stringsAsFactors = FALSE, check.names = FALSE)
      current_counts <- readRDS(current[[3L]])
      replay_counts <- readRDS(replay[[3L]])
      content_exact <- c(
        identical(current_rda, replay_rda),
        identical(current_csv, replay_csv),
        identical(current_counts, replay_counts),
        identical(readLines(current[[4L]], warn = FALSE), readLines(replay[[4L]], warn = FALSE))
      )
    }
    byte_exact <- vapply(seq_along(current), function(index) {
      if (!output_exists[[index]]) {
        return(FALSE)
      }
      identical(
        data_fixture_sha256(current[[index]]),
        data_fixture_sha256(replay[[index]])
      )
    }, logical(1))
    replay_md5 <- rep(NA_character_, length(replay))
    replay_sha256 <- rep(NA_character_, length(replay))
    replay_md5[output_exists] <- data_fixture_md5(replay[output_exists])
    replay_sha256[output_exists] <- data_fixture_sha256(replay[output_exists])
    pass <- identical(as.integer(status), 0L) && all(output_exists) &&
      all(content_exact) && all(byte_exact[c(2L, 4L)]) && expected_warning
    replay_writer <- rep(NA_character_, length(replay))
    replay_writer[c(2L, 4L)] <- "not_applicable"
    replay_writer[[1L]] <- "not_available_for_rda"
    if (output_exists[[3L]]) {
      replay_writer[[3L]] <- as.character(infoRDS(replay[[3L]])$writer_version)
    }
    table <- data.frame(
      path = .data_fixture_public_paths,
      replay_status = if (pass) "CONTENT_REPLAY_VERIFIED" else "REPLAY_FAILED",
      content_exact = content_exact,
      byte_exact = byte_exact,
      current_md5 = data_fixture_md5(current),
      replay_md5 = replay_md5,
      current_sha256 = data_fixture_sha256(current),
      replay_sha256 = replay_sha256,
      current_writer = current_writer,
      replay_writer = replay_writer,
      stringsAsFactors = FALSE
    )
    result <- list(
      table = table,
      pass = pass,
      status = if (pass) "CONTENT_REPLAY_VERIFIED" else "REPLAY_FAILED",
      claim = if (pass) {
        "CONTENT_REPLAY_VERIFIED_BINARY_BYTES_NOT_CROSS_R_CONTRACT"
      } else {
        "NO_REBUILD_CLAIM"
      },
      log = log_path,
      replay_root = replay_root,
      expected_warning = expected_warning
    )
  }
  result
}

data_fixture_baseline_contract <- function(root) {
  baseline_path <- file.path(
    root,
    "tests", "testthat", "_data", "regression", "regression-baselines.rds"
  )
  counts_path <- file.path(root, "inst", "extdata", "alprek_subset_counts.rds")
  baseline <- readRDS(baseline_path)
  expected_sha256 <- .data_fixture_protected_sha256[[
    "tests/testthat/_data/regression/regression-baselines.rds"
  ]]
  observed_sha256 <- data_fixture_sha256(baseline_path)
  expected_schema <- paste(.data_fixture_baseline_schema, collapse = "|")
  observed_schema <- paste(names(baseline), collapse = "|")
  expected_counts_md5 <- data_fixture_md5(counts_path)
  writer_version <- as.character(infoRDS(baseline_path)$writer_version)
  rows <- list(
    data_fixture_contract_row(
      "protected_baseline_sha256",
      expected_sha256,
      observed_sha256,
      identical(observed_sha256, expected_sha256)
    ),
    data_fixture_contract_row(
      "baseline_schema",
      expected_schema,
      observed_schema,
      identical(names(baseline), .data_fixture_baseline_schema)
    ),
    data_fixture_contract_row(
      "fixture_version",
      "2",
      baseline$metadata$fixture_version,
      identical(baseline$metadata$fixture_version, 2L)
    ),
    data_fixture_contract_row(
      "counts_file",
      "alprek_subset_counts.rds",
      baseline$metadata$counts_file,
      identical(baseline$metadata$counts_file, "alprek_subset_counts.rds")
    ),
    data_fixture_contract_row(
      "counts_md5",
      expected_counts_md5,
      baseline$metadata$counts_md5,
      identical(baseline$metadata$counts_md5, expected_counts_md5)
    ),
    data_fixture_contract_row(
      "scalar_tolerance",
      "1e-12",
      format(baseline$tolerances$scalar, scientific = TRUE),
      identical(baseline$tolerances$scalar, 1e-12)
    ),
    data_fixture_contract_row(
      "matrix_tolerance",
      "1e-10",
      format(baseline$tolerances$matrix, scientific = TRUE),
      identical(baseline$tolerances$matrix, 1e-10)
    ),
    data_fixture_contract_row(
      "baseline_writer",
      "4.6.0",
      writer_version,
      identical(writer_version, "4.6.0")
    ),
    data_fixture_contract_row(
      "numeric_baseline_regenerated",
      "FALSE",
      "FALSE",
      TRUE
    )
  )
  list(table = do.call(rbind, rows), baseline = baseline)
}

data_fixture_review_replay <- function(root, baseline, work_dir) {
  replay_dir <- file.path(work_dir, "regression-review-replay")
  dir.create(replay_dir, recursive = TRUE)
  helper <- file.path(root, "tests", "testthat", "helper-regression.R")
  helper_sha256 <- data_fixture_sha256(helper)
  if (!identical(helper_sha256, .data_fixture_helper_sha256)) {
    stop(
      "Regression review helper disagrees with its approved SHA-256.",
      call. = FALSE
    )
  }
  helper_env <- new.env(parent = globalenv())
  sys.source(helper, envir = helper_env)
  helper_env$regression_write_review_csvs(baseline, replay_dir)
  current <- file.path(root, .data_fixture_review_paths)
  replay <- file.path(replay_dir, basename(.data_fixture_review_paths))
  exists <- file.exists(replay)
  byte_exact <- vapply(seq_along(current), function(index) {
    if (!exists[[index]]) {
      return(FALSE)
    }
    identical(
      data_fixture_sha256(current[[index]]),
      data_fixture_sha256(replay[[index]])
    )
  }, logical(1))
  replay_md5 <- rep(NA_character_, length(replay))
  replay_sha256 <- rep(NA_character_, length(replay))
  replay_md5[exists] <- data_fixture_md5(replay[exists])
  replay_sha256[exists] <- data_fixture_sha256(replay[exists])
  data.frame(
    path = .data_fixture_review_paths,
    replay_exists = exists,
    byte_exact = byte_exact,
    current_md5 = data_fixture_md5(current),
    replay_md5 = replay_md5,
    current_sha256 = data_fixture_sha256(current),
    replay_sha256 = replay_sha256,
    numeric_baseline_regenerated = FALSE,
    stringsAsFactors = FALSE
  )
}

data_fixture_build_source <- function(root, work_dir) {
  build_dir <- file.path(work_dir, "source-build")
  dir.create(build_dir)
  log_path <- file.path(work_dir, "source-build.log")
  old <- setwd(build_dir)
  on.exit(setwd(old), add = TRUE)
  status <- system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD", "build", "--no-build-vignettes", "--no-manual",
      shQuote(root)
    ),
    stdout = log_path,
    stderr = log_path
  )
  tarballs <- list.files(build_dir, pattern = "[.]tar[.]gz$", full.names = TRUE)
  if (!identical(as.integer(status), 0L) || length(tarballs) != 1L) {
    stop("Source-package build failed; see source-build.log.", call. = FALSE)
  }
  contents <- utils::untar(tarballs[[1L]], list = TRUE)
  relative_contents <- sub("^[^/]+/", "", contents)
  content_table <- data.frame(
    archive_path = contents,
    relative_path = relative_contents,
    stringsAsFactors = FALSE
  )
  list(
    tarball = tarballs[[1L]],
    sha256 = data_fixture_sha256(tarballs[[1L]]),
    contents = contents,
    relative_contents = relative_contents,
    content_table = content_table,
    log = log_path
  )
}

data_fixture_source_disposition <- function(source_build) {
  relative_contents <- source_build$relative_contents
  exact_present <- function(path) {
    path %in% relative_contents
  }
  public_builder <- "inst/scripts/build-alprek-subset.R"
  numeric_builder <- "inst/scripts/build-regression-baselines.R"
  audit_script <- "inst/scripts/audit-data-fixture-provenance.R"
  manifest_path <- "inst/gates/data-fixture-provenance.csv"
  maintenance_note <- "inst/gates/data-fixture-provenance.md"
  protected_baseline <-
    "tests/testthat/_data/regression/regression-baselines.rds"
  restricted_pattern <- "student_panel_2021-2026|dev/data-ALprek-example"
  restricted_present <- any(grepl(restricted_pattern, relative_contents))
  public_present <- vapply(
    .data_fixture_public_paths,
    exact_present,
    logical(1)
  )
  review_present <- vapply(
    .data_fixture_review_paths,
    exact_present,
    logical(1)
  )
  rows <- list(
    data_fixture_contract_row(
      "public_data_builder_included",
      "TRUE",
      exact_present(public_builder),
      exact_present(public_builder)
    ),
    data_fixture_contract_row(
      "numeric_builder_excluded",
      "FALSE",
      exact_present(numeric_builder),
      !exact_present(numeric_builder)
    ),
    data_fixture_contract_row(
      "restricted_source_excluded",
      "FALSE",
      restricted_present,
      !restricted_present
    ),
    data_fixture_contract_row(
      "public_data_artifacts_included",
      "4",
      sum(public_present),
      all(public_present)
    ),
    data_fixture_contract_row(
      "audit_script_included",
      "TRUE",
      exact_present(audit_script),
      exact_present(audit_script)
    ),
    data_fixture_contract_row(
      "provenance_manifest_included",
      "TRUE",
      exact_present(manifest_path),
      exact_present(manifest_path)
    ),
    data_fixture_contract_row(
      "maintenance_note_included",
      "TRUE",
      exact_present(maintenance_note),
      exact_present(maintenance_note)
    ),
    data_fixture_contract_row(
      "review_csvs_included",
      "5",
      sum(review_present),
      all(review_present)
    ),
    data_fixture_contract_row(
      "protected_baseline_included",
      "TRUE",
      exact_present(protected_baseline),
      exact_present(protected_baseline)
    )
  )
  do.call(rbind, rows)
}

data_fixture_git_tracked <- function(root, path) {
  output <- tempfile("sitemix-git-ls-files-")
  on.exit(unlink(output), add = TRUE)
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  status <- suppressWarnings(system2(
    "git",
    c("ls-files", "--error-unmatch", shQuote(path)),
    stdout = output,
    stderr = output
  ))
  status <- as.integer(status)
  if (identical(status, 0L)) {
    return(TRUE)
  }
  if (identical(status, 1L)) {
    return(FALSE)
  }
  stop(
    "Git index inspection failed for `", path, "` with status ", status, ".",
    call. = FALSE
  )
}

data_fixture_builder_disposition <- function(root, source_build) {
  paths <- c(
    "inst/scripts/build-alprek-subset.R",
    "inst/scripts/build-regression-baselines.R"
  )
  source_included <- vapply(paths, function(path) {
    path %in% source_build$relative_contents
  }, logical(1))
  git_tracked <- vapply(paths, data_fixture_git_tracked, logical(1), root = root)
  data.frame(
    path = paths,
    present_in_worktree = file.exists(file.path(root, paths)),
    git_index_status = ifelse(git_tracked, "tracked", "untracked_pending_handoff"),
    source_package_status = ifelse(source_included, "included", "excluded"),
    expected_source_package_status = c("included", "excluded"),
    execution_policy = c(
      "explicit_validated_restricted_source_to_fresh_temp_only",
      "never_execute_in_automated_audit"
    ),
    git_contract_pass = c(git_tracked[[1L]], TRUE),
    handoff_action_required = !git_tracked,
    sha256 = data_fixture_sha256(file.path(root, paths)),
    stringsAsFactors = FALSE
  )
}

data_fixture_protected_manifest <- function(root) {
  paths <- file.path(root, names(.data_fixture_protected_sha256))
  present <- file.exists(paths)
  observed <- rep(NA_character_, length(paths))
  observed[present] <- data_fixture_sha256(paths[present])
  data.frame(
    path = names(.data_fixture_protected_sha256),
    expected_sha256 = unname(.data_fixture_protected_sha256),
    observed_sha256 = observed,
    exact = present & observed == unname(.data_fixture_protected_sha256),
    stringsAsFactors = FALSE
  )
}

data_fixture_toolchain <- function(root) {
  description <- read.dcf(file.path(root, "DESCRIPTION"))
  source_version <- unname(description[1L, "Version"])
  installed_version <- tryCatch(
    as.character(utils::packageVersion("sitemix")),
    error = function(error) "not_installed"
  )
  rng <- RNGkind()
  counts_path <- file.path(
    root,
    "inst", "extdata", "alprek_subset_counts.rds"
  )
  baseline_path <- file.path(
    root,
    "tests", "testthat", "_data", "regression", "regression-baselines.rds"
  )
  counts_writer <- as.character(infoRDS(counts_path)$writer_version)
  baseline_writer <- as.character(infoRDS(baseline_path)$writer_version)
  data.frame(
    component = c(
      "validation_session_role", "source_package_version",
      "installed_sitemix_version", "installed_matches_source", "R",
      "platform", "locale", "rng_kind", "normal_kind", "sample_kind",
      "counts_rds_writer", "baseline_rds_writer", "tibble"
    ),
    value = c(
      "current_validation_session_not_original_build_session",
      source_version,
      installed_version,
      as.character(identical(installed_version, source_version)),
      as.character(getRversion()),
      R.version$platform,
      paste(Sys.getlocale(), collapse = ";"),
      rng[[1L]], rng[[2L]], rng[[3L]],
      counts_writer,
      baseline_writer,
      if (requireNamespace("tibble", quietly = TRUE)) {
        as.character(utils::packageVersion("tibble"))
      } else {
        "not_installed"
      }
    ),
    stringsAsFactors = FALSE
  )
}

data_fixture_source_manifest <- function(root) {
  relative <- c(
    "inst/gates/data-fixture-provenance.csv",
    "inst/gates/data-fixture-provenance.md",
    "inst/scripts/audit-data-fixture-provenance.R",
    "inst/scripts/build-alprek-subset.R",
    "inst/scripts/build-regression-baselines.R",
    "tests/testthat/helper-regression.R",
    "tests/testthat/test-data-alprek.R",
    "tests/testthat/test-regression.R",
    "tests/testthat/test-test-architecture.R",
    "tests/testthat/_data/test-architecture/fixture-provenance.csv",
    ".Rbuildignore",
    .data_fixture_public_paths,
    "tests/testthat/_data/regression/regression-baselines.rds",
    .data_fixture_review_paths
  )
  paths <- file.path(root, relative)
  data.frame(
    path = relative,
    size_bytes = file.info(paths)$size,
    md5 = data_fixture_md5(paths),
    sha256 = data_fixture_sha256(paths),
    stringsAsFactors = FALSE
  )
}

data_fixture_summary_markdown <- function(summary) {
  c(
    "# Data and fixture provenance summary",
    "",
    paste0("- Status: **", summary$status, "**"),
    paste0("- Focused manifest rows: ", summary$focused_manifest_rows),
    paste0("- Focused manifest failures: ", summary$focused_manifest_failures),
    paste0("- Broad fixture rows exact: ", summary$broad_fixture_exact, "/26"),
    paste0("- Public contract failures: ", summary$public_contract_failures),
    paste0("- Restricted source status: ", summary$source_status),
    paste0("- Public replay status: ", summary$public_replay_status),
    paste0("- Public replay claim: ", summary$public_replay_claim),
    paste0("- Regression review CSVs exact: ", summary$review_csvs_exact, "/5"),
    paste0("- Numeric baseline regenerated: ", summary$numeric_baseline_regenerated),
    paste0("- Source-package disposition failures: ", summary$source_disposition_failures),
    paste0("- Protected failures: ", summary$protected_failures),
    paste0("- Repository handoff actions: ", summary$repository_handoff_actions)
  )
}

data_fixture_write_outputs <- function(
  target,
  summary,
  focused_manifest,
  broad_inventory,
  public_contract,
  restricted_status,
  public_replay,
  baseline_contract,
  review_replay,
  source_disposition,
  builder_disposition,
  source_build,
  protected,
  toolchain,
  source_manifest,
  work_dir
) {
  staging <- paste0(target, ".tmp-", Sys.getpid())
  if (file.exists(staging) || dir.exists(staging)) {
    stop("Data-fixture staging directory already exists.", call. = FALSE)
  }
  dir.create(staging)
  committed <- FALSE
  on.exit(if (!committed) unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  tables <- list(
    "data-fixture-summary.csv" = summary,
    "focused-provenance-manifest.csv" = focused_manifest,
    "broad-fixture-inventory.csv" = broad_inventory,
    "public-data-contract.csv" = public_contract,
    "restricted-source-status.csv" = restricted_status,
    "public-data-replay.csv" = public_replay$table,
    "regression-baseline-contract.csv" = baseline_contract,
    "regression-review-replay.csv" = review_replay,
    "source-package-disposition.csv" = source_disposition,
    "builder-disposition.csv" = builder_disposition,
    "source-tar-contents.csv" = source_build$content_table,
    "protected-artifacts.csv" = protected,
    "validation-toolchain.csv" = toolchain,
    "data-fixture-source-manifest.csv" = source_manifest
  )
  for (name in names(tables)) {
    utils::write.csv(tables[[name]], file.path(staging, name), row.names = FALSE)
  }
  writeLines(
    data_fixture_summary_markdown(summary),
    file.path(staging, "data-fixture-summary.md"),
    useBytes = TRUE
  )
  writeLines(
    capture.output(utils::sessionInfo()),
    file.path(staging, "validation-session.txt"),
    useBytes = TRUE
  )
  saveRDS(
    list(
      summary = summary,
      focused_manifest = focused_manifest,
      broad_inventory = broad_inventory,
      public_contract = public_contract,
      restricted_source = restricted_status,
      public_replay = public_replay$table,
      baseline_contract = baseline_contract,
      review_replay = review_replay,
      source_disposition = source_disposition,
      builder_disposition = builder_disposition,
      protected = protected,
      toolchain = toolchain,
      source_manifest = source_manifest,
      source_tarball_sha256 = source_build$sha256,
      numeric_baseline_regenerated = FALSE
    ),
    file.path(staging, "data-fixture-evidence.rds"),
    version = 3
  )
  logs_copied <- file.copy(
    c(source_build$log, public_replay$log),
    file.path(staging, c("source-build.log", "public-replay.log"))
  )
  if (!all(logs_copied)) {
    stop("Could not copy data-fixture execution logs.", call. = FALSE)
  }
  review_source <- file.path(work_dir, "regression-review-replay")
  if (!file.copy(review_source, staging, recursive = TRUE)) {
    stop("Could not copy regression review replay outputs.", call. = FALSE)
  }
  replay_files <- list.files(
    public_replay$replay_root,
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(replay_files)) {
    if (!file.copy(public_replay$replay_root, staging, recursive = TRUE)) {
      stop("Could not copy public replay outputs.", call. = FALSE)
    }
  }
  if (!file.rename(staging, target)) {
    stop("Could not atomically commit data-fixture artifacts.", call. = FALSE)
  }
  committed <- TRUE
  normalizePath(target, mustWork = TRUE)
}

data_fixture_main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  data_fixture_validate_args(args)
  root <- data_fixture_root()
  target <- data_fixture_output_target(data_fixture_arg_value(args, "out-dir"))
  source_arg <- data_fixture_arg_value(args, "source", "")
  replay_requested <- data_fixture_bool(
    data_fixture_arg_value(args, "replay-public-data", "FALSE"),
    "replay-public-data"
  )
  package <- unname(read.dcf(file.path(root, "DESCRIPTION"))[1L, "Package"])
  if (!identical(package, "sitemix")) {
    stop("Data-fixture audit requires the sitemix source tree.", call. = FALSE)
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Data-fixture audit requires package `tibble`.", call. = FALSE)
  }

  protected_before <- data_fixture_protected_manifest(root)
  work_dir <- tempfile("sitemix-data-fixture-work-")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

  focused <- data_fixture_manifest(root, source_arg)
  broad <- data_fixture_broad_inventory(root)
  public_contract <- data_fixture_public_contract(root)
  restricted <- data_fixture_restricted_status(focused, source_arg, replay_requested)
  replay <- data_fixture_public_replay(
    root,
    source_arg,
    restricted,
    replay_requested,
    work_dir
  )
  baseline <- data_fixture_baseline_contract(root)
  review <- data_fixture_review_replay(root, baseline$baseline, work_dir)
  source_build <- data_fixture_build_source(root, work_dir)
  source_disposition <- data_fixture_source_disposition(source_build)
  builder_disposition <- data_fixture_builder_disposition(root, source_build)
  protected_after <- data_fixture_protected_manifest(root)
  protected <- protected_after
  protected$before_sha256 <- protected_before$observed_sha256
  protected$unchanged_during_gate <-
    protected_before$observed_sha256 == protected_after$observed_sha256
  toolchain <- data_fixture_toolchain(root)
  source_manifest <- data_fixture_source_manifest(root)

  source_failure <- restricted$source_supplied &&
    !identical(restricted$source_status, "SUPPLIED_SOURCE_VALIDATED")
  focused_failures <- length(focused$issues)
  broad_failures <- length(broad$issues)
  public_failures <- sum(!public_contract$pass)
  baseline_failures <- sum(!baseline$table$pass)
  review_failures <- sum(!review$byte_exact | !review$replay_exists)
  source_disposition_failures <- sum(!source_disposition$pass)
  builder_failures <- sum(
    !builder_disposition$present_in_worktree |
      !builder_disposition$git_contract_pass |
      builder_disposition$source_package_status !=
        builder_disposition$expected_source_package_status
  )
  protected_failures <- sum(
    !protected$exact | !protected$unchanged_during_gate
  )
  status <- if (
    focused_failures || broad_failures || public_failures || source_failure ||
      !replay$pass || baseline_failures || review_failures ||
      source_disposition_failures || builder_failures || protected_failures
  ) "FAIL" else "PASS"
  summary <- data.frame(
    status = status,
    focused_manifest_rows = nrow(focused$table),
    focused_manifest_failures = focused_failures,
    broad_fixture_exact = sum(broad$table$exact),
    broad_fixture_failures = broad_failures,
    public_contract_failures = public_failures,
    source_status = restricted$source_status,
    public_replay_status = replay$status,
    public_replay_claim = replay$claim,
    public_replay_content_exact = sum(replay$table$content_exact %in% TRUE),
    public_replay_byte_exact = sum(replay$table$byte_exact %in% TRUE),
    baseline_contract_failures = baseline_failures,
    review_csvs_exact = sum(review$byte_exact),
    numeric_baseline_regenerated = FALSE,
    source_disposition_failures = source_disposition_failures,
    builder_disposition_failures = builder_failures,
    protected_failures = protected_failures,
    repository_handoff_actions = sum(builder_disposition$handoff_action_required),
    source_tarball_sha256 = source_build$sha256,
    stringsAsFactors = FALSE
  )
  artifact_dir <- data_fixture_write_outputs(
    target,
    summary,
    focused$table,
    broad$table,
    public_contract,
    restricted,
    replay,
    baseline$table,
    review,
    source_disposition,
    builder_disposition,
    source_build,
    protected,
    toolchain,
    source_manifest,
    work_dir
  )
  print(summary, row.names = FALSE)
  cat("Artifacts: ", artifact_dir, "\n", sep = "")
  if (identical(status, "PASS")) 0L else 1L
}

data_fixture_entry <- function() {
  tryCatch(
    data_fixture_main(),
    error = function(error) {
      message("data-fixture provenance contract error: ", conditionMessage(error))
      2L
    }
  )
}

if (sys.nframe() == 0L) {
  quit(save = "no", status = data_fixture_entry(), runLast = FALSE)
}
