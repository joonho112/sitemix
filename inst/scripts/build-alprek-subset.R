#!/usr/bin/env Rscript

# Deterministic builder for the package's public Alabama Pre-K sample data.
# The restricted source panel is not shipped with the package.

option_value <- function(args, name, default = NULL) {
  eq_prefix <- paste0(name, "=")
  eq_idx <- which(startsWith(args, eq_prefix))
  if (length(eq_idx) > 0L) {
    return(sub(eq_prefix, "", args[[eq_idx[[length(eq_idx)]]]], fixed = TRUE))
  }

  bare_idx <- which(args == name)
  if (length(bare_idx) > 0L) {
    idx <- bare_idx[[length(bare_idx)]] + 1L
    if (idx <= length(args)) {
      return(args[[idx]])
    }
  }

  default
}

abort_build <- function(message) {
  stop(message, call. = FALSE)
}

expect_identical <- function(actual, expected, label) {
  if (!identical(actual, expected)) {
    abort_build(sprintf(
      "%s drifted: expected %s, got %s.",
      label,
      paste(expected, collapse = ", "),
      paste(actual, collapse = ", ")
    ))
  }
}

count_lines <- function(path) {
  con <- file(path, open = "rt")
  on.exit(close(con), add = TRUE)

  n <- 0L
  repeat {
    lines <- readLines(con, n = 100000L, warn = FALSE)
    if (length(lines) == 0L) {
      break
    }
    n <- n + length(lines)
  }
  n
}

as_tibble <- function(x) {
  if (requireNamespace("tibble", quietly = TRUE)) {
    tibble::as_tibble(x)
  } else {
    x
  }
}

write_md5_input <- function(values) {
  tmp <- tempfile(fileext = ".txt")
  writeLines(values, con = tmp, useBytes = TRUE)
  tmp
}

args <- commandArgs(trailingOnly = TRUE)
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
default_source <- file.path(root, "dev/data-ALprek-example/student_panel_2021-2026.rds")

source_path <- option_value(
  args,
  "--source",
  Sys.getenv("ALPREK_PANEL_RDS", unset = default_source)
)
out_data <- option_value(args, "--out-data", file.path(root, "data/alprek_subset.rda"))
out_extdata <- option_value(args, "--out-extdata", file.path(root, "inst/extdata"))
audit_dir <- option_value(
  args,
  "--audit-dir",
  file.path(root, "dev/data-ALprek-example/audit")
)

source_path <- normalizePath(source_path, winslash = "/", mustWork = FALSE)
if (!file.exists(source_path)) {
  abort_build(sprintf("Source panel not found: %s", source_path))
}

dir.create(dirname(out_data), recursive = TRUE, showWarnings = FALSE)
dir.create(out_extdata, recursive = TRUE, showWarnings = FALSE)
dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)

raw_obj <- readRDS(source_path)
panel <- if (is.list(raw_obj) && !is.data.frame(raw_obj) && "data" %in% names(raw_obj)) {
  raw_obj$data
} else {
  raw_obj
}

required_cols <- c(
  "adece_id",
  "site_code",
  "year",
  "free_reduced_lunch",
  "snap",
  "wic",
  "tanf"
)
missing_cols <- setdiff(required_cols, names(panel))
if (length(missing_cols) > 0L) {
  abort_build(sprintf("Source panel is missing required columns: %s", paste(missing_cols, collapse = ", ")))
}

expect_identical(as.integer(dim(panel)), c(116689L, 261L), "source panel dimensions")
expect_identical(sort(unique(as.integer(panel$year))), 2021:2025, "source school years")
expect_identical(length(unique(panel$adece_id)), 116378L, "unique source students")
expect_identical(length(unique(panel$site_code)), 948L, "unique source sites")

indicator_source <- c(
  frpm = "free_reduced_lunch",
  snap = "snap",
  wic = "wic",
  tanf = "tanf"
)
indicators <- names(indicator_source)
raw_missing <- colSums(is.na(panel[, unname(indicator_source), drop = FALSE]))

csv_path <- sub("[.]rds$", ".csv", source_path)
csv_rows <- NA_integer_
csv_row_note <- "not_checked"
if (file.exists(csv_path)) {
  csv_rows <- count_lines(csv_path) - 1L
  csv_row_note <- if (identical(csv_rows, nrow(panel))) "matches_rds" else "differs_from_rds"
  if (!identical(csv_rows, nrow(panel))) {
    warning(
      sprintf(
        "Canonical RDS has %s rows, but sibling CSV has %s data rows. Continuing with RDS.",
        nrow(panel),
        csv_rows
      ),
      call. = FALSE
    )
  }
}

work <- data.frame(
  adece_id = as.character(panel$adece_id),
  site_code = as.character(panel$site_code),
  year = as.integer(panel$year),
  frpm = as.integer(panel[[indicator_source[["frpm"]]]]),
  snap = as.integer(panel[[indicator_source[["snap"]]]]),
  wic = as.integer(panel[[indicator_source[["wic"]]]]),
  tanf = as.integer(panel[[indicator_source[["tanf"]]]]),
  stringsAsFactors = FALSE
)

indicator_values <- unlist(work[indicators], use.names = FALSE)
bad_indicator_values <- !is.na(indicator_values) & !(indicator_values %in% c(0L, 1L))
if (any(bad_indicator_values)) {
  abort_build("Source indicators must be coded as 0/1 with optional missing values.")
}

site_has_missing <- tapply(
  !stats::complete.cases(work[indicators]),
  work$site_code,
  any
)
eligible_sites <- names(site_has_missing)[!site_has_missing]
work <- work[work$site_code %in% eligible_sites, , drop = FALSE]

cell_counts <- stats::aggregate(
  adece_id ~ site_code + year,
  data = work,
  FUN = length
)
names(cell_counts)[names(cell_counts) == "adece_id"] <- "n_jt"

complete_site <- tapply(
  cell_counts$year,
  cell_counts$site_code,
  function(years) identical(sort(unique(as.integer(years))), 2021:2025)
)
complete_sites <- names(complete_site)[complete_site]
cell_counts <- cell_counts[cell_counts$site_code %in% complete_sites, , drop = FALSE]

site_summaries <- do.call(
  rbind,
  lapply(split(cell_counts$n_jt, cell_counts$site_code), function(n) {
    c(min_n = min(n), median_n = stats::median(n), total_n = sum(n))
  })
)
site_summaries <- data.frame(
  site_code = rownames(site_summaries),
  site_summaries,
  stringsAsFactors = FALSE,
  row.names = NULL
)
site_summaries$stratum <- ifelse(
  site_summaries$min_n < 10,
  "small",
  ifelse(site_summaries$median_n < 30, "medium", "large")
)

expected_candidate_counts <- c(small = 49L, medium = 327L, large = 290L)
candidate_counts <- table(factor(site_summaries$stratum, levels = names(expected_candidate_counts)))
expect_identical(as.integer(candidate_counts), as.integer(expected_candidate_counts), "stratum candidate counts")
names(candidate_counts) <- names(expected_candidate_counts)

set.seed(2026)
sample_plan <- c(small = 10L, medium = 20L, large = 20L)
sampled_sites <- unlist(
  lapply(names(sample_plan), function(stratum) {
    candidates <- sort(site_summaries$site_code[site_summaries$stratum == stratum], method = "radix")
    sample(candidates, size = sample_plan[[stratum]], replace = FALSE)
  }),
  use.names = FALSE
)

sampled_summary <- site_summaries[match(sampled_sites, site_summaries$site_code), , drop = FALSE]
sampled_summary$stratum <- factor(sampled_summary$stratum, levels = names(sample_plan), ordered = TRUE)
sampled_summary <- sampled_summary[
  order(sampled_summary$stratum, sampled_summary$total_n, sampled_summary$site_code),
  ,
  drop = FALSE
]
sampled_summary$site_id <- sprintf("S%03d", seq_len(nrow(sampled_summary)))

site_map <- setNames(sampled_summary$site_id, sampled_summary$site_code)
selected <- work[work$site_code %in% sampled_summary$site_code, , drop = FALSE]
selected$site_id <- unname(site_map[selected$site_code])

student_keys <- unique(selected[, c("site_id", "adece_id"), drop = FALSE])
student_keys <- student_keys[order(student_keys$site_id, student_keys$adece_id), , drop = FALSE]
student_keys$student_id <- sprintf("ST%05d", seq_len(nrow(student_keys)))
student_map <- setNames(student_keys$student_id, paste(student_keys$site_id, student_keys$adece_id, sep = "\r"))
selected$student_id <- unname(student_map[paste(selected$site_id, selected$adece_id, sep = "\r")])

selected <- selected[order(selected$site_id, selected$year, selected$student_id), , drop = FALSE]
alprek_subset <- selected[, c("student_id", "site_id", "year", indicators), drop = FALSE]
row.names(alprek_subset) <- NULL
alprek_subset <- as_tibble(alprek_subset)

groups <- split(
  seq_len(nrow(alprek_subset)),
  paste(alprek_subset$site_id, alprek_subset$year, sep = "\r")
)
groups <- groups[order(names(groups))]

pair_index <- utils::combn(indicators, 2, simplify = FALSE)
count_rows <- lapply(groups, function(idx) {
  row <- data.frame(
    site_id = alprek_subset$site_id[[idx[[1]]]],
    year = as.integer(alprek_subset$year[[idx[[1]]]]),
    n_jt = as.integer(length(idx)),
    stringsAsFactors = FALSE
  )

  for (indicator in indicators) {
    row[[paste0("c_jt_", indicator)]] <- as.integer(sum(alprek_subset[[indicator]][idx]))
  }
  for (pair in pair_index) {
    row[[paste0("c_jt_", pair[[1]], "_", pair[[2]])]] <-
      as.integer(sum(alprek_subset[[pair[[1]]]][idx] * alprek_subset[[pair[[2]]]][idx]))
  }
  row
})
alprek_subset_counts <- do.call(rbind, count_rows)
alprek_subset_counts <- alprek_subset_counts[
  order(alprek_subset_counts$site_id, alprek_subset_counts$year),
  ,
  drop = FALSE
]
row.names(alprek_subset_counts) <- NULL
alprek_subset_counts <- as_tibble(alprek_subset_counts)

source_digest <- unname(tools::md5sum(source_path))
site_digest_file <- write_md5_input(sort(sampled_summary$site_code, method = "radix"))
on.exit(unlink(site_digest_file), add = TRUE)
selected_site_digest <- unname(tools::md5sum(site_digest_file))

stratum_counts <- table(factor(sampled_summary$stratum, levels = names(sample_plan)))
names(stratum_counts) <- names(sample_plan)
site_year_n <- alprek_subset_counts$n_jt
disclosure_audit <- list(
  public_columns_only = identical(names(alprek_subset), c("student_id", "site_id", "year", indicators)),
  no_original_identifier_columns = !any(c("adece_id", "site_code") %in% names(alprek_subset)),
  synthetic_site_id_format = all(grepl("^S[0-9]{3}$", alprek_subset$site_id)),
  synthetic_student_id_format = all(grepl("^ST[0-9]{5}$", alprek_subset$student_id)),
  selected_site_count = length(unique(alprek_subset$site_id)),
  selected_year_count = length(unique(alprek_subset$year)),
  min_site_year_n = min(site_year_n),
  max_site_year_n = max(site_year_n),
  tanf_zero_cell_count = sum(alprek_subset_counts$c_jt_tanf == 0L)
)

build_info <- list(
  build_script = "inst/scripts/build-alprek-subset.R",
  build_script_version = 1L,
  seed = 2026L,
  source_basename = basename(source_path),
  source_digest_algorithm = "MD5",
  source_digest = source_digest,
  source_panel_rows = nrow(panel),
  source_panel_cols = ncol(panel),
  source_unique_students = length(unique(panel$adece_id)),
  source_unique_sites = length(unique(panel$site_code)),
  source_years = 2021:2025,
  source_indicator_missing = as.list(raw_missing),
  sibling_csv_rows = csv_rows,
  sibling_csv_note = csv_row_note,
  indicator_mapping = as.list(indicator_source),
  complete_no_missing_candidate_sites = as.list(as.integer(candidate_counts)),
  sampled_strata = as.list(as.integer(stratum_counts)),
  selected_site_digest_algorithm = "MD5",
  selected_site_digest = selected_site_digest,
  public_schema = names(alprek_subset),
  row_count = nrow(alprek_subset),
  site_year_count = nrow(alprek_subset_counts),
  count_schema = names(alprek_subset_counts),
  disclosure_audit = disclosure_audit
)
names(build_info$complete_no_missing_candidate_sites) <- names(candidate_counts)
names(build_info$sampled_strata) <- names(stratum_counts)

attr(alprek_subset, "build_info") <- build_info
attr(alprek_subset_counts, "build_info") <- build_info

expect_identical(names(alprek_subset), c("student_id", "site_id", "year", indicators), "public schema")
expect_identical(nrow(sampled_summary), 50L, "selected site count")
expect_identical(length(unique(alprek_subset$site_id)), 50L, "public selected site count")
expect_identical(sort(unique(alprek_subset$year)), 2021:2025, "public selected years")
expect_identical(dim(alprek_subset_counts), c(250L, 13L), "count artifact dimensions")

count_bounds_ok <- all(alprek_subset_counts$n_jt >= alprek_subset_counts$c_jt_frpm) &&
  all(alprek_subset_counts$n_jt >= alprek_subset_counts$c_jt_snap) &&
  all(alprek_subset_counts$n_jt >= alprek_subset_counts$c_jt_wic) &&
  all(alprek_subset_counts$n_jt >= alprek_subset_counts$c_jt_tanf)
if (!count_bounds_ok) {
  abort_build("Aggregated marginal counts must not exceed cell denominators.")
}

save(alprek_subset, file = out_data, compress = "xz")
utils::write.csv(alprek_subset, file = file.path(out_extdata, "alprek_subset.csv"), row.names = FALSE)
saveRDS(alprek_subset_counts, file = file.path(out_extdata, "alprek_subset_counts.rds"), version = 2)

provenance <- c(
  "alprek_subset provenance",
  "========================",
  "",
  "Restricted source panel: not shipped with the package.",
  sprintf("Canonical source file basename: %s", basename(source_path)),
  sprintf("Canonical source MD5: %s", source_digest),
  sprintf("Builder: %s", build_info$build_script),
  sprintf("Builder version: %s", build_info$build_script_version),
  sprintf("Seed: %s", build_info$seed),
  sprintf("Selected real-site digest algorithm: %s", build_info$selected_site_digest_algorithm),
  sprintf("Selected real-site digest: %s", build_info$selected_site_digest),
  "",
  "Public artifacts:",
  sprintf("- data/alprek_subset.rda: %s rows, %s columns", nrow(alprek_subset), ncol(alprek_subset)),
  sprintf("- inst/extdata/alprek_subset.csv: %s rows, %s columns", nrow(alprek_subset), ncol(alprek_subset)),
  sprintf("- inst/extdata/alprek_subset_counts.rds: %s rows, %s columns", nrow(alprek_subset_counts), ncol(alprek_subset_counts)),
  "",
  "Disclosure audit summary:",
  sprintf("- synthetic sites: %s", disclosure_audit$selected_site_count),
  sprintf("- years: %s", disclosure_audit$selected_year_count),
  sprintf("- minimum public site-year n: %s", disclosure_audit$min_site_year_n),
  sprintf("- maximum public site-year n: %s", disclosure_audit$max_site_year_n),
  sprintf("- TANF zero-count site-year cells: %s", disclosure_audit$tanf_zero_cell_count),
  "- original site_code and adece_id columns are not included in shipped artifacts.",
  "- student_id is synthetic, sequential, and stable across years for the same selected child within a synthetic site.",
  "- stable student_id values preserve longitudinal structure for examples but are not original source identifiers."
)
writeLines(provenance, con = file.path(out_extdata, "alprek_subset_provenance.txt"), useBytes = TRUE)

audit <- data.frame(
  metric = c(
    "source_rows",
    "source_cols",
    "source_unique_students",
    "source_unique_sites",
    "csv_data_rows",
    "public_rows",
    "public_site_year_rows",
    "public_min_site_year_n",
    "public_max_site_year_n",
    "public_tanf_zero_cells",
    paste0("candidate_", names(candidate_counts)),
    paste0("sampled_", names(stratum_counts))
  ),
  value = as.character(c(
    nrow(panel),
    ncol(panel),
    length(unique(panel$adece_id)),
    length(unique(panel$site_code)),
    csv_rows,
    nrow(alprek_subset),
    nrow(alprek_subset_counts),
    disclosure_audit$min_site_year_n,
    disclosure_audit$max_site_year_n,
    disclosure_audit$tanf_zero_cell_count,
    as.integer(candidate_counts),
    as.integer(stratum_counts)
  )),
  stringsAsFactors = FALSE
)
utils::write.csv(audit, file = file.path(audit_dir, "alprek_subset_build_audit.csv"), row.names = FALSE)

message("Built alprek_subset artifacts:")
message(sprintf("- %s", out_data))
message(sprintf("- %s", file.path(out_extdata, "alprek_subset.csv")))
message(sprintf("- %s", file.path(out_extdata, "alprek_subset_counts.rds")))
message(sprintf("- %s", file.path(out_extdata, "alprek_subset_provenance.txt")))
message(sprintf("- %s", file.path(audit_dir, "alprek_subset_build_audit.csv")))
