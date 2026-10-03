#!/usr/bin/env Rscript

# Maintainer-only helper. This script is excluded from source package builds
# because it depends on tests/testthat/helper-regression.R in the source checkout.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else "inst/scripts/build-regression-baselines.R"
root <- normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE)

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(root, quiet = TRUE)
}

source(file.path(root, "tests", "testthat", "helper-regression.R"), chdir = TRUE)

baseline <- regression_build_baselines(root = root)
out_dir <- file.path(root, "tests", "testthat", "_data", "regression")
out <- file.path(out_dir, "regression-baselines.rds")
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
saveRDS(baseline, out, version = 2)
regression_write_review_csvs(baseline, out_dir)

cat("Wrote regression baseline: ", out, "\n", sep = "")
cat("Fixture version: ", baseline$metadata$fixture_version, "\n", sep = "")
cat("Counts MD5: ", baseline$metadata$counts_md5, "\n", sep = "")
