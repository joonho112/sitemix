#!/usr/bin/env Rscript
# Step 3.3: deterministic exact finite-binomial sums, independent of sitemix.
# No package production helper, simulation, confidence interval, or coverage
# calculation is used. jsonlite is used only to serialize the evidence record.
options(stringsAsFactors = FALSE, digits = 17)
started <- format(Sys.time(), tz = "UTC", usetz = TRUE)
arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(arg) != 1L) stop("Run this file with Rscript.")
script <- normalizePath(sub("^--file=", "", arg))
root <- dirname(dirname(script))
out <- file.path(root, "qa", "validation")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
n_grid <- c(1L, 2L, 5L, 10L, 20L, 50L, 100L)
theta_grid <- c(0, .01, .05, .1, .5, .9, .95, .99, 1)
tolerance <- 1e-12
rows <- list()
checks <- list()
check <- function(name, n, theta, error, tol = tolerance) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = name, n = n, theta = theta, absolute_error = error,
    tolerance = tol, passed = is.finite(error) && error <= tol
  )
}

for (n in n_grid) {
  for (theta in theta_grid) {
    count <- 0:n
    probability <- stats::dbinom(count, size = n, prob = theta)
    mass <- sum(probability)
    observed <- count / n
    target <- asin(sqrt(theta))
    raw_mean <- sum(probability * observed)
    raw_variance <- sum(probability * (observed - raw_mean)^2)
    check("probability_mass", n, theta, abs(mass - 1))
    check("raw_mean_identity", n, theta, abs(raw_mean - theta))
    check("raw_variance_identity", n, theta,
          abs(raw_variance - theta * (1 - theta) / n))
    for (method in c("arcsine", "anscombe")) {
      transformed <- if (method == "arcsine") {
        asin(sqrt(observed))
      } else {
        asin(sqrt((count + 3 / 8) / (n + 3 / 4)))
      }
      exact_mean <- sum(probability * transformed)
      exact_variance <- sum(probability * (transformed - exact_mean)^2)
      bias <- exact_mean - target
      exact_mse <- sum(probability * (transformed - target)^2)
      working_variance <- if (method == "arcsine") 1 / (4 * n) else 1 / (4 * (n + .5))
      degenerate <- theta %in% c(0, 1)
      check(paste0(method, "_variance_identity"), n, theta,
            abs(exact_variance - (sum(probability * transformed^2) - exact_mean^2)))
      check(paste0(method, "_mse_identity"), n, theta,
            abs(exact_mse - exact_variance - bias^2))
      if (n == 1L && method == "arcsine") {
        check("arcsine_n1_closed_variance", n, theta,
              abs(exact_variance - pi^2 * theta * (1 - theta) / 4))
      }
      if (degenerate) {
        check(paste0(method, "_endpoint_zero_variance"), n, theta, abs(exact_variance), tol = 0)
      }
      rows[[length(rows) + 1L]] <- data.frame(
        n = n, theta = theta, method = method, transform_target = target,
        exact_mean = exact_mean, exact_bias = bias,
        exact_variance = exact_variance, exact_mse = exact_mse,
        working_variance = working_variance,
        working_to_exact_ratio = if (degenerate) NA_real_ else working_variance / exact_variance,
        relative_variance_error = if (degenerate) NA_real_ else working_variance / exact_variance - 1,
        exact_to_working_ratio = exact_variance / working_variance,
        ratio_status = if (degenerate) "undefined_exact_variance_zero" else "defined",
        probability_mass = mass,
        expected_successes = n * theta, expected_failures = n * (1 - theta),
        fpc = "none", bias_correction = "none"
      )
    }
  }
}
result <- do.call(rbind, rows)

# Complement symmetry: g(1-p) = pi/2 - g(p), including the Anscombe map.
for (method in unique(result$method)) {
  for (n in n_grid) {
    for (theta in theta_grid[theta_grid <= .5]) {
      a <- result[result$method == method & result$n == n & abs(result$theta - theta) < 1e-14, ]
      b <- result[result$method == method & result$n == n & abs(result$theta - (1 - theta)) < 1e-14, ]
      stopifnot(nrow(a) == 1L, nrow(b) == 1L)
      check(paste0(method, "_complement_mean"), n, theta,
            abs(a$exact_mean + b$exact_mean - pi / 2))
      check(paste0(method, "_complement_variance"), n, theta,
            abs(a$exact_variance - b$exact_variance))
      check(paste0(method, "_complement_bias"), n, theta,
            abs(a$exact_bias + b$exact_bias))
    }
  }
}
checks <- do.call(rbind, checks)
write.csv(result, file.path(out, "small-n.csv"), row.names = FALSE, na = "NA")
write.csv(checks, file.path(out, "small-n-checks.csv"), row.names = FALSE)
if (!all(checks$passed)) stop("Exact summation invariants failed; inspect small-n-checks.csv.")

selection <- rbind(
  c(1, .5), c(5, .01), c(10, .5), c(100, .01), c(100, .5), c(10, 0)
)
selected <- do.call(rbind, lapply(seq_len(nrow(selection)), function(i) {
  result[result$n == selection[i, 1] & result$theta == selection[i, 2], ]
}))
write.csv(selected, file.path(out, "small-n-selected.csv"), row.names = FALSE, na = "NA")
defined <- result[!is.na(result$working_to_exact_ratio), ]
extrema <- do.call(rbind, lapply(unique(result$method), function(method) {
  subset <- defined[defined$method == method, ]
  lo <- min(subset$working_to_exact_ratio)
  hi <- max(subset$working_to_exact_ratio)
  min_rows <- subset[abs(subset$working_to_exact_ratio - lo) < 1e-12, ]
  max_rows <- subset[abs(subset$working_to_exact_ratio - hi) < 1e-12, ]
  rbind(cbind(extremum = "minimum", min_rows), cbind(extremum = "maximum", max_rows))
}))
write.csv(extrema, file.path(out, "small-n-extrema.csv"), row.names = FALSE)

fmt <- function(x, digits = 8L) ifelse(is.na(x), "NA", formatC(x, digits = digits, format = "g"))
table_lines <- function(data) {
  vapply(seq_len(nrow(data)), function(i) {
    r <- data[i, ]
    paste0("| ", r$n, " | ", fmt(r$theta, 3), " | ", r$method,
           " | ", fmt(r$exact_variance), " | ", fmt(r$working_variance),
           " | ", fmt(r$working_to_exact_ratio, 6), " | ", fmt(r$exact_bias), " |")
  }, character(1))
}
arcsine <- defined[defined$method == "arcsine", ]
anscombe <- defined[defined$method == "anscombe", ]
merged <- merge(arcsine, anscombe, by = c("n", "theta"), suffixes = c("_arcsine", "_anscombe"))
closer <- abs(merged$relative_variance_error_anscombe) < abs(merged$relative_variance_error_arcsine)
further <- abs(merged$relative_variance_error_anscombe) > abs(merged$relative_variance_error_arcsine)
identity <- jsonlite::read_json(file.path(root, "qa", "software-identity.json"), simplifyVector = TRUE)
summary <- list(
  step = "Step 3.3", status = "complete", started_utc = started,
  completed_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  calculation = "Exact finite-support binomial summation using stats::dbinom; no Monte Carlo",
  independence = "No sitemix code or API used to compute or validate results; jsonlite serializes metadata only",
  n_grid = n_grid, theta_grid = theta_grid, methods = c("arcsine", "anscombe"),
  cells_per_method = nrow(result) / 2L, total_rows = nrow(result),
  ratio_definition = "working_variance / exact_variance",
  undefined_ratio_rule = "NA at theta = 0 or 1 because exact variance is zero",
  undefined_ratio_rows = sum(is.na(result$working_to_exact_ratio)),
  exact_variance_definition = "sum_{c=0}^n Bin(n,theta)[c] * (g_n(c) - exact_mean)^2",
  bias_target = "asin(sqrt(theta)) for both transforms",
  mse_definition = "variance + bias^2 relative to asin(sqrt(theta))",
  checks = nrow(checks), failed_checks = sum(!checks$passed),
  tolerance = tolerance, maximum_check_error = max(checks$absolute_error),
  endpoint_check_tolerance = 0,
  coverage_evaluated = FALSE,
  uncertainty_scope = "Fixed-n IID binomial, no FPC, no bias correction, transformed working variance only; not raw boundary surrogates",
  extrema = extrema[, c("extremum", "method", "n", "theta", "working_to_exact_ratio")],
  anscombe_closer_to_exact_by_relative_variance_error = sum(closer),
  anscombe_further_from_exact_by_relative_variance_error = sum(further),
  comparison_denominator = nrow(merged),
  limitations = c("Finite grid only; no uniform accuracy guarantee", "Variance approximation quality is not interval coverage", "Anscombe changes both the statistic and the target approximation; bias is reported separately", "No posterior or ranking-uncertainty calibration"),
  contextual_software_source_manifest_sha256 = identity$source_manifest_sha256,
  contextual_software_commit = identity$public_source$commit,
  contextual_identity_role = "Context only; calculation does not import this software",
  script_md5 = unname(tools::md5sum(script)),
  session_info = capture.output(sessionInfo())
)
jsonlite::write_json(summary, file.path(out, "small-n-summary.json"), pretty = TRUE,
                     auto_unbox = TRUE, digits = NA, na = "null")

# Internal drafting memo omitted from the public distribution.

cat("Step 3.3 exact-binomial summation completed.\n")
cat("Grid rows:", nrow(result), "(63 per method)\n")
cat("Checks:", nrow(checks), "passed; failures:", sum(!checks$passed), "\n")
cat("Maximum numerical check error:", max(checks$absolute_error), "\n")
cat("Undefined working/exact ratios:", sum(is.na(result$working_to_exact_ratio)), "\n")
print(selected[, c("n", "theta", "method", "exact_variance", "working_variance", "working_to_exact_ratio", "exact_bias")], row.names = FALSE)
print(extrema[, c("extremum", "n", "theta", "method", "working_to_exact_ratio")], row.names = FALSE)
cat("\nSession:\n")
print(sessionInfo())
