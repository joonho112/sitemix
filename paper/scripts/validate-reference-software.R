# Recheck Wilson and SRSWOR against established R implementations.
.libPaths(c(normalizePath("qa/r-library"), .libPaths()))
suppressPackageStartupMessages(library(sitemix))
z <- qnorm(0.975)
cat("== (a) Wilson surrogate vs prop.test score interval ==\n")
mx <- 0
for (n in c(1, 2, 5, 9, 10, 26, 100)) for (C in c(0, n)) {
  ci <- suppressWarnings(prop.test(C, n, correct = FALSE)$conf.int)
  ref <- diff(ci) / (2 * z)
  api <- suppressWarnings(sm_estimate_from_counts(data.frame(site_id = "s", year = 2026L, n_jt = as.integer(n), c_jt_x = as.integer(C)),
                                                 family = "binomial", indicator = "x", vst = "none"))$se_raw
  mx <- max(mx, abs(api - ref))
}
stopifnot(mx < 1e-12)
wilson_max <- mx
cat("max |se_raw - prop.test half-width / z| over 14 boundary cells:", format(mx), "\n")
cat("\n== (b) SRSWOR vs survey::svymean ==\n")
if (requireNamespace("survey", quietly = TRUE)) {
  cat("survey version:", as.character(packageVersion("survey")), "\n")
  mx <- 0
  for (cfg in list(c(3, 8, 19), c(5, 12, 40), c(1, 6, 7), c(10, 30, 31), c(50, 100, 1000))) {
    C <- cfg[1]; n <- cfg[2]; N <- cfg[3]
    d <- data.frame(y = c(rep(1, C), rep(0, n - C)), N = N)
    des <- survey::svydesign(ids = ~1, fpc = ~N, data = d)
    ref <- as.numeric(survey::SE(survey::svymean(~y, des)))
    api <- suppressWarnings(sm_estimate_from_counts(data.frame(site_id = "s", year = 2026L, n_jt = as.integer(n), c_jt_x = as.integer(C)),
             family = "binomial", indicator = "x", vst = "none", fpc = N, bias_correction = "binomial_bc"))$se_raw
    cat(sprintf("C=%d n=%d N=%d  survey SE=%.12f  sitemix bc+fpc SE=%.12f  diff=%.2e\n", C, n, N, ref, api, api - ref))
    mx <- max(mx, abs(api - ref))
  }
  cat("max abs diff:", format(mx), "\n")
  # multivariate: survey vcov of two overlapping indicators vs Scenario B with bc + fpc
  set.seed(1); n <- 15; N <- 60
  d <- data.frame(site_id = "s", year = 2026L, a = rbinom(n, 1, .4), b = rbinom(n, 1, .5)); d$b[d$a == 1][1:3] <- 1
  des <- survey::svydesign(ids = ~1, fpc = rep(N, n), data = d)
  ref <- unclass(vcov(survey::svymean(~a + b, des)))
  api <- as.matrix(suppressWarnings(sm_estimate(d, family = "multivariate", indicators = c("a", "b"), vjt = TRUE, vst = "none",
                    boundary_method = "none", bias_correction = "binomial_bc", fpc = N))$V[[1]])
  cat("Scenario B bc+fpc vs survey vcov, max abs diff:", format(max(abs(api - ref))), "\n")
} else cat("survey not installed; skipped\n")

stopifnot(requireNamespace("survey", quietly=TRUE), mx < 1e-12, max(abs(api-ref)) < 1e-12)

dir.create("qa/validation", showWarnings=FALSE, recursive=TRUE)
jsonlite::write_json(list(version=as.character(packageVersion("sitemix")), survey_version=as.character(packageVersion("survey")), wilson_cases=14L, wilson_max_abs_error=wilson_max, corrected_scalar_cases=5L, scalar_max_abs_error=mx, B_matrix_cases=1L, matrix_max_abs_error=max(abs(api-ref)), tolerance=1e-12, status="pass"), "qa/validation/reference-software-summary.json", pretty=TRUE,auto_unbox=TRUE,digits=NA)
