# Exhaustive public-API boundary audit, separate from the fast test suite.
# Install the package version being audited before running this script.
library(sitemix)
local({
  for (n in 1:40) {
    records <- expand.grid(count = 0:n, edge = c(0, n), swap = c(FALSE, TRUE))
    ok <- vapply(seq_len(nrow(records)), function(i) {
      a <- records$edge[i]
      b <- records$count[i]
      counts <- if (records$swap[i]) c(b, a) else c(a, b)
      d <- data.frame(site_id = "S", year = 2026L, indicator = c("a", "b"),
                      c_jt = counts, n_jt = n)
      e <- suppressWarnings(sm_estimate_from_aggregates(
        d, family = "multivariate", indicator_col = "indicator",
        sampling_relation = "same_units", min_n = 1L
      ))
      env <- sm_frechet_envelope(e, population_regime = "d1a")
      z <- env$raw_pairwise_intervals
      # Independent count-scale oracle; production computes from probabilities.
      lower <- max(0, sum(as.double(counts)) - n) / n
      upper <- min(counts) / n
      printed <- capture.output(print(env))
      summary(env)
      identical(z$joint_probability_lower, lower) &&
        identical(z$joint_probability_upper, upper) &&
        z$pairwise_covariance_lower == 0 && z$pairwise_covariance_upper == 0 &&
        length(printed) > 0
    }, logical(1))
    stopifnot(all(ok))
    cat("n =", n, "; cases =", length(ok), "; all passed\n")
  }
})
cat("FRECHET-BOUNDARY-OK: 3440 cases, n = 1..40, zero/one margins and both orders.\n")
