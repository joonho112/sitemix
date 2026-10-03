# Package index

## Package overview

- [`sitemix`](https://joonho112.github.io/sitemix/reference/sitemix-package.md)
  [`sitemix-package`](https://joonho112.github.io/sitemix/reference/sitemix-package.md)
  : sitemix: Site- and group-level proportions, proportions, and
  sampling uncertainty

## Estimate proportions

Estimate site-year proportions from student records, counts, or
published aggregates. The count and aggregate wrappers select the
corresponding input path.

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  : Estimate site-year proportions and standard errors
- [`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
  : Estimate site-year proportions from sufficient counts
- [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
  : Estimate site-year proportions from published aggregate rows

## Check estimates and suppression

Examine returned estimates and covariance, or summarize suppression and
denominator thresholds in a publisher’s input table.

- [`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
  : Diagnose uncertainty in a sitemix_estimates tibble
- [`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
  : Summarize suppression and reporting thresholds in aggregate data

## Covariance and dependence

Inspect covariance matrices and explore dependence when only marginal
counts are available. Fréchet results separate pairwise intervals from
projected dependence scenarios.

- [`sm_vcov()`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
  : Construct or inspect a within-site covariance object
- [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  : Explore dependence between published marginal proportions

## Experimental variance smoothing

Fit a log-variance model to eligible rows. The function adds alternative
standard errors by default; improved performance is not guaranteed.

- [`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
  : Smooth standard errors with an experimental variance model

## Reshape subgroup data

Prepare subgroup tables for estimation. Treat subgroups as separate
sites or as indicators within a site, according to the analysis.

- [`sm_pivot_subgroups_to_sites()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md)
  : Prepare published counts for subgroup proportions or compositions
- [`sm_pivot_subgroups_to_indicators()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md)
  : Prepare subgroup proportions as indicators within each site

## Simulated example data

A simulated pre-kindergarten panel for learning and checking the
methods.

- [`prek_sim`](https://joonho112.github.io/sitemix/reference/prek_sim.md)
  : Simulated pre-kindergarten site panel
