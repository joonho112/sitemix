# sitemix: Site- and group-level proportions, proportions, and sampling uncertainty

`sitemix` estimates site- and group-level proportions and their sampling
uncertainty. Use
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
for student rows,
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
for sufficient counts, or
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
for published summaries. The results include point estimates, standard
errors, and optional within-site covariance matrices.

[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
checks the returned estimates and uncertainty at the summary, row, or
covariance level. Before estimating from published data,
[`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
summarizes suppressed rows and sample-size reporting thresholds.

## Estimation families and aggregate inputs

The package supports the following cases. Covariance matrices are
included when `vjt = TRUE`; see
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
for supported options and input requirements.

- **Scenario A — binomial**:

  One binary indicator per site-year, supplied as student rows or
  sufficient counts. The covariance matrix is \\1 \times 1\\.

- **Scenario B — multivariate**:

  Overlapping binary indicators per site-year with seemingly unrelated
  regression (SUR) covariance. Input is student rows or complete
  sufficient counts containing marginal and pairwise co-occurrence
  counts. The covariance is \\K \times K\\, where \\K\\ is the number of
  indicators.

- **Scenario C — multinomial**:

  Mutually exclusive categories whose counts sum to the denominator. The
  covariance has simplex structure and analytic support rank \\S - 1\\,
  where \\S\\ is the number of categories with positive observed counts.
  Input is student rows or complete category counts, not published D1
  marginals.

- **Scenario D0 — aggregate binomial**:

  Published numerator and denominator for one indicator per site-year.

- **Scenario D1 — aggregate marginal**:

  Multiple published marginal proportions per site-year. Marginals alone
  do not identify cross-indicator covariance; the optional matrix
  assumes working independence. See
  [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  for pairwise intervals and projected dependence scenarios.

## Examples and methods

The applied vignettes (a1 to a9) show how to prepare inputs, estimate
proportions, check uncertainty, and use the results in further analyses.
Start with
[`vignette("a1-getting-started", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/a1-getting-started.md).
The methods vignettes (m1 to m8) explain the sampling assumptions,
standard-error and covariance calculations, and sensitivity analyses.
Start with
[`vignette("m1-statistical-foundations", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md).

The examples draw on
[prek_sim](https://joonho112.github.io/sitemix/reference/prek_sim.md), a
fully simulated panel of 50 pre-kindergarten sites; the complete case
study uses this simulated panel. A corresponding count table is
available in `inst/extdata/prek_sim_counts.rds`; see
[prek_sim](https://joonho112.github.io/sitemix/reference/prek_sim.md)
for the data description and file access examples.

## See also

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  for estimation options and returned columns.

- [`sm_vcov()`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
  for covariance matrices and their scale information.

- [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  for D1 pairwise intervals and projected stress scenarios, whose
  interpretation depends on the sampling regime.

- [`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
  for experimental variance smoothing that appends alternative standard
  errors by default.

Repository: <https://github.com/joonho112/sitemix>; Issues:
<https://github.com/joonho112/sitemix/issues>.

## Author

**Author and maintainer**: JoonHo Lee <jlee296@ua.edu> (ORCID:
[0009-0006-4019-8703](https://orcid.org/0009-0006-4019-8703)), Assistant
Professor, The University of Alabama.
