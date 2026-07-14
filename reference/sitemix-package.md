# sitemix: Site- and group-level proportions, rates, and sampling uncertainty

`sitemix` produces site- and group-level point estimates, standard
errors, and optional covariance matrices from student rows, sufficient
counts, or published aggregates. The package covers five estimation
scenarios and ships diagnostics, publisher-side suppression auditing,
optional variance smoothing, and raw pairwise Fréchet intervals with
projected stress scenarios for unidentified D1 covariance.

## Scenarios

Five estimation scenarios are dispatched by
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md):

- **Scenario A — binomial**:

  One binary indicator per site-year. Input is student rows via
  [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  or sufficient counts via
  [`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md).
  Covariance matrix is \\1 \times 1\\.

- **Scenario B — multivariate**:

  Overlapping binary indicators per site-year with SUR-style covariance.
  Input is student rows or complete sufficient counts containing
  marginal and pairwise co-occurrence counts. Covariance is \\K \times
  K\\.

- **Scenario C — multinomial**:

  Mutually exclusive categories summing to the denominator. Simplex
  covariance with analytic rank \\S - 1\\, where \\S\\ is positive
  observed support. Input is student rows or complete category counts,
  not published D1 marginals.

- **Scenario D0 — aggregate binomial**:

  Published numerator/denominator per site-year. Dispatch via
  [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md).

- **Scenario D1 — aggregate marginal**:

  Multiple published marginals per site-year with working-independence
  covariance. Dispatch via
  [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md).
  Use
  [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  for sensitivity.

## Entry points

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md):

  Main dispatcher for student rows, counts, and aggregates.

- [`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md):

  Sufficient-counts wrapper.

- [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md):

  Published-aggregates wrapper with D0 / D1 dispatch.

- [`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md):

  Uncertainty audit at three levels (summary / row / vcov).

- [`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md):

  Publisher-side three-tier suppression audit.

## Vignettes

**Applied track** (a1 — a9) walks through workflows for student rows,
sufficient counts, published aggregates, diagnostics, smoothing,
downstream workflows, and a real-data case study. Start with
[`vignette("a1-getting-started", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/a1-getting-started.md).

**Method track** (m1 — m8) covers the formal specifications: scalar SE
pipelines, SUR and multinomial covariance, aggregate engines, variance
smoothing theory, Fréchet pairwise/stress semantics, and the output
contract. Start with
[`vignette("m1-statistical-foundations", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md).

## Bundled data

[alprek_subset](https://joonho112.github.io/sitemix/reference/alprek_subset.md)
is an anonymized 50-site Alabama Pre-K sample used throughout the
documentation and tests. The package also ships
`inst/extdata/alprek_subset_counts.rds` for sufficient-counts examples.

## See also

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  for the main dispatcher.

- [`sm_vcov()`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
  for the covariance-object contract.

- [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  for D1 dependence sensitivity.

- [`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
  for optional smoothing alternatives.

Repository: <https://github.com/joonho112/sitemix>; Issues:
<https://github.com/joonho112/sitemix/issues>.

## Author

**Maintainer**: JoonHo Lee <jlee296@ua.edu> (ORCID:
[0009-0006-4019-8703](https://orcid.org/0009-0006-4019-8703)), Assistant
Professor, The University of Alabama.
