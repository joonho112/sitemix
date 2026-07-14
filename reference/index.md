# Package index

## Package

- [`sitemix`](https://joonho112.github.io/sitemix/reference/sitemix-package.md)
  [`sitemix-package`](https://joonho112.github.io/sitemix/reference/sitemix-package.md)
  : sitemix: Site- and group-level proportions, rates, and sampling
  uncertainty

## Estimate site-year rates

Main entry points: student rows, sufficient counts, and published
aggregates. Use
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
for the unified dispatcher; the wrappers preconfigure subsets of the
same signature.

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  : Estimate site-year rates and standard errors
- [`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
  : Estimate site-year rates from sufficient counts
- [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
  : Estimate site-year rates from published aggregate rows

## Audit uncertainty outputs

Diagnose scalar and covariance uncertainty and audit publisher-side
suppression before downstream use.

- [`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
  : Diagnose uncertainty in a sitemix_estimates tibble
- [`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
  : Audit aggregate-input suppression and accountability tiers

## Inspect and stress-test covariance

Work with `sm_vcov` matrices directly; compute formal raw pairwise
Fréchet intervals and separate projected stress scenarios for
unidentified D1 aggregate dependence.

- [`sm_vcov()`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
  : Construct or inspect a within-site covariance object
- [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  : Compute D1 pairwise Fréchet intervals and projected stress scenarios

## Smooth standard errors (experimental)

Opt-in generalized variance-function/log-variance sensitivity
alternatives. A fixed-seed audit found no evidence for default
promotion; append-only output preserves canonical SE columns and does
not guarantee improvement.

- [`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
  : Smooth standard errors with an experimental GVF model

## Reshape published aggregates

Pivot subgroup files into the schema expected by
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md).
Framing X turns subgroups into sites; Framing Y turns them into
indicators.

- [`sm_pivot_subgroups_to_sites()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md)
  : Pivot subgroup aggregate rows into subgroup-as-site input (Framing
  X)
- [`sm_pivot_subgroups_to_indicators()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md)
  : Pivot subgroup aggregate rows into subgroup-as-indicator input
  (Framing Y)

## Data

Bundled example data for documentation, examples, and tests.

- [`alprek_subset`](https://joonho112.github.io/sitemix/reference/alprek_subset.md)
  : Alabama Pre-K sample panel
