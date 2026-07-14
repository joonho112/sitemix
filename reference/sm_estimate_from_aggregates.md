# Estimate site-year rates from published aggregate rows

`sm_estimate_from_aggregates()` is the published-aggregates wrapper
around
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
for analysts working from publisher CSVs rather than student rows. It
locks `from_aggregates = TRUE` in the underlying dispatch and refuses
`from_counts` (raises `sitemix_error_input_path_conflict`). The
aggregate path supports two scenarios: **D0** single-indicator binomial
rows (one numerator and one denominator per site-year) and **D1**
marginal multivariate rows (multiple aggregate marginals per site-year)
with optional working-independence covariance and raw pairwise Fréchet
intervals and projected stress scenarios via
[`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md).
Aggregate multinomial composition is not a D1 mode and is rejected with
`sitemix_error_ambiguous_dispatch`; use
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
with complete category counts for Scenario C.

This wrapper is the recommended public entry point for published
aggregate rows. Its v0.2 formals are frozen, and no argument is
deprecated. Direct `sm_estimate(..., from_aggregates = TRUE)` calls
remain supported for compatibility and produce the same result.

## Usage

``` r
sm_estimate_from_aggregates(
  data,
  family,
  indicator = NULL,
  indicators = NULL,
  id_cols = c("site_id", "year"),
  numerator_col = NULL,
  denominator_col = NULL,
  indicator_col = NULL,
  subgroup_col = NULL,
  aggregate_case = c("auto", "D0", "D1"),
  framing = NA_character_,
  sampling_relation = c("unknown", "same_units", "different_units"),
  accountability_n = 30L,
  suppression = c("drop", "upper_bound"),
  suppression_col = NULL,
  suppression_flag_value = "",
  suppression_when = NULL,
  suppressed_theta_hat = 0.5,
  suppression_sensitivity_acknowledge = FALSE,
  suppressed_n_strategy = c("observed_n", "worst_case_bound"),
  suppressed_n_bound = NULL,
  ...
)
```

## Arguments

- data:

  A data frame or tibble. Required columns depend on the dispatched
  Scenario: `(site_id, year, indicator)` for Scenario A from student
  rows; `(site_id, year, indicators)` for Scenario B;
  `(site_id, year, indicator)` as a factor or character column for
  Scenario C from student rows. Sufficient counts require `n_jt` plus
  family-specific `c_jt_*` columns. Published aggregates use numerator
  and denominator columns named by `numerator_col` and
  `denominator_col`.

- family:

  Character scalar. Estimation family selecting the dispatched engine.
  One of `"binomial"`, `"multivariate"`, or `"multinomial"`. No default;
  omission raises `sitemix_error_invalid_family`.

- indicator:

  Character scalar or `NULL` (default `NULL`). Name of the single
  indicator column in `data`. Required for Scenarios A, C, and D0. For
  Scenario A the column must be logical or 0/1 numeric; for Scenario C
  the column must be a factor or character.

- indicators:

  Character vector or `NULL` (default `NULL`). For Scenario B, the
  column names of overlapping binary indicators whose joint moments are
  estimated. For Scenario C with `from_counts = TRUE` (including
  [`sm_estimate_from_counts`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)),
  the explicit category order applied to the supplied `c_jt_*` count
  columns. For Scenario D1, the marginal column names.

- id_cols:

  Character vector of length two. Column names identifying site and
  year, in that order. Defaults to `c("site_id", "year")`.

- numerator_col:

  Character scalar or `NULL` (default `NULL`). Name of the aggregate
  numerator column. Required for Scenario D0 inputs; ignored otherwise.

- denominator_col:

  Character scalar or `NULL` (default `NULL`). Name of the aggregate
  denominator column. Required for Scenarios D0 / D1; ignored otherwise.

- indicator_col:

  Character scalar or `NULL` (default `NULL`). Name of the long-form
  indicator-key column in aggregate inputs (one row per
  site-year-indicator).

- subgroup_col:

  Character scalar or `NULL` (default `NULL`). Name of the subgroup-key
  column in aggregate D1 inputs.

- aggregate_case:

  Aggregate case: `"auto"` (infer; default), `"D0"`, or `"D1"`. Ignored
  off the aggregate path; invalid values fail before dispatch.

- framing:

  Aggregate subgroup framing. Character scalar or `NA_character_`
  (default; direct D0 framing). For Framing X or Framing Y, pivot first
  with the corresponding subgroup helper listed under *See Also*. Valid
  inactive values are accepted silently; invalid values always raise
  `sitemix_error_invalid_framing`.

- sampling_relation:

  Character scalar describing D1 sampling-unit provenance. One of
  `"unknown"` (default), `"same_units"`, or `"different_units"`. These
  map to object-level `d1_regime` values `"unknown"`, `"D1a"`, and
  `"D1b"`, respectively. Equal denominators are recorded separately and
  never establish common observational units. Invalid values raise
  `sitemix_error_invalid_sampling_relation`. A valid value outside D1 is
  accepted silently and has no effect.

- accountability_n:

  Positive integer scalar. Threshold for the `flag_below_accountability`
  output column; rows with \\n\_{jt} \<\\ `accountability_n` are
  flagged. Defaults to `30L`.

- suppression:

  Character scalar. Aggregate Tier-1 handling mode. One of `"drop"`
  (default; retain an unavailable audit row with canonical point/SE
  columns missing) or `"upper_bound"` (legacy option for an explicitly
  acknowledged, separated worst-case Bernoulli variance sensitivity).
  The latter never writes a synthetic point or SE to canonical columns
  and is excluded from ordinary `V` and Fréchet inputs. A valid value is
  ignored when `from_aggregates = FALSE`; invalid values are rejected
  before dispatch.

- suppression_col:

  Character scalar or `NULL` (default `NULL`). Name of the publisher
  suppression flag column in `data`. `NULL` disables flag-based
  detection.

- suppression_flag_value:

  Value or vector of values marking Tier-1 suppression in
  `suppression_col`. Defaults to `""` (the empty string).

- suppression_when:

  Function or `NULL` (default `NULL`). Optional predicate overriding
  flag-based detection.

- suppressed_theta_hat:

  Numeric scalar in \\\[0, 1\]\\. Legacy compatibility name for the
  raw-scale probability used only to maximize Bernoulli variance under
  `suppression = "upper_bound"`. Any finite interior value is
  syntactically valid, but an active upper-bound sensitivity with a
  suppressed row requires `0.5`. It is stored in
  `sensitivity_probability` and never substituted into canonical
  estimate columns. The argument is retained without a deprecation
  warning or removal schedule in v0.2.

- suppression_sensitivity_acknowledge:

  Logical scalar. Must be `TRUE` when `suppression = "upper_bound"`
  actually encounters suppressed rows. This explicitly acknowledges that
  the returned separated fields are a non-identified
  variance-sensitivity scenario, not an estimate or an ordinary
  covariance input.

- suppressed_n_strategy:

  Character scalar. Denominator strategy for hidden suppressed rows. One
  of `"observed_n"` (default) or legacy `"worst_case_bound"` (record
  `suppressed_n_bound` as an operational placeholder). Because an upper
  bound on an unknown denominator is not a conservative SE denominator,
  hidden-denominator sensitivity rows make no numeric variance claim.

- suppressed_n_bound:

  Positive integer scalar or `NULL` (default). Legacy audit placeholder
  for the worst-case strategy; never a variance denominator.

- ...:

  Additional arguments forwarded to
  [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md).

## Value

A `sitemix_estimates` tibble with the same column structure as
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md);
see that function's *Return* section for the canonical column glossary
and object metadata.

## Details

**D0 vs D1.** The aggregate-input dispatch follows the same Scenario
taxonomy as
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md):

- **D0**:

  Use when `family = "binomial"` and the input has one row per site-year
  with explicit `numerator_col` and `denominator_col`.

- **D1**:

  Use when `family = "multivariate"` and the input has multiple
  aggregate marginals per site-year. The cross-indicator covariance is
  not identified from marginals alone; the engine emits a diagonal
  working-independence `V` (when `vjt = TRUE`). For a pairwise interval
  and projected stress analysis of unidentified joints, see
  [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md).

Set `aggregate_case = "auto"` (default) to resolve one unique indicator
as D0 and two or more as D1; pass `"D0"` or `"D1"` to assert the case.
D1 requires the same ordered indicator set in every site-year group. Set
`sampling_relation = "same_units"` only when the marginal rows are known
to describe the same observational units, or `"different_units"` when
they are known to differ. The default `"unknown"` makes no such claim.
Common denominators are recorded separately and never imply
`"same_units"`.

**Subgroup framings.** For publisher files where each site carries
multiple subgroup rows, pivot the file first via
[`sm_pivot_subgroups_to_sites()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md)
(Framing X) or
[`sm_pivot_subgroups_to_indicators()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md)
(Framing Y), then pass the pivoted table here. See
[`vignette("a5-published-aggregates", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md).

**Suppression.** The wrapper exposes these publisher-side controls:

- Detection: `suppression_col`, `suppression_flag_value`, and
  `suppression_when`.

- Policy mode: `suppression`.

- Sensitivity point: `suppressed_theta_hat`.

- Required acknowledgement: `suppression_sensitivity_acknowledge`.

- Hidden denominators: `suppressed_n_strategy` and `suppressed_n_bound`.

For an audit pass before estimation, use
[`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md).
`suppression = "drop"` retains an unavailable audit row with canonical
estimate and SE columns missing. The legacy `"upper_bound"` label now
means a separated worst-case variance sensitivity.

`suppression_sensitivity_acknowledge = TRUE` is required for that
sensitivity; it never populates canonical estimates or ordinary
covariance.

## See also

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  for the main dispatcher and canonical column glossary.

- [`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
  for the sufficient-counts sister wrapper.

- [`sm_pivot_subgroups_to_sites()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md)
  and
  [`sm_pivot_subgroups_to_indicators()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md)
  for the Framing X and Framing Y pivots.

- [`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
  for publisher-side suppression auditing.

- [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  for D1 aggregate sensitivity.

- [`vignette("a5-published-aggregates")`](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md)
  for the applied walkthrough.

- [`vignette("m5-aggregate-engines")`](https://joonho112.github.io/sitemix/articles/m5-aggregate-engines.md)
  for formal D0 / D1 specifications.

Other estimation:
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md),
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)

## Examples

``` r
# Build a D0 aggregate slice from the bundled count artifact:
counts_path <- system.file(
  "extdata", "alprek_subset_counts.rds",
  package = "sitemix", mustWork = TRUE
)
counts <- readRDS(counts_path)

d0_frpm <- counts[counts$year == 2024, c("site_id", "year", "n_jt", "c_jt_frpm")]
d0_frpm$indicator <- "frpm"
d0_frpm$c_jt <- d0_frpm$c_jt_frpm
d0_frpm <- d0_frpm[c("site_id", "year", "indicator", "c_jt", "n_jt")]

est <- sm_estimate_from_aggregates(
  d0_frpm,
  family    = "binomial",
  indicator = "frpm"
)
head(est, 5)
#> sitemix_estimates: 5 rows x 18 columns | family=binomial | role=summary_uncertainty
#> groups=5 sites=5 years=1 indicators=1 V=FALSE K=FALSE
#> # A tibble: 5 × 18
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 frpm          0.4       0.685  0.155 0.158    10    10
#> 2 S002     2024 frpm          0.222     0.491  0.139 0.167     9     9
#> 3 S003     2024 frpm          0.545     0.831  0.150 0.151    11    11
#> 4 S004     2024 frpm          0.333     0.615  0.157 0.167     9     9
#> 5 S005     2024 frpm          0.556     0.841  0.166 0.167     9     9
#> # ℹ 9 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>
unique(est$estimate_scale)
#> [1] "arcsine"
```
