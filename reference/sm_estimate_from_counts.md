# Estimate site-year rates from sufficient counts

`sm_estimate_from_counts()` is the sufficient-counts wrapper around
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
for sites that have already aggregated their student rows into complete
per-site-year sufficient statistics. It locks `from_counts = TRUE` in
the underlying dispatch; otherwise the contract – arguments, output
schema, scale conventions – is identical to
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md).
The sufficient-counts identity guarantees agreement with
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
applied to the original student rows to within `1e-10`; see
[`vignette("m2-scalar-se-binomial", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
for the T2.5 invariant.

This wrapper is the recommended public entry point for sufficient
counts. Its v0.2 formals are frozen, and no argument is deprecated.
Direct `sm_estimate(..., from_counts = TRUE)` calls remain supported for
compatibility and produce the same result.

## Usage

``` r
sm_estimate_from_counts(
  data,
  family,
  indicator = NULL,
  indicators = NULL,
  id_cols = c("site_id", "year"),
  accountability_n = 30L,
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
  `sm_estimate_from_counts`), the explicit category order applied to the
  supplied `c_jt_*` count columns. For Scenario D1, the marginal column
  names.

- id_cols:

  Character vector of length two. Column names identifying site and
  year, in that order. Defaults to `c("site_id", "year")`.

- accountability_n:

  Positive integer scalar. Threshold for the `flag_below_accountability`
  output column; rows with \\n\_{jt} \<\\ `accountability_n` are
  flagged. Defaults to `30L`.

- ...:

  Additional arguments forwarded to
  [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md).

## Value

A `sitemix_estimates` tibble with the same column structure as
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md);
see that function's *Return* section for the canonical column glossary
and object metadata.

## Details

The input `data` is one row per site-year with an `n_jt` denominator and
family-specific `c_jt_*` columns. Scenario A requires one named marginal
count. Scenario B requires two or more ordered marginal counts plus
every ordered pairwise co-occurrence count; joint feasibility is
verified for \\K = 2\\ and \\K = 3\\, while \\K \ge 4\\ count input
fails closed. Scenario C requires at least two category counts whose row
sum equals `n_jt`; the category order can be set explicitly by
`indicators`.

This wrapper raises `sitemix_error_invalid_from_counts` if the caller
passes `from_counts` explicitly; call
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
directly if you need to override the wrapper's lock. See the Scenario
dispatch table in
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
for which family / indicator combinations apply.

## See also

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  for the main dispatcher and canonical column glossary.

- [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
  for the published-aggregates sister wrapper.

- [`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
  for output uncertainty auditing.

- [`vignette("a2-input-formats")`](https://joonho112.github.io/sitemix/articles/a2-input-formats.md)
  for the input-mode decision tree.

- [`vignette("a3-scenario-binomial")`](https://joonho112.github.io/sitemix/articles/a3-scenario-binomial.md)
  for the Scenario A counts pathway.

- [`vignette("m2-scalar-se-binomial")`](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
  for the T2.5 sufficient-counts identity.

Other estimation:
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md),
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)

## Examples

``` r
counts_path <- system.file(
  "extdata", "alprek_subset_counts.rds",
  package = "sitemix", mustWork = TRUE
)
counts <- readRDS(counts_path)

# Build a one-indicator sufficient-counts slice for Scenario A:
snap_counts <- counts[
  counts$year == 2024,
  c("site_id", "year", "n_jt", "c_jt_snap")
]
est <- sm_estimate_from_counts(
  snap_counts,
  family    = "binomial",
  indicator = "snap"
)
head(est, 5)
#> sitemix_estimates: 5 rows x 18 columns | family=binomial | role=summary_uncertainty
#> groups=5 sites=5 years=1 indicators=1 V=FALSE K=FALSE
#> # A tibble: 5 × 18
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 snap          0.6       0.886  0.155 0.158    10    10
#> 2 S002     2024 snap          0.333     0.615  0.157 0.167     9     9
#> 3 S003     2024 snap          0.455     0.740  0.150 0.151    11    11
#> 4 S004     2024 snap          0.444     0.730  0.166 0.167     9     9
#> 5 S005     2024 snap          0.444     0.730  0.166 0.167     9     9
#> # ℹ 9 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>
unique(est$estimate_scale)
#> [1] "arcsine"
```
