# A5 · Published aggregates D0 / D1

Abstract

For external researchers working from publisher CSVs (state
accountability files, district reports) rather than student rows. This
vignette walks through the D0 (single numerator/denominator per
site-year) and D1 (multiple marginals per site-year) paths, the Framing
X / Framing Y subgroup pivots, and the three-tier publisher-side
suppression audit.

## Overview

### 1. Why you are here

Dana is an external researcher with no microdata access. She works
exclusively from state-published accountability files: numerator counts
plus subgroup splits with cells masked at `< 10`. She needs site-level
rates and standard errors from these files, plus raw pairwise intervals
and projected stress scenarios for what the publisher masked.

### 2. What you will leave with

By the end you will have:

- A D0 (single-indicator) and a D1 (multi-marginal) call each producing
  a `sitemix_estimates` tibble.
- The Framing X and Framing Y pivots for subgroup files.
- A three-tier suppression audit confirming how many rows survive
  publisher-side masking.

**Prerequisites.** [A2 · Input
formats](https://joonho112.github.io/sitemix/articles/a2-input-formats.md).

> **About the example data.** All results here are computed live from
> `alprek_subset`, an anonymized, illustrative 50-site sample of Alabama
> First Class Pre-K records shipped in the package (see
> [`?alprek_subset`](https://joonho112.github.io/sitemix/reference/alprek_subset.md)).
> The D0 and D1 rows are aggregate counts built from that sample (its
> companion `alprek_subset_counts` file), and the subgroup section adds
> a small synthetic fixture. It is not a real accountability dataset and
> must not be cited as empirical Pre-K results. Every code block runs
> offline with a fixed random seed.

## 3. Scenario D0 — one numerator and denominator per site-year

D0 is the simplest publisher case: each row is one site-year with
explicit numerator (`c_jt`) and denominator (`n_jt`). Build a D0 slice
from the bundled counts file:

``` r

counts_path <- system.file(
  "extdata", "alprek_subset_counts.rds",
  package = "sitemix", mustWork = TRUE
)
counts <- readRDS(counts_path)

d0_frpm <- counts[counts$year == 2024,
                  c("site_id", "year", "n_jt", "c_jt_frpm")]
d0_frpm$indicator <- "frpm"
d0_frpm$c_jt <- d0_frpm$c_jt_frpm
d0_frpm <- d0_frpm[c("site_id", "year", "indicator", "c_jt", "n_jt")]
head(d0_frpm, 3)
#> # A tibble: 3 × 5
#>   site_id  year indicator  c_jt  n_jt
#>   <chr>   <int> <chr>     <int> <int>
#> 1 S001     2024 frpm          4    10
#> 2 S002     2024 frpm          2     9
#> 3 S003     2024 frpm          6    11
```

Estimate:

``` r

est_d0 <- sm_estimate_from_aggregates(
  d0_frpm,
  family    = "binomial",
  indicator = "frpm"
)
head(est_d0, 3)
#> sitemix_estimates: 3 rows x 18 columns | family=binomial | role=summary_uncertainty
#> groups=3 sites=3 years=1 indicators=1 V=FALSE K=FALSE
#> # A tibble: 3 × 18
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 frpm          0.4       0.685  0.155 0.158    10    10
#> 2 S002     2024 frpm          0.222     0.491  0.139 0.167     9     9
#> 3 S003     2024 frpm          0.545     0.831  0.150 0.151    11    11
#> # ℹ 9 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>
unique(est_d0$estimate_scale)
#> [1] "arcsine"
```

The output is identical in shape to
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
Scenario A output; the only difference is `input_mode = "aggregate"`.

## 4. Scenario D1 — multiple aggregate marginals per site-year

D1 is the case where the publisher reports several marginal indicators
per site-year (e.g., FRPM rate + SNAP rate + WIC rate). Each marginal
becomes a row; cross-marginal correlation is unidentified from marginals
alone, so the engine emits a diagonal working-independence `V` (when
`vjt = TRUE`).

Tell the engine whether those marginals describe the same observed units
with `sampling_relation`. Use `"same_units"` only with source
documentation that establishes a common sample, `"different_units"` when
the marginal samples are known to differ, and the default `"unknown"`
otherwise. Equal denominators are recorded as a denominator pattern;
they do not prove common units.

``` r

# Build a long-form D1 slice with two marginals (FRPM, SNAP):
d1 <- counts[counts$year == 2024, ]
d1_long <- rbind(
  data.frame(
    site_id   = d1$site_id, year = d1$year,
    indicator = "frpm",
    c_jt      = d1$c_jt_frpm, n_jt = d1$n_jt
  ),
  data.frame(
    site_id   = d1$site_id, year = d1$year,
    indicator = "snap",
    c_jt      = d1$c_jt_snap, n_jt = d1$n_jt
  )
)
head(d1_long, 4)
#>   site_id year indicator c_jt n_jt
#> 1    S001 2024      frpm    4   10
#> 2    S002 2024      frpm    2    9
#> 3    S003 2024      frpm    6   11
#> 4    S004 2024      frpm    3    9
```

Estimate:

``` r

est_d1 <- capture_expected_sitemix_warning(
  sm_estimate_from_aggregates(
    d1_long,
    family = "multivariate",
    indicator_col = "indicator",
    sampling_relation = "same_units",
    vjt = TRUE
  ),
  "sitemix_warning_working_independence_default"
)
head(est_d1, 4)
#> sitemix_estimates: 4 rows x 20 columns | family=multivariate | role=summary_uncertainty
#> groups=2 sites=2 years=1 indicators=2 V=TRUE K=TRUE
#> # A tibble: 4 × 20
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 frpm          0.4       0.685  0.155 0.158    10    10
#> 2 S001     2024 snap          0.6       0.886  0.155 0.158    10    10
#> 3 S002     2024 frpm          0.222     0.491  0.139 0.167     9     9
#> 4 S002     2024 snap          0.333     0.615  0.157 0.167     9     9
#> # ℹ 11 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>,
#> #   V <list>, K <int>
```

The working-independence `V` matrix for site `S001`:

``` r

V_d1 <- as.matrix(est_d1$V[[1L]])
round(V_d1, 4)
#>       frpm  snap
#> frpm 0.025 0.000
#> snap 0.000 0.025
est_d1$V[[1L]]$vcov_method
#> [1] "working_independence"
attr(est_d1, "sampling_relation")
#> [1] "same_units"
attr(est_d1, "denominator_pattern")
#> [1] "common"
attr(est_d1, "d1_regime")
#> [1] "D1a"
```

`vcov_method = "working_independence"` — the off-diagonals are exactly
zero because cross-marginal covariance is not identified from publisher
marginals alone. For formal raw pairwise intervals and explicitly
non-bound projected stress scenarios, see [A7 · Variance smoothing and
Fréchet](https://joonho112.github.io/sitemix/articles/a7-variance-smoothing-and-frechet.md)
and the formal derivation in [M7 · Fréchet envelope
theory](https://joonho112.github.io/sitemix/articles/m7-frechet-envelope-theory.md).

## 5. Subgroup files — Framing X vs Framing Y

When the publisher file carries one row per `(site, year, subgroup)`
triple, you must pivot before estimation. The choice between Framing X
and Framing Y depends on your question.

Both helpers map the fixed publisher total vocabulary `ALL`,
`ALL STUDENT(S)`, `TOTAL`, and `OVERALL` to canonical `ALL`; matching
ignores case, surrounding whitespace, and punctuation between words.
Framing X keeps the publisher spelling in `source_subgroup` for audit
while using canonical `ALL` in its composite key, and Framing Y uses
canonical `ALL` as the indicator. Alias collisions fail as duplicates.
Mixed school/district/state rows are not routed automatically: split
them into homogeneous reporting-level tables before pivoting.

``` r

# Synthetic subgroup file (5 sites × 2 subgroups × 1 year)
subgroups <- expand.grid(
  site_id  = paste0("S", sprintf("%03d", 1:5)),
  year     = 2024L,
  subgroup = c("ell", "non_ell"),
  stringsAsFactors = FALSE
)
subgroups$c_jt <- c(8, 4, 7, 5, 9, 3, 10, 6, 5, 8)
subgroups$n_jt <- c(12, 6, 11, 9, 13, 7, 15, 10, 8, 12)
head(subgroups, 4)
#>   site_id year subgroup c_jt n_jt
#> 1    S001 2024      ell    8   12
#> 2    S002 2024      ell    4    6
#> 3    S003 2024      ell    7   11
#> 4    S004 2024      ell    5    9
```

**Framing X** — each subgroup becomes its own site (composite
`site_id`):

``` r

fx <- sm_pivot_subgroups_to_sites(
  subgroups,
  subgroup_col    = "subgroup",
  numerator_col   = "c_jt",
  denominator_col = "n_jt",
  indicator       = "frpm_take_up"
)
head(fx, 4)
#> # A tibble: 4 × 9
#>   site_id     year indicator  c_jt  n_jt suppression_flag framing source_site_id
#>   <chr>      <int> <chr>     <int> <int> <lgl>            <chr>   <chr>         
#> 1 S001_ell    2024 frpm_tak…     8    12 FALSE            subgro… S001          
#> 2 S001_non_…  2024 frpm_tak…     3     7 FALSE            subgro… S001          
#> 3 S002_ell    2024 frpm_tak…     4     6 FALSE            subgro… S002          
#> 4 S002_non_…  2024 frpm_tak…    10    15 FALSE            subgro… S002          
#> # ℹ 1 more variable: source_subgroup <chr>
```

The Framing-X default keeps `partition_target = "none"` and returns
conditional rates. The only composition options are
`"denominator_composition"` and `"case_composition"`; both require the
same complete category grid and one canonical `ALL` total in every
site-year. A missing category row is an error, not an implicit zero.
Suppressed numerators rule out case composition, whereas denominator
composition is available only when every required denominator is
observed.

**Framing Y** — each subgroup becomes a marginal indicator of the
original site. Subgroups commonly describe different observational
units, so use `sampling_relation = "different_units"` unless the source
establishes otherwise:

``` r

fy <- sm_pivot_subgroups_to_indicators(
  subgroups,
  subgroup_col    = "subgroup",
  numerator_col   = "c_jt",
  denominator_col = "n_jt"
)
head(fy, 4)
#> # A tibble: 4 × 8
#>   site_id  year indicator source_subgroup  c_jt  n_jt suppression_flag framing  
#>   <chr>   <int> <chr>     <chr>           <int> <int> <lgl>            <chr>    
#> 1 S001     2024 ell       ell                 8    12 FALSE            subgroup…
#> 2 S001     2024 non_ell   non_ell             3     7 FALSE            subgroup…
#> 3 S002     2024 ell       ell                 4     6 FALSE            subgroup…
#> 4 S002     2024 non_ell   non_ell            10    15 FALSE            subgroup…

est_fy <- sm_estimate_from_aggregates(
  fy,
  family = "multivariate",
  indicators = attr(fy, "indicator_set"),
  framing = "subgroup_as_indicator",
  sampling_relation = "different_units"
)
```

Pass either pivoted tibble to
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
with the appropriate family.

## 6. Publisher-side suppression audit

Before estimation, count how many rows are Tier 1
(publisher-suppressed), Tier 2 (observed below `accountability_n`), or
Tier 3 (publishable). Use
[`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md).

``` r

d0_with_subgroup <- d0_frpm
d0_with_subgroup$subgroup <- "all"
report <- sm_suppression_report(
  d0_with_subgroup,
  by              = c("subgroup", "year"),
  numerator_col   = "c_jt",
  denominator_col = "n_jt",
  indicator_col   = "indicator",
  subgroup_col    = "subgroup"
)
print(as.data.frame(report), row.names = FALSE)
#>  subgroup year n_rows n_tier1 n_tier2 n_tier3 n_suppressed_hidden_denominator
#>       all 2024     50       0      29      21                               0
#>  n_denominator_missing pct_suppressed pct_below_accountability
#>                      0              0                     0.58
#>  median_n_suppressed denominator_observed_on_suppressed suppression_sources
#>                   NA                               TRUE                    
#>       recommended_action sensitivity_role
#>  no_suppression_detected             none
#>  sensitivity_numeric_variance_available sensitivity_requires_acknowledgement
#>                                   FALSE                                FALSE
#>  upper_bound_role upper_bound_numeric_variance_available
#>    not_applicable                                  FALSE
#>  upper_bound_requires_acknowledgement
#>                                 FALSE
```

If `pct_suppressed` is high, the default `suppression = "drop"` retains
unavailable audit rows with canonical estimates and SEs missing. The
legacy `suppression = "upper_bound"` option is not imputation: with
`suppression_sensitivity_acknowledge = TRUE`, it stores a worst-case
Bernoulli variance scenario only in separated `sensitivity_*` fields. It
never populates canonical estimates or ordinary covariance. When the
denominator is hidden, even those numeric sensitivity variances remain
missing.

## 7. Audit

``` r

stopifnot(nrow(est_d0) == 50L)
stopifnot(identical(unique(est_d0$estimate_scale), "arcsine"))
stopifnot(nrow(est_d1) == 100L)  # 50 sites × 2 marginals
stopifnot(est_d1$V[[1]]$vcov_method == "working_independence")
stopifnot(all(diag(as.matrix(est_d1$V[[1]])) > 0))
stopifnot(max(abs(as.matrix(est_d1$V[[1]])[lower.tri(as.matrix(est_d1$V[[1]]))])) < 1e-12)
stopifnot(nrow(fx) == 10L)  # 5 sites × 2 subgroups
stopifnot(nrow(fy) == 10L)
```

## 8. What’s next?

- [A6 · Diagnostics and
  suppression](https://joonho112.github.io/sitemix/articles/a6-diagnostics-and-suppression.md)
  for the full three-tier audit workflow.
- [A7 · Variance smoothing and
  Fréchet](https://joonho112.github.io/sitemix/articles/a7-variance-smoothing-and-frechet.md)
  for D1 raw pairwise Fréchet intervals and projected stress scenarios.
- [M5 · Aggregate engines D0 /
  D1](https://joonho112.github.io/sitemix/articles/m5-aggregate-engines.md)
  for the formal derivations.

## References
