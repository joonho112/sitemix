# Choosing an input format

Abstract

Choose an estimation function from the information in your data:
individual student records, sufficient counts, or published aggregates.
Examples show the required columns and the conditions under which counts
reproduce estimates from student records.

Choose the function by what each row represents and which counts are
available. For a single binary indicator, a numerator and denominator
contain the information needed to reproduce the calculation from student
records. For several indicators, the distinction between marginal
counts, joint counts, and category counts matters.

## Match your data to a function

| Information in the file | Required columns | Function |
|:---|:---|:---|
| One row per student-year | Site and year, plus binary indicators or a categorical variable | [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md) (A/B/C) |
| One row of sufficient counts per site-year | Site and year, `n_jt`, and the complete counts for the chosen family | [`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md) (A/B/C) |
| Published numerator/denominator pairs, possibly with suppression or missing joint counts | Site and year, indicator labels, and numerator/denominator columns in long or wide form | [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md) (D0/D1) |

The labels A/B/C/D0/D1 identify these combinations in the documentation.
A publisher can supply complete sufficient counts, and those counts can
be used with
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md).
The choice depends on the information retained in the file, not who
created it. Separate counts for FRPM and SNAP, for example, do not
reveal how many students receive both.

The examples use the bundled `prek_sim` records and counts derived from
them. These describe 50 simulated pre-kindergarten sites and no real
children or programs; the results illustrate calculations, not empirical
Pre-K findings. [Getting started with site-level
proportions](https://joonho112.github.io/sitemix/articles/a1-getting-started.md)
explains the main returned columns.

## Estimate from student records

For one binary indicator, the data need `site_id`, `year`, and a logical
or numeric 0/1 column. Here `frpm` records free and reduced-price meal
status, and the call estimates one proportion per site in 2024.

``` r

data(prek_sim, package = "sitemix")
est_rows <- sm_estimate(
  subset(prek_sim, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)
head(est_rows, 3)
#> sitemix_estimates: 3 rows x 18 columns | family=binomial | role=summary_uncertainty
#> groups=3 sites=3 years=1 indicators=1 V=FALSE K=FALSE
#> # A tibble: 3 × 18
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 frpm          0.111     0.340  0.105 0.167     9     9
#> 2 S002     2024 frpm          0.8       1.11   0.126 0.158    10    10
#> 3 S003     2024 frpm          0.5       0.785  0.177 0.177     8     8
#> # ℹ 9 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>
```

Each returned row contains a site’s raw proportion in `theta_raw` and
the estimate and standard error on `estimate_scale` in `theta_hat` and
`se`. The default scale is `"arcsine"` for all three functions shown
here. Supply `id_cols` if the site and year columns have different
names.

For overlapping binary indicators, use `family = "multivariate"` and
name the columns in `indicators`. For a mutually exclusive categorical
variable, use `family = "multinomial"` and name its column in
`indicator`. Those two analyses are illustrated in [Analyzing
overlapping indicators and mutually exclusive
categories](https://joonho112.github.io/sitemix/articles/a4-multivariate-multinomial.md).

## Estimate from sufficient counts

A sufficient-count table has one row per site-year. Along with the site
and year columns, the required counts depend on `family`:

- `"binomial"` (A): `n_jt` and one `c_jt_<indicator>` column.
- `"multivariate"` (B): `n_jt`, every marginal count, and a
  co-occurrence count for every pair. Pair names follow the order in
  `indicators`: `indicators = c("frpm", "snap")` requires `c_jt_frpm`,
  `c_jt_snap`, and `c_jt_frpm_snap`.
- `"multinomial"` (C): `n_jt` and at least two `c_jt_<category>` columns
  whose sum is `n_jt` in each row. `indicators` specifies category
  order.

For overlapping indicators, `vjt = TRUE` requests covariance and checks
whether the counts can come from one common sample. That check supports
two or three indicators. Counts for four or more indicators are accepted
with `vjt = FALSE`; requesting covariance raises
`sitemix_error_input_indicator_count`. Use student records if you need
covariance for four or more overlapping indicators. Complete pairwise
counts are still required for the multivariate counts input when
`vjt = FALSE`.

The package includes a count table derived from `prek_sim`. Select the
denominator and SNAP count for 2024, then name that indicator in the
call:

``` r

counts_path <- system.file(
  "extdata", "prek_sim_counts.rds",
  package = "sitemix", mustWork = TRUE
)
counts <- readRDS(counts_path)
head(counts, 3)
#> # A tibble: 3 × 13
#>   site_id  year  n_jt c_jt_frpm c_jt_snap c_jt_wic c_jt_tanf c_jt_frpm_snap
#>   <chr>   <int> <int>     <int>     <int>    <int>     <int>          <int>
#> 1 S001     2021     8         1         1        2         0              0
#> 2 S001     2022    12         0         2        1         0              0
#> 3 S001     2023     6         0         0        1         0              0
#> # ℹ 5 more variables: c_jt_frpm_wic <int>, c_jt_frpm_tanf <int>,
#> #   c_jt_snap_wic <int>, c_jt_snap_tanf <int>, c_jt_wic_tanf <int>

snap_counts <- counts[
  counts$year == 2024,
  c("site_id", "year", "n_jt", "c_jt_snap")
]
est_counts <- sm_estimate_from_counts(
  snap_counts,
  family    = "binomial",
  indicator = "snap"
)
head(est_counts, 3)
#> sitemix_estimates: 3 rows x 18 columns | family=binomial | role=summary_uncertainty
#> groups=3 sites=3 years=1 indicators=1 V=FALSE K=FALSE
#> # A tibble: 3 × 18
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 snap          0.222     0.491  0.139 0.167     9     9
#> 2 S002     2024 snap          0.7       0.991  0.145 0.158    10    10
#> 3 S003     2024 snap          0.125     0.361  0.117 0.177     8     8
#> # ℹ 9 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>
```

The `c_jt_snap` column supplies the numerator and `n_jt` the
denominator. The result has one row per site for SNAP, with the same
estimate and standard-error columns as the student-record result. This
example uses a different indicator from the FRPM example above; the
comparison below uses SNAP for both inputs.

## Estimate from published aggregates

Use
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
for a published single indicator (`family = "binomial"`, D0) or several
published marginal indicators (`family = "multivariate"`, D1). In long
form, the standard columns are `site_id`, `year`, `indicator`, `c_jt`,
and `n_jt`. A D0 call can supply the indicator label through `indicator`
if that column is absent.

In wide form, use `c_jt_<indicator>` numerator columns and either a
common `n_jt` or one `n_jt_<indicator>` for each numerator. Do not mix
the two denominator forms. Column-mapping arguments such as
`numerator_col` and `denominator_col` allow other source column names;
`NULL` uses the standard names. The [function
help](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
lists the full mappings, including subgroup and suppression flags.

To illustrate D0 without an external download, arrange the simulated
FRPM counts in the standard long form:

``` r

# Arrange the simulated counts as published numerator/denominator rows:
d0 <- counts[counts$year == 2024, c("site_id", "year", "n_jt", "c_jt_frpm")]
d0$indicator <- "frpm"
d0$c_jt <- d0$c_jt_frpm
d0 <- d0[c("site_id", "year", "indicator", "c_jt", "n_jt")]

est_agg <- sm_estimate_from_aggregates(
  d0,
  family    = "binomial",
  indicator = "frpm"
)
head(est_agg, 3)
#> sitemix_estimates: 3 rows x 18 columns | family=binomial | role=summary_uncertainty
#> groups=3 sites=3 years=1 indicators=1 V=FALSE K=FALSE
#> # A tibble: 3 × 18
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 frpm          0.111     0.340  0.105 0.167     9     9
#> 2 S002     2024 frpm          0.8       1.11   0.126 0.158    10    10
#> 3 S003     2024 frpm          0.5       0.785  0.177 0.177     8     8
#> # ℹ 9 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>
```

Here `theta_raw` is the observed `c_jt / n_jt`. It describes the same
FRPM records as `est_rows`; only the input form has changed. For
complete, non-suppressed counts, matching options give the same
numerical estimates and standard errors, while fields describing the
input type can differ.

For D1, marginal counts do not identify cross-indicator covariance.
Requested `V` matrices use working independence: their off-diagonal
zeros are an assumption. Record whether the marginals refer to the same
observational units with `sampling_relation`; equal denominators alone
do not establish that they do.

Missing joint counts are different from suppressed marginal counts. The
former leave dependence unknown even when all marginal proportions are
observed. Suppression hides a numerator or denominator needed for a
particular rate. With an observed denominator, `suppression = "drop"`
retains suppressed rows with missing estimates and SEs. Separately
acknowledged variance-sensitivity values do not recover those estimates
or supply ordinary covariance. Hidden denominators require additional
settings and cannot support numeric sensitivity variances. See
[Estimating proportions from published
aggregates](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md)
for D1, subgroup files, and suppression handling.

Published marginals also do not establish a multinomial composition.
`family = "multinomial"` is rejected by the aggregate function; use
student categories or complete sufficient category counts for Scenario
C.

## Compare student records with their counts

The counts must summarize the same retained student records, including
the same missing-value exclusions. Match the family, indicator order,
transformations, corrections, and other estimation options. Under these
conditions, estimates and standard errors agree up to numerical
rounding; covariance also agrees for requests supported by the count
input.

This comparison uses the same 2024 SNAP records and default options:

``` r

snap_2024 <- sm_estimate(
  subset(prek_sim, year == 2024),
  family    = "binomial",
  indicator = "snap"
)
snap_counts <- counts[counts$year == 2024, c("site_id", "year", "n_jt", "c_jt_snap")]
snap_from_counts <- sm_estimate_from_counts(
  snap_counts,
  family    = "binomial",
  indicator = "snap"
)
stopifnot(identical(snap_from_counts$site_id, snap_2024$site_id))
stopifnot(all.equal(snap_from_counts$theta_raw, snap_2024$theta_raw))
stopifnot(all.equal(snap_from_counts$theta_hat, snap_2024$theta_hat))
stopifnot(all.equal(snap_from_counts$se, snap_2024$se))
```

The checks compare site IDs, raw proportions, transformed estimates, and
standard errors. They pass without printed output. The `input_mode`
column still distinguishes `"student_level"` from `"counts_full_suff"`;
numerical agreement does not require the two objects to have identical
metadata. The binomial calculation is explained in [Binomial standard
errors and
transformations](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md).

Keep the input that preserves the information your analysis needs.
Student records allow you to change the indicator set and missing-value
exclusions. Counts are enough for the supported calculations when the
required counts and their source definitions are available.

## Check the example results

All three examples use the same 50-site panel and the default arcsine
reporting scale. These checks confirm the expected row counts and
scales:

``` r

stopifnot(nrow(est_rows) == 50L)
stopifnot(nrow(est_counts) == 50L)
stopifnot(nrow(est_agg) == 50L)
stopifnot(identical(unique(est_rows$estimate_scale), "arcsine"))
stopifnot(identical(unique(est_agg$estimate_scale), "arcsine"))
```

## Choose the next analysis

- [Estimating proportions for a binary
  indicator](https://joonho112.github.io/sitemix/articles/a3-scenario-binomial.md)
  for transformations and proportions at 0 or 1.
- [Analyzing overlapping indicators and mutually exclusive
  categories](https://joonho112.github.io/sitemix/articles/a4-multivariate-multinomial.md)
  when your data have multiple indicators per site-year.
- [Estimating proportions from published
  aggregates](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md)
  for published marginal counts, suppression, and subgroup tables.
- [Sampling uncertainty in site-level
  proportions](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md)
  for the sampling-uncertainty framework underlying every input path.
