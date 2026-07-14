# A2 · Input formats — student rows, counts, or aggregates?

Abstract

For applied researchers who have site-year data in some form but are not
sure which sitemix entry function applies. This vignette walks through a
three-branch decision tree (student rows / sufficient counts / published
aggregates), gives one runnable example per branch, and shows the
equivalence between branches.

## Overview

### 1. Why you are here

Marco is a state-agency analyst. Three different districts handed him
three different files: District A sent one row per student (student
rows); District B sent pre-aggregated count rows (sufficient counts);
District C sent the publisher’s CSV (a published aggregate). Marco needs
to estimate site-level rates from all three without spending a day
deciding which sitemix function to call.

This vignette is the decision tree.

**What you will leave with.** By the end of this article you will be
able to:

1.  Decide whether your site-year data are student rows, sufficient
    counts, or a published aggregate.
2.  Call the matching entry function
    ([`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md),
    [`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md),
    or
    [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)).
3.  Rely on the count-versus-rows equivalence guarantee when your source
    data are already aggregated.

**Prerequisites.** [A1 · Getting
started](https://joonho112.github.io/sitemix/articles/a1-getting-started.md).

> **About the example data.** All results here are computed live from
> `alprek_subset`, an anonymized, illustrative 50-site sample of Alabama
> First Class Pre-K records shipped in the package (see
> [`?alprek_subset`](https://joonho112.github.io/sitemix/reference/alprek_subset.md)),
> and `alprek_subset_counts` for the sufficient-count path. It is not a
> real accountability dataset and must not be cited as empirical Pre-K
> results. Every code block runs offline with a fixed random seed.

## 2. The three-branch decision tree

Use the table below to pick your entry point.

| Your data look like | Use this function | Scenario |
|:---|:---|:--:|
| One row per **student-year** | [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md) | A/B/C |
| One row per **site-year** with complete family-specific sufficient counts | [`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md) | A/B/C |
| Publisher CSV with one row per **site-year** (or **site-year-subgroup**), with `c_jt` and `n_jt` columns or analogous | [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md) | D0/D1 |

Three questions to disambiguate:

1.  **“Does every row identify one student?”** Yes → branch 1 (student
    rows). No → branch 2 or 3.
2.  **“Does every row carry an explicit numerator and denominator?”**
    Yes → branch 2 (counts) or 3 (aggregates).
3.  **“Is the data product a publisher CSV with a known schema (e.g.,
    state accountability files)?”** Yes → branch 3. Otherwise → branch
    2.

## 3. Branch 1 — student rows

This is the modal entry point and the one A1 demonstrated. One row per
student-year; the indicator column is logical or 0/1.

``` r

data(alprek_subset, package = "sitemix")
est_rows <- sm_estimate(
  subset(alprek_subset, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)
head(est_rows, 3)
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
```

Use when your data are an enrollment file, a student-level
administrative export, or any table where each row is one student in one
year.

## 4. Branch 2 — sufficient counts

When student rows have already been aggregated to one row per site-year
with complete family-specific sufficient statistics, use
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md).
The package ships a count artifact derived deterministically from
`alprek_subset`:

- Scenario A needs `n_jt` and one `c_jt_<indicator>` column.
- Scenario B needs `n_jt`, all ordered marginal counts, and all ordered
  pairwise co-occurrence counts. Count-input feasibility is verified for
  two or three indicators and fails closed at four or more.
- Scenario C needs `n_jt` and at least two `c_jt_<category>` columns
  whose row sum is exactly `n_jt`.

``` r

counts_path <- system.file(
  "extdata", "alprek_subset_counts.rds",
  package = "sitemix", mustWork = TRUE
)
counts <- readRDS(counts_path)
head(counts, 3)
#> # A tibble: 3 × 13
#>   site_id  year  n_jt c_jt_frpm c_jt_snap c_jt_wic c_jt_tanf c_jt_frpm_snap
#>   <chr>   <int> <int>     <int>     <int>    <int>     <int>          <int>
#> 1 S001     2021    10         7         9        8         0              6
#> 2 S001     2022    12         6         6        5         0              5
#> 3 S001     2023     9         6         7        4         1              5
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
#> 1 S001     2024 snap          0.6       0.886  0.155 0.158    10    10
#> 2 S002     2024 snap          0.333     0.615  0.157 0.167     9     9
#> 3 S003     2024 snap          0.455     0.740  0.150 0.151    11    11
#> # ℹ 9 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>
```

## 5. Branch 3 — published aggregates

When the file is a publisher product (you did not aggregate it
yourself), use
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md).
The wrapper supports two cases: **D0** (one numerator/denominator per
site-year) and **D1** (multiple aggregate marginals per site-year).

D1 is a multivariate marginal model, not aggregate multinomial
composition. `family = "multinomial"` is therefore rejected on this
branch; a Scenario C composition must use student rows or complete
sufficient category counts.

``` r

# Build a D0 aggregate slice from the bundled count file:
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
#> 1 S001     2024 frpm          0.4       0.685  0.155 0.158    10    10
#> 2 S002     2024 frpm          0.222     0.491  0.139 0.167     9     9
#> 3 S003     2024 frpm          0.545     0.831  0.150 0.151    11    11
#> # ℹ 9 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>
```

For D1 (multiple marginals) and the publisher-side suppression audit,
read [A5 · Published aggregates D0 /
D1](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md).

## 6. The equivalence guarantee

Branches 1 and 2 must agree on the same underlying data. The
sufficient-counts identity (T2.5 in [M2 · Scalar SE —
binomial](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md))
guarantees:

``` r

snap_2024 <- sm_estimate(
  subset(alprek_subset, year == 2024),
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

The two pathways agree to numerical tolerance. If you have a choice
between student rows and counts, prefer student rows for clarity; the
counts path exists for users whose source data are already aggregated.

## 7. Audit

``` r

stopifnot(nrow(est_rows) == 50L)
stopifnot(nrow(est_counts) == 50L)
stopifnot(nrow(est_agg) == 50L)
stopifnot(identical(unique(est_rows$estimate_scale), "arcsine"))
stopifnot(identical(unique(est_agg$estimate_scale), "arcsine"))
```

## 8. What’s next?

- [A3 · Scenario A —
  binomial](https://joonho112.github.io/sitemix/articles/a3-scenario-binomial.md)
  — the Scenario A deep dive (boundary methods, VST choice, the
  sufficient-counts identity from §6 made formal).
- [A4 · Scenarios B / C — multivariate /
  multinomial](https://joonho112.github.io/sitemix/articles/a4-multivariate-multinomial.md)
  when your data have multiple indicators per site-year.
- [A5 · Published aggregates D0 /
  D1](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md)
  for branch-3 deep dive including subgroup pivots.
- [M1 · Statistical
  foundations](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md)
  for the sampling-uncertainty framework underlying every input path.

## References
