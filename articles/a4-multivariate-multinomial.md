# A4 · Scenarios B / C — multivariate and multinomial

Abstract

For applied researchers whose data have more than one outcome per
site-year and need joint covariance. This vignette disambiguates
Scenarios B (overlapping binary indicators with SUR-style covariance)
and C (mutually exclusive categories with simplex covariance) and shows
how to inspect the `V` list-column.

## Overview

### 1. Why you are here

Priya runs an accountability framework with both `FRPM` status (binary)
and a language-of-instruction category (4 mutually- exclusive levels).
Her downstream analysis compares indicators jointly, so she needs not
just SEs but **covariance** — the `V` list-column. She also needs to
decide between Scenario B (overlapping binaries) and Scenario C
(mutually exclusive categories) before her first call.

**What you will leave with.** By the end of this article you will be
able to:

1.  Distinguish overlapping-indicator (Scenario B) covariance from
    mutually-exclusive (Scenario C) covariance before your first call.
2.  Produce a `sitemix_estimates` tibble that carries a joint `V`
    list-column via `vjt = TRUE`.
3.  Read the `vcov_scale` and `vcov_method` metadata before consuming a
    covariance matrix.

**Prerequisites.** [A2 · Input
formats](https://joonho112.github.io/sitemix/articles/a2-input-formats.md).

> **About the example data.** All results here are computed live from
> `alprek_subset`, an anonymized, illustrative 50-site sample of Alabama
> First Class Pre-K records shipped in the package (see
> [`?alprek_subset`](https://joonho112.github.io/sitemix/reference/alprek_subset.md)),
> and a synthetic `primary_language` category for the Scenario C
> example. It is not a real accountability dataset and must not be cited
> as empirical Pre-K results. Every code block runs offline with a fixed
> random seed.

## 2. Scenario B vs Scenario C — the disambiguation

Two questions in order:

1.  **“Can a single student be in two indicators simultaneously?”** Yes
    → Scenario B (overlapping binaries). No → go to question 2.
2.  **“Do my indicators partition the population (each student belongs
    to exactly one)?”** Yes → Scenario C (multinomial).

Concrete examples:

- FRPM / SNAP / WIC / TANF: a student can be in any subset. Scenario B.
- Primary language ∈ {English, Spanish, Other}: a student is in exactly
  one. Scenario C.

The two scenarios share the structure “multiple components per site-year
with non-diagonal covariance,” but they differ in **which covariance**:
Scenario B uses SUR-style cross-covariance from joint proportions;
Scenario C uses the multinomial simplex covariance
`(diag(π) − π π') / n`.

Both scenarios accept student rows through
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
and complete sufficient counts through
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md).
Scenario B count input must include every pairwise co-occurrence count;
Scenario C count input must include every category count and those
counts must sum to `n_jt`. Published aggregate marginals are a separate
D1 multivariate path and are never routed to Scenario C.

## 3. Scenario B — overlapping binary indicators

Call
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
with `family = "multivariate"` and an `indicators` vector of column
names. Set `vjt = TRUE` to attach the per-row `V` list-column.

``` r

benefits_2024 <- sm_estimate(
  subset(alprek_subset, year == 2024),
  family     = "multivariate",
  indicators = c("frpm", "snap", "wic", "tanf"),
  vjt        = TRUE
)
head(benefits_2024, 4)
#> sitemix_estimates: 4 rows x 20 columns | family=multivariate | role=summary_uncertainty
#> groups=1 sites=1 years=1 indicators=4 V=TRUE K=TRUE
#> # A tibble: 4 × 20
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 frpm            0.4     0.685 0.155  0.158    10    10
#> 2 S001     2024 snap            0.6     0.886 0.155  0.158    10    10
#> 3 S001     2024 wic             0.9     1.25  0.0949 0.158    10    10
#> 4 S001     2024 tanf            0.1     0.322 0.0949 0.158    10    10
#> # ℹ 11 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>,
#> #   V <list>, K <int>
```

Each site-year emits 4 rows (one per indicator). The covariance across
the four indicators for site `S001` is in the first element of the `V`
list-column:

``` r

V_S001 <- benefits_2024$V[[1L]]
round(as.matrix(V_S001), 4)
#>       frpm  snap   wic  tanf
#> frpm 0.024 0.016 0.004 0.006
#> snap 0.016 0.024 0.006 0.004
#> wic  0.004 0.006 0.009 0.001
#> tanf 0.006 0.004 0.001 0.009
V_S001$vcov_scale
#> [1] "raw"
V_S001$vcov_method
#> [1] "sur"
```

**The `vcov_scale` caveat.** Note that `vcov_scale = "raw"`: the
covariance matrix is computed on the raw probability scale, even though
`theta_hat` is on the arcsine scale (the default). This is because the
SUR construction is numerically stable in raw space; the matrix is not
currently transformed to the arcsine-delta scale.

The practical implication: **do not assume `sqrt(diag(V)) == se`**. The
row-level `se` is on the arcsine scale; the matrix is on the raw scale.
Always read `V[[i]]$vcov_scale` before consuming the matrix.

## 4. Scenario C — mutually exclusive categories

`alprek_subset` does not ship with a mutually-exclusive category, so
this section constructs a deterministic synthetic example on the same
site-year grid.

``` r

language_data <- transform(
  alprek_subset,
  primary_language = sample(
    c("english", "spanish", "other"),
    nrow(alprek_subset),
    replace = TRUE,
    prob    = c(0.70, 0.25, 0.05)
  )
)
language_data <- language_data[
  c("student_id", "site_id", "year", "primary_language")
]
head(language_data, 5)
#>   student_id site_id year primary_language
#> 1    ST00001    S001 2021          english
#> 2    ST00030    S001 2021          english
#> 3    ST00031    S001 2021          english
#> 4    ST00032    S001 2021          spanish
#> 5    ST00037    S001 2021          english
```

Now call
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
with `family = "multinomial"`:

``` r

language_2024 <- sm_estimate(
  subset(language_data, year == 2024),
  family    = "multinomial",
  indicator = "primary_language",
  vjt       = TRUE
)
head(language_2024, 6)
#> sitemix_estimates: 6 rows x 20 columns | family=multinomial | role=summary_uncertainty
#> groups=2 sites=2 years=1 indicators=3 V=TRUE K=TRUE
#> # A tibble: 6 × 20
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 english       0.6       0.886 0.155  0.158    10    10
#> 2 S001     2024 other         0         0     0.0708 0.158    10    10
#> 3 S001     2024 spanish       0.4       0.685 0.155  0.158    10    10
#> 4 S002     2024 english       0.556     0.841 0.166  0.167     9     9
#> 5 S002     2024 other         0         0     0.0763 0.167     9     9
#> 6 S002     2024 spanish       0.444     0.730 0.166  0.167     9     9
#> # ℹ 11 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>,
#> #   V <list>, K <int>
```

Three rows per site-year (one per category). The covariance for site
`S001`:

``` r

V_lang_S001 <- as.matrix(language_2024$V[[1L]])
round(V_lang_S001, 4)
#>         english other spanish
#> english   0.024     0  -0.024
#> other     0.000     0   0.000
#> spanish  -0.024     0   0.024
```

The row sums of the simplex covariance are zero (the simplex
constraint):

``` r

rowSums(V_lang_S001)
#> english   other spanish 
#>       0       0       0
```

## 5. Preparing scalar and joint uncertainty inputs

Some downstream analyses want one estimate and standard error per
indicator rather than a joint matrix. Select the canonical columns and
split them directly:

``` r

scalar_inputs <- split(
  as.data.frame(benefits_2024[, c(
    "site_id", "year", "indicator", "theta_hat", "se",
    "estimate_scale", "var_method"
  )]),
  benefits_2024$indicator
)
names(scalar_inputs)
#> [1] "frpm" "snap" "tanf" "wic"
```

This preserves the estimate scale and variance provenance instead of
silently translating them. Analyses that use cross-indicator uncertainty
should consume the `V` list-column and its metadata directly; see [A8 ·
Downstream
workflows](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md).

## 6. Audit

``` r

stopifnot(nrow(benefits_2024) == 200L)  # 50 sites × 4 indicators
stopifnot(inherits(benefits_2024$V[[1]], "sm_vcov"))
stopifnot(benefits_2024$V[[1]]$vcov_method == "sur")
stopifnot(benefits_2024$V[[1]]$vcov_scale == "raw")
stopifnot(min(eigen(as.matrix(benefits_2024$V[[1]]),
                    symmetric = TRUE)$values) >= -1e-10)
stopifnot(nrow(language_2024) == 150L)  # 50 sites × 3 categories
stopifnot(language_2024$V[[1]]$vcov_method == "multinomial")
stopifnot(max(abs(rowSums(as.matrix(language_2024$V[[1]])))) < 1e-10)
```

## 7. What’s next?

- [A6 · Diagnostics and
  suppression](https://joonho112.github.io/sitemix/articles/a6-diagnostics-and-suppression.md)
  for the `level = "vcov"` diagnostic that confirms PSD and reports the
  condition number.
- [A8 · Downstream
  workflows](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md)
  — when the joint covariance is consumed by a downstream analysis.
- [M3 · Multivariate SUR
  covariance](https://joonho112.github.io/sitemix/articles/m3-multivariate-sur-covariance.md)
  for the formal SUR derivation.
- [M4 · Multinomial
  simplex](https://joonho112.github.io/sitemix/articles/m4-multinomial-simplex.md)
  for the formal simplex-covariance derivation.

## References
