# A3 · Scenario A — binomial estimates

Abstract

For applied researchers running Scenario A (one binary indicator per
site-year, the modal sitemix call) who want to understand the
boundary-method, reporting-transform, and accountability-threshold
knobs. This vignette walks through the knobs with worked examples on
`alprek_subset`.

## Overview

### 1. Why you are here

Asha (back from A1) noticed that some of her sites are tiny: only one or
two students were enrolled in 2024, and the resulting raw proportions
are 0/1 or 1/1. Her colleague asked: “Is the standard error meaningful
at a zero-cell row? And why is the default arcsine instead of the raw
proportion?” She came here for the knobs that answer these questions.

### 2. What you will leave with

By the end you will have:

- Comfort with the three boundary methods (`"wilson_floor"`,
  `"agresti_coull"`, `"none"`) and when to pick which.
- A clear picture of the three reporting transforms (`"arcsine"`,
  `"logit"`, `"none"`) — only `"arcsine"` is variance-stabilizing.
- The role of `accountability_n` and how the `flag_below_accountability`
  flag supports reporting decisions.
- A side-by-side count vs student-row demonstration that confirms the
  T2.5 identity numerically.

**Prerequisites.** [A1 · Getting
started](https://joonho112.github.io/sitemix/articles/a1-getting-started.md).

> **About the example data.** All results here are computed live from
> `alprek_subset`, an anonymized, illustrative 50-site sample of Alabama
> First Class Pre-K records shipped in the package (see
> [`?alprek_subset`](https://joonho112.github.io/sitemix/reference/alprek_subset.md)).
> It is not a real accountability dataset and must not be cited as
> empirical Pre-K results. Every code block runs offline with a fixed
> random seed.

## 3. The boundary methods

Boundary cells are rows where `theta_raw` is exactly 0 or 1 (the
numerator equals 0 or the denominator). Boundary policy matters most on
raw-scale output; under the default arcsine VST, the row-level `se`
remains the closed-form arcsine SE while `se_raw` records the
boundary-safe raw fallback. The example below uses `vst = "none"` so the
`var_method` labels are visible.

``` r

wf <- sm_estimate(
  subset(alprek_subset, year == 2021),
  family          = "binomial",
  indicator       = "tanf",
  vst             = "none",
  boundary_method = "wilson_floor"
)

ac <- sm_estimate(
  subset(alprek_subset, year == 2021),
  family          = "binomial",
  indicator       = "tanf",
  vst             = "none",
  boundary_method = "agresti_coull"
)
```

Inspect a few sites and look at the `var_method` column:

``` r

side_by_side <- data.frame(
  site_id      = wf$site_id,
  n            = wf$n,
  theta_raw    = wf$theta_raw,
  wilson_se    = wf$se,
  wilson_meth  = wf$var_method,
  agresti_se   = ac$se,
  agresti_meth = ac$var_method
)
print(as.data.frame(head(side_by_side, 6)), row.names = FALSE)
#>  site_id  n theta_raw  wilson_se               wilson_meth agresti_se
#>     S001 10 0.0000000 0.07080048 wilson_boundary_surrogate 0.09292054
#>     S002  6 0.0000000 0.09957690 wilson_boundary_surrogate 0.12633581
#>     S003 14 0.0000000 0.05492723 wilson_boundary_surrogate 0.07337853
#>     S004 13 0.1538462 0.10006825                  binomial 0.10006825
#>     S005 17 0.0000000 0.04702080 wilson_boundary_surrogate 0.06335923
#>     S006 10 0.0000000 0.07080048 wilson_boundary_surrogate 0.09292054
#>                      agresti_meth
#>  agresti_coull_boundary_surrogate
#>  agresti_coull_boundary_surrogate
#>  agresti_coull_boundary_surrogate
#>                          binomial
#>  agresti_coull_boundary_surrogate
#>  agresti_coull_boundary_surrogate
```

When raw-scale `theta_raw` is `0` or `1`, the Wilson method records
`"wilson_boundary_surrogate"` and the Agresti-Coull method records
`"agresti_coull_boundary_surrogate"`. Interior rows record `"binomial"`
under raw output.

**When to pick which.** Use `"wilson_floor"` (the default, (Wilson,
1927)) for accountability-style reports: it floors the SE at a small
positive number without shifting `theta_raw`. Use `"agresti_coull"`
(Agresti & Coull, 1998) for the standard z-general adjusted-Wald
boundary uncertainty surrogate. Both methods retain the observed
`theta_raw = C/n`; neither replaces the point estimate. Use `"none"`
only for diagnostic comparisons (zero-cell SEs collapse).

## 4. The reporting transforms

`vst` selects the reporting scale of `theta_hat` and `se` via three
choices:

``` r

arc <- sm_estimate(subset(alprek_subset, year == 2024),
                   family = "binomial", indicator = "frpm",
                   vst = "arcsine")
lgt <- sm_estimate(subset(alprek_subset, year == 2024),
                   family = "binomial", indicator = "frpm",
                   vst = "logit")
raw <- sm_estimate(subset(alprek_subset, year == 2024),
                   family = "binomial", indicator = "frpm",
                   vst = "none")
unique(arc$estimate_scale)
#> [1] "arcsine"
unique(lgt$estimate_scale)
#> [1] "logit"
unique(raw$estimate_scale)
#> [1] "none"
```

The `estimate_scale` column records the chosen scale. The default
`"arcsine"` is the only variance-stabilizing choice; `"logit"` is a
reporting transform useful when downstream models expect log-odds; and
`"none"` retains the raw proportion scale for inspection or direct
reporting (see
[M2](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)).

## 5. The accountability threshold

`accountability_n` is the row-level denominator below which a row is
flagged `flag_below_accountability = TRUE`. The default is 30. Smaller
sites are estimated but flagged so an analyst can apply the project’s
publication or modeling policy explicitly.

``` r

strict <- sm_estimate(
  subset(alprek_subset, year == 2024),
  family           = "binomial",
  indicator        = "frpm",
  accountability_n = 50L
)
table(strict$flag_below_accountability)
#> 
#> FALSE  TRUE 
#>     7    43
```

Setting `accountability_n = 50L` flags rows with fewer than 50
observations. Use a higher threshold when your project’s rules treat
sites below 50 as not publishable.

## 6. Sufficient-counts identity (T2.5 preview)

Demonstrate that the student-row path and the counts path agree on the
same underlying data:

``` r

counts_path <- system.file("extdata", "alprek_subset_counts.rds",
                           package = "sitemix", mustWork = TRUE)
counts <- readRDS(counts_path)

snap_rows <- sm_estimate(subset(alprek_subset, year == 2024),
                         family = "binomial", indicator = "snap")
snap_cnts <- sm_estimate_from_counts(
  counts[counts$year == 2024,
         c("site_id", "year", "n_jt", "c_jt_snap")],
  family = "binomial", indicator = "snap"
)
stopifnot(all.equal(snap_rows$theta_hat, snap_cnts$theta_hat, tolerance = 1e-10))
stopifnot(all.equal(snap_rows$se, snap_cnts$se, tolerance = 1e-10))
```

The two pathways agree to `1e-10`. This is the T2.5 invariant; the
formal derivation lives in [M2 · Scalar SE —
binomial](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md).

## 7. Audit

``` r

stopifnot(nrow(arc) == 50L)
stopifnot(all(arc$indicator == "frpm"))
stopifnot(all(is.finite(arc$theta_hat)))
stopifnot(all(arc$se > 0))
stopifnot(all(wf$var_method[wf$theta_raw %in% c(0, 1)] ==
                    "wilson_boundary_surrogate"))
stopifnot(all(ac$var_method[ac$theta_raw %in% c(0, 1)] ==
                    "agresti_coull_boundary_surrogate"))
```

## 8. What’s next?

- [A4 · Scenarios B / C — multivariate /
  multinomial](https://joonho112.github.io/sitemix/articles/a4-multivariate-multinomial.md)
  if your project has multiple indicators per site-year.
- [A6 · Diagnostics and
  suppression](https://joonho112.github.io/sitemix/articles/a6-diagnostics-and-suppression.md)
  for the per-row audit and the accountability flag tour.
- [M2 · Scalar SE —
  binomial](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
  for the formal delta-method derivation of every SE in this vignette.

## References

Agresti, A., & Coull, B. A. (1998). Approximate is better than “exact”
for interval estimation of binomial proportions. *The American
Statistician*, *52*(2), 119–126.
<https://doi.org/10.1080/00031305.1998.10480550>

Wilson, E. B. (1927). Probable inference, the law of succession, and
statistical inference. *Journal of the American Statistical
Association*, *22*(158), 209–212.
<https://doi.org/10.1080/01621459.1927.10502953>
