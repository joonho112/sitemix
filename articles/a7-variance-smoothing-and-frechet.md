# A7 · Variance smoothing and Fréchet stress scenarios

Abstract

For applied researchers conducting an opt-in experimental GVF
sensitivity analysis
([`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md))
or evaluating D1 aggregate dependence with formal pairwise intervals and
projected stress scenarios
([`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)).
Canonical SEs remain primary; neither small sample size nor an
unusual-looking SE is by itself a trigger to smooth.

## Overview

### 1. Why you are here

Tariq’s tibble passed
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
but a methods reviewer pointed at the smallest sites and said “I don’t
believe these SEs.” Dana’s D1 aggregate analysis (from A5) lacks
publisher correlation info; her reviewer wants a sensitivity analysis of
the unidentified cross-marginal covariance.

Both readers are in this vignette because it presents two sensitivity
tools: an experimental GVF alternative (available across scenarios) and
Fréchet pairwise intervals with projected stress scenarios
(D1-specific). The first half demonstrates the GVF alternative without
recommending it; the second half covers Fréchet analysis.

### 2. What you will leave with

By the end you will have:

- A scale-specific experimental sensitivity column (`se_smoothed` or
  `se_raw_smoothed`) that leaves the canonical SE primary.
- Formal raw pairwise intervals plus explicitly non-bound projected
  stress scenarios for the D1 sensitivity case.
- Interpretation rules that keep non-identified or experimental
  alternatives separate from canonical sampling uncertainty.

**Prerequisites.** [A1 · Getting
started](https://joonho112.github.io/sitemix/articles/a1-getting-started.md).

> **About the example data.** All results here are computed live from
> `alprek_subset`, an anonymized, illustrative 50-site sample of Alabama
> First Class Pre-K records shipped in the package (see
> [`?alprek_subset`](https://joonho112.github.io/sitemix/reference/alprek_subset.md)).
> The smoothing section runs on a `sitemix_estimates` tibble produced
> from that sample, and the Fréchet section uses its companion
> `alprek_subset_counts` count file. It is not a real accountability
> dataset and must not be cited as empirical Pre-K results. Every code
> block runs offline with a fixed random seed.

## 3. Variance smoothing (`sm_smooth_variance()`)

The following call demonstrates the experimental generalized
variance-function (GVF) / log-variance sensitivity alternative. It is
not a recommended response to small cells, and an unusual-looking SE is
not evidence that the fitted alternative is better:

``` r

est <- sm_estimate(
  subset(alprek_subset, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)
est_s <- sm_smooth_variance(est, method = "loglinear")
head(est_s[, c("site_id", "n", "se", "se_smoothed")], 6)
#> # A tibble: 6 × 4
#>   site_id     n    se se_smoothed
#>   <chr>   <int> <dbl>       <dbl>
#> 1 S001       10 0.158       0.158
#> 2 S002        9 0.167       0.167
#> 3 S003       11 0.151       0.151
#> 4 S004        9 0.167       0.167
#> 5 S005        9 0.167       0.167
#> 6 S006       17 0.121       0.121
```

`se_smoothed` is the cross-row trend-fitted SE on the same scale as
`se`. By default, `est$se` is preserved untouched; pass
`overwrite = TRUE` to replace it (with the original preserved as
`se_pre_smoothing`). The alternative’s provenance is stored in
`var_method_smoothed`, while canonical `var_method` also remains
untouched under the default append-only path. The appended column is a
sensitivity candidate only; canonical `se` remains the primary
uncertainty input unless a separate, prespecified validation supports a
different choice.

This helper is not a Fay–Herriot area-level estimator: it does not
smooth point estimates or fit a between-area outcome model. When a
matching-scale `V` exists, overwrite is rejected because retaining that
matrix would leave a stale diagonal. Append-only smoothing remains
available, and an incompatible-scale `V` is retained with its
relationship recorded in `attr(x, "smoothing")`.

**How to interpret the optional sensitivity.** The smoother is a model
on the log-variance scale; it assumes the bulk of rows lies on a common
log-variance trend. If your data have a few sites that genuinely have a
different variance regime (e.g., a heavily oversampled flagship site),
the smoother will pull their SEs toward the bulk. Inspect
`se_smoothed - se` for outliers before considering the alternative. A
fixed-seed package audit over small/large denominators and
near-boundary/interior rates did not find a candidate that met all
predeclared variance-MSE, coverage, and inverse-weight criteria.
Consequently smoothing remains experimental, append-only, and opt-in;
the audit does not support replacing canonical SEs or treating the
smoothed alternative as a generally improved default.

## 4. GAM smoothing (optional)

For non-linear log-variance trends, the GAM backend uses
[`mgcv::gam`](https://rdrr.io/pkg/mgcv/man/gam.html) under a runtime
guard:

``` r

if (requireNamespace("mgcv", quietly = TRUE)) {
  # Use a raw-output fixture: the default arcsine SE is an exact function of n
  # and can make a spline fit numerically degenerate on this example data.
  est_gam_input <- sm_estimate(
    subset(alprek_subset, year == 2024),
    family = "binomial", indicator = "frpm", vst = "none"
  )
  est_gam <- sm_smooth_variance(est_gam_input, method = "gam")
  head(est_gam[, c("site_id", "n", "se", "se_smoothed")], 6)
} else {
  message("mgcv is not installed; use method = \"loglinear\".")
}
#> # A tibble: 6 × 4
#>   site_id     n    se se_smoothed
#>   <chr>   <int> <dbl>       <dbl>
#> 1 S001       10 0.155       0.147
#> 2 S002        9 0.139       0.154
#> 3 S003       11 0.150       0.140
#> 4 S004        9 0.157       0.154
#> 5 S005        9 0.166       0.154
#> 6 S006       17 0.103       0.113
```

`mgcv` is a Suggests-only dependency. If it is not installed, fall back
to `method = "loglinear"`. The raw-output fixture is deliberate:
unexpected fitting warnings fail the reproducibility audit instead of
being silently accepted as tutorial output.

## 5. Fréchet pairwise intervals and stress scenarios (D1 only)

For same-unit D1a inputs, Fréchet intervals formally constrain each raw
pairwise covariance that is unidentified from publisher marginals alone.
Build a D1 input and call
[`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md):

``` r

counts_path <- system.file(
  "extdata", "alprek_subset_counts.rds",
  package = "sitemix", mustWork = TRUE
)
counts <- readRDS(counts_path)
d1 <- counts[counts$year == 2024, ]
d1_long <- rbind(
  data.frame(
    site_id = d1$site_id, year = d1$year,
    indicator = "frpm",
    c_jt = d1$c_jt_frpm, n_jt = d1$n_jt
  ),
  data.frame(
    site_id = d1$site_id, year = d1$year,
    indicator = "snap",
    c_jt = d1$c_jt_snap, n_jt = d1$n_jt
  )
)
est_d1 <- capture_expected_sitemix_warning(
  sm_estimate_from_aggregates(
    d1_long, family = "multivariate",
    indicator_col = "indicator",
    sampling_relation = "same_units",
    vjt = TRUE
  ),
  "sitemix_warning_working_independence_default"
)

env <- sm_frechet_envelope(est_d1, population_regime = "d1a")
class(env)
#> [1] "sm_frechet_envelope" "list"
env$psd_method
#> [1] "higham"
```

The formal result is `raw_pairwise_intervals`, which records the
marginal probabilities, common denominator, joint-probability endpoints,
correlation endpoints, and raw covariance endpoints. The two PSD
matrices are projected stress scenarios sourced from the negative- and
positive-dependence corners; they are not multivariate lower or upper
bounds. Inspect site `S001`:

``` r

head(env$raw_pairwise_intervals, 3)
#> # A tibble: 3 × 17
#>   site_id  year site_key   indicator_1 indicator_2   p_1   p_2 n_common
#>   <chr>   <int> <chr>      <chr>       <chr>       <dbl> <dbl>    <dbl>
#> 1 S001     2024 S001::2024 frpm        snap        0.4   0.6         10
#> 2 S002     2024 S002::2024 frpm        snap        0.222 0.333        9
#> 3 S003     2024 S003::2024 frpm        snap        0.545 0.455       11
#> # ℹ 9 more variables: joint_probability_lower <dbl>,
#> #   joint_probability_upper <dbl>, pairwise_correlation_lower <dbl>,
#> #   pairwise_correlation_upper <dbl>, pairwise_covariance_lower <dbl>,
#> #   pairwise_covariance_upper <dbl>, interval_scale <chr>,
#> #   covariance_construction <chr>, interval_scope <chr>
V_negative_stress <- as.matrix(env$projected_negative_dependence_stress[[1L]])
V_positive_stress <- as.matrix(env$projected_positive_dependence_stress[[1L]])
round(V_negative_stress, 4)
#>        frpm   snap
#> frpm  0.024 -0.024
#> snap -0.024  0.024
round(V_positive_stress, 4)
#>       frpm  snap
#> frpm 0.024 0.016
#> snap 0.016 0.024
summary(env)[, c("site_key", "scenario", "projection_status",
                 "sign_changes", "projected_order_reversals",
                 "raw_interval_violations",
                 "projection_distance_relative")]
#> # A tibble: 100 × 7
#>    site_key   scenario     projection_status sign_changes projected_order_reve…¹
#>    <chr>      <chr>        <chr>                    <int>                  <int>
#>  1 S001::2024 negative_de… identity_k_le_2              0                      0
#>  2 S001::2024 positive_de… identity_k_le_2              0                      0
#>  3 S002::2024 negative_de… identity_k_le_2              0                      0
#>  4 S002::2024 positive_de… identity_k_le_2              0                      0
#>  5 S003::2024 negative_de… identity_k_le_2              0                      0
#>  6 S003::2024 positive_de… identity_k_le_2              0                      0
#>  7 S004::2024 negative_de… identity_k_le_2              0                      0
#>  8 S004::2024 positive_de… identity_k_le_2              0                      0
#>  9 S005::2024 negative_de… identity_k_le_2              0                      0
#> 10 S005::2024 positive_de… identity_k_le_2              0                      0
#> # ℹ 90 more rows
#> # ℹ abbreviated name: ¹​projected_order_reversals
#> # ℹ 2 more variables: raw_interval_violations <int>,
#> #   projection_distance_relative <dbl>
```

Before projection, each off-diagonal is a raw pairwise interval
endpoint. For `K > 2`, PSD projection may change signs, reverse the
elementwise order of the two source corners, or leave a raw pairwise
interval. The diagnostics report those events directly. Treat the
projected matrices only as stress scenarios, never as bounds or as one
identified sampling covariance matrix.

**D1a vs D1b.** Pass `population_regime = "d1a"` when the marginals
refer to the same sampled units, have one common finite denominator, and
use the IID plug-in rule without FPC or bias correction (formal raw
pairwise intervals). Pass `"d1b"` when the marginals are subgroup-
conditional (heuristic stress test). D1b requires explicit
acknowledgement via `subgroup_conditional_action = "allow"`.

## 6. Audit

``` r

stopifnot("se_smoothed" %in% names(est_s))
stopifnot(all(est_s$se_smoothed > 0))
stopifnot(all(est_s$se == est$se))  # default doesn't overwrite
stopifnot(inherits(env, "sm_frechet_envelope"))
stopifnot(length(env$projected_negative_dependence_stress) == nrow(d1))
stopifnot(all(env$raw_pairwise_intervals$interval_scope ==
              "formal_raw_pairwise_interval"))
```

## 7. What’s next?

- [A6 · Diagnostics and
  suppression](https://joonho112.github.io/sitemix/articles/a6-diagnostics-and-suppression.md)
  if you have not yet audited the tibble.
- [A8 · Downstream
  workflows](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md)
  once smoothing / Fréchet are settled.
- [M6 · Variance smoothing
  theory](https://joonho112.github.io/sitemix/articles/m6-variance-smoothing-theory.md)
  and [M7 · Fréchet envelope
  theory](https://joonho112.github.io/sitemix/articles/m7-frechet-envelope-theory.md)
  for the formal derivations and the D1a / D1b distinction.

## References
