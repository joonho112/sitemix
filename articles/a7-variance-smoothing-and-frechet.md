# Variance smoothing and Fréchet sensitivity analysis

Abstract

Compare an optional model for standard errors with the original
estimates, then explore unknown dependence between published
proportions. The examples distinguish experimental smoothed SEs, formal
pairwise Fréchet intervals, and covariance stress scenarios.

[`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
fits a model across rows to produce alternative standard errors.
[`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
explores dependence that cannot be recovered from published marginal
counts. These tools answer different questions: whether a fitted
variance trend changes an analysis, and how unknown covariance between
indicators might affect it. Neither tool estimates new site-level
outcomes.

We use `prek_sim` and its companion count file, both fully simulated.
The first example estimates FRPM at 50 sites in 2024. The second uses
FRPM and SNAP marginals from those same students while withholding their
joint counts. See [Getting started with site-level
proportions](https://joonho112.github.io/sitemix/articles/a1-getting-started.md)
for the estimate columns and [Estimating proportions from published
aggregates](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md)
for aggregate inputs.

## Compare a fitted variance trend with the original SEs

The optional generalized variance-function (GVF) model fits log
variances across eligible rows. With `method = "loglinear"` and the
default `scale = "se"`, the default formula is `log_var ~ log_n`, where
`log_var = log(se^2)` and `log_n = log(n)`. The result is an
experimental sensitivity alternative. Small sample size or an unusual SE
alone does not justify replacing the original uncertainty calculation.

Start with one indicator and one year:

``` r

est <- sm_estimate(
  subset(prek_sim, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)
est_s <- sm_smooth_variance(est, method = "loglinear")
head(est_s[, c("site_id", "n", "se", "se_smoothed")], 6)
#> # A tibble: 6 × 4
#>   site_id     n    se se_smoothed
#>   <chr>   <int> <dbl>       <dbl>
#> 1 S001        9 0.167       0.167
#> 2 S002       10 0.158       0.158
#> 3 S003        8 0.177       0.177
#> 4 S004       14 0.134       0.134
#> 5 S005        8 0.177       0.177
#> 6 S006       11 0.151       0.151
```

The new `se_smoothed` column uses the same scale as `se`, here the
arcsine scale. The original `se`, `var_method`, and point estimates are
unchanged. `var_method_smoothed` labels the alternative, and
`attr(est_s, "smoothing")` records the selected scale, formula, eligible
rows, and fit status.

Here the two SE columns agree to numerical precision. With the default
arcsine calculation and no extra corrections, `se = 1 / (2 * sqrt(n))`
already lies on a straight line after squaring and taking logs.
Agreement in this example is not evidence of improved performance on
other data. The model does not smooth point estimates or fit a
Fay–Herriot outcome model. Improved precision, interval coverage, or
downstream weighting is not guaranteed; see [Experimental models for
variance
smoothing](https://joonho112.github.io/sitemix/articles/m6-variance-smoothing-theory.md)
for the model assumptions and simulation findings.

## Check which rows can enter the model

`scope = "all"` means all eligible rows. A row must not be suppressed,
with finite, strictly positive `n` and a finite, strictly positive SE on
the selected scale. Thus a census row with zero sampling SE is retained
in the output but excluded from the model. The added column copies the
input SE for ineligible rows. Objects containing suppression-sensitivity
rows are rejected.

`scope = "tier2"` further restricts the model to `11 <= n <= 29`. This
is a fixed interval; it does not follow `accountability_n` or the Tier 2
classification in
[`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md).
`min_n` can impose an additional lower limit. All 50 rows in this
example meet the basic eligibility conditions. Count those that also
fall in the fixed interval:

``` r

sum(est$n >= 11L & est$n <= 29L)
#> [1] 27
```

Only 27 rows qualify, below the default `min_rows = 50L` required to
fit. Calling `sm_smooth_variance(est, scope = "tier2")` would therefore
issue `sitemix_warning_smoother_skipped`, copy `se` to `se_smoothed`,
and copy `var_method` to `var_method_smoothed`. There would be no fitted
alternative. A skipped fit performs no overwrite, even if requested, and
adds no new pre-smoothing snapshots, residuals, or model.

If you combine years, the default pools eligible rows in one model.
`by = "year"` adds year fixed effects to the default formula; it does
not fit separate models. For separate fits, call the function on each
year’s subset. `return_diagnostics = TRUE` adds residuals and the fitted
model when a fit succeeds, so you can inspect departures from the trend.

## Keep the selected scale and covariance matrix consistent

The default adds an alternative SE column while preserving the original
columns. `scale = "se_raw"` instead adds `se_raw_smoothed` on the raw
probability scale. Its default model includes `offset(p_offset)` to
account for the rate-dependent factor in the raw variance; the first
raw-scale request after loading the package gives
`sitemix_warning_raw_scale_smoothing`. In contrast, `scale = "se"` uses
the reported scale, which is also raw when `estimate_scale = "none"`.

With a successful fit, `overwrite = TRUE` replaces the selected SE for
eligible rows and preserves its input values in `se_pre_smoothing` or
`se_raw_pre_smoothing`. Overwriting `se_raw` also updates `se` for rows
with `estimate_scale = "none"`, while transformed `se` values remain
unchanged. Use the original SE as the primary input unless a separate
validation supports replacing it.

Smoothing does not update `V`. When an eligible row has a `V` on the
selected scale, overwrite is rejected because the retained matrix would
no longer have a diagonal consistent with the new SE. Adding a separate
alternative column is still possible. A `V` on a different scale is
retained unchanged. Inspect `attr(x, "smoothing")$v` on the returned
object: `relation` records `"absent"`, `"matching"`, `"incompatible"`,
or `"mixed"`, and `matrix_effect` records whether matrices are absent or
unchanged. Do not pair a changed SE with a matrix merely because both
are present in the output.

## Fit a nonlinear trend with the optional GAM method

`method = "gam"` uses
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) to fit a smooth
function of `log_n`. Here the input uses raw output, so the fitted and
original SEs are both on the proportion scale. This avoids fitting a
spline to the exact arcsine relationship demonstrated above:

``` r

if (requireNamespace("mgcv", quietly = TRUE)) {
  # Raw output avoids the exact arcsine SE relationship in this example.
  est_gam_input <- sm_estimate(
    subset(prek_sim, year == 2024),
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
#> 1 S001        9 0.105       0.141
#> 2 S002       10 0.126       0.137
#> 3 S003        8 0.177       0.145
#> 4 S004       14 0.132       0.123
#> 5 S005        8 0.117       0.145
#> 6 S006       11 0.145       0.134
```

`mgcv` is optional; `method = "loglinear"` works without it. A visible
change in this table means the fitted trend differs from the original SE
calculation. It does not show which SE is more accurate. Inspect fit
diagnostics and assess the alternative for the intended analysis.

## Explore dependence when only marginal counts are available

The aggregate example below retains FRPM and SNAP counts but leaves out
their joint count. It therefore uses the D1 input path from [Estimating
proportions from published
aggregates](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md).
Working independence supplies a starting covariance matrix; it is an
assumption, not an observed lack of association between the indicators.

Use `population_regime = "d1a"` for formal raw pairwise intervals only
when the marginals describe the same sampled units and share one finite
denominator per site-year. They must use the IID plug-in raw-probability
variance rule, with no FPC, no `binomial_bc`, and `anscombe = FALSE` so
that `n_eff = n`. These conditions must be recorded in the input
estimates. Equal denominator values alone cannot establish same-unit
sampling. Here the bundled count file was built from the same students,
so `sampling_relation = "same_units"` describes its source:

``` r

counts_path <- system.file(
  "extdata", "prek_sim_counts.rds",
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

The expected warning marks the working-independence assumption.
`raw_pairwise_intervals` is the formal result: one row per site-year and
indicator pair, with joint-probability, correlation, and raw covariance
endpoints. All envelope matrices are on the raw probability scale, even
though `est_d1` reports transformed estimates and SEs. For D1a,
`V_independence` uses the IID plug-in raw diagonal.

## Read the intervals and check the stress scenarios

Inspect the first site, `S001`, in 2024:

``` r

interval_columns <- c(
  "site_id", "n_common",
  "pairwise_covariance_lower", "pairwise_covariance_upper"
)
print(as.data.frame(env$raw_pairwise_intervals[1L, interval_columns]),
      row.names = FALSE)
#>  site_id n_common pairwise_covariance_lower pairwise_covariance_upper
#>     S001        9              -0.002743484               0.009602195
V_negative_stress <- as.matrix(env$projected_negative_dependence_stress[[1L]])
V_positive_stress <- as.matrix(env$projected_positive_dependence_stress[[1L]])
round(V_negative_stress, 4)
#>         frpm    snap
#> frpm  0.0110 -0.0027
#> snap -0.0027  0.0192
round(V_positive_stress, 4)
#>        frpm   snap
#> frpm 0.0110 0.0096
#> snap 0.0096 0.0192
projection_check <- summary(env)
print(as.data.frame(head(projection_check[, c(
  "scenario", "projection_status", "projection_distance_relative"
)], 2)), row.names = FALSE)
#>                    scenario projection_status projection_distance_relative
#>  negative_dependence_stress   identity_k_le_2                            0
#>  positive_dependence_stress   identity_k_le_2                            0
```

The site’s raw pairwise covariance interval is approximately
`[-0.0027435, 0.0096022]`. Each endpoint constrains one pair under the
D1a conditions. The two matrices put the negative and positive endpoint
in their off-diagonal entries. With two indicators, these matrices are
returned unchanged: `projection_status = "identity_k_le_2"` and
projection distance is zero. Although `psd_method` records `"higham"`,
no iterative projection is needed here.

With more than two indicators, assembling all pairwise endpoints can
produce a matrix that is not positive semidefinite (PSD). The function
then adjusts the matrices using the selected method while preserving the
raw diagonal. The resulting matrices are covariance stress scenarios,
not multivariate lower and upper bounds. PSD alone does not show that
the entries are jointly attainable by binary indicators.

Projection may change signs, reverse the elementwise order of the two
scenarios, or move entries outside their raw pairwise intervals. Check
those changes in `projection_diagnostics`, also returned by
[`summary()`](https://rdrr.io/r/base/summary.html):

``` r

colSums(projection_check[, c(
  "sign_changes", "projected_order_reversals", "raw_interval_violations"
)])
#>              sign_changes projected_order_reversals   raw_interval_violations 
#>                         0                         0                         0
```

These totals are zero in this two-indicator example. A larger example
may require closer inspection of each site’s projection status,
distance, and interval departures. Neither matrix identifies the unknown
sampling covariance or supplies a general bound for a later multivariate
analysis.

If the marginals are subgroup-conditional, the input must instead record
`sampling_relation = "different_units"` or `"unknown"`.
`population_regime = "d1b"` then gives a heuristic stress test, not
formal pairwise bounds. `subgroup_conditional_action = "allow"`
acknowledges that use silently; the default `"warn"` proceeds with a
warning, and `"error"` rejects it. A same-unit D1a input cannot be
relabeled as D1b at this step. Both regimes reject suppressed-missing
and suppression-sensitivity rows; see [Pairwise Fréchet bounds and
projected dependence
scenarios](https://joonho112.github.io/sitemix/articles/m7-frechet-envelope-theory.md)
for the conditions and derivations.

## Check these example results

The following checks confirm the original SEs were retained and the
Fréchet result has the intended scope. They do not test whether
smoothing improves an estimator or whether the stress scenarios are a
suitable model for the unknown dependence.

``` r

stopifnot("se_smoothed" %in% names(est_s))
stopifnot(all(est_s$se_smoothed > 0))
stopifnot(all(est_s$se == est$se))  # default doesn't overwrite
stopifnot(inherits(env, "sm_frechet_envelope"))
stopifnot(length(env$projected_negative_dependence_stress) == nrow(d1))
stopifnot(all(env$raw_pairwise_intervals$interval_scope ==
              "formal_raw_pairwise_interval"))
stopifnot(attr(est_s, "smoothing")$n_eligible == nrow(est))
stopifnot(sum(est$n >= 11L & est$n <= 29L) == 27L)
stopifnot(all(!projection_check$projection_attempted))
```

Use [Checking estimates and handling suppressed
data](https://joonho112.github.io/sitemix/articles/a6-diagnostics-and-suppression.md)
to inspect estimate and covariance problems before an optional
sensitivity analysis. [Using estimates and covariance matrices in
further
analyses](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md)
shows how to keep estimates, SEs, and matrices consistent in later
calculations.

## References
