# Smooth standard errors with an experimental variance model

`sm_smooth_variance()` models log variances across rows and adds
smoothed standard errors without changing point estimates. By default it
preserves the original standard errors and adds either `se_smoothed` or
`se_raw_smoothed`, according to `scale`.

This optional generalized variance-function (GVF) model is experimental.
Use its output as a sensitivity alternative; small sample size alone is
not a reason to smooth, and improved precision or downstream performance
is not guaranteed. The function does not fit a Fay–Herriot model or
estimate small-area posterior means.

## Usage

``` r
sm_smooth_variance(
  x,
  method = c("loglinear", "gam"),
  scale = c("se", "se_raw"),
  scope = c("all", "tier2"),
  by = NULL,
  formula = NULL,
  bias_correct = TRUE,
  min_n = NULL,
  min_rows = 50L,
  overwrite = FALSE,
  return_diagnostics = FALSE,
  ...
)
```

## Arguments

- x:

  A `sitemix_estimates` object from
  [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  or one of its wrappers. Suppressed-missing rows may be retained but
  are not smoothed. An object containing non-identified
  suppression-sensitivity rows is rejected; smooth identified estimates
  before conducting a separate sensitivity analysis.

- method:

  A single string: `"loglinear"` (default, using
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html)) or `"gam"` (requires
  the optional `mgcv` package).

- scale:

  The standard-error column to smooth: `"se"` (default, on each row's
  `estimate_scale`) or `"se_raw"` (on the raw probability scale). `se`
  is also raw when `estimate_scale = "none"`.

- scope:

  A single string: `"all"` (default, all eligible rows) or `"tier2"`
  (eligible rows with \\11 \le n \le 29\\). This fixed interval does not
  follow `accountability_n` or `flag_below_accountability`. In
  [`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md),
  Tier 2 instead depends on the report's `accountability_n` threshold.

- by:

  Column names, or `NULL` (default). Each column is converted to a
  factor named `by_1`, `by_2`, and so on in the model data. The default
  formula includes these fixed effects in one model. A custom `formula`
  must include the desired terms explicitly. See *Details* for multiple
  years.

- formula:

  A model formula, or `NULL` for the default described in *Details*. The
  response must be `log_var`. Available variables are `log_var`,
  `log_n`, `n`, `theta_raw`, `p_offset`, and the generated `by_*`
  factors. Use ordinary model terms and
  [`offset()`](https://rdrr.io/r/stats/offset.html); GAM formulas also
  allow `s()`, `te()`, `ti()`, and `t2()`. The offset variable is
  `log(p_star * (1 - p_star))`, where `p_star` uses `theta_raw` in the
  interior and the Wilson center at 0/1 boundaries, then is clipped away
  from 0 and 1 for numerical stability.

- bias_correct:

  A single logical value. If `TRUE` (default), add half the fitted
  residual variance to the predicted log variance before applying the
  exponential function to return to the variance scale.

- min_n:

  A finite numeric threshold of at least 1, or `NULL` (default).
  Eligible rows must also have `n >= min_n`.

- min_rows:

  Minimum number of eligible rows required to fit. Supply a finite
  number of at least 2; it is converted to an integer. Defaults to
  `50L`. See *Details* for skipped fits.

- overwrite:

  A single logical value. Defaults to `FALSE`, which preserves the
  original SE columns. If `TRUE` and a model is fit, also replace the
  selected SEs for eligible rows, keeping snapshots of overwritten
  columns. With `scale = "se_raw"`, rows on `estimate_scale = "none"`
  also have `se` updated. Overwrite is rejected if any eligible row has
  a `V` matrix on the selected scale, because its unchanged diagonal
  could then disagree with the new SE. A `V` on a different scale is
  retained unchanged; its relation to the selected SE is recorded in
  `smoothing`.

- return_diagnostics:

  A single logical value. If `TRUE` and a model is fit, add
  `residual_log_var` and attach the fitted model as `smoother_fit`.
  Defaults to `FALSE`. The `smoother_fit_summary` and `smoothing`
  attributes are recorded regardless of this setting.

- ...:

  Additional arguments passed to
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) or
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html), according to
  `method`.

## Value

A `sitemix_estimates` tibble with the same rows and point estimates as
`x`. The function adds or updates these columns:

- `se_smoothed`, `se_raw_smoothed`:

  The alternative SE: `se_smoothed` for `scale = "se"`, or
  `se_raw_smoothed` for `scale = "se_raw"`. That column is added or
  updated; other SE columns are preserved by default. Ineligible rows
  keep their input values.

- `var_method_smoothed`:

  The calculation label for the alternative SE. Fitted rows receive a
  `" + gvf_smooth_loglinear"` or `" + gvf_smooth_gam"` suffix; other
  rows retain their input `var_method`.

- `residual_log_var`:

  Fit residuals for eligible rows, with `NA` elsewhere. Added only when
  a model is fit and `return_diagnostics = TRUE`.

- `se_pre_smoothing`, `se_raw_pre_smoothing`:

  Copies of the input columns that are overwritten. Added only when a
  model is fit with `overwrite = TRUE`. Raw-scale overwrite also saves
  `se_pre_smoothing` when it updates `se` for rows with
  `estimate_scale = "none"`.

With `overwrite = TRUE`, `var_method` is updated wherever `se` is
replaced. Overwriting `se_raw` on transformed rows leaves their `se` and
`var_method` unchanged. Covariance matrices and finite-population
sampling information are preserved.

The `smoother_fit_summary` attribute records fit status, method,
formula, number of eligible rows, and covariance-scale relation. A
completed fit also records rank, convergence, prediction checks, and
model summaries. The `smoothing` attribute records the settings,
selected output column, eligible row indices, and `v` information. Its
`v$relation` is `"absent"`, `"matching"`, `"incompatible"`, or
`"mixed"`; `v$matrix_effect` records whether matrices are absent or
retained unchanged. Both attributes are recorded for skipped fits. With
`return_diagnostics = TRUE`, a completed fit is additionally attached as
`smoother_fit`; a skipped call adds no fitted model.

## Details

**Eligible rows.** The input is validated before fitting. A row is
eligible only when it is not suppressed and both its selected standard
error and `n` are finite and strictly positive. Thus an exact-census row
with zero SE is excluded. `scope = "tier2"` and `min_n` further restrict
the eligible rows. The model is fit and predictions are applied only to
those rows; the added SE column retains input values elsewhere.

If fewer than `min_rows` rows qualify, fitting is skipped with
`sitemix_warning_smoother_skipped`. The added SE column copies the
selected input column, and `var_method_smoothed` copies `var_method`. No
overwrite is attempted, even when `overwrite = TRUE`; no new
pre-smoothing snapshots, fit residuals, or fitted model are added. Both
summary attributes record the skipped fit.

**Model and grouping.** With `method = "loglinear"`,
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) fits `log_var ~ log_n`
for `scale = "se"`, or `log_var ~ log_n + offset(p_offset)` for
`scale = "se_raw"`. Here `log_var` is the log of the selected SE
squared. The raw-scale offset accounts for the rate-dependent factor
\\p(1-p)\\. With `method = "gam"`,
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) uses `s(log_n)`
in place of `log_n`; the raw-scale offset is retained.

By default, eligible rows from all years enter one model. With
`by = "year"`, the default formula adds a year fixed effect to that
model. To fit a separate model for each year, call this function on each
year's subset.

**Warnings.** In addition to the skipped-fit warning above:

- `sitemix_warning_smoother_multi_year_default`:

  Eligible rows span multiple years and `by = NULL`.

- `sitemix_warning_unexpected_slope`:

  A loglinear fit to `scale = "se"` on arcsine or Anscombe-arcsine rows
  has a finite `log_n` coefficient more than 0.15 away from -1.

- `sitemix_warning_raw_scale_smoothing`:

  The first request for `scale = "se_raw"` after loading the package. A
  custom formula should include `offset(p_offset)` or another
  rate-dependent term.

Invalid logical controls raise `sitemix_error_invalid_smoothing_flag`.

## References

Wood, S. N. (2017). *Generalized Additive Models: An Introduction with
R* (2nd ed.). Chapman and Hall/CRC.

## See also

[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
for estimates and their original standard errors;
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
for checking uncertainty before smoothing;
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) for fitting the
GAM;
[`vignette("a7-variance-smoothing-and-frechet", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/a7-variance-smoothing-and-frechet.md)
for a worked example;
[`vignette("m6-variance-smoothing-theory", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/m6-variance-smoothing-theory.md)
for the model, assumptions, and simulation findings.

## Examples

``` r
data(prek_sim, package = "sitemix")
est <- sm_estimate(
  subset(prek_sim, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)

# Keep the experimental alternative alongside the original SE:
est_s <- sm_smooth_variance(est, method = "loglinear")
head(est_s[, c("site_id", "n", "se", "se_smoothed")], 5)
#> # A tibble: 5 × 4
#>   site_id     n    se se_smoothed
#>   <chr>   <int> <dbl>       <dbl>
#> 1 S001        9 0.167       0.167
#> 2 S002       10 0.158       0.158
#> 3 S003        8 0.177       0.177
#> 4 S004       14 0.134       0.134
#> 5 S005        8 0.177       0.177
```
