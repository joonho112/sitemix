# Smooth standard errors with an experimental GVF model

`sm_smooth_variance()` is an opt-in experimental generalized
variance-function (GVF) helper that fits a cross-row log-variance trend
and records smoothed standard errors **without changing the point
estimates**. By default it appends either `se_smoothed` or
`se_raw_smoothed`, according to `scale`; pass `overwrite = TRUE` to also
replace the selected SE column in place while preserving pre-smoothing
snapshots. The appended value is an experimental sensitivity
alternative, not a generally improved estimate. A fixed-seed simulation
study did not support promoting smoothing to a default: small sample
size alone is not a trigger to smooth, canonical SEs remain primary
under the append-only default, and no downstream performance gain is
guaranteed.

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

  A `sitemix_estimates` object produced by
  [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  or one of its wrappers. Objects containing non-identified
  suppression-sensitivity rows are rejected; smooth identified estimates
  before conducting a separate sensitivity analysis.

- method:

  Character scalar. Smoother to fit. One of `"loglinear"` (default;
  lightweight [`stats::lm()`](https://rdrr.io/r/stats/lm.html) fit) or
  `"gam"` (requires `mgcv` at runtime).

- scale:

  Character scalar. Standard-error column to smooth. One of `"se"`
  (default; the row-level SE) or `"se_raw"` (the raw-scale SE).

- scope:

  Character scalar. Rows eligible for smoothing. One of `"all"`
  (default; every row) or `"tier2"` (\\11 \le n\_{jt} \le 29\\; the
  accountability boundary).

- by:

  Character vector or `NULL` (default `NULL`). Optional column names
  added as fixed-effect factors in one joint smoothing model. When
  eligible rows span multiple years and `by = NULL`, the helper emits
  the multi-year warning described in *Details*.

- formula:

  Formula object or `NULL` (default `NULL`). Optional model formula
  evaluated against helper variables `log_var`, `log_n`, `n`,
  `theta_raw`, and `p_offset`. The model-frame variable `p_offset` is
  `log(p_star * (1 - p_star))`, where `p_star` is the boundary-safe
  probability: `theta_raw` in the interior and the Wilson center for 0/1
  rows. When `NULL`, `scale = "se"` uses `log_var ~ log_n` and
  `scale = "se_raw"` uses `log_var ~ log_n + offset(p_offset)` by
  default.

- bias_correct:

  Logical scalar. If `TRUE` (default), apply Jensen correction when
  back-transforming predicted log variances. Invalid values raise the
  stable invalid-smoothing condition documented in *Details*.

- min_n:

  Positive integer scalar or `NULL` (default `NULL`). Optional minimum
  denominator for rows entering the fit.

- min_rows:

  Positive integer scalar. Minimum eligible rows required to fit.
  Defaults to `50L`.

- overwrite:

  Logical scalar. If `TRUE`, also replace the selected SE column after
  adding its scale-specific smoothed alternative. Overwrite is rejected
  when an existing `V` list-column is on the same scale because
  retaining that matrix would make its diagonal stale. An
  incompatible-scale `V` remains unchanged and that fact is recorded in
  smoothing provenance. Defaults to `FALSE`.

- return_diagnostics:

  Logical scalar. If `TRUE` and a model is fit, add the
  `residual_log_var` column and attach the fitted model as
  `smoother_fit`. The `smoother_fit_summary` and `smoothing` attributes
  are attached regardless of this setting; a skipped fit has neither
  residuals nor a fitted-model attribute. Defaults to `FALSE`.

- ...:

  Additional arguments forwarded to
  [`stats::lm()`](https://rdrr.io/r/stats/lm.html) (when
  `method = "loglinear"`) or
  [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) (when
  `method = "gam"`).

## Value

A `sitemix_estimates` tibble with the same column structure as the input
`x`; see
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
for the canonical column glossary. This function adds or modifies the
following columns:

- `se_smoothed`, `se_raw_smoothed`:

  Numeric; the smoothed standard error on transformed/canonical `se`
  scale or raw `se_raw` scale, respectively. Exactly one is added
  according to `scale`.

- `var_method_smoothed`:

  Character provenance for the scale-specific smoothed alternative. With
  `overwrite = FALSE`, canonical `var_method` remains unchanged.

- `residual_log_var`:

  Numeric; the log-variance fit residual. Added when
  `return_diagnostics = TRUE` and a smoother is fit; omitted when
  smoothing is skipped before model fitting.

- `se_pre_smoothing`, `se_raw_pre_smoothing`:

  Numeric snapshots of the pre-smoothing SE values. Added only when
  `overwrite = TRUE` so the original SE is preserved alongside the
  overwritten column.

When `overwrite = TRUE`, the `se` (or `se_raw`) column is also replaced;
otherwise it is preserved verbatim. The returned object always carries
`smoother_fit_summary` and `smoothing` attributes, including when the
fit is skipped. When `return_diagnostics = TRUE` and a model is fit, it
additionally carries the `smoother_fit` attribute and `residual_log_var`
column; skipped fits carry neither.

## Details

**Method choice.** The default `method = "loglinear"` is a lightweight
[`stats::lm()`](https://rdrr.io/r/stats/lm.html) fit. For the default
`scale = "se"` path it fits \\\log v\_{jt} \sim \log n\_{jt}\\. For
`scale = "se_raw"`, where raw binomial variances also depend on the rate
level, the default adds the boundary-safe offset
\\\mathrm{offset}(\log(p^{\mathrm{off}}\_{jt}(1 -
p^{\mathrm{off}}\_{jt})))\\. The alternative `method = "gam"` fits a
generalized additive model via
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html); the `mgcv`
dependency is runtime-guarded.

This is not a Fay–Herriot area-level estimator: it neither models the
point estimates nor produces small-area posterior means.

**Condition surface.** Three warning classes are emitted under specific
conditions:

- `sitemix_warning_smoother_multi_year_default`:

  When eligible rows span multiple years and `by = NULL`; the helper
  pools years by default but warns. Use `by = "year"` to add a year
  fixed effect.

- `sitemix_warning_unexpected_slope`:

  When the fitted denominator slope deviates from \\-1\\ by more than
  0.15 in loglinear arcsine smoothing.

- `sitemix_warning_raw_scale_smoothing`:

  When smoothing on raw-scale SE; the default raw-scale model includes
  the rate-dependent `p_offset` term, and custom formulas should include
  an analogous rate-dependent term.

`sitemix_error_invalid_smoothing_flag` is raised when a public logical
smoothing control is not `TRUE` or `FALSE`.

**Multi-year handling.** By default the helper pools years into a single
joint model. To add a year fixed effect (still in one joint fit, not
per-year fits), pass `by = "year"`. To obtain per-year smoothing, call
the function once per year on a subset.

## References

Wood, S. N. (2017). *Generalized Additive Models: An Introduction with
R* (2nd ed.). Chapman and Hall/CRC.

## See also

[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
for the upstream producer;
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
for the pre-smoothing audit;
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) for the GAM
smoother backend;
[`vignette("a7-variance-smoothing-and-frechet", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/a7-variance-smoothing-and-frechet.md)
for the applied walkthrough;
[`vignette("m6-variance-smoothing-theory", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/m6-variance-smoothing-theory.md)
for the GVF/log-variance derivation and condition taxonomy.

## Examples

``` r
data(alprek_subset, package = "sitemix")
est <- sm_estimate(
  subset(alprek_subset, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)

# Experimental loglinear sensitivity alternative (lightweight, no mgcv):
est_s <- sm_smooth_variance(est, method = "loglinear")
head(est_s[, c("site_id", "n", "se", "se_smoothed")], 5)
#> # A tibble: 5 × 4
#>   site_id     n    se se_smoothed
#>   <chr>   <int> <dbl>       <dbl>
#> 1 S001       10 0.158       0.158
#> 2 S002        9 0.167       0.167
#> 3 S003       11 0.151       0.151
#> 4 S004        9 0.167       0.167
#> 5 S005        9 0.167       0.167
```
