# Diagnose uncertainty in a sitemix_estimates tibble

`sm_diagnose()` is the canonical uncertainty audit for a
`sitemix_estimates` tibble. It returns one of three S3-classed tibbles
depending on `level`: an object-level summary (`"summary"`), a row-level
audit with all canonical flags (`"row"`), or a covariance-level audit
reporting PSD and covariance-contract facts (`"vcov"`).

## Usage

``` r
sm_diagnose(x, level = c("summary", "row", "vcov"), verbose = TRUE)
```

## Arguments

- x:

  A `sitemix_estimates` tibble produced by
  [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  or one of its wrappers.

- level:

  Character scalar. Output granularity. One of `"summary"` (default),
  `"row"`, or `"vcov"`. See *Details* for the per-level return class.

- verbose:

  Logical scalar. If `TRUE` (default), print a compact CLI summary
  alongside the returned tibble.

## Value

One of three S3-classed tibbles depending on `level`; see *Details*. The
summary variant has one object-level row and adds one integer
`n_var_method_<label>` column for each observed `var_method`; this
dynamic column set therefore depends on the diagnosed object. The row
and vcov variants have row-per-unit structures suitable for filtering /
dplyr operations.

## Details

The three diagnostic levels return distinct S3 classes:

- `level = "summary"`:

  Returns a `sitemix_diagnostics_summary` tibble: one row summarizing
  the object, including denominator percentiles, tier counts, and
  intrinsic scalar/covariance validity facts. The cheapest level;
  suitable for an interactive sanity check.

- `level = "row"`:

  Returns a `sitemix_diagnostics_row` tibble: one row per
  site-year-indicator with every canonical flag described below and the
  row-level severity tier. Use before any audit that needs per-row
  provenance.

- `level = "vcov"`:

  Returns a `sitemix_diagnostics_vcov` tibble: one row per
  site-year-indicator block of the `V` list-column with PSD margin
  (smallest eigenvalue), scale compatibility, smoothing relation, and
  stale-matrix facts. Only meaningful when `x` was produced with
  `vjt = TRUE`.

Row diagnostics include these canonical flags:

- `flag_small_n` and `flag_below_accountability`.

- `flag_zero_cell` and `flag_suppressed`.

Summary diagnostics describe properties of the estimate object itself:
finite scalar uncertainty, positive standard errors, indicator-level
scale consistency, covariance validity, and estimate/covariance scale
compatibility. They do not inspect or assume a downstream consumer.

Diagnostic severity follows an intrinsic four-level matrix. `"error"`
records invalid scalar uncertainty, mixed scales within an indicator,
invalid smoothing provenance, or a stale matching-scale covariance.
`"warning"` records unavailable suppression rows, non-identified
variance sensitivity, or an explicit estimate/covariance scale mismatch.
`"note"` records descriptive facts such as small denominators, boundary
cells, and accountability thresholds. `"ok"` means none of those facts
applies. Severity priority is error, warning, note, then ok. A
diagnostic reports these facts after ordinary object validation; it does
not replace `validate.sitemix_estimates()`.

The summary field matrix is grouped as follows:

- Scalar facts:

  `scalar_uncertainty_finite`, `scalar_se_positive`, and
  `indicator_scale_consistent`. An exact zero-uncertainty SRSWOR census
  is reported separately and is a note, not an error.

- Covariance facts:

  `v_present`, `v_valid`, and `estimate_vcov_scale_compatible`.

- Suppression facts:

  Counts for identified, suppressed-missing, and sensitivity rows, plus
  role provenance, numeric-variance availability, and acknowledgement.

- Smoothing facts:

  `smoothing_present`, provenance validity, `smoothing_v_relation`, and
  `v_stale`.

- Classification:

  `diag_severity` and semicolon-delimited `diag_notes`.

The suppression role-provenance field is `suppression_sensitivity_role`.

## See also

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  for the upstream producer and canonical column glossary.

- [`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
  for publisher-side suppression auditing alongside `sm_diagnose()`.

- [`vignette("a6-diagnostics-and-suppression")`](https://joonho112.github.io/sitemix/articles/a6-diagnostics-and-suppression.md)
  for the applied walkthrough of all three levels.

Other audit:
[`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)

## Examples

``` r
data(alprek_subset, package = "sitemix")
est <- sm_estimate(
  subset(alprek_subset, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)

# Summary diagnostics (the default; cheapest):
diag_s <- sm_diagnose(est, verbose = FALSE)
class(diag_s)
#> [1] "sitemix_diagnostics_summary" "tbl_df"                     
#> [3] "tbl"                         "data.frame"                 

# Row-level diagnostics for a full audit:
diag_r <- sm_diagnose(est, level = "row", verbose = FALSE)
head(diag_r, 5)
#> sitemix_diagnostics_row: 5 rows | ok=0 note=5 warning=0 error=0
#> # A tibble: 5 × 34
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 frpm          0.4       0.685  0.155 0.158    10    10
#> 2 S002     2024 frpm          0.222     0.491  0.139 0.167     9     9
#> 3 S003     2024 frpm          0.545     0.831  0.150 0.151    11    11
#> 4 S004     2024 frpm          0.333     0.615  0.157 0.167     9     9
#> 5 S005     2024 frpm          0.556     0.841  0.166 0.167     9     9
#> # ℹ 25 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>,
#> #   scalar_uncertainty_finite <lgl>, scalar_se_positive <lgl>,
#> #   scalar_se_nonpositive_unexplained <lgl>, zero_uncertainty_census <lgl>,
#> #   v_present <lgl>, v_valid <lgl>, estimate_vcov_scale_compatible <lgl>,
#> #   suppression_sensitivity_role <chr>, …
```
