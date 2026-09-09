# Diagnose uncertainty in a sitemix_estimates tibble

`sm_diagnose()` checks estimates, standard errors, and covariance
matrices in a `sitemix_estimates` tibble. Use `level` to return a
summary of the object (`"summary"`), diagnostics for each row (`"row"`),
or checks on the covariance matrices (`"vcov"`).

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

  A single string choosing the result: one of `"summary"` (default),
  `"row"`, or `"vcov"`. See *Details* for the rows and class returned at
  each level.

- verbose:

  A single logical value. If `TRUE` (default), print a short summary as
  well as returning the tibble.

## Value

A tibble with the class and row unit described in *Details*. Summary
output has one row and includes one integer `n_var_method_<label>`
column for each observed `var_method`. The method-count columns
therefore depend on the input object. Row output has one row per
estimate; vcov output has one row per site-year covariance matrix.

## Details

The result is a tibble whose rows and class depend on `level`:

- `level = "summary"`:

  Returns a `sitemix_diagnostics_summary` tibble: one row summarizing
  denominators, reporting thresholds, standard errors, and covariance
  matrices. Use this level for an initial check.

- `level = "row"`:

  Returns a `sitemix_diagnostics_row` tibble: one row per
  site-year-indicator, with the flags described below and a severity
  level. Use it to find rows that need closer inspection.

- `level = "vcov"`:

  Returns a `sitemix_diagnostics_vcov` tibble: one row per site-year
  covariance matrix stored in the `V` list-column. It reports the
  smallest eigenvalue, the estimate and covariance scales, and whether
  the matrix still matches any smoothed standard errors. This level is
  useful when `x` was produced with `vjt = TRUE`.

Row diagnostics include these flags:

- `flag_small_n` and `flag_below_accountability`.

- `flag_zero_cell` and `flag_suppressed`.

Summary diagnostics check whether scalar uncertainty is finite, standard
errors are positive, scales agree within each indicator, and covariance
matrices are valid and on compatible scales. These checks describe the
results; they do not establish that a particular subsequent model is
appropriate.

The severity is `"error"`, `"warning"`, `"note"`, or `"ok"`, in that
order of priority. `"error"` indicates invalid scalar uncertainty, mixed
scales within an indicator, inconsistent smoothing information, or a
covariance matrix that no longer matches standard errors on the same
scale. `"warning"` records unavailable suppression rows, non-identified
variance sensitivity, or an explicit estimate/covariance scale mismatch.
`"note"` records descriptive facts such as small denominators, boundary
cells, and accountability thresholds. `"ok"` means none of these
conditions applies. A diagnostic reports these facts after ordinary
object validation; it does not replace `validate.sitemix_estimates()`.

The summary includes:

- Scalar uncertainty:

  `scalar_uncertainty_finite`, `scalar_se_positive`, and
  `indicator_scale_consistent`. An exact zero-uncertainty SRSWOR census
  is reported separately and is a note, not an error.

- Covariance:

  `v_present`, `v_valid`, and `estimate_vcov_scale_compatible`.

- Suppression:

  Counts for identified, suppressed-missing, and sensitivity rows,
  whether numeric sensitivity variances are available, and whether that
  analysis was acknowledged.

- Smoothing:

  `smoothing_present`, consistency of the stored settings,
  `smoothing_v_relation`, and `v_stale`.

- Classification:

  `diag_severity` and semicolon-delimited `diag_notes`.

The field describing the role of suppressed-row sensitivity results is
`suppression_sensitivity_role`.

## See also

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  for producing estimates and descriptions of the estimate columns.

- [`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
  for examining suppression in published input data.

- [`vignette("a6-diagnostics-and-suppression")`](https://joonho112.github.io/sitemix/articles/a6-diagnostics-and-suppression.md)
  for the applied walkthrough of all three levels.

Other audit:
[`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)

## Examples

``` r
data(prek_sim, package = "sitemix")
est <- sm_estimate(
  subset(prek_sim, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)

# Start with a summary of all estimates:
diag_s <- sm_diagnose(est, verbose = FALSE)
class(diag_s)
#> [1] "sitemix_diagnostics_summary" "tbl_df"
#> [3] "tbl"                         "data.frame"

# Inspect the diagnostics for individual estimates:
diag_r <- sm_diagnose(est, level = "row", verbose = FALSE)
head(diag_r, 5)
#> sitemix_diagnostics_row: 5 rows | ok=0 note=5 warning=0 error=0
#> # A tibble: 5 × 34
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 frpm          0.111     0.340  0.105 0.167     9     9
#> 2 S002     2024 frpm          0.8       1.11   0.126 0.158    10    10
#> 3 S003     2024 frpm          0.5       0.785  0.177 0.177     8     8
#> 4 S004     2024 frpm          0.429     0.714  0.132 0.134    14    14
#> 5 S005     2024 frpm          0.875     1.21   0.117 0.177     8     8
#> # ℹ 25 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>,
#> #   scalar_uncertainty_finite <lgl>, scalar_se_positive <lgl>,
#> #   scalar_se_nonpositive_unexplained <lgl>, zero_uncertainty_census <lgl>,
#> #   v_present <lgl>, v_valid <lgl>, estimate_vcov_scale_compatible <lgl>,
#> #   suppression_sensitivity_role <chr>, …
```
