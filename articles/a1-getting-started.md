# Getting started with site-level proportions

Abstract

Estimate a proportion and its standard error at each site using
simulated pre-kindergarten data. Learn how to read the returned
estimates and check them with
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md).

This example estimates the proportion of students receiving free and
reduced-price meals (FRPM) at each site in 2024. We use
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
to calculate the proportions and standard errors, then
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
to check the results.

The bundled `prek_sim` data describe 50 simulated pre-kindergarten
sites. They contain no real children or programs, so the results
illustrate the calculations rather than empirical Pre-K findings (see
[`?prek_sim`](https://joonho112.github.io/sitemix/reference/prek_sim.md)).

## Install and load

If you have not installed sitemix yet:

``` r

# install.packages("pak")
pak::pak("joonho112/sitemix")
```

Then load the package and the bundled sample data:

``` r

library(sitemix)
data(prek_sim, package = "sitemix")
```

`prek_sim` carries four overlapping binary indicators (`frpm`, `snap`,
`wic`, `tanf`) observed across years 2021–2025.

## Estimate the FRPM proportion

Select one year and one binary indicator. The default transformation is
arcsine; the result also retains each proportion on its original scale.

``` r

frpm_2024 <- sm_estimate(
  subset(prek_sim, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)
print(
  as.data.frame(frpm_2024[
    1:5,
    c("site_id", "year", "indicator", "n", "theta_raw", "theta_hat", "se")
  ]),
  row.names = FALSE
)
#>  site_id year indicator  n theta_raw theta_hat        se
#>     S001 2024      frpm  9 0.1111111 0.3398369 0.1666667
#>     S002 2024      frpm 10 0.8000000 1.1071487 0.1581139
#>     S003 2024      frpm  8 0.5000000 0.7853982 0.1767767
#>     S004 2024      frpm 14 0.4285714 0.7137244 0.1336306
#>     S005 2024      frpm  8 0.8750000 1.2094292 0.1767767
```

Each row is one site in 2024. `n` is the site denominator (number of
students with non-missing FRPM status); `theta_raw` is the raw
proportion in `[0, 1]`; `theta_hat` is the same estimate on the
arcsine-stabilized scale; `se` is its delta-method standard error on
that scale. Use `theta_raw` to report percentages and keep `theta_hat`
with `se` when an analysis uses the transformed estimates.

## Read the result

The result is a `sitemix_estimates` tibble. It includes the estimates,
their standard errors, and information about how they were calculated:

``` r

names(frpm_2024)
#>  [1] "site_id"                   "year"
#>  [3] "indicator"                 "theta_raw"
#>  [5] "theta_hat"                 "se_raw"
#>  [7] "se"                        "n"
#>  [9] "n_eff"                     "estimate_scale"
#> [11] "transform"                 "var_method"
#> [13] "flag_small_n"              "flag_zero_cell"
#> [15] "input_mode"                "flag_suppressed"
#> [17] "framing"                   "flag_below_accountability"
```

Start with these columns:

- `site_id`, `year`, `indicator` — the site, year, and indicator
  identifying each row.
- `n` — site denominator.
- `theta_raw` — observed proportion in `[0, 1]` for these complete
  student records.
- `theta_hat`, `se` — point estimate and SE on the `estimate_scale`
  (default `"arcsine"`).
- `var_method` — how the standard error was calculated; the possible
  method labels are listed in
  [`sm_vcov()`](https://joonho112.github.io/sitemix/reference/sm_vcov.md).
- `flag_small_n`, `flag_below_accountability`, `flag_zero_cell`,
  `flag_suppressed` — flags for sample size, reporting thresholds,
  boundary proportions, and suppressed data. Inspect the flagged rows
  before deciding whether they belong in a particular analysis.
- `input_mode` — `"student_level"` here; would be `"counts_full_suff"`
  or `"aggregate"` under the wrappers.

The Value section of
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
describes all returned columns and their conditions.

## Check estimates and uncertainty

Before passing the tibble to another analysis, run the diagnostics:

``` r

diag <- sm_diagnose(frpm_2024, verbose = FALSE)
class(diag)
#> [1] "sitemix_diagnostics_summary" "tbl_df"
#> [3] "tbl"                         "data.frame"
print(as.data.frame(diag), row.names = FALSE)
#>    family        sitemix_role n_cells n_groups n_sites n_years n_indicators
#>  binomial summary_uncertainty      50       50      50       1            1
#>  n_flag_small_n n_flag_zero_cell n_flag_both n_flag_suppressed
#>               4                0           0                 0
#>  n_flag_below_accountability n_identified n_suppressed_missing
#>                           33           50                    0
#>  n_suppression_sensitivity n_zero_uncertainty_census min_n median_n max_n
#>                          0                         0     8     23.5   103
#>  estimate_scale v_present k_present n_psd_repair_fired
#>         arcsine     FALSE     FALSE                 NA
#>  scalar_uncertainty_finite scalar_se_positive scalar_se_nonpositive_unexplained
#>                       TRUE               TRUE                             FALSE
#>  indicator_scale_consistent v_valid estimate_vcov_scale_compatible
#>                        TRUE      NA                             NA
#>  suppression_sensitivity_present suppression_sensitivity_role
#>                            FALSE                         none
#>  sensitivity_numeric_variance_available sensitivity_acknowledged
#>                                      NA                       NA
#>  smoothing_present smoothing_provenance_valid smoothing_v_relation v_stale
#>              FALSE                         NA                 <NA>   FALSE
#>  diag_severity                    diag_notes n_var_method_arcsine_vst
#>           note small_n; below_accountability                       50
```

A `sitemix_diagnostics_summary` reports one row summarizing the object,
including denominator percentiles, tier counts, and checks on the
estimates and standard errors. For scalar output, check
`scalar_uncertainty_finite`, `scalar_se_positive`, and
`indicator_scale_consistent`. For output with a `V` list-column, also
inspect `v_valid` and `estimate_vcov_scale_compatible`.

Use `level = "row"` to locate individual flags. If you request
covariance matrices with `vjt = TRUE`, `level = "vcov"` returns one
diagnostic row per site-year matrix. The full walkthrough is in
[Checking estimates and handling suppressed
data](https://joonho112.github.io/sitemix/articles/a6-diagnostics-and-suppression.md).

## Check the example results

These checks confirm the expected rows and default scale for this
simulated example. Their positive-SE condition is specific to these
data; an exact census under a finite-population design can have zero
sampling SE.

``` r

stopifnot(nrow(frpm_2024) == 50L)  # 50 sites in 2024
stopifnot(all(frpm_2024$indicator == "frpm"))  # single indicator
stopifnot(all(frpm_2024$estimate_scale == "arcsine"))  # default scale
stopifnot(all(is.finite(frpm_2024$theta_hat)))  # no NaN/Inf
stopifnot(all(frpm_2024$se > 0))  # strictly positive SE
stopifnot("var_method" %in% names(frpm_2024))  # calculation method recorded
stopifnot(all(frpm_2024$var_method == "arcsine_vst"))  # this interior example
```

## Continue with your own data

You now have a `sitemix_estimates` tibble for one indicator in one year.
The next steps depend on what your data look like:

- If your data are **not student rows** (you have sufficient counts or a
  publisher CSV), read [Choosing an input
  format](https://joonho112.github.io/sitemix/articles/a2-input-formats.md)
  and pick the matching wrapper.
- For transformations and proportions at 0 or 1, read [Estimating
  proportions for a binary
  indicator](https://joonho112.github.io/sitemix/articles/a3-scenario-binomial.md).
- If you have **multiple indicators** (e.g., FRPM + SNAP + WIC), read
  [Analyzing overlapping indicators and mutually exclusive
  categories](https://joonho112.github.io/sitemix/articles/a4-multivariate-multinomial.md).
- When you are ready to use the estimates in another analysis, read
  [Using estimates and covariance matrices in further
  analyses](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md).
- For the **formal foundations** (the sampling model and the
  delta-method SE derivation), read [Sampling uncertainty in site-level
  proportions](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md)
  and [Binomial standard errors and
  transformations](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md).

## Session info
