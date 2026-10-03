# A complete analysis with simulated pre-kindergarten data

Abstract

Follow a complete analysis of FRPM proportions in simulated
pre-kindergarten data, from selecting a year to checking and exporting
estimates. The example also shows how to describe the calculation and
keep an optional smoothing comparison separate.

This example estimates the proportion of students receiving free and
reduced-price meals (FRPM) at each of 50 simulated sites in 2024. We
prepare a table of rates and sampling uncertainty and flag small
denominators for review. A reporting threshold is a chosen rule in the
example, not a finding about whether a real program may publish its
data.

The bundled `prek_sim` panel contains no real children or programs.
Every result below is a calculation on simulated records, so it should
not be reported as an empirical Pre-K result. See [Getting
started](https://joonho112.github.io/sitemix/articles/a1-getting-started.md)
for the first estimation call on its own.

## Select the year

``` r

target_year <- 2024L
dat <- subset(prek_sim, year == target_year)
nrow(dat)
#> [1] 1544
length(unique(dat$site_id))
#> [1] 50
```

Restricting to 2024 leaves 50 sites in this sample.

## Estimate FRPM proportions

``` r

est <- sm_estimate(
  dat,
  family    = "binomial",
  indicator = "frpm"
)
head(est[, c("site_id", "n", "theta_raw", "theta_hat", "se",
             "flag_small_n", "flag_below_accountability")], 5)
#> # A tibble: 5 × 7
#>   site_id     n theta_raw theta_hat    se flag_small_n flag_below_accountability
#>   <chr>   <int>     <dbl>     <dbl> <dbl> <lgl>        <lgl>                    
#> 1 S001        9     0.111     0.340 0.167 TRUE         TRUE                     
#> 2 S002       10     0.8       1.11  0.158 FALSE        TRUE                     
#> 3 S003        8     0.5       0.785 0.177 TRUE         TRUE                     
#> 4 S004       14     0.429     0.714 0.134 FALSE        TRUE                     
#> 5 S005        8     0.875     1.21  0.177 TRUE         TRUE
```

The default `vst = "arcsine"` returns an arcsine-stabilized estimate and
its standard error; `estimate_scale` records that choice. `theta_raw`
retains the proportion for reporting. Both small-sample and
accountability flags are returned without removing the flagged rows.

## Review the estimates

``` r

diag <- sm_diagnose(est, verbose = FALSE)
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

The summary reports `n_flag_small_n`, `n_flag_below_accountability`, and
checks such as `scalar_uncertainty_finite`, `scalar_se_positive`, and
`indicator_scale_consistent`.

## An optional variance-smoothing comparison

This analysis uses the original `se`. Small sample size alone is not a
reason to replace it with a smoothed value. The optional GVF helper is
experimental, and improved precision or model performance is not
guaranteed. If a separate, prespecified sensitivity analysis calls for
smoothing, create another object and retain the additional SE column:

``` r

est_sensitivity <- sm_smooth_variance(
  est,
  method = "loglinear",
  overwrite = FALSE
)
# Keep est_sensitivity$se_smoothed alongside the original est$se.
```

The optional chunk is marked `eval = FALSE`; the reporting example below
uses the original estimates. See [the variance-smoothing
methods](https://joonho112.github.io/sitemix/articles/m6-variance-smoothing-theory.md)
for the model assumptions and the scope of the package’s simulation
checks.

## Export estimates and retain the review flags

``` r

analysis_input <- data.frame(
  unit_id        = est$site_id,
  year           = est$year,
  indicator      = est$indicator,
  estimate       = est$theta_hat,
  std_error      = est$se,
  estimate_scale = est$estimate_scale,
  var_method     = est$var_method,
  n              = est$n,
  proportion     = est$theta_raw,
  se_raw         = est$se_raw,
  flag_small_n   = est$flag_small_n,
  flag_below_accountability = est$flag_below_accountability,
  flag_zero_cell = est$flag_zero_cell,
  flag_suppressed = est$flag_suppressed
)
# If a prespecified sensitivity was run, add it under a separate name:
# analysis_input$std_error_gvf_sensitivity <- est_sensitivity$se_smoothed
head(analysis_input, 5)
#>   unit_id year indicator  estimate std_error estimate_scale  var_method  n
#> 1    S001 2024      frpm 0.3398369 0.1666667        arcsine arcsine_vst  9
#> 2    S002 2024      frpm 1.1071487 0.1581139        arcsine arcsine_vst 10
#> 3    S003 2024      frpm 0.7853982 0.1767767        arcsine arcsine_vst  8
#> 4    S004 2024      frpm 0.7137244 0.1336306        arcsine arcsine_vst 14
#> 5    S005 2024      frpm 1.2094292 0.1767767        arcsine arcsine_vst  8
#>   proportion    se_raw flag_small_n flag_below_accountability flag_zero_cell
#> 1  0.1111111 0.1047566         TRUE                      TRUE          FALSE
#> 2  0.8000000 0.1264911        FALSE                      TRUE          FALSE
#> 3  0.5000000 0.1767767         TRUE                      TRUE          FALSE
#> 4  0.4285714 0.1322600        FALSE                      TRUE          FALSE
#> 5  0.8750000 0.1169268         TRUE                      TRUE          FALSE
#>   flag_suppressed
#> 1           FALSE
#> 2           FALSE
#> 3           FALSE
#> 4           FALSE
#> 5           FALSE
```

The table retains all sites. For a percentage table, use
`100 * proportion` and, when needed, `100 * se_raw`. For an
arcsine-scale analysis, use `estimate` with `std_error`, keeping
`estimate_scale` and `var_method`. The flags and denominator remain
available to explain later row selection. [Using estimates in further
analyses](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md)
shows additional handling for suppressed rows, exact censuses, and
covariance matrices; this example contains complete student records.

## Describe what was calculated

A methods description should identify the data, estimate scale,
uncertainty calculation, and any row-selection rules. For this simulated
example:

> We calculated FRPM proportions for 50 simulated pre-kindergarten sites
> in 2024 using sitemix (Lee, 2026). We retained raw proportions for
> percentage reporting and arcsine-transformed estimates with their
> delta-method standard errors for further analysis. We used the
> package’s default boundary setting, which supplies a Wilson-based
> variance surrogate if a proportion is 0 or 1 (Wilson, 1927). Sites
> with n \< 30 were flagged but retained in the exported table. The
> table contains the original unsmoothed standard errors, their scale
> and calculation method, and the denominator and review flags. These
> simulated results are not estimates for real children or programs.

## Check the exported table

``` r

stopifnot(nrow(est) == 50L)
stopifnot(all(est$indicator == "frpm"))
stopifnot(diag$scalar_uncertainty_finite == TRUE)
stopifnot(diag$scalar_se_positive == TRUE)
stopifnot(diag$indicator_scale_consistent == TRUE)
stopifnot(all(est$estimate_scale == "arcsine"))
stopifnot(all(is.finite(est$theta_hat)))
stopifnot(nrow(analysis_input) == nrow(est))
stopifnot(identical(analysis_input$std_error, est$se))
stopifnot(!"se_smoothed" %in% names(est))
```

## Methods and related examples

- [Sampling uncertainty in site-level
  proportions](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md)
  for the sampling-uncertainty framing behind the case study.
- [Binomial standard errors and
  transformations](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
  for the arcsine and boundary-method derivations.
- [Experimental models for variance
  smoothing](https://joonho112.github.io/sitemix/articles/m6-variance-smoothing-theory.md)
  for the experimental variance model and its simulation findings.
- [Understanding returned estimates and
  uncertainty](https://joonho112.github.io/sitemix/articles/m8-output-contract.md)
  for the returned columns and covariance-scale requirements.

## Cite the software

``` r

citation("sitemix")
```

The bibliography entry used in this article is `lee_2026_sitemix`. Use
`citation("sitemix")` to obtain citation details for your installed
version.

## References

Lee, J. (2026). *sitemix: Site- and group-level proportions, rates, and
sampling uncertainty*. <https://joonho112.github.io/sitemix/>

Wilson, E. B. (1927). Probable inference, the law of succession, and
statistical inference. *Journal of the American Statistical
Association*, *22*(158), 209–212.
<https://doi.org/10.1080/01621459.1927.10502953>
