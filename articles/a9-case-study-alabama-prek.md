# A9 · Case study — Alabama Pre-K

Abstract

An end-to-end applied case study using the bundled `alprek_subset`. The
case study mirrors a realistic project arc: question → data prep →
site-level estimation → diagnostics → optional experimental GVF
sensitivity → canonical uncertainty export → methods- appendix summary.
Use this vignette as the citable URL when you write up your own sitemix
workflow.

## Overview

### 1. The reporting question

Asha is the accountability lead for a 50-site Alabama Pre-K program. The
question for the legislative report: **what are the site-level FRPM
(free and reduced-price meals) take-up rates in 2024, and which sites
meet the stated publication criteria?**

The audience is the legislature; the deliverable is a table and a
paragraph in a methods appendix.

### 2. What you will be able to do

By the end of this case study you will be able to complete this full
`sitemix` pipeline:

1.  **Data prep** — slice `alprek_subset` to the year of interest.
2.  **Site-level estimation** with
    [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md).
3.  **Diagnostics** with
    [`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md).
4.  **Optional sensitivity** with the experimental GVF helper, without
    replacing canonical SEs.
5.  **Direct uncertainty export** with canonical columns.
6.  **Methods-appendix paragraph** for the legislative report.

**Prerequisites.** [A1 · Getting
started](https://joonho112.github.io/sitemix/articles/a1-getting-started.md)
for the one-call basics; this capstone assembles the full arc.

> **About the example data.** All results here are computed live from
> `alprek_subset`, an anonymized, illustrative 50-site sample of Alabama
> First Class Pre-K records shipped in the package (see
> [`?alprek_subset`](https://joonho112.github.io/sitemix/reference/alprek_subset.md)).
> It is not a real accountability dataset and must not be cited as
> empirical Pre-K results. Every code block runs offline with a fixed
> random seed.

## 3. Data prep

``` r

target_year <- 2024L
dat <- subset(alprek_subset, year == target_year)
nrow(dat)
#> [1] 1510
length(unique(dat$site_id))
#> [1] 50
```

Restricting to 2024 leaves 50 sites in this sample.

## 4. Site-level estimation

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
#> 1 S001       10     0.4       0.685 0.158 FALSE        TRUE                     
#> 2 S002        9     0.222     0.491 0.167 TRUE         TRUE                     
#> 3 S003       11     0.545     0.831 0.151 FALSE        TRUE                     
#> 4 S004        9     0.333     0.615 0.167 TRUE         TRUE                     
#> 5 S005        9     0.556     0.841 0.167 TRUE         TRUE
```

The default `vst = "arcsine"` returns an arcsine-stabilized estimate and
its standard error; `estimate_scale` records that choice.

## 5. Diagnostics

``` r

diag <- sm_diagnose(est, verbose = FALSE)
print(as.data.frame(diag), row.names = FALSE)
#>    family        sitemix_role n_cells n_groups n_sites n_years n_indicators
#>  binomial summary_uncertainty      50       50      50       1            1
#>  n_flag_small_n n_flag_zero_cell n_flag_both n_flag_suppressed
#>               3                0           0                 0
#>  n_flag_below_accountability n_identified n_suppressed_missing
#>                           29           50                    0
#>  n_suppression_sensitivity n_zero_uncertainty_census min_n median_n max_n
#>                          0                         0     9     19.5   125
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
intrinsic facts such as `scalar_uncertainty_finite`,
`scalar_se_positive`, and `indicator_scale_consistent`.

## 6. Optional experimental GVF sensitivity

Small sample size alone is not a trigger to smooth. A prespecified
simulation study found no evidence for promoting either GVF backend to
the default, so the reporting workflow proceeds with canonical `se`. If
a separate analysis prespecifies a GVF sensitivity comparison, create an
append-only candidate under a separate object and column name:

``` r

est_sensitivity <- sm_smooth_variance(
  est,
  method = "loglinear",
  overwrite = FALSE
)
# Keep est_sensitivity$se_smoothed separate from canonical est$se.
```

This optional code is deliberately not part of the default executed path
and makes no general improvement claim.

## 7. Direct uncertainty export

``` r

analysis_input <- data.frame(
  unit_id        = est$site_id,
  year           = est$year,
  indicator      = est$indicator,
  estimate       = est$theta_hat,
  std_error      = est$se,
  estimate_scale = est$estimate_scale,
  var_method     = est$var_method
)
# If a prespecified sensitivity was run, add it under a separate name:
# analysis_input$std_error_gvf_sensitivity <- est_sensitivity$se_smoothed
head(analysis_input, 5)
#>   unit_id year indicator  estimate std_error estimate_scale  var_method
#> 1    S001 2024      frpm 0.6847192 0.1581139        arcsine arcsine_vst
#> 2    S002 2024      frpm 0.4908827 0.1666667        arcsine arcsine_vst
#> 3    S003 2024      frpm 0.8309156 0.1507557        arcsine arcsine_vst
#> 4    S004 2024      frpm 0.6154797 0.1666667        arcsine arcsine_vst
#> 5    S005 2024      frpm 0.8410687 0.1666667        arcsine arcsine_vst
```

The export is an ordinary data frame. Any downstream package can map
`estimate` and `std_error` to its own interface while retaining scale
and variance-method provenance.

## 8. Methods-appendix paragraph (citable template)

> *We computed site-level FRPM rates for the 50 sites in the 2024 cohort
> using the sitemix R package (Lee, 2026). Site-level estimates used the
> arcsine variance-stabilizing transform with the default Wilson
> boundary surrogate (Wilson, 1927). Rows below the accountability
> threshold of `n = 30` were flagged but retained in the uncertainty
> table. We exported canonical unsmoothed standard errors and reported
> the estimate scale and variance method with every estimate.
> Experimental GVF alternatives were not substituted for canonical
> uncertainty.*

## 9. Audit

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

## 10. What’s next?

- [M1 · Statistical
  foundations](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md)
  for the sampling-uncertainty framing behind the case study.
- [M2 · Scalar SE —
  binomial](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
  for the arcsine and boundary-method derivations.
- [M6 · Variance smoothing
  theory](https://joonho112.github.io/sitemix/articles/m6-variance-smoothing-theory.md)
  for the experimental sensitivity model and its NO-GO simulation
  decision.
- [M8 · Output
  contract](https://joonho112.github.io/sitemix/articles/m8-output-contract.md)
  for the package-neutral column and covariance contract.

## 11. Citation

``` r

citation("sitemix")
```

A canonical BibTeX entry lives in `vignettes/references.bib` under the
key `lee_2026_sitemix`.

## References

Lee, J. (2026). *sitemix: Site- and group-level proportions, rates, and
sampling uncertainty*. <https://joonho112.github.io/sitemix/>

Wilson, E. B. (1927). Probable inference, the law of succession, and
statistical inference. *Journal of the American Statistical
Association*, *22*(158), 209–212.
<https://doi.org/10.1080/01621459.1927.10502953>
