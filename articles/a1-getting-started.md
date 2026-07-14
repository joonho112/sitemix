# A1 · Getting started — your first site-level estimate

Abstract

For applied researchers with site-year data who need site-level point
estimates and sampling uncertainty for downstream analysis. This
vignette walks through one
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
call on the bundled Alabama Pre-K sample and the column glossary every
downstream consumer reads. You leave with a 50-site `sitemix_estimates`
tibble, the diagnostic pass that confirms its uncertainty contract, and
pointers to the next vignette.

## Overview

### 1. Why you are here

Asha is the accountability lead at her district. She has one season of
student-row Pre-K enrollment data, and she has been told to produce
site-level FRPM (free and reduced-price meals) rates with standard
errors by Friday so her colleague can use them in a downstream model.
She knows what
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
*should* do; she has not yet typed the first call.

If you opened this vignette, your situation is close enough.

### 2. What you will leave with

By the end of this five-minute walkthrough you will have:

- A 50-site `sitemix_estimates` tibble for FRPM rates in 2024, one row
  per site-year-indicator.
- A short tour of the column glossary every downstream consumer of
  `sitemix_estimates` reads.
- The uncertainty audit
  ([`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md))
  showing that estimates, standard errors, scales, and any covariance
  payload are coherent.
- Pointers to A2 (input formats) and M1 (statistical foundations) for
  when you want to go deeper.

**Prerequisites.** None — start here.

> **About the example data.** All results here are computed live from
> `alprek_subset`, an anonymized, illustrative 50-site sample of Alabama
> First Class Pre-K records shipped in the package (see
> [`?alprek_subset`](https://joonho112.github.io/sitemix/reference/alprek_subset.md)).
> It is not a real accountability dataset and must not be cited as
> empirical Pre-K results. Every code block runs offline with a fixed
> random seed.

## 3. Install and load

If you have not installed sitemix yet:

``` r

# install.packages("pak")
pak::pak("joonho112/sitemix")
```

Then load the package and the bundled sample data:

``` r

library(sitemix)
data(alprek_subset, package = "sitemix")
```

`alprek_subset` carries four overlapping binary indicators (`frpm`,
`snap`, `wic`, `tanf`) observed across years 2021–2025.

## 4. Your first estimate (the five-minute teaser)

The teaser: run
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
on one year of FRPM data and print the result.

``` r

frpm_2024 <- sm_estimate(
  subset(alprek_subset, year == 2024),
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
#>     S001 2024      frpm 10 0.4000000 0.6847192 0.1581139
#>     S002 2024      frpm  9 0.2222222 0.4908827 0.1666667
#>     S003 2024      frpm 11 0.5454545 0.8309156 0.1507557
#>     S004 2024      frpm  9 0.3333333 0.6154797 0.1666667
#>     S005 2024      frpm  9 0.5555556 0.8410687 0.1666667
```

Each row is one site in 2024. `n` is the site denominator (number of
students with non-missing FRPM status); `theta_raw` is the raw
proportion in `[0, 1]`; `theta_hat` is the same estimate on the
arcsine-stabilized scale; `se` is its delta-method standard error on
that scale. Down the page you will see the full column glossary.

## 5. Read the columns

`frpm_2024` is a tibble of class `sitemix_estimates` carrying the
estimates, uncertainty, and provenance a downstream analysis can
consume. The full list:

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

The columns you must know on first contact:

- `site_id`, `year`, `indicator` — the keyed triple identifying each
  row.
- `n` — site denominator.
- `theta_raw` — raw proportion (always in `[0, 1]`).
- `theta_hat`, `se` — point estimate and SE on the `estimate_scale`
  (default `"arcsine"`).
- `var_method` — the row-level SE provenance; see
  [`sm_vcov()`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
  for the implemented lexicon.
- `flag_small_n`, `flag_below_accountability`, `flag_zero_cell`,
  `flag_suppressed` — the canonical row flags. Use these to audit or
  filter rows. A flag alone is not a smoothing trigger; any GVF
  alternative is a prespecified experimental sensitivity that must
  remain separate from canonical SEs.
- `input_mode` — `"student_level"` here; would be `"counts_full_suff"`
  or `"aggregate"` under the wrappers.

The full glossary lives in
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)’s
`@return` section.

## 6. Confirm the uncertainty contract

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

A `sitemix_diagnostics_summary` reports one row summarizing the object,
with denominator percentiles, tier counts, and intrinsic uncertainty
facts. For scalar output, check `scalar_uncertainty_finite`,
`scalar_se_positive`, and `indicator_scale_consistent`. For output with
a `V` list-column, also inspect `v_valid` and
`estimate_vcov_scale_compatible`.

For a row-level audit (every site, every flag), pass `level = "row"`;
for covariance-level (when `vjt = TRUE`), pass `level = "vcov"`. The
full walkthrough is in [A6 · Diagnostics and
suppression](https://joonho112.github.io/sitemix/articles/a6-diagnostics-and-suppression.md).

## 7. Audit before going further

A bottom-of-file [`stopifnot()`](https://rdrr.io/r/base/stopifnot.html)
block makes the workflow assertions explicit. If you fork this vignette
into a methods appendix, keep this chunk:

``` r

stopifnot(nrow(frpm_2024) == 50L)  # 50 sites in 2024
stopifnot(all(frpm_2024$indicator == "frpm"))  # single indicator
stopifnot(all(frpm_2024$estimate_scale == "arcsine"))  # default scale
stopifnot(all(is.finite(frpm_2024$theta_hat)))  # no NaN/Inf
stopifnot(all(frpm_2024$se > 0))  # strictly positive SE
stopifnot("var_method" %in% names(frpm_2024))  # provenance column present
stopifnot(all(frpm_2024$var_method %in% c(
  "arcsine_vst", "arcsine_anscombe", "logit_delta",
  "binomial", "binomial_bc", "wilson_floor", "agresti_coull",
  "suppressed_drop", "suppression_sensitivity"
)))  # implemented base lexicon
```

## 8. What’s next?

You now have a `sitemix_estimates` tibble for one indicator in one year.
The next steps depend on what your data look like:

- If your data are **not student rows** (you have sufficient counts or a
  publisher CSV), read [A2 · Input
  formats](https://joonho112.github.io/sitemix/articles/a2-input-formats.md)
  and pick the matching wrapper.
- If you want the **boundary-method / VST knobs**, read [A3 · Scenario A
  —
  binomial](https://joonho112.github.io/sitemix/articles/a3-scenario-binomial.md).
- If you have **multiple indicators** (e.g., FRPM + SNAP + WIC), read
  [A4 · Scenarios B / C — multivariate /
  multinomial](https://joonho112.github.io/sitemix/articles/a4-multivariate-multinomial.md).
- When you are ready to use the estimates in another analysis, read [A8
  · Downstream
  workflows](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md).
- For the **formal foundations** (the sampling model and the
  delta-method SE derivation), read [M1 · Statistical
  foundations](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md)
  and [M2 · Scalar SE —
  binomial](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md).

## References

## Session info
