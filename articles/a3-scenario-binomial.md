# Estimating proportions for a binary indicator

Abstract

Estimate a binary indicator at each site using simulated
pre-kindergarten data. Compare reporting scales, inspect proportions at
0 or 1, and apply sample-size flags without changing the estimates.

This example estimates one binary indicator at each site. We use FRPM in
2024 to compare reporting scales and TANF in 2021 to examine sites with
no observed participants. Both come from `prek_sim`, a fully simulated
panel of 50 pre-kindergarten sites. The results illustrate calculations
and do not describe real children or programs.

For a first introduction to the returned columns, see [Getting started
with site-level
proportions](https://joonho112.github.io/sitemix/articles/a1-getting-started.md).

## Choose the reporting scale

Select the year, set `family = "binomial"`, and name the binary column
in `indicator`. The denominator is the number of students with a
non-missing value for that indicator at each site. `theta_raw` always
retains the observed proportion for these complete student records.

`vst` sets the scale of the pair `theta_hat` and `se`. The following
calls use the same students and differ only in that choice:

``` r

arc <- sm_estimate(subset(prek_sim, year == 2024),
                   family = "binomial", indicator = "frpm",
                   vst = "arcsine")
lgt <- sm_estimate(subset(prek_sim, year == 2024),
                   family = "binomial", indicator = "frpm",
                   vst = "logit")
raw <- sm_estimate(subset(prek_sim, year == 2024),
                   family = "binomial", indicator = "frpm",
                   vst = "none")

scale_comparison <- data.frame(
  site_id = raw$site_id[1],
  n = raw$n[1],
  estimate_scale = c("none", "arcsine", "logit"),
  theta_raw = raw$theta_raw[1],
  theta_hat = c(raw$theta_hat[1], arc$theta_hat[1], lgt$theta_hat[1]),
  se = c(raw$se[1], arc$se[1], lgt$se[1])
)
print(scale_comparison, row.names = FALSE)
#>  site_id n estimate_scale theta_raw  theta_hat        se
#>     S001 9           none 0.1111111  0.1111111 0.1047566
#>     S001 9        arcsine 0.1111111  0.3398369 0.1666667
#>     S001 9          logit 0.1111111 -2.0794415 1.0606602
```

These three rows describe the same site and observed proportion. With
`vst = "none"`, `theta_hat` is the proportion and `se` is on the same
scale. With `"arcsine"`, the estimate is `asin(sqrt(theta_raw))`; with
`"logit"`, it is the log-odds. Keep `theta_hat` with the `se` on its own
scale rather than mixing a raw estimate with a transformed SE. Use
`theta_raw` and `se_raw` when you need the raw-scale pair.

The default arcsine square-root transform stabilizes the first-order
binomial variance. In these calls, without finite-population or bias
correction and with `anscombe = FALSE`, the implemented arcsine SE is
`1 / (2 * sqrt(n))`. This is a delta-method calculation, not an exact
finite-sample variance for the transformed random variable. Logit is a
reporting transform for log-odds and requires an observed proportion
strictly between 0 and 1. The FRPM rows above meet that condition; logit
calls at a boundary are rejected. See [Binomial standard errors and
transformations](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
for the formulas and the additional correction options.

## Inspect proportions at 0 or 1

A boundary proportion occurs when no students, or all students, have the
indicator. TANF is rare in the simulated panel, so the 2021 slice
provides examples with zero observed counts. Use raw-scale output to
compare the three `boundary_method` options:

``` r

wf <- sm_estimate(
  subset(prek_sim, year == 2021),
  family          = "binomial",
  indicator       = "tanf",
  vst             = "none",
  boundary_method = "wilson_floor"
)

ac <- sm_estimate(
  subset(prek_sim, year == 2021),
  family          = "binomial",
  indicator       = "tanf",
  vst             = "none",
  boundary_method = "agresti_coull"
)

unadjusted <- sm_estimate(
  subset(prek_sim, year == 2021),
  family          = "binomial",
  indicator       = "tanf",
  vst             = "none",
  boundary_method = "none"
)
```

The estimates remain the observed proportions. The boundary methods
change the uncertainty assigned to the zero-count rows:

``` r

side_by_side <- data.frame(
  site_id    = wf$site_id,
  n          = wf$n,
  theta_raw  = wf$theta_raw,
  wilson_se  = wf$se,
  agresti_se = ac$se,
  plugin_se  = unadjusted$se
)
print(as.data.frame(head(side_by_side, 6)), row.names = FALSE)
#>  site_id  n  theta_raw  wilson_se agresti_se  plugin_se
#>     S001  8 0.00000000 0.08275855 0.10712654 0.00000000
#>     S002  7 0.00000000 0.09039208 0.11595826 0.00000000
#>     S003  9 0.00000000 0.07631391 0.09952619 0.00000000
#>     S004  8 0.00000000 0.08275855 0.10712654 0.00000000
#>     S005 11 0.09090909 0.08667842 0.08667842 0.08667842
#>     S006 11 0.00000000 0.06603003 0.08712880 0.00000000
```

At 0 or 1, the default `"wilson_floor"` records
`var_method = "wilson_boundary_surrogate"`; `"agresti_coull"` records
`"agresti_coull_boundary_surrogate"`. These are the package’s positive
raw-scale uncertainty surrogates based on the Wilson and Agresti–Coull
constructions (Agresti & Coull, 1998; Wilson, 1927). Both retain
`theta_raw = C/n`; on raw output, `theta_hat` also stays at that
observed value. These substitutes for the plug-in variance do not make
the data less sparse or guarantee accurate small-sample inference.

With `boundary_method = "none"`, the raw plug-in SE is zero at 0 or 1. A
zero from this calculation does not establish that the population
proportion is known. Interior rows use the ordinary binomial calculation
and record `var_method = "binomial"` in all three calls.

With the default arcsine output and no additional corrections, `se`
continues to use `1 / (2 * sqrt(n))` at boundaries, while `se_raw`
records the selected raw-scale boundary calculation. Inspect the scale
and method together when interpreting a standard error.

## Flag small denominators

`min_n` and `accountability_n` describe different sample-size flags. The
default `min_n = 10L` marks `flag_small_n = TRUE` when `n < 10`. The
default `accountability_n = 30L` marks
`flag_below_accountability = TRUE` when `n < 30`. These flags retain the
rows and do not change estimates or standard errors.

Set a reporting threshold from your project’s rules. Here, 50 is an
illustrative threshold:

``` r

strict <- sm_estimate(
  subset(prek_sim, year == 2024),
  family           = "binomial",
  indicator        = "frpm",
  accountability_n = 50L
)
table(strict$flag_below_accountability)
#> 
#> FALSE  TRUE 
#>     7    43
```

Rows below 50 observations are flagged, and all 50 sites are still
present. The package does not decide which rows your report or model
should use. A sample-size flag also differs from publisher suppression:
it does not conceal or replace an observed value. Small denominators
warrant attention to the uncertainty approximation as well as the
project’s reporting threshold.

## If the sample comes from a known finite population

The calls above leave `fpc = NULL`. Supply `fpc` only when a fixed
site-year population of size `N` is sampled by simple random sampling
without replacement. The argument takes the population size, not a
correction multiplier. Every retained group requires `N >= n`; a
non-scalar vector must align with the input rows and be constant within
each site-year.

When `N = n`, the complete finite population has been observed, and the
package returns zero sampling SE. This census result differs from the
zero plug-in SE at a boundary without a declared sampling design.
Finite-population correction preserves the point estimates and `n_eff`;
its variance multiplier depends on the chosen variance rule. See
[Binomial standard errors and
transformations](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
for the plug-in and bias-corrected formulas, and [Using estimates and
covariance matrices in further
analyses](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md)
for handling zero-SE rows in later analyses. A census SE describes
sampling uncertainty under that design; it does not account for
measurement error or other sources of uncertainty.

## Use counts from the same students

The bundled count table was computed from `prek_sim`. Select the same
year and indicator to compare the two input types:

``` r

counts_path <- system.file("extdata", "prek_sim_counts.rds",
                           package = "sitemix", mustWork = TRUE)
counts <- readRDS(counts_path)

snap_rows <- sm_estimate(subset(prek_sim, year == 2024),
                         family = "binomial", indicator = "snap")
snap_cnts <- sm_estimate_from_counts(
  counts[counts$year == 2024,
         c("site_id", "year", "n_jt", "c_jt_snap")],
  family = "binomial", indicator = "snap"
)
stopifnot(all.equal(snap_rows$theta_hat, snap_cnts$theta_hat, tolerance = 1e-10))
stopifnot(all.equal(snap_rows$se, snap_cnts$se, tolerance = 1e-10))
c(
  max_estimate_difference = max(abs(snap_rows$theta_hat - snap_cnts$theta_hat)),
  max_se_difference = max(abs(snap_rows$se - snap_cnts$se))
)
#> max_estimate_difference       max_se_difference 
#>                       0                       0
```

Both differences are zero for this example. The comparison requires
counts from the same retained observations, including the same
missing-value exclusions, with matching indicator order and estimation
options. `input_mode` still distinguishes `"student_level"` from
`"counts_full_suff"`. The tolerance in the checks is a numerical test
for this example, not a guarantee for arbitrary input tables.

## Check these example results

These checks apply to the calls above, which use no FPC. In particular,
positive SEs in the arcsine example are not a rule that excludes valid
census outputs.

``` r

stopifnot(nrow(arc) == 50L)
stopifnot(all(arc$indicator == "frpm"))
stopifnot(all(is.finite(arc$theta_hat)))
stopifnot(all(arc$se > 0))
stopifnot(all(wf$var_method[wf$theta_raw %in% c(0, 1)] ==
                    "wilson_boundary_surrogate"))
stopifnot(all(ac$var_method[ac$theta_raw %in% c(0, 1)] ==
                    "agresti_coull_boundary_surrogate"))
stopifnot(identical(strict$theta_hat, arc$theta_hat))
stopifnot(identical(strict$se, arc$se))
```

For several indicators at each site, continue to [Analyzing overlapping
indicators and mutually exclusive
categories](https://joonho112.github.io/sitemix/articles/a4-multivariate-multinomial.md).
[Checking estimates and handling suppressed
data](https://joonho112.github.io/sitemix/articles/a6-diagnostics-and-suppression.md)
explains how to inspect flagged rows, and [Binomial standard errors and
transformations](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
gives the sampling assumptions and calculations used here.

## References

Agresti, A., & Coull, B. A. (1998). Approximate is better than “exact”
for interval estimation of binomial proportions. *The American
Statistician*, *52*(2), 119–126.
<https://doi.org/10.1080/00031305.1998.10480550>

Wilson, E. B. (1927). Probable inference, the law of succession, and
statistical inference. *Journal of the American Statistical
Association*, *22*(158), 209–212.
<https://doi.org/10.1080/01621459.1927.10502953>
