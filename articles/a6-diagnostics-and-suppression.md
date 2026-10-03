# Checking estimates and handling suppressed data

Abstract

Check site estimates, standard errors, and covariance matrices with
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md).
Examine suppression and denominator thresholds in published input data
with
[`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md).

We check estimates of free and reduced-price meal participation (FRPM)
at each site in 2024. The `prek_sim` data describe 50 simulated
pre-kindergarten sites; the results illustrate calculations rather than
findings about real children or programs. For the first estimation call,
see [Getting
started](https://joonho112.github.io/sitemix/articles/a1-getting-started.md).

## Check the estimates as a group

First estimate each site’s proportion and standard error:

``` r

est <- sm_estimate(
  subset(prek_sim, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)
```

The default
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
call returns one row describing the entire result. Start with the number
of estimates, their standard errors, and the counts flagged by the
denominator thresholds:

``` r

diag_s <- sm_diagnose(est, verbose = FALSE)
class(diag_s)
#> [1] "sitemix_diagnostics_summary" "tbl_df"                     
#> [3] "tbl"                         "data.frame"
print(as.data.frame(diag_s)[, c(
  "n_cells", "n_flag_small_n", "n_flag_below_accountability",
  "scalar_uncertainty_finite", "scalar_se_positive",
  "indicator_scale_consistent", "diag_severity"
)], row.names = FALSE)
#>  n_cells n_flag_small_n n_flag_below_accountability scalar_uncertainty_finite
#>       50              4                          33                      TRUE
#>  scalar_se_positive indicator_scale_consistent diag_severity
#>                TRUE                       TRUE          note
```

Here `n_cells` is 50: one estimate per site. `scalar_uncertainty_finite`
checks identified estimates and standard errors; intentionally missing
suppression results are counted separately. `indicator_scale_consistent`
checks that each indicator uses one estimate scale. Passing these checks
does not establish that a particular subsequent model is appropriate.

`scalar_se_positive` is `FALSE` for an exact census under simple random
sampling without replacement (SRSWOR), because its sampling standard
error is zero. Such rows receive a `zero_uncertainty_census` note. Keep
them distinguishable from unexplained zero standard errors, and do not
give zero-SE rows ordinary inverse-variance weights.

## Find rows that need attention

Use `level = "row"` to locate the estimates behind the summary counts.
It returns one row per site-year-indicator, retaining the estimates and
adding diagnostic columns:

``` r

diag_r <- sm_diagnose(est, level = "row", verbose = FALSE)
class(diag_r)
#> [1] "sitemix_diagnostics_row" "tbl_df"                 
#> [3] "tbl"                     "data.frame"
head(as.data.frame(diag_r)[, c(
  "site_id", "n", "flag_small_n", "flag_below_accountability",
  "flag_zero_cell", "flag_suppressed", "diag_severity", "diag_notes"
)], 5)
#>   site_id  n flag_small_n flag_below_accountability flag_zero_cell
#> 1    S001  9         TRUE                      TRUE          FALSE
#> 2    S002 10        FALSE                      TRUE          FALSE
#> 3    S003  8         TRUE                      TRUE          FALSE
#> 4    S004 14        FALSE                      TRUE          FALSE
#> 5    S005  8         TRUE                      TRUE          FALSE
#>   flag_suppressed diag_severity                    diag_notes
#> 1           FALSE          note small_n; below_accountability
#> 2           FALSE          note          below_accountability
#> 3           FALSE          note small_n; below_accountability
#> 4           FALSE          note          below_accountability
#> 5           FALSE          note small_n; below_accountability
```

The flags identify different reasons to inspect a row:

| Flag | Meaning |
|----|----|
| `flag_small_n` | The denominator is below the estimation call’s `min_n` (default 10). |
| `flag_below_accountability` | The denominator is below `accountability_n` (default 30). |
| `flag_zero_cell` | The observed numerator is zero or equals the denominator. It is `NA` when suppression hides the numerator. |
| `flag_suppressed` | The published input row was identified as suppressed; individual-student input has no such rows. |

Read `diag_notes` alongside `diag_severity`. The severity uses the
following priority; the first applicable level is reported:

| Severity | Examples of the conditions recorded |
|----|----|
| `error` | Unexplained nonpositive SE, mixed scales within an indicator, inconsistent smoothing information, or a matrix no longer matching same-scale SEs. |
| `warning` | Suppressed-missing or variance-sensitivity rows, an estimate/covariance scale mismatch, or mixed smoothing-to-matrix scale relations. |
| `note` | Small denominators, boundary proportions, reporting thresholds, or an exact SRSWOR census. |
| `ok` | None of the preceding conditions applies. |

These values are returned data. A `diag_severity` value of `"warning"`
does not mean that the call emitted an R warning. Similarly,
`diag_warnings` and `diag_errors` contain diagnostic labels rather than
conditions raised by the call.
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
validates the input first, so an invalid object may stop the call before
any table is returned. `verbose = FALSE` suppresses the printed summary,
not validation errors.

## Check covariance matrices and their scales

When estimates include `V` (`vjt = TRUE`), `level = "vcov"` returns one
row per site-year covariance matrix. For overlapping FRPM and SNAP
indicators, request the matrices during estimation:

``` r

est_b <- sm_estimate(
  subset(prek_sim, year == 2024),
  family     = "multivariate",
  indicators = c("frpm", "snap"),
  vjt        = TRUE
)
diag_v <- sm_diagnose(est_b, level = "vcov", verbose = FALSE)
class(diag_v)
#> [1] "sitemix_diagnostics_vcov" "tbl_df"                  
#> [3] "tbl"                      "data.frame"
head(as.data.frame(diag_v)[, c(
  "site_id", "K", "min_eigenvalue", "psd_tol", "v_valid",
  "estimate_scale", "vcov_scale", "estimate_vcov_scale_compatible",
  "diag_severity"
)], 5)
#>   site_id K min_eigenvalue      psd_tol v_valid estimate_scale vcov_scale
#> 1    S001 2    0.010143277 5.694304e-16    TRUE        arcsine        raw
#> 2    S002 2    0.013783009 6.598666e-16    TRUE        arcsine        raw
#> 3    S003 2    0.010701568 9.725996e-16    TRUE        arcsine        raw
#> 4    S004 2    0.009315201 6.467295e-16    TRUE        arcsine        raw
#> 5    S005 2    0.005859375 8.881784e-16    TRUE        arcsine        raw
#>   estimate_vcov_scale_compatible diag_severity
#> 1                          FALSE       warning
#> 2                          FALSE       warning
#> 3                          FALSE       warning
#> 4                          FALSE       warning
#> 5                          FALSE       warning
```

Here `v_valid` is `TRUE`, but `estimate_vcov_scale_compatible` is
`FALSE`: the default estimates use the arcsine scale, while these
multivariate matrices stay on the raw probability scale. The diagnostic
records this as a warning. Use estimates and a covariance matrix on
compatible scales for a joint analysis; a valid matrix alone is not
enough.

`min_eigenvalue` and `psd_tol` describe the positive-semidefinite check.
A matrix with `min_eigenvalue < -psd_tol` fails input validation before
diagnostics can summarize it. If smoothing has been used, also inspect
`smoothing_v_relation` and `v_stale` to distinguish scale differences
from a matrix that no longer matches the standard errors on its own
scale. See [Understanding the returned
estimates](https://joonho112.github.io/sitemix/articles/m8-output-contract.md)
for the scalar and matrix scale conventions.

## Examine suppression in published input

Before estimating published numerator/denominator pairs, use
[`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
to count suppressed rows and compare observed denominators with a
reporting threshold. This example uses the companion counts from the
simulated data, grouped by subgroup and year:

``` r

counts_path <- system.file(
  "extdata", "prek_sim_counts.rds",
  package = "sitemix", mustWork = TRUE
)
counts <- readRDS(counts_path)
d0 <- counts[, c("site_id", "year", "n_jt", "c_jt_frpm")]
d0$indicator <- "frpm"
d0$c_jt <- d0$c_jt_frpm
d0$subgroup <- "all"
d0 <- d0[c("site_id", "year", "subgroup", "indicator", "c_jt", "n_jt")]

report <- sm_suppression_report(
  d0,
  by              = c("subgroup", "year"),
  numerator_col   = "c_jt",
  denominator_col = "n_jt",
  indicator_col   = "indicator",
  subgroup_col    = "subgroup"
)
print(as.data.frame(report)[, c(
  "subgroup", "year", "n_rows", "n_tier1", "n_tier2", "n_tier3",
  "n_denominator_missing"
)], row.names = FALSE)
#>  subgroup year n_rows n_tier1 n_tier2 n_tier3 n_denominator_missing
#>       all 2021     50       0      32      18                     0
#>       all 2022     50       0      31      19                     0
#>       all 2023     50       0      33      17                     0
#>       all 2024     50       0      33      17                     0
#>       all 2025     50       0      33      17                     0
```

The report’s three tiers use suppression status and `accountability_n`
(30 by default):

| Report column | Rows counted |
|----|----|
| `n_tier1` | Publisher-suppressed rows. |
| `n_tier2` | Rows that are not suppressed and have an observed denominator below the threshold. |
| `n_tier3` | Rows that are not suppressed and have an observed denominator at or above the threshold. |

These are counts of input rows, not children. Meeting the threshold
alone does not establish that an estimate is suitable for publication or
further analysis. Missing denominators are also counted separately.

The report’s `min_n` argument is stored as an attribute; changing it
does not change the tier counts. This differs from
`sm_estimate(min_n = ...)`, which sets `flag_small_n`. It also differs
from `sm_smooth_variance(scope = "tier2")`, which uses the fixed
denominator range 11–29 among otherwise eligible rows, irrespective of
`accountability_n`.

For published data, supply a suppression flag column or use an existing
`suppression_flag` column. A missing numerator with an observed positive
denominator also identifies suppression. A missing denominator alone
does not; a `suppression_when` predicate replaces the usual detection
rules. The report describes these inputs and does not fill in hidden
values.

When Tier 1 rows are present, inspect `sensitivity_role`,
`sensitivity_numeric_variance_available`, and
`sensitivity_requires_acknowledgement` before estimation. The older
`upper_bound_*` columns remain available, but use the `sensitivity_*`
names when reading the report. In the estimator:

- `suppression = "drop"` retains the row with missing estimates and SEs.
- `suppression = "upper_bound"` requires
  `suppression_sensitivity_acknowledge = TRUE`, `vst = "arcsine"`, and
  `anscombe = FALSE` when suppressed rows are present. It leaves
  estimates and SEs missing and records a separate variance-sensitivity
  scenario in `sensitivity_*` fields. With a hidden denominator it
  provides no numeric sensitivity variance.

Suppressed rows cannot enter ordinary `V` matrices or Fréchet
calculations; use `vjt = FALSE` when retaining them. For worked
estimation examples, see [Published
aggregates](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md).

## Check the example results

``` r

stopifnot(inherits(diag_s, "sitemix_diagnostics_summary"))
stopifnot(inherits(diag_r, "sitemix_diagnostics_row"))
stopifnot(inherits(diag_v, "sitemix_diagnostics_vcov"))
stopifnot(nrow(diag_s) == 1L)  # one row summarizing the object
stopifnot(nrow(diag_r) == 50L)  # one row per site-year-indicator
stopifnot(diag_s$scalar_uncertainty_finite == TRUE)
stopifnot(diag_s$scalar_se_positive == TRUE)
stopifnot(diag_s$indicator_scale_consistent == TRUE)
```

## Using the results

Keep reporting flags and suppression status with the estimates when
selecting rows for another analysis. [Downstream
workflows](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md)
shows how to carry the estimates, SEs, and scales together. [Variance
smoothing and
Fréchet](https://joonho112.github.io/sitemix/articles/a7-variance-smoothing-and-frechet.md)
explores optional sensitivity analyses; a high `flag_small_n` count
alone is not a reason to replace the original standard errors. For the
sampling assumptions behind the diagnostics, see [Statistical
foundations](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md).

## References
