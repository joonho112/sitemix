# A6 · Diagnostics and suppression

Abstract

For applied researchers who have a `sitemix_estimates` tibble and need
to audit its uncertainty before downstream use. This vignette walks
through all three
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
levels (summary, row, vcov) and the three-tier
[`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
framework.

## Overview

### 1. Why you are here

Asha (from A1) and Dana (from A5) both arrive here with the same
question: “My tibble exists; how do I confirm its estimates and
uncertainty are coherent?” The answer is the same two-function audit:
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
(post-estimation row/vcov audit) and
[`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
(publisher-side audit applicable to D0 / D1 inputs).

### 2. What you will leave with

By the end you will have:

- Comfort with all three
  [`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
  levels.
- A reading of the four canonical flag columns (`flag_small_n`,
  `flag_below_accountability`, `flag_zero_cell`, `flag_suppressed`).
- A three-tier suppression report and a red-flags checklist you can
  paste into a methods appendix.

**Prerequisites.** [A1 · Getting
started](https://joonho112.github.io/sitemix/articles/a1-getting-started.md).

> **About the example data.** All results here are computed live from
> `alprek_subset`, an anonymized, illustrative 50-site sample of Alabama
> First Class Pre-K records shipped in the package (see
> [`?alprek_subset`](https://joonho112.github.io/sitemix/reference/alprek_subset.md)).
> The diagnostics run on a `sitemix_estimates` tibble produced from that
> sample, and the suppression section uses its companion
> `alprek_subset_counts` count file. It is not a real accountability
> dataset and must not be cited as empirical Pre-K results. Every code
> block runs offline with a fixed random seed.

## 3. Set up an estimates tibble

``` r

est <- sm_estimate(
  subset(alprek_subset, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)
```

## 4. `sm_diagnose(level = "summary")` — one row per object

The default `level = "summary"` returns one row summarizing the object,
with denominator percentiles, tier counts, and intrinsic uncertainty
facts.

``` r

diag_s <- sm_diagnose(est, verbose = FALSE)
class(diag_s)
#> [1] "sitemix_diagnostics_summary" "tbl_df"                     
#> [3] "tbl"                         "data.frame"
print(as.data.frame(diag_s), row.names = FALSE)
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

Key columns:

- `n_cells` — total rows.
- `n_flag_small_n` — rows below `min_n`.
- `n_flag_below_accountability` — rows below `accountability_n`.
- `scalar_uncertainty_finite` — retained estimates and SEs are finite.
- `scalar_se_positive` — retained scalar SEs are strictly positive. An
  exact SRSWOR census has zero uncertainty by design; it sets this fact
  to `FALSE` but is classified as the note `zero_uncertainty_census`.
- `indicator_scale_consistent` — each indicator uses one estimate scale.
- `v_valid` and `estimate_vcov_scale_compatible` — validity and scale
  compatibility when a `V` list-column is present.
- `suppression_sensitivity_role` — either `none` or the explicit
  `nonidentified_variance_sensitivity` role.
- `smoothing_v_relation` and `v_stale` — whether an experimental scalar
  smoothing result is absent, matching, incompatible, or mixed relative
  to `V`, and whether a matching-scale matrix was left stale.

The same severity priority is used at every level:

| Severity | Intrinsic meaning | Representative facts |
|----|----|----|
| `error` | Invalid or internally incoherent uncertainty | unexplained nonpositive SE, mixed scale within one indicator, invalid smoothing provenance, stale matching-scale `V` |
| `warning` | Valid object that needs an explicit analytical decision | suppressed-missing row, non-identified variance sensitivity, estimate/`V` scale mismatch, mixed smoothing-to-`V` relation |
| `note` | Descriptive fact, not invalidity | small denominator, zero cell, accountability threshold, exact SRSWOR census |
| `ok` | None of the preceding facts applies | coherent identified uncertainty without flagged context |

[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
calls ordinary `sitemix_estimates` validation first. It summarizes a
validated object; it is not a substitute for validation.

## 5. `sm_diagnose(level = "row")` — one row per site-year-indicator

Use the row level when a reviewer asks “show me every flag for every
site”:

``` r

diag_r <- sm_diagnose(est, level = "row", verbose = FALSE)
class(diag_r)
#> [1] "sitemix_diagnostics_row" "tbl_df"                 
#> [3] "tbl"                     "data.frame"
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

The row-level diagnostic includes the severity tier and every flag
column for every row.

## 6. `sm_diagnose(level = "vcov")` — covariance audit (Scenarios B/C/D1)

When the tibble was produced with `vjt = TRUE`, the `vcov` level reports
the minimum eigenvalue, its scale-aware PSD tolerance, matrix validity,
estimate/`V` scale compatibility, and smoothing/stale-`V` facts per
site-year:

``` r

est_b <- sm_estimate(
  subset(alprek_subset, year == 2024),
  family     = "multivariate",
  indicators = c("frpm", "snap"),
  vjt        = TRUE
)
diag_v <- sm_diagnose(est_b, level = "vcov", verbose = FALSE)
class(diag_v)
#> [1] "sitemix_diagnostics_vcov" "tbl_df"                  
#> [3] "tbl"                      "data.frame"
head(diag_v, 5)
#> sitemix_diagnostics_vcov: 5 matrices | PSD ok=5/5 | PSD repairs=0 | scale=raw | note=0 | warning=5 error=0
#> # A tibble: 5 × 29
#>   site_id  year family     K indicator_order matrix_rank min_eigenvalue  psd_tol
#>   <chr>   <int> <chr>  <int> <list>                <int>          <dbl>    <dbl>
#> 1 S001     2024 multi…     2 <chr [2]>                 2        0.00800 1.14e-15
#> 2 S002     2024 multi…     2 <chr [2]>                 2        0.0170  7.64e-16
#> 3 S003     2024 multi…     2 <chr [2]>                 2        0.00376 1.17e-15
#> 4 S004     2024 multi…     2 <chr [2]>                 2        0.00544 1.33e-15
#> 5 S005     2024 multi…     2 <chr [2]>                 2        0.0247  8.58e-16
#> # ℹ 21 more variables: psd_ok <lgl>, v_valid <lgl>, psd_repair <chr>,
#> #   vcov_method <chr>, vcov_scale <chr>, estimate_scale <chr>,
#> #   matrix_boundary_rule <chr>, scalar_correction_rule <list>,
#> #   positive_support <int>, n_jt <int>, n_eff <dbl>, simplex_residual <dbl>,
#> #   row_sum_zero_ok <lgl>, repeated_v_equal <lgl>,
#> #   zero_uncertainty_census <lgl>, estimate_vcov_scale_compatible <lgl>,
#> #   smoothing_provenance_valid <lgl>, smoothing_v_relation <chr>, …
```

If any row has `min_eigenvalue < -psd_tol`, the matrix is not PSD and
ordinary object validation fails before diagnostics can summarize it.
For a validated audit, require `v_valid == TRUE`; then interpret
`estimate_vcov_scale_compatible`, `smoothing_v_relation`, and `v_stale`
separately.

## 7. Suppression report (D0 / D1 only)

[`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
audits the publisher’s three-tier denominator regime before estimation.
Build a D0 slice with a subgroup column:

``` r

counts_path <- system.file(
  "extdata", "alprek_subset_counts.rds",
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
print(as.data.frame(report), row.names = FALSE)
#>  subgroup year n_rows n_tier1 n_tier2 n_tier3 n_suppressed_hidden_denominator
#>       all 2021     50       0      31      19                               0
#>       all 2022     50       0      31      19                               0
#>       all 2023     50       0      29      21                               0
#>       all 2024     50       0      29      21                               0
#>       all 2025     50       0      30      20                               0
#>  n_denominator_missing pct_suppressed pct_below_accountability
#>                      0              0                     0.62
#>                      0              0                     0.62
#>                      0              0                     0.58
#>                      0              0                     0.58
#>                      0              0                     0.60
#>  median_n_suppressed denominator_observed_on_suppressed suppression_sources
#>                   NA                               TRUE                    
#>                   NA                               TRUE                    
#>                   NA                               TRUE                    
#>                   NA                               TRUE                    
#>                   NA                               TRUE                    
#>       recommended_action sensitivity_role
#>  no_suppression_detected             none
#>  no_suppression_detected             none
#>  no_suppression_detected             none
#>  no_suppression_detected             none
#>  no_suppression_detected             none
#>  sensitivity_numeric_variance_available sensitivity_requires_acknowledgement
#>                                   FALSE                                FALSE
#>                                   FALSE                                FALSE
#>                                   FALSE                                FALSE
#>                                   FALSE                                FALSE
#>                                   FALSE                                FALSE
#>  upper_bound_role upper_bound_numeric_variance_available
#>    not_applicable                                  FALSE
#>    not_applicable                                  FALSE
#>    not_applicable                                  FALSE
#>    not_applicable                                  FALSE
#>    not_applicable                                  FALSE
#>  upper_bound_requires_acknowledgement
#>                                 FALSE
#>                                 FALSE
#>                                 FALSE
#>                                 FALSE
#>                                 FALSE
```

Three-tier interpretation:

- **Tier 1** — publisher-suppressed. `n_tier1` rows.
- **Tier 2** — observed below `accountability_n`. Estimable but not
  publishable.
- **Tier 3** — observed and meets threshold. The publishable population.

The canonical `sensitivity_role`,
`sensitivity_numeric_variance_available`, and
`sensitivity_requires_acknowledgement` fields make the post-audit
decision explicit. The `upper_bound_*` names remain legacy compatibility
fields. `suppression = "drop"` retains a canonical missing audit row.
The legacy `"upper_bound"` option is an acknowledged, non-identified
variance sensitivity stored only in `sensitivity_*` fields. It cannot be
used to construct ordinary `V` or a formal Fréchet input; a hidden
denominator yields no numeric variance claim.

## 8. Red-flags checklist (paste into methods appendix)

Use the following short checklist when reviewing a tibble before
publishing:

1.  `scalar_uncertainty_finite == TRUE` and
    `indicator_scale_consistent == TRUE`; require
    `scalar_se_positive == TRUE` except for a documented
    `zero_uncertainty_census` note.
2.  `n_flag_zero_cell == 0` (or boundary-method handling documented).
3.  `n_flag_below_accountability` reported and either small or
    explicitly accepted.
4.  For Scenarios B/C/D1: `v_valid == TRUE`,
    `min_eigenvalue >= -psd_tol`, and `v_stale == FALSE` on every
    matrix.
5.  Record any estimate/`V` scale mismatch as an intentional downstream
    scale decision; do not call it consumer readiness.
6.  For D0/D1: report Tier 1 alongside Tier 3 and label any sensitivity
    as non-identified.

## 9. Audit

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

## 10. What’s next?

- [A7 · Variance smoothing and
  Fréchet](https://joonho112.github.io/sitemix/articles/a7-variance-smoothing-and-frechet.md)
  to inspect an opt-in experimental GVF sensitivity alternative; a high
  `flag_small_n` count alone is not a reason to replace canonical SEs.
- [A8 · Downstream
  workflows](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md)
  when the audit passes.
- [M1 · Statistical
  foundations](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md)
  for the formal `sitemix_estimates` schema invariants the diagnostics
  check.

## References
