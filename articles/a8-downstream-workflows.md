# Using estimates and covariance matrices in further analyses

Abstract

Prepare estimates, standard errors, and covariance matrices for further
analysis. Keep their scales and calculation methods together, identify
rows suitable for inverse-variance weighting, and calculate a contrast
using a matching covariance matrix.

An analysis may need one estimate and standard error per row, or several
estimates with their within-site covariance. This article prepares both
forms from `sitemix_estimates` objects. It also calculates a raw-scale
FRPM-minus-SNAP contrast to show why the estimate and covariance scales
must agree. Fitting a meta-analysis, small-area, or hierarchical model
is a separate step in the chosen analysis software.

The examples use the bundled, fully simulated `prek_sim` data. Their
results illustrate calculations, not findings about real children or
programs. [Getting
started](https://joonho112.github.io/sitemix/articles/a1-getting-started.md)
introduces the initial estimation call.

## Keep estimates with their uncertainty information

The core scalar fields are:

- `site_id`, `year`, and `indicator` for row identity;
- `theta_hat` and `se` for the estimate and standard error;
- `estimate_scale` for their common scale; and
- `var_method` for the standard-error calculation method.

The raw-scale pair is `theta_raw` plus `se_raw`; do not mix one member
of the raw pair with one member of the transformed pair.

Keep an export of all rows before selecting the subset for a model.
Retain `flag_suppressed`, `flag_below_accountability`, and any
`estimate_status` and `sensitivity_*` columns that are present. A
suppressed missing row or a non-identified sensitivity row remains part
of the audit even though it cannot provide an identified estimate with
an ordinary sampling SE. An exact SRSWOR census can have `se = 0`;
retain it as a separate state instead of attempting an infinite
inverse-variance weight.

When `V` is present, it stores a covariance object for each site-year,
repeated on that group’s rows. Read its `vcov_scale`, `estimate_scale`,
and `vcov_method` before using the matrix.

## Prepare a table for one indicator

Estimate FRPM rates for 2024, inspect their uncertainty, and make a
plain data frame. The code keeps every row and adds `iv_eligible` and
`exclusion_reason` to explain which rows could receive ordinary
inverse-variance weights. These two columns are created by the example;
they are not added by
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md).
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) changes
the container without checking whether a model can use the values, so
diagnostics come first:

``` r

est_a <- sm_estimate(
  subset(prek_sim, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)

diag_a <- sm_diagnose(est_a, verbose = FALSE)
object_metadata <- attributes(est_a)[c(
  "description", "family", "sitemix_role", "aggregate_case",
  "sampling_relation", "denominator_pattern", "d1_regime",
  "d1_regime_by_group", "suppression", "smoothing"
)]
audit_columns <- setdiff(names(est_a), c("V", "K"))
scalar_input <- as.data.frame(est_a[, audit_columns, drop = FALSE])

status <- if ("estimate_status" %in% names(scalar_input)) {
  scalar_input$estimate_status
} else {
  rep("identified", nrow(scalar_input))
}
exact_census <- rep(FALSE, nrow(scalar_input))
if ("sampling_fraction" %in% names(scalar_input)) {
  exact_census <-
    is.finite(scalar_input$sampling_fraction) &
    scalar_input$sampling_fraction == 1 & scalar_input$se == 0
}
scalar_input$iv_eligible <-
  status == "identified" & is.finite(scalar_input$theta_hat) &
  is.finite(scalar_input$se) & scalar_input$se > 0
scalar_input$exclusion_reason <- ifelse(
  scalar_input$iv_eligible, "eligible",
  ifelse(
    status != "identified", status,
    ifelse(
      exact_census, "exact_census_not_inverse_weighted",
      ifelse(
        !is.finite(scalar_input$theta_hat) | !is.finite(scalar_input$se),
        "nonfinite_scalar_uncertainty",
        "nonpositive_se_unexplained"
      )
    )
  )
)
head(scalar_input[, c(
  "site_id", "year", "indicator", "theta_hat", "se", "estimate_scale",
  "var_method", "flag_suppressed", "flag_below_accountability",
  "iv_eligible", "exclusion_reason"
)], 5)
#>   site_id year indicator theta_hat        se estimate_scale  var_method
#> 1    S001 2024      frpm 0.3398369 0.1666667        arcsine arcsine_vst
#> 2    S002 2024      frpm 1.1071487 0.1581139        arcsine arcsine_vst
#> 3    S003 2024      frpm 0.7853982 0.1767767        arcsine arcsine_vst
#> 4    S004 2024      frpm 0.7137244 0.1336306        arcsine arcsine_vst
#> 5    S005 2024      frpm 1.2094292 0.1767767        arcsine arcsine_vst
#>   flag_suppressed flag_below_accountability iv_eligible exclusion_reason
#> 1           FALSE                      TRUE        TRUE         eligible
#> 2           FALSE                      TRUE        TRUE         eligible
#> 3           FALSE                      TRUE        TRUE         eligible
#> 4           FALSE                      TRUE        TRUE         eligible
#> 5           FALSE                      TRUE        TRUE         eligible
```

Check the counts before selecting rows:

``` r

table(scalar_input$iv_eligible, scalar_input$exclusion_reason)
#>       
#>        eligible
#>   TRUE       50
```

All 50 rows in this complete example qualify. In other data, retain the
excluded rows and their reasons with the exported results. An exact
census has zero sampling variance under its specified finite-population
design; it is valid but cannot receive a finite weight of `1 / se^2`.

Save `object_metadata` with the table when its input description,
sampling assumptions, or smoothing settings matter. If another package
needs different column names, rename the selected estimate and SE while
retaining their `estimate_scale` and `var_method`. A `sensitivity_var`
is a separately assumed variance scenario; it is not a standard error or
an inverse-variance weight.

## Separate analyses by indicator

For separate single-indicator analyses, first make a scalar-only table
containing all rows, then split it. A `sitemix_estimates` object
containing `V` requires a complete indicator set for each retained
matrix, so dropping individual indicator rows from that object would
fail its checks:

``` r

est_b <- sm_estimate(
  subset(prek_sim, year == 2024),
  family     = "multivariate",
  indicators = c("frpm", "snap"),
  vjt        = TRUE
)

audit_columns_b <- setdiff(names(est_b), c("V", "K"))
scalar_b <- as.data.frame(est_b[, audit_columns_b, drop = FALSE])
scalar_by_indicator <- split(scalar_b, scalar_b$indicator)
names(scalar_by_indicator)
#> [1] "frpm" "snap"
```

Each resulting table retains that indicator’s estimates and SEs. This
split discards cross-indicator covariance, so it is suitable for
separate analyses, not for a contrast or other calculation involving
both indicators.

## Calculate a contrast with within-site covariance

The difference between two rates depends on their covariance. Here we
calculate FRPM minus SNAP within each site, with coefficient vector a =
(1,-1)^\top, and use a^\top V a for its sampling variance.

First inspect the estimates and matrices. Each retained site-year needs
a complete indicator set, identified estimates, a valid `V`, and
standard errors that remain consistent with any recorded smoothing
changes. Dropping a suppressed or sensitivity row while retaining its
full matrix would break that correspondence.

The code checks all three diagnostic levels, then makes one covariance
row per site-year. It matches estimates to `indicator_order` rather than
assuming the data-frame order matches the matrix. Because Scenario B
stores `V` on the raw scale, this contrast uses `theta_raw`:

``` r

diag_b <- sm_diagnose(est_b, verbose = FALSE)
diag_b_row <- sm_diagnose(est_b, level = "row", verbose = FALSE)
diag_b_vcov <- sm_diagnose(est_b, level = "vcov", verbose = FALSE)

status_b <- if ("estimate_status" %in% names(diag_b_row)) {
  diag_b_row$estimate_status
} else {
  rep("identified", nrow(diag_b_row))
}
stopifnot(
  diag_b$v_present,
  diag_b$v_valid,
  all(status_b == "identified"),
  !any(diag_b_row$flag_suppressed),
  !identical(diag_b$smoothing_provenance_valid, FALSE),
  identical(diag_b$v_stale, FALSE),
  all(diag_b_vcov$v_valid),
  all(diag_b_vcov$psd_ok),
  all(diag_b_vcov$repeated_v_equal),
  !any(diag_b_vcov$row_sum_zero_ok %in% FALSE),
  !any(diag_b_vcov$diag_severity == "error")
)

reported_ready <- all(diag_b_vcov$estimate_vcov_scale_compatible)
stopifnot(!reported_ready)
analysis_scale <- "raw"       # explicit analytical choice
contrast <- c(frpm = 1, snap = -1)

estimate_table <- as.data.frame(est_b)
group_keys <- unique(estimate_table[c("site_id", "year")])
covariance_table <- group_keys
covariance_table$indicator_order <- I(vector("list", nrow(group_keys)))
covariance_table$V <- I(vector("list", nrow(group_keys)))
covariance_table$vcov_scale <- rep(NA_character_, nrow(group_keys))
covariance_table$vcov_method <- rep(NA_character_, nrow(group_keys))
covariance_table$diag_contract <- rep(NA_character_, nrow(group_keys))
covariance_table$point_scale <- rep(NA_character_, nrow(group_keys))
covariance_table$contrast_estimate <- rep(NA_real_, nrow(group_keys))
covariance_table$contrast_variance <- rep(NA_real_, nrow(group_keys))

for (g in seq_len(nrow(group_keys))) {
  take <- estimate_table$site_id == group_keys$site_id[g] &
    estimate_table$year == group_keys$year[g]
  block <- estimate_table[take, , drop = FALSE]
  V <- block$V[[1L]]
  stopifnot(all(vapply(block$V, identical, logical(1), V)))
  stopifnot(identical(V$site_id, group_keys$site_id[g]))
  stopifnot(identical(V$year, group_keys$year[g]))
  stopifnot(!anyDuplicated(block$indicator))
  stopifnot(setequal(block$indicator, V$indicator_order))
  coordinate <- match(V$indicator_order, block$indicator)
  stopifnot(!anyNA(coordinate), length(coordinate) == nrow(block))
  aligned <- block[coordinate, , drop = FALSE]
  matrix <- as.matrix(V)       # revalidates the sm_vcov object
  stopifnot(identical(aligned$indicator, V$indicator_order))
  stopifnot(identical(rownames(matrix), V$indicator_order))
  stopifnot(identical(colnames(matrix), V$indicator_order))
  stopifnot(all(block$K == length(V$indicator_order)))

  if (identical(analysis_scale, "raw")) {
    stopifnot(identical(V$vcov_scale, "raw"))
    stopifnot(V$diag_contract %in% c(
      "row_se_raw_squared",
      "row_se_raw_squared_except_boundary_surrogates"
    ))
    point <- aligned$theta_raw
    point_scale <- "raw_probability"
  } else {
    stopifnot(reported_ready)
    point <- aligned$theta_hat
    point_scale <- unique(aligned$estimate_scale)
  }
  weight <- unname(contrast[V$indicator_order])
  stopifnot(!anyNA(weight), all(is.finite(point)))
  contrast_variance <- drop(crossprod(weight, matrix %*% weight))
  stopifnot(is.finite(contrast_variance), contrast_variance >= -1e-14)

  covariance_table$indicator_order[[g]] <- V$indicator_order
  covariance_table$V[[g]] <- V
  covariance_table$vcov_scale[g] <- V$vcov_scale
  covariance_table$vcov_method[g] <- V$vcov_method
  covariance_table$diag_contract[g] <- V$diag_contract
  covariance_table$point_scale[g] <- point_scale
  covariance_table$contrast_estimate[g] <- sum(weight * point)
  covariance_table$contrast_variance[g] <- max(0, contrast_variance)
}

head(covariance_table[c(
  "site_id", "year", "point_scale", "vcov_scale", "vcov_method",
  "diag_contract", "contrast_estimate", "contrast_variance"
)], 5)
#>   site_id year     point_scale vcov_scale vcov_method      diag_contract
#> 1    S001 2024 raw_probability        raw         sur row_se_raw_squared
#> 3    S002 2024 raw_probability        raw         sur row_se_raw_squared
#> 5    S003 2024 raw_probability        raw         sur row_se_raw_squared
#> 7    S004 2024 raw_probability        raw         sur row_se_raw_squared
#> 9    S005 2024 raw_probability        raw         sur row_se_raw_squared
#>   contrast_estimate contrast_variance
#> 1        -0.1111111        0.03566529
#> 3         0.1000000        0.02900000
#> 5         0.3750000        0.02929688
#> 7         0.1428571        0.01895044
#> 9         0.1250000        0.01367188
```

`contrast_estimate` is the FRPM-minus-SNAP difference in proportion
units; multiply it by 100 for percentage points. Its SE is the square
root of `contrast_variance`. This calculation uses the off-diagonal
covariance as well as both marginal variances.

The diagnostic field `estimate_vcov_scale_compatible` is `FALSE` here
because `theta_hat` is arcsine while `V` is raw. That field is a
returned diagnostic, not an R warning. The calculation uses `theta_raw`
with the raw `V` and records `point_scale = "raw_probability"`. Choosing
`analysis_scale = "reported"` would require compatible scales; the
example performs no automatic transformation.

The quadratic form does not invert `V`. It therefore also works with a
valid singular multinomial covariance or a census zero matrix, provided
the point estimates and matrix are on matching scales and the full group
is retained. A procedure requiring `solve(V)` would need to handle
singularity separately.

## Carry Fréchet scenarios separately

[`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
has a different role from the ordinary `V` above. Its
`raw_pairwise_intervals` are formal under the documented D1a conditions:
complete same-unit marginals, a common finite denominator, IID plug-in
variance, `anscombe = FALSE`, and no FPC or `binomial_bc`. The projected
fields `projected_negative_dependence_stress` and
`projected_positive_dependence_stress` are separately labeled PSD stress
scenarios with `projected_scenario_role = "stress_scenario_not_bound"`.

Call [`summary()`](https://rdrr.io/r/base/summary.html) to check an
envelope before extracting its results. Save the population regime and
projection diagnostics with each scenario, including sign changes,
departures from raw pairwise intervals, and reversals in the ordering of
corresponding entries in the scenarios. Repeat the chosen calculation
for each scenario and report the comparison as a sensitivity analysis.
Do not wrap a projected matrix as `sm_vcov`, insert it into ordinary
`V`, average the two scenarios, or report them as multivariate lower and
upper bounds. Even when the `K = 2` projection is an identity, the
formal claim comes from `raw_pairwise_intervals`; D1b remains heuristic.

## Select standard errors and inspect diagnostics

Use these
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
fields to check the values being passed onward:

| Diagnostic fact | Interpretation |
|:---|:---|
| `scalar_uncertainty_finite` | identified estimates and SEs are finite |
| `scalar_se_positive` | identified scalar SEs are strictly positive; FALSE for exact-census zero SE |
| `n_zero_uncertainty_census` | exact SRSWOR census rows are valid but are not inverse-weighted |
| `indicator_scale_consistent` | each indicator uses one estimate scale |
| `v_present` / `v_valid` | covariance matrices are present and structurally valid |
| `estimate_vcov_scale_compatible` | row estimates and covariance use compatible scales |
| `n_suppressed_missing` / `n_suppression_sensitivity` | coordinates that cannot enter an ordinary joint group |
| `smoothing_provenance_valid` / `v_stale` | whether recorded smoothing settings and the unchanged `V` agree |

These checks describe the estimates and uncertainty. A particular model
may impose additional requirements, such as a covariance matrix with an
inverse or a particular estimate scale.

Use `se` with `var_method` for the original reported uncertainty. If a
prespecified sensitivity analysis chooses experimental smoothing, use
`se_smoothed` (or `se_raw_smoothed` for raw proportions) with
`var_method_smoothed`, and keep the original columns and fit
diagnostics. Smoothing does not guarantee better precision or downstream
performance. It also leaves `V` unchanged: a smoothed scalar SE cannot
be substituted for its diagonal without checking the scale and
relationship. See [the smoothing
example](https://joonho112.github.io/sitemix/articles/a7-variance-smoothing-and-frechet.md)
for eligibility, skipped fits, and overwrite restrictions.

## Check the prepared inputs

``` r

stopifnot(inherits(est_a, "sitemix_estimates"))
stopifnot(inherits(est_b, "sitemix_estimates"))
stopifnot(diag_a$scalar_uncertainty_finite)
stopifnot(diag_a$scalar_se_positive)
stopifnot(diag_a$indicator_scale_consistent)
stopifnot(diag_b$v_present)
stopifnot(diag_b$v_valid)
stopifnot(!diag_b$estimate_vcov_scale_compatible)
stopifnot(identical(diag_b$v_stale, FALSE))
stopifnot(nrow(scalar_input) == nrow(est_a))
stopifnot(sum(table(scalar_input$exclusion_reason)) == nrow(est_a))
stopifnot(length(covariance_table$V) == 50L)
stopifnot(all(lengths(covariance_table$indicator_order) == 2L))
stopifnot(all(covariance_table$point_scale == "raw_probability"))
stopifnot(all(covariance_table$contrast_variance >= 0))
```

## Further reading

- [A complete analysis with simulated pre-kindergarten
  data](https://joonho112.github.io/sitemix/articles/a9-case-study-end-to-end.md)
  for a complete simulated analysis from data preparation to export.
- [Understanding returned estimates and
  uncertainty](https://joonho112.github.io/sitemix/articles/m8-output-contract.md)
  for the returned fields and the conditions checked on scalar and
  covariance output.
