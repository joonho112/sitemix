# A8 · Downstream workflows

Abstract

For applied researchers who have a `sitemix_estimates` tibble and need
to use its estimates and sampling uncertainty in another analysis. This
vignette shows package-neutral scalar, split, and covariance workflows
for reporting, meta-analysis, small-area estimation, and hierarchical
models, built from `theta_hat`, `se`, `V`, and their provenance columns.

## Overview

### 1. Why you are here

Marco has finished estimating site-level rates. One collaborator wants a
flat table with one estimate and standard error per row; another wants
the joint covariance across indicators. Both can use sitemix outputs
directly. No consumer-specific object is required.

**What you will leave with.** By the end of this article you will be
able to:

1.  Export the package-neutral scalar, split, and joint-covariance
    contract from a `sitemix_estimates` tibble.
2.  Reconcile inverse-variance eligibility (`iv_eligible` and
    `exclusion_reason`) before filtering rows.
3.  Treat
    [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
    output as a separate stress scenario, never as an ordinary `V`.

**Prerequisites.** [A1 · Getting
started](https://joonho112.github.io/sitemix/articles/a1-getting-started.md).

> **About the example data.** All results here are computed live from
> `alprek_subset`, an anonymized, illustrative 50-site sample of Alabama
> First Class Pre-K records shipped in the package (see
> [`?alprek_subset`](https://joonho112.github.io/sitemix/reference/alprek_subset.md)).
> The workflows run on `sitemix_estimates` tibbles produced from that
> sample. It is not a real accountability dataset and must not be cited
> as empirical Pre-K results. Every code block runs offline with a fixed
> random seed.

## 2. The portable output contract

The core scalar fields are:

- `site_id`, `year`, and `indicator` for row identity;
- `theta_hat` and `se` for the estimate and standard error;
- `estimate_scale` for their common scale; and
- `var_method` for uncertainty provenance.

The raw-scale pair is `theta_raw` plus `se_raw`; do not mix one member
of the raw pair with one member of the transformed pair.

Portable exchange is an all-row audit table, not a pre-filtered model
matrix. Retain `flag_suppressed`, `flag_below_accountability`, and any
`estimate_status` and `sensitivity_*` columns that are present. A
suppressed missing row or a non-identified sensitivity row remains part
of the audit even though it is not ordinary scalar uncertainty. An exact
SRSWOR census can have `se = 0`; retain it as a separate state instead
of attempting an infinite inverse-variance weight.

When `V` is present, each `sm_vcov` matrix adds joint uncertainty. Read
its `vcov_scale`, `estimate_scale`, and `vcov_method` metadata before
using it.

## 3. Scalar output (Scenarios A / D0)

Estimate one indicator, diagnose it, and project every row to portable
columns. [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
is a transport operation, not validation, so the diagnostic check comes
first:

``` r

est_a <- sm_estimate(
  subset(alprek_subset, year == 2024),
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
#> 1    S001 2024      frpm 0.6847192 0.1581139        arcsine arcsine_vst
#> 2    S002 2024      frpm 0.4908827 0.1666667        arcsine arcsine_vst
#> 3    S003 2024      frpm 0.8309156 0.1507557        arcsine arcsine_vst
#> 4    S004 2024      frpm 0.6154797 0.1666667        arcsine arcsine_vst
#> 5    S005 2024      frpm 0.8410687 0.1666667        arcsine arcsine_vst
#>   flag_suppressed flag_below_accountability iv_eligible exclusion_reason
#> 1           FALSE                      TRUE        TRUE         eligible
#> 2           FALSE                      TRUE        TRUE         eligible
#> 3           FALSE                      TRUE        TRUE         eligible
#> 4           FALSE                      TRUE        TRUE         eligible
#> 5           FALSE                      TRUE        TRUE         eligible
```

Reconcile `iv_eligible` and `exclusion_reason` counts before filtering.
Carry `object_metadata` as an explicit sidecar when those attributes
matter, because a plain data frame does not preserve the
`sitemix_estimates` contract. Rename `theta_hat` and `se` only at the
boundary where another package specifies different argument names. Keep
`estimate_scale`, `var_method`, and audit columns alongside them so the
translation remains reviewable. Never relabel `sensitivity_var` as an
ordinary SE or inverse-variance weight.

## 4. One scalar table per indicator (Scenarios B / C / D1)

For a multi-indicator result, remove `V` deliberately by projecting all
rows before filtering or splitting. Directly selecting a partial
indicator set from a `V`-bearing object is rejected because it would
leave a partial covariance group:

``` r

est_b <- sm_estimate(
  subset(alprek_subset, year == 2024),
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

This workflow deliberately omits cross-indicator covariance. It is
appropriate only when the downstream analyses are genuinely separate.

## 5. Joint covariance output

For a joint analysis, validate at summary, row, and covariance levels
before conversion. A consumed group must contain only identified
coordinates, a valid non-stale `V`, valid smoothing provenance, and one
complete indicator set. A suppression or sensitivity row cannot be
removed while its matrix is retained.

Make one covariance-index row per `(site_id, year)` tuple. Align rows
with `indicator_order`; do not assume the incoming data-frame order is
the matrix order. This Scenario-B example deliberately chooses a named
contrast on the raw scale because its reported arcsine estimates are not
compatible with the raw covariance:

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
#> 1       -0.20000000       0.016000000
#> 3       -0.11111111       0.035665295
#> 5        0.09090909       0.007513148
#> 7       -0.11111111       0.010973937
#> 9        0.11111111       0.060356653
```

In this example `theta_hat` uses the arcsine scale while `V` uses the
raw scale, so `diag_b$estimate_vcov_scale_compatible` is `FALSE`. The
example does not ignore that warning: it explicitly uses `theta_raw`
with the raw `V` and records `point_scale = "raw_probability"`. If
`analysis_scale = "reported"`, compatibility must be `TRUE`; there is no
automatic transformation. A named contrast uses $`a^\top V a`$ and does
not require `solve(V)`, so valid singular multinomial matrices and
exact-census zero matrices remain consumable.

## 6. Fréchet projections are separate stress scenarios

[`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
has a different role from the ordinary `V` above. Its
`raw_pairwise_intervals` are formal pairwise intervals only for approved
D1a input. The canonical projected fields
`projected_negative_dependence_stress` and
`projected_positive_dependence_stress` are separately labeled PSD stress
scenarios with `projected_scenario_role = "stress_scenario_not_bound"`.

Validate an envelope by calling
[`summary()`](https://rdrr.io/r/base/summary.html) before extraction,
carry its population regime, scope, scenario, projection method/status,
distance, sign-change, raw-interval-violation, and order-reversal
diagnostics, and rerun the downstream calculation once per scenario. Do
not wrap a projected matrix as `sm_vcov`, insert it into ordinary `V`,
average the two scenarios, or report them as multivariate lower and
upper bounds. Even when the `K = 2` projection is an identity, the
formal claim comes from `raw_pairwise_intervals`; D1b remains heuristic.

## 7. Diagnostics before downstream use

Use
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
as an intrinsic contract check:

| Diagnostic fact | Interpretation |
|:---|:---|
| `scalar_uncertainty_finite` | identified canonical estimates and SEs are finite |
| `scalar_se_positive` | identified non-census scalar SEs are strictly positive |
| `n_zero_uncertainty_census` | exact SRSWOR census rows are valid but are not inverse-weighted |
| `indicator_scale_consistent` | each indicator uses one estimate scale |
| `v_present` / `v_valid` | covariance payload is present and structurally valid |
| `estimate_vcov_scale_compatible` | row estimates and covariance use compatible scales |
| `n_suppressed_missing` / `n_suppression_sensitivity` | coordinates that cannot enter an ordinary joint group |
| `smoothing_provenance_valid` / `v_stale` | whether canonical `V` remains coherent after smoothing activity |

These are properties of the uncertainty output itself, not claims about
compatibility with any particular downstream package.

The canonical pair is `se` plus `var_method`. If an analysis
deliberately chooses the experimental append-only smoother, it must
instead pair `se_smoothed` (or `se_raw_smoothed` on the raw scale) with
`var_method_smoothed`, preserve the canonical columns for audit, and
record the smoothing diagnostic. No portable workflow substitutes a
smoothed SE automatically.

## 8. Audit

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

## 9. What’s next?

- [A9 · Case study — Alabama
  Pre-K](https://joonho112.github.io/sitemix/articles/a9-case-study-alabama-prek.md)
  for the estimate → audit → canonical uncertainty export pipeline, with
  any experimental sensitivity kept separate and explicitly labeled.
- [M8 · Output
  contract](https://joonho112.github.io/sitemix/articles/m8-output-contract.md)
  for the formal scalar and covariance invariants.

## References
