# M8 · Output contract

Abstract

For methodologists evaluating how sitemix outputs can be consumed by
reporting systems or statistical models. Defines a package-neutral
scalar and covariance contract, the required scale metadata, and
executable invariants for reporting, meta-analysis, small-area
estimation, hierarchical models, and other direct-column workflows.

## Overview

This article is written for **methodologists** evaluating how sitemix
outputs can be consumed by reporting systems or statistical models who
need the package-neutral contract every consumer inherits and *why* it
stays independent of any downstream class. We cover, in order:

1.  the scope of the output contract;
2.  the notation-to-field crosswalk;
3.  the scalar contract;
4.  the covariance contract;
5.  the scenario map;
6.  the diagnostic-facts table;
7.  the implementation invariants;
8.  why Fréchet projections are stress scenarios, not covariance bounds;
9.  the consumer checklist.

**Established vs. novel.** *Established:* the downstream use cases
sitemix serves — reporting, meta-analysis, small-area estimation,
hierarchical modeling — are standard and package-independent. *This
package:* the package-neutral scalar and covariance output contract, the
diagnostic-facts table, and the UO1–UO6 invariants that any consumer can
check.

| Result | Attribution |
|:---|:---|
| downstream use cases (reporting / meta-analysis / small-area / hierarchical) | standard, package-independent |
| package-neutral output contract + diagnostic facts + UO invariants | this package (sitemix) |

> **About the example data.** All results here are computed live from
> `alprek_subset`, an anonymized, illustrative 50-site sample of Alabama
> First Class Pre-K records shipped in the package (see
> [`?alprek_subset`](https://joonho112.github.io/sitemix/reference/alprek_subset.md)).
> It is not a real accountability dataset and must not be cited as
> empirical Pre-K results. Every code block runs offline with a fixed
> random seed.

## 1. Scope

sitemix quantifies sampling uncertainty for site- and group-level
proportions and rates. Its output contract is the data carried by a
`sitemix_estimates` tibble; it does not depend on a consumer-specific
class. A downstream system may use the estimates for reporting,
meta-analysis, small-area estimation, or another model, provided it
respects the recorded scales and provenance.

## 2. Notation crosswalk

| Symbol | sitemix field | Meaning |
|:---|:---|:---|
| $`\hat\theta_{jt,k}`$ | `theta_hat` | estimate on `estimate_scale` |
| $`s_{jt,k}`$ | `se` | scalar standard error on the same scale |
| $`\hat\pi_{jt,k}`$ | `theta_raw` | raw proportion or rate |
| $`V_{jt}`$ | `V` | optional within-site covariance matrix |
| $`\sigma_{jt,k}`$ | `estimate_scale` | row estimate/SE scale |
| $`\sigma^V_{jt}`$ | `V[[i]]$vcov_scale` | covariance scale |
| $`m_{jt,k}`$ | `var_method` | scalar uncertainty provenance |
| $`d_{jt,k}`$ | eight FPC/design columns, when present | sampling-design provenance block |

The row identifiers `site_id`, `year`, and `indicator` bind these
quantities to observational units. Consumers may rename columns, but
should not discard the scale or method fields during translation.
Object-level family, role, D1, suppression, and smoothing provenance
should be captured in an explicit sidecar before conversion because a
plain data frame is not a validated `sitemix_estimates` object.

## 3. Scalar contract

**Definition 1 (scalar audit row).** For every output row $`r`$, the
tuple

``` math
(\text{unit}_r,\; \hat\theta_r,\; s_r,\; \sigma_r,\; m_r;\; d_r)
```

contains the canonical estimate and standard error (which can be missing
for suppressed rows), their declared scale and method provenance, and
the row’s suppression/accountability/status fields. Optional sensitivity
fields remain separate and are not ordinary sampling uncertainty. The
optional $`d_r`$ is absent when no fixed population is supplied. When
present, it is the indivisible eight-column FPC/design block:

| Field | Portable meaning |
|:---|:---|
| `population_size` | fixed site-year population size $`N`$ |
| `sampling_fraction` | $`n/N`$ |
| `fpc_variance_multiplier` | canonical SRSWOR variance multiplier $`q`$ |
| `fpc_se_multiplier` | canonical $`\sqrt q`$ |
| `variance_multiplier_applied` | multiplier actually applied under the selected scalar rule |
| `se_multiplier_applied` | square root of the applied variance multiplier |
| `sampling_design` | design label, currently `SRSWOR` |
| `variance_rule` | `plugin` or `design_corrected` |

A consumer must preserve all eight fields together. The canonical and
applied multipliers differ for corrected interior rows, so retaining
only $`N`$ or only the numerical SE is not a complete audit contract.
`n_eff` is not part of this block and is not changed by
finite-population correction.

**Definition 2 (inverse-variance-eligible row).** A scalar audit row is
eligible only when it is identified, its estimate and standard error are
finite, and $`s_r > 0`$. An exact SRSWOR census with $`s_r = 0`$ is
valid but forms a separate audited state; a consumer must not create an
infinite weight. Suppressed-missing and non-identified sensitivity rows
remain in the exchange for count reconciliation but are ineligible.

The scalar contract is portable because ordinary columns represent all
of its fields. No constructor from another package is part of the
sitemix API.

## 4. Covariance contract

**Definition 3 (joint uncertainty group).** For exactly one
`(site_id, year)` tuple with $`K`$ indicators, `V` is a symmetric
$`K \times K`$ matrix with indicator dimnames and `sm_vcov` metadata.
The group is complete only when its row indicators equal
`indicator_order`, without duplicates, and its row count, matrix
dimensions, dimnames, and matrix tuple metadata all agree. When the
output carries `K`, it must also equal the row and matrix dimension;
Scenario A/D0 can validly carry a keyed 1-by-1 `V` without a `K` column.
Tuple keys and matrix coordinates are authoritative; incidental
data-frame row order is not.

**Definition 4 (joint-consumption gate).** Before extraction, run
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
at summary, row, and covariance levels. Consumption requires identified
finite coordinates, no suppression or sensitivity role, a valid
positive-semidefinite repeated matrix, no diagnostic error, valid
smoothing provenance, and `v_stale` exactly `FALSE`. A partial
coordinate group, an unknown stale state, or a smoothed scalar
alternative paired with the old canonical matrix fails closed.

`vcov_scale`, `diag_contract`, and row-level `estimate_scale` determine
which point estimate can accompany `V`:

| Analysis branch | Required companion |
|:---|:---|
| reported | `theta_hat`, only when diagnostics say the estimate and matrix scales are compatible |
| raw | `theta_raw`, only when `vcov_scale = "raw"` and `diag_contract` explicitly identifies the raw row-SE companion |

There is no automatic transformation for an incompatible reported branch
and no automatic companion for `diag_contract = "not_checked"`. For a
complete named contrast $`a`$, use $`a^\top\hat\theta`$ and
$`a^\top V a`$. This operation does not require `solve(V)`: a valid
singular multinomial matrix and an exact census zero matrix remain
legitimate covariance states.

## 5. Scenario map

| Scenario | Scalar rows | Optional joint `V` | Main provenance |
|:---|:--:|:--:|:---|
| A: one binary indicator | yes | 1 x 1 when `vjt = TRUE` | `var_method`, `estimate_scale` |
| B: overlapping binaries | yes | yes when `vjt = TRUE` | SUR metadata |
| C: exclusive categories | yes | yes when `vjt = TRUE` | simplex metadata |
| D0: published single indicator | yes | 1 x 1 when `vjt = TRUE` | aggregate/suppression metadata |
| D1: published multiple indicators | yes | method-dependent | aggregate/sensitivity metadata |

The map describes which uncertainty objects sitemix can return. It does
not promise compatibility with an external model or software package.

## 6. Diagnostic facts

At summary level,
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
exposes facts rather than a single consumer-readiness flag:

| Field | Required interpretation |
|:---|:---|
| `scalar_uncertainty_finite` | identified canonical estimates and SEs are finite |
| `scalar_se_positive` | identified non-census scalar SEs are strictly positive |
| `n_zero_uncertainty_census` | exact SRSWOR census rows are valid but not inverse-weighted |
| `indicator_scale_consistent` | one estimate scale is used within each indicator |
| `v_present` | a covariance payload is available |
| `v_valid` | the payload passed structural and PSD validation |
| `estimate_vcov_scale_compatible` | estimate and covariance scale pairs are compatible |
| `n_suppressed_missing` / `n_suppression_sensitivity` | coordinates that cannot enter an ordinary joint group |
| `smoothing_provenance_valid` / `v_stale` | whether canonical covariance is coherent after smoothing activity |

`v_valid` and scale compatibility are `NA` when `V` is absent. This
distinguishes “not applicable” from a failed covariance check.

## 7. Implementation invariants

| ID | Claim |
|:---|:---|
| UO1 | All-row scalar export preserves identity, estimate, SE, scale, method, and present audit/status columns. |
| UO2 | IV eligibility is explicit: identified finite positive-SE rows only; census zero, suppression, and sensitivity are retained but not weighted. |
| UO3 | A returned `V` matrix is square, symmetric, and labeled by indicator. |
| UO4 | Diagnostics report estimate/covariance scale compatibility independently of matrix validity. |
| UO5 | Tuple keys, row indicators, `indicator_order`, dimnames, `K` when present, and all three diagnostic levels agree before conversion. A strict multivariate consumer may require `K`; a scalar 1-by-1 group need not carry it. |
| UO6 | A named linear contrast uses the explicitly selected compatible point scale and $`a^\top V a`$, without matrix inversion. |
| UO7 | If any row-level FPC/design field is present, all eight are present and portable together. |

Verify these claims on bundled data:

``` r

data(alprek_subset, package = "sitemix")

est <- sm_estimate(
  subset(alprek_subset, year == 2024),
  family = "binomial", indicator = "frpm"
)
est_b <- sm_estimate(
  subset(alprek_subset, year == 2024),
  family = "multivariate", indicators = c("frpm", "snap"),
  vjt = TRUE
)

# UO1: validate first, then make an all-row package-neutral scalar export.
diag_s <- sm_diagnose(est, verbose = FALSE)
object_metadata <- attributes(est)[c(
  "description", "family", "sitemix_role", "aggregate_case",
  "sampling_relation", "denominator_pattern", "d1_regime",
  "d1_regime_by_group", "suppression", "smoothing"
)]
audit_columns <- setdiff(names(est), c("V", "K"))
scalar <- as.data.frame(est[, audit_columns, drop = FALSE])
stopifnot(nrow(scalar) == nrow(est))

# UO2: eligibility is a local, auditable consumer decision.
status <- if ("estimate_status" %in% names(scalar)) {
  scalar$estimate_status
} else {
  rep("identified", nrow(scalar))
}
scalar$iv_eligible <-
  status == "identified" & is.finite(scalar$theta_hat) &
  is.finite(scalar$se) & scalar$se > 0
stopifnot(diag_s$scalar_uncertainty_finite)
stopifnot(diag_s$scalar_se_positive)
stopifnot(diag_s$indicator_scale_consistent)
stopifnot(sum(table(scalar$iv_eligible)) == nrow(est))

# UO7: finite-population design provenance is an all-or-none row block.
fpc_fields <- c(
  "population_size", "sampling_fraction",
  "fpc_variance_multiplier", "fpc_se_multiplier",
  "variance_multiplier_applied", "se_multiplier_applied",
  "sampling_design", "variance_rule"
)
fpc_counts <- data.frame(
  site_id = "F", year = 2024L, n_jt = 8L, c_jt_rate = 3L
)
est_fpc <- sm_estimate_from_counts(
  fpc_counts, family = "binomial", indicator = "rate",
  fpc = 20, min_n = 1L
)
stopifnot(all(fpc_fields %in% names(est_fpc)))
stopifnot(!anyNA(est_fpc[fpc_fields]))
stopifnot(identical(est_fpc$n_eff, 8))

# UO3/UO5: diagnose first; align tuple keys and matrix coordinates rather than
# incidental row order.
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
  !any(diag_b_row$suppression_sensitivity_role != "none"),
  !identical(diag_b$smoothing_provenance_valid, FALSE),
  identical(diag_b$v_stale, FALSE),
  !any(diag_b_row$diag_severity == "error"),
  all(diag_b_vcov$v_valid),
  all(diag_b_vcov$psd_ok),
  all(diag_b_vcov$repeated_v_equal),
  !any(diag_b_vcov$row_sum_zero_ok %in% FALSE),
  !any(diag_b_vcov$diag_severity == "error")
)

plain_b <- as.data.frame(est_b)
first_key <- unique(plain_b[c("site_id", "year")])[1L, , drop = FALSE]
take <- plain_b$site_id == first_key$site_id & plain_b$year == first_key$year
block <- plain_b[take, , drop = FALSE]
V_object <- block$V[[1L]]
stopifnot(all(vapply(block$V, identical, logical(1), V_object)))
stopifnot(identical(V_object$site_id, first_key$site_id))
stopifnot(identical(V_object$year, first_key$year))
stopifnot(!anyDuplicated(block$indicator))
stopifnot(setequal(block$indicator, V_object$indicator_order))
coordinate <- match(V_object$indicator_order, block$indicator)
stopifnot(!anyNA(coordinate), length(coordinate) == nrow(block))
aligned <- block[coordinate, , drop = FALSE]
V <- as.matrix(V_object)
stopifnot(identical(aligned$indicator, V_object$indicator_order))
stopifnot(nrow(V) == ncol(V))
if ("K" %in% names(block)) {
  stopifnot(all(block$K == nrow(V)))
} else {
  stopifnot(nrow(V) == 1L)
}
stopifnot(isTRUE(all.equal(V, t(V), tolerance = 1e-12)))
stopifnot(identical(rownames(V), V_object$indicator_order))
stopifnot(identical(colnames(V), V_object$indicator_order))

# UO4/UO6: matrix validity and scale compatibility are separate facts. This
# Scenario-B result needs an explicit raw branch; a reported branch is blocked.
stopifnot(!diag_b$estimate_vcov_scale_compatible)
stopifnot(identical(V_object$vcov_scale, "raw"))
stopifnot(V_object$diag_contract %in% c(
  "row_se_raw_squared",
  "row_se_raw_squared_except_boundary_surrogates"
))
analysis_scale <- "raw"
contrast <- c(frpm = 1, snap = -1)
point <- aligned$theta_raw
weight <- unname(contrast[V_object$indicator_order])
stopifnot(identical(analysis_scale, "raw"), !anyNA(weight))
contrast_estimate <- sum(weight * point)
contrast_variance <- drop(crossprod(weight, V %*% weight))
stopifnot(
  is.finite(contrast_estimate),
  is.finite(contrast_variance),
  contrast_variance >= -1e-14
)
contrast_variance <- max(0, contrast_variance)
```

## 8. Fréchet projections are stress scenarios, not covariance bounds

[`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
is not an alternative constructor for ordinary `V`. For approved D1a
input, `raw_pairwise_intervals` contains the formal raw-scale pairwise
intervals. The projected matrices repair incompatible pairwise corners
into positive-semidefinite **stress scenarios**. Their canonical role is
`stress_scenario_not_bound`; projection can change signs, leave raw
intervals, or reverse the two requested corners for particular
contrasts.

The following example validates the envelope through
[`summary()`](https://rdrr.io/r/base/summary.html), extracts one plain
matrix, and carries the scenario and projection diagnostics in a
sidecar. The downstream contrast is rerun under that scenario; the
matrix is never relabeled `sm_vcov` or inserted into the ordinary `V`
column.

``` r

d1_data <- data.frame(
  site_id = rep("D", 3L),
  year = rep(2025L, 3L),
  indicator = c("a", "b", "c"),
  c_jt = c(20L, 70L, 40L),
  n_jt = rep(100L, 3L)
)
d1_est <- capture_expected_sitemix_warning(
  sm_estimate_from_aggregates(
    d1_data,
    family = "multivariate",
    sampling_relation = "same_units",
    vst = "none",
    boundary_method = "none",
    vjt = FALSE,
    min_n = 1L
  ),
  "sitemix_warning_working_independence_default"
)
envelope <- sm_frechet_envelope(
  d1_est,
  population_regime = "d1a",
  psd_method = "higham"
)
envelope_diag <- as.data.frame(summary(envelope))
stopifnot(
  identical(envelope$projected_scenario_role, "stress_scenario_not_bound"),
  identical(envelope$covariance_scale, "raw"),
  all(envelope$raw_pairwise_intervals$interval_scope ==
        "formal_raw_pairwise_interval")
)

scenario <- "negative_dependence_stress"
scenario_diag <- envelope_diag[
  envelope_diag$scenario == scenario,
  ,
  drop = FALSE
]
stopifnot(
  nrow(scenario_diag) == 1L,
  scenario_diag$diagonal_preserved,
  scenario_diag$symmetry_preserved,
  scenario_diag$psd_preserved,
  if (scenario_diag$projection_attempted) {
    isTRUE(scenario_diag$converged)
  } else {
    is.na(scenario_diag$converged)
  }
)

stress_matrix <- envelope$projected_negative_dependence_stress[[1L]]
stress_contrast <- c(a = 1, b = -1, c = 0)
stress_weight <- unname(stress_contrast[rownames(stress_matrix)])
stopifnot(
  is.matrix(stress_matrix),
  !inherits(stress_matrix, "sm_vcov"),
  identical(colnames(stress_matrix), rownames(stress_matrix)),
  !anyNA(stress_weight)
)
stress_variance <- drop(crossprod(
  stress_weight,
  stress_matrix %*% stress_weight
))
stress_sidecar <- data.frame(
  scenario = scenario,
  scenario_role = envelope$projected_scenario_role,
  population_regime = envelope$population_regime,
  frechet_scope = envelope$frechet_scope,
  estimate_scale = scenario_diag$estimate_scale,
  vcov_scale = scenario_diag$vcov_scale,
  projection_method = scenario_diag$projection_method,
  projection_status = scenario_diag$projection_status,
  projection_distance_relative = scenario_diag$projection_distance_relative,
  sign_changes = scenario_diag$sign_changes,
  raw_interval_violations = scenario_diag$raw_interval_violations,
  projected_order_reversals = scenario_diag$projected_order_reversals,
  contrast_variance = max(0, stress_variance)
)
stress_sidecar
#>                     scenario             scenario_role population_regime
#> 1 negative_dependence_stress stress_scenario_not_bound               d1a
#>   frechet_scope  estimate_scale vcov_scale projection_method projection_status
#> 1        formal raw_probability        raw            higham         projected
#>   projection_distance_relative sign_changes raw_interval_violations
#> 1                      0.16526            0                       0
#>   projected_order_reversals contrast_variance
#> 1                         0       0.005760642
```

For $`K=2`$, an identity projection still does not turn the projected
field into a bound: the formal statement remains in
`raw_pairwise_intervals`. D1b uses
`frechet_scope = "heuristic_stress_test"` and its pairwise ranges are
heuristic, not identified covariance intervals. Deprecated lower/upper
aliases are not a portable exchange contract.

## 9. Consumer checklist

Before using sitemix output elsewhere:

1.  Preserve row identifiers and `indicator` ordering.
2.  Map `theta_hat` and `se` without changing their scale implicitly.
3.  Preserve `estimate_scale`, `var_method`, suppression/accountability
    flags, and every present `estimate_status`/`sensitivity_*` field.
4.  If any FPC/design field is present, carry all eight fields together:
    `population_size`, `sampling_fraction`, `fpc_variance_multiplier`,
    `fpc_se_multiplier`, `variance_multiplier_applied`,
    `se_multiplier_applied`, `sampling_design`, and `variance_rule`.
5.  Assign an explicit eligibility/exclusion reason and reconcile all
    row counts before filtering; never treat sensitivity variance or
    census zero as an ordinary inverse-variance weight.
6.  For joint analysis, run all three diagnostic levels and align
    complete tuple/coordinate groups with `indicator_order`, matrix
    dimnames, and `K` when present. A strict multivariate consumer may
    require `K`; Scenario A/D0 1-by-1 covariance can validly omit it.
7.  Select either a compatible reported branch or an explicit raw
    branch; evaluate named contrasts with $`a^\top V a`$ without
    assuming invertibility.
8.  Reject stale covariance, invalid smoothing provenance, partial
    groups, suppression, and sensitivity coordinates.
9.  Keep Fréchet projections as separately labeled stress-scenario
    matrices, with their full projection-diagnostic sidecar.
10. Carry applicable object-level provenance in an explicit metadata
    sidecar.

## 10. Where to go next

- [A8 · Downstream
  workflows](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md)
  for the applied direct-column workflow.
- [M1 · Statistical
  foundations](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md)
  for the sampling models behind `theta_hat`, `se`, and `V`.

## References
