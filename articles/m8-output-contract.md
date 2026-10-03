# Understanding returned estimates and uncertainty

Abstract

Read the estimates, standard errors, covariance matrices, and diagnostic
fields returned by sitemix. Match each quantity to its scale, retain
missing and zero-uncertainty rows when exporting, and check the
information needed for a subsequent analysis.

The result of
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
is a `sitemix_estimates` tibble. Its columns contain estimates and their
sampling uncertainty; its attributes record choices that apply to the
object as a whole. This article explains how to read those values and
check them before exporting or using them in another analysis. The
examples use the package’s fully simulated `prek_sim` data. They
describe no real children or programs.

## Read the scalar estimates

Each row describes one `site_id`, `year`, and `indicator`, which may
represent a site or a group constructed by a pivot. Keep these
identifiers with the values. The most frequently used columns are:

| Field | Meaning |
|:---|:---|
| `theta_raw` | Observed proportion, \hat\pi |
| `theta_hat` | Reported estimate, \hat\theta=g(\hat\pi) under the selected transformation |
| `se_raw` | Standard error on the proportion scale |
| `se` | Standard error on the scale of `theta_hat` |
| `n` | Recorded row denominator; suppression can record a bound instead of an observed count |
| `n_eff` | Calculation denominator for identified rows; it can record the same bound on an unavailable row |
| `estimate_scale`, `transform` | Labels identifying the estimate and transformation |
| `var_method` | Method used for the reported scalar uncertainty |
| `flag_small_n`, `flag_zero_cell`, `flag_suppressed`, `flag_below_accountability` | Conditions to inspect alongside the estimate |

`theta_raw` and `theta_hat` agree for `vst = "none"`. By default,
`theta_hat` is on the arcsine-square-root scale and `se` is its
corresponding working SE. Anscombe adjustment changes the probability
used in the point transformation as described in [Binomial standard
errors and
transformations](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md);
it does not change the observed `theta_raw`. Boundary handling can
supply a positive raw SE at an observed zero or one. Read `var_method`
rather than assuming every SE is the unadjusted binomial formula.

For a hidden suppressed denominator, `n` and `n_eff` may record
`suppressed_n_bound` even though no numeric sampling or sensitivity
variance is available. Those recorded values do not identify the actual
sample size. Inspect `sensitivity_n`, `sensitivity_method`, the original
input, and `attr(x, "suppression")$denominator_observed_on_suppressed`
for aggregate results (where `x` is the returned object). For identified
rows, `n_eff` is a calculation-specific denominator, not a general
survey effective sample size. [Sampling uncertainty from published
aggregates](https://joonho112.github.io/sitemix/articles/m5-aggregate-engines.md)
explains the observed- and hidden-denominator cases.

Optional columns depend on the request. `V` stores within-site-year
covariance when requested; B, C, and D1 also carry `K`. Aggregate
results add `estimate_status` and suppression information. An
acknowledged suppression sensitivity analysis adds `sensitivity_*`
columns while leaving the ordinary estimate and SE missing. Smoothing
adds alternative SE columns and method information by default. These
additions do not make every returned row identified or eligible for a
model.

Some information is stored in attributes, including `family`,
`sitemix_role`, `aggregate_case`, the D1 unit relationship, suppression
settings, and smoothing results. Save relevant attributes alongside an
export; a CSV cannot retain R attributes or validate a
`sitemix_estimates` object. The example below saves them in
`object_metadata`. Renaming an exported column does not change its scale
or statistical meaning.

## Retain the finite-population information

Supplying a fixed population adds these eight columns together:

| Field | Meaning |
|:---|:---|
| `population_size` | Fixed site-year population size N |
| `sampling_fraction` | n/N |
| `fpc_variance_multiplier` | Conventional SRSWOR plug-in variance multiplier q |
| `fpc_se_multiplier` | \sqrt q |
| `variance_multiplier_applied` | Multiplier used by the selected scalar variance rule |
| `se_multiplier_applied` | Square root of the applied variance multiplier |
| `sampling_design` | Design label, currently `SRSWOR` |
| `variance_rule` | `plugin` or `design_corrected` |

Keep all eight in an export so another reader can reconstruct which rule
was used. For plug-in rows outside a census, q=(N-n)/(N-1); corrected
interior rows instead use (N-n)/N as their applied multiplier. Boundary
surrogates use the plug-in multiplier, and the exact census case has
multiplier zero. [Binomial standard errors and
transformations](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
explains the assumptions and exceptions. `n_eff` is separate from these
fields: FPC does not change it. Without a supplied fixed population, the
eight-column block is absent.

## Choose rows for inverse-variance weighting

A report can retain every output row, including suppressed and census
rows. For an inverse-variance calculation, separately identify rows with
an identified, finite estimate and a finite, strictly positive SE. Use
an estimate and SE on the same scale. The example’s `iv_eligible` column
records this selection; it is created by the example and is not an
estimator output.

An exact SRSWOR census can validly have zero SE. Retain that fact
without calculating an infinite weight. Suppressed-missing rows and
sensitivity values that are not identified cannot supply ordinary
inverse-variance weights. Keep an exclusion reason and reconcile row
counts before filtering, as illustrated in [Using estimates and
covariance matrices in further
analyses](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md).
These numerical conditions permit a weight calculation; a subsequent
model still needs its own assumptions. `sitemix` does not fit that model
or construct another package’s input class.

## Match a covariance matrix to its estimates

When requested, `V` contains one covariance object per site-year,
repeated across that group’s indicator rows. Each object is an `sm_vcov`
with a symmetric matrix, indicator names, and information about its
calculation. Use [`as.matrix()`](https://rdrr.io/r/base/matrix.html) to
extract the numeric matrix. Before calculating a joint quantity, select
a complete `(site_id, year)` group and match the rows to
`V[[i]]$indicator_order`. Check for duplicate or missing indicators and
confirm that the matrix names, dimensions, site, and year agree. The `K`
column, when present, must agree with the dimension. A/D0 can return a
keyed 1-by-1 matrix without a `K` column.

The matrix scale need not match the reported scalar scale. Use
`vcov_scale`, `diag_contract`, and `estimate_scale` together:

| Quantity to analyze | Point estimate and matrix needed |
|:---|:---|
| Reported estimates | `theta_hat` with a matrix on a compatible reported scale |
| Raw proportions | `theta_raw` with `vcov_scale = "raw"` and the documented raw diagonal relationship |

A B matrix is raw-scale by default, while the default `theta_hat` and
`se` are transformed. Its diagonal agrees with `se_raw^2`, not the
transformed `se^2`. A C matrix is also raw-scale; its empirical
multinomial diagonal can differ from a boundary-adjusted `se_raw^2`. The
value `diag_contract = "row_se_raw_squared_except_boundary_surrogates"`
records that exception. See [Covariance for overlapping binary
indicators](https://joonho112.github.io/sitemix/articles/m3-multivariate-sur-covariance.md)
and [Covariance for multinomial
proportions](https://joonho112.github.io/sitemix/articles/m4-multinomial-simplex.md)
for the formulas. An unchecked diagonal relationship (`"not_checked"`)
does not establish a matching SE.

For a named linear contrast a, the estimate is a^\top\hat\theta and its
variance is a^\top V a, using the selected compatible scale. This does
not require matrix inversion. A valid singular multinomial covariance or
an exact census zero matrix can still support such a calculation. C’s
recorded support rank describes the unscaled empirical composition; at
census the numeric matrix has rank zero.

Run
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
at summary, row, and covariance levels before extraction. Inspect
errors, complete coordinate groups, repeated-matrix agreement, and scale
compatibility. If smoothing is present, also inspect
`smoothing_provenance_valid` and `v_stale`. A smoothed scalar
alternative cannot simply be paired with an unchanged matrix on the same
scale. The smoother normally appends columns and leaves `V` unchanged; a
successful fit cannot overwrite matching row SEs while retaining their
old matrix. The skip and different-scale cases are explained in
[Experimental models for variance
smoothing](https://joonho112.github.io/sitemix/articles/m6-variance-smoothing-theory.md).

## Which scenarios return a matrix?

| Input case | Scalar rows | Optional `V` |
|:---|:---|:---|
| A: one binary indicator | One rate per site-year | 1 x 1 on the reported scale |
| B: overlapping binaries | One rate per indicator | Raw covariance from joint observations or supported joint counts |
| C: exclusive categories | One proportion per category | Raw multinomial covariance |
| D0: one published marginal | One rate per site-year | 1 x 1 on the reported scale for observed counts |
| D1: several published marginals | One rate per marginal | Working-independence diagonal on the reported scale |

These matrices require `vjt = TRUE` and valid inputs. Suppressed
aggregate rows cannot enter ordinary `V`. B counts with K≥4 can produce
scalar output but are currently rejected when covariance is requested
because joint feasibility is not checked for that count representation.
Fréchet sensitivity matrices are returned separately, as described
below.

## Read the diagnostic fields together

[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
returns several checks so that an analysis can use the ones relevant to
its calculation. No single field establishes suitability for every
model.

| Summary field | Interpretation |
|:---|:---|
| `scalar_uncertainty_finite` | At least one identified row exists and its reported estimates and SEs are all finite |
| `scalar_se_positive` | At least one identified row exists and all identified scalar SEs are positive; a census zero makes this `FALSE` |
| `n_zero_uncertainty_census` | Count of identified exact-SRSWOR-census rows with zero raw and reported SE |
| `indicator_scale_consistent` | Each indicator uses one estimate scale |
| `v_present` | A `V` column is present |
| `v_valid` | The matrix objects passed validation |
| `estimate_vcov_scale_compatible` | Reported estimate scales match the matrix scales |
| `n_suppressed_missing` / `n_suppression_sensitivity` | Counts of unavailable rows, excluded from ordinary scalar and joint calculations |
| `smoothing_provenance_valid` / `v_stale` | Whether the recorded smoothing information is valid and whether matching covariance is stale |

When `V` is absent, `v_valid` and scale compatibility are `NA`. Without
smoothing, `smoothing_provenance_valid` is `NA` and `v_stale` is
`FALSE`. These missing values describe an inapplicable check. With a B
matrix, `v_valid = TRUE` and `estimate_vcov_scale_compatible = FALSE`
can occur together: the matrix can be valid for raw proportions while
the reported estimates are arcsine-transformed. The example below
selects the raw values explicitly. Diagnostic warning and error fields
are returned information; inspect them separately from any R warning
emitted by a function call.

## Check and export an example

The next examples retain every scalar row, select eligible rows
explicitly, and then calculate a raw-scale contrast using a complete
covariance group. The assertions make mismatched rows or scales visible
before the calculation.

### Scalar export and eligibility

``` r

data(prek_sim, package = "sitemix")

est <- sm_estimate(
  subset(prek_sim, year == 2024),
  family = "binomial", indicator = "frpm"
)
est_b <- sm_estimate(
  subset(prek_sim, year == 2024),
  family = "multivariate", indicators = c("frpm", "snap"),
  vjt = TRUE
)

# Diagnose the estimates, then retain every scalar row and its metadata.
diag_s <- sm_diagnose(est, verbose = FALSE)
object_metadata <- attributes(est)[c(
  "description", "family", "sitemix_role", "aggregate_case",
  "sampling_relation", "denominator_pattern", "d1_regime",
  "d1_regime_by_group", "suppression", "smoothing"
)]
audit_columns <- setdiff(names(est), c("V", "K"))
scalar <- as.data.frame(est[, audit_columns, drop = FALSE])
stopifnot(nrow(scalar) == nrow(est))

# Add an explicit eligibility decision for inverse-variance weighting.
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

table(iv_eligible = scalar$iv_eligible)
#> iv_eligible
#> TRUE 
#>   50
```

All 50 rows in this example are identified and have a positive finite
SE. The table reconciles the selection with the exported row count. For
a result with census or suppressed rows, keep those rows and record why
their weights are unavailable; do not reuse the positive-SE assertions
as universal claims.

### Preserve the population-size fields

A separate eight-student example shows the fields added by `fpc = 20`.

``` r

# Check that the complete finite-population field set is present.
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

as.data.frame(est_fpc)[fpc_fields]
#>   population_size sampling_fraction fpc_variance_multiplier fpc_se_multiplier
#> 1              20               0.4               0.6315789         0.7947194
#>   variance_multiplier_applied se_multiplier_applied sampling_design
#> 1                   0.6315789             0.7947194          SRSWOR
#>   variance_rule
#> 1        plugin
```

The conventional and applied variance multipliers are both 12/19 under
this plug-in calculation. The recorded denominator remains eight.

### Align a complete covariance group

The B example above returns a raw matrix for each site-year. Check all
three diagnostic levels and align the first group’s rows to its matrix.

``` r

# Match site-year keys and matrix coordinates.
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
```

These checks establish the group and coordinate correspondence. They do
not make its raw matrix compatible with the default arcsine estimates.
The next calculation uses `theta_raw` and compares the FRPM and SNAP
proportions.

``` r

# Use raw points with the validated raw covariance matrix.
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
data.frame(
  site_id = first_key$site_id,
  year = first_key$year,
  frpm_minus_snap = contrast_estimate,
  contrast_se = sqrt(contrast_variance)
)
#>   site_id year frpm_minus_snap contrast_se
#> 1    S001 2024      -0.1111111   0.1888526
```

The difference is on the proportion scale, and the SE includes the
observed FRPM–SNAP covariance. Multiplying both by 100 expresses them in
percentage points. The calculation uses the full named contrast and does
not invert the matrix. The tiny negative-variance allowance is only for
floating-point rounding in this example.

## Use a Fréchet scenario for sensitivity analysis

For D1 marginals observed on the same units,
[`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
can calculate pairwise covariance intervals conditional on the supplied
marginal proportions. Formal D1a use requires explicit `same_units`, a
common finite denominator, the IID plug-in calculation, no FPC or
`binomial_bc`, and `anscombe = FALSE`. These are not confidence
intervals for population covariances. [Pairwise Fréchet bounds and
projected dependence
scenarios](https://joonho112.github.io/sitemix/articles/m7-frechet-envelope-theory.md)
gives the derivation and explains why different-unit D1b use is
heuristic.

`raw_pairwise_intervals` contains those pairwise results. For K\>2,
setting all pairs to their lower endpoints can produce an indefinite
matrix. The projected fields provide PSD dependence scenarios for a
joint sensitivity calculation. Their role is recorded as
`stress_scenario_not_bound`: PSD alone does not establish a jointly
attainable binary distribution or sharp multivariate covariance bounds.
Projection can alter an entry’s sign, take it outside its raw pairwise
interval, or reverse the ordering of projected entries. Keep these
diagnostics with the scenario.

The following three-indicator example checks `summary(envelope)`,
extracts one plain matrix, and calculates the variance of the a-minus-b
contrast. `stress_sidecar` is an example-created table that saves the
scenario labels and diagnostic results alongside that variance. The
calculation is separate from the ordinary `V` object and does not fit a
downstream model.

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

The displayed variance describes the selected negative-dependence
scenario. It is not a lower or upper limit for every possible a-minus-b
variance. `projection_status` and `projection_distance_relative` show
whether and how much the source corner changed; the sign, range, and
ordering counts describe changes to individual entries. Retain the full
envelope when another reader needs all scenarios or diagnostic fields,
rather than only this summary.

For K=2, the projected matrices equal their original corners before
projection. In formal D1a use their entries therefore coincide
numerically with the pairwise endpoints, while the projected-field role
remains a sensitivity scenario. Use `raw_pairwise_intervals` when
reporting the formal interval. D1b records
`frechet_scope = "heuristic_stress_test"`; its covariance ranges do not
gain a formal interpretation from a successful PSD check. Deprecated
lower/upper aliases are documented in the help, but the descriptive
current fields make the intended use clearer.

## Before using the result elsewhere

Keep row identifiers, indicator order, estimate/SE scales, method
labels, status and suppression fields, and any population-size fields
with the values. Save applicable object attributes separately. Retain
unavailable rows long enough to explain exclusions, and choose finite
positive-SE rows explicitly for inverse-variance calculations.

For joint calculations, check complete groups and matrix coordinates at
all three diagnostic levels. Select the compatible point-estimate scale,
inspect smoothing and stale-matrix information, and calculate named
contrasts without assuming the matrix has an inverse. Keep Fréchet
scenarios separate from ordinary sampling covariance and retain their
diagnostics. The statistical assumptions of a subsequent model require a
separate assessment.

## Related examples

- [Using estimates and covariance matrices in further
  analyses](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md)
  walks through export and a raw covariance contrast with the simulated
  site data.
- [Sampling uncertainty in site-level
  proportions](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md)
  explains the sampling models behind the estimates and their
  uncertainty.
- [Pairwise Fréchet bounds and projected dependence
  scenarios](https://joonho112.github.io/sitemix/articles/m7-frechet-envelope-theory.md)
  derives the pairwise probability bounds and discusses PSD projection.

## References
