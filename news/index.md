# Changelog

## sitemix 0.2.0 (development)

This development version is the package-neutral v0.2.0 release
candidate. External release remains pending the final quality and
maintainer approval gates.

### Highlights

- **Standalone and package-neutral.** `sitemix` no longer depends on any
  downstream consumer package; it returns estimates and sampling
  uncertainty through its own data-frame and covariance contracts.
- **One handoff, many consumers.** Select IDs with `theta_hat`/`se` (or
  `theta_raw`/`se_raw`) and a validated grouped `V`; downstream
  reporting, meta-analysis, small-area, or hierarchical models all
  consume the same output.
- **Explicit D1 dependence provenance.** Aggregate-marginal (D1) outputs
  now require `sampling_relation` (`"unknown"` by default); equal
  denominators no longer imply a common-population regime.
- **Formal vs. stress, clearly separated.**
  [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  returns formal raw pairwise intervals and *separately labeled*
  projected stress scenarios — never treated as covariance bounds.
- **Smoothing stays experimental.**
  [`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
  is opt-in and append-only; a fixed-seed audit found no evidence to
  promote it to a default, so canonical standard errors remain primary.
- **Documentation realigned and reproducible.** The 17 canonical
  vignettes and all roxygen were realigned to the v0.2.0 positioning
  with pinned dates and seeds; test coverage rose to about 91%.

### Migrating from v0.1.0

| v0.1 surface | v0.2.0 contract |
|:---|:---|
| Optional `ebrecipe` dependency | Removed; no replacement consumer package is required |
| `as_eb_input()` | Retired; use canonical IDs, estimates, SEs, provenance, and validated grouped `V` directly |
| `sitemix_role = "eb_handoff"` | Replaced by `sitemix_role = "summary_uncertainty"` |
| Adapter-readiness diagnostics | Replaced by intrinsic scalar, covariance, scale, suppression, and sensitivity facts |

### Documentation and reproducibility

- Renamed the installed A8 and M8 vignette topics to
  `a8-downstream-workflows` and `m8-output-contract`. The prior pkgdown
  HTML article URLs redirect to the new pages; the old installed
  [`vignette()`](https://rdrr.io/r/utils/vignette.html) topic names are
  not retained as extra canonical vignettes.
- Fixed all 17 canonical vignette dates, RNG algorithms, and seed; made
  code errors and unexpected warnings fatal; and explicitly capture only
  the six documented classed warnings expected in isolated fresh-session
  renders.

### Statistical, API, and data-contract hardening

*The remaining sections record the statistical, numerical, and
data-contract hardening in v0.2.0; applied readers can skim them.*

- Hardened subgroup pivots around publisher total labels and composition
  routing. A fixed, case/whitespace/punctuation-tolerant vocabulary now
  maps `ALL`, `ALL STUDENT(S)`, `TOTAL`, and `OVERALL` to canonical
  `ALL` in structural keys and Framing-Y indicators while retaining the
  publisher label in `source_subgroup`. Alias collisions, incomplete
  site-year category grids, suppressed case-composition counts, and
  partition mismatches fail closed; absent subgroup rows are never
  inferred to be structural zeros. The existing partition-target
  vocabulary is unchanged, and mixed reporting-level routing remains an
  explicit classed no-go.
- Replaced fixed absolute covariance checks with separate scale-aware
  tolerances for PSD validity, symmetry, simplex residuals, and
  numerical rank. Equivalent matrices now retain pass/fail and
  numerical-rank decisions under rescaling, while multinomial rank
  remains the analytic support rank. Fréchet projection eigenvalue
  checks now interpret `psd_tol` relatively, consistent with the
  projection algorithms.
- Separated formal Fréchet results from projected stress scenarios.
  [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  now stores D1a `/n` oracles in `raw_pairwise_intervals`, including
  marginal probabilities, common denominator, joint-probability,
  correlation, and raw covariance endpoints. Formal D1a fails closed
  unless same-unit, common-denominator, IID plug-in,
  no-FPC/no-bias-correction provenance is present. PSD outputs are now
  named `projected_negative_dependence_stress` and
  `projected_positive_dependence_stress`; sign changes, pair-order
  reversals, and raw-interval violations are explicit diagnostics, not
  lower/upper-bound claims. Legacy `V_lower_*`/`V_upper_*` and
  `R_lower`/`R_upper` fields remain deprecated exact aliases for v0.2
  compatibility. D1b remains an explicitly acknowledged heuristic stress
  analysis.
- Hardened Fréchet PSD numerics and projection provenance. Canonical
  `projection_diagnostics` is now long (site-year by stress scenario)
  with method/status, attempted/converged state, iteration caps,
  scale-aware relative and absolute tolerances, before/after eigen
  diagnostics, requested/applied shrinkage, projection distances, and
  diagonal/symmetry/PSD invariants. Non-converged Higham and
  automatic-shrink repairs now error. Fixed shrinkage is applied exactly
  for `K > 2`, while `K <= 2` remains identity. A complete sanitized
  `projection_config` supports deterministic replay validation;
  deprecated `psd_diagnostics` remains a wide compatibility table.

### Breaking package-neutral transition

- Removed the optional `ebrecipe` dependency and the consumer-specific
  `as_eb_input()` adapter. `sitemix` now exposes estimates and sampling
  uncertainty through its native data-frame and covariance contracts.
- Replaced `sitemix_role = "eb_handoff"` with
  `sitemix_role = "summary_uncertainty"` and replaced adapter-readiness
  diagnostics with intrinsic scalar, covariance, and scale facts.
  [`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
  now applies an error/warning/note/ok severity matrix, distinguishes
  estimate/`V` scale mismatch from matrix invalidity, and reports
  suppression-sensitivity, smoothing-relation, and stale-`V` facts.
  Exact SRSWOR census zero uncertainty is an explicit note rather than
  an invalid nonpositive-SE error.
  [`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
  exposes canonical `sensitivity_*` decision fields while retaining
  `upper_bound_*` compatibility fields. Long aggregate publisher flags
  now remain aligned after normalized row sorting.
- Locked the package-neutral joint-uncertainty contract: diagnose
  summary, row, and covariance levels before conversion; align complete
  tuple groups by `indicator_order`; use `theta_hat` only with a
  compatible reported-scale matrix or explicitly pair `theta_raw` with a
  validated raw matrix; and evaluate named contrasts with $`a^\top V a`$
  without assuming invertibility. Stale matrices, invalid smoothing
  provenance, suppression, and sensitivity coordinates fail closed.
  Fréchet projected matrices remain separately labeled stress scenarios,
  never ordinary `sm_vcov` objects or covariance bounds.
- Migration: diagnose before conversion, then project every row to IDs,
  `theta_hat`/`se`, scale/method provenance, suppression/accountability
  flags, and all present status/sensitivity fields before filtering.
  Ordinary inverse-variance input is restricted to identified finite
  positive-SE rows; exact-census zero-SE, suppressed-missing, and
  non-identified sensitivity rows remain explicitly audited and are not
  weighted. Use `theta_raw` and `se_raw` together for raw-scale
  analysis; use a validated grouped `V` and its
  `indicator_order`/`vcov_scale` metadata for joint analysis.
  Experimental smoothed SEs must remain paired with
  `var_method_smoothed`. Consumer-specific conversion now belongs
  downstream.
- D1 aggregate outputs now require explicit sampling-unit provenance
  through `sampling_relation = "unknown"`, `"same_units"`, or
  `"different_units"`. The default is `"unknown"`; equal denominators no
  longer imply D1a. Outputs record `denominator_pattern` independently,
  require one complete ordered indicator set across groups, and retain
  diagonal `vcov_method = "working_independence"` covariance.
- Suppression rows now carry an explicit `estimate_status`. The default
  `suppression = "drop"` retains canonical missing point/SE audit rows.
  The legacy `"upper_bound"` label requires
  `suppression_sensitivity_acknowledge = TRUE` and stores the Bernoulli
  worst-case variance scenario only in separated `sensitivity_*` fields.
  Synthetic sensitivity rows cannot enter ordinary `V` or Fréchet
  inputs; hidden denominators make no numeric variance claim.
- Reframed
  [`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
  as an experimental generalized variance-function/log-variance
  smoother, not a Fay–Herriot area-level estimator. Append-only
  smoothing now keeps canonical `se`, `se_raw`, and `var_method`
  unchanged; raw and transformed alternatives use distinct columns and
  provenance. Matching-scale matrix overwrite is a hard error, and
  custom formulas, fits, and predictions are validated before provenance
  is stamped.
- Added a fixed-seed smoothing audit over small/large denominators and
  near-boundary/interior rates. Neither loglinear nor GAM smoothing met
  the predeclared joint MSE, coverage, and inverse-weight promotion
  criteria on raw or transformed SEs, so smoothing remains opt-in,
  append-only, and experimental rather than a recommended canonical
  default.
- Reinterpreted `fpc` explicitly as the fixed population size `N` under
  SRSWOR. Scalar or input-row-aligned, group-constant population sizes
  are accepted; `N = n` is a valid zero-uncertainty census. FPC now
  propagates through raw, arcsine, logit, and Anscombe scalar SEs, while
  `binomial_bc` uses the approved design-corrected formula. Outputs with
  FPC include population size, sampling fraction, multiplier, design,
  and variance-rule provenance without overloading `n_eff`.
- Propagated SRSWOR through Scenario B/C whole covariance matrices and
  D1 working-independence diagonals. Scenario B uses `q * Q / n^2` for
  plug-in variance and `(N-n) * Q / (N*n*(n-1))` for `binomial_bc`;
  Scenario C uses the corresponding full-simplex formulas, preserving
  PSD and `V %*% 1 = 0`. `sm_vcov` now records population size,
  coordinate sampling fractions, conventional and actually applied
  multipliers, sampling design, variance rule, and an explicit row-SE
  diagonal contract. Scenario A/D0 1x1 matrices expose the same
  metadata, while `n_eff` remains transform provenance only.
- Independently characterized Scenario B against student-row and
  sufficient- count oracles. The whole-matrix correction includes
  off-diagonals and preserves correlations, symmetry, and PSD.
  Constant-indicator off-diagonals stay zero; Wilson remains a named
  positive diagonal surrogate, while Agresti–Coull boundary matrix
  output fails deterministically. Post-construction validation now
  cross-checks repeated Scenario B matrices against their row identity,
  scale, denominators, scalar rules, boundary provenance, and
  raw-diagonal contract.
- Corrected Scenario C rank metadata to the analytic simplex support
  rank `positive_support - 1`, including full, zero-category,
  degenerate, and zero-variance census cases. Independent count oracles
  now cover plug-in and whole-matrix `binomial_bc` formulas with and
  without FPC. Scenario C records one uniform global matrix rule and
  multiplier; zero-support row scalar provenance remains plug-in or
  Wilson because its exact zero matrix row and column cannot identify
  the scaling. A Scenario-C-specific output validator now locks support
  rank, matrix formula, scale, denominators, boundary and diagonal
  contracts, and row/matrix provenance against repeated tampering.

### Current limitations

- Supported estimands are site- and group-level proportions and rates;
  v0.2.0 does not claim arbitrary summary-statistic support.
- Published D1 marginals do not identify dependence. Working
  independence, formal raw pairwise intervals where assumptions permit
  them, and projected stress scenarios remain explicitly distinct.
- Variance smoothing remains experimental, opt-in, and append-only after
  the fixed-seed promotion criteria were not met.
- Suppression-sensitivity rows are audit scenarios rather than ordinary
  identified estimates and cannot enter ordinary covariance or Fréchet
  input.

## sitemix 0.1.0 (2026-05-25; alpha / internal release) — superseded by 0.2.0

First alpha / internal release of `sitemix`. This release brings the
package to feature parity with the v0.1.0 blueprint: a complete
first-stage estimator for proportions and rates across five scenarios (A
binomial, B multivariate, C multinomial, D0 / D1 aggregate), a
`sitemix_estimates` output schema with stable columns and audit trails,
opt-in variance smoothing, Fréchet-envelope sensitivity for unidentified
D1 covariance, and a scalar empirical- Bayes handoff to the downstream
`ebrecipe` package. API names and edge-case behavior may evolve before
v1.0.

### Public API

The 12 exported objects, grouped by what the analyst is doing:

**Estimate site-year rates**

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  — main dispatcher for student rows, sufficient counts, and published
  aggregates.
- [`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
  — sufficient-counts wrapper.
- [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
  — published-aggregates wrapper with D0 / D1 dispatch.

**Audit before handoff**

- [`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
  — summary-, row-, and covariance-level diagnostics.
- [`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
  — pre-estimation publisher-side suppression audit.

**Hand off to empirical Bayes**

- `as_eb_input()` — scalar handoff adapter for `ebrecipe`, with explicit
  capability gates for unsupported multivariate and Fréchet-corner
  paths.

**Inspect and stress-test covariance**

- [`sm_vcov()`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
  — S3 covariance-matrix constructor and validator carrying
  `vcov_scale`, `vcov_method`, and provenance metadata.
- [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  — Fréchet-Hoeffding envelope diagnostic for D1 aggregate marginals
  (D1a formal + D1b heuristic).

**Smooth and stabilize standard errors**

- [`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
  — opt-in Fay-Herriot-style variance smoothing with log-linear
  (default) and `mgcv` GAM backends.

**Reshape published aggregates**

- [`sm_pivot_subgroups_to_sites()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md)
  — Framing X (subgroup-as-site).
- [`sm_pivot_subgroups_to_indicators()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md)
  — Framing Y (subgroup-as-indicator).

**Data**

- `alprek_subset` — anonymized 50-site Alabama Pre-K sample used across
  examples, tests, and vignettes.

### Estimation Scenarios

- **Scenario A — binomial.** Single binary indicator per site- year,
  from student rows or sufficient counts. Scalar SE on the arcsine VST
  by default; logit and identity transforms supported.
- **Scenario B — multivariate.** Overlapping binary indicators with
  SUR-style cross-indicator covariance in an optional `V` list-column.
- **Scenario C — multinomial.** Mutually exclusive categories with full
  simplex covariance.
- **Scenario D0 — aggregate binomial.** One published numerator and
  denominator per site-year.
- **Scenario D1 — aggregate marginal.** Multiple aggregate marginal
  indicators per site-year with working-independence diagonal covariance
  and optional Fréchet-envelope sensitivity.

### Aggregate Inputs, Suppression, and Pivots

- Long / wide aggregate validators for D0 and D1 inputs.
- Suppression detection from structural missingness, explicit publisher
  flags, or a user-supplied predicate.
- `suppression = "drop"` and `suppression = "upper_bound"` handling.
- Subgroup pivot helpers for subgroup-as-site (Framing X) and
  subgroup-as-indicator (Framing Y) framings.
- [`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
  for pre-estimation suppression audits.

### Diagnostics, Covariance, and Sensitivity Tools

- `sitemix_estimates` and `sm_vcov` S3 surfaces with validation and
  print / format methods.
- [`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
  for output-schema, adapter-readiness, and covariance diagnostics at
  three levels (summary / row / vcov).
- [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  for formal D1a and heuristic D1b covariance sensitivity checks with
  Higham nearest-PSD or shrinkage repair.
- [`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
  for opt-in Fay-Herriot-style standard- error smoothing while
  preserving audit-trail columns by default. The default method is
  dependency-free log-linear smoothing; `method = "gam"` is optional and
  runtime-guarded through `mgcv`. Raw-scale smoothing uses a scale-aware
  `offset(p_offset)`; boundary rows use a Wilson-centered offset rather
  than the observed 0/1 rate. Multi-year smoothing emits
  `sitemix_warning_smoother_multi_year_default` when eligible rows span
  multiple years and `by = NULL`; use `by = "year"` to add a year fixed
  effect in one pooled smoothing model.

### EB Handoff

- Scalar `as_eb_input()` handoff to the installed `ebrecipe` package.
- Capability-based guardrails for unsupported multivariate and Fréchet
  matrix handoff paths. Unsupported paths fail with classed `sitemix`
  conditions rather than silently dropping covariance information.
- Per-session cached `ebrecipe` capability detection while preserving
  formal-name-based dispatch instead of version-string gating.

### Data, Examples, and Documentation

- Deterministic AL Pre-K sample-data build scripts and provenance notes.
- `inst/extdata/alprek_subset.csv` for non-R consumers.
- `inst/extdata/alprek_subset_counts.rds` for sufficient-count examples
  and regression tests.
- README quickstart and 5-minute teaser for Scenario A.
- 17 vignettes split across an Applied Track (`a1` – `a9`) and a
  Methodological Track (`m1` – `m8`) covering all five scenarios,
  diagnostics, suppression, smoothing, Fréchet sensitivity, the EB
  handoff, and an end-to-end Alabama Pre-K case study.
- Bundled `alprek_subset` `student_id` values are synthetic, sequential,
  and stable across years for the same selected child.
- Pkgdown reference grouped into eight thematic verb-led sections.

### Package Metadata

- Maintainer email confirmed as `jlee296@ua.edu`.
- Maintainer ORCID `0009-0006-4019-8703` recorded in `Authors@R`.
- GitHub repository and issue tracker recorded in `DESCRIPTION`:
  `https://github.com/joonho112/sitemix` and
  `https://github.com/joonho112/sitemix/issues`.
- Runtime imports trimmed to the actually-used surface: `rlang`,
  `tibble`, `vctrs`, `cli`, `Matrix`.
- Pkgdown GitHub Pages deployment is deferred until the Pages URL is
  live; `_pkgdown.yml` intentionally omits `url:` for now. Local pkgdown
  builds remain part of the package-quality workflow.
- Developer-only regression-baseline builder excluded from source
  package builds.

### Tests, Regression Fixtures, and CI

- Schema snapshots for A / B / C / D0 / D1 outputs, smoothing, and
  suppression.
- Error-class matrix tests for the documented condition hierarchy.
- Deterministic regression fixtures for small synthetic cases and AL
  Pre-K aggregate examples.
- Advisory performance smoke benchmarks under `inst/bench/`.
- GitHub Actions and local-equivalent CI configuration for package
  checks, docs checks, coverage artifacts, and performance smoke
  artifacts.

### Behavior Changes

- Boundary options now regularize uncertainty without replacing the
  observed point: `theta_raw` remains `C / n` under every
  `boundary_method`. `agresti_coull` now uses the standard z-general
  adjusted-Wald SE rather than the former fixed plus-four followed by a
  second Wilson adjustment.
- Raw boundary provenance is now explicit: `wilson_boundary_surrogate`
  and `agresti_coull_boundary_surrogate` replace the ambiguous output
  labels `wilson_floor` and `agresti_coull`. The public
  `boundary_method` option values are unchanged. The same explicit
  labels apply to `sm_vcov$scalar_correction_rule`.
- `binomial_bc` now propagates the n-1 raw variance through legal
  interior arcsine and logit delta methods, recorded as
  `arcsine_delta_binomial_bc` and `logit_delta_binomial_bc`.
- Logit output remains interior-only for every boundary method.
  Agresti-Coull or `binomial_bc` combined with Anscombe now fails early
  with `sitemix_error_anscombe_incompatible_correction`. Agresti-Coull
  boundary regularization with matrix output remains unsupported.

### Known Limitations

- This is a v0.1 alpha release. API names and edge-case behavior may
  still change before a v1.0 release.
- Current `ebrecipe` integration is scalar. Matrix-capable multivariate
  and Frechet-corner EB handoff paths are capability-gated and
  intentionally error when the installed `ebrecipe` consumer lacks the
  needed API.
- Scenario B/C `V` matrices are raw-scale diagnostic covariance matrices
  even when row-level `theta_hat` and `se` are on the default arcsine
  scale. Always check `sm_vcov$vcov_scale` before using a matrix
  downstream.
- Published aggregate input is intentionally scoped to D0 single-
  indicator binomial rows and D1 marginal multivariate rows. Aggregate
  multinomial composition estimation is not implemented, pairwise
  co-occurrence aggregate columns belong on the sufficient-count path,
  and raw subgroup files should be normalized with the pivot helpers
  before D0/D1 estimation.
- [`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
  audits publisher and structural suppression but does not reconstruct
  hidden values. Suppression sensitivity remains non-identified and is
  never an ordinary estimate or covariance input. A hidden denominator
  precludes a numeric sensitivity variance without stronger publisher
  constraints.
- D1 covariance matrices are working-independence approximations with
  zero off-diagonals because cross-indicator covariance is not
  identified from published marginals alone.
  [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  is a sensitivity diagnostic; formal interpretation is limited to D1a
  common-population marginals, while D1b subgroup-conditional envelopes
  are heuristic stress tests.
- [`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
  is opt-in scalar-SE smoothing. By default it appends `se_smoothed`
  without changing `se`; `overwrite = TRUE` changes scalar SE columns
  but does not recompute existing `V` matrices. Matrix-bearing overwrite
  emits `sitemix_warning_smoothing_v_stale` because stored covariance
  matrices remain pre-smoothing audit objects. Raw-scale smoothing
  remains warning-gated, although its default formula is now
  scale-aware. Multi-year smoothing pools eligible years unless
  `by = "year"` is supplied, and warns with
  `sitemix_warning_smoother_multi_year_default` when that pooled default
  is used. `by = "year"` is a fixed-effect adjustment in one pooled
  model, not separate per-year fits. Direct `as_eb_input()` selection of
  `se_smoothed` is deferred.
- The pkgdown site and vignettes are alpha documentation. Vignettes
  cover representative workflows, not every publisher schema or
  downstream EB configuration. Rebuild pkgdown before publication so
  generated news reflects source `NEWS.md`. GitHub repository and issues
  metadata are configured, but GitHub Pages deployment is deferred until
  the Pages URL exists; `_pkgdown.yml` intentionally omits `url:` until
  that deployment target is live.
- Some v1.1 adapter conveniences, including selecting `se_smoothed`
  directly in `as_eb_input()` and automatically excluding rows below
  accountability thresholds, are deferred.
- The Gate F deferral register is explicit in the blueprint roadmap.
  Deferred non-blockers include user-configurable additions beyond the
  fixed `ALL` alias vocabulary, automatic suppression-report emission,
  mixed-scale diagnostic refinements, `cran-comments.md`, and optional
  GAM REML reproducibility tests.
- Coverage is currently below the 90% v1.0 target. The Gate F follow-up
  coverage rerun measured total coverage at 83.68%; targeted coverage
  work remains planned before v1.0.
