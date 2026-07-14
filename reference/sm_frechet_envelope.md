# Compute D1 pairwise Fréchet intervals and projected stress scenarios

`sm_frechet_envelope()` preserves the raw pairwise Fréchet intervals for
D1 marginal aggregate estimates as the formal result. It also assembles
the pairwise negative- and positive-dependence corners and, for more
than two indicators, PSD-projects those corners while preserving the
raw-scale plug-in diagonal. The projected matrices are stress scenarios,
not multivariate lower or upper bounds, and are not validated `sm_vcov`
objects. Projection can change a pair's sign, move it outside its raw
pairwise interval, or reverse the elementwise order of the two
scenarios; those events are reported explicitly in
`projection_diagnostics`.

## Usage

``` r
sm_frechet_envelope(
  x,
  indicator = NULL,
  population_regime = NULL,
  subgroup_conditional_action = c("warn", "allow", "error"),
  return_correlations = FALSE,
  psd_method = c("higham", "shrink"),
  psd_tol = 1e-08,
  psd_max_iter = 100L,
  shrink_alpha = NULL,
  ...
)
```

## Arguments

- x:

  A D1 aggregate-path `sitemix_estimates` object. Both `vjt = FALSE` and
  `vjt = TRUE` outputs are supported; formal D1a validation uses row and
  matrix provenance when each is available.

- indicator:

  Character vector or `NULL` (default `NULL`). Optional indicators to
  retain before envelope construction. `NULL` retains all indicators
  present in `x`.

- population_regime:

  Character scalar. Required regime label. One of `"d1a"` (formal
  common-population) or `"d1b"` (subgroup-conditional heuristic). No
  default; omission raises the stable required-regime condition
  documented under *Errors*.

- subgroup_conditional_action:

  Character scalar. Action for `population_regime = "d1b"`. One of
  `"warn"` (default; proceed with a warning), `"allow"` (proceed
  silently), or `"error"` (abort). Ignored when
  `population_regime = "d1a"`.

- return_correlations:

  Logical scalar. If `TRUE`, return the raw pairwise Fréchet correlation
  endpoints as matrices in `pairwise_correlation_lower` and
  `pairwise_correlation_upper`. Defaults to `FALSE`; invalid values
  raise the stable condition documented under *Errors*.

- psd_method:

  Character scalar. PSD repair method: `"higham"` (default;
  [`Matrix::nearPD()`](https://rdrr.io/pkg/Matrix/man/nearPD.html)) or
  `"shrink"` (line search toward the working-independence diagonal).

- psd_tol:

  Positive numeric scalar. Relative PSD tolerance for the smallest
  eigenvalue and projection convergence. Eigenvalue checks scale this
  value by the matrix eigenvalue scale, with a machine-range absolute
  floor. Must not exceed `sqrt(.Machine$double.eps)` so that this
  numerical allowance cannot relabel materially indefinite matrices as
  PSD. Defaults to `1e-8`.

- psd_max_iter:

  Positive integer scalar. Maximum iterations for the PSD repair
  algorithm. Defaults to `100L`.

- shrink_alpha:

  Positive numeric scalar in \\(0, 1\]\\ or `NULL` (default `NULL`).
  Fixed shrinkage weight used exactly for \\K \> 2\\ when
  `psd_method = "shrink"`. `NULL` triggers a line search for the largest
  PSD-feasible weight. Ignored only by the exact \\K \le 2\\ identity
  policy.

- ...:

  Supported deterministic arguments forwarded to
  [`Matrix::nearPD()`](https://rdrr.io/pkg/Matrix/man/nearPD.html) when
  `psd_method = "higham"`: `base.matrix`, `doSym`, `doDykstra`,
  `posd.tol`, and `conv.norm.type`. Arguments that conflict with the
  package's diagonal, tolerance, iteration, symmetry, or output contract
  are rejected.

## Value

An `sm_frechet_envelope` list object with the following fields:

- `raw_pairwise_intervals`:

  One row per site-year and indicator pair, with raw pairwise
  joint-probability, correlation, and covariance interval endpoints. The
  covariance scale is raw probability. The row-level `interval_scope`
  distinguishes formal D1a intervals from D1b heuristic ranges.

- `V_independence`:

  List of per-site-year raw-scale covariance matrices. For formal D1a
  this is the IID plug-in diagonal, not the transformed row `V`.

- `unprojected_negative_dependence_corner`,
  `unprojected_positive_dependence_corner`:

  Lists of matrices assembled from the raw pairwise covariance endpoints
  before PSD projection. They are corners, not globally ordered
  covariance bounds.

- `projected_negative_dependence_stress`,
  `projected_positive_dependence_stress`:

  Lists of PSD-projected corner stress scenarios. They are not
  lower/upper bounds.

- `pairwise_correlation_lower`, `pairwise_correlation_upper`:

  Lists of raw pairwise correlation endpoint matrices when
  `return_correlations = TRUE`. Both slots are always present on the
  object; they are `NULL` otherwise.

- `projection_diagnostics`:

  Canonical long diagnostics with one row per site-year and stress
  scenario. Rows record estimate and covariance scales; method, status,
  attempted/converged state and iterations; relative and realized
  absolute tolerances; before/after eigenvalue scales and minima;
  requested/applied shrinkage; raw and projected norms and distances;
  and diagonal, symmetry, PSD, sign-change, projected-order, and
  raw-interval invariants.

- `site_keys`:

  Site-year identifier mapping for the list slots.

- `call`:

  The function call as captured by
  [`match.call()`](https://rdrr.io/r/base/match.call.html).

- `population_regime`:

  Character scalar; the regime label passed to `population_regime`.

- `frechet_scope`:

  Character scalar; either formal raw pairwise intervals or a heuristic
  D1b stress range.

- `covariance_scale`, `projected_scenario_role`:

  Canonical scale and role metadata.

- `psd_method`, `psd_tol`, `psd_max_iter`, `shrink_alpha`,
  `projection_config`:

  PSD repair settings actually applied.

- `V_lower_raw`, `V_upper_raw`, `V_lower_psd`, `V_upper_psd`, `R_lower`,
  `R_upper`:

  Deprecated compatibility aliases for the canonical matrix fields
  above. New code should not use these names because projected matrices
  are not bounds.

- `psd_diagnostics`:

  Deprecated one-row-per-site-year wide compatibility table. New code
  should consume the canonical long `projection_diagnostics`.

## Details

**Two interpretations: D1a and D1b.** The function supports two
population regimes that the caller must explicitly choose between:

- **D1a – formal common-population**:

  The published marginals refer to the same sampled units, use one
  finite denominator per site-year, and use the IID plug-in
  raw-probability variance rule without FPC or `binomial_bc`. These
  conditions must already be recorded by the D1 estimator. Under them,
  each raw pairwise covariance interval is formal. A matching
  denominator alone is not provenance.

- **D1b – subgroup-conditional heuristic**:

  The published marginals are subgroup-conditional and the envelope is a
  **heuristic stress test**, not a formal bound. Requires explicit
  acknowledgement via `subgroup_conditional_action = "allow"` (or
  proceeds with a warning under `"warn"`); `"error"` aborts the call.

**PSD stress scenarios.** The assembled corner matrices are not
guaranteed to be PSD at the corners of the Fréchet box; the function
applies PSD projection via `psd_method = "higham"` (Higham 2002
nearest-PSD algorithm via
[`Matrix::nearPD()`](https://rdrr.io/pkg/Matrix/man/nearPD.html)) or
`"shrink"` (line search for the largest shrinkage weight that yields a
PSD matrix). The working-independence diagonal is preserved through
repair. For \\K \le 2\\, both projected fields are exact identities of
their source corners and no iterative method is attempted. For \\K \>
2\\, a fixed `shrink_alpha` is applied exactly even when the source
corner is already PSD; an infeasible fixed value errors. Higham and
automatic-shrink nonconvergence also error rather than returning a
partially repaired matrix.

For the formal derivation see
[`vignette("m7-frechet-envelope-theory", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/m7-frechet-envelope-theory.md).
Suppressed-missing and suppression-sensitivity rows are rejected at
entry: formal pairwise intervals require complete identified marginals.

## Errors

Omitting `population_regime` raises
`sitemix_error_population_regime_required`. Other invalid regime values
raise the corresponding stable invalid-regime condition. Invalid
`return_correlations` values raise
`sitemix_error_invalid_return_correlations`.

## References

Fréchet, M. (1951). Sur les tableaux de corrélation dont les marges sont
données. *Annales de l'Université de Lyon, 3e série, Section A: Sciences
Mathématiques et Astronomie*, **14**, 53–77.

Hoeffding, W. (1994). Scale-Invariant Correlation Theory. In N. I.
Fisher and P. K. Sen (Eds.), *The Collected Works of Wassily Hoeffding*
(pp. 57–107). Springer. doi:10.1007/978-1-4612-0865-5_4. (English
reprint of the 1940 original.)

Nelsen, R. B. (2006). *An Introduction to Copulas* (2nd ed.). Springer.

Higham, N. J. (2002). Computing the nearest correlation matrix—a problem
from finance. *IMA Journal of Numerical Analysis*, **22**(3), 329–343.
doi:10.1093/imanum/22.3.329.

## See also

- [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
  for the D1 estimator producing the working-independence `V`.

- [`sm_vcov()`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
  for the matrix class spec and `vcov_scale` convention.

- [`vignette("a7-variance-smoothing-and-frechet")`](https://joonho112.github.io/sitemix/articles/a7-variance-smoothing-and-frechet.md)
  for the applied walkthrough.

- [`vignette("m7-frechet-envelope-theory")`](https://joonho112.github.io/sitemix/articles/m7-frechet-envelope-theory.md)
  for the formal derivation and D1a / D1b distinction.

Other covariance:
[`sm_vcov()`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)

## Examples

``` r
# D1 example using bundled counts as a synthetic aggregate slice
counts_path <- system.file(
  "extdata", "alprek_subset_counts.rds",
  package = "sitemix", mustWork = TRUE
)
counts <- readRDS(counts_path)

# Build a tiny D1 input: two marginals (frpm, snap), one year
d1 <- counts[counts$year == 2024, ]
d1_long <- rbind(
  data.frame(
    site_id   = d1$site_id, year = d1$year,
    indicator = "frpm",
    c_jt      = d1$c_jt_frpm, n_jt = d1$n_jt
  ),
  data.frame(
    site_id   = d1$site_id, year = d1$year,
    indicator = "snap",
    c_jt      = d1$c_jt_snap, n_jt = d1$n_jt
  )
)

est <- sm_estimate_from_aggregates(
  d1_long, family = "multivariate",
  indicator_col = "indicator",
  sampling_relation = "same_units",
  vjt = TRUE
)
#> Warning: Working-independence default selected for D1 aggregate input.
#> ✖ Expected `identified within-site covariance off-diagonals`.
#> ℹ Actual: `off-diagonals structurally unidentified from marginal aggregates`.
#> ℹ Fix: Use `sm_frechet_envelope()` once sensitivity diagnostics are available.
env <- sm_frechet_envelope(est, population_regime = "d1a")
class(env)
#> [1] "sm_frechet_envelope" "list"               
env$psd_method
#> [1] "higham"
```
