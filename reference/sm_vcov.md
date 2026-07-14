# Construct or inspect a within-site covariance object

`sm_vcov()` is the constructor and validator for the `sm_vcov` S3 class
— the per-site-year within-indicator covariance carrier that populates
the optional `V` list-column of a `sitemix_estimates` tibble. Every
`sitemix_estimates` row produced with `vjt = TRUE` carries one `sm_vcov`
object; this function is the canonical home for the scale and method
lexicons every downstream consumer reads.

## Usage

``` r
sm_vcov(
  matrix,
  site_id = NA_character_,
  year = NA_integer_,
  indicator_order = colnames(matrix),
  family,
  vcov_method = NA_character_,
  estimate_scale,
  vcov_scale,
  matrix_boundary_rule = "none",
  scalar_correction_rule = rep("none", length(indicator_order)),
  psd_repair = "none",
  matrix_rank = NULL,
  positive_support = NA_integer_,
  n_jt = NA_integer_,
  n_eff = NA_real_,
  population_size = NA_real_,
  sampling_fraction = NA_real_,
  fpc_variance_multiplier = 1,
  fpc_se_multiplier = 1,
  variance_multiplier_applied = 1,
  se_multiplier_applied = 1,
  sampling_design = "not_specified",
  variance_rule = "plugin",
  diag_contract = "not_checked"
)
```

## Arguments

- matrix:

  Symmetric PSD numeric matrix of dimension `K x K` with \\K \ge 1\\.
  Rows and columns are indexed by `indicator_order`.

- site_id:

  Site identifier metadata; character or integer. Defaults to
  `NA_character_`.

- year:

  Integer scalar. Year identifier stored as metadata. Defaults to
  `NA_integer_`.

- indicator_order:

  Character row and column order for `matrix`; defaults to
  `colnames(matrix)`.

- family:

  Character scalar. Estimation family that produced the matrix. One of
  `"binomial"`, `"multivariate"`, or `"multinomial"`. Required.

- vcov_method:

  Construction method; see *vcov_method convention*. Character scalar;
  defaults to `NA_character_`, which is valid only for
  `family = "binomial"`. Multivariate and multinomial callers must
  supply a compatible recorded method.

- estimate_scale:

  Character scalar. The row-level `estimate_scale` of the producing
  `sitemix_estimates` tibble. One of `"none"`, `"arcsine"`,
  `"arcsine_anscombe"`, or `"logit"`; required.

- vcov_scale:

  Character scalar. The scale of `matrix` entries; one of `"raw"`,
  `"arcsine_delta"`, `"logit_delta"`, or `"reference_raw"`. Required.
  **May differ from estimate_scale** under Scenarios B and C; see
  *vcov_scale convention* below.

- matrix_boundary_rule:

  Character scalar. Metadata recording how boundary cells were handled
  during matrix construction. Defaults to `"none"`.

- scalar_correction_rule:

  Character vector of length `length(indicator_order)`. Per-indicator
  scalar SE correction rules; supported values are listed under *Scalar
  correction rules*. Defaults to `rep("none", length(indicator_order))`.

- psd_repair:

  Character scalar. Metadata recording the PSD repair (if any) applied
  during matrix construction. Defaults to `"none"`.

- matrix_rank:

  Integer scalar or `NULL` (default `NULL`). Rank metadata. For Scenario
  C this is the analytic simplex rank `positive_support - 1`, including
  when a census makes the realized sampling covariance exactly zero. For
  other families it is the numerical matrix rank. When `NULL`, the
  constructor computes the family-appropriate value.

- positive_support:

  Integer scalar. Number of positive categories for Scenario C
  multinomial output and the basis of its analytic rank; `NA_integer_`
  otherwise. Defaults to `NA_integer_`.

- n_jt:

  Integer scalar. Cell size metadata. Defaults to `NA_integer_`.

- n_eff:

  Numeric scalar. Effective sample size metadata. Defaults to
  `NA_real_`.

- population_size:

  Numeric scalar. Fixed site-year population size under SRSWOR, or
  `NA_real_` when no finite-population design was supplied.

- sampling_fraction:

  Sampling fraction `n / population_size`; numeric scalar or aligned
  vector. Missing without an FPC design.

- fpc_variance_multiplier:

  Numeric scalar or coordinate-aligned vector. Conventional SRSWOR
  variance multiplier `(N - n) / (N - 1)`, with census value zero.

- fpc_se_multiplier:

  Numeric scalar or coordinate-aligned vector. Square root of the
  conventional FPC variance multiplier documented above.

- variance_multiplier_applied:

  Numeric scalar or coordinate-aligned vector recording the multiplier
  actually applied: the conventional FPC for plug-in variance or
  `(N - n) / N` for the design-corrected rule.

- se_multiplier_applied:

  Numeric scalar or coordinate-aligned vector. Square root of the
  applied variance multiplier documented above.

- sampling_design:

  Character scalar, one of `"not_specified"` or `"SRSWOR"`.

- variance_rule:

  Character scalar or aligned vector: `"plugin"` or
  `"design_corrected"`.

- diag_contract:

  Character scalar documenting which row companion the covariance
  diagonal matches. See *Diagonal contract*.

## Value

An S3 object of class `sm_vcov` with fields `matrix`, `site_id`, `year`,
`indicator_order`, `family`, `vcov_method`, `estimate_scale`,
`vcov_scale`, `matrix_boundary_rule`, `scalar_correction_rule`,
`psd_repair`, `matrix_rank`, `positive_support`, `n_jt`, `n_eff`, the
finite-population provenance fields (`population_size`,
`sampling_fraction`, `fpc_variance_multiplier`, `fpc_se_multiplier`,
`variance_multiplier_applied`, `se_multiplier_applied`,
`sampling_design`, `variance_rule`), and `diag_contract`.

## Details

An `sm_vcov` object is a structured list carrying the covariance matrix
and structured metadata documenting how it was constructed, on which
scale, and over which site-year. The class enforces PSD-ness up to a
numerical tolerance and validates the lexicon values listed below.

**Scale and method lexicons.** The `vcov_scale` and `vcov_method` fields
are the canonical home for sitemix's covariance vocabulary; every other
function that touches a covariance matrix reads these fields rather than
re-deriving the lexicon. See the named sections below for the locked
vocabulary. `sm_vcov()` is the only exported constructor. Low-level
construction and validation helpers remain internal implementation
details; the registered
[`format()`](https://rdrr.io/r/base/format.html),
[`print()`](https://rdrr.io/r/base/print.html), and
[`as.matrix()`](https://rdrr.io/r/base/matrix.html) methods revalidate
an object before reading it so direct post-construction tampering fails
with a classed covariance error. Matrix validity uses separate
scale-aware tolerances for symmetry, positive-semidefiniteness, the
multinomial simplex identity, and numerical rank. Each tolerance
combines a matrix-scale-relative term with a machine-range absolute
floor; numerical rank does not reuse the more permissive PSD-validity
threshold. Multinomial rank remains the analytic support rank described
below.

For the SUR derivation see
[`vignette("m3-multivariate-sur-covariance")`](https://joonho112.github.io/sitemix/articles/m3-multivariate-sur-covariance.md);
for the multinomial simplex covariance see
[`vignette("m4-multinomial-simplex")`](https://joonho112.github.io/sitemix/articles/m4-multinomial-simplex.md);
for the D1 working-independence rationale see
[`vignette("m5-aggregate-engines")`](https://joonho112.github.io/sitemix/articles/m5-aggregate-engines.md).

## Scalar correction rules

- `"none"`: no scalar correction.

- `"binomial_bc"`: the binomial n-1 correction.

- `"wilson_boundary_surrogate"`: Wilson boundary uncertainty.

- `"agresti_coull_boundary_surrogate"`: Agresti–Coull boundary
  uncertainty.

## vcov_scale convention

`vcov_scale` reports the scale on which the stored covariance is
computed; it **may differ from a row's estimate_scale** under Scenarios
B and C, where the row-level `theta_hat` is reported on the requested
`vst` (e.g., arcsine) but the joint covariance is computed in
raw-proportion space for numerical stability. Always read `vcov_scale`
from this slot before consuming `matrix`.

## Scenario B whole-matrix contract

Let \\Q = \sum_i (y_i - \bar y)(y_i - \bar y)^\top\\ and \\q =
(N-n)/(N-1)\\. Scenario B stores the raw-scale plug-in matrix \\Q/n^2\\;
with a fixed SRSWOR population it stores \\qQ/n^2\\. When
`bias_correction = "binomial_bc"`, the entire matrix—not only its
diagonal—is replaced by \\Q/\[n(n-1)\]\\, or by \\(N-n)Q/\[Nn(n-1)\]\\
under SRSWOR. Uniform whole-matrix scaling preserves correlations,
symmetry, and positive semidefiniteness. A constant indicator has zero
off-diagonals. Wilson boundary handling is an explicit positive
scalar/diagonal surrogate; Agresti–Coull boundary handling is not legal
when a Scenario B matrix is requested. When an `sm_vcov` is attached to
package-produced Scenario B rows, `validate.sitemix_estimates()`
cross-checks the SUR method, raw matrix scale, row-raw-SE diagonal
contract, denominators, scalar rules, and boundary rule against those
rows; direct stand-alone `sm_vcov()` construction remains available for
explicitly user-defined covariance objects.

## Scenario C whole-matrix contract

For \\M = \mathrm{diag}(\hat\pi)-\hat\pi\hat\pi^\top\\, Scenario C
stores \\M/n\\ without FPC and \\qM/n\\ under SRSWOR. With
`bias_correction = "binomial_bc"`, the whole matrix is \\M/(n-1)\\
without FPC and \\(N-n)M/\[N(n-1)\]\\ under SRSWOR. These rules preserve
PSD and \\V\mathbf 1=0\\. If \\S\\ categories have positive observed
count, `matrix_rank` records the analytic simplex support rank \\S-1\\;
this remains the metadata rank for a census even though its realized
sampling covariance is the zero matrix. The matrix-level `variance_rule`
and applied multiplier are uniform over all coordinates. A zero-support
coordinate keeps an exact zero matrix row and column, so its scalar
plug-in or Wilson provenance may intentionally differ from the global
design-corrected matrix rule. Package-output validation permits that
mismatch only at an exact structural zero and retains the explicit
Wilson boundary diagonal exception.

## Diagonal contract

`diag_contract = "row_se_squared"` means the matrix is on the row
estimate scale and its diagonal equals `se^2`; this is used by A, D0,
and D1. `"row_se_raw_squared"` means a raw-scale B/C matrix agrees with
`se_raw^2`. For multinomial boundary surrogates, the value formed by
concatenating `"row_se_raw_squared_"` and `"except_boundary_surrogates"`
records that the simplex-preserving matrix deliberately keeps a zero
boundary diagonal while the scalar Wilson surrogate remains positive.
`"not_checked"` is reserved for user-constructed objects without a row
companion.

## vcov_method convention

- `NA_character_`:

  No recorded method (e.g., user-constructed matrix).

- `"sur"`:

  Scenario B; multivariate SUR-style covariance with off-diagonal
  \\\sigma\_{kk'}\\ from joint proportions.

- `"multinomial"`:

  Scenario C; simplex covariance \\(\mathrm{diag}(\pi) - \pi \pi^\top) /
  n\\.

- `"working_independence"`:

  Scenario D1; working-independence diagonal covariance when
  cross-marginal joints are unidentified.

## var_method convention (row-level SE provenance)

Although `var_method` is a column of `sitemix_estimates` rather than a
field of `sm_vcov`, its locked lexicon is canonically defined here so
all SE-provenance documentation has a single home. The implemented base
values are grouped as follows:

- Arcsine values: `"arcsine_vst"` and `"arcsine_anscombe"`.

- Bias-corrected arcsine: `"arcsine_delta_binomial_bc"`.

- Logit values: `"logit_delta"` and `"logit_delta_binomial_bc"`.

- Raw-scale values: `"binomial"` and `"binomial_bc"`.

- Boundary values: `"wilson_boundary_surrogate"` and
  `"agresti_coull_boundary_surrogate"`.

- Suppression values: `"suppressed_drop"` and
  `"suppression_sensitivity"`.

Experimental GVF/log-variance smoothing records
`" + gvf_smooth_loglinear"` or `" + gvf_smooth_gam"` in the alternative
`var_method_smoothed` column; an allowed overwrite may copy that
provenance to canonical `var_method`. Legacy `" + fh_smooth_*"` labels
remain readable. Derivations are documented in:

- [`vignette("m2-scalar-se-binomial")`](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
  for scalar binomial SEs.

- [`vignette("m6-variance-smoothing-theory")`](https://joonho112.github.io/sitemix/articles/m6-variance-smoothing-theory.md)
  for smoothing.

## References

Zellner, A. (1962). An efficient method of estimating seemingly
unrelated regressions and tests for aggregation bias. *Journal of the
American Statistical Association*, **57**(298), 348–368.
[doi:10.1080/01621459.1962.10480664](https://doi.org/10.1080/01621459.1962.10480664)

## See also

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  for the main dispatcher that produces `V` list-columns of `sm_vcov`
  objects.

- [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  for Scenario-D1 pairwise intervals and projected stress.

- [`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
  for the `level = "vcov"` diagnostic.

- [`vignette("m3-multivariate-sur-covariance")`](https://joonho112.github.io/sitemix/articles/m3-multivariate-sur-covariance.md).

- [`vignette("m4-multinomial-simplex")`](https://joonho112.github.io/sitemix/articles/m4-multinomial-simplex.md).

- [`vignette("m7-frechet-envelope-theory")`](https://joonho112.github.io/sitemix/articles/m7-frechet-envelope-theory.md).

Other covariance:
[`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)

## Examples

``` r
data(alprek_subset, package = "sitemix")
est <- sm_estimate(
  subset(alprek_subset, year == 2024),
  family     = "multivariate",
  indicators = c("frpm", "snap"),
  vjt        = TRUE
)
v1 <- est$V[[1L]]
class(v1)
#> [1] "sm_vcov"
v1$vcov_scale
#> [1] "raw"
v1$vcov_method
#> [1] "sur"
dim(v1$matrix)
#> [1] 2 2
```
