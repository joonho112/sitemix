# Construct or inspect a within-site covariance object

`sm_vcov()` creates a covariance matrix object with its site, year,
indicator order, and calculation method. Estimation functions return
these objects in the `V` list-column when `vjt = TRUE`. Use
[`as.matrix()`](https://rdrr.io/r/base/matrix.html) to extract a matrix,
and inspect `vcov_scale` before combining it with estimates or standard
errors.

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

  A symmetric positive semidefinite numeric matrix of dimension `K x K`
  with \\K \ge 1\\. Rows and columns are indexed by `indicator_order`.

- site_id:

  A character or integer site identifier. Defaults to `NA_character_`.

- year:

  A single integer year. Defaults to `NA_integer_`.

- indicator_order:

  Character row and column order for `matrix`; defaults to
  `colnames(matrix)`.

- family:

  Character scalar. Estimation family that produced the matrix. One of
  `"binomial"`, `"multivariate"`, or `"multinomial"`. Required.

- vcov_method:

  The calculation method; see *Covariance methods*. Character scalar;
  defaults to `NA_character_`, which is valid only for
  `family = "binomial"`. Multivariate and multinomial callers must
  supply a compatible method.

- estimate_scale:

  Character scalar. The row-level `estimate_scale` of the corresponding
  estimates. One of `"none"`, `"arcsine"`, `"arcsine_anscombe"`, or
  `"logit"`; required.

- vcov_scale:

  Character scalar. The scale of `matrix` entries; one of `"raw"`,
  `"arcsine_delta"`, `"logit_delta"`, or `"reference_raw"`. Required.
  **May differ from estimate_scale** under Scenarios B and C; see
  *Covariance scale* below.

- matrix_boundary_rule:

  A single string recording how boundary cells were handled during
  matrix construction. Defaults to `"none"`.

- scalar_correction_rule:

  Character vector of length `length(indicator_order)`. Per-indicator
  scalar SE correction rules; supported values are listed under *Scalar
  correction rules*. Defaults to `rep("none", length(indicator_order))`.

- psd_repair:

  A single string recording any positive-semidefinite repair applied
  during matrix construction. Defaults to `"none"`.

- matrix_rank:

  Integer scalar or `NULL` (default `NULL`). For Scenario C this is the
  analytic simplex rank `positive_support - 1`, including when a census
  makes the realized sampling covariance exactly zero. For other
  families it is the numerical matrix rank. When `NULL`, the constructor
  computes the family-appropriate value.

- positive_support:

  Integer scalar. Number of positive categories for Scenario C
  multinomial output and the basis of its analytic rank; `NA_integer_`
  otherwise. Defaults to `NA_integer_`.

- n_jt:

  A single integer denominator. Defaults to `NA_integer_`.

- n_eff:

  A numeric denominator used for transformed standard errors. Package
  estimates record `n + 1/2` for Anscombe or `n` otherwise, unchanged by
  FPC. This differs from the `n + 3/4` denominator in the Anscombe point
  transformation. Defaults to `NA_real_` for a directly constructed
  object.

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

  A single string identifying which scalar variance the covariance
  diagonal matches. See *Diagonal and standard errors*.

## Value

An S3 object of class `sm_vcov` with fields `matrix`, `site_id`, `year`,
`indicator_order`, `family`, `vcov_method`, `estimate_scale`,
`vcov_scale`, `matrix_boundary_rule`, `scalar_correction_rule`,
`psd_repair`, `matrix_rank`, `positive_support`, `n_jt`, `n_eff`, the
finite-population fields (`population_size`, `sampling_fraction`,
`fpc_variance_multiplier`, `fpc_se_multiplier`,
`variance_multiplier_applied`, `se_multiplier_applied`,
`sampling_design`, `variance_rule`), and `diag_contract`.

## Details

An `sm_vcov` object is a list containing a matrix and fields that
describe its calculation. The constructor checks dimensions, symmetry,
positive semidefiniteness, and the supplied field values.
[`format()`](https://rdrr.io/r/base/format.html),
[`print()`](https://rdrr.io/r/base/print.html), and
[`as.matrix()`](https://rdrr.io/r/base/matrix.html) also check the
object, so inconsistent changes to a matrix or its fields can cause an
error when it is used.

Symmetry, positive semidefiniteness, the multinomial simplex identity,
and numerical rank use separate tolerances. Each accounts for the
matrix's scale and has a small absolute floor. The numerical-rank
tolerance is stricter than the tolerance for positive semidefiniteness.
For multinomial matrices, `matrix_rank` instead records the analytic
support rank described below.

For overlapping binary indicators, see
[`vignette("m3-multivariate-sur-covariance")`](https://joonho112.github.io/sitemix/articles/m3-multivariate-sur-covariance.md);
for mutually exclusive categories, see
[`vignette("m4-multinomial-simplex")`](https://joonho112.github.io/sitemix/articles/m4-multinomial-simplex.md).
The working-independence assumption for published marginals is explained
in
[`vignette("m5-aggregate-engines")`](https://joonho112.github.io/sitemix/articles/m5-aggregate-engines.md).

## Scalar correction rules

- `"none"`: no scalar correction.

- `"binomial_bc"`: the binomial n-1 correction.

- `"wilson_boundary_surrogate"`: Wilson boundary uncertainty.

- `"agresti_coull_boundary_surrogate"`: Agresti–Coull boundary
  uncertainty.

## Covariance scale

`vcov_scale` gives the scale of the stored matrix. In multivariate
student-level and multinomial estimates (Scenarios B and C), the matrix
stays on the raw-proportion scale even when `theta_hat` uses arcsine or
logit. Check this field before using `matrix` in a model or combining it
with the reported standard errors.

## Multivariate student-level covariance (Scenario B)

Let \\Q = \sum_i (y_i - \bar y)(y_i - \bar y)^\top\\ and \\q =
(N-n)/(N-1)\\. Scenario B stores the raw-scale plug-in matrix \\Q/n^2\\;
with a fixed SRSWOR population it stores \\qQ/n^2\\. With
`bias_correction = "binomial_bc"`, the matrix becomes \\Q/\[n(n-1)\]\\,
or \\(N-n)Q/\[Nn(n-1)\]\\ under SRSWOR. Multiplying the matrix by a
positive scalar preserves correlations where they are defined, symmetry,
and positive semidefiniteness. A census gives a zero covariance matrix,
whose correlations are undefined. A constant indicator has zero
off-diagonals. Wilson boundary handling is a positive scalar and
diagonal substitute; Agresti–Coull boundary handling is unavailable when
requesting this matrix. Package estimates are checked for agreement
between the matrix and its rows, including the method, scale, diagonal,
denominators, and boundary corrections.

## Multinomial covariance (Scenario C)

For \\M = \mathrm{diag}(\hat\pi)-\hat\pi\hat\pi^\top\\, Scenario C
stores \\M/n\\ without FPC and \\qM/n\\ under SRSWOR. With
`bias_correction = "binomial_bc"`, the whole matrix is \\M/(n-1)\\
without FPC and \\(N-n)M/\[N(n-1)\]\\ under SRSWOR. These rules preserve
PSD and \\V\mathbf 1=0\\. If \\S\\ categories have positive observed
count, `matrix_rank` records the analytic simplex support rank \\S-1\\;
this remains the recorded rank for a census even though its realized
sampling covariance is the zero matrix. The matrix-level `variance_rule`
and applied multiplier are uniform over all coordinates. A zero-support
coordinate keeps an exact zero matrix row and column. The scalar
calculation for that category can therefore differ from the matrix's
design-corrected rule. In particular, a positive Wilson boundary
standard error does not replace the zero matrix diagonal.

## Diagonal and standard errors

`diag_contract = "row_se_squared"` means the matrix is on the row
estimate scale and its diagonal equals `se^2`; this is used by A, D0,
and D1. `"row_se_raw_squared"` means a raw-scale B/C matrix agrees with
`se_raw^2`. For multinomial boundary surrogates, the value formed by
concatenating `"row_se_raw_squared_"` and `"except_boundary_surrogates"`
records that the simplex-preserving matrix deliberately keeps a zero
boundary diagonal while the scalar Wilson surrogate remains positive.
`"not_checked"` is reserved for user-constructed objects without a row
companion.

## Covariance methods

- `NA_character_`:

  No recorded method; valid only for `family = "binomial"`.

- `"sur"`:

  Scenario B; multivariate SUR-style covariance with off-diagonal
  \\\sigma\_{kk'}\\ from joint proportions.

- `"multinomial"`:

  Scenario C; simplex covariance \\(\mathrm{diag}(\pi) - \pi \pi^\top) /
  n\\.

- `"working_independence"`:

  Scenario D1; working-independence diagonal covariance when
  cross-marginal joints are unidentified.

## Standard-error methods

The `var_method` column in the estimates records how each standard error
was calculated. It is separate from the matrix's `vcov_method`. Its base
values are:

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
`var_method_smoothed` column; an allowed overwrite may copy that method
label to `var_method`. Legacy `" + fh_smooth_*"` labels remain readable.
Derivations are documented in:

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
  for the estimator that produces `V` list-columns of `sm_vcov` objects.

- [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  for pairwise intervals and dependence stress scenarios for D1
  aggregates.

- [`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
  for the `level = "vcov"` diagnostic.

- [`vignette("m3-multivariate-sur-covariance")`](https://joonho112.github.io/sitemix/articles/m3-multivariate-sur-covariance.md).

- [`vignette("m4-multinomial-simplex")`](https://joonho112.github.io/sitemix/articles/m4-multinomial-simplex.md).

- [`vignette("m7-frechet-envelope-theory")`](https://joonho112.github.io/sitemix/articles/m7-frechet-envelope-theory.md).

Other covariance:
[`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)

## Examples

``` r
data(prek_sim, package = "sitemix")
est <- sm_estimate(
  subset(prek_sim, year == 2024),
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
as.matrix(v1)
#>              frpm         snap
#> frpm  0.010973937 -0.002743484
#> snap -0.002743484  0.019204390
```
