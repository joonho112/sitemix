# Estimate site-year proportions and standard errors

`sm_estimate()` estimates proportions and standard errors for each site
and year. It accepts individual student rows, sufficient counts, or
published aggregates. Choose `family = "binomial"` for one binary
indicator, `"multivariate"` for overlapping binary indicators, or
`"multinomial"` for mutually exclusive categories.

For counts or published aggregates,
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
and
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
set the input type for you. You can also use `from_counts = TRUE` or
`from_aggregates = TRUE` here. The result is a `sitemix_estimates`
tibble. Use
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
to check the estimates and their uncertainty before further analysis.

## Usage

``` r
sm_estimate(
  data,
  family,
  indicator = NULL,
  indicators = NULL,
  id_cols = c("site_id", "year"),
  vst = c("arcsine", "logit", "none"),
  boundary_method = c("wilson_floor", "agresti_coull", "none"),
  bias_correction = NULL,
  vjt = FALSE,
  min_n = 10L,
  accountability_n = 30L,
  fpc = NULL,
  anscombe = FALSE,
  from_counts = FALSE,
  na_action = c("drop_rows", "error"),
  description = NULL,
  from_aggregates = FALSE,
  aggregate_case = c("auto", "D0", "D1"),
  framing = NA_character_,
  sampling_relation = c("unknown", "same_units", "different_units"),
  suppression = c("drop", "upper_bound"),
  suppression_col = NULL,
  suppression_flag_value = "",
  suppression_when = NULL,
  suppressed_theta_hat = 0.5,
  suppression_sensitivity_acknowledge = FALSE,
  suppressed_n_strategy = c("observed_n", "worst_case_bound"),
  suppressed_n_bound = NULL,
  numerator_col = NULL,
  denominator_col = NULL,
  indicator_col = NULL,
  subgroup_col = NULL
)
```

## Arguments

- data:

  A data frame or tibble containing site and year columns named by
  `id_cols`. Student rows need the column named by `indicator`, or the
  binary columns named by `indicators`. Counts input needs `n_jt` and
  family-specific `c_jt_*` columns. Published aggregates may use the
  standard aggregate column names or the column mappings below; see
  [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md).

- family:

  A single string: `"binomial"`, `"multivariate"`, or `"multinomial"`.
  Required; omitting it raises `sitemix_error_invalid_family`. Published
  aggregate input supports only `"binomial"` and `"multivariate"`.

- indicator:

  A single column name, or `NULL` (default). Required for
  single-indicator student/count input (A) and categorical student input
  (C). Binary columns must be logical or numeric 0/1; categorical
  columns must be factors or character vectors. For long-form aggregate
  input, this argument supplies or replaces the indicator label; `NULL`
  keeps the labels already in the data.

- indicators:

  A character vector, or `NULL` (default). Names of overlapping binary
  indicators for B. For multinomial counts (C), gives the category order
  of the `c_jt_*` columns. For wide-form D1 aggregates, gives the
  marginal indicator names.

- id_cols:

  A character vector of length two, giving the site and year column
  names in that order. Defaults to `c("site_id", "year")`.

- vst:

  A single string giving the transformation used for `theta_hat` and
  `se`: `"arcsine"` (default), `"logit"`, or `"none"`. The returned
  `estimate_scale` column records the chosen scale.

- boundary_method:

  A single string specifying how to calculate uncertainty when \\C\_{jt}
  \in \\0, n\_{jt}\\\\: `"wilson_floor"` (default), `"agresti_coull"`,
  or `"none"`. Boundary rows have `flag_zero_cell = TRUE`. The Wilson
  and Agresti–Coull options replace boundary uncertainty with a
  surrogate; they do not move the observed raw proportion away from 0
  or 1. Logit output is unavailable at these boundaries.

- bias_correction:

  `NULL` (default) or `"binomial_bc"`. The latter uses the n-1
  correction for interior scalar variances and propagates it to arcsine
  or logit standard errors by the delta method. B/C covariance-matrix
  correction is handled separately. This option cannot be combined with
  `anscombe = TRUE`.

- vjt:

  A single logical value. If `TRUE`, include within-site covariance
  matrices as a `V` list-column of
  [`sm_vcov()`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
  objects. Defaults to `FALSE`. Use it for analyses involving several
  indicators together. Agresti–Coull boundary uncertainty is not
  supported for covariance-matrix output.

- min_n:

  A positive whole number. Rows with `n < min_n` receive
  `flag_small_n = TRUE`. Defaults to `10L`.

- accountability_n:

  A positive whole number. Rows with `n < accountability_n` receive
  `flag_below_accountability = TRUE`. Defaults to `30L`.

- fpc:

  Positive whole-number population sizes, or `NULL` (default). Supply
  the population size \\N\\ for simple random sampling without
  replacement (SRSWOR). A single value is used for all site-year groups.
  A vector must align with input rows and be constant within each
  `id_cols` group. Each retained group requires \\N \ge n\\; equality
  represents a census with zero sampling uncertainty. This argument is a
  population size, not a variance multiplier. Aggregate inputs
  containing any suppressed rows, including retained missing rows and
  sensitivity rows, cannot currently use `fpc`.

- anscombe:

  A single logical value. If `TRUE`, apply the Anscombe correction shown
  in *Details*. Defaults to `FALSE`. Requires `vst = "arcsine"` and
  cannot be combined with `boundary_method = "agresti_coull"` or
  `bias_correction = "binomial_bc"`.

- from_counts:

  A single logical value. If `TRUE`, treat `data` as one row of
  sufficient counts per site-year. Defaults to `FALSE`; cannot be
  combined with `from_aggregates = TRUE`.
  [`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
  sets this argument automatically.

- na_action:

  A single string describing how to handle missing indicator values:
  `"drop_rows"` (default) or `"error"`. The latter raises
  `sitemix_error_input_missing` on any `NA`.

- description:

  A single string, or `NULL` (default). An optional label stored in the
  returned object's `description` attribute.

- from_aggregates:

  A single logical value. If `TRUE`, treat `data` as published
  aggregates (D0/D1). Defaults to `FALSE`; cannot be combined with
  `from_counts = TRUE`.
  [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
  sets this argument automatically.

- aggregate_case:

  A single string: `"auto"` (default, inferred from the indicators),
  `"D0"`, or `"D1"`. Valid values have no effect outside aggregate
  input; invalid values always raise an error.

- framing:

  `NA_character_` (default), `"subgroup_as_site"`, or
  `"subgroup_as_indicator"`. Describes how aggregate subgroups are
  represented. Reshape the data first with
  [`sm_pivot_subgroups_to_sites()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md)
  or
  [`sm_pivot_subgroups_to_indicators()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md);
  this argument does not reshape raw subgroup rows. Invalid values raise
  `sitemix_error_invalid_framing`.

- sampling_relation:

  A single string describing the observational units behind D1
  marginals: `"unknown"` (default), `"same_units"`, or
  `"different_units"`. These produce `d1_regime` labels `"unknown"`,
  `"D1a"`, and `"D1b"`, respectively. Equal denominators do not imply
  the same units. Valid values have no effect outside D1; invalid values
  raise `sitemix_error_invalid_sampling_relation`.

- suppression:

  A single string for handling publisher-suppressed aggregate rows:
  `"drop"` (default) or `"upper_bound"`. `"drop"` retains a row with
  missing estimates and standard errors. `"upper_bound"` adds separately
  labeled Bernoulli variance-sensitivity fields after explicit
  acknowledgement; it leaves the estimate and SE columns missing. These
  rows cannot supply ordinary `V` or Fréchet inputs. When suppressed
  rows are present, `"upper_bound"` requires `vst = "arcsine"` and
  `anscombe = FALSE`. Valid values have no effect for other input types;
  invalid values always raise an error.

- suppression_col:

  A single column name, or `NULL` (default). Names the publisher
  suppression flag. With `NULL`, an existing `suppression_flag` column
  is used if present; missing numerators can also identify suppressed
  rows.

- suppression_flag_value:

  Values in the suppression flag column that indicate suppression.
  Defaults to `""` (the empty string). For character flags, explicitly
  supply the publisher's marker (for example `"*"`); a blank is
  otherwise treated as suppressed. Logical flags use `TRUE` irrespective
  of this argument.

- suppression_when:

  A function, or `NULL` (default). An optional predicate returning one
  logical value per aggregate row; it overrides the usual flag and
  missing-numerator detection.

- suppressed_theta_hat:

  A finite numeric value strictly between 0 and 1; defaults to `0.5`.
  Used only for `suppression = "upper_bound"`. When suppressed rows are
  present, it must be `0.5`, which maximizes Bernoulli variance. The
  value is stored as `sensitivity_probability`; it never replaces an
  observed or missing point estimate.

- suppression_sensitivity_acknowledge:

  A single logical value, defaulting to `FALSE`. Must be `TRUE` when
  `suppression = "upper_bound"` encounters suppressed rows. The
  additional fields describe a variance-sensitivity scenario; they are
  not estimated values or ordinary covariance inputs.

- suppressed_n_strategy:

  A single string: `"observed_n"` (default) or `"worst_case_bound"`.
  Published denominators are always retained as `n`, `n_eff`, and the
  sensitivity denominator. The latter strategy records
  `suppressed_n_bound` as `n` and `n_eff` only when the denominator is
  hidden. A hidden denominator supplies no numeric sensitivity variance;
  a recorded bound does not identify the actual sample size.

- suppressed_n_bound:

  A positive whole number, or `NULL` (default). Required for suppressed
  upper-bound rows with hidden denominators and
  `suppressed_n_strategy = "worst_case_bound"`; must be no larger than
  `min_n`. This recorded bound never replaces a published denominator.
  Hidden-denominator rows retain missing sensitivity variances.

- numerator_col:

  A single column name, or `NULL` (default). Maps an aggregate numerator
  to `c_jt`. With `NULL`, long-form input uses an existing `c_jt`
  column; wide-form input uses `c_jt_*` columns.

- denominator_col:

  A single column name, or `NULL` (default). Maps an aggregate
  denominator to `n_jt`. With `NULL`, use the standard denominator
  columns described in
  [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md).

- indicator_col:

  A single column name, or `NULL` (default). Maps long-form aggregate
  indicator labels to `indicator`; `NULL` uses that name if present.

- subgroup_col:

  A single column name, or `NULL` (default). Maps aggregate subgroup
  labels to `subgroup`; `NULL` uses that name if present.

## Value

A `sitemix_estimates` tibble with one row per site-year-indicator. The
main columns are:

- `site_id`:

  Character site identifier.

- `year`:

  Integer year.

- `indicator`:

  Character indicator or category name.

- `theta_raw`:

  Numeric in \\\[0, 1\]\\; the observed proportion \\\hat\pi\_{jt} =
  C\_{jt}/n\_{jt}\\. Missing for suppressed rows.

- `theta_hat`:

  Numeric; the point estimate on the scale named by the `estimate_scale`
  column.

- `se_raw`:

  Numeric; the raw-scale standard error before any VST. Finite and
  non-negative when estimable; `NA` for intentionally suppressed/drop
  rows.

- `se`:

  Numeric; the scalar standard error on the `estimate_scale`. Finite and
  non-negative when estimable; `NA` for intentionally suppressed/drop
  rows.

- `n`:

  Positive integer denominator. A hidden-denominator suppression row may
  instead record `suppressed_n_bound`; that value does not establish an
  observed sample size.

- `n_eff`:

  Numeric denominator used for transformed standard errors: `n + 1/2`
  with Anscombe, otherwise `n`. This is distinct from the `n + 3/4` in
  the Anscombe point transformation. Finite-population correction leaves
  it unchanged.

- `estimate_scale`:

  Character scalar; the scale of `theta_hat` and `se`. One of `"none"`,
  `"arcsine"`, `"arcsine_anscombe"`, or `"logit"`.

- `transform`:

  Character scalar; the VST applied (typically equals `estimate_scale`).

- `var_method`:

  Character string naming the standard-error calculation; possible
  values are listed in *Details*.

- `flag_small_n`:

  Logical; `TRUE` if \\n\_{jt} \<\\ `min_n`.

- `flag_zero_cell`:

  Logical; `TRUE` at an identified boundary \\C\_{jt} \in \\0,
  n\_{jt}\\\\, `FALSE` for an observed interior proportion, and `NA`
  when publisher suppression hides the numerator.

- `input_mode`:

  Character scalar; one of `"student_level"`, `"counts_full_suff"`, or
  `"aggregate"`, recording the input type.

- `flag_suppressed`:

  Logical; `TRUE` if the site-year was suppressed by the publisher (Tier
  1 in the reporting scheme). Always `FALSE` when
  `from_aggregates = FALSE`.

- `framing`:

  Character scalar; the aggregate subgroup framing label
  (`NA_character_` for direct D0 / non-aggregate paths).

- `flag_below_accountability`:

  Logical; `TRUE` if \\n\_{jt} \<\\ `accountability_n`.

- **Suppression information**:

  Optional columns returned as one set when an aggregate input contains
  Tier-1 rows:

  - Status: `estimate_status`.

  - Values: `sensitivity_probability`, `sensitivity_var_raw`,
    `sensitivity_var`, and `sensitivity_n`.

  - Method and acknowledgement: `sensitivity_method` and
    `sensitivity_acknowledged`.

  `estimate_status` distinguishes identified rows, retained
  suppressed-missing rows, and non-identified sensitivity rows.
  Observed-denominator sensitivity uses probability 0.5 and variance
  \\0.25/n\\; hidden-denominator rows leave the numeric sensitivity
  variance and denominator missing.

- **Finite-population information**:

  Optional structured SRSWOR columns returned together when `fpc` is
  supplied:

  - Design: `population_size`, `sampling_fraction`, and
    `sampling_design`.

  - Conventional FPC multipliers: `fpc_variance_multiplier` and
    `fpc_se_multiplier`.

  - Applied multipliers: `variance_multiplier_applied` and
    `se_multiplier_applied`, with `variance_rule`.

  `n_eff` remains unchanged; `variance_rule` distinguishes plug-in from
  design-corrected `binomial_bc` uncertainty. The `fpc_*` columns record
  the conventional SRSWOR \\q\\, while `*_applied` records the
  multiplier actually applied relative to the same infinite-population
  variance rule.

- `V`:

  Optional list-column of
  [`sm_vcov`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
  objects. Present when `vjt = TRUE`. For Scenarios A / D0 each element
  is \\1 \times 1\\; for B / C / D1 the dimension is the indicator
  count.

- `K`:

  Optional integer column; present only alongside `V` for Scenarios B /
  C / D1 when `vjt = TRUE`, recording the number of indicators or
  categories.

The returned object also carries the following object-level attributes:

- `description`:

  Character scalar or `NULL`; verbatim copy of the `description`
  argument.

- `family`:

  Character scalar or `NULL`; the estimation family.

- `sitemix_role`:

  Character scalar; the output role, normally `"summary_uncertainty"`.

- `aggregate_case`:

  For aggregate input, the resolved `"D0"` or `"D1"` scenario.

- `sampling_relation`:

  For D1, whether the caller stated that the indicators describe the
  same or different units.

- `denominator_pattern`:

  For D1, `"common"`, `"varying"`, `"incomplete"`, or `"mixed"`,
  summarized across site-year groups.

- `d1_regime`:

  For D1, the object-level regime summary derived from sampling relation
  and denominator pattern.

- `d1_regime_by_group`:

  For D1, a per-group table that records indicator count, sampling
  relation, denominator pattern, and the resulting regime label.

- `suppression`:

  For D0/D1, suppression detection, whether denominators are observed,
  and any acknowledged sensitivity analysis.

- `smoothing`:

  After experimental smoothing, the target, method, fit status, and
  relationship to the covariance matrices.

## Details

**Input types.** The documentation uses the following scenario labels
for the supported combinations of input and family:

|                      |            |                |               |
|----------------------|------------|----------------|---------------|
| Input                | `binomial` | `multivariate` | `multinomial` |
| Student rows         | A          | B              | C             |
| Sufficient counts    | A          | B              | C             |
| Published aggregates | D0         | D1             | Not supported |

For student rows, use `indicator` to name a single binary column (A) or
a factor/character column of categories (C). Use `indicators` to name
several binary columns that can overlap (B).

Counts input for overlapping indicators requires each marginal count and
every pairwise co-occurrence count, in the order specified by
`indicators`. When `vjt = TRUE`, the covariance calculation checks
whether these counts can come from a common sample. It supports two or
three indicators; covariance requests with four or more indicators
require student rows. Counts input with four or more indicators is
accepted when `vjt = FALSE`.

Published aggregates use numerator/denominator pairs (D0) or several
marginal proportions (D1). D1 covariance assumes working independence;
it does not recover dependence from the marginals. The
`sampling_relation` argument records whether the indicators refer to the
same observational units. Equal denominators alone do not establish
this. See
[`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
for dependence sensitivity. Published marginals do not supply a
multinomial composition; use complete category counts for C instead. An
aggregate request with `family = "multinomial"` raises
`sitemix_error_ambiguous_dispatch`.

`from_counts` and `from_aggregates` cannot both be `TRUE`; this raises
`sitemix_error_input_path_conflict`. Omitting `family` raises
`sitemix_error_invalid_family`. Argument values are checked even when
they do not affect the selected input type. Valid unused values have no
effect; invalid values raise an error. Required data columns and
combinations of arguments are checked for the input type being used.

**Transformations.** By default, `theta_hat` and `se` use the arcsine
square-root scale (`vst = "arcsine"`). Without bias correction, its
first-order standard error, before any finite-population correction, is
\$\$\mathrm{SE}(\hat\theta\_{jt}) = 1 / (2\sqrt{n\_{jt}}).\$\$ With
`bias_correction = "binomial_bc"`, interior arcsine and logit rows
instead propagate the n-1-corrected raw variance through the
corresponding delta method. Use `vst = "logit"` for the logit scale or
`vst = "none"` for proportions. The `estimate_scale` column identifies
the scale of the returned estimates and standard errors. Any `V` matrix
has its own `vcov_scale`, which can differ; see *Scale conventions*
below.

With `anscombe = TRUE`, the transformation is
\$\$\arcsin\sqrt{(C\_{jt}+3/8)/(n\_{jt}+3/4)}\$\$ This option requires
`vst = "arcsine"`; other transforms raise
`sitemix_error_anscombe_requires_arcsine`. For derivations, see
[`vignette("m2-scalar-se-binomial")`](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md).

**Standard-error methods.** The `var_method` column names the
calculation used for each row:

- Arcsine: `"arcsine_vst"`, `"arcsine_anscombe"`, or
  `"arcsine_delta_binomial_bc"`.

- Logit: `"logit_delta"` or `"logit_delta_binomial_bc"`.

- Untransformed: `"binomial"` or `"binomial_bc"`.

- Boundary standard errors: `"wilson_boundary_surrogate"` or
  `"agresti_coull_boundary_surrogate"`.

- Suppressed rows: `"suppressed_drop"` or `"suppression_sensitivity"`.

[`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
records experimental alternatives with `" + gvf_smooth_loglinear"` or
`" + gvf_smooth_gam"` in `var_method_smoothed`. They replace
`var_method` only when an allowed overwrite is requested. Older
`" + fh_smooth_*"` labels can still be read. The method used for a
covariance matrix is recorded separately in its `vcov_method` field; see
[`sm_vcov()`](https://joonho112.github.io/sitemix/reference/sm_vcov.md).

## Using the estimates in other analyses

Run
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
before selecting or converting the results.
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) changes
their format but does not check whether they are suitable for an
analysis. Keep site/year/indicator identifiers, `theta_hat`, `se`,
`estimate_scale`, `var_method`, reporting flags, and any
`estimate_status` or `sensitivity_*` columns together so that the
meaning of each row remains clear.

Ordinary inverse-variance weighting requires identified finite estimates
and finite, strictly positive standard errors. Census rows with zero SE,
suppressed rows, and sensitivity scenarios should remain available for
inspection, but should not receive ordinary inverse-variance weights.
`sensitivity_var` is not a replacement for `se`. If you choose a
smoothed SE, retain `var_method_smoothed` and the original SE/method
pair as well. For joint analyses, keep complete site-year indicator
groups and check each matrix's `indicator_order`, dimnames, and
`vcov_scale`. See
[`vignette("a8-downstream-workflows", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md).

## Scale conventions

With a single binary indicator (A/D0), `V` is a \\1 \times 1\\ matrix on
the row's estimate scale. For example, arcsine output has
`vcov_scale = "arcsine_delta"`. D1 working-independence matrices also
follow the row's estimate scale.

For overlapping indicators (B) and mutually exclusive categories (C),
`V` stays on the raw probability scale even when the row estimates are
transformed. Thus `sqrt(diag(V))` should not be compared with
transformed `se`. sitemix does not transform B/C matrices to match a
different row scale. Always inspect `vcov_scale` before using a matrix
in a joint analysis.

The `diag_contract` field identifies the corresponding scalar variance:
A/D0/D1 matrices match `se^2`; B/C raw matrices usually match
`se_raw^2`. For multinomial boundary cells, scalar boundary surrogates
intentionally differ from the simplex diagonal. With `fpc`, the matrix
records the population size, sampling fraction, conventional FPC,
applied multiplier, and variance rule. It does not change `n_eff`. See
[`sm_vcov()`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
for these fields and
[`vignette("m8-output-contract", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/m8-output-contract.md)
for the conditions needed to use the returned estimates together.

## References

Agresti, A. & Coull, B. A. (1998). Approximate is better than "exact"
for interval estimation of binomial proportions. *The American
Statistician*, **52**(2), 119–126.
[doi:10.1080/00031305.1998.10480550](https://doi.org/10.1080/00031305.1998.10480550)

Anscombe, F. J. (1948). The transformation of Poisson, binomial and
negative-binomial data. *Biometrika*, **35**(3/4), 246–254.
[doi:10.1093/biomet/35.3-4.246](https://doi.org/10.1093/biomet/35.3-4.246)

Wilson, E. B. (1927). Probable inference, the law of succession, and
statistical inference. *Journal of the American Statistical
Association*, **22**(158), 209–212.

## See also

[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
for sufficient counts;
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
for published aggregates;
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
for checking uncertainty;
[`sm_vcov()`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
for covariance matrices and their method and scale fields;
[`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
for experimental variance smoothing;
[`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
for sensitivity to unknown D1 dependence;
[`vignette("a1-getting-started", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/a1-getting-started.md)
for a first example;
[`vignette("m1-statistical-foundations", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md)
for sampling uncertainty and notation;
[`vignette("m2-scalar-se-binomial", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
for standard-error calculations.

Other estimation:
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md),
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)

## Examples

``` r
data(prek_sim, package = "sitemix")

# Estimate the FRPM proportion at each site in 2024.
est_a <- sm_estimate(
  subset(prek_sim, year == 2024),
  family    = "binomial",
  indicator = "frpm",
  vst       = "arcsine"
)
head(est_a, 5)
#> sitemix_estimates: 5 rows x 18 columns | family=binomial | role=summary_uncertainty
#> groups=5 sites=5 years=1 indicators=1 V=FALSE K=FALSE
#> # A tibble: 5 × 18
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 frpm          0.111     0.340  0.105 0.167     9     9
#> 2 S002     2024 frpm          0.8       1.11   0.126 0.158    10    10
#> 3 S003     2024 frpm          0.5       0.785  0.177 0.177     8     8
#> 4 S004     2024 frpm          0.429     0.714  0.132 0.134    14    14
#> 5 S005     2024 frpm          0.875     1.21   0.117 0.177     8     8
#> # ℹ 9 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>
unique(est_a$estimate_scale) # theta_hat and se use the arcsine scale
#> [1] "arcsine"

# Estimate two overlapping indicators and retain their covariance.
est_b <- sm_estimate(
  subset(prek_sim, year == 2024),
  family     = "multivariate",
  indicators = c("frpm", "snap"),
  vst        = "arcsine",
  vjt        = TRUE
)
head(est_b, 4)
#> sitemix_estimates: 4 rows x 20 columns | family=multivariate | role=summary_uncertainty
#> groups=2 sites=2 years=1 indicators=2 V=TRUE K=TRUE
#> # A tibble: 4 × 20
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 frpm          0.111     0.340  0.105 0.167     9     9
#> 2 S001     2024 snap          0.222     0.491  0.139 0.167     9     9
#> 3 S002     2024 frpm          0.8       1.11   0.126 0.158    10    10
#> 4 S002     2024 snap          0.7       0.991  0.145 0.158    10    10
#> # ℹ 11 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>,
#> #   V <list>, K <int>
est_b$V[[1L]]$vcov_scale   # V is on the raw probability scale
#> [1] "raw"
est_b$V[[1L]]$vcov_method  # "sur"
#> [1] "sur"
```
