# Estimate site-year rates and standard errors

`sm_estimate()` is the main entry point for producing site-year point
estimates and standard errors for downstream analyses that propagate
sampling uncertainty. It accepts three input shapes – student rows,
sufficient counts, or published aggregates – and five estimation
scenarios (A/B/C/D0/D1) selected by a combination of `family`,
`from_counts`, and `from_aggregates`. For the two narrow input shapes,
prefer the wrappers listed under *See Also*; for student rows, call
`sm_estimate()` directly. The output is a `sitemix_estimates` tibble;
pass it to
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
to audit scalar and joint sampling uncertainty before downstream use.

The v0.2 public signature is frozen. None of the current arguments is
deprecated: direct `from_counts` / `from_aggregates` dispatch and the
`vjt` covariance opt-in remain supported compatibility surfaces. The
wrappers are the recommended entry points for counts and published
aggregates because they lock the input path explicitly.

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

  A data frame or tibble. Required columns depend on the dispatched
  Scenario: `(site_id, year, indicator)` for Scenario A from student
  rows; `(site_id, year, indicators)` for Scenario B;
  `(site_id, year, indicator)` as a factor or character column for
  Scenario C from student rows. Sufficient counts require `n_jt` plus
  family-specific `c_jt_*` columns. Published aggregates use numerator
  and denominator columns named by `numerator_col` and
  `denominator_col`.

- family:

  Character scalar. Estimation family selecting the dispatched engine.
  One of `"binomial"`, `"multivariate"`, or `"multinomial"`. No default;
  omission raises `sitemix_error_invalid_family`.

- indicator:

  Character scalar or `NULL` (default `NULL`). Name of the single
  indicator column in `data`. Required for Scenarios A, C, and D0. For
  Scenario A the column must be logical or 0/1 numeric; for Scenario C
  the column must be a factor or character.

- indicators:

  Character vector or `NULL` (default `NULL`). For Scenario B, the
  column names of overlapping binary indicators whose joint moments are
  estimated. For Scenario C with `from_counts = TRUE` (including
  [`sm_estimate_from_counts`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)),
  the explicit category order applied to the supplied `c_jt_*` count
  columns. For Scenario D1, the marginal column names.

- id_cols:

  Character vector of length two. Column names identifying site and
  year, in that order. Defaults to `c("site_id", "year")`.

- vst:

  Character scalar. Variance-stabilizing transform applied to
  `theta_hat` and `se`. One of `"arcsine"` (default), `"logit"`, or
  `"none"`. Sets the row-level `estimate_scale` column of the returned
  tibble.

- boundary_method:

  Character scalar. Policy applied to boundary cells where \\C\_{jt} \in
  \\0, n\_{jt}\\\\. One of `"wilson_floor"` (default),
  `"agresti_coull"`, or `"none"`. Boundary rows are flagged via
  `flag_zero_cell` regardless of the selected policy. Wilson and
  Agresti–Coull values regularize uncertainty only: `theta_raw` and
  `theta_hat` retain the observed \\C\_{jt}/n\_{jt}\\ point on raw
  output. The public option names are retained for compatibility; row
  provenance explicitly labels the resulting values as boundary
  surrogates.

- bias_correction:

  Character scalar or `NULL` (default `NULL`). When `"binomial_bc"`,
  applies the binomial n-1 correction to legal interior row-level scalar
  SEs. Raw rows carry `var_method = "binomial_bc"`; arcsine and logit
  rows carry scale-specific delta-method provenance. Scenario B/C
  covariance-matrix correction is handled separately. The option is
  incompatible with `anscombe = TRUE`. `NULL` disables the correction.

- vjt:

  Logical scalar. If `TRUE`, attach the within-site covariance as a
  [`sm_vcov`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
  list-column `V`; if `FALSE` (default), omit it. Use it when a
  downstream analysis needs joint covariance (Scenarios B / C / D1).
  Agresti–Coull boundary regularization with `vjt = TRUE` is not
  supported. This stable opt-in is not deprecated.

- min_n:

  Positive integer scalar. Threshold for the `flag_small_n` output
  column; rows with \\n\_{jt} \<\\ `min_n` are flagged. Defaults to
  `10L`.

- accountability_n:

  Positive integer scalar. Threshold for the `flag_below_accountability`
  output column; rows with \\n\_{jt} \<\\ `accountability_n` are
  flagged. Defaults to `30L`.

- fpc:

  Positive whole-number population size(s) or `NULL` (default `NULL`).
  This declares a fixed finite population of size \\N\\ sampled by
  simple random sampling without replacement (SRSWOR), not a generic
  correction factor. A scalar is recycled across site-year groups. A
  vector must align with input rows and be constant within each
  `id_cols` group. Every retained group requires \\N \ge n\\; equality
  is a census with zero sampling uncertainty. FPC is not allowed for
  synthetic suppression-sensitivity rows. The current aggregate
  implementation additionally fails closed when any Tier-1 row is
  present, including retained `suppressed_missing` rows, because
  object-wide FPC provenance requires fully observed aggregate rows.

- anscombe:

  Logical scalar. If `TRUE`, applies the Anscombe arcsine correction
  shown in *Details*; the resulting rows carry
  `var_method = "arcsine_anscombe"`. Defaults to `FALSE`. It requires
  `vst = "arcsine"` and is incompatible with
  `boundary_method = "agresti_coull"` and
  `bias_correction = "binomial_bc"`.

- from_counts:

  Logical scalar. If `TRUE`, treat `data` as sufficient counts (one row
  per site-year with `c_jt_*` columns) rather than student rows.
  Mutually exclusive with `from_aggregates`. Defaults to `FALSE`. This
  direct-dispatch switch remains supported; the counts wrapper is
  preferred for new code.

- na_action:

  Character scalar. Missing-value policy for the indicator column(s).
  One of `"drop_rows"` (default) or `"error"`. `"error"` raises
  `sitemix_error_input_missing` on any `NA`.

- description:

  Character scalar or `NULL` (default `NULL`). Optional human-readable
  label preserved in the `description` attribute of the returned tibble.

- from_aggregates:

  Logical scalar. If `TRUE`, treat `data` as published aggregate input
  (Scenarios D0 / D1). Mutually exclusive with `from_counts`. Defaults
  to `FALSE`. This direct-dispatch switch remains supported;
  [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
  is preferred for new aggregate-input code.

- aggregate_case:

  Aggregate case: `"auto"` (infer; default), `"D0"`, or `"D1"`. Ignored
  off the aggregate path; invalid values fail before dispatch.

- framing:

  Aggregate subgroup framing. Character scalar or `NA_character_`
  (default; direct D0 framing). For Framing X or Framing Y, pivot first
  with the corresponding subgroup helper listed under *See Also*. Valid
  inactive values are accepted silently; invalid values always raise
  `sitemix_error_invalid_framing`.

- sampling_relation:

  Character scalar describing D1 sampling-unit provenance. One of
  `"unknown"` (default), `"same_units"`, or `"different_units"`. These
  map to object-level `d1_regime` values `"unknown"`, `"D1a"`, and
  `"D1b"`, respectively. Equal denominators are recorded separately and
  never establish common observational units. Invalid values raise
  `sitemix_error_invalid_sampling_relation`. A valid value outside D1 is
  accepted silently and has no effect.

- suppression:

  Character scalar. Aggregate Tier-1 handling mode. One of `"drop"`
  (default; retain an unavailable audit row with canonical point/SE
  columns missing) or `"upper_bound"` (legacy option for an explicitly
  acknowledged, separated worst-case Bernoulli variance sensitivity).
  The latter never writes a synthetic point or SE to canonical columns
  and is excluded from ordinary `V` and Fréchet inputs. A valid value is
  ignored when `from_aggregates = FALSE`; invalid values are rejected
  before dispatch.

- suppression_col:

  Character scalar or `NULL` (default `NULL`). Name of the publisher
  suppression flag column in `data`. `NULL` disables flag-based
  detection.

- suppression_flag_value:

  Value or vector of values marking Tier-1 suppression in
  `suppression_col`. Defaults to `""` (the empty string).

- suppression_when:

  Function or `NULL` (default `NULL`). Optional predicate overriding
  flag-based detection.

- suppressed_theta_hat:

  Numeric scalar in \\\[0, 1\]\\. Legacy compatibility name for the
  raw-scale probability used only to maximize Bernoulli variance under
  `suppression = "upper_bound"`. Any finite interior value is
  syntactically valid, but an active upper-bound sensitivity with a
  suppressed row requires `0.5`. It is stored in
  `sensitivity_probability` and never substituted into canonical
  estimate columns. The argument is retained without a deprecation
  warning or removal schedule in v0.2.

- suppression_sensitivity_acknowledge:

  Logical scalar. Must be `TRUE` when `suppression = "upper_bound"`
  actually encounters suppressed rows. This explicitly acknowledges that
  the returned separated fields are a non-identified
  variance-sensitivity scenario, not an estimate or an ordinary
  covariance input.

- suppressed_n_strategy:

  Character scalar. Denominator strategy for hidden suppressed rows. One
  of `"observed_n"` (default) or legacy `"worst_case_bound"` (record
  `suppressed_n_bound` as an operational placeholder). Because an upper
  bound on an unknown denominator is not a conservative SE denominator,
  hidden-denominator sensitivity rows make no numeric variance claim.

- suppressed_n_bound:

  Positive integer scalar or `NULL` (default). Legacy audit placeholder
  for the worst-case strategy; never a variance denominator.

- numerator_col:

  Character scalar or `NULL` (default `NULL`). Name of the aggregate
  numerator column. Required for Scenario D0 inputs; ignored otherwise.

- denominator_col:

  Character scalar or `NULL` (default `NULL`). Name of the aggregate
  denominator column. Required for Scenarios D0 / D1; ignored otherwise.

- indicator_col:

  Character scalar or `NULL` (default `NULL`). Name of the long-form
  indicator-key column in aggregate inputs (one row per
  site-year-indicator).

- subgroup_col:

  Character scalar or `NULL` (default `NULL`). Name of the subgroup-key
  column in aggregate D1 inputs.

## Value

A `sitemix_estimates` tibble with one row per site-year-indicator.
Columns (the order follows the constructor in `R/output-assembly.R`):

- `site_id`:

  Character site identifier; the first member of the keyed pair
  `(site_id, year)`.

- `year`:

  Integer year identifier; the second member of the keyed pair.

- `indicator`:

  Character scalar; the indicator name passed via `indicator =`
  (Scenarios A, D0) or one of the indicator-component names (Scenarios
  B, C, D1).

- `theta_raw`:

  Numeric in \\\[0, 1\]\\; the raw-scale proportion \\\hat\pi\_{jt} =
  C\_{jt}/n\_{jt}\\.

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

  Positive integer; the site-year denominator \\n\_{jt}\\.

- `n_eff`:

  Numeric; transform-denominator provenance. Finite-population sampling
  does not change this value.

- `estimate_scale`:

  Character scalar; the scale of `theta_hat` and `se`. One of `"none"`,
  `"arcsine"`, `"arcsine_anscombe"`, or `"logit"`.

- `transform`:

  Character scalar; the VST applied (typically equals `estimate_scale`).

- `var_method`:

  Character scalar; the row-level SE provenance, drawn from the
  implemented lexicon described in *Details*.

- `flag_small_n`:

  Logical; `TRUE` iff \\n\_{jt} \<\\ `min_n`.

- `flag_zero_cell`:

  Logical; `TRUE` at an identified boundary \\C\_{jt} \in \\0,
  n\_{jt}\\\\, `FALSE` in an identified interior, and `NA` when
  publisher suppression hides the numerator.

- `input_mode`:

  Character scalar; one of `"student_level"`, `"counts_full_suff"`, or
  `"aggregate"`, recording the dispatched input pathway.

- `flag_suppressed`:

  Logical; `TRUE` iff the site-year was suppressed by the publisher
  (Tier 1 in the three-tier framework). Always `FALSE` when
  `from_aggregates = FALSE`.

- `framing`:

  Character scalar; the aggregate subgroup framing label
  (`NA_character_` for direct D0 / non-aggregate paths).

- `flag_below_accountability`:

  Logical; `TRUE` iff \\n\_{jt} \<\\ `accountability_n`.

- **Suppression provenance**:

  Optional columns emitted as one set when an aggregate input contains
  Tier-1 rows:

  - Status: `estimate_status`.

  - Values: `sensitivity_probability`, `sensitivity_var_raw`,
    `sensitivity_var`, and `sensitivity_n`.

  - Provenance: `sensitivity_method` and `sensitivity_acknowledged`.

  `estimate_status` distinguishes identified rows, retained
  suppressed-missing rows, and non-identified sensitivity rows.
  Observed-denominator sensitivity uses probability 0.5 and variance
  \\0.25/n\\; hidden-denominator rows leave the numeric sensitivity
  variance and denominator missing.

- **Finite-population provenance**:

  Optional structured SRSWOR columns emitted together when `fpc` is
  supplied:

  - Design: `population_size`, `sampling_fraction`, and
    `sampling_design`.

  - Canonical multipliers: `fpc_variance_multiplier` and
    `fpc_se_multiplier`.

  - Applied multipliers: `variance_multiplier_applied` and
    `se_multiplier_applied`, with `variance_rule`.

  `n_eff` remains unchanged; `variance_rule` distinguishes plug-in from
  design-corrected `binomial_bc` uncertainty. The `fpc_*` columns record
  the canonical SRSWOR \\q\\, while `*_applied` records the multiplier
  actually applied relative to the same infinite-population variance
  rule.

- `V`:

  Optional list-column of
  [`sm_vcov`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
  objects. Present iff `vjt = TRUE`. For Scenarios A / D0 each element
  is \\1 \times 1\\; for B / C / D1 the dimension is the indicator
  count.

- `K`:

  Optional integer column; present only alongside `V` for Scenarios B /
  C / D1 when `vjt = TRUE`, recording the indicator-component count for
  the row.

The returned object also carries the following object-level attributes:

- `description`:

  Character scalar or `NULL`; verbatim copy of the `description`
  argument.

- `family`:

  Character scalar or `NULL`; the dispatched estimation family.

- `sitemix_role`:

  Character scalar; the output role, normally `"summary_uncertainty"`.

- `aggregate_case`:

  For aggregate input, the resolved `"D0"` or `"D1"` scenario.

- `sampling_relation`:

  For D1, the explicit sampling-unit provenance supplied by the caller.

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

  For D0/D1, suppression detection, denominator observability,
  acknowledgement, and sensitivity-role provenance.

- `smoothing`:

  When experimental append-only smoothing has been attempted, its
  target, method, fit status, and covariance-relation provenance.

## Details

**Scenario dispatch.** The function selects one of five engines from
`family`, `from_counts`, and `from_aggregates`:

|                      |            |                |               |
|----------------------|------------|----------------|---------------|
| Input shape          | `binomial` | `multivariate` | `multinomial` |
| Student rows         | A          | B              | C             |
| Sufficient counts    | A          | B              | C             |
| Published aggregates | D0         | D1             | rejected      |

Count-input Scenario B requires ordered marginal counts and every
pairwise co-occurrence count; the approved public path verifies joint
feasibility for \\K = 2\\ or \\K = 3\\ and fails closed for \\K \ge 4\\.
Published aggregate marginals do not identify a multinomial composition:
that cell raises `sitemix_error_ambiguous_dispatch`. Use sufficient
category counts for Scenario C instead. Anscombe is an arcsine-only
transform in every legal cell; `anscombe = TRUE` with a non-arcsine
`vst` raises `sitemix_error_anscombe_requires_arcsine`.

- **Scenario A – binomial**:

  One binary indicator per site-year. Input is student rows
  (`from_counts = FALSE`, `from_aggregates = FALSE`) or sufficient
  counts (`from_counts = TRUE`). Selected when `family = "binomial"` and
  `from_aggregates = FALSE`. `V` is \\1 \times 1\\.

- **Scenario B – multivariate**:

  Overlapping binary indicators per site-year with SUR-style covariance.
  Input is student rows with multiple logical or 0/1 numeric columns
  named in `indicators`, or sufficient counts containing the ordered
  marginal and pairwise co-occurrence columns. Selected when
  `family = "multivariate"` and `from_aggregates = FALSE`. `V` is \\K
  \times K\\.

- **Scenario C – multinomial**:

  Mutually exclusive categories summing to \\n\_{jt}\\. Input is student
  rows with one factor or character column named in `indicator`, or
  sufficient counts with `c_jt_*` columns when `from_counts = TRUE`.
  Selected when `family = "multinomial"` and `from_aggregates = FALSE`.
  `V` is simplex-structured with analytic rank \\S - 1\\, where \\S\\ is
  the number of categories with positive observed counts.

- **Scenario D0 – aggregate binomial**:

  Published numerator and denominator per site-year. Selected when
  `from_aggregates = TRUE` and `family = "binomial"`. `V` is \\1 \times
  1\\.

- **Scenario D1 – aggregate marginal**:

  Published marginal rates with working-independence covariance and
  optional Fréchet stress analysis. Selected when
  `from_aggregates = TRUE` and `family = "multivariate"`. With
  `vjt = TRUE`, `V` is diagonal (\\K \times K\\); see the [Fréchet
  diagnostic](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md).
  The `sampling_relation` argument, not denominator equality, determines
  whether the output is labeled D1a, D1b, or unknown.

**Mutual exclusion.** `from_counts = TRUE` and `from_aggregates = TRUE`
are mutually exclusive; supplying both raises a
`sitemix_error_input_path_conflict` condition. `family` is required;
omitting it raises `sitemix_error_invalid_family`.

**Inactive public controls.** Supplied path-specific controls are
validated for syntax before dispatch. A valid control that is inactive
for the selected input path is accepted silently and has no effect; an
invalid value raises its stable classed condition even when that path is
inactive. This preserves valid legacy calls without adding lifecycle
warnings while preventing dispatch-dependent validation surprises.
Data-column existence and cross-argument compatibility remain
active-path checks.

**Scale conventions.** On the default scale (`vst = "arcsine"`),
`theta_hat` is the arcsine-stabilized rate and, without bias correction,
`se` is its closed-form delta-method standard error
\$\$\mathrm{SE}(\hat\theta\_{jt}) \\=\\ 1 / (2\sqrt{n\_{jt}}).\$\$ With
`bias_correction = "binomial_bc"`, legal interior arcsine and logit rows
propagate the n-1 raw variance through the corresponding delta method.
Switch with `vst = "logit"` or `vst = "none"`; the row-level
`estimate_scale` column records the choice. The matrix scale of any `V`
list-column is recorded separately in each `sm_vcov` object's
`vcov_scale` field and *may differ* from `estimate_scale` under
Scenarios B and C. Derivations live in
[`vignette("m2-scalar-se-binomial")`](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md).
The Anscombe option uses
\$\$\arcsin\sqrt{(C\_{jt}+3/8)/(n\_{jt}+3/4)}\$\$

**var_method lexicon.** The `var_method` column records row-level SE
provenance from this implemented base lexicon:

- Arcsine values: `"arcsine_vst"` and `"arcsine_anscombe"`.

- Bias-corrected arcsine: `"arcsine_delta_binomial_bc"`.

- Logit values: `"logit_delta"` and `"logit_delta_binomial_bc"`.

- Raw-scale values: `"binomial"` and `"binomial_bc"`.

- Boundary values: `"wilson_boundary_surrogate"` and
  `"agresti_coull_boundary_surrogate"`.

- Suppression values: `"suppressed_drop"` and
  `"suppression_sensitivity"`.

Experimental GVF/log-variance smoothing records
`" + gvf_smooth_loglinear"` or `" + gvf_smooth_gam"` in
`var_method_smoothed`; these labels enter canonical `var_method` only
under an allowed overwrite. Legacy `" + fh_smooth_*"` labels remain
readable. The matrix-level `vcov_method` field on each
[`sm_vcov`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
object uses its own lexicon; see
[`sm_vcov()`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
for the canonical `vcov_method` / `vcov_scale` specification.

## Package-neutral export

Call
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
before converting the object:
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) is a
transport boundary, not validation. For scalar work, project all rows to
the identity, `theta_hat`, `se`, `estimate_scale`, `var_method`,
suppression/accountability flags, and every present
`estimate_status`/`sensitivity_*` field before filtering by indicator.
Define local eligibility explicitly; only identified rows with finite
estimates and strictly positive finite SEs are ordinary inverse-variance
inputs. Retain exact-census zero-SE rows, suppressed rows, and
non-identified sensitivity rows for audit, but do not assign them
inverse-variance weights. Never substitute `sensitivity_var` for `se`.
If an experimental smoothed SE is selected, pair it with
`var_method_smoothed` and preserve the canonical pair. For joint work,
retain complete site-year indicator groups and verify each `V`'s
`indicator_order`, dimnames, and scale metadata.

## Scale conventions

Under Scenario A (binomial 1x1) and Scenario D0, each `V` entry is a \\1
\times 1\\ covariance matrix whose `vcov_scale` is derived from the row
`estimate_scale`; for example, arcsine rows record
`vcov_scale = "arcsine_delta"`. Under Scenarios B and C, `V` is returned
on the raw probability scale regardless of `estimate_scale`;
consequently `sqrt(diag(V))` is *not* equal to `se` for those rows when
row-level output is transformed. Always inspect each `sm_vcov` object's
`vcov_scale` field before downstream use. Under Scenario D1 with
working-independence covariance, `V` follows `estimate_scale`;
matrix-scale transformations aligning Scenario B / C `V` with the row
estimate scale are not implemented in v0.2. The `sm_vcov$diag_contract`
field makes the row companion explicit: A/D0/D1 matrices match `se^2`;
B/C raw matrices ordinarily match `se_raw^2`; and multinomial boundary
surrogates record the intentional scalar/simplex-diagonal exception.
When `fpc` is supplied, matrix metadata records the SRSWOR population,
sampling fraction, conventional FPC, actually applied multiplier, and
plug-in/design-corrected rule without changing `n_eff`.

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
for the sufficient-counts wrapper;
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
for the published-aggregates wrapper;
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
for the uncertainty audit;
[`sm_vcov()`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
for the `V` list-column class spec and the canonical `vcov_scale` /
`vcov_method` / `var_method` lexicons;
[`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
for experimental GVF/log-variance smoothing;
[`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
for D1-aggregate sensitivity;
[`vignette("a1-getting-started", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/a1-getting-started.md)
for the five-minute applied tutorial;
[`vignette("m1-statistical-foundations", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md)
for the sampling-uncertainty framework and notation;
[`vignette("m2-scalar-se-binomial", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
for the delta-method SE derivation.

Other estimation:
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md),
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)

## Examples

``` r
data(alprek_subset, package = "sitemix")

# Scenario A -- binomial from student rows
est_a <- sm_estimate(
  subset(alprek_subset, year == 2024),
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
#> 1 S001     2024 frpm          0.4       0.685  0.155 0.158    10    10
#> 2 S002     2024 frpm          0.222     0.491  0.139 0.167     9     9
#> 3 S003     2024 frpm          0.545     0.831  0.150 0.151    11    11
#> 4 S004     2024 frpm          0.333     0.615  0.157 0.167     9     9
#> 5 S005     2024 frpm          0.556     0.841  0.166 0.167     9     9
#> # ℹ 9 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>
unique(est_a$estimate_scale)
#> [1] "arcsine"

# Scenario B -- multivariate with joint covariance
est_b <- sm_estimate(
  subset(alprek_subset, year == 2024),
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
#> 1 S001     2024 frpm          0.4       0.685  0.155 0.158    10    10
#> 2 S001     2024 snap          0.6       0.886  0.155 0.158    10    10
#> 3 S002     2024 frpm          0.222     0.491  0.139 0.167     9     9
#> 4 S002     2024 snap          0.333     0.615  0.157 0.167     9     9
#> # ℹ 11 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>,
#> #   V <list>, K <int>
est_b$V[[1L]]$vcov_scale   # raw under Scenario B
#> [1] "raw"
est_b$V[[1L]]$vcov_method  # "sur"
#> [1] "sur"
```
