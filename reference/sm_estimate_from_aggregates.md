# Estimate site-year proportions from published aggregate rows

`sm_estimate_from_aggregates()` estimates site-year proportions and
standard errors from published numerator and denominator counts. Use
`family = "binomial"` for one indicator (D0), or
`family = "multivariate"` for several marginal indicators (D1). D1 can
return a covariance matrix based on working independence; marginal
counts alone do not identify cross-indicator dependence.

This function calls
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
with `from_aggregates = TRUE`. Calling
`sm_estimate(..., from_aggregates = TRUE)` directly produces the same
result. Do not pass `from_aggregates` or `from_counts` through `...`;
the latter raises `sitemix_error_input_path_conflict`. Published
aggregates do not support `family = "multinomial"`; this raises
`sitemix_error_ambiguous_dispatch`. Use
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
with complete category counts for Scenario C.

## Usage

``` r
sm_estimate_from_aggregates(
  data,
  family,
  indicator = NULL,
  indicators = NULL,
  id_cols = c("site_id", "year"),
  numerator_col = NULL,
  denominator_col = NULL,
  indicator_col = NULL,
  subgroup_col = NULL,
  aggregate_case = c("auto", "D0", "D1"),
  framing = NA_character_,
  sampling_relation = c("unknown", "same_units", "different_units"),
  accountability_n = 30L,
  suppression = c("drop", "upper_bound"),
  suppression_col = NULL,
  suppression_flag_value = "",
  suppression_when = NULL,
  suppressed_theta_hat = 0.5,
  suppression_sensitivity_acknowledge = FALSE,
  suppressed_n_strategy = c("observed_n", "worst_case_bound"),
  suppressed_n_bound = NULL,
  ...
)
```

## Arguments

- data:

  A data frame or tibble containing site and year columns named by
  `id_cols`. Student rows need the column named by `indicator`, or the
  binary columns named by `indicators`. Counts input needs `n_jt` and
  family-specific `c_jt_*` columns. Published aggregates may use the
  standard aggregate column names or the column mappings below; see
  `sm_estimate_from_aggregates()`.

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

- numerator_col:

  A single column name, or `NULL` (default). Maps an aggregate numerator
  to `c_jt`. With `NULL`, long-form input uses an existing `c_jt`
  column; wide-form input uses `c_jt_*` columns.

- denominator_col:

  A single column name, or `NULL` (default). Maps an aggregate
  denominator to `n_jt`. With `NULL`, use the standard denominator
  columns described in `sm_estimate_from_aggregates()`.

- indicator_col:

  A single column name, or `NULL` (default). Maps long-form aggregate
  indicator labels to `indicator`; `NULL` uses that name if present.

- subgroup_col:

  A single column name, or `NULL` (default). Maps aggregate subgroup
  labels to `subgroup`; `NULL` uses that name if present.

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

- accountability_n:

  A positive whole number. Rows with `n < accountability_n` receive
  `flag_below_accountability = TRUE`. Defaults to `30L`.

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

- ...:

  Additional arguments forwarded to
  [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md).

## Value

A `sitemix_estimates` tibble with one row per site-year-indicator. It
contains proportions, standard errors, their scales and calculation
methods, and suppression flags. With `vjt = TRUE`, it also includes a
`V` list-column. See
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
for the returned columns and attributes.

## Details

**Input columns.** Each row needs the site and year columns named by
`id_cols`. Counts can be arranged in either of two forms:

- Long form: one row per site-year-indicator, with `indicator`, `c_jt`,
  and `n_jt`. For D0, the `indicator` argument can supply the label when
  the column is absent.

- Wide form: one row per site-year, with numerator columns named
  `c_jt_<indicator>`. Supply either a common `n_jt` denominator or a
  matching `n_jt_<indicator>` column for every indicator. Do not mix
  these denominator forms. Use `indicators` to specify the marginal
  indicator names.

Use `numerator_col`, `denominator_col`, `indicator_col`, `subgroup_col`,
and `suppression_col` when the corresponding columns have other names.
With `NULL` (the default), the function uses existing `c_jt`, `n_jt`,
`indicator`, `subgroup`, and `suppression_flag` columns, respectively.
Wide-form numerator and per-indicator denominator columns use the naming
patterns above. Subgroup and suppression-flag columns are optional.

**One or several indicators.** Choose the family to match the available
counts:

- **D0**:

  Use when `family = "binomial"` and the input has one numerator and one
  denominator per site-year.

- **D1**:

  Use when `family = "multivariate"` and the input has multiple
  aggregate marginals per site-year. With `vjt = TRUE`, the returned `V`
  is diagonal: off-diagonal zeros express a working independence
  assumption, not observed independence. For raw pairwise Fréchet
  intervals and separately labeled projected stress scenarios, see
  [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md).

Set `aggregate_case = "auto"` (default) to resolve one unique indicator
as D0 and two or more as D1; pass `"D0"` or `"D1"` to require that case.
D1 requires the same ordered indicator set in every site-year group. Set
`sampling_relation = "same_units"` only when the marginal rows are known
to describe the same observational units, or `"different_units"` when
they are known to differ. The default `"unknown"` makes no such claim.
Common denominators are recorded separately and never imply
`"same_units"`.

**Subgroups.** If each site has several subgroup rows, decide whether to
estimate each subgroup as a separate site (Framing X) or as an indicator
within the original site (Framing Y). Reshape the file first with
[`sm_pivot_subgroups_to_sites()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md)
or
[`sm_pivot_subgroups_to_indicators()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md),
respectively, then pass the resulting table here. See
[`vignette("a5-published-aggregates", package = "sitemix")`](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md).

**Suppression.** Use
[`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
to count publisher-suppressed rows before estimation. Detection uses the
suppression flag and missing numerators unless `suppression_when`
supplies a custom rule; see the suppression arguments below.
`suppression = "drop"` retains each suppressed row with missing
estimates and standard errors.

`suppression = "upper_bound"` requires
`suppression_sensitivity_acknowledge = TRUE` when suppressed rows are
present. It stores a worst-case Bernoulli variance scenario in separate
`sensitivity_*` fields, leaving the estimate and SE columns missing.
These rows cannot supply ordinary covariance or formal Fréchet inputs. A
hidden denominator cannot support a numeric sensitivity variance;
`suppressed_n_strategy` and `suppressed_n_bound` record the available
denominator information without estimating that variance.

## See also

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  for estimation options and returned columns.

- [`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
  for estimates from sufficient counts.

- [`sm_pivot_subgroups_to_sites()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md)
  and
  [`sm_pivot_subgroups_to_indicators()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md)
  for the Framing X and Framing Y pivots.

- [`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
  for publisher-side suppression auditing.

- [`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  for D1 aggregate sensitivity.

- [`vignette("a5-published-aggregates")`](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md)
  for the applied walkthrough.

- [`vignette("m5-aggregate-engines")`](https://joonho112.github.io/sitemix/articles/m5-aggregate-engines.md)
  for D0 / D1 estimation methods and assumptions.

Other estimation:
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md),
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)

## Examples

``` r
# Select one indicator and year from the bundled simulated counts:
counts_path <- system.file(
  "extdata", "prek_sim_counts.rds",
  package = "sitemix", mustWork = TRUE
)
counts <- readRDS(counts_path)

d0_frpm <- counts[counts$year == 2024, c("site_id", "year", "n_jt", "c_jt_frpm")]
d0_frpm$indicator <- "frpm"
d0_frpm$c_jt <- d0_frpm$c_jt_frpm
d0_frpm <- d0_frpm[c("site_id", "year", "indicator", "c_jt", "n_jt")]

est <- sm_estimate_from_aggregates(
  d0_frpm,
  family    = "binomial",
  indicator = "frpm"
)
head(est, 5)
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
# theta_hat and se use this scale; theta_raw remains a proportion.
unique(est$estimate_scale)
#> [1] "arcsine"
```
