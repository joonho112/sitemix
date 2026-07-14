# Audit aggregate-input suppression and accountability tiers

`sm_suppression_report()` audits the **three-tier aggregate denominator
regime** before estimation. It reports observed tier counts and
denominator observability per group; it does **not** impute hidden
values. Use it before
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
to understand how many rows are publisher-suppressed (Tier 1), observed
but below accountability threshold (Tier 2), or observed and publishable
(Tier 3).

## Usage

``` r
sm_suppression_report(
  x,
  by = c("subgroup", "year"),
  id_cols = c("site_id", "year"),
  numerator_col = NULL,
  denominator_col = NULL,
  indicator_col = NULL,
  subgroup_col = NULL,
  suppression_col = NULL,
  suppression_flag_value = "",
  suppression_when = NULL,
  min_n = 10L,
  accountability_n = 30L
)
```

## Arguments

- x:

  A data frame or tibble of aggregate input data. Required columns
  depend on the publisher schema; see
  [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
  for the canonical input format.

- by:

  Character vector. Columns to group the report by after aggregate
  normalization. Defaults to `c("subgroup", "year")`. Each group gets
  its own row in the returned tibble.

- id_cols:

  Character vector of length two. Site and year column names. Defaults
  to `c("site_id", "year")`.

- numerator_col:

  Character scalar or `NULL` (default `NULL`). Source numerator column
  name. Required when the publisher schema uses non-default column
  names.

- denominator_col:

  Character scalar or `NULL` (default `NULL`). Source denominator column
  name. Required when the publisher schema uses non-default column
  names.

- indicator_col:

  Character scalar or `NULL` (default `NULL`). Source indicator column
  for long-form input (one row per site-year-indicator).

- subgroup_col:

  Character scalar or `NULL` (default `NULL`). Source subgroup column
  for publisher files with subgroup decomposition.

- suppression_col:

  Character scalar or `NULL` (default `NULL`). Source publisher
  suppression flag column.

- suppression_flag_value:

  Value or vector of values marking publisher suppression in
  `suppression_col`. Defaults to `""` (the empty string).

- suppression_when:

  Function or `NULL` (default `NULL`). Optional predicate with highest
  detection priority; overrides flag-based detection.

- min_n:

  Positive integer scalar. Tier-1 boundary reference used for
  diagnostics on the boundary between observed and suppressed rows.
  Defaults to `10L`.

- accountability_n:

  Positive integer scalar. Tier-2 / Tier-3 boundary. Rows with \\n\_{jt}
  \<\\ `accountability_n` are classified Tier 2; others Tier 3. Defaults
  to `30L`.

## Value

A `sitemix_suppression_report` tibble with one row per group defined by
`by` and the following columns:

- group columns:

  The grouping columns from `by` (e.g., `subgroup`, `year`).

- `n_rows`:

  Integer total row count in the group.

- `n_tier1`:

  Integer count of Tier 1 (publisher- suppressed) rows in the group.

- `n_tier2`:

  Integer count of Tier 2 (observed below `accountability_n`) rows.

- `n_tier3`:

  Integer count of Tier 3 (observed and meets threshold) rows.

- `n_suppressed_hidden_denominator`:

  Integer count of suppressed rows whose denominator is also hidden.

- `n_denominator_missing`:

  Integer count of rows missing the denominator column.

- `pct_suppressed`:

  Numeric share of suppressed rows in the group (Tier 1).

- `pct_below_accountability`:

  Numeric share of rows that are not publishable under the three-tier
  framework: Tier 1 publisher-suppressed rows plus Tier 2 observed rows
  below `accountability_n`.

- `median_n_suppressed`:

  Numeric; median denominator of suppressed rows when observable, else
  `NA`.

- `denominator_observed_on_suppressed`:

  Logical; `TRUE` when every Tier 1 row in the group carries an
  observable denominator.

- `suppression_sources`:

  Character; a compact enumeration of which detection rule fired
  (publisher flag, structural missingness, or user predicate).

- `recommended_action`:

  Character; a one-line recommendation distinguishing canonical missing
  retention from an acknowledged variance sensitivity.

- `sensitivity_role`:

  Character with two values:

  - `"none"` when Tier 1 is absent.

  - `"nonidentified_variance_sensitivity"` otherwise.

- `sensitivity_numeric_variance_available`:

  Logical; `TRUE` only when Tier-1 denominators are all observed, so a
  separated worst-case variance can be quantified.

- `sensitivity_requires_acknowledgement`:

  Logical; `TRUE` whenever a Tier-1 row is present.

- `upper_bound_role`:

  Character; identifies the legacy `"upper_bound"` option as a
  non-identified variance-sensitivity scenario, never an estimate.
  Legacy counterpart of `sensitivity_role`; it retains
  `"not_applicable"` when Tier 1 is absent, where the canonical role is
  `"none"`.

The report also retains two legacy compatibility aliases.
`upper_bound_numeric_variance_available` mirrors
`sensitivity_numeric_variance_available`;
`upper_bound_requires_acknowledgement` mirrors
`sensitivity_requires_acknowledgement`.

## Details

**Three-tier framework.** Aggregate inputs from state and district
publishers carry three distinct denominator regimes that the audit
summarizes separately:

- **Tier 1 – publisher-suppressed**:

  The publisher masked the row. Detected via missing numerator /
  denominator, an explicit publisher suppression flag (controlled by
  `suppression_col` and `suppression_flag_value`), or a user-supplied
  predicate (`suppression_when`). These rows cannot be estimated; they
  appear in the report so the analyst can retain canonical missing audit
  rows or explicitly acknowledge a separated variance-sensitivity
  scenario through the aggregate estimator's `suppression =` argument.

- **Tier 2 – observed below accountability**:

  Denominator is present and \\n\_{jt} \<\\ `accountability_n`. The row
  is estimable, but the publisher (or the analyst's project rules)
  treats it as too small to publish individually.

- **Tier 3 – observed and meets threshold**:

  Denominator is present and \\n\_{jt} \ge\\ `accountability_n`. The row
  is estimable and publishable under the project's accountability rules.

The function returns one row per group defined by `by` (e.g.,
`c("subgroup", "year")`) with Tier 1 / Tier 2 / Tier 3 counts and the
share of the total. This is the recommended pre-flight check for any D0
or D1 estimation; the
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
audit operates on the post-estimation tibble and assumes Tier 1 rows
have already been dispositioned.

## See also

- [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
  for the upstream aggregate estimator and suppression controls.

- [`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
  for the post-estimation audit.

- [`sm_pivot_subgroups_to_sites()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md)
  and
  [`sm_pivot_subgroups_to_indicators()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md)
  for subgroup-file pivots used before this audit.

- [`vignette("a5-published-aggregates")`](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md)
  and
  [`vignette("a6-diagnostics-and-suppression")`](https://joonho112.github.io/sitemix/articles/a6-diagnostics-and-suppression.md)
  for the applied walkthroughs.

Other audit:
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)

## Examples

``` r
# Build a small aggregate slice from bundled counts:
counts_path <- system.file(
  "extdata", "alprek_subset_counts.rds",
  package = "sitemix", mustWork = TRUE
)
counts <- readRDS(counts_path)

d0 <- counts[, c("site_id", "year", "n_jt", "c_jt_frpm")]
d0$indicator <- "frpm"
d0$c_jt <- d0$c_jt_frpm
d0$subgroup <- "all"
d0 <- d0[c("site_id", "year", "subgroup", "indicator", "c_jt", "n_jt")]

report <- sm_suppression_report(
  d0,
  by              = c("subgroup", "year"),
  numerator_col   = "c_jt",
  denominator_col = "n_jt",
  indicator_col   = "indicator",
  subgroup_col    = "subgroup"
)
head(report)
#> # A tibble: 5 × 20
#>   subgroup  year n_rows n_tier1 n_tier2 n_tier3 n_suppressed_hidden_denominator
#>   <chr>    <int>  <int>   <int>   <int>   <int>                           <int>
#> 1 all       2021     50       0      31      19                               0
#> 2 all       2022     50       0      31      19                               0
#> 3 all       2023     50       0      29      21                               0
#> 4 all       2024     50       0      29      21                               0
#> 5 all       2025     50       0      30      20                               0
#> # ℹ 13 more variables: n_denominator_missing <int>, pct_suppressed <dbl>,
#> #   pct_below_accountability <dbl>, median_n_suppressed <dbl>,
#> #   denominator_observed_on_suppressed <lgl>, suppression_sources <chr>,
#> #   recommended_action <chr>, sensitivity_role <chr>,
#> #   sensitivity_numeric_variance_available <lgl>,
#> #   sensitivity_requires_acknowledgement <lgl>, upper_bound_role <chr>,
#> #   upper_bound_numeric_variance_available <lgl>, …
```
