# Summarize suppression and reporting thresholds in aggregate data

`sm_suppression_report()` counts publisher-suppressed rows and rows
below a chosen denominator threshold. It also reports whether suppressed
denominators are observed. Use it to examine published input data before
calling
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md).
It does not fill in hidden values.

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
  depend on the input format; see
  [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md).

- by:

  A character vector, or `NULL` for one overall row. Columns to group
  the report by after aggregate normalization. Defaults to
  `c("subgroup", "year")`. Each group gets its own row in the returned
  tibble.

- id_cols:

  Character vector of length two. Site and year column names. Defaults
  to `c("site_id", "year")`.

- numerator_col:

  Character scalar or `NULL` (default `NULL`). Source numerator column
  name. Required when the publisher uses other column names.

- denominator_col:

  Character scalar or `NULL` (default `NULL`). Source denominator column
  name. Required when the publisher uses other column names.

- indicator_col:

  Character scalar or `NULL` (default `NULL`). Source indicator column
  for long-form input (one row per site-year-indicator).

- subgroup_col:

  Character scalar or `NULL` (default `NULL`). Source subgroup column
  for publisher files with subgroup decomposition.

- suppression_col:

  Character scalar or `NULL` (default `NULL`). Source publisher
  suppression flag column. With `NULL`, an existing `suppression_flag`
  column is used.

- suppression_flag_value:

  Value or vector of values marking publisher suppression in
  `suppression_col`. Defaults to `""` (the empty string).

- suppression_when:

  Function or `NULL` (default `NULL`). Optional predicate returning one
  logical value per normalized row; overrides flag and missing-numerator
  detection.

- min_n:

  A positive whole number stored in the report's `min_n` attribute.
  Defaults to `10L`. The tier counts use suppression status and
  `accountability_n`; changing `min_n` alone does not change those
  counts.

- accountability_n:

  Positive integer scalar. Tier-2 / Tier-3 boundary. Among rows that are
  not suppressed, those with \\n\_{jt} \<\\ `accountability_n` are Tier
  2; those meeting the threshold are Tier 3. Defaults to `30L`.

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

  Integer count of rows with a missing denominator value.

- `pct_suppressed`:

  Numeric share of suppressed rows in the group (Tier 1).

- `pct_below_accountability`:

  Numeric share of suppressed rows plus rows that are not suppressed and
  fall below the threshold: Tier 1 rows plus Tier 2 rows below
  `accountability_n`.

- `median_n_suppressed`:

  Numeric; median denominator of suppressed rows when observable, else
  `NA`.

- `denominator_observed_on_suppressed`:

  Logical; `TRUE` when every Tier 1 row in the group carries an
  observable denominator.

- `suppression_sources`:

  Character; a compact list of the detection rules used (publisher flag,
  missing numerator, or user predicate).

- `recommended_action`:

  Character; a one-line recommendation distinguishing retained missing
  estimates from an acknowledged variance sensitivity.

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
  non-identified variance-sensitivity scenario rather than an estimate.
  This older counterpart of `sensitivity_role` uses `"not_applicable"`
  when Tier 1 is absent, whereas `sensitivity_role` uses `"none"`.

Two older column names are also retained:
`upper_bound_numeric_variance_available` mirrors
`sensitivity_numeric_variance_available`;
`upper_bound_requires_acknowledgement` mirrors
`sensitivity_requires_acknowledgement`.

## Details

The report groups rows into three tiers:

- Tier 1 – suppressed:

  Rows identified by a publisher flag or a missing numerator with an
  observed positive denominator. A `suppression_when` predicate
  overrides both rules. Missing denominators alone do not identify
  suppression.

- Tier 2 – below threshold:

  Rows that are not suppressed and have an observed denominator smaller
  than `accountability_n`.

- Tier 3 – meets threshold:

  Rows that are not suppressed and have an observed denominator at least
  as large as `accountability_n`.

These tiers describe suppression and the chosen size threshold. The
report retains missing-denominator counts separately. Its
`accountability_n` threshold can be changed; it is distinct from the
fixed 11–29 denominator range used by
`sm_smooth_variance(scope = "tier2")`.

Each group defined by `by` (for example, `c("subgroup", "year")`)
receives counts and proportions. For suppressed rows, the report
distinguishes retaining missing estimates from an explicitly
acknowledged variance-sensitivity analysis. Use
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
to examine the resulting estimates.

## See also

- [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
  for estimating proportions and handling suppressed input rows.

- [`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
  for the post-estimation diagnostics.

- [`sm_pivot_subgroups_to_sites()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md)
  and
  [`sm_pivot_subgroups_to_indicators()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md)
  for reshaping subgroup data before making the report.

- [`vignette("a5-published-aggregates")`](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md)
  and
  [`vignette("a6-diagnostics-and-suppression")`](https://joonho112.github.io/sitemix/articles/a6-diagnostics-and-suppression.md)
  for the applied walkthroughs.

Other audit:
[`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)

## Examples

``` r
# Prepare aggregate counts from the simulated data:
counts_path <- system.file(
  "extdata", "prek_sim_counts.rds",
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
#> 1 all       2021     50       0      32      18                               0
#> 2 all       2022     50       0      31      19                               0
#> 3 all       2023     50       0      33      17                               0
#> 4 all       2024     50       0      33      17                               0
#> 5 all       2025     50       0      33      17                               0
#> # ℹ 13 more variables: n_denominator_missing <int>, pct_suppressed <dbl>,
#> #   pct_below_accountability <dbl>, median_n_suppressed <dbl>,
#> #   denominator_observed_on_suppressed <lgl>, suppression_sources <chr>,
#> #   recommended_action <chr>, sensitivity_role <chr>,
#> #   sensitivity_numeric_variance_available <lgl>,
#> #   sensitivity_requires_acknowledgement <lgl>, upper_bound_role <chr>,
#> #   upper_bound_numeric_variance_available <lgl>, …
```
