# Prepare published counts for subgroup proportions or compositions

`sm_pivot_subgroups_to_sites()` reshapes published subgroup counts for
estimation. By default, each site-subgroup pair receives a combined
`site_id`, keeping its own numerator and denominator. Pass this table to
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
to estimate a rate within each subgroup. The documentation calls this
**Framing X**.

A composition `partition_target` instead prepares category counts with a
common denominator for
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
with `family = "multinomial"` (Scenario C). These counts describe each
subgroup's share of a site total, rather than its within-subgroup rate.
The function reshapes counts; it does not estimate proportions or
standard errors.

## Usage

``` r
sm_pivot_subgroups_to_sites(
  data,
  site_col = "site_id",
  year_col = "year",
  subgroup_col,
  numerator_col,
  denominator_col,
  indicator = "subgroup_rate",
  separator = "_",
  level_override = NULL,
  rtype_col = NULL,
  partition_target = c("none", "denominator_composition", "case_composition"),
  partition_tolerance = 0.5,
  suppression_col = NULL,
  suppression_flag_value = ""
)
```

## Arguments

- data:

  A data frame or tibble containing subgroup aggregate rows, one row per
  `(site, year, subgroup)` triple.

- site_col:

  A single column name containing source site identifiers. Defaults to
  `"site_id"`.

- year_col:

  A single column name containing integer-like years. Defaults to
  `"year"`.

- subgroup_col:

  A single column name containing subgroup labels. Required.

- numerator_col:

  A single column name containing aggregate numerator counts. Required.

- denominator_col:

  A single column name containing aggregate denominator counts.
  Required.

- indicator:

  A single string giving the indicator label to place in the output
  `indicator` column. Defaults to `"subgroup_rate"`.

- separator:

  A single non-empty string used to join the source site and subgroup
  labels. Defaults to `"_"`.

- level_override:

  Must be `NULL` (default). Other values raise
  `sitemix_error_invalid_level_override`.

- rtype_col:

  Must be `NULL` (default). Other values raise
  `sitemix_error_invalid_level_override`; split the source by reporting
  level before using this function.

- partition_target:

  A single string selecting the counts to prepare: `"none"` (default; D0
  conditional-rate rows), `"denominator_composition"`, or
  `"case_composition"` (both return Scenario C count input).

- partition_tolerance:

  A single finite, non-negative number. Absolute tolerance for
  composition partition checks against the required `ALL` row. Defaults
  to `0.5`.

- suppression_col:

  A single column name, or `NULL` (default). Names the publisher
  suppression flag column. With `NULL`, an existing `suppression_flag`
  column is used if present. If neither source is available, source rows
  receive `suppression_flag = FALSE`.

- suppression_flag_value:

  Value or vector of values marking publisher suppression in the flag
  column. Defaults to `""`.

## Value

With `partition_target = "none"`, a tibble with one row per
site-year-subgroup for
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md).
It contains:

- `site_id`:

  Composite identifier constructed by joining the source site and
  subgroup labels with `separator`.

- `year`:

  The source year, stored as an integer.

- `indicator`:

  Character scalar (the `indicator` argument).

- `c_jt`, `n_jt`:

  Numerator and denominator copied from `numerator_col` and
  `denominator_col`.

- `suppression_flag`:

  Always-present logical. It is `TRUE` for rows flagged by the publisher
  and otherwise `FALSE`. Flags come from `suppression_col`, or from an
  existing `suppression_flag` column when that argument is `NULL`. If
  neither source flag column is available, all rows receive `FALSE`.

- `framing`:

  Character scalar; the framing label (`"subgroup_as_site"`).

- `source_site_id`, `source_subgroup`:

  The original site and publisher subgroup labels, preserved for
  reference. A recognized total label becomes `"ALL"` only in the
  combined `site_id`; `source_subgroup` keeps its source spelling.

With a composition target, a tibble with one row per original site-year,
containing `site_id`, `year`, a common `n_jt`, and one `c_jt_<category>`
column per non-total subgroup. The common `n_jt` is the sum of these
category counts; the `ALL` row supplies the comparison total and is not
a category. The `partition_categories` and `indicator_order` attributes
record category order. Use this order as `indicators` in
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
with `family = "multinomial"`.

## Details

**Proportions and compositions.** Choose `partition_target` according to
the quantity you want to estimate:

- `"none"` (default):

  Retains each subgroup's numerator and denominator for a conditional
  rate: cases within that subgroup divided by its own denominator. The
  combined `site_id` joins the source site and subgroup label with
  `separator`. This is binomial aggregate input (D0).

- `"denominator_composition"`:

  Uses subgroup denominator counts as category counts. The common
  denominator is their sum, checked against the `ALL` denominator. The
  resulting proportions describe each subgroup's share of the site's
  denominator total.

- `"case_composition"`:

  Uses subgroup numerator counts as category counts. The common
  denominator is their sum, checked against the `ALL` numerator. The
  resulting proportions describe how the site's cases are distributed
  across subgroups.

For a composition, the subgroups must represent mutually exclusive
categories covering the population or cases being counted. Both targets
require at least two categories, the same complete category set in every
site-year, and exactly one total row. A missing category row is an
error; supply an explicit zero for a category with no observations.
Category counts must sum to the relevant total within
`partition_tolerance`, and that total must be positive. These count
checks do not establish whether the source categories overlap.

Denominator composition requires observed denominators for every
category and the total, but permits suppressed numerators. For case
composition, every category and the total must have an observed
numerator that is not suppressed.

To keep subgroups as marginal indicators of the original site instead of
assigning combined site identifiers, use
[`sm_pivot_subgroups_to_indicators()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md)
(Framing Y). That alternative retains each subgroup's own numerator and
denominator.

**Total labels.** The function recognizes the publisher labels `"ALL"`,
`"ALL STUDENT"`, `"ALL STUDENTS"`, `"TOTAL"`, and `"OVERALL"` as
`"ALL"`. Matching ignores case, surrounding whitespace, and punctuation
between words. Two recognized total labels in the same site-year become
duplicate `"ALL"` rows and raise an error. With the default target, the
combined `site_id` uses `"ALL"`, while `source_subgroup` preserves the
publisher's spelling.

**Reporting levels.** Split files that mix school, district, or state
rows into separate tables for each reporting level before calling either
subgroup helper. The functions do not separate these levels. Supplying
`level_override` or `rtype_col` raises
`sitemix_error_invalid_level_override`; both must remain `NULL`.

## See also

- [Framing Y
  helper](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md).

- [Aggregate
  estimation](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
  for the default output.

- [Estimation from
  counts](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
  for composition targets.

- [Suppression
  audit](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
  before pivoting.

- [`vignette("a5-published-aggregates")`](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md)
  for the walkthrough.

Other reshape:
[`sm_pivot_subgroups_to_indicators()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md)

## Examples

``` r
# Synthetic publisher file with school-by-subgroup rows
subgroups <- expand.grid(
  site_id  = paste0("S", sprintf("%03d", 1:5)),
  year     = 2024L,
  subgroup = c("frpm_yes", "frpm_no"),
  stringsAsFactors = FALSE
)
subgroups$c_jt <- c(8, 4, 7, 5, 9, 3, 10, 6, 5, 8)
subgroups$n_jt <- c(12, 6, 11, 9, 13, 7, 15, 10, 8, 12)

pivoted <- sm_pivot_subgroups_to_sites(
  subgroups,
  subgroup_col    = "subgroup",
  numerator_col   = "c_jt",
  denominator_col = "n_jt",
  indicator       = "frpm_take_up"
)
head(pivoted)
#> # A tibble: 6 × 9
#>   site_id     year indicator  c_jt  n_jt suppression_flag framing source_site_id
#>   <chr>      <int> <chr>     <int> <int> <lgl>            <chr>   <chr>         
#> 1 S001_frpm…  2024 frpm_tak…     3     7 FALSE            subgro… S001          
#> 2 S001_frpm…  2024 frpm_tak…     8    12 FALSE            subgro… S001          
#> 3 S002_frpm…  2024 frpm_tak…    10    15 FALSE            subgro… S002          
#> 4 S002_frpm…  2024 frpm_tak…     4     6 FALSE            subgro… S002          
#> 5 S003_frpm…  2024 frpm_tak…     6    10 FALSE            subgro… S003          
#> 6 S003_frpm…  2024 frpm_tak…     7    11 FALSE            subgro… S003          
#> # ℹ 1 more variable: source_subgroup <chr>
```
