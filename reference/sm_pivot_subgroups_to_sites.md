# Pivot subgroup aggregate rows into subgroup-as-site input (Framing X)

`sm_pivot_subgroups_to_sites()` implements **Framing X** for
school-by-subgroup aggregate rows: each `(site, subgroup)` pair becomes
its own aggregate site with a composite `site_id`. The output is
consumable by
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
(default) or, with an explicit composition `partition_target`, by
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
as Scenario C multinomial count input. Use this when subgroup × site is
the unit of analysis; for the alternative framing where subgroups become
indicators of the original site, see
[`sm_pivot_subgroups_to_indicators()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md)
(Framing Y).

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

  Character scalar. Column name containing source site identifiers.
  Defaults to `"site_id"`.

- year_col:

  Character scalar. Column name containing integer-like years. Defaults
  to `"year"`.

- subgroup_col:

  Character scalar. Column name containing subgroup labels. Required.

- numerator_col:

  Character scalar. Column name containing aggregate numerators.
  Required.

- denominator_col:

  Character scalar. Column name containing aggregate denominators.
  Required.

- indicator:

  Character scalar. Single indicator label to place in the output
  `indicator` column. Defaults to `"subgroup_rate"`.

- separator:

  Character scalar. Separator used to construct composite
  subgroup-as-site IDs. Defaults to `"_"`.

- level_override:

  Must be `NULL` (default). Mixed-level override semantics are
  intentionally unsupported and any other value raises
  `sitemix_error_invalid_level_override`.

- rtype_col:

  Must be `NULL` (default). Declaring a publisher row- type column
  raises the same stable invalid-level condition as `level_override`;
  split the source into one homogeneous reporting level first.

- partition_target:

  Character scalar. Explicit partition estimand. One of `"none"`
  (default; D0 conditional-rate rows), `"denominator_composition"`, or
  `"case_composition"` (both return Scenario C count input).

- partition_tolerance:

  Non-negative numeric scalar. Absolute tolerance for composition
  partition checks against the required `ALL` row. Defaults to `0.5`.

- suppression_col:

  Character scalar or `NULL` (default `NULL`). Optional publisher
  suppression flag column.

- suppression_flag_value:

  Value or vector of values marking publisher suppression in
  `suppression_col`. Defaults to `""`.

## Value

A tibble consumable by
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
when `partition_target = "none"`, or by
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
with `family = "multinomial"` when a composition target is requested.
Schema for the default case:

- `site_id`:

  Composite identifier constructed by joining the source site and
  subgroup labels with `separator`.

- `year`:

  Integer year (copied verbatim).

- `indicator`:

  Character scalar (the `indicator` argument).

- `c_jt`, `n_jt`:

  Numerator and denominator copied from `numerator_col` and
  `denominator_col`.

- `suppression_flag`:

  Always-present logical. It is `TRUE` for rows flagged by the publisher
  and otherwise `FALSE`; when no `suppression_col` is supplied, all rows
  are `FALSE`.

- `framing`:

  Character scalar; the framing label (`"subgroup_as_site"`).

- `source_site_id`, `source_subgroup`:

  The original site and publisher subgroup labels, preserved for
  traceback. A recognized total alias is canonicalized only in the
  composite `site_id`; `source_subgroup` keeps its source spelling.

## Details

**Framing X vs Framing Y.** The two pivot helpers solve the same input
problem (publisher files where each row is a school-by-subgroup
observation) but produce different schemas:

- **Framing X (this function)**:

  Each subgroup becomes its own site. The composite `site_id` is
  constructed by concatenating the source site identifier and the
  subgroup label with `separator`. Downstream estimation treats subgroup
  × site as the keyed pair. Use when the analyst's question is "what is
  each subgroup's rate within each site?"

- **Framing Y (sister function)**:

  Each subgroup becomes a marginal indicator of the original site. The
  site_id is preserved; the indicator column carries the subgroup label.
  Use when the analyst's question is "what does each site's subgroup
  profile look like?" See the [Framing Y
  helper](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md).

**Partition targets.** Pass `partition_target = "none"` (default) for
D0-ready conditional-rate output. Pass `"denominator_composition"` or
`"case_composition"` to emit Scenario C multinomial count input; both
partition targets require one complete, identical category grid per
site-year and one explicit total row in `subgroup_col`. Total labels are
normalized to canonical `"ALL"` from the fixed publisher vocabulary
`"ALL"`, `"ALL STUDENT"`, `"ALL STUDENTS"`, `"TOTAL"`, and `"OVERALL"`;
matching ignores case, surrounding whitespace, and punctuation between
words. Two labels that collapse to `"ALL"` in the same site-year are
duplicates and fail closed. Composition sums are checked against the
canonical total within `partition_tolerance`; a missing category row is
never inferred to be a structural zero.

**Mixed-level scope.** Mixed school/district/state routing is not
identified by the current public arguments. Any non-`NULL`
`level_override` or `rtype_col` therefore fails closed with the stable
invalid-level condition documented for `level_override`. Split
mixed-level publisher files into homogeneous tables before calling
either pivot helper.

## See also

- [Framing Y
  helper](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_indicators.md).

- [Aggregate
  wrapper](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
  for the default output.

- [Counts
  wrapper](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
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
