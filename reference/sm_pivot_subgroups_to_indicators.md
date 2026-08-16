# Pivot subgroup aggregate rows into subgroup-as-indicator input (Framing Y)

`sm_pivot_subgroups_to_indicators()` implements **Framing Y** for
school-by-subgroup aggregate rows: each subgroup becomes a D1 marginal
indicator while the original site remains the `site_id`. The [aggregate
wrapper](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
consumes this D1 output. For the alternative framing where subgroups
become composite sites, see the [Framing X
helper](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md).

## Usage

``` r
sm_pivot_subgroups_to_indicators(
  data,
  site_col = "site_id",
  year_col = "year",
  subgroup_col,
  numerator_col,
  denominator_col,
  indicator_set = NULL,
  na_action = c("drop_row", "keep_na"),
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

- indicator_set:

  Character vector or `NULL` (default `NULL`). Optional subgroup labels
  to retain and order in the output. When `NULL`, labels are taken from
  first appearance in `subgroup_col`. Recognized total aliases in this
  vector are normalized to canonical `"ALL"`; alias collisions are
  rejected as duplicate indicators.

- na_action:

  Character scalar. Missing/suppressed row handling. One of `"drop_row"`
  (default; removes rows whose numerator or denominator is missing or
  whose suppression flag is present) or `"keep_na"` (keeps/inserts NA
  rows for downstream suppression handling).

- suppression_col:

  Character scalar or `NULL` (default `NULL`). Optional publisher
  suppression flag column.

- suppression_flag_value:

  Value or vector of values marking publisher suppression in
  `suppression_col`. Defaults to `""`.

## Value

A tibble consumable by the [aggregate
wrapper](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
with `family = "multivariate"` and `aggregate_case = "D1"`. Schema:

- `site_id`:

  Original source site identifier (preserved, not composite).

- `year`:

  Integer year.

- `indicator`:

  Character scalar; the subgroup label (each subgroup becomes a marginal
  indicator).

- `c_jt`, `n_jt`:

  Numerator and denominator from `numerator_col` and `denominator_col`.

- `suppression_flag`:

  Always-present logical. It is `TRUE` for publisher-flagged rows and
  otherwise `FALSE`. Without a source flag, observed rows are `FALSE`;
  synthesized incomplete-grid rows created by `na_action = "keep_na"`
  are `TRUE`.

- `framing`:

  Character scalar; the framing label (`"subgroup_as_indicator"`).

- `source_subgroup`:

  Original publisher subgroup label for observed rows; synthesized
  incomplete-grid rows carry `NA_character_`.

## Details

**Framing X vs Framing Y.** The two pivot helpers solve the same input
problem but produce different schemas (see the [Framing X
details](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md)
for the side-by-side comparison). Pick Framing Y when the analyst's
question is "what does each site's subgroup profile look like?" — each
subgroup becomes a column-like marginal indicator and the downstream D1
estimator computes per-marginal SEs with working-independence
cross-marginal covariance. The fixed total-alias vocabulary documented
for
[`sm_pivot_subgroups_to_sites()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md)
is normalized to canonical `"ALL"` before duplicate and grid checks.
Mixed-level routing is likewise unsupported and must be split upstream.

## See also

- [Framing X
  helper](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md).

- [Aggregate
  wrapper](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
  for the D1 estimator.

- [Fréchet
  diagnostic](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
  for pairwise intervals and projected stress.

- [Suppression
  audit](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
  before pivoting.

- [`vignette("a5-published-aggregates")`](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md)
  for the walkthrough.

Other reshape:
[`sm_pivot_subgroups_to_sites()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md)

## Examples

``` r
# Synthetic publisher file (same as Framing X example)
subgroups <- expand.grid(
  site_id  = paste0("S", sprintf("%03d", 1:5)),
  year     = 2024L,
  subgroup = c("frpm_yes", "frpm_no"),
  stringsAsFactors = FALSE
)
subgroups$c_jt <- c(8, 4, 7, 5, 9, 3, 10, 6, 5, 8)
subgroups$n_jt <- c(12, 6, 11, 9, 13, 7, 15, 10, 8, 12)

pivoted_y <- sm_pivot_subgroups_to_indicators(
  subgroups,
  subgroup_col    = "subgroup",
  numerator_col   = "c_jt",
  denominator_col = "n_jt"
)
head(pivoted_y)
#> # A tibble: 6 × 8
#>   site_id  year indicator source_subgroup  c_jt  n_jt suppression_flag framing  
#>   <chr>   <int> <chr>     <chr>           <int> <int> <lgl>            <chr>    
#> 1 S001     2024 frpm_yes  frpm_yes            8    12 FALSE            subgroup…
#> 2 S001     2024 frpm_no   frpm_no             3     7 FALSE            subgroup…
#> 3 S002     2024 frpm_yes  frpm_yes            4     6 FALSE            subgroup…
#> 4 S002     2024 frpm_no   frpm_no            10    15 FALSE            subgroup…
#> 5 S003     2024 frpm_yes  frpm_yes            7    11 FALSE            subgroup…
#> 6 S003     2024 frpm_no   frpm_no             6    10 FALSE            subgroup…
```
