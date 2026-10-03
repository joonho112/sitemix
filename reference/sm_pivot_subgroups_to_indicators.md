# Prepare subgroup proportions as indicators within each site

`sm_pivot_subgroups_to_indicators()` prepares published subgroup counts
for comparing subgroup proportions within each site. It keeps the
original `site_id` and uses each subgroup label as an `indicator`,
preserving that subgroup's numerator and denominator. The documentation
calls this **Framing Y**.

Pass the result to
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
with `family = "multivariate"` for marginal aggregate estimates (D1).
This helper reshapes the counts; it does not estimate proportions,
standard errors, or dependence between subgroups.

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

- indicator_set:

  A character vector of at least two distinct subgroup labels to retain
  in the specified order, or `NULL` (default). With `NULL`, labels
  follow their first appearance in `subgroup_col`. Supplied labels must
  occur in the data. Recognized total labels become `"ALL"`; labels that
  then coincide are rejected as duplicate indicators.

- na_action:

  A single string: `"drop_row"` (default) or `"keep_na"`. The default
  removes the entire site-year group if any selected subgroup has a
  missing numerator, missing denominator, or publisher suppression flag.
  `"keep_na"` retains these groups and inserts `NA` rows for absent
  subgroup indicators.

- suppression_col:

  A single column name, or `NULL` (default). Names the publisher
  suppression flag column. With `NULL`, an existing `suppression_flag`
  column is used if present. If neither source is available, source rows
  receive `suppression_flag = FALSE`.

- suppression_flag_value:

  Value or vector of values marking publisher suppression in the flag
  column. Defaults to `""`.

## Value

A tibble with one row per retained site-year-subgroup, ordered by site,
year, and `indicator_set`. It is structured for
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
with `family = "multivariate"` and `aggregate_case = "D1"`. It contains:

- `site_id`:

  Original source site identifier.

- `year`:

  Integer year.

- `indicator`:

  Character scalar; the subgroup label (each subgroup becomes a marginal
  indicator).

- `c_jt`, `n_jt`:

  Numerator and denominator from `numerator_col` and `denominator_col`.

- `suppression_flag`:

  Always-present logical. It is `TRUE` for publisher-flagged rows and
  otherwise `FALSE`. Flags come from `suppression_col`, or from an
  existing `suppression_flag` column when that argument is `NULL`.
  Without either source flag column, observed rows are `FALSE`; missing
  subgroup rows inserted by `na_action = "keep_na"` are `TRUE`.

- `framing`:

  Character scalar; the framing label (`"subgroup_as_indicator"`).

- `source_subgroup`:

  Original publisher subgroup label for observed rows; inserted
  missing-subgroup rows carry `NA_character_`.

The `indicator_set` attribute records the selected subgroup order. The
`framing` and `na_action` attributes record how the table was prepared.
An error is raised if no site-year groups remain or fewer than two
indicators are retained per group.

## Details

**Analysis unit and denominator.** Each returned site-year has several
subgroup indicators. Their proportions are conditional on membership in
each subgroup, using that subgroup's own denominator. Equal counts or
denominators do not show that the same observational units contributed
to different indicators. For example, two disjoint subgroups can have
the same number of students. Subgroups can also overlap; labels and
marginal counts alone do not identify the joint counts.

When estimating D1 proportions, set `sampling_relation` in
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
from the source information: `"same_units"` only when the marginals
describe the same observed units, `"different_units"` when they differ,
or `"unknown"` when this is not known. The pivot does not determine this
relationship. If covariance is requested, D1 uses working independence;
its off-diagonal zeros are an assumption. See
[`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
for the conditions on raw pairwise intervals and projected stress
scenarios.

Use
[`sm_pivot_subgroups_to_sites()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md)
with its default `partition_target = "none"` when each site-subgroup
pair should instead have its own site identifier (Framing X). Both
defaults preserve subgroup-specific numerators and denominators. If the
question concerns subgroup shares of a common site total, use that
function's explicit composition targets; merely representing subgroups
as indicators does not create multinomial composition counts.

**Subgroup selection and missing rows.** `indicator_set` selects and
orders subgroup labels. Each retained site-year must have at least two
indicators and the same selected indicator set. With
`na_action = "drop_row"`, any missing count or publisher-suppressed
indicator removes the entire site-year group. With `"keep_na"`, the
function retains those groups and inserts missing subgroup rows with
`NA` counts and `suppression_flag = TRUE`. These are missing values, not
zero counts. Review the suppression and hidden-denominator settings in
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
before estimation.

Recognized total labels, listed in
[`sm_pivot_subgroups_to_sites()`](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md),
become `"ALL"` before duplicate and indicator-set checks.
`source_subgroup` retains the publisher's spelling for observed rows.
Split files containing school, district, or state rows by reporting
level before using either helper; these functions do not separate
levels.

## See also

- [Subgroup-as-site and composition
  counts](https://joonho112.github.io/sitemix/reference/sm_pivot_subgroups_to_sites.md)
  for the alternative analysis units and denominators.

- [Aggregate
  estimation](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
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
