# Estimate site-year proportions from sufficient counts

`sm_estimate_from_counts()` estimates proportions and standard errors
from one row of counts per site and year. It calls
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
with `from_counts = TRUE`, so you do not need to set the input type. You
can also use `sm_estimate(..., from_counts = TRUE)` directly.

## Usage

``` r
sm_estimate_from_counts(
  data,
  family,
  indicator = NULL,
  indicators = NULL,
  id_cols = c("site_id", "year"),
  accountability_n = 30L,
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

- accountability_n:

  A positive whole number. Rows with `n < accountability_n` receive
  `flag_below_accountability = TRUE`. Defaults to `30L`.

- ...:

  Additional arguments forwarded to
  [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md).

## Value

A `sitemix_estimates` tibble with one row per site-year-indicator. It
has the same columns and scale information as
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md);
see that function's *Value* section for the estimates, standard errors,
optional covariance, and object attributes.

## Details

Each row needs the site and year columns named by `id_cols`, an `n_jt`
denominator, and count columns for the chosen `family`:

- `"binomial"` (A):

  One marginal count named `c_jt_<indicator>`, with its label supplied
  in `indicator`.

- `"multivariate"` (B):

  At least two marginal counts named `c_jt_<indicator>`, plus a
  co-occurrence count for every pair. Pair names follow the order in
  `indicators`: for example, `indicators = c("frpm", "snap")` requires
  `c_jt_frpm`, `c_jt_snap`, and `c_jt_frpm_snap`.

- `"multinomial"` (C):

  At least two category counts in `c_jt_*` columns, summing to `n_jt` in
  each row. Use `indicators` to set the category order explicitly.

For overlapping indicators, `vjt = TRUE` requests covariance and checks
whether the counts can come from a common sample. This check supports
two or three indicators. Counts with four or more indicators are
accepted when `vjt = FALSE`; requesting covariance for them raises
`sitemix_error_input_indicator_count`. Use student rows with
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
if covariance is needed for four or more indicators.

Counts constructed from the same retained student rows, including the
same missing-value exclusions, give the same estimates and standard
errors up to numerical rounding when the family, indicator order, and
estimation options match. Covariance values also agree for supported
requests. The `input_mode` column distinguishes the two input types.

Do not pass `from_counts` to this function: it is set internally, and
supplying it raises `sitemix_error_invalid_from_counts`. Use
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
to set the input type yourself, or
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
for published aggregate rows.

## See also

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  for estimation options and returned columns.

- [`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)
  for published aggregate rows.

- [`sm_diagnose()`](https://joonho112.github.io/sitemix/reference/sm_diagnose.md)
  for checking estimates and their uncertainty.

- [`vignette("a2-input-formats")`](https://joonho112.github.io/sitemix/articles/a2-input-formats.md)
  for choosing an input format.

- [`vignette("a3-scenario-binomial")`](https://joonho112.github.io/sitemix/articles/a3-scenario-binomial.md)
  for a binomial example using counts.

- [`vignette("m2-scalar-se-binomial")`](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
  for why sufficient counts reproduce the binomial calculations from
  student rows.

Other estimation:
[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md),
[`sm_estimate_from_aggregates()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_aggregates.md)

## Examples

``` r
counts_path <- system.file(
  "extdata", "prek_sim_counts.rds",
  package = "sitemix", mustWork = TRUE
)
counts <- readRDS(counts_path)

# Estimate SNAP proportions for 2024 from sufficient counts:
snap_counts <- counts[
  counts$year == 2024,
  c("site_id", "year", "n_jt", "c_jt_snap")
]
est <- sm_estimate_from_counts(
  snap_counts,
  family    = "binomial",
  indicator = "snap"
)
head(est, 5)
#> sitemix_estimates: 5 rows x 18 columns | family=binomial | role=summary_uncertainty
#> groups=5 sites=5 years=1 indicators=1 V=FALSE K=FALSE
#> # A tibble: 5 × 18
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 snap          0.222     0.491  0.139 0.167     9     9
#> 2 S002     2024 snap          0.7       0.991  0.145 0.158    10    10
#> 3 S003     2024 snap          0.125     0.361  0.117 0.177     8     8
#> 4 S004     2024 snap          0.286     0.564  0.121 0.134    14    14
#> 5 S005     2024 snap          0.75      1.05   0.153 0.177     8     8
#> # ℹ 9 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>
unique(est$estimate_scale)
#> [1] "arcsine"
```
