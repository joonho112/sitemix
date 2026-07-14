# Alabama Pre-K sample panel

A deterministic, anonymized 50-site subset of the Alabama First Class
Pre-K administrative panel. The data demonstrate four overlapping binary
means-test indicators across five school years and are used in package
examples and regression tests.

## Usage

``` r
alprek_subset
```

## Format

A tibble with 7,312 rows and 7 columns:

- student_id:

  Synthetic `STxxxxx` identifier, stable across years for the same
  selected child within this shipped sample build.

- site_id:

  Synthetic site identifier, `"S001"` through `"S050"`.

- year:

  School year, 2021 through 2025.

- frpm:

  Integer 0/1. Free and reduced-price meals eligibility.

- snap:

  Integer 0/1. SNAP enrollment.

- wic:

  Integer 0/1. WIC enrollment.

- tanf:

  Integer 0/1. TANF enrollment.

## Source

Stratified and anonymized subset of the Alabama First Class Pre-K
administrative panel. See `inst/scripts/build-alprek-subset.R` and
`inst/extdata/alprek_subset_provenance.txt`.

## Details

The restricted source panel is not shipped with the package. Rebuild
metadata are stored under the `build_info` attribute of `alprek_subset`.
They include the source-file digest, sampling seed, candidate site
counts, selected-site digest, public schema, and disclosure-audit
summary.

The same builder generates two external artifacts:

- `inst/extdata/alprek_subset.csv` for non-R consumers.

- `inst/extdata/alprek_subset_counts.rds` for pre-aggregated
  multivariate sufficient counts.

Access both artifacts with
[`system.file()`](https://rdrr.io/r/base/system.file.html).

## See also

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  for the primary consumer.

- [`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
  for the bundled `alprek_subset_counts.rds` consumer.

- [`vignette("a1-getting-started")`](https://joonho112.github.io/sitemix/articles/a1-getting-started.md)
  for the applied tutorial.

## Examples

``` r
data(alprek_subset)
attr(alprek_subset, "build_info")$row_count
#> [1] 7312

counts_path <- system.file(
  "extdata",
  "alprek_subset_counts.rds",
  package = "sitemix"
)
counts <- readRDS(counts_path)
head(counts)
#> # A tibble: 6 × 13
#>   site_id  year  n_jt c_jt_frpm c_jt_snap c_jt_wic c_jt_tanf c_jt_frpm_snap
#>   <chr>   <int> <int>     <int>     <int>    <int>     <int>          <int>
#> 1 S001     2021    10         7         9        8         0              6
#> 2 S001     2022    12         6         6        5         0              5
#> 3 S001     2023     9         6         7        4         1              5
#> 4 S001     2024    10         4         6        9         1              4
#> 5 S001     2025     4         3         3        1         1              3
#> 6 S002     2021     6         2         3        1         0              2
#> # ℹ 5 more variables: c_jt_frpm_wic <int>, c_jt_frpm_tanf <int>,
#> #   c_jt_snap_wic <int>, c_jt_snap_tanf <int>, c_jt_wic_tanf <int>

one_year <- subset(alprek_subset, year == 2024)
out <- sm_estimate(
  one_year,
  family = "multivariate",
  indicators = c("frpm", "snap", "wic", "tanf")
)
head(out)
#> sitemix_estimates: 6 rows x 18 columns | family=multivariate | role=summary_uncertainty
#> groups=2 sites=2 years=1 indicators=4 V=FALSE K=FALSE
#> # A tibble: 6 × 18
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 frpm          0.4       0.685 0.155  0.158    10    10
#> 2 S001     2024 snap          0.6       0.886 0.155  0.158    10    10
#> 3 S001     2024 wic           0.9       1.25  0.0949 0.158    10    10
#> 4 S001     2024 tanf          0.1       0.322 0.0949 0.158    10    10
#> 5 S002     2024 frpm          0.222     0.491 0.139  0.167     9     9
#> 6 S002     2024 snap          0.333     0.615 0.157  0.167     9     9
#> # ℹ 9 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>
```
