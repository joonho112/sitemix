# Simulated pre-kindergarten site panel

Simulated enrollment records for 50 pre-kindergarten sites from 2021
through 2025. Each row represents one simulated student and contains
four overlapping binary indicators of program eligibility or
participation. The panel is used in the package examples and regression
tests; it represents no real students or sites.

## Usage

``` r
prek_sim
```

## Format

A tibble with 7,845 rows and 7 columns:

- student_id:

  Generated `STxxxxx` identifier; one per row.

- site_id:

  Generated site identifier, `"S001"` through `"S050"`.

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

Simulated by `inst/scripts/build-prek-sim.R`. See
`inst/extdata/prek_sim_design.txt` for the generative model and the
realized panel summary.

## Details

`inst/scripts/build-prek-sim.R` generates every row from design
constants without reading external records. The chosen parameters
produce different site sizes, correlated indicators, and many zero-count
TANF cells. They illustrate estimation with sparse and overlapping
indicators and are not estimates of any program's caseload.

Sites have three size strata, with enrollment varying by year around
each site's typical size. Correlated site effects on the logit scale and
a Gaussian copula generate associations among the indicators. The
`build_info` attribute records the design targets, calibrated
parameters, and a summary of the resulting panel.

The package also includes two files generated from the same panel:

- `inst/extdata/prek_sim.csv`, a CSV copy of the student rows.

- `inst/extdata/prek_sim_counts.rds`, a table of site-year denominators,
  marginal counts, and pairwise co-occurrence counts.

Locate either file with
[`system.file()`](https://rdrr.io/r/base/system.file.html).

## See also

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  for estimating site-level proportions from the student rows.

- [`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
  for estimation from the bundled count table.

- [`vignette("a1-getting-started")`](https://joonho112.github.io/sitemix/articles/a1-getting-started.md)
  for the applied tutorial.

## Examples

``` r
data(prek_sim)
attr(prek_sim, "build_info")$row_count
#> [1] 7845

counts_path <- system.file(
  "extdata",
  "prek_sim_counts.rds",
  package = "sitemix"
)
counts <- readRDS(counts_path)
head(counts)
#> # A tibble: 6 × 13
#>   site_id  year  n_jt c_jt_frpm c_jt_snap c_jt_wic c_jt_tanf c_jt_frpm_snap
#>   <chr>   <int> <int>     <int>     <int>    <int>     <int>          <int>
#> 1 S001     2021     8         1         1        2         0              0
#> 2 S001     2022    12         0         2        1         0              0
#> 3 S001     2023     6         0         0        1         0              0
#> 4 S001     2024     9         1         2        1         0              0
#> 5 S001     2025     7         3         0        0         0              0
#> 6 S002     2021     7         5         4        2         0              3
#> # ℹ 5 more variables: c_jt_frpm_wic <int>, c_jt_frpm_tanf <int>,
#> #   c_jt_snap_wic <int>, c_jt_snap_tanf <int>, c_jt_wic_tanf <int>

one_year <- subset(prek_sim, year == 2024)
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
#> 1 S001     2024 frpm          0.111     0.340 0.105  0.167     9     9
#> 2 S001     2024 snap          0.222     0.491 0.139  0.167     9     9
#> 3 S001     2024 wic           0.111     0.340 0.105  0.167     9     9
#> 4 S001     2024 tanf          0         0     0.0763 0.167     9     9
#> 5 S002     2024 frpm          0.8       1.11  0.126  0.158    10    10
#> 6 S002     2024 snap          0.7       0.991 0.145  0.158    10    10
#> # ℹ 9 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>
```
