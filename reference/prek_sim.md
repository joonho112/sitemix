# Simulated pre-kindergarten site panel

A fully simulated 50-site panel of pre-kindergarten enrollment records
with four overlapping binary means-test indicators across five school
years. The panel is the worked example used throughout the package
documentation and in the regression tests.

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

No administrative, restricted, or person-level source data of any kind
is read by the builder or represented in the shipped artifacts. Every
row is generated from design constants recorded in
`inst/scripts/build-prek-sim.R`, so the panel can be regenerated from a
plain R installation with no special data access.

The design targets are round numbers chosen to give the package a
didactic example with the features its estimators are built for:
site-year cells ranging from a handful of children to more than a
hundred, four overlapping means-tested indicators, one rare indicator
that produces many zero cells, and enough between-site heterogeneity for
a meaningful empirical-Bayes demonstration. They are not estimates of
any particular program's caseload.

Sites are drawn in three size strata; each site carries a vector of
correlated random effects on the logit scale, so high-need sites are
high-need on every indicator at once; and within a site-year cell the
four indicators are drawn from a Gaussian copula thresholded at that
cell's indicator probabilities. Design targets, calibrated latent
parameters, and a summary of the realized panel are stored under the
`build_info` attribute.

The same builder generates two external artifacts:

- `inst/extdata/prek_sim.csv` for non-R consumers.

- `inst/extdata/prek_sim_counts.rds` for pre-aggregated multivariate
  sufficient counts.

Access both artifacts with
[`system.file()`](https://rdrr.io/r/base/system.file.html).

## See also

- [`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
  for the primary consumer.

- [`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
  for the bundled `prek_sim_counts.rds` consumer.

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
