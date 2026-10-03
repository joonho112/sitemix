# Analyzing overlapping indicators and mutually exclusive categories

Abstract

Choose between overlapping binary indicators and mutually exclusive
categories, then estimate their site-level proportions and covariance.
Examples show how to interpret raw-scale matrices alongside transformed
standard errors and why multinomial covariance can be singular.

Estimates for several indicators measured on the same students can have
dependent sampling errors. To retain their covariance, set `vjt = TRUE`.
First choose the estimation family from how the variables are recorded.

The examples use 50 simulated sites in `prek_sim` and an added simulated
language category. They describe no real children or programs and are
not empirical Pre-K findings. See [Choosing an input
format](https://joonho112.github.io/sitemix/articles/a2-input-formats.md)
for the distinction between student records, counts, and published
aggregates.

## Choose from the observed variables

| Data | Family and argument | Information used for covariance |
|:---|:---|:---|
| Several binary indicators on the same students; a student may have more than one | `family = "multivariate"`, `indicators = c(...)` (B) | Marginal and pairwise co-occurrence counts from the same observations |
| One categorical variable; every retained student belongs to exactly one category | `family = "multinomial"`, `indicator = "..."` (C) | Category counts that sum to the shared site-year denominator |

FRPM, SNAP, WIC, and TANF are overlapping binary indicators: a student
can have any combination of these statuses. Primary language, recorded
as exactly one of English, Spanish, or Other, is a multinomial category.
The categories must be mutually exclusive and together cover all
retained observations. A set of categories with omitted observations or
overlapping membership does not meet that requirement.

For B, the same student records supply the marginal proportions and the
proportion positive for each pair of indicators. A pair’s covariance can
be positive, negative, or zero; the possibility of overlapping
membership does not determine its value. With C, the category
proportions sum to one, and the covariance matrix must respect that
constraint.

Both families also accept complete sufficient counts through
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md).
B requires every marginal and pairwise co-occurrence count from a common
sample, in the supplied indicator order. Count-based covariance is
supported for two or three overlapping indicators; use student records
for `vjt = TRUE` with four or more. C requires every category count,
with an exact sum of `n_jt` per site-year. A category with zero count
must remain in the specified category set.

Separate published marginal counts, without the joint counts, use the D1
aggregate method. Equal denominators alone do not establish that the
same students were observed. D1 working independence does not recover
joint counts and is not a multinomial composition; see [Estimating
proportions from published
aggregates](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md).

## Estimate overlapping binary indicators

Use `indicators` to name the four binary columns and `vjt = TRUE` to
request their joint covariance:

``` r

benefits_2024 <- sm_estimate(
  subset(prek_sim, year == 2024),
  family     = "multivariate",
  indicators = c("frpm", "snap", "wic", "tanf"),
  vjt        = TRUE
)
head(benefits_2024, 4)
#> sitemix_estimates: 4 rows x 20 columns | family=multivariate | role=summary_uncertainty
#> groups=1 sites=1 years=1 indicators=4 V=TRUE K=TRUE
#> # A tibble: 4 × 20
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 frpm          0.111     0.340 0.105  0.167     9     9
#> 2 S001     2024 snap          0.222     0.491 0.139  0.167     9     9
#> 3 S001     2024 wic           0.111     0.340 0.105  0.167     9     9
#> 4 S001     2024 tanf          0         0     0.0763 0.167     9     9
#> # ℹ 11 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>,
#> #   V <list>, K <int>
```

The result has four rows per site-year, one per indicator. Each row
carries the same site-year covariance object in `V`; these are repeated
copies of one group matrix, not four independent matrices. The first
object is for site `S001` in 2024:

``` r

V_S001 <- benefits_2024$V[[1L]]
round(as.matrix(V_S001), 4)
#>         frpm    snap     wic   tanf
#> frpm  0.0110 -0.0027 -0.0014 0.0000
#> snap -0.0027  0.0192  0.0096 0.0000
#> wic  -0.0014  0.0096  0.0110 0.0000
#> tanf  0.0000  0.0000  0.0000 0.0058
V_S001$vcov_scale
#> [1] "raw"
V_S001$vcov_method
#> [1] "sur"
```

The matrix rows and columns identify the indicators, and `vcov_method`
is `"sur"`, the package’s label for covariance calculated from their
joint observations. `vcov_scale = "raw"` means that it describes the raw
proportions in `theta_raw`. This scale is a package choice; the matrix
is not automatically transformed with the row estimates.

By default, `theta_hat` and `se` use the arcsine scale. Consequently,
`sqrt(diag(V))` should be compared with `se_raw` for B, not with the
transformed `se`. Keep `vcov_scale` and the matrix’s indicator order
when using the result in a joint analysis.

## Estimate mutually exclusive categories

`prek_sim` has no categorical language variable. The following code
simulates one label for every student, using the fixed seed for this
article. The specified probabilities are example settings.

``` r

language_data <- transform(
  prek_sim,
  primary_language = sample(
    c("english", "spanish", "other"),
    nrow(prek_sim),
    replace = TRUE,
    prob    = c(0.70, 0.25, 0.05)
  )
)
language_data <- language_data[
  c("student_id", "site_id", "year", "primary_language")
]
head(language_data, 5)
#>   student_id site_id year primary_language
#> 1    ST00001    S001 2021          english
#> 2    ST00002    S001 2021          english
#> 3    ST00003    S001 2021          english
#> 4    ST00004    S001 2021          spanish
#> 5    ST00005    S001 2021          english
```

Use `indicator` to name this single categorical column and
`family = "multinomial"` to estimate all of its category proportions:

``` r

language_2024 <- sm_estimate(
  subset(language_data, year == 2024),
  family    = "multinomial",
  indicator = "primary_language",
  vjt       = TRUE
)
head(language_2024, 6)
#> sitemix_estimates: 6 rows x 20 columns | family=multinomial | role=summary_uncertainty
#> groups=2 sites=2 years=1 indicators=3 V=TRUE K=TRUE
#> # A tibble: 6 × 20
#>   site_id  year indicator theta_raw theta_hat se_raw    se     n n_eff
#>   <chr>   <int> <chr>         <dbl>     <dbl>  <dbl> <dbl> <int> <dbl>
#> 1 S001     2024 english       0.778     1.08  0.139  0.167     9     9
#> 2 S001     2024 other         0         0     0.0763 0.167     9     9
#> 3 S001     2024 spanish       0.222     0.491 0.139  0.167     9     9
#> 4 S002     2024 english       0.7       0.991 0.145  0.158    10    10
#> 5 S002     2024 other         0         0     0.0708 0.158    10    10
#> 6 S002     2024 spanish       0.3       0.580 0.145  0.158    10    10
#> # ℹ 11 more variables: estimate_scale <chr>, transform <chr>, var_method <chr>,
#> #   flag_small_n <lgl>, flag_zero_cell <lgl>, input_mode <chr>,
#> #   flag_suppressed <lgl>, framing <chr>, flag_below_accountability <lgl>,
#> #   V <list>, K <int>
```

The result has three rows per site-year, one per category. Within each
site-year, the raw proportions in `theta_raw` sum to one. The covariance
matrix for `S001` is:

``` r

V_lang_S001 <- as.matrix(language_2024$V[[1L]])
round(V_lang_S001, 4)
#>         english other spanish
#> english  0.0192     0 -0.0192
#> other    0.0000     0  0.0000
#> spanish -0.0192     0  0.0192
```

This matrix also uses the raw-proportion scale, while the default
`theta_hat` and `se` remain on the arcsine scale. Its method label is
`"multinomial"`. Because the category proportions have a fixed sum, the
covariance row sums are zero up to numerical rounding:

``` r

rowSums(V_lang_S001)
#> english   other spanish 
#>       0       0       0
```

This sum-to-one constraint is called the **simplex** constraint. It
makes the covariance matrix singular: the category estimates cannot vary
independently. Singularity is expected here and does not itself indicate
an invalid matrix.

If `S` categories have positive observed counts, the matrix’s
`positive_support` field is `S` and `matrix_rank` records `S - 1`. This
is the analytic support rank. It remains the recorded value when an
exact census under a supplied finite-population design makes the actual
covariance matrix zero. A census therefore has numerical rank zero even
if `matrix_rank` is positive.

A category with no observations at a site has an exact zero row and
column in its multinomial matrix. This zero in the estimated matrix does
not establish that membership in the category is impossible. With the
default Wilson boundary treatment and no finite-population correction,
that category still has a positive scalar `se_raw`. Its squared value
does not replace the zero matrix diagonal, which is needed to preserve
the simplex constraint. The `diag_contract` field records this boundary
exception. At an exact census, the finite-population correction makes
sampling uncertainty zero. See [Covariance for multinomial
proportions](https://joonho112.github.io/sitemix/articles/m4-multinomial-simplex.md)
for the formulas and the distinction between scalar boundary uncertainty
and the joint matrix.

## Use individual estimates or joint covariance

For analyses that treat each indicator separately, select the estimates,
standard errors, scale, and calculation method from this complete
example:

``` r

scalar_inputs <- split(
  as.data.frame(benefits_2024[, c(
    "site_id", "year", "indicator", "theta_hat", "se",
    "estimate_scale", "var_method"
  )]),
  benefits_2024$indicator
)
names(scalar_inputs)
#> [1] "frpm" "snap" "tanf" "wic"
```

These tables keep `theta_hat` with its matching `se` and
`estimate_scale`. They omit cross-indicator covariance, so they are not
sufficient for comparisons or combinations that require joint
uncertainty. Use the complete site-year `V` matrix and its corresponding
raw estimates for a raw-scale joint analysis. Match the indicator order
before calculating contrasts. A linear contrast uses the covariance
matrix without requiring its inverse, so a valid singular multinomial
matrix can still be used. See [Using estimates and covariance matrices
in further
analyses](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md)
for the required checks and extraction examples.

## Check the example results

The following checks confirm the expected row counts, methods, raw B
covariance scale, positive semidefiniteness of the first B matrix, and
the zero-sum constraint for the first C matrix:

``` r

stopifnot(nrow(benefits_2024) == 200L)  # 50 sites × 4 indicators
stopifnot(inherits(benefits_2024$V[[1]], "sm_vcov"))
stopifnot(benefits_2024$V[[1]]$vcov_method == "sur")
stopifnot(benefits_2024$V[[1]]$vcov_scale == "raw")
stopifnot(min(eigen(as.matrix(benefits_2024$V[[1]]),
                    symmetric = TRUE)$values) >= -1e-10)
stopifnot(nrow(language_2024) == 150L)  # 50 sites × 3 categories
stopifnot(language_2024$V[[1]]$vcov_method == "multinomial")
stopifnot(max(abs(rowSums(as.matrix(language_2024$V[[1]])))) < 1e-10)
```

## Examine the uncertainty in more detail

- [Checking estimates and handling suppressed
  data](https://joonho112.github.io/sitemix/articles/a6-diagnostics-and-suppression.md)
  for `level = "vcov"` diagnostics including the smallest eigenvalue,
  PSD tolerance, and estimate/covariance scale compatibility.
- [Using estimates and covariance matrices in further
  analyses](https://joonho112.github.io/sitemix/articles/a8-downstream-workflows.md)
  for using estimates and joint covariance in another analysis.
- [Covariance for overlapping binary
  indicators](https://joonho112.github.io/sitemix/articles/m3-multivariate-sur-covariance.md)
  for the formal SUR derivation.
- [Covariance for multinomial
  proportions](https://joonho112.github.io/sitemix/articles/m4-multinomial-simplex.md)
  for the formal simplex-covariance derivation.
