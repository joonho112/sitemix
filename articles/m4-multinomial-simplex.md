# Covariance for multinomial proportions

Abstract

Explains the covariance of proportions in mutually exclusive, exhaustive
categories. Derives the simplex constraint and support rank, then
distinguishes the returned raw-scale matrix from scalar boundary
uncertainty and from a zero-variance census matrix.

## Category proportions that sum to one

Scenario C describes a categorical outcome for which every retained
student belongs to exactly one category. For example, a recorded primary
language might be English, Spanish, or Other. The category proportions
sum to one, so their uncertainties are linked: increasing one proportion
requires a decrease elsewhere. The covariance matrix records this
constraint and is singular even when every category is observed.

`sm_estimate(family = "multinomial", vjt = TRUE)` returns one row per
site-year and category, with the same covariance object repeated across
the group’s `V` entries. Student input uses a categorical column named
by `indicator`. Counts input uses `from_counts = TRUE`, `n_jt`, and a
complete set of `c_jt_<category>` columns whose counts sum to `n_jt`.
These categories must be mutually exclusive and exhaustive for the same
observed students; separate marginal rates do not define this
composition.

## 1. The multinomial sampling model

For one site-year, suppose n=n\_{jt} independent students each have the
same probabilities \pi\_{jt}=(\pi\_{jt,1},\ldots,\pi\_{jt,K})^\top of
falling in the K categories. The count vector follows

(C\_{jt,1}, \ldots, C\_{jt,K}) \mid n\_{jt}, \pi\_{jt} \\\sim\\
\mathrm{Multinomial}(n\_{jt}, \pi\_{jt}),

where \pi\_{jt,k}\ge0 and \sum_k\pi\_{jt,k}=1. This set of probability
vectors is called the *simplex*. The estimated probabilities are
\hat\pi\_{jt,k}=C\_{jt,k}/n\_{jt}, returned as `theta_raw`; they are
distinct from the unknown population probabilities \pi\_{jt,k}.

## 2. From the probability model to an estimated covariance

For one student, let Y_k equal one for category k and zero otherwise.
Exactly one component is one. Thus Y_k\\Y\_{k'}=0 for distinct
categories, giving covariance -\pi_k\pi\_{k'}; the diagonal variance is
\pi_k(1-\pi_k). Independence across students then gives

\boxed{ \operatorname{Cov}(\hat\pi\_{jt})=\Sigma\_{jt} \\=\\
\frac{1}{n\_{jt}}\bigl(\mathrm{diag}(\pi\_{jt}) -
\pi\_{jt}\pi\_{jt}^{\top}\bigr). } \tag{M4.1}

These are the multinomial count moments divided by n^2 (Agresti, 2013,
Section 1.2.2, equation (1.3)). Distinct categories with positive
probabilities have negative covariance under this model. If either
probability is zero, that cross-covariance is zero.

The package substitutes the observed proportions for the unknown
probabilities. Suppressing the site-year subscripts, write
M=\mathrm{diag}(\hat\pi)-\hat\pi\hat\pi^\top. The default matrix is
\widehat V\_{\mathrm{plugin}}=M/n. With
`bias_correction = "binomial_bc"`, it is M/(n-1) for n\>1. The latter
corrects the finite-sample bias of the empirical covariance. Both are
estimated covariance matrices, whereas M4.1 uses the population
probabilities. For indicator vectors representing one category per
student, the centered cross-product matrix in [Covariance for
overlapping binary
indicators](https://joonho112.github.io/sitemix/articles/m3-multivariate-sur-covariance.md)
is Q=n\\M.

## 3. Sampling from a fixed population

Supplying `fpc = N` assumes simple random sampling without replacement
(SRSWOR) of n students from the site-year population of size N\ge n.
Define

q=\begin{cases} 0, & N=n,\\ (N-n)/(N-1), & N\>n. \end{cases}

The plug-in and design-corrected estimates are

\widehat V\_{\mathrm{plugin}}=qM/n,\qquad \widehat V\_{\mathrm{design}}=
\begin{cases} 0\_{K\times K}, & N=n,\\ (N-n)M/\[N(n-1)\], & N\>n\text{
and }n\>1. \end{cases} \tag{M4.2}

The second rule is selected by `bias_correction = "binomial_bc"`.
Relative to each no-FPC matrix, the multipliers are q and 1-n/N,
respectively. [Sampling uncertainty in site-level
proportions](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md)
explains the scalar sampling factor, and [Covariance for overlapping
binary
indicators](https://joonho112.github.io/sitemix/articles/m3-multivariate-sur-covariance.md)
shows its extension to a vector of observations. Substituting Q=n\\M
into the M3 formulas gives M4.2.

Both rules multiply the entire matrix, preserving positive
semidefiniteness and the simplex identity \widehat V\mathbf1=0. A census
N=n has zero sampling covariance, including N=n=1. Otherwise
`binomial_bc` requires n\>1. The covariance object’s
`fpc_variance_multiplier` records q, while `variance_multiplier_applied`
records the factor used by its selected rule. FPC does not change
`n_eff`.

## 4. Why the matrix is singular

Because category proportions sum to one, their total has no sampling
variation. Algebraically, M\mathbf1=0. Each zero-count category also
contributes a zero row and column to the empirical matrix. A zero
observed count does not establish that the population probability is
zero or that the category is impossible.

Let S=\\\\k:C\_{jt,k}\>0\\ be the observed positive support. To see why
the rank is S-1, consider any vector a and put \bar a=\sum_k\hat\pi_k
a_k. Then

a^\top M a=\sum_k\hat\pi_k(a_k-\bar a)^2.

This is nonnegative and is zero exactly when a is constant over the
positive-support categories; values outside that support are
unrestricted. There are therefore S-1 independent directions with
variation. Full support gives rank K-1, and a single observed category
gives rank zero. A positive whole-matrix multiplier preserves this
mathematical rank.

Scenario C records `positive_support = S` and `matrix_rank = S - 1`
without estimating rank from eigenvalues. The recorded value describes
the empirical support even when a census multiplier makes the actual
matrix zero. In that case the actual matrix rank is zero, while the
recorded `matrix_rank` can remain positive. Check `sampling_design` and
`sampling_fraction` along with the matrix when interpreting that value.

The constructor checks PSD, the simplex identity, and the recorded
support-rank relation. Scenario C records `psd_repair = "none"` because
no projection or other repair is applied. A singular matrix can still be
used to compute the variance of a linear contrast as a^\top\widehat V a;
this calculation needs no matrix inverse.

## 5. Matrix scale and category-specific standard errors

The returned covariance has `vcov_method = "multinomial"` and
`vcov_scale = "raw"`: it describes the vector of `theta_raw`
proportions. The row’s `theta_hat` and `se` use the selected
transformation, which is arcsine by default (Lee, 2026). Align matrix
coordinates using `V[[i]]$indicator_order`, and distinguish `se_raw^2`
from transformed `se^2` when comparing the diagonal with scalar
uncertainty.

For interior categories, the diagonal agrees with `se_raw^2`. At a
boundary, C_k=0 or C_k=n, the empirical matrix keeps an exact zero row
and column. The default Wilson option instead assigns each boundary
category a scalar uncertainty surrogate. This can make `se_raw` positive
while its matrix diagonal stays zero, preserving the simplex identity.
With no boundary surrogate the object records
`diag_contract = "row_se_raw_squared"`; with Wilson boundary uncertainty
it records `"row_se_raw_squared_except_boundary_surrogates"` and
`matrix_boundary_rule = "simplex_preserve"`. At a census, the scalar
surrogates also become zero. Agresti–Coull boundary uncertainty is not
supported for `vjt = TRUE`.

With `binomial_bc`, the covariance object’s `variance_rule` and applied
multiplier are uniform across all coordinates, including zero rows and
columns. A boundary category’s scalar calculation can still use the
plug-in or Wilson rule; its Wilson surrogate uses the conventional q
when FPC is supplied. The validator allows this scalar/matrix rule
difference only where the matrix row and column are exactly zero.
Replacing a zero matrix diagonal with a positive scalar surrogate would
destroy the simplex row-sum constraint.

## 6. Check the estimates on simulated categories

The example assigns a simulated primary language to each row of
`prek_sim`, a simulated 50-site pre-kindergarten panel. The category
probabilities are example settings; the resulting proportions describe
no real children, sites, or programs. The fixed seed makes the offline
calculation reproducible.

The checks below verify that raw proportions sum to one, matrix rows sum
to zero, and the recorded method, scale, support rank, and matrix
variance rule agree with the Scenario C calculation.

``` r

data(prek_sim, package = "sitemix")
language_data <- transform(
  prek_sim,
  primary_language = sample(c("english", "spanish", "other"),
                            nrow(prek_sim), replace = TRUE,
                            prob = c(0.70, 0.25, 0.05))
)
est <- sm_estimate(
  subset(language_data, year == 2024),
  family    = "multinomial",
  indicator = "primary_language",
  vjt       = TRUE
)
V1 <- as.matrix(est$V[[1L]])

# Raw proportions sum to one in each site-year
per_site <- aggregate(theta_raw ~ site_id, data = est, FUN = sum)
stopifnot(all(abs(per_site$theta_raw - 1) < 1e-12))

# The empirical covariance has zero row sums
stopifnot(max(abs(rowSums(V1))) < 1e-12)

# Check the recorded method and scale
stopifnot(est$V[[1L]]$vcov_method == "multinomial")
stopifnot(est$V[[1L]]$vcov_scale == "raw")
stopifnot(est$V[[1L]]$psd_repair == "none")

# Check the analytic support rank
stopifnot(est$V[[1L]]$matrix_rank == est$V[[1L]]$positive_support - 1L)

# Check the global matrix variance rule and multiplier
stopifnot(length(unique(est$V[[1L]]$variance_rule)) == 1L)
stopifnot(length(unique(est$V[[1L]]$variance_multiplier_applied)) == 1L)
```

The two sum checks use an absolute tolerance of `1e-12` for this
example. They compare observed proportions and their estimated
covariance; they do not check the unknown population probabilities. The
constructor separately uses matrix-scale tolerances for its numerical
checks.

### A zero-count category and a census

The following complete counts have two observed categories and one
zero-count category. The ordinary estimate illustrates the scalar Wilson
exception. Treating the same 20 students as a census makes all sampling
uncertainty zero while retaining the analytic support rank.

``` r

category_counts <- data.frame(
  site_id = "example", year = 2024, n_jt = 20L,
  c_jt_a = 10L, c_jt_b = 10L, c_jt_c = 0L
)
observed <- sm_estimate(
  category_counts, family = "multinomial", from_counts = TRUE,
  indicators = c("a", "b", "c"), vjt = TRUE
)
census <- sm_estimate(
  category_counts, family = "multinomial", from_counts = TRUE,
  indicators = c("a", "b", "c"), fpc = 20L, vjt = TRUE
)
V_observed <- as.matrix(observed$V[[1L]])
V_census <- as.matrix(census$V[[1L]])
zero_category <- which(observed$indicator == "c")
stopifnot(all(V_observed["c", ] == 0),
          all(V_observed[, "c"] == 0),
          observed$se_raw[zero_category] > 0)
stopifnot(all(V_census == 0), all(census$se_raw == 0),
          all(census$se == 0), census$V[[1L]]$matrix_rank == 1L)
data.frame(
  estimate = c("observed sample", "census"),
  positive_support = c(observed$V[[1L]]$positive_support,
                       census$V[[1L]]$positive_support),
  recorded_rank = c(observed$V[[1L]]$matrix_rank,
                    census$V[[1L]]$matrix_rank),
  actual_matrix_rank = c(qr(V_observed)$rank, qr(V_census)$rank),
  zero_category_se_raw = c(observed$se_raw[zero_category],
                           census$se_raw[zero_category])
)
#>          estimate positive_support recorded_rank actual_matrix_rank
#> 1 observed sample                2             1                  1
#> 2          census                2             1                  0
#>   zero_category_se_raw
#> 1           0.04110411
#> 2           0.00000000
```

Both rows record positive support two and analytic rank one. The census
matrix has actual rank zero. In the ordinary estimate, the positive
scalar uncertainty for category c does not change its zero covariance
row or column.

## 7. Where to go next

- [Covariance for overlapping binary
  indicators](https://joonho112.github.io/sitemix/articles/m3-multivariate-sur-covariance.md)
  — covariance for overlapping binary indicators observed on the same
  students.
- [Sampling uncertainty from published
  aggregates](https://joonho112.github.io/sitemix/articles/m5-aggregate-engines.md)
  for separate published marginal rates, whose cross-covariance is
  unidentified without joint information. D1 uses a working-independence
  assumption; Scenario C requires student rows or complete category
  counts defining a common composition.
- [Analyzing overlapping indicators and mutually exclusive
  categories](https://joonho112.github.io/sitemix/articles/a4-multivariate-multinomial.md)
  for examples of interpreting the estimates and covariance together.

## References

Agresti, A. (2013). *Categorical data analysis* (3rd ed.). Wiley.

Lee, J. (2026). *sitemix: Site- and group-level proportions, rates, and
sampling uncertainty*. <https://joonho112.github.io/sitemix/>
