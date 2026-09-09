# Sampling uncertainty from published aggregates

Abstract

Explain what published numerator and denominator counts identify. The
single-indicator calculation reuses binomial standard errors; multiple
marginals require an assumption about their covariance. Suppressed
values and unknown joint counts have different consequences.

A published table can preserve enough information to estimate individual
proportions while omitting the joint observations needed for their
covariance. The D0 and D1 aggregate calculations address these two
cases. The equations below describe observed, identified counts;
suppression handling is discussed separately. Examples use counts
derived from the fully simulated `prek_sim` data and do not describe
real children or programs.

## Information in the input

D0 contains one numerator and denominator per site-year for one
indicator. D1 contains several marginal numerators and denominators,
with no joint counts supplied to the estimator. Each D1 marginal can
have its own n\_{jt,k}. A common denominator value does not establish
common sampled units.

In long form, `indicator` labels the marginal and `c_jt`, `n_jt` contain
its counts. Wide form uses `c_jt_<indicator>` and either a shared `n_jt`
or `n_jt_<indicator>` columns. Both forms describe the same quantities:

| Symbol | Meaning | Input or output |
|:---|:---|:---|
| C\_{jt}^{\mathrm{agg}} | D0 numerator, 0\le C\le n | input numerator |
| n\_{jt}^{\mathrm{agg}} | D0 positive integer denominator | input denominator |
| C\_{jt,k} | D1 marginal numerator | input numerator for indicator k |
| n\_{jt,k} | D1 marginal denominator | input denominator for indicator k |
| \Sigma\_{jt}^{\mathrm{D1}} | Working covariance on the reported scale | `V[[i]]$matrix` |

## One observed proportion (D0)

D0 uses the same scalar binomial calculations as Scenario A counts. For
valid, non-suppressed counts that agree exactly, with matching
estimation options, the numerical outputs agree up to floating-point
rounding:

\hat\pi\_{jt}^{D0} \\=\\ C\_{jt}^{\mathrm{agg}} /
n\_{jt}^{\mathrm{agg}}, \qquad s\_{jt}^{D0} = s\_{jt}^{A}. \tag{M5.1}

This is an implementation equivalence for `theta_raw`, `theta_hat`,
`se_raw`, `se`, and `n`. Information about the input path, aggregate
case, suppression, and object attributes can differ. The input paths
also have different validation rules, so an error from one path need not
be identical to an error from the other.

The same counts produce the same formula, but the way a publisher
collected them determines whether its assumptions are appropriate. Under
the IID binomial model, `C/n` estimates a common probability within a
site-year. Under a specified finite-population SRSWOR design, the
sampling variance uses the corresponding finite-population rule. See
[Sampling uncertainty in site-level
proportions](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md)
for the distinction and [Binomial standard errors and
transformations](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
for transformations, corrections, and boundary handling. The aggregate
file format alone establishes neither design.

## Several marginal proportions (D1)

D1 does not receive the joint observations needed to estimate dependence
between indicators. When `vjt = TRUE`, it constructs a
working-independence matrix on the reported row scale:

\boxed{ \Sigma\_{jt}^{D1} \\=\\ \mathrm{diag}\bigl(s\_{jt,1}^2, \ldots,
s\_{jt,K}^2\bigr). } \tag{M5.2}

The off-diagonals are set to zero as an explicit working assumption;
they are not identified zeros. `vcov_method = "working_independence"`
records this choice.

With a keyed site-year population size **fpc = N**, each D1 diagonal
uses its own marginal denominator n_k. Plug-in rows use

q_k=\begin{cases} 0, & N=n_k,\\ (N-n_k)/(N-1), & N\>n_k, \end{cases}

including the package’s explicit census case N=n_k=1. The SRSWOR setting
is discussed by Cochran (1977). Interior rows using `binomial_bc` use
the design multiplier (N-n_k)/N; boundary surrogates retain the plug-in
multiplier. The single covariance object stores one population size plus
coordinate-aligned sampling fractions and multipliers. With varying
denominators, the matrix’s `n_jt` and `n_eff` are missing because no one
value describes the group; the row denominators remain available.
`diag_contract = "row_se_squared"` records that the matrix diagonal
matches row `se^2`. The supplied population size must be appropriate for
every marginal; subgroup labels do not provide that information.

Which units were sampled is a separate question from whether denominator
values match. The D1 API accepts `sampling_relation = "same_units"`,
`"different_units"`, or `"unknown"` (the default), mapping to
`d1_regime = "D1a"`, `"D1b"`, or `"unknown"`. Equal denominators alone
never establish that the marginals were observed on the same units.
`denominator_pattern` records `"common"` or `"varying"` independently,
and `d1_regime_by_group` preserves both facts for every site-year. Every
D1 group must contain the same ordered indicator set.

### Why the marginals leave covariance unknown

For two Bernoulli variables on common units, write their probabilities
as p and q, and their joint success probability as r. The four joint
cell probabilities are

|       | Y_2=1 | Y_2=0   |
|:------|:------|:--------|
| Y_1=1 | r     | p-r     |
| Y_1=0 | q-r   | 1-p-q+r |

Requiring each cell to be nonnegative gives \max(0,p+q-1)\le
r\le\min(p,q). This direct derivation gives the classical pairwise
Fréchet–Hoeffding range (Fréchet, 1951; Hoeffding, 1994; Nelsen, 2006).
The covariance of individual outcomes is r-p\\q. Under a common IID
sample of size n, the covariance of their sample proportions is
(r-p\\q)/n.

For example, p=.25 and q=.5 permit both r=0 and r=.25. The marginal
rates are identical, while the individual-outcome covariance is
respectively -.125 and .125. Marginals therefore do not usually identify
covariance. Degenerate margins are exceptions: if a variable is
constant, its covariance is fixed at zero.

With more than two indicators, separate feasible choices for every pair
need not describe a jointly attainable binary distribution. A PSD matrix
alone is also insufficient for that purpose. [Pairwise Fréchet bounds
and projected dependence
scenarios](https://joonho112.github.io/sitemix/articles/m7-frechet-envelope-theory.md)
distinguishes raw pairwise intervals from projected dependence
scenarios. Those intervals condition on the supplied marginal
proportions; they are not confidence intervals for the unknown
population covariance. The common-unit IID derivation does not establish
formal covariance bounds for marginals sampled from different or unknown
sets of units.

### Record the assumptions separately

| Input information | Package treatment | Interpretation |
|:---|:---|:---|
| Complete marginal counts, joints absent | D1 estimates each rate; optional diagonal `V` | Off-diagonal zeros are a working assumption |
| Same units established by the source | `sampling_relation = "same_units"`, regime D1a | Formal Fréchet use still needs common n, IID plug-in, no FPC/bias correction, and `anscombe = FALSE` |
| Different units established | `sampling_relation = "different_units"`, regime D1b | Fréchet analysis is an acknowledged heuristic |
| Unit relationship unknown | Keep `sampling_relation = "unknown"` | Equal denominators do not establish same units |

## What suppression leaves unidentified

A publisher’s suppression rule describes which values are withheld. It
has statistical consequences because a hidden numerator does not
identify an observed proportion. Do not conflate this missing marginal
value with the missing joint information of an otherwise observed D1
input.

[`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
counts suppressed rows (Tier 1), observed rows below `accountability_n`
(Tier 2), and observed rows at or above that threshold (Tier 3). These
categories do not themselves establish a permission to publish or a
probability model for the hidden values. With `suppression = "drop"`,
the estimator retains unavailable rows with missing point estimates and
SEs.

An explicitly acknowledged `"upper_bound"` analysis stores a separate
Bernoulli variance-sensitivity value at p=.5 when the denominator is
known. It requires default arcsine output with `anscombe = FALSE`; it
does not replace the missing estimate. With the default
`suppressed_n_strategy = "observed_n"`, the sensitivity denominator is
the observed count. Selecting `"worst_case_bound"` instead uses
`suppressed_n_bound` as the recorded `n` and, for an
observed-denominator row, as `sensitivity_n` in that calculation.
Inspect `sensitivity_n` and the original input together. A hidden
denominator supplies no numeric sensitivity variance, even when a
denominator upper bound is recorded. Suppressed rows cannot enter
ordinary `V` or formal Fréchet calculations, and aggregate FPC currently
requires completely observed counts.

The formulas above apply to identified rows under their stated
assumptions. See [the applied suppression
example](https://joonho112.github.io/sitemix/articles/a6-diagnostics-and-suppression.md)
for input detection and the returned fields.

## Compare the calculations on simulated counts

The example checks matching D0/count estimates and, for D1, the zero
off-diagonals, recorded assumptions, and agreement between the diagonal
and row `se^2`. The D1 call captures its one expected
working-independence warning:

``` r

counts <- readRDS(system.file("extdata", "prek_sim_counts.rds",
                              package = "sitemix"))

# Compare D0 with the same binomial counts
d0 <- counts[counts$year == 2024, c("site_id", "year", "n_jt", "c_jt_frpm")]
d0$indicator <- "frpm"
d0$c_jt <- d0$c_jt_frpm
d0 <- d0[c("site_id", "year", "indicator", "c_jt", "n_jt")]

est_a <- sm_estimate_from_counts(
  counts[counts$year == 2024, c("site_id", "year", "n_jt", "c_jt_frpm")],
  family = "binomial", indicator = "frpm"
)
est_d0 <- sm_estimate_from_aggregates(
  d0, family = "binomial", indicator = "frpm"
)
numeric_fields <- c("theta_raw", "theta_hat", "se_raw", "se", "n")
stopifnot(all(vapply(numeric_fields, function(field) {
  isTRUE(all.equal(est_a[[field]], est_d0[[field]], tolerance = 1e-10))
}, logical(1))))
stopifnot(!identical(est_a$input_mode, est_d0$input_mode))

# Check D1 working independence and the recorded unit relationship
d1_long <- rbind(
  data.frame(site_id = d0$site_id, year = d0$year,
             indicator = "frpm",
             c_jt = counts[counts$year == 2024, ]$c_jt_frpm,
             n_jt = counts[counts$year == 2024, ]$n_jt),
  data.frame(site_id = d0$site_id, year = d0$year,
             indicator = "snap",
             c_jt = counts[counts$year == 2024, ]$c_jt_snap,
             n_jt = counts[counts$year == 2024, ]$n_jt)
)
est_d1 <- capture_expected_sitemix_warning(
  sm_estimate_from_aggregates(
    d1_long, family = "multivariate",
    indicator_col = "indicator",
    sampling_relation = "same_units",
    vjt = TRUE
  ),
  "sitemix_warning_working_independence_default"
)
V1 <- as.matrix(est_d1$V[[1L]])
grp <- est_d1[est_d1$site_id == est_d1$site_id[1L] &
                est_d1$year == est_d1$year[1L], ]
off_diag <- V1[upper.tri(V1)]
stopifnot(max(abs(off_diag)) < 1e-12)
stopifnot(est_d1$V[[1L]]$vcov_method == "working_independence")
stopifnot(attr(est_d1, "sampling_relation") == "same_units")
stopifnot(attr(est_d1, "denominator_pattern") == "common")
stopifnot(attr(est_d1, "d1_regime") == "D1a")
stopifnot(all.equal(unname(diag(V1)), unname(grp$se^2), tolerance = 1e-12))
```

The checks pass for these complete simulated counts. They verify the
implemented calculations and recorded assumptions, not the
appropriateness of working independence for a particular publisher’s
data.

## Related methods and examples

- [Pairwise Fréchet bounds and projected dependence
  scenarios](https://joonho112.github.io/sitemix/articles/m7-frechet-envelope-theory.md)
  for formal raw pairwise intervals and the separate K \> 2 projected
  dependence scenarios.
- [Estimating proportions from published
  aggregates](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md)
  for the applied walkthrough.
- [Binomial standard errors and
  transformations](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
  for the row-level SE derivation reused by D0.
- [Experimental models for variance
  smoothing](https://joonho112.github.io/sitemix/articles/m6-variance-smoothing-theory.md)
  for the experimental log-variance smoother that can be applied to the
  same row-level SEs.

## References

Cochran, W. G. (1977). *Sampling techniques* (3rd ed.). John Wiley &
Sons.

Fréchet, M. (1951). Sur les tableaux de corrélation dont les marges sont
données. *Annales de l’Université de Lyon, 3e série, Section A: Sciences
Mathématiques Et Astronomie*, *14*, 53–77.

Hoeffding, W. (1994). Scale-invariant correlation theory. In N. I.
Fisher & P. K. Sen (Eds.), *The collected works of wassily hoeffding*
(pp. 57–107). Springer. <https://doi.org/10.1007/978-1-4612-0865-5_4>

Nelsen, R. B. (2006). *An introduction to copulas* (2nd ed.). Springer.
<https://doi.org/10.1007/0-387-28678-0>
