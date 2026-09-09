# Sampling uncertainty in site-level proportions

Abstract

Define the site-level proportions, standard errors, and covariance
matrices returned by sitemix. Distinguish the binomial working model
from finite-population sampling, explain the arcsine approximation, and
identify assumptions left to a subsequent analysis.

A site-level proportion and its standard error describe different
things: the estimated rate and the uncertainty attributed to sampling.
This article defines those quantities, introduces the binomial working
model used by the basic calculation, and explains why the selected
estimate scale matters. The runnable example uses the fully simulated
`prek_sim` data; it does not provide empirical findings about real
children or programs.

## Estimates and their sampling uncertainty

For a panel of sites j = 1, \ldots, J observed across years t = 1,
\ldots, T, write the result for each site-year-indicator as:

\mathcal{D}\_{jt,k} \\=\\ \bigl(\hat\theta\_{jt,k}, \\s\_{jt,k},
\\n\_{jt}, \\V\_{jt}\bigr).

\hat\theta\_{jt,k} is the direct point estimate on the selected scale
for indicator k at site j in year t; s\_{jt,k} is its scalar standard
error on the row’s `estimate_scale`; n\_{jt} is the denominator (the
same across k for student-row scenarios); V\_{jt} is the optional joint
covariance across indicators.

The three supported input forms are student records, sufficient counts,
and published aggregates. They lead to the binomial (A), multivariate
(B), multinomial (C), and single- or multiple-marginal aggregate (D0/D1)
calculations. For D1, denominators may differ across indicators; write
n\_{jt,k} when that distinction is needed.

The scale of V\_{jt} must be read separately from the scale of
\hat\theta\_{jt,k}. In particular, B and C store raw-proportion
covariance even when the reported estimates use a transformation. The
[output
guide](https://joonho112.github.io/sitemix/articles/m8-output-contract.md)
describes which scalar variance matches the matrix diagonal.

## Notation and returned fields

Let \pi denote a raw probability and \theta = g(\pi) the same quantity
on the selected scale, with g(p)=p for raw output. For identified
estimates:

| Symbol | Meaning | Code column | Range |
|:---|:---|:---|:---|
| j | Site index | `site_id` | 1, \ldots, J |
| t | Year index | `year` | integer |
| k | Indicator / category index | `indicator` | 1, \ldots, K |
| \pi\_{jt,k} | Underlying raw probability | not observed | \[0,1\] |
| \theta\_{jt,k}=g(\pi\_{jt,k}) | Target on the selected scale | not observed | depends on g |
| \hat\theta\_{jt,k} | Direct estimate | `theta_hat` | scale-dep. |
| s\_{jt,k} | Scalar SE on `estimate_scale` | `se` | \ge 0; zero only at an exact SRSWOR census or a boundary cell on the diagnostic `vst = "none"`, `boundary_method = "none"` path |
| V\_{jt} | Within-site covariance | `V` (list-col) | PSD; K\times K |
| n\_{jt} | Site-year denominator | `n` | \> 0 integer |
| C\_{jt,k} | Numerator (success count) | input `c_jt` or `c_jt_<indicator>` | 0 \le C \le n |
| \hat\pi\_{jt,k} | Raw-scale proportion | `theta_raw` | \[0, 1\] |

Suppressed rows can have missing estimate and SE fields; the table is
not a claim that every returned row is identified. Similarly, a recorded
bound on a hidden denominator is not an observed sample size. Each later
article defines additional symbols where they are used.

## A binomial working model

For one binary indicator, suppose the retained observations are
independent Bernoulli trials with a common probability \pi\_{jt} within
a site-year. Conditional on the observed denominator, this gives

C\_{jt} \mid n\_{jt}, \pi\_{jt} \\\sim\\ \mathrm{Binomial}(n\_{jt},
\pi\_{jt}), \tag{M1.1}

where \pi\_{jt} \in \[0,1\]. The maximum-likelihood estimate is
\hat\pi\_{jt}=C\_{jt}/n\_{jt}. Under this model,
\operatorname{Var}(\hat\pi\_{jt})=\pi\_{jt}(1-\pi\_{jt})/n\_{jt}. For an
interior probability, applying the first-order delta method to
g(p)=\arcsin\sqrt p gives the baseline working standard error:

\boxed{ \begin{aligned} \hat\theta\_{jt} &\\=\\
\arcsin\sqrt{\hat\pi\_{jt}}, \\ s\_{jt} &\\=\\
\frac{1}{2\sqrt{n\_{jt}}}. \end{aligned}} \tag{M1.2}

The variance result is a first-order approximation, not the exact
finite-sample variance of a transformed binomial count. In sitemix,
(M1.2) is the baseline arcsine calculation without FPC, Anscombe
adjustment, or `binomial_bc`; boundary uncertainty requires the
additional rules in [Binomial standard errors and
transformations](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md).
The derivative used by the delta method is not finite at 0 or 1. The
[delta-method
derivation](https://stat238.berkeley.edu/spring-2026/lectureten238spring2026/)
shows the interior asymptotic result. The transformation work of
Anscombe (Anscombe, 1948) and the package’s chosen adjustment are
discussed separately in M2.

With complete observed numerators and denominators, D0 and each D1
marginal use the same scalar calculations under matching options. This
numerical identity does not establish the sampling assumptions of a
publisher’s data, and it does not identify D1 cross-indicator
covariance.

### Sampling without replacement from a finite population

When the target is a fixed finite population and observations are
selected by simple random sampling without replacement, the success
count has a hypergeometric distribution rather than an exact binomial
distribution. For population size N\_{jt}, population success fraction
P\_{jt}, and sample size n\_{jt}, its variance implies

\operatorname{Var}(\hat\pi\_{jt})= \frac{P\_{jt}(1-P\_{jt})}{n\_{jt}}
\frac{N\_{jt}-n\_{jt}}{N\_{jt}-1}, \qquad N\_{jt}\>1.

This follows from the [hypergeometric
moments](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Hypergeometric.html).
The population fraction in this identity is unknown outside a census.
sitemix uses the supplied `fpc` and observed proportions with the
selected variance rule; plug-in and design-corrected estimates of this
variance have different applied multipliers. A census has zero sampling
uncertainty under that design. M2 explains the estimators and their
conditions. Neither a binomial working model nor an FPC accounts
automatically for selection bias, measurement error, or an unspecified
complex sampling design.

## What a subsequent model adds

sitemix supplies direct site estimates and sampling uncertainty. It does
not fit a model for the distribution of site-level rates or shrink their
point estimates. A separate analysis may report rates, compare groups,
or fit a hierarchical model using the estimates and their uncertainty.

For example, the model of Fay and Herriot treats the sampling variances
as known while estimating regression and between-area variation (Fay &
Herriot, 1979, pp. 271–272). Treating an estimated sitemix SE as fixed
in such a model is an additional working assumption, not a statement
that the sampling variance has been observed without error.

A Bayesian analysis could use a prior G to calculate

\tilde\theta\_{jt} \\=\\ \mathrm{E}\[\theta\_{jt} \mid \hat\theta\_{jt},
s\_{jt}; G\], \tag{M1.3}

after specifying a likelihood on the same scale. The prior, likelihood,
fitting method, and suitability of that model are decisions outside
sitemix. The optional [variance-smoothing
helper](https://joonho112.github.io/sitemix/articles/m6-variance-smoothing-theory.md)
models SEs; it does not perform this outcome-model calculation.

## Check an identified example

The following binomial example returns 50 identified estimates. Check
that the expected columns are present, estimates and SEs are finite, the
selected scale is recorded, and row diagnostics contain no
error-severity result:

``` r

data(prek_sim, package = "sitemix")
est <- sm_estimate(
  subset(prek_sim, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)

# Finite estimates in this complete example
stopifnot(all(is.finite(est$theta_hat)))

# Required columns and reported scale
stopifnot(all(c("theta_hat", "se", "estimate_scale") %in% names(est)))
stopifnot(identical(unique(est$estimate_scale), "arcsine"))

# Finite, nonnegative standard errors
stopifnot(all(is.finite(est$se)))
stopifnot(all(est$se >= 0))

# Inspect row diagnostics
diag_r <- sm_diagnose(est, level = "row", verbose = FALSE)
stopifnot(all(c("flag_small_n", "flag_zero_cell") %in% names(diag_r)))
stopifnot(!any(diag_r$diag_severity == "error", na.rm = TRUE))
```

The checks pass silently. They describe this example, not every possible
input: suppressed values remain missing, exact censuses may have zero
SE, and a valid raw covariance can differ in scale from the reported
estimates. For matrix examples, see M3, M4, and M5.

## Assumptions to carry into an analysis

- `se` and `V` are calculated under the selected sampling and variance
  rules. Assess those assumptions against how observations were
  collected.
- The package does not estimate covariance between different sites or
  years. A model involving such dependence needs additional information
  or assumptions.
- Keep the chosen estimate, SE, and matrix scales together. A later
  transformation needs a corresponding treatment of uncertainty.
- Missing or separately assumed suppression-sensitivity values are not
  identified estimates. Keep their status when preparing a model input.

## Further derivations

- [Binomial standard errors and
  transformations](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
  for the delta-method calculation, corrections, and student/count
  equivalence.
- [Covariance for overlapping binary
  indicators](https://joonho112.github.io/sitemix/articles/m3-multivariate-sur-covariance.md)
  for the Scenario B extension of V\_{jt}.
- [Understanding returned estimates and
  uncertainty](https://joonho112.github.io/sitemix/articles/m8-output-contract.md)
  for returned fields, scales, and diagnostic conditions.
- [Getting started with site-level
  proportions](https://joonho112.github.io/sitemix/articles/a1-getting-started.md)
  for the first worked estimation example.

## References

Anscombe, F. J. (1948). The transformation of Poisson, binomial and
negative-binomial data. *Biometrika*, *35*(3/4), 246–254.
<https://doi.org/10.1093/biomet/35.3-4.246>

Fay, R. E., & Herriot, R. A. (1979). Estimates of income for small
places: An application of James-Stein procedures to census data.
*Journal of the American Statistical Association*, *74*(366), 269–277.
<https://doi.org/10.1080/01621459.1979.10482505>
