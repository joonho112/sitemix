# Binomial standard errors and transformations

Abstract

Relate a binomial proportion to its raw and transformed standard errors.
Distinguish first-order approximations, the optional Anscombe
adjustment, boundary variance rules, and finite-population correction.
Compare student and count inputs under matching conditions.

[`sm_estimate()`](https://joonho112.github.io/sitemix/reference/sm_estimate.md)
and
[`sm_estimate_from_counts()`](https://joonho112.github.io/sitemix/reference/sm_estimate_from_counts.md)
report a proportion together with a standard error on the selected
scale. This article starts with the IID binomial model, then connects
its variance to the package’s arcsine, logit, and raw output. The later
sections describe boundary rules and the separate finite-population
sampling branch.

The notation follows [Sampling uncertainty in site-level
proportions](https://joonho112.github.io/sitemix/articles/m1-statistical-foundations.md):
\pi denotes a raw probability and \theta = g(\pi) a target on the
reporting scale. The calculations below use the fully simulated
`prek_sim` data; see [Estimating proportions for a binary
indicator](https://joonho112.github.io/sitemix/articles/a3-scenario-binomial.md)
for an applied comparison of the scales.

## The binomial model and the returned columns

At site j in year t, let C\_{jt} count students with the indicator among
n\_{jt} retained observations. The IID model treats their binary
outcomes as independent Bernoulli variables with a common probability
\pi\_{jt}, conditional on the positive denominator:

C\_{jt} \mid n\_{jt}, \pi\_{jt} \sim \mathrm{Binomial}(n\_{jt},
\pi\_{jt}), \qquad \hat\pi\_{jt} = C\_{jt}/n\_{jt}.

Under this model the sampling variance is exactly

\operatorname{Var}(\hat\pi\_{jt} \mid n\_{jt},\pi\_{jt}) =
\frac{\pi\_{jt}(1-\pi\_{jt})}{n\_{jt}}.

The default raw calculation substitutes the observed \hat\pi\_{jt} for
the unknown \pi\_{jt}. That plug-in variance is an estimate of the
sampling variance. It should not be confused with an exact variance for
a transformed binomial variable or a guarantee about confidence interval
coverage.

| Quantity | Meaning | Returned column |
|:---|:---|:---|
| n\_{jt} | Retained denominator | `n` |
| \hat\pi\_{jt} | Observed proportion C\_{jt}/n\_{jt} | `theta_raw` |
| \hat\theta\_{jt} | Estimate on the selected reporting scale | `theta_hat` |
| s\_{\mathrm{raw},jt} | Estimated SE of the observed proportion | `se_raw` |
| s\_{jt} | Estimated SE on `estimate_scale` | `se` |

Keep `theta_raw` with `se_raw`, or `theta_hat` with `se`. A supplied
`fpc` describes sampling without replacement from a fixed finite
population. That design is separate from the IID model above; its
variance multipliers are discussed after the transforms.

## Reporting transforms and first-order standard errors

For a differentiable transform g, the delta method uses a linear
approximation to g(\hat\pi) near the population probability \pi. At a
fixed interior 0 \< \pi \< 1, as n grows, this gives the first-order
approximation

\operatorname{Var}\\g(\hat\pi)\\ \approx
\[g'(\pi)\]^2\frac{\pi(1-\pi)}{n}.

Replacing \pi by an interior observed proportion gives the working SE
formulas below. They use no FPC, no `binomial_bc`, and
`anscombe = FALSE`. The [delta-method derivation in the Berkeley lecture
notes](https://stat238.berkeley.edu/spring-2026/lectureten238spring2026/)
states the limiting argument and applies it to the arcsine transform. A
finite reported SE does not establish that this approximation is
accurate for a small or sparse cell.

### Arcsine square root: the default scale

With `vst = "arcsine"`, the unadjusted point transform is g(p) =
\arcsin\sqrt{p}, and

g'(p) = \frac{1}{2\sqrt{p(1-p)}}.

Multiplying this derivative by the raw plug-in SE cancels the
rate-dependent factor for 0 \< \hat\pi\_{jt} \< 1:

s\_{jt} = \frac{1}{2\sqrt{n\_{jt}}}. \tag{M2.1}

This is the formula implemented by the default arcsine branch. The
cancellation is exact algebra within the first-order approximation;
\operatorname{Var}\\\arcsin\sqrt{C/n}\\ is not exactly 1/(4n) at finite
n. At observed proportions 0 or 1, the derivative argument above is not
defined. The package still uses the same arcsine SE formula in this
default branch, while a separate boundary rule supplies `se_raw`.

### Logit: report log-odds

With `vst = "logit"`, g(p) = \log\\p/(1-p)\\ and g'(p) = 1/\\p(1-p)\\.
The working SE is

s\_{jt} = \sqrt{\frac{1}{n\_{jt}\hat\pi\_{jt}(1-\hat\pi\_{jt})}}.
\tag{M2.2}

The logit SE still depends on the observed proportion, so logit is not
variance-stabilizing. Both the point transform and its SE require 0 \<
\hat\pi\_{jt} \< 1. A boundary logit call raises
`sitemix_error_estimate_var_method`.

### Raw output: retain the observed proportion

With `vst = "none"`, g(p) = p, so `theta_hat = theta_raw` and
`se = se_raw`. For an interior row under the default variance rule,

s\_{jt} = \sqrt{\frac{\hat\pi\_{jt}(1-\hat\pi\_{jt})}{n\_{jt}}}.

The raw scale does not stabilize variance. Boundary methods and
`binomial_bc` change the raw variance calculation as described below.

### Anscombe-adjusted arcsine output

With `vst = "arcsine", anscombe = TRUE`, the package uses the Anscombe
adjustment (Anscombe, 1948) in the point transform:

\tilde\pi\_{jt} = \frac{C\_{jt}+3/8}{n\_{jt}+3/4}, \qquad
\hat\theta\_{jt} = \arcsin\sqrt{\tilde\pi\_{jt}}.

This adds 3/8 to both the success and failure counts before the
transform. It leaves `theta_raw = C/n` unchanged and records
`estimate_scale = "arcsine_anscombe"`.

The implemented SE uses a different denominator from the adjusted
proportion. With no FPC and no variance bias correction,

n\_{\mathrm{eff},jt} = n\_{jt}+1/2, \qquad s\_{jt} =
\frac{1}{2\sqrt{n\_{\mathrm{eff},jt}}}.

Here `n_eff` is the package’s transform-specific SE denominator; it is
not a count of extra students. Do not substitute n+3/4 from the point
transform into this SE formula. The reported SE is a working
approximation, not an exact finite-sample variance of the adjusted
transform. `anscombe = TRUE` cannot be combined with `binomial_bc`.

## Finite-population and bias-correction branches

Supply `fpc = N` when the n observations are a simple random sample
without replacement from a fixed population of size N. Each site-year
requires a known N \ge n. Here \pi is the proportion in that finite
population. The hypergeometric sampling variance for N\>1 is

\operatorname{Var}(\hat\pi) = \frac{\pi(1-\pi)}{n}\frac{N-n}{N-1}.

This is a design variance, rather than the IID binomial variance. The
without-replacement distribution and its moments are given in the
[official R hypergeometric
documentation](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Hypergeometric.html).
See Cochran (1977) for the broader sampling framework.

### Plug-in and corrected variance rules

The default plug-in branch replaces \pi by \hat\pi and uses the variance
multiplier

q\_{jt}=\begin{cases} 0, & N\_{jt}=n\_{jt},\\
(N\_{jt}-n\_{jt})/(N\_{jt}-1), & N\_{jt}\>n\_{jt}. \end{cases}

The raw plug-in variance is \hat\pi(1-\hat\pi)q/n. Arcsine and logit SEs
without `binomial_bc`, and raw boundary surrogates, use \sqrt q as their
SE multiplier.

For an interior row with `bias_correction = "binomial_bc"`, the no-FPC
raw variance uses n-1 instead of n. The corresponding raw and
delta-method SEs are

\begin{aligned} s\_{\mathrm{raw,bc}} &=
\sqrt{\frac{\hat\pi(1-\hat\pi)}{n-1}}, \\ s\_{\mathrm{asin,bc}} &=
\frac{1}{2\sqrt{n-1}}, \\ s\_{\mathrm{logit,bc}} &=
\frac{1}{\sqrt{(n-1)\hat\pi(1-\hat\pi)}}. \end{aligned}

With FPC, these corrected interior SEs use \sqrt{(N-n)/N}. In
particular, the corrected raw variance is

\widehat{\operatorname{Var}}\_{\mathrm{design}}(\hat\pi) =
\frac{\hat\pi(1-\hat\pi)}{n-1}\frac{N-n}{N}.

The different multipliers accompany different baseline variance
estimators; they are not interchangeable. These corrected expressions
require n\>1. Boundary rows retain their selected raw boundary rule and
\sqrt q, even when `binomial_bc` is requested.

With FPC, `fpc_variance_multiplier` records q, while
`variance_multiplier_applied` records the multiplier used by the
selected variance rule. `fpc_se_multiplier` and `se_multiplier_applied`
are their square roots. The call also records `sampling_design`,
`population_size`, `sampling_fraction`, and `variance_rule`. FPC changes
neither the point estimates nor `n_eff`.

### A census has zero sampling variance

When N=n, all members of the declared finite population have been
observed. The implementation returns zero sampling SE, including the
one-unit census N=n=1; it does not evaluate the undefined ratio 0/0 in
that case. The point transform must still be valid: a logit call at an
observed 0 or 1 remains an error.

A zero census SE follows from the sampling design. It is different from
a zero raw plug-in SE at a boundary when `fpc = NULL`. Neither value
accounts for measurement error or uncertainty from other sources.

## Raw uncertainty at observed proportions 0 or 1

At \hat\pi \in \\0,1\\, the raw plug-in variance is zero. The
`boundary_method` options below determine `se_raw` at those rows. They
leave the observed `theta_raw = C/n` unchanged. On raw output,
`theta_hat` also stays at C/n.

The interval methods behind these options use adjusted centers and
interval endpoints. `sitemix` instead takes a positive SE-sized quantity
from those constructions as a named boundary variance surrogate.
Returning that quantity alongside the observed point estimate does not
reproduce the original confidence interval or transfer its coverage
properties to later analyses.

### Wilson boundary surrogate

Writing p=\hat\pi and z=\Phi^{-1}(0.975), the Wilson score interval has
an adjusted center

c_W = \frac{p+z^2/(2n)}{1+z^2/n}

and half-width z s_W, where

s_W(p,n) = \frac{\sqrt{p(1-p)/n+z^2/(4n^2)}}{1+z^2/n}.

These follow from inverting the score inequality; the center is
generally different from p (Wilson, 1927). With the default
`boundary_method = "wilson_floor"`, the package uses s_W only at p=0 or
p=1. Its no-FPC value there is z/\\2(n+z^2)\\, recorded as
`wilson_boundary_surrogate` on raw output. The package does not replace
the observed point estimate with c_W.

### Agresti–Coull boundary surrogate

The adjusted-Wald construction in Agresti & Coull (1998) adds two
successes and two failures for an approximate 95% interval. Its adjusted
center is (C+2)/(n+4), and the interval uses the Wald formula with the
adjusted counts. The package uses the related z-based calculation

\tilde n=n+z^2,\qquad \tilde\pi=\frac{C+z^2/2}{\tilde n},\qquad
s\_{AC}=\sqrt{\frac{\tilde\pi(1-\tilde\pi)}{\tilde n}},

with the same fixed z=\Phi^{-1}(0.975). Thus z^2 is about 3.84, rather
than exactly 4. `boundary_method = "agresti_coull"` uses s\_{AC} at
boundary rows and records `agresti_coull_boundary_surrogate` on raw
output. It retains C/n as the point estimate; it does not report the
adjusted-Wald interval centered at \tilde\pi. This boundary option
cannot be combined with `anscombe = TRUE`.

### Keep the boundary rule and output scale together

With `boundary_method = "none"`, boundary rows keep the zero raw plug-in
SE. This choice does not establish that the population proportion is
known. Interior rows use their binomial variance rule regardless of the
boundary option.

For default arcsine output without extra corrections, `se` remains
1/(2\sqrt n) at boundaries, while `se_raw` records the selected boundary
calculation. With FPC, the applicable SEs are multiplied as described
above and become zero at a census. Applying an FPC to a boundary
surrogate is a package calculation; it is not a derivation of a
finite-population Wilson or adjusted-Wald confidence interval.

## Read the calculation labels

`var_method` describes the calculation for `se` on `estimate_scale`:

| `var_method` | Calculation |
|:---|:---|
| `arcsine_vst` | Unadjusted arcsine working SE |
| `arcsine_delta_binomial_bc` | Interior arcsine SE with `binomial_bc` |
| `arcsine_anscombe` | Anscombe-adjusted arcsine SE |
| `logit_delta` | Interior logit SE without `binomial_bc` |
| `logit_delta_binomial_bc` | Interior logit SE with `binomial_bc` |
| `binomial` | Raw plug-in SE, including a boundary with `boundary_method = "none"` |
| `binomial_bc` | Interior raw SE with `binomial_bc` |
| `wilson_boundary_surrogate` | Wilson-derived raw boundary SE |
| `agresti_coull_boundary_surrogate` | Agresti–Coull-derived raw boundary SE |
| `suppressed_drop` | Aggregate value retained as unavailable |
| `suppression_sensitivity` | Non-identified sensitivity result; the main estimate and SE columns remain missing |

A transformed row keeps its transform label even when `se_raw` uses a
boundary surrogate. When `vjt = TRUE`, the associated `sm_vcov` records
the raw correction in `scalar_correction_rule`. See
[`?sm_vcov`](https://joonho112.github.io/sitemix/reference/sm_vcov.md)
for covariance scales and calculation labels, and [Sampling uncertainty
from published
aggregates](https://joonho112.github.io/sitemix/articles/m5-aggregate-engines.md)
for suppression-specific outputs.

## Compare the formulas with calculated results

The first part of this example uses FRPM in 2024 to check the default
arcsine point estimate and SE. The companion counts were computed from
the same `prek_sim` students. A student/count comparison requires the
same retained observations, missing-value exclusions, indicator order,
and estimation options. The tolerance in these checks applies to this
example, rather than promising agreement for arbitrary input tables.

A two-site fixture then gives one boundary proportion and one interior
proportion. Finally, a count fixture with C=3, n=8, and N=20 checks the
two FPC multipliers: 12/19 for the plug-in branch and 12/20 for the
corrected interior branch.

``` r

data(prek_sim, package = "sitemix")
est <- sm_estimate(
  subset(prek_sim, year == 2024),
  family = "binomial", indicator = "frpm", vst = "arcsine"
)
interior <- which(est$theta_raw > 0 & est$theta_raw < 1)

# Check the unadjusted arcsine point transform.
manual_theta <- asin(sqrt(est$theta_raw[interior]))
stopifnot(all.equal(est$theta_hat[interior], manual_theta, tolerance = 1e-10))

# Check the default arcsine SE for interior rows.
manual_se <- 1 / (2 * sqrt(est$n[interior]))
stopifnot(all.equal(est$se[interior], manual_se, tolerance = 1e-10))

# Compare counts from the same retained students.
counts <- readRDS(system.file("extdata", "prek_sim_counts.rds",
                              package = "sitemix"))
est_counts <- sm_estimate_from_counts(
  counts[counts$year == 2024,
         c("site_id", "year", "n_jt", "c_jt_frpm")],
  family = "binomial", indicator = "frpm"
)
stopifnot(all.equal(est$theta_hat, est_counts$theta_hat, tolerance = 1e-10))
stopifnot(all.equal(est$se, est_counts$se, tolerance = 1e-10))

# Use two sites to check boundary and logit behavior.
toy <- data.frame(
  site_id = c(rep("A", 10), rep("B", 10)),
  year = 2024,
  y = c(rep(1, 10), rep(c(1, 0), each = 5))
)
toy_raw <- sm_estimate(
  toy, family = "binomial", indicator = "y",
  vst = "none", boundary_method = "wilson_floor"
)
z <- qnorm(0.975)
manual_wilson <- z / (2 * 10 * (1 + z^2 / 10))
stopifnot(all.equal(toy_raw$se[toy_raw$site_id == "A"],
                    manual_wilson, tolerance = 1e-10))
stopifnot(toy_raw$var_method[toy_raw$site_id == "A"] ==
            "wilson_boundary_surrogate")

toy_logit <- sm_estimate(
  subset(toy, site_id == "B"),
  family = "binomial", indicator = "y", vst = "logit"
)
stopifnot(all.equal(toy_logit$theta_hat, log(0.5 / 0.5), tolerance = 1e-10))

logit_boundary <- tryCatch(
  sm_estimate(toy, family = "binomial", indicator = "y", vst = "logit"),
  error = identity
)
stopifnot(inherits(logit_boundary, "sitemix_error_estimate_var_method"))

# Compare the plug-in and corrected finite-population branches.
fpc_counts <- data.frame(
  site_id = "F", year = 2024L, n_jt = 8L, c_jt_rate = 3L
)
fpc_plugin <- sm_estimate_from_counts(
  fpc_counts, family = "binomial", indicator = "rate",
  vst = "arcsine", fpc = 20, min_n = 1L
)
stopifnot(all.equal(
  fpc_plugin$se,
  (1 / (2 * sqrt(8))) * sqrt(12 / 19),
  tolerance = 1e-12
))
stopifnot(all.equal(fpc_plugin$variance_multiplier_applied, 12 / 19,
                    tolerance = 1e-12))

fpc_corrected <- sm_estimate_from_counts(
  fpc_counts, family = "binomial", indicator = "rate",
  vst = "arcsine", bias_correction = "binomial_bc",
  fpc = 20, min_n = 1L
)
stopifnot(all.equal(
  fpc_corrected$se,
  (1 / (2 * sqrt(7))) * sqrt(12 / 20),
  tolerance = 1e-12
))
stopifnot(all.equal(fpc_corrected$variance_multiplier_applied, 12 / 20,
                    tolerance = 1e-12))
stopifnot(identical(fpc_corrected$n_eff, 8))
```

All checks pass for these inputs. The FRPM student/count estimates and
SEs agree; the boundary fixture retains its observed proportion while
using the Wilson surrogate; and the FPC fixture keeps `n_eff = 8` under
both variance rules. These checks verify the displayed package
calculations. They do not establish finite-sample coverage for the
working SEs or a general numerical guarantee across input types.

Small denominators still require care with the uncertainty
approximation. Sample-size flags mark rows without dropping them or
changing the calculation; [Estimating proportions for a binary
indicator](https://joonho112.github.io/sitemix/articles/a3-scenario-binomial.md)
shows those flags alongside the estimates. For overlapping binary
indicators, continue to [Covariance for overlapping binary
indicators](https://joonho112.github.io/sitemix/articles/m3-multivariate-sur-covariance.md).
For published marginal or subgroup counts, see [Sampling uncertainty
from published
aggregates](https://joonho112.github.io/sitemix/articles/m5-aggregate-engines.md).

## References

Agresti, A., & Coull, B. A. (1998). Approximate is better than “exact”
for interval estimation of binomial proportions. *The American
Statistician*, *52*(2), 119–126.
<https://doi.org/10.1080/00031305.1998.10480550>

Anscombe, F. J. (1948). The transformation of Poisson, binomial and
negative-binomial data. *Biometrika*, *35*(3/4), 246–254.
<https://doi.org/10.1093/biomet/35.3-4.246>

Cochran, W. G. (1977). *Sampling techniques* (3rd ed.). John Wiley &
Sons.

Wilson, E. B. (1927). Probable inference, the law of succession, and
statistical inference. *Journal of the American Statistical
Association*, *22*(158), 209–212.
<https://doi.org/10.1080/01621459.1927.10502953>
