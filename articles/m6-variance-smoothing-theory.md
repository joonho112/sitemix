# Experimental models for variance smoothing

Abstract

[`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
fits a relationship between supplied variance estimates and their
denominators. This article describes the log-linear and additive models,
the raw-scale offset, and the assumptions behind transforming fitted
log-variances back to standard errors.

## 1. What the smoother estimates

[`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
models the logarithm of the variance estimates already present in a
`sitemix_estimates` object. It fits a relationship across eligible rows
and converts the fitted values to alternative SEs. This generalized
variance function (GVF) is experimental and must be requested
explicitly. By default, it adds `se_smoothed` or `se_raw_smoothed`,
together with `var_method_smoothed`, and preserves the original SEs.

A smaller denominator can itself explain a larger SE. For example, the
uncorrected arcsine working SE without a finite-population correction is
s\_{jt} = 1/(2\sqrt{n\_{jt}}): it is about `0.224` at n = 5 and `0.050`
at n = 100. Fitting this denominator relationship does not establish
that the original uncertainty needs correction.

The response here is a log-variance estimate. In contrast, the
Fay–Herriot area-level model relates direct outcome estimates to
unobserved area means and a regression model, with specified sampling
variances (Fay & Herriot, 1979, p. 271).
[`sm_smooth_variance()`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md)
leaves `theta_hat` and `theta_raw` unchanged and does not estimate
area-level posterior means. The Fay–Herriot paper supplies a comparison
of model targets, rather than a derivation of this variance smoother.

The worked example in Section 7 uses `prek_sim`, a fully simulated
50-site pre-kindergarten panel. Section 8 describes a separate
simulation that generates binomial counts. Neither uses observations of
real children, sites, or programs.

## 2. Variances and predictors

Let s\_{jt} denote the selected input SE and v\_{jt}=s\_{jt}^2 its
variance estimate. Here v\_{jt} is an observed input to a regression,
not the unknown sampling variance itself. Only rows with a positive,
finite selected SE and denominator can enter the log-variance model; the
additional row-selection rules are described below.

| Quantity | Model variable | Meaning |
|:---|:---|:---|
| \log v\_{jt} | `log_var` | Log of the squared input `se` or `se_raw` |
| \log n\_{jt} | `log_n` | Log of the row denominator |
| \ell^{\mathrm{off}}\_{jt}=\log\[p^\*\_{jt}(1-p^\*\_{jt})\] | `p_offset` | Log offset for the default `scale = "se_raw"` formula |
| \tilde s\_{jt} | `se_smoothed` or `se_raw_smoothed` | SE obtained from the fitted log-variance |

The probability p^\*\_{jt} used in the offset is defined in Section 4.
The `scale` argument selects the input column. In particular,
`scale = "se"` can select raw-scale SEs when the estimates were made
with `vst = "none"`; it does not automatically select the raw-offset
formula.

## 3. The default log-linear model

With `method = "loglinear"`, `scale = "se"`, `by = NULL`, and no custom
formula or weights, the function fits an ordinary least-squares
regression using [`stats::lm()`](https://rdrr.io/r/stats/lm.html):

\log v\_{jt} \\=\\ \beta_0 + \beta_1 \log n\_{jt} + \varepsilon\_{jt}.
\tag{M6.1}

For `scale = "se_raw"`, the default formula adds an offset with its
coefficient fixed at one:

\log v^{\mathrm{raw}}\_{jt} \\=\\ \beta_0 + \beta_1 \log n\_{jt} +
\ell^{\mathrm{off}}\_{jt} + \varepsilon\_{jt}. \tag{M6.2}

The corresponding formulas are `log_var ~ log_n` and
`log_var ~ log_n + offset(p_offset)`. Columns supplied through `by`
enter as factor main effects; they do not cause separate fits. For
uncorrected arcsine inputs without FPC, v=1/(4n) gives a slope of -1.
Corrections already present in the input SE can change that
relationship.

### Returning from log-variance to SE

Write \hat\eta\_{jt} for the fitted value on the log-variance scale,
including any formula offset. With the default `bias_correct = TRUE`,
the implementation calculates

\tilde v\_{jt}=\exp\\\left(\hat\eta\_{jt}
+\tfrac12\hat\sigma\_\varepsilon^2\right), \qquad \tilde
s\_{jt}=\sqrt{\tilde v\_{jt}}.

The half-variance term follows from a log-normal model: if the log-scale
error is conditionally normal with mean zero and a common variance
\sigma\_\varepsilon^2, then E(v\_{jt}\mid\text{predictors})=
\exp(\eta\_{jt}+\sigma\_\varepsilon^2/2). This is the log-normal mean
identity documented in [R’s log-normal distribution
reference](https://stat.ethz.ch/R-manual/R-devel/library/stats/html/Lognormal.html).
Jensen’s inequality alone does not determine this particular correction.

The package substitutes `stats::sigma(fit)^2` for a linear model or
`summary(fit)$scale` for a GAM. Setting `bias_correct = FALSE` omits the
half-variance term; under the same log-normal model, \exp(\eta\_{jt}) is
the conditional median of the variance input. These calculations treat
the fitted model parameters as fixed. They do not account for
uncertainty in the fitted regression or make the result an unbiased
estimate of the true sampling variance. A common half-variance
correction also needs separate justification if the log-scale errors are
non-normal or their variance changes across rows.

## 4. The Wilson-centered raw-scale offset

The binomial variance has a rate-dependent factor p(1-p), which
motivates the offset in (M6.2). Substituting a boundary estimate
\hat\pi\in\\0,1\\ makes its logarithm undefined. For flagged boundary
rows, the implementation instead uses

p^\*\_{jt} \\=\\ \frac{\hat\pi\_{jt} + z^2/(2n\_{jt})}{1 + z^2/n\_{jt}}
\\=\\ \frac{C\_{jt} + z^2/2}{n\_{jt} + z^2}

with z=\Phi^{-1}(0.975). This is the Wilson score center (Wilson, 1927);
taking z\approx2 gives the related plus-four center (Agresti & Coull,
1998). Interior rows use their raw estimates, and the result is clamped
away from zero and one at machine precision. Using this center for the
offset is a package modeling choice. It does not replace the response
variance: an input SE of zero still cannot enter the log-variance
regression.

The model-frame column is not this probability itself. It is
`p_offset = log(p_star * (1 - p_star))`, the term
\ell^{\mathrm{off}}\_{jt} in (M6.2). A custom formula should therefore
use `offset(p_offset)`, not apply a second probability-to-log
transformation.

### Inputs with a finite-population correction

Smoothing consumes the SEs already present in the input. If those SEs
were adjusted for simple random sampling without replacement (SRSWOR),
the fitted response contains that adjustment. For the plug-in arcsine
calculation with `anscombe = FALSE`, let N\>n be the finite population
size and q=(N-n)/(N-1)\>0. The supplied variance is q/(4n), so

\log(s^2)=-\log 4-\log n+\log q,

Variation in q across rows can change the fitted denominator slope; the
default smoothing formula does not add an FPC offset. This equation does
not cover Anscombe or bias-corrected inputs. Those inputs retain their
own SE calculation, described in [Binomial standard errors and
transformations](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md).
At an exact census, the SE is zero and the row is excluded from the
log-variance fit.

By default, the returned object keeps `se`, `se_raw`, and the sampling
information used to calculate them. If overwrite is requested and
allowed, the original SEs are saved in `se_pre_smoothing` or
`se_raw_pre_smoothing`. The unchanged FPC fields continue to describe
the original design calculation; they do not establish a new
design-based interpretation for the smoothed SE.

## 5. An additive model for a curved relationship

With `method = "gam"`, the default `scale = "se"`, `by = NULL` model
replaces the straight-line term with a smooth function:

\log v\_{jt} \\=\\ \beta_0 + f(\log n\_{jt}) + \varepsilon\_{jt}.
\tag{M6.3}

The default call uses
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) with `s(log_n)`,
a Gaussian family with an identity link, and `method = "REML"` to
estimate smoothness. The smooth uses a penalized thin plate regression
spline, the default basis for
[`mgcv::s()`](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/s.html).
The penalty controls how much curvature is fitted. For
`scale = "se_raw"`, the formula also includes `offset(p_offset)`; `by`
columns again add factor main effects.

Wood’s treatment of additive models (Wood, 2017) and the [`mgcv::gam()`
documentation](https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/gam.html)
describe the fitting method. Applying it to these log-variance inputs is
the package’s choice, and those sources do not establish an improvement
in sitemix SEs or interval coverage. The conversion back to variance
above uses the estimated scale parameter of the GAM; its log-normal
interpretation applies to the default Gaussian model and requires
reconsideration if that family or error structure is changed.

## 6. Which rows are fitted

After validating the input object, the function selects rows that are
not suppressed and have a finite, positive selected SE and denominator.
With the default `scope = "all"`, every such row is eligible.
`scope = "tier2"` further restricts the fit to 11\le n\le29; this
interval is fixed and does not follow `accountability_n` or
`flag_below_accountability`. If supplied, `min_n` also requires
`n >= min_n`. The Tier 2 label in
[`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
uses a separate, adjustable reporting threshold.

The fitted SEs replace values only in eligible rows of the added column.
Other rows retain their input values, including zero SEs for exact
censuses and missing SEs for suppressed rows. Thus a successful fit does
not imply that every row in the result has a positive or finite SE.
Objects containing non-identified suppression-sensitivity estimates are
rejected before fitting.

At least `min_rows` eligible rows are needed; the default is `50L`. If
fewer qualify, the function skips fitting, copies the selected input SE
column to the alternative column, and records `status = "skipped"` in
both summary attributes. It performs no overwrite, even with
`overwrite = TRUE`, and adds no new residuals, fitted model, or
pre-smoothing snapshots.

### Warnings and fit diagnostics

Four warning classes describe distinct conditions:

- `sitemix_warning_smoother_skipped`: fewer than `min_rows` rows
  qualified. Inspect the row selection before interpreting the
  alternative column as fitted values.
- `sitemix_warning_smoother_multi_year_default`: eligible rows span
  multiple years and `by = NULL`. Use `by = "year"` for a year fixed
  effect in one fit, or call the function on separate yearly subsets for
  separate fits.
- `sitemix_warning_unexpected_slope`: a loglinear fit using
  `scale = "se"` on arcsine or Anscombe-arcsine rows has a finite
  `log_n` coefficient more than 0.15 from -1. Interpret the slope
  alongside any corrections already included in the input SEs.
- `sitemix_warning_raw_scale_smoothing`: the first request for
  `scale = "se_raw"` after loading the package. The default formula
  includes `offset(p_offset)`; a custom formula needs its own
  appropriate rate-dependent term.

The `smoother_fit_summary` attribute records the current fit status,
formula, eligible-row count, and relationship to any covariance
matrices. After fitting, it also records rank, convergence, prediction
checks, and model summaries. The `smoothing` attribute records the
settings and eligible row indices. With `return_diagnostics = TRUE`, a
completed fit adds `residual_log_var` and the `smoother_fit` attribute.
Check the current status first: a skipped call adds no new fitted model
or residuals.

A custom formula must use `log_var` as its response and the model
variables and terms documented in
[`?sm_smooth_variance`](https://joonho112.github.io/sitemix/reference/sm_smooth_variance.md).
The fitted model must have full rank, positive residual degrees of
freedom, and finite predictions for all eligible rows; a GAM must also
converge. Failure raises an error instead of substituting a fallback
fit.

### Overwrite and covariance matrices

When enough rows qualify to fit, `overwrite = TRUE` is rejected with
`sitemix_error_smoothing_v_stale` if an eligible row has a `V` matrix on
the selected SE scale. Keeping that matrix while replacing its SE could
make the diagonal inconsistent. A matrix on a different scale remains
unchanged, and its relation to the selected scale is recorded in
`smoothing$v`. The insufficient-row skip described above occurs before
the overwrite rejection and leaves both SEs and `V` unchanged.

If overwrite is allowed, `scale = "se"` replaces eligible `se` values.
With `scale = "se_raw"`, it replaces eligible `se_raw` values and also
updates `se` for rows with `estimate_scale = "none"`. Transformed `se`
values are preserved in that raw-column operation. `var_method` changes
where `se` changes; `var_method_smoothed` describes the alternative SEs.

## 7. Checking the returned columns

This example fits the two default loglinear formulas to the 2024 FRPM
estimates and checks that the original columns are preserved. All rows
in this example have finite SEs; the checks on the whole column below
therefore apply to this example, while the general rules for missing or
zero input values are given in Section 6. The calls explicitly use
`min_rows = 2`, rather than the default 50.

``` r

data(prek_sim, package = "sitemix")
est <- sm_estimate(
  subset(prek_sim, year == 2024),
  family = "binomial", indicator = "frpm"
)
est_s <- sm_smooth_variance(
  est, method = "loglinear", return_diagnostics = TRUE, min_rows = 2
)

# Check the smoothed SEs in this eligible-row example.
stopifnot(all(is.finite(est_s$se_smoothed)))
stopifnot(all(est_s$se_smoothed >= 0))

# The default transformed-scale formula has no offset.
fit_formula <- formula(attr(est_s, "smoother_fit"))
stopifnot(identical(deparse(fit_formula), "log_var ~ log_n"))

est_raw <- capture_expected_sitemix_warning(
  sm_smooth_variance(
    est, method = "loglinear", scale = "se_raw",
    return_diagnostics = TRUE, min_rows = 2
  ),
  "sitemix_warning_raw_scale_smoothing"
)
raw_formula <- formula(attr(est_raw, "smoother_fit"))
stopifnot(identical(deparse(raw_formula),
                    "log_var ~ log_n + offset(p_offset)"))

# The default call retains the original SE column.
stopifnot(all.equal(est_s$se, est$se, tolerance = 1e-12))

# Keep the original method label and record the smoothed alternative.
stopifnot(identical(est_s$var_method, est$var_method))
stopifnot("var_method_smoothed" %in% names(est_s))

# Retain the original SEs and finite-population fields.
counts <- readRDS(system.file("extdata", "prek_sim_counts.rds",
                              package = "sitemix"))
fpc_counts <- counts[counts$year == 2024,
                     c("site_id", "year", "n_jt", "c_jt_frpm")]
est_fpc <- sm_estimate_from_counts(
  fpc_counts, family = "binomial", indicator = "frpm",
  fpc = 2 * fpc_counts$n_jt, min_n = 1L
)
est_fpc_s <- sm_smooth_variance(est_fpc, method = "loglinear", min_rows = 2L)
fpc_fields <- c(
  "population_size", "sampling_fraction",
  "fpc_variance_multiplier", "fpc_se_multiplier",
  "variance_multiplier_applied", "se_multiplier_applied",
  "sampling_design", "variance_rule"
)
stopifnot(identical(est_fpc_s$se, est_fpc$se))
stopifnot(identical(est_fpc_s$se_raw, est_fpc$se_raw))
stopifnot(all(vapply(fpc_fields, function(field) {
  identical(est_fpc_s[[field]], est_fpc[[field]])
}, logical(1))))
```

The transformed-scale fit uses all 50 rows and reproduces the original
uncorrected arcsine SEs to numerical precision. That follows from their
1/(2\sqrt n) relationship; it is not evidence that smoothing improves
uncertainty estimation. The raw-column fit uses the additional rate
offset. Both fits preserve the original SE and calculation-label columns
by default.

The final part sets each simulated finite population to twice the sample
size and checks the eight returned sampling fields. Preserving those
fields records the original SRSWOR calculation. It does not test the
statistical performance of smoothed SEs under finite-population
sampling.

## 8. What the fixed simulation shows

The maintainer script `inst/scripts/audit-smoothing-simulation.R`
compares unsmoothed SEs with loglinear and GAM alternatives. The
recorded full run used seed `20260712` and 60 replicates. Each replicate
generated independent binomial counts on an 84-row grid: 12 denominators
from 8 to 150 crossed with seven true rates from 0.02 to 0.98. The four
reporting strata crossed n\le20 versus larger n with near-boundary rates
(p\le0.10 or p\ge0.90) versus interior rates.

The estimates used `vst = "arcsine"`,
`boundary_method = "wilson_floor"`, and `min_n = 1`, without FPC. Each
smoother used all eligible rows, `min_rows = 50`, `bias_correct = TRUE`,
and `overwrite = FALSE`. The variance targets, calculated separately
from the package helpers, were the arcsine working approximation 1/(4n)
and the exact binomial proportion variance p(1-p)/n. The first target is
not the exact finite-sample variance of the transformed estimator.

Relative variance MSE means the average of \[(\hat v-v)/v\]^2, using the
target v on the corresponding scale. Coverage measures symmetric normal
intervals for p or \arcsin\sqrt p, using the same point estimates with
each candidate SE. The weight measure is the average total-variation
distance between normalized 1/\hat v weights and normalized target 1/v
weights; it does not evaluate a fitted hierarchical model.

The recorded raw-scale results were:

| Method     | Relative variance MSE | 95% interval coverage | Weight distance |
|:-----------|----------------------:|----------------------:|----------------:|
| Unsmoothed |                0.7404 |                0.9373 |          0.1984 |
| Loglinear  |                1.5980 |                0.9452 |          0.2042 |
| GAM        |                1.5889 |                0.9442 |          0.2047 |

Both raw-scale smoothers moved pooled coverage closer to 95%, while more
than doubling relative variance MSE and slightly increasing weight
distortion. On the transformed scale, all three methods reproduced the
deterministic variance target to numerical precision, and all had
coverage about 0.9101. Agreement with 1/(4n) therefore did not imply
nominal coverage.

The criteria specified in advance by the maintainer required all of the
following: at least 10% lower pooled relative MSE; no stratum with more
than 10% relative-MSE deterioration; increases in absolute coverage
error of at most 0.01 overall and 0.03 in every stratum; and at least 5%
lower weight distance. For effectively zero baseline errors, matching
that baseline counted as equality, not improvement. None of the four
scale-by-method candidates met all criteria.

These results do not support a general recommendation to smooth or
overwrite SEs. They also do not establish that smoothing is inferior
under other data-generating processes. The simulation contains no
finite-population sampling, suppression, covariance matrices,
multiple-year trends, or downstream hierarchical model. The function
remains an optional, experimental way to compare alternative SEs.

For worked comparisons with a GAM and Fréchet sensitivity results, see
[Variance smoothing and Fréchet sensitivity
analysis](https://joonho112.github.io/sitemix/articles/a7-variance-smoothing-and-frechet.md).
For the scope of the Fréchet calculation itself, see [Pairwise Fréchet
bounds and projected dependence
scenarios](https://joonho112.github.io/sitemix/articles/m7-frechet-envelope-theory.md).

## References

Agresti, A., & Coull, B. A. (1998). Approximate is better than “exact”
for interval estimation of binomial proportions. *The American
Statistician*, *52*(2), 119–126.
<https://doi.org/10.1080/00031305.1998.10480550>

Fay, R. E., & Herriot, R. A. (1979). Estimates of income for small
places: An application of James-Stein procedures to census data.
*Journal of the American Statistical Association*, *74*(366), 269–277.
<https://doi.org/10.1080/01621459.1979.10482505>

Wilson, E. B. (1927). Probable inference, the law of succession, and
statistical inference. *Journal of the American Statistical
Association*, *22*(158), 209–212.
<https://doi.org/10.1080/01621459.1927.10502953>

Wood, S. N. (2017). *Generalized additive models: An introduction with
R* (2nd ed.). Chapman; Hall/CRC. <https://doi.org/10.1201/9781315370279>
