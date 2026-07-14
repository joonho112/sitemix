# M1 · Statistical foundations — sampling uncertainty

Abstract

For methodologists and reviewers who need the exact specification behind
every `sitemix` call. This vignette derives the design-based sampling
model, defines the notation every later method vignette inherits, lists
the invariants every `sitemix_estimates` tibble must satisfy, and maps
each canonical column onto its symbol.

## Overview

This article is written for **methodologists and reviewers** who want
the exact specification behind every `sitemix` call and *why* it takes
this form. We cover, in order:

1.  the sampling tuple an analyst sees and the schema `sitemix` encodes;
2.  the locked notation the whole method track inherits;
3.  the within-site binomial sampling model and its closed-form arcsine
    SE;
4.  why downstream modeling is out of scope;
5.  the implementation invariants every `sitemix_estimates` tibble
    satisfies;
6.  the limitations that carry forward to every later method vignette.

**Established vs. novel.** *Established:* the binomial sampling model,
the arcsine variance-stabilizing transform (Anscombe, 1948), and the
known-sampling-variance setup familiar from Fay–Herriot small-area
estimation (Fay & Herriot, 1979). *This package:* the
`sitemix_estimates` schema, the locked cross-track notation, and the
invariant contract that ties each documented claim to a runnable
identity check.

> **About the example data.** All results here are computed live from
> `alprek_subset`, an anonymized, illustrative 50-site sample of Alabama
> First Class Pre-K records shipped in the package (see
> [`?alprek_subset`](https://joonho112.github.io/sitemix/reference/alprek_subset.md)).
> It is not a real accountability dataset and must not be cited as
> empirical Pre-K results. Every code block runs offline with a fixed
> random seed.

## 1. What an analyst sees, what `sitemix` encodes

For a panel of sites $`j = 1, \ldots, J`$ observed across years
$`t = 1, \ldots, T`$, an analyst sees a tuple per site-year-indicator:

``` math
\mathcal{D}_{jt,k} \;=\; \bigl(\hat\theta_{jt,k}, \;s_{jt,k}, \;n_{jt}, \;V_{jt}\bigr).
```

$`\hat\theta_{jt,k}`$ is the direct (design-based) point estimate of
indicator $`k`$ at site $`j`$ in year $`t`$; $`s_{jt,k}`$ is its scalar
standard error on the row’s `estimate_scale`; $`n_{jt}`$ is the
denominator (the same across $`k`$ for student-row scenarios);
$`V_{jt}`$ is the optional joint covariance across indicators.

`sitemix` produces this uncertainty tuple from three input shapes
(student rows, sufficient counts, published aggregates) under five
scenario dispatches (A / B / C / D0 / D1). Downstream analyses may
report the rates directly, compare groups, or fit a model that treats
sampling uncertainty as known. sitemix does not prescribe a downstream
model. This separation mirrors the known-sampling-variance setup used in
Fay-Herriot small-area estimation (Fay & Herriot, 1979).

## 2. Notation map

The locked notation table used throughout the method track:

| Symbol | Meaning | Code column | Range |
|:---|:---|:---|:---|
| $`j`$ | Site index | `site_id` | $`1, \ldots, J`$ |
| $`t`$ | Year index | `year` | integer |
| $`k`$ | Indicator / category index | `indicator` | $`1, \ldots, K`$ |
| $`\theta_{jt,k}`$ | True site-year rate | (latent) | scale-dep. |
| $`\hat\theta_{jt,k}`$ | Direct estimate | `theta_hat` | scale-dep. |
| $`s_{jt,k}`$ | Scalar SE on `estimate_scale` | `se` | $`\ge 0`$; zero only at an exact SRSWOR census or a boundary cell on the diagnostic `vst = "none"`, `boundary_method = "none"` path |
| $`V_{jt}`$ | Within-site covariance | `V` (list-col) | PSD; $`K\times K`$ |
| $`n_{jt}`$ | Site-year denominator | `n` | $`> 0`$ integer |
| $`C_{jt,k}`$ | Numerator (success count) | `c_jt` etc. | $`0 \le C \le n`$ |
| $`\hat\pi_{jt,k}`$ | Raw-scale proportion | `theta_raw` | $`[0, 1]`$ |

Each method vignette m2–m8 introduces additional symbols local to its
derivation; this notation map is the canonical home for every symbol
shared across the method track.

## 3. The within-site sampling model

For Scenario A (binomial, student rows or counts), the within-site
sampling model is

``` math
C_{jt} \mid n_{jt}, \theta_{jt} \;\sim\; \mathrm{Binomial}(n_{jt}, \theta_{jt}),
\tag{M1.1}
```

where $`\theta_{jt} \in [0, 1]`$ is the raw-scale true rate. The MLE is
$`\hat\pi_{jt} = C_{jt}/n_{jt}`$ with closed-form delta-method SE on the
arcsine-stabilized scale:

``` math
\boxed{
\begin{aligned}
\hat\theta_{jt} &\;=\; \arcsin\sqrt{\hat\pi_{jt}}, \\
s_{jt} &\;=\; \frac{1}{2\sqrt{n_{jt}}}.
\end{aligned}}
\tag{M1.2}
```

The boxed identity is the first-order working result on the reported
arcsine scale for the no-FPC, uncorrected Scenario A baseline. Scenarios
B and C additionally return a scenario-specific joint covariance
$`V_{jt}`$. In the current package that matrix is stored on the
raw-proportion scale, so it does **not** replace $`s_{jt}^2`$ unless
`vcov_scale` and `estimate_scale` are compatible. M3 and M4 derive the
raw matrices; M8 defines the compatibility gate and the scalar/matrix
diagonal contracts.

For Scenarios D0 and D1, the same closed-form holds on the implied
counts; the formal proof of equivalence is the **T2.5 sufficient-counts
identity** derived in M2.

## 4. Downstream modeling is out of scope

`sitemix` does not fit a population model or perform shrinkage. A
compatible downstream consumer can read the direct columns `theta_hat`,
`se`, and `estimate_scale`; a joint consumer can also read `V` together
with its covariance metadata. Downstream uses are peers: some report the
rates directly, some compare groups or run a meta-analysis, and some fit
a hierarchical model. As one such example, a hierarchical consumer might
compute a posterior mean

``` math
\tilde\theta_{jt} \;=\; \mathrm{E}[\theta_{jt} \mid \hat\theta_{jt}, s_{jt}; G],
\tag{M1.3}
```

but the prior $`G`$, fitting method, and software interface are choices
made outside sitemix. See M8 for the package-neutral output contract.

## 5. Implementation invariants

Each invariant ties a documented claim to a runnable identity check on
`alprek_subset`.

| ID | Layer | Claim |
|:---|:---|:---|
| I-1 | column–symbol | Every column of `sitemix_estimates` maps to a documented symbol. |
| I-2 | finite output | Every row’s `theta_hat` is finite. |
| I-3 | nonnegative SE | Every estimable row’s `se` is finite and nonnegative. |
| I-4 | PSD V | Every `V[[i]]` is PSD up to documented tolerance. |
| I-5 | schema audit | `sm_diagnose(level = "row")` returns 0 error-severity flags. |

Verify on a Scenario A slice:

``` r

data(alprek_subset, package = "sitemix")
est <- sm_estimate(
  subset(alprek_subset, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)

# I-2: finite theta_hat
stopifnot(all(is.finite(est$theta_hat)))

# I-1: required columns are present and scale is row-level metadata
stopifnot(all(c("theta_hat", "se", "estimate_scale") %in% names(est)))
stopifnot(identical(unique(est$estimate_scale), "arcsine"))

# I-3: finite nonnegative SE for estimable rows
stopifnot(all(is.finite(est$se)))
stopifnot(all(est$se >= 0))

# I-5: clean diagnostic
diag_r <- sm_diagnose(est, level = "row", verbose = FALSE)
stopifnot(all(c("flag_small_n", "flag_zero_cell") %in% names(diag_r)))
stopifnot(!any(diag_r$diag_severity == "error", na.rm = TRUE))
```

Each invariant evaluates to `TRUE` (silent
[`stopifnot()`](https://rdrr.io/r/base/stopifnot.html)). For Scenarios B
/ C / D1 invariants involving the matrix-valued $`V_{jt}`$, see M3 / M4
/ M5.

## 6. Limitations and assumptions

This specification covers **sitemix’s design-based estimation** only.
Three limitations carry forward to every later method vignette:

- **Estimated sampling variance.** `se` and `V` quantify sampling
  uncertainty using the selected method. A downstream analysis is
  responsible for deciding whether to treat those values as fixed.
- **No model on $`\theta_{jt}`$ here.** sitemix does not place a prior
  or population model; a downstream analysis supplies one when the
  research question requires it.
- **Scale lock at construction.** The row-level `estimate_scale` column
  is fixed by the `vst` argument and is invariant across downstream
  consumers.

## 7. Where to go next

- [M2 · Scalar SE —
  binomial](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
  for the closed-form derivation of (M1.2) and the T2.5 identity.
- [M3 · Multivariate SUR
  covariance](https://joonho112.github.io/sitemix/articles/m3-multivariate-sur-covariance.md)
  for the Scenario B extension of $`V_{jt}`$.
- [M8 · Output
  contract](https://joonho112.github.io/sitemix/articles/m8-output-contract.md)
  for the package-neutral output contract.
- [A1 · Getting
  started](https://joonho112.github.io/sitemix/articles/a1-getting-started.md)
  for the applied face of this specification.

## References

Anscombe, F. J. (1948). The transformation of Poisson, binomial and
negative-binomial data. *Biometrika*, *35*(3/4), 246–254.
<https://doi.org/10.1093/biomet/35.3-4.246>

Fay, R. E., & Herriot, R. A. (1979). Estimates of income for small
places: An application of James-Stein procedures to census data.
*Journal of the American Statistical Association*, *74*(366), 269–277.
<https://doi.org/10.1080/01621459.1979.10482505>
