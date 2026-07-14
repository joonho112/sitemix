# M7 · Fréchet pairwise intervals and projected stress theory

Abstract

For methodologists evaluating D1 cross-marginal sensitivity. Derives the
Fréchet-Hoeffding pairwise intervals on the joint cross-marginal
probability, distinguishes the formal D1a (common-population) regime
from the heuristic D1b (subgroup- conditional) regime, and separates
those intervals from PSD-projected corner stress scenarios.

## Overview

This article is written for **methodologists** evaluating D1
cross-marginal sensitivity who need the exact pairwise intervals
`sitemix` reports and *why* their PSD projections are labeled stress
scenarios rather than bounds. We cover, in order:

1.  what an analyst sees and what `sitemix` encodes;
2.  the local notation this derivation adds;
3.  the Fréchet–Hoeffding bounds;
4.  the formal-D1a vs heuristic-D1b distinction;
5.  why PSD projection creates stress scenarios, not bounds;
6.  the implementation invariants.

**Established vs. novel.** *Established:* the Fréchet–Hoeffding bounds
(Fréchet, 1951; Hoeffding, 1994) and the copula framing (Nelsen, 2006).
*This package:* the formal-D1a vs heuristic-D1b distinction, and the PSD
projection that yields labeled stress scenarios (explicitly NOT
lower/upper bounds).

| Result | Attribution |
|:---|:---|
| Fréchet–Hoeffding bounds | Fréchet (1951), Hoeffding (1940) |
| copula framing | Nelsen (2006) |
| D1a/D1b distinction + projected stress scenarios | this package (sitemix) |

> **About the example data.** All results here are computed live from
> `alprek_subset`, an anonymized, illustrative 50-site sample of Alabama
> First Class Pre-K records shipped in the package (see
> [`?alprek_subset`](https://joonho112.github.io/sitemix/reference/alprek_subset.md)).
> It is not a real accountability dataset and must not be cited as
> empirical Pre-K results. Every code block runs offline with a fixed
> random seed.

## 1. What an analyst sees, what `sitemix` encodes

In Scenario D1, the publisher provides marginal proportions
$`\pi_{jt,k}`$ for $`K`$ indicators but **not** the joint proportions
$`\pi_{jt,kk'}`$. The cross-marginal covariance is unidentified from the
marginals alone (M5 §4). The Fréchet- Hoeffding bounds are the tightest
interval the marginals can support; any joint distribution consistent
with the marginals must lie in this interval.

## 2. Notation map

| Symbol | Meaning | Code | Range |
|:---|:---|:---|:---|
| $`\pi_x, \pi_y`$ | Marginal proportions for two indicators | `theta_raw` | $`[0, 1]`$ |
| $`\pi_{\mathrm{lower}}`$ | Fréchet lower bound on joint | (internal) | $`\max(0, \pi_x + \pi_y - 1)`$ |
| $`\pi_{\mathrm{upper}}`$ | Fréchet upper bound on joint | (internal) | $`\min(\pi_x, \pi_y)`$ |
| $`\sigma_{kk'}^{D1a}`$ | Formal raw pairwise covariance interval | `raw_pairwise_intervals` | pairwise Fréchet endpoints |
| $`\Sigma^-_{\mathrm{stress}}, \Sigma^+_{\mathrm{stress}}`$ | PSD-projected corner stress matrices | `projected_negative_dependence_stress` / `projected_positive_dependence_stress` | not ordered bounds |

## 3. The Fréchet-Hoeffding bounds

**Proposition 1 (Fréchet-Hoeffding bounds).** *For binary marginals
$`\pi_x, \pi_y \in [0, 1]`$, every joint proportion $`\pi_{xy}`$
consistent with the marginals satisfies*

``` math
\boxed{
\max(0, \pi_x + \pi_y - 1) \;\le\; \pi_{xy} \;\le\; \min(\pi_x, \pi_y).
}
\tag{M7.1}
```

**Attribution.** Classical Fréchet-Hoeffding bounds (Fréchet, 1951;
Hoeffding, 1994); see also Nelsen (2006) §2.5.

The probability bounds imply pairwise correlation bounds. For formal
D1a, the raw covariance endpoint is
$`\sigma_{kk'} = (\pi_{xy} - \pi_x \pi_y) / n_{jt}`$ (M3.1).
`raw_pairwise_intervals` stores the two marginals, common denominator,
joint probability endpoints, correlation endpoints, and these raw
covariance endpoints directly. Formal D1a therefore requires explicit
`same_units` provenance, one finite common denominator per site-year,
the IID plug-in variance rule, and no FPC or `binomial_bc` correction.

For heuristic D1b, there is no justified common $`n_{jt}`$. The object
records `n_common = NA` and constructs a heuristic covariance range as

``` math
V_{kk'}^{\mathrm{corner}} = \rho_{kk'}^{\mathrm{corner}} s_k s_{k'},
```

where $`s_k`$ and $`s_{k'}`$ are raw-scale row standard errors. The
`covariance_construction` and `interval_scope` columns keep this
heuristic range separate from the formal `/n` result.

## 4. D1a vs D1b — formal vs heuristic

**D1a (formal).** When the publisher marginals refer to the same sampled
units and satisfy the compatible denominator/design contract, each raw
pairwise interval is *formally valid*: any feasible pairwise joint
distribution must lie in that interval. Equal denominators alone do not
establish same-unit provenance.

**D1b (heuristic).** When the marginals are *subgroup- conditional*
(e.g., FRPM rate among ELL students vs FRPM rate among non-ELL
students), the marginals refer to different populations and the Fréchet
interval is *not* a formal joint constraint. The returned ranges and
projected matrices are then heuristic stress quantities under an
explicit counterfactual dependence construction.

[`sm_frechet_envelope()`](https://joonho112.github.io/sitemix/reference/sm_frechet_envelope.md)
requires explicit acknowledgement of this distinction via the
`population_regime` argument (`"d1a"` or `"d1b"`).

## 5. PSD projection creates scenarios, not bounds

The matrices assembled from all negative or all positive pairwise
endpoints are not guaranteed to be PSD. The function applies one of two
PSD repair methods:

- **`psd_method = "higham"`** (default) — Higham’s nearest-PSD algorithm
  via [`Matrix::nearPD()`](https://rdrr.io/pkg/Matrix/man/nearPD.html).
  Iterates using relative eigenvalue and convergence tolerances. The
  final PSD check requires the smallest eigenvalue to be no smaller than
  minus `psd_tol` times the matrix eigenvalue scale, plus a
  machine-range absolute floor. A `nearPD()` result whose `converged`
  flag is false is rejected. Supported `...` controls are sanitized and
  stored; package-owned diagonal, tolerance, iteration, symmetry, and
  output controls cannot be overridden (Higham, 2002).

The public relative tolerance is restricted to
`0 < psd_tol <= sqrt(.Machine$double.eps)`. This keeps it a
floating-point allowance rather than a policy knob that could relabel a
materially indefinite corner as already PSD. -
**`psd_method = "shrink"`** — line search for the largest shrinkage
weight $`\alpha \in (0, 1]`$ such that
$`\alpha \cdot \Sigma + (1 - \alpha) \cdot \mathrm{diag}(\Sigma)`$ is
PSD. Preserves the diagonal exactly. Automatic search errors when its
alpha interval has not converged by `psd_max_iter`. A fixed alpha is the
requested raw-corner retention and is applied exactly for `K > 2`, even
when the source corner is already PSD; an infeasible value errors.

For `K <= 2`, both projected fields are exact identities of the source
corners; no Higham or shrink projection is attempted.

For `K > 2`, projection is a global matrix operation. An off-diagonal
can change sign, the projected negative-corner value can exceed the
projected positive-corner value, and either projected value can leave
its raw pairwise interval. Consequently the projected matrices are named
stress scenarios. `projection_diagnostics` is canonical long data with
one row per site-year and stress scenario. In addition to sign changes,
projected-order reversals, and raw-interval violations, it records scale
labels, method/status, attempted/converged state, iterations, relative
and realized absolute tolerances, eigenvalue scales and minima,
requested/applied shrinkage, projection distance, and
diagonal/symmetry/PSD invariants. Deprecated `psd_diagnostics` remains a
wide compatibility view.

## 6. Implementation invariants

| ID | Layer | Claim |
|:---|:---|:---|
| FR1 | frechet | D1a `raw_pairwise_intervals` reconstructs every covariance endpoint as $`(q-p_kp_l)/n`$. |
| FR2 | frechet | Each projected corner stress matrix satisfies the scale-aware relative `psd_tol` criterion and preserves the raw plug-in diagonal. |
| FR3 | frechet | For `K=2`, no projection is needed, so both stress matrices equal their source corners and pairwise ordering is preserved. |
| FR4 | frechet | For `K>2`, sign/order/range changes are diagnostics, not evidence of multivariate bounds. |
| FR5 | frechet | Every `K>2` projected stress matrix is deterministically replayable from its source corner and stored `projection_config`; nonconvergence is a hard error. |

Verify on a synthetic D1 slice:

``` r

counts <- readRDS(system.file("extdata", "alprek_subset_counts.rds",
                              package = "sitemix"))
d1 <- counts[counts$year == 2024, ]
d1_long <- rbind(
  data.frame(site_id = d1$site_id, year = d1$year,
             indicator = "frpm", c_jt = d1$c_jt_frpm, n_jt = d1$n_jt),
  data.frame(site_id = d1$site_id, year = d1$year,
             indicator = "snap", c_jt = d1$c_jt_snap, n_jt = d1$n_jt)
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
env <- sm_frechet_envelope(est_d1, population_regime = "d1a")

V_indep <- as.matrix(env$V_independence[[1L]])
raw_intervals <- env$raw_pairwise_intervals
V_negative <- as.matrix(env$projected_negative_dependence_stress[[1L]])
V_positive <- as.matrix(env$projected_positive_dependence_stress[[1L]])

# FR1: direct formal /n oracle
row1 <- raw_intervals[raw_intervals$site_key == env$site_keys$site_key[[1L]], ][1L, ]
stopifnot(all.equal(
  row1$pairwise_covariance_lower,
  (row1$joint_probability_lower - row1$p_1 * row1$p_2) / row1$n_common
))
stopifnot(all.equal(
  row1$pairwise_covariance_upper,
  (row1$joint_probability_upper - row1$p_1 * row1$p_2) / row1$n_common
))

# FR2: PSD after repair (relative tolerance + machine-range floor)
eig_negative <- eigen(V_negative, symmetric = TRUE, only.values = TRUE)$values
eig_positive <- eigen(V_positive, symmetric = TRUE, only.values = TRUE)$values
tol_negative <- env$psd_tol * max(abs(eig_negative)) +
  nrow(V_negative) * .Machine$double.xmin
tol_positive <- env$psd_tol * max(abs(eig_positive)) +
  nrow(V_positive) * .Machine$double.xmin
stopifnot(min(eig_negative) >= -tol_negative)
stopifnot(min(eig_positive) >= -tol_positive)

# FR3: K=2 shortcuts preserve their source corners
stopifnot(nrow(V_negative) == 2L)
stopifnot(all.equal(
  V_negative,
  as.matrix(env$unprojected_negative_dependence_corner[[1L]])
))
stopifnot(all.equal(
  V_positive,
  as.matrix(env$unprojected_positive_dependence_corner[[1L]])
))
stopifnot(all.equal(diag(V_negative), diag(V_indep), tolerance = 1e-8))
stopifnot(all.equal(diag(V_positive), diag(V_indep), tolerance = 1e-8))
```

The first three invariants hold for the two-indicator bundled example
(relative iterative-solver tolerance `1e-8`). For `K > 2`, pairwise
Fréchet feasibility does not by itself guarantee global joint
compatibility; the PSD step produces diagnostic scenarios and cannot
promote pairwise intervals into multivariate bounds.

## 7. Where to go next

- [M5 · Aggregate engines D0 /
  D1](https://joonho112.github.io/sitemix/articles/m5-aggregate-engines.md)
  for the working-independence baseline and D1 provenance contract.
- [A7 · Variance smoothing and
  Fréchet](https://joonho112.github.io/sitemix/articles/a7-variance-smoothing-and-frechet.md)
  for the applied diagnostic-envelope workflow.
- [M8 · Output
  contract](https://joonho112.github.io/sitemix/articles/m8-output-contract.md)
  for the package-neutral scalar and covariance contract, including why
  these projections stay separate from ordinary covariance.

## References

Fréchet, M. (1951). Sur les tableaux de corrélation dont les marges sont
données. *Annales de l’Université de Lyon, 3e série, Section A: Sciences
Mathématiques Et Astronomie*, *14*, 53–77.

Higham, N. J. (2002). Computing the nearest correlation matrix—a problem
from finance. *IMA Journal of Numerical Analysis*, *22*(3), 329–343.
<https://doi.org/10.1093/imanum/22.3.329>

Hoeffding, W. (1994). Scale-invariant correlation theory. In N. I.
Fisher & P. K. Sen (Eds.), *The collected works of wassily hoeffding*
(pp. 57–107). Springer. <https://doi.org/10.1007/978-1-4612-0865-5_4>

Nelsen, R. B. (2006). *An introduction to copulas* (2nd ed.). Springer.
<https://doi.org/10.1007/0-387-28678-0>
