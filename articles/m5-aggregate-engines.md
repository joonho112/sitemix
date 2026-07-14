# M5 · Aggregate engines D0 / D1

Abstract

For methodologists working from publisher-aggregate inputs. Formalizes
the D0 (single-indicator binomial pass-through) and D1 (multi-marginal
working-independence) engines, proves the non-identification of
cross-marginal covariance from marginals alone, and notes that publisher
suppression is an operational concept handled outside the formal model.

## Overview

This article is written for **methodologists** working from
publisher-aggregate inputs — per-site-year counts already aggregated by
the publisher — who need the exact engines `sitemix` dispatches for the
D0 and D1 cases and *why* they take this form. We cover, in order:

1.  the aggregate inputs an analyst sees and what `sitemix` encodes;
2.  the local notation this derivation adds;
3.  the D0 single-indicator binomial pass-through engine;
4.  the D1 multi-marginal working-independence engine and its regime
    mapping;
5.  suppression as an operational (non-statistical) layer;
6.  the implementation invariants.

**Established vs. novel.** *Established:* the Fréchet–Hoeffding
non-identification result for aggregate marginals (Fréchet, 1951;
Hoeffding, 1994). *This package:* the D0 pass-through and D1
working-independence conventions, the formal-D1a vs heuristic-D1b regime
mapping, and suppression as an operational (non-statistical) layer.

| Result | Attribution |
|:---|:---|
| Fréchet–Hoeffding non-identification | Fréchet (1951), Hoeffding (1940) |
| D0/D1 engines + regime mapping + suppression layer | this package (sitemix) |

> **About the example data.** All results here are computed live from
> `alprek_subset`, an anonymized, illustrative 50-site sample of Alabama
> First Class Pre-K records shipped in the package (see
> [`?alprek_subset`](https://joonho112.github.io/sitemix/reference/alprek_subset.md)).
> It is not a real accountability dataset and must not be cited as
> empirical Pre-K results. Every code block runs offline with a fixed
> random seed.

## 1. What an analyst sees, what `sitemix` encodes

Aggregate inputs differ from student-row inputs in what they hand the
estimator: not a per-student table, but per-site-year counts already
aggregated by the publisher. `sitemix` supports two cases:

- **D0** — one numerator and denominator per site-year for a single
  indicator.
- **D1** — multiple aggregate marginals per site-year; the
  cross-marginal joints are not in the file.

The two engines share the aggregate-input parsing pipeline but differ in
covariance assembly.

## 2. Notation map

| Symbol | Meaning | Code | Range |
|:---|:---|:---|:---|
| $`C_{jt}^{\mathrm{agg}}`$ | D0 numerator | `c_jt` | $`\ge 0`$ |
| $`n_{jt}^{\mathrm{agg}}`$ | D0 denominator | `n_jt` | $`> 0`$ |
| $`C_{jt,k}`$ (D1) | Marginal-$`k`$ numerator | `c_jt_k` | $`\ge 0`$ |
| $`n_{jt,k}`$ (D1) | Marginal-$`k`$ denominator | `n_jt_k` | $`> 0`$ |
| $`\Sigma_{jt}^{\mathrm{D1}}`$ | D1 covariance | `V[[i]]$matrix` | diagonal |

## 3. D0 engine — binomial pass-through

**Proposition 1 (D0 reuses Scenario A).** *The D0 engine applied to
$`(C_{jt}^{\mathrm{agg}}, n_{jt}^{\mathrm{agg}})`$ and the Scenario A
counts engine produce identical canonical numerical fields when counts
are valid, non-suppressed and integer-identical and all estimator
options match. Formally, for every such row,*

``` math
\hat\pi_{jt}^{D0} \;=\; C_{jt}^{\mathrm{agg}} / n_{jt}^{\mathrm{agg}},
\qquad
s_{jt}^{D0} = s_{jt}^{A},
\tag{M5.1}
```

*to numerical tolerance.*

The invariant covers `theta_raw`, `theta_hat`, `se_raw`, `se`, and `n`.
Input-path provenance, aggregate-case metadata, suppression state,
object attributes, and path-specific validation errors can differ and
are not part of the identity.

**Attribution.** Standard pass-through; the binomial sampling model
(M1.1) is invariant to whether $`C`$ arrives as a sum of student rows or
a publisher-aggregated count.

## 4. D1 engine — working independence

For multiple aggregate marginals per site-year, the cross- marginal
covariance $`\sigma_{kk'}`$ requires the *joint* count $`C_{jt,kk'}`$,
which the publisher did not provide. The D1 engine therefore emits a
**working-independence diagonal**:

``` math
\boxed{
\Sigma_{jt}^{D1} \;=\; \mathrm{diag}\bigl(s_{jt,1}^2, \ldots, s_{jt,K}^2\bigr).
}
\tag{M5.2}
```

The off-diagonals are set to zero as an explicit working assumption;
they are not identified zeros. `vcov_method = "working_independence"`
records this choice.

With a keyed site-year population size **fpc = N**, each D1 diagonal
uses its own marginal denominator $`n_k`$. Plug-in rows use

``` math
q_k=\begin{cases}
0, & N=n_k,\\
(N-n_k)/(N-1), & N>n_k,
\end{cases}
```

including $`N=n_k=1`$(Cochran, 1977); binomial_bc rows use the design
multiplier $`(N-n_k)/N`$. The single covariance object stores one
population size plus coordinate-aligned sampling fractions and
multipliers. Thus varying denominators remain visible, while n_jt and
n_eff stay missing at matrix level when there is no honest common
scalar. The diagonal contract “row_se_squared” verifies that every
working-independence diagonal equals its row’s scale-specific squared
SE.

Sampling-unit provenance is a separate fact from denominator shape. The
D1 API accepts `sampling_relation = "same_units"`, `"different_units"`,
or `"unknown"` (the default), mapping to `d1_regime = "D1a"`, `"D1b"`,
or `"unknown"`. Equal denominators alone never establish that the
marginals were observed on the same units. `denominator_pattern` records
`"common"` or `"varying"` independently, and `d1_regime_by_group`
preserves both facts for every site-year. Every D1 group must contain
the same ordered indicator set.

**Proposition 2 (Non-identification).** *Without $`C_{jt,kk'}`$, the
cross-marginal covariance is unidentified from the marginals. Any value
in the pairwise Fréchet-Hoeffding interval is feasible pairwise.*

**Proof sketch.** Construct two joint distributions on $`\{0,1\}^K`$
that share marginals but differ in cross-covariance. The
Fréchet-Hoeffding bounds give the pairwise range (Fréchet, 1951;
Hoeffding, 1994). For $`K > 2`$, pairwise feasible choices need not
define a globally compatible joint distribution or PSD covariance
matrix; that is why M7 preserves the formal raw pairwise intervals but
labels PSD-projected corners only as stress scenarios. $`\square`$

**Attribution.** Standard copula-theory result; see M7 and (Nelsen,
2006).

## 5. Suppression as an operational layer

Tier 1 (publisher-suppressed), Tier 2 (observed below accountability),
and Tier 3 (observed and publishable) are **operational, not
statistical** concepts. They live in the
[`sm_suppression_report()`](https://joonho112.github.io/sitemix/reference/sm_suppression_report.md)
audit, not in the statistical model derived here. `suppression = "drop"`
retains an unavailable audit row with canonical point and SE fields
missing. The legacy `"upper_bound"` label is available only as an
explicitly acknowledged, separated worst-case variance sensitivity; it
does not create an identified point, ordinary covariance, or formal
Fréchet input. A hidden denominator cannot support a numeric variance
claim. The formal model in §3–§4 uses identified rows only.

## 6. Implementation invariants

| ID | Layer | Claim |
|:---|:---|:---|
| AG1 | engine-aggregate-d0 | (M5.1) — qualified canonical numerical fields agree to tolerance; path provenance may differ. |
| AG2 | engine-aggregate-d1 | (M5.2) — D1 `V` off-diagonals are exactly zero. |
| AG3 | engine-aggregate-d1 | `vcov_method = "working_independence"` and diagonal equals row `se^2`. |

Verify on `alprek_subset`:

``` r

counts <- readRDS(system.file("extdata", "alprek_subset_counts.rds",
                              package = "sitemix"))

# AG1: D0 = Scenario A
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
canonical_numeric <- c("theta_raw", "theta_hat", "se_raw", "se", "n")
stopifnot(all(vapply(canonical_numeric, function(field) {
  isTRUE(all.equal(est_a[[field]], est_d0[[field]], tolerance = 1e-10))
}, logical(1))))
stopifnot(!identical(est_a$input_mode, est_d0$input_mode))

# AG2 + AG3: D1 working independence
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

All three invariants hold.

## 7. Where to go next

- [M7 · Fréchet envelope
  theory](https://joonho112.github.io/sitemix/articles/m7-frechet-envelope-theory.md)
  for formal raw pairwise intervals and the separate K \> 2 projected
  stress-scenario semantics.
- [A5 · Published aggregates D0 /
  D1](https://joonho112.github.io/sitemix/articles/a5-published-aggregates.md)
  for the applied walkthrough.
- [M2 · Scalar SE —
  binomial](https://joonho112.github.io/sitemix/articles/m2-scalar-se-binomial.md)
  for the row-level SE derivation reused by D0.
- [M6 · Variance smoothing
  theory](https://joonho112.github.io/sitemix/articles/m6-variance-smoothing-theory.md)
  for the experimental append-only log-variance smoother that can be
  applied to the same row-level SEs.

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
