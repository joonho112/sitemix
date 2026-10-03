% Please edit documentation in R/frechet.R
\encoding{UTF-8}
\name{sm_frechet_envelope}
\alias{sm_frechet_envelope}
\title{Explore dependence between published marginal proportions}
\usage{
sm_frechet_envelope(
  x,
  indicator = NULL,
  population_regime = NULL,
  subgroup_conditional_action = c("warn", "allow", "error"),
  return_correlations = FALSE,
  psd_method = c("higham", "shrink"),
  psd_tol = 1e-08,
  psd_max_iter = 100L,
  shrink_alpha = NULL,
  ...
)
}
\arguments{
\item{x}{A D1 aggregate-path \code{sitemix_estimates} object. Both
\code{vjt = FALSE} and
\code{vjt = TRUE} outputs are supported; formal D1a validation uses
the recorded sampling and variance information in rows and, when
present, matrices.}

\item{indicator}{Character vector or \code{NULL} (default
\code{NULL}). Optional indicators to retain before envelope
construction. \code{NULL} retains all indicators present in
\code{x}.}

\item{population_regime}{Character scalar. Required regime label.
One of \code{"d1a"} (formal common-population) or \code{"d1b"}
(subgroup-conditional heuristic). The default \code{NULL} is rejected;
supply a regime explicitly. See \emph{Errors}.}

\item{subgroup_conditional_action}{Character scalar. Action for
\code{population_regime = "d1b"}. One of \code{"warn"} (default;
proceed with a warning), \code{"allow"} (proceed silently), or
\code{"error"} (abort). Ignored when
\code{population_regime = "d1a"}.}

\item{return_correlations}{Logical scalar. If \code{TRUE}, return
the raw pairwise Fréchet correlation endpoints as matrices in
\code{pairwise_correlation_lower} and
\code{pairwise_correlation_upper}. Defaults to \code{FALSE}; invalid
values raise the condition documented under \emph{Errors}.}

\item{psd_method}{Character scalar. PSD repair method: \code{"higham"}
(default; \code{Matrix::nearPD()}) or \code{"shrink"} (line search toward
the working-independence diagonal).}

\item{psd_tol}{Positive numeric scalar. Relative PSD tolerance for the
smallest eigenvalue and projection convergence. Eigenvalue checks scale
this value by the matrix eigenvalue scale, with a machine-range absolute
floor. Must not exceed \code{sqrt(.Machine$double.eps)} so that this
numerical allowance cannot relabel materially indefinite matrices as PSD.
Defaults to \code{1e-8}.}

\item{psd_max_iter}{Positive integer scalar. Maximum iterations
for the PSD repair algorithm. Defaults to \code{100L}.}

\item{shrink_alpha}{Positive numeric scalar in \eqn{(0, 1]}{(0, 1]}
or \code{NULL} (default \code{NULL}). Weight on the source corner
when mixing it with the independence diagonal. For \eqn{K > 2}{K > 2},
a supplied value is used exactly; \code{NULL} searches for the largest
weight that yields a PSD matrix. With \eqn{K \le 2}{K <= 2}, the
corners are returned unchanged. A supplied weight requires
\code{psd_method = "shrink"} even in that case.}

\item{...}{Additional arguments forwarded to
\code{Matrix::nearPD()} when \code{psd_method = "higham"}:
\code{base.matrix}, \code{doSym}, \code{doDykstra},
\code{posd.tol}, and \code{conv.norm.type}. Arguments that conflict with
the required diagonal, tolerance, iteration, symmetry, or output
settings are rejected.}
}
\value{
An \code{sm_frechet_envelope} list object with the
following fields:
\describe{
\item{\code{raw_pairwise_intervals}}{One row per site-year and
indicator pair, with raw pairwise joint-probability, correlation,
and covariance interval endpoints. The covariance scale is raw
probability. The row-level \code{interval_scope} distinguishes
formal D1a intervals from D1b heuristic ranges.}
\item{\code{V_independence}}{List of per-site-year
raw-scale covariance matrices. For formal D1a this is the IID
plug-in diagonal, not the transformed row \code{V}.}
\item{\code{unprojected_negative_dependence_corner},
\code{unprojected_positive_dependence_corner}}{Lists of matrices
assembled from the raw pairwise covariance endpoints before PSD
projection. They are corners, not globally ordered covariance bounds.}
\item{\code{projected_negative_dependence_stress},
\code{projected_positive_dependence_stress}}{Lists of PSD-projected
corner stress scenarios. They are not lower/upper bounds.}
\item{\code{pairwise_correlation_lower},
\code{pairwise_correlation_upper}}{Lists of raw pairwise correlation
endpoint matrices when \code{return_correlations = TRUE}. Both slots
are always present on the object; they are \code{NULL} otherwise.}
\item{\code{projection_diagnostics}}{A table with
one row per site-year and stress scenario. Rows record estimate and
covariance scales; method, status, attempted/converged state and
iterations; relative and realized absolute tolerances; before/after
eigenvalue scales and minima; requested/applied shrinkage; raw and
projected norms and distances; and diagonal, symmetry, PSD,
sign changes, scenario ordering, and departures from raw intervals.}
\item{\code{site_keys}}{Site-year identifier mapping for the
list slots.}
\item{\code{call}}{The function call as captured by
\code{match.call()}.}
\item{\code{population_regime}}{Character scalar; the regime
label passed to \code{population_regime}.}
\item{\code{frechet_scope}}{Character scalar; either formal raw
pairwise intervals or a heuristic D1b stress range.}
\item{\code{covariance_scale}, \code{projected_scenario_role}}{
The covariance scale and the interpretation of projected matrices.}
\item{\code{psd_method}, \code{psd_tol}, \code{psd_max_iter},
\code{shrink_alpha}, \code{projection_config}}{
PSD repair settings actually applied.}
\item{\code{V_lower_raw}, \code{V_upper_raw},
\code{V_lower_psd}, \code{V_upper_psd}, \code{R_lower},
\code{R_upper}}{Deprecated compatibility aliases for the
matrix fields above. New code should not use these names because
projected matrices are not bounds.}
\item{\code{psd_diagnostics}}{Deprecated one-row-per-site-year wide
compatibility table. New code should use
\code{projection_diagnostics}.}
}
}
\description{
\code{sm_frechet_envelope()} calculates pairwise Fréchet intervals for
published marginal proportions estimated through the D1 aggregate path.
It also builds covariance matrices for exploring negative and positive
dependence. Read \code{raw_pairwise_intervals} for the pairwise ranges
and \code{projection_diagnostics} before using the matrices. The ranges
have a formal interpretation only under the D1a conditions below.
}
\details{
Choose \code{population_regime} to describe how the published
marginals relate to one another:

\describe{
\item{\strong{D1a -- formal common-population}}{The published
marginals refer to the same sampled units, use one finite denominator
per site-year, and use the IID plug-in raw-probability variance rule
without FPC or \code{binomial_bc}, with \code{anscombe = FALSE}
so that \code{n_eff = n}. These conditions must already be
recorded by the D1 estimator. Under them, each raw pairwise covariance
interval is formal. Equal denominator values alone do not establish
that the marginals describe the same units.}
\item{\strong{D1b -- subgroup-conditional heuristic}}{The
published marginals are subgroup-conditional and the envelope
is a \strong{heuristic stress test}, not a formal bound.
Requires explicit acknowledgement via
\code{subgroup_conditional_action = "allow"} (or proceeds
with a warning under \code{"warn"}); \code{"error"} aborts
the call. The input must record \code{sampling_relation} as
\code{"different_units"} or \code{"unknown"}; a same-unit D1a
input cannot be relabeled as D1b in this call.}
}

The function assembles matrices from the lower and upper pairwise
endpoints. For more than two indicators, those matrices need not be
positive semidefinite (PSD), so they are adjusted with
\code{psd_method = "higham"} (using \code{Matrix::nearPD()}) or
\code{"shrink"} (toward the working-independence diagonal). Both
methods preserve the raw-scale diagonal. The resulting matrices are
dependence stress scenarios: they are not multivariate lower or upper
bounds or validated \code{sm_vcov} objects. PSD alone does not establish
that all pairwise entries can arise from a joint binary distribution.
Projection can change a pair's sign, move it outside its raw pairwise
interval, or reverse the elementwise order of the two scenarios.
\code{projection_diagnostics} records these changes.
For \eqn{K \le 2}{K <= 2}, both projected fields are exact identities of
their source corners and no iterative method is attempted. For
\eqn{K > 2}{K > 2}, a fixed \code{shrink_alpha} is applied exactly even
when the source corner is already PSD; an infeasible fixed value errors.
Higham and automatic-shrink nonconvergence also error rather than returning
a partially repaired matrix.

For the formal derivation see
\code{vignette("m7-frechet-envelope-theory", package = "sitemix")}.
Suppressed-missing and suppression-sensitivity rows are rejected at
entry: formal pairwise intervals require complete identified marginals.
}
\section{Errors}{

Omitting \code{population_regime} raises
\code{sitemix_error_population_regime_required}. Other invalid regime
values raise \code{sitemix_error_invalid_population_regime}.
Invalid \code{return_correlations} values raise
\code{sitemix_error_invalid_return_correlations}.
}

\examples{
\dontshow{set.seed(1L)}
# D1 example using bundled counts as a synthetic aggregate slice
counts_path <- system.file(
  "extdata", "prek_sim_counts.rds",
  package = "sitemix", mustWork = TRUE
)
counts <- readRDS(counts_path)

# Build a tiny D1 input: two marginals (frpm, snap), one year
d1 <- counts[counts$year == 2024, ]
d1_long <- rbind(
  data.frame(
    site_id   = d1$site_id, year = d1$year,
    indicator = "frpm",
    c_jt      = d1$c_jt_frpm, n_jt = d1$n_jt
  ),
  data.frame(
    site_id   = d1$site_id, year = d1$year,
    indicator = "snap",
    c_jt      = d1$c_jt_snap, n_jt = d1$n_jt
  )
)

est <- sm_estimate_from_aggregates(
  d1_long, family = "multivariate",
  indicator_col = "indicator",
  sampling_relation = "same_units",
  vjt = TRUE
)
env <- sm_frechet_envelope(est, population_regime = "d1a")
class(env)
env$psd_method
head(env$raw_pairwise_intervals)
head(env$projection_diagnostics)

}
\references{
Fréchet, M. (1951). Sur les tableaux de corrélation dont les
marges sont données. \emph{Annales de l'Université de Lyon, 3e série,
Section A: Sciences Mathématiques et Astronomie}, \bold{14}, 53--77.

Hoeffding, W. (1994). Scale-Invariant Correlation Theory. In
N. I. Fisher and P. K. Sen (Eds.), \emph{The Collected Works of
Wassily Hoeffding} (pp. 57--107). Springer.
doi:10.1007/978-1-4612-0865-5_4. (English reprint of the 1940 original.)

Nelsen, R. B. (2006). \emph{An Introduction to Copulas} (2nd
ed.). Springer.

Higham, N. J. (2002). Computing the nearest correlation matrix---a
problem from finance. \emph{IMA Journal of Numerical Analysis},
\bold{22}(3), 329--343. doi:10.1093/imanum/22.3.329.
}
\seealso{
\itemize{
\item \code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}}
for the D1 estimator producing the working-independence \code{V}.
\item \code{\link[=sm_vcov]{sm_vcov()}} for covariance objects and
their \code{vcov_scale} field.
\item \code{vignette("a7-variance-smoothing-and-frechet")} for the
applied walkthrough.
\item \code{vignette("m7-frechet-envelope-theory")} for the formal
derivation and D1a / D1b distinction.
}

Other covariance:
\code{\link[=sm_vcov]{sm_vcov()}}
}
\concept{covariance}
