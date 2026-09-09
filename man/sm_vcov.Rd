% Please edit documentation in R/sm_vcov.R
\encoding{UTF-8}
\name{sm_vcov}
\alias{sm_vcov}
\title{Construct or inspect a within-site covariance object}
\usage{
sm_vcov(
  matrix,
  site_id = NA_character_,
  year = NA_integer_,
  indicator_order = colnames(matrix),
  family,
  vcov_method = NA_character_,
  estimate_scale,
  vcov_scale,
  matrix_boundary_rule = "none",
  scalar_correction_rule = rep("none", length(indicator_order)),
  psd_repair = "none",
  matrix_rank = NULL,
  positive_support = NA_integer_,
  n_jt = NA_integer_,
  n_eff = NA_real_,
  population_size = NA_real_,
  sampling_fraction = NA_real_,
  fpc_variance_multiplier = 1,
  fpc_se_multiplier = 1,
  variance_multiplier_applied = 1,
  se_multiplier_applied = 1,
  sampling_design = "not_specified",
  variance_rule = "plugin",
  diag_contract = "not_checked"
)
}
\arguments{
\item{matrix}{A symmetric positive semidefinite numeric matrix of dimension
\code{K x K} with \eqn{K \ge 1}{K >= 1}. Rows and columns are
indexed by \code{indicator_order}.}

\item{site_id}{A character or integer site identifier. Defaults to
\code{NA_character_}.}

\item{year}{A single integer year.
Defaults to \code{NA_integer_}.}

\item{indicator_order}{Character row and column order for \code{matrix};
defaults to \code{colnames(matrix)}.}

\item{family}{Character scalar. Estimation family that produced
the matrix. One of \code{"binomial"}, \code{"multivariate"}, or
\code{"multinomial"}. Required.}

\item{vcov_method}{The calculation method; see \emph{Covariance methods}.
Character scalar; defaults to \code{NA_character_}, which is valid only
for \code{family = "binomial"}. Multivariate and multinomial callers
must supply a compatible method.}

\item{estimate_scale}{Character scalar. The row-level
\code{estimate_scale} of the corresponding estimates.
One of \code{"none"}, \code{"arcsine"}, \code{"arcsine_anscombe"}, or
\code{"logit"}; required.}

\item{vcov_scale}{Character scalar. The scale of \code{matrix}
entries; one of \code{"raw"}, \code{"arcsine_delta"},
\code{"logit_delta"}, or \code{"reference_raw"}. Required.
\strong{May differ from estimate_scale} under Scenarios B and C;
see \emph{Covariance scale} below.}

\item{matrix_boundary_rule}{A single string recording
how boundary cells were handled during matrix construction.
Defaults to \code{"none"}.}

\item{scalar_correction_rule}{Character vector of length
\code{length(indicator_order)}. Per-indicator scalar SE correction rules;
supported values are listed under \emph{Scalar correction rules}. Defaults to
\code{rep("none", length(indicator_order))}.}

\item{psd_repair}{A single string recording any positive-semidefinite
repair applied during matrix construction. Defaults to
\code{"none"}.}

\item{matrix_rank}{Integer scalar or \code{NULL} (default
\code{NULL}). For Scenario C this is the analytic
simplex rank \code{positive_support - 1}, including when a census makes
the realized sampling covariance exactly zero. For other families it is
the numerical matrix rank. When \code{NULL}, the constructor computes the
family-appropriate value.}

\item{positive_support}{Integer scalar. Number of positive
categories for Scenario C multinomial output and the basis of its
analytic rank; \code{NA_integer_} otherwise. Defaults to
\code{NA_integer_}.}

\item{n_jt}{A single integer denominator. Defaults to
\code{NA_integer_}.}

\item{n_eff}{A numeric denominator used for transformed standard errors.
Package estimates record \code{n + 1/2} for Anscombe or \code{n}
otherwise, unchanged by FPC. This differs from the \code{n + 3/4}
denominator in the Anscombe point transformation. Defaults to
\code{NA_real_} for a directly constructed object.}

\item{population_size}{Numeric scalar. Fixed site-year population size
under SRSWOR, or \code{NA_real_} when no finite-population design was
supplied.}

\item{sampling_fraction}{Sampling fraction \code{n / population_size};
numeric scalar or aligned vector. Missing without an FPC design.}

\item{fpc_variance_multiplier}{Numeric scalar or coordinate-aligned vector.
Conventional SRSWOR variance multiplier
\code{(N - n) / (N - 1)}, with census value zero.}

\item{fpc_se_multiplier}{Numeric scalar or coordinate-aligned vector. Square
root of the conventional FPC variance multiplier documented above.}

\item{variance_multiplier_applied}{Numeric scalar or coordinate-aligned
vector recording the multiplier actually applied: the conventional FPC
for plug-in variance or \code{(N - n) / N} for the design-corrected rule.}

\item{se_multiplier_applied}{Numeric scalar or coordinate-aligned vector.
Square root of the applied variance multiplier documented above.}

\item{sampling_design}{Character scalar, one of \code{"not_specified"} or
\code{"SRSWOR"}.}

\item{variance_rule}{Character scalar or aligned vector:
\code{"plugin"} or \code{"design_corrected"}.}

\item{diag_contract}{A single string identifying which scalar variance
the covariance diagonal matches. See \emph{Diagonal and standard errors}.}
}
\value{
An S3 object of class \code{sm_vcov} with fields
\code{matrix}, \code{site_id}, \code{year},
\code{indicator_order}, \code{family}, \code{vcov_method},
\code{estimate_scale}, \code{vcov_scale},
\code{matrix_boundary_rule}, \code{scalar_correction_rule},
\code{psd_repair}, \code{matrix_rank}, \code{positive_support},
\code{n_jt}, \code{n_eff}, the finite-population fields
(\code{population_size}, \code{sampling_fraction},
\code{fpc_variance_multiplier}, \code{fpc_se_multiplier},
\code{variance_multiplier_applied}, \code{se_multiplier_applied},
\code{sampling_design}, \code{variance_rule}), and \code{diag_contract}.
}
\description{
\code{sm_vcov()} creates a covariance matrix object with its site, year,
indicator order, and calculation method. Estimation functions return
these objects in the \code{V} list-column when \code{vjt = TRUE}.
Use \code{as.matrix()} to extract a matrix, and inspect
\code{vcov_scale} before combining it with estimates or standard errors.
}
\details{
An \code{sm_vcov} object is a list containing a matrix and fields
that describe its calculation. The constructor checks dimensions,
symmetry, positive semidefiniteness, and the supplied field values.
\code{format()}, \code{print()}, and \code{as.matrix()} also check
the object, so inconsistent changes to a matrix or its fields can
cause an error when it is used.

Symmetry, positive semidefiniteness, the multinomial simplex identity,
and numerical rank use separate tolerances. Each accounts for the
matrix's scale and has a small absolute floor. The numerical-rank
tolerance is stricter than the tolerance for positive semidefiniteness.
For multinomial matrices, \code{matrix_rank} instead records the
analytic support rank described below.

For overlapping binary indicators, see
\code{vignette("m3-multivariate-sur-covariance")}; for mutually
exclusive categories, see \code{vignette("m4-multinomial-simplex")}.
The working-independence assumption for published marginals is
explained in \code{vignette("m5-aggregate-engines")}.
}
\section{Scalar correction rules}{

\itemize{
\item \code{"none"}: no scalar correction.
\item \code{"binomial_bc"}: the binomial n-1 correction.
\item \code{"wilson_boundary_surrogate"}: Wilson boundary uncertainty.
\item \code{"agresti_coull_boundary_surrogate"}: Agresti--Coull boundary
uncertainty.
}
}

\section{Covariance scale}{

\code{vcov_scale} gives the scale of the stored matrix. In
multivariate student-level and multinomial estimates (Scenarios B and C),
the matrix stays on the raw-proportion scale even when \code{theta_hat}
uses arcsine or logit. Check this field before using \code{matrix} in a
model or combining it with the reported standard errors.
}

\section{Multivariate student-level covariance (Scenario B)}{

Let \eqn{Q = \sum_i (y_i - \bar y)(y_i - \bar y)^\top} and
\eqn{q = (N-n)/(N-1)}. Scenario B stores the raw-scale plug-in matrix
\eqn{Q/n^2}; with a fixed SRSWOR population it stores \eqn{qQ/n^2}.
With \code{bias_correction = "binomial_bc"}, the matrix becomes
\eqn{Q/[n(n-1)]}, or \eqn{(N-n)Q/[Nn(n-1)]} under SRSWOR.
Multiplying the matrix by a positive scalar preserves correlations
where they are defined, symmetry, and positive semidefiniteness. A census
gives a zero covariance matrix, whose correlations are undefined.
A constant indicator has zero off-diagonals. Wilson boundary handling is
a positive scalar and diagonal substitute; Agresti--Coull boundary
handling is unavailable when requesting this matrix. Package estimates
are checked for agreement between the matrix and its rows, including
the method, scale, diagonal, denominators, and boundary corrections.
}

\section{Multinomial covariance (Scenario C)}{

For \eqn{M = \mathrm{diag}(\hat\pi)-\hat\pi\hat\pi^\top}, Scenario C
stores \eqn{M/n} without FPC and \eqn{qM/n} under SRSWOR. With
\code{bias_correction = "binomial_bc"}, the whole matrix is
\eqn{M/(n-1)} without FPC and
\eqn{(N-n)M/[N(n-1)]} under SRSWOR. These rules preserve PSD and
\eqn{V\mathbf 1=0}. If \eqn{S} categories have positive observed count,
\code{matrix_rank} records the analytic simplex support rank \eqn{S-1};
this remains the recorded rank for a census even though its realized
sampling covariance is the zero matrix. The matrix-level
\code{variance_rule} and applied multiplier are uniform over all
coordinates. A zero-support coordinate keeps an exact zero matrix row and
column. The scalar calculation for that category can therefore differ
from the matrix's design-corrected rule. In particular, a positive Wilson
boundary standard error does not replace the zero matrix diagonal.
}

\section{Diagonal and standard errors}{

\code{diag_contract = "row_se_squared"} means the matrix is on the row
estimate scale and its diagonal equals \code{se^2}; this is used by A, D0,
and D1. \code{"row_se_raw_squared"} means a raw-scale B/C matrix agrees
with \code{se_raw^2}. For multinomial boundary surrogates,
the value formed by concatenating \code{"row_se_raw_squared_"} and
\code{"except_boundary_surrogates"} records that the
simplex-preserving matrix deliberately keeps a zero boundary diagonal while
the scalar Wilson surrogate remains positive. \code{"not_checked"} is
reserved for user-constructed objects without a row companion.
}

\section{Covariance methods}{

\describe{
\item{\code{NA_character_}}{No recorded method; valid only for
\code{family = "binomial"}.}
\item{\code{"sur"}}{Scenario B; multivariate SUR-style
covariance with off-diagonal \eqn{\sigma_{kk'}}{sigma_kk'}
from joint proportions.}
\item{\code{"multinomial"}}{Scenario C; simplex covariance
\eqn{(\mathrm{diag}(\pi) - \pi \pi^\top) / n}{(diag(pi) - pi pi') / n}.}
\item{\code{"working_independence"}}{Scenario D1;
working-independence diagonal covariance when cross-marginal
joints are unidentified.}
}
}

\section{Standard-error methods}{

The \code{var_method} column in the estimates records how each
standard error was calculated. It is separate from the matrix's
\code{vcov_method}. Its base values are:
\itemize{
\item Arcsine values: \code{"arcsine_vst"} and
\code{"arcsine_anscombe"}.
\item Bias-corrected arcsine: \code{"arcsine_delta_binomial_bc"}.
\item Logit values: \code{"logit_delta"} and
\code{"logit_delta_binomial_bc"}.
\item Raw-scale values: \code{"binomial"} and \code{"binomial_bc"}.
\item Boundary values: \code{"wilson_boundary_surrogate"} and
\code{"agresti_coull_boundary_surrogate"}.
\item Suppression values: \code{"suppressed_drop"} and
\code{"suppression_sensitivity"}.
}
Experimental GVF/log-variance smoothing records
\code{" + gvf_smooth_loglinear"} or
\code{" + gvf_smooth_gam"} in the alternative
\code{var_method_smoothed} column; an allowed overwrite may copy
that method label to \code{var_method}. Legacy
\code{" + fh_smooth_*"} labels remain readable. Derivations are documented
in:
\itemize{
\item \code{vignette("m2-scalar-se-binomial")} for scalar binomial SEs.
\item \code{vignette("m6-variance-smoothing-theory")} for smoothing.
}
}

\examples{
\dontshow{set.seed(1L)}
data(prek_sim, package = "sitemix")
est <- sm_estimate(
  subset(prek_sim, year == 2024),
  family     = "multivariate",
  indicators = c("frpm", "snap"),
  vjt        = TRUE
)
v1 <- est$V[[1L]]
class(v1)
v1$vcov_scale
v1$vcov_method
dim(v1$matrix)
as.matrix(v1)

}
\references{
Zellner, A. (1962). An efficient method of estimating seemingly
unrelated regressions and tests for aggregation bias.
\emph{Journal of the American Statistical Association},
\bold{57}(298), 348--368.
\doi{10.1080/01621459.1962.10480664}
}
\seealso{
\itemize{
\item \code{\link[=sm_estimate]{sm_estimate()}} for the estimator
that produces \code{V} list-columns of \code{sm_vcov} objects.
\item \code{\link[=sm_frechet_envelope]{sm_frechet_envelope()}} for
pairwise intervals and dependence stress scenarios for D1 aggregates.
\item \code{\link[=sm_diagnose]{sm_diagnose()}} for the
\code{level = "vcov"} diagnostic.
\item \code{vignette("m3-multivariate-sur-covariance")}.
\item \code{vignette("m4-multinomial-simplex")}.
\item \code{vignette("m7-frechet-envelope-theory")}.
}

Other covariance:
\code{\link[=sm_frechet_envelope]{sm_frechet_envelope()}}
}
\concept{covariance}
