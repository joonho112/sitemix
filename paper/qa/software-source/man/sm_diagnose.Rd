% Please edit documentation in R/sm_diagnose.R
\encoding{UTF-8}
\name{sm_diagnose}
\alias{sm_diagnose}
\title{Diagnose uncertainty in a sitemix_estimates tibble}
\usage{
sm_diagnose(x, level = c("summary", "row", "vcov"), verbose = TRUE)
}
\arguments{
\item{x}{A \code{sitemix_estimates} tibble produced by
\code{\link[=sm_estimate]{sm_estimate()}} or one of its wrappers.}

\item{level}{A single string choosing the result: one of
\code{"summary"} (default), \code{"row"}, or \code{"vcov"}. See
\emph{Details} for the rows and class returned at each level.}

\item{verbose}{A single logical value. If \code{TRUE} (default),
print a short summary as well as returning the tibble.}
}
\value{
A tibble with the class and row unit described in \emph{Details}.
Summary output has one row and includes one integer
\code{n_var_method_<label>} column for each observed \code{var_method}.
The method-count columns therefore depend on the input object.
Row output has one row per estimate; vcov output has one row per
site-year covariance matrix.
}
\description{
\code{sm_diagnose()} checks estimates, standard errors, and covariance
matrices in a \code{sitemix_estimates} tibble. Use \code{level} to
return a summary of the object (\code{"summary"}), diagnostics for
each row (\code{"row"}), or checks on the covariance matrices
(\code{"vcov"}).
}
\details{
The result is a tibble whose rows and class depend on \code{level}:

\describe{
\item{\code{level = "summary"}}{Returns a
\code{sitemix_diagnostics_summary} tibble: one row
summarizing denominators, reporting thresholds, standard errors,
and covariance matrices. Use this level for an initial check.}
\item{\code{level = "row"}}{Returns a
\code{sitemix_diagnostics_row} tibble: one row per
site-year-indicator, with the flags described below and a
severity level. Use it to find rows that need closer inspection.}
\item{\code{level = "vcov"}}{Returns a
\code{sitemix_diagnostics_vcov} tibble: one row per
site-year covariance matrix stored in the \code{V} list-column.
It reports the smallest
eigenvalue, the estimate and covariance scales, and whether the
matrix still matches any smoothed standard errors. This level is
useful when \code{x} was produced with \code{vjt = TRUE}.}
}

Row diagnostics include these flags:
\itemize{
\item \code{flag_small_n} and \code{flag_below_accountability}.
\item \code{flag_zero_cell} and \code{flag_suppressed}.
}

Summary diagnostics check whether scalar uncertainty is finite,
standard errors are positive, scales agree within each indicator,
and covariance matrices are valid and on compatible scales. These
checks describe the results; they do not establish that a particular
subsequent model is appropriate.

The severity is \code{"error"}, \code{"warning"}, \code{"note"},
or \code{"ok"}, in that order of priority. \code{"error"} indicates
invalid scalar uncertainty, mixed scales within an indicator,
inconsistent smoothing information, or a covariance matrix that no
longer matches standard errors on the same scale.
\code{"warning"} records unavailable suppression rows, non-identified
variance sensitivity, or an explicit estimate/covariance scale mismatch.
\code{"note"} records descriptive facts such as small denominators,
boundary cells, and accountability thresholds. \code{"ok"} means none of
these conditions applies.
A diagnostic reports these facts after ordinary object validation; it does
not replace \code{validate.sitemix_estimates()}.

The summary includes:
\describe{
\item{Scalar uncertainty}{\code{scalar_uncertainty_finite},
\code{scalar_se_positive}, and
\code{indicator_scale_consistent}. An exact zero-uncertainty SRSWOR
census is reported separately and is a note, not an error.}
\item{Covariance}{\code{v_present}, \code{v_valid}, and
\code{estimate_vcov_scale_compatible}.}
\item{Suppression}{Counts for identified, suppressed-missing, and
sensitivity rows, whether numeric sensitivity variances are available,
and whether that analysis was acknowledged.}
\item{Smoothing}{\code{smoothing_present}, consistency of the stored settings,
\code{smoothing_v_relation}, and \code{v_stale}.}
\item{Classification}{\code{diag_severity} and semicolon-delimited
\code{diag_notes}.}
}
The field describing the role of suppressed-row sensitivity results is
\code{suppression_sensitivity_role}.
}
\examples{
\dontshow{set.seed(1L)}
data(prek_sim, package = "sitemix")
est <- sm_estimate(
  subset(prek_sim, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)

# Start with a summary of all estimates:
diag_s <- sm_diagnose(est, verbose = FALSE)
class(diag_s)

# Inspect the diagnostics for individual estimates:
diag_r <- sm_diagnose(est, level = "row", verbose = FALSE)
head(diag_r, 5)

}
\seealso{
\itemize{
\item \code{\link[=sm_estimate]{sm_estimate()}} for producing estimates
and descriptions of the estimate columns.
\item \code{\link[=sm_suppression_report]{sm_suppression_report()}} for
examining suppression in published input data.
\item \code{vignette("a6-diagnostics-and-suppression")} for the applied
walkthrough of all three levels.
}

Other audit:
\code{\link[=sm_suppression_report]{sm_suppression_report()}}
}
\concept{audit}
