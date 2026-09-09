% Please edit documentation in R/sm_estimate_from_counts.R
\encoding{UTF-8}
\name{sm_estimate_from_counts}
\alias{sm_estimate_from_counts}
\title{Estimate site-year proportions from sufficient counts}
\usage{
sm_estimate_from_counts(
  data,
  family,
  indicator = NULL,
  indicators = NULL,
  id_cols = c("site_id", "year"),
  accountability_n = 30L,
  ...
)
}
\arguments{
\item{data}{A data frame or tibble containing site and year columns
named by \code{id_cols}. Student rows need the column named by
\code{indicator}, or the binary columns named by \code{indicators}.
Counts input needs \code{n_jt} and family-specific \verb{c_jt_*}
columns. Published aggregates may use the standard aggregate column
names or the column mappings below; see \code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}}.}

\item{family}{A single string: \code{"binomial"},
\code{"multivariate"}, or \code{"multinomial"}. Required;
omitting it raises \code{sitemix_error_invalid_family}.
Published aggregate input supports only \code{"binomial"} and
\code{"multivariate"}.}

\item{indicator}{A single column name, or \code{NULL} (default).
Required for single-indicator student/count input (A) and categorical
student input (C). Binary columns must be logical or numeric 0/1;
categorical columns must be factors or character vectors. For
long-form aggregate input, this argument supplies or replaces the
indicator label; \code{NULL} keeps the labels already in the data.}

\item{indicators}{A character vector, or \code{NULL} (default).
Names of overlapping binary indicators for B. For multinomial counts
(C), gives the category order of the \verb{c_jt_*} columns. For
wide-form D1 aggregates, gives the marginal indicator names.}

\item{id_cols}{A character vector of length two, giving the site and
year column names in that order. Defaults to
\code{c("site_id", "year")}.}

\item{accountability_n}{A positive whole number. Rows with
\code{n < accountability_n} receive
\code{flag_below_accountability = TRUE}. Defaults to \code{30L}.}

\item{...}{Additional arguments forwarded to \code{\link[=sm_estimate]{sm_estimate()}}.}
}
\value{
A \code{sitemix_estimates} tibble with one row per
site-year-indicator. It has the same columns and scale information as
\code{\link[=sm_estimate]{sm_estimate()}}; see that function's \emph{Value} section for the
estimates, standard errors, optional covariance, and object attributes.
}
\description{
\code{sm_estimate_from_counts()} estimates proportions and standard errors
from one row of counts per site and year. It calls \code{\link[=sm_estimate]{sm_estimate()}} with
\code{from_counts = TRUE}, so you do not need to set the input type.
You can also use \code{sm_estimate(..., from_counts = TRUE)} directly.
}
\details{
Each row needs the site and year columns named by \code{id_cols}, an
\code{n_jt} denominator, and count columns for the chosen \code{family}:
\describe{
\item{\code{"binomial"} (A)}{One marginal count named
\verb{c_jt_<indicator>}, with its label supplied in \code{indicator}.}
\item{\code{"multivariate"} (B)}{At least two marginal counts named
\verb{c_jt_<indicator>}, plus a co-occurrence count for every pair.
Pair names follow the order in \code{indicators}: for example,
\code{indicators = c("frpm", "snap")} requires
\code{c_jt_frpm}, \code{c_jt_snap}, and \code{c_jt_frpm_snap}.}
\item{\code{"multinomial"} (C)}{At least two category counts in
\verb{c_jt_*} columns, summing to \code{n_jt} in each row.
Use \code{indicators} to set the category order explicitly.}
}

For overlapping indicators, \code{vjt = TRUE} requests covariance and
checks whether the counts can come from a common sample. This check
supports two or three indicators. Counts with four or more indicators
are accepted when \code{vjt = FALSE}; requesting covariance for them
raises \code{sitemix_error_input_indicator_count}. Use student rows
with \code{\link[=sm_estimate]{sm_estimate()}} if covariance is needed for four or more indicators.

Counts constructed from the same retained student rows, including the
same missing-value exclusions, give the same estimates and standard
errors up to numerical rounding when the family, indicator order, and
estimation options match. Covariance values also agree for supported
requests. The \code{input_mode} column distinguishes the two input types.

Do not pass \code{from_counts} to this function: it is set internally,
and supplying it raises \code{sitemix_error_invalid_from_counts}.
Use \code{\link[=sm_estimate]{sm_estimate()}} to set the input type yourself, or
\code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}} for published aggregate rows.
}
\examples{
\dontshow{set.seed(1L)}
counts_path <- system.file(
  "extdata", "prek_sim_counts.rds",
  package = "sitemix", mustWork = TRUE
)
counts <- readRDS(counts_path)

# Estimate SNAP proportions for 2024 from sufficient counts:
snap_counts <- counts[
  counts$year == 2024,
  c("site_id", "year", "n_jt", "c_jt_snap")
]
est <- sm_estimate_from_counts(
  snap_counts,
  family    = "binomial",
  indicator = "snap"
)
head(est, 5)
unique(est$estimate_scale)

}
\seealso{
\itemize{
\item \code{\link[=sm_estimate]{sm_estimate()}} for estimation options
and returned columns.
\item \code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}}
for published aggregate rows.
\item \code{\link[=sm_diagnose]{sm_diagnose()}} for checking estimates
and their uncertainty.
\item \code{vignette("a2-input-formats")} for choosing an input format.
\item \code{vignette("a3-scenario-binomial")} for a binomial example
using counts.
\item \code{vignette("m2-scalar-se-binomial")} for why sufficient
counts reproduce the binomial calculations from student rows.
}

Other estimation:
\code{\link[=sm_estimate]{sm_estimate()}},
\code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}}
}
\concept{estimation}
