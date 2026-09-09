% Please edit documentation in R/pivot-subgroups.R
\encoding{UTF-8}
\name{sm_pivot_subgroups_to_sites}
\alias{sm_pivot_subgroups_to_sites}
\title{Prepare published counts for subgroup rates or compositions}
\usage{
sm_pivot_subgroups_to_sites(
  data,
  site_col = "site_id",
  year_col = "year",
  subgroup_col,
  numerator_col,
  denominator_col,
  indicator = "subgroup_rate",
  separator = "_",
  level_override = NULL,
  rtype_col = NULL,
  partition_target = c("none", "denominator_composition", "case_composition"),
  partition_tolerance = 0.5,
  suppression_col = NULL,
  suppression_flag_value = ""
)
}
\arguments{
\item{data}{A data frame or tibble containing subgroup aggregate
rows, one row per \code{(site, year, subgroup)} triple.}

\item{site_col}{A single column name containing source
site identifiers. Defaults to \code{"site_id"}.}

\item{year_col}{A single column name containing
integer-like years. Defaults to \code{"year"}.}

\item{subgroup_col}{A single column name containing
subgroup labels. Required.}

\item{numerator_col}{A single column name containing
aggregate numerator counts. Required.}

\item{denominator_col}{A single column name containing
aggregate denominator counts. Required.}

\item{indicator}{A single string giving the indicator label to
place in the output \code{indicator} column. Defaults to
\code{"subgroup_rate"}.}

\item{separator}{A single non-empty string used to join the source
site and subgroup labels. Defaults to \code{"_"}.}

\item{level_override}{Must be \code{NULL} (default). Other values raise
\code{sitemix_error_invalid_level_override}.}

\item{rtype_col}{Must be \code{NULL} (default). Other values raise
\code{sitemix_error_invalid_level_override}; split the source by
reporting level before using this function.}

\item{partition_target}{A single string selecting the counts to
prepare: \code{"none"} (default; D0 conditional-rate
rows), \code{"denominator_composition"}, or
\code{"case_composition"} (both return Scenario C count input).}

\item{partition_tolerance}{A single finite, non-negative number. Absolute
tolerance for composition partition checks against the
required \code{ALL} row. Defaults to \code{0.5}.}

\item{suppression_col}{A single column name, or \code{NULL}
(default). Names the publisher suppression flag column. With
\code{NULL}, an existing \code{suppression_flag} column is used
if present. If neither source is available, source rows receive
\code{suppression_flag = FALSE}.}

\item{suppression_flag_value}{Value or vector of values marking
publisher suppression in the flag column. Defaults to
\code{""}.}
}
\value{
With \code{partition_target = "none"}, a tibble with one
row per site-year-subgroup for \code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}}.
It contains:
\describe{
\item{\code{site_id}}{Composite identifier constructed by
joining the source site and subgroup labels with
\code{separator}.}
\item{\code{year}}{The source year, stored as an integer.}
\item{\code{indicator}}{Character scalar (the \code{indicator}
argument).}
\item{\code{c_jt}, \code{n_jt}}{Numerator and denominator
copied from \code{numerator_col} and \code{denominator_col}.}
\item{\code{suppression_flag}}{Always-present logical. It is
\code{TRUE} for rows flagged by the publisher and otherwise
\code{FALSE}. Flags come from \code{suppression_col}, or from an
existing \code{suppression_flag} column when that argument is
\code{NULL}. If neither source flag column is available, all rows
receive \code{FALSE}.}
\item{\code{framing}}{Character scalar; the framing label
(\code{"subgroup_as_site"}).}
\item{\code{source_site_id}, \code{source_subgroup}}{The
original site and publisher subgroup labels, preserved for
reference. A recognized total label becomes \code{"ALL"} only in
the combined \code{site_id}; \code{source_subgroup} keeps its source
spelling.}
}

With a composition target, a tibble with one row per original
site-year, containing \code{site_id}, \code{year}, a common
\code{n_jt}, and one \verb{c_jt_<category>} column per non-total
subgroup. The common \code{n_jt} is the sum of these category counts;
the \code{ALL} row supplies the comparison total and is not a category.
The \code{partition_categories} and \code{indicator_order}
attributes record category order. Use this order as \code{indicators}
in \code{\link[=sm_estimate_from_counts]{sm_estimate_from_counts()}} with \code{family = "multinomial"}.
}
\description{
\code{sm_pivot_subgroups_to_sites()} reshapes published subgroup counts
for estimation. By default, each site-subgroup pair receives a
combined \code{site_id}, keeping its own numerator and denominator.
Pass this table to \code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}} to estimate a rate
within each subgroup. The documentation calls this \strong{Framing X}.

A composition \code{partition_target} instead prepares category counts
with a common denominator for \code{\link[=sm_estimate_from_counts]{sm_estimate_from_counts()}} with
\code{family = "multinomial"} (Scenario C). These counts describe
each subgroup's share of a site total, rather than its within-subgroup
rate. The function reshapes counts; it does not estimate rates or
standard errors.
}
\details{
\strong{Rates and compositions.} Choose \code{partition_target}
according to the quantity you want to estimate:
\describe{
\item{\code{"none"} (default)}{Retains each subgroup's numerator
and denominator for a conditional rate: cases within that subgroup
divided by its own denominator. The combined \code{site_id}
joins the source site and subgroup label with \code{separator}.
This is binomial aggregate input (D0).}
\item{\code{"denominator_composition"}}{Uses subgroup denominator
counts as category counts. The common denominator is their sum,
checked against the \code{ALL} denominator. The resulting
proportions describe each subgroup's share of the site's
denominator total.}
\item{\code{"case_composition"}}{Uses subgroup numerator counts
as category counts. The common denominator is their sum, checked
against the \code{ALL} numerator. The resulting proportions
describe how the site's cases are distributed across subgroups.}
}
For a composition, the subgroups must represent mutually exclusive
categories covering the population or cases being counted. Both targets
require at least two categories, the same complete category set in every
site-year, and exactly one total row. A missing category row is an error;
supply an explicit zero for a category with no observations. Category
counts must sum to the relevant total within \code{partition_tolerance},
and that total must be positive. These count checks do not establish
whether the source categories overlap.

Denominator composition requires observed denominators for every category
and the total, but permits suppressed numerators. For case composition,
every category and the total must have an observed numerator that is not
suppressed.

To keep subgroups as marginal indicators of the original site instead
of assigning combined site identifiers, use
\code{\link[=sm_pivot_subgroups_to_indicators]{sm_pivot_subgroups_to_indicators()}} (Framing Y). That alternative
retains each subgroup's own numerator and denominator.

\strong{Total labels.} The function recognizes the publisher labels
\code{"ALL"}, \code{"ALL STUDENT"}, \code{"ALL STUDENTS"},
\code{"TOTAL"}, and \code{"OVERALL"} as \code{"ALL"}.
Matching ignores case, surrounding whitespace, and punctuation between
words. Two recognized total labels in the same site-year become
duplicate \code{"ALL"} rows and raise an error. With the default
target, the combined \code{site_id} uses \code{"ALL"}, while
\code{source_subgroup} preserves the publisher's spelling.

\strong{Reporting levels.} Split files that mix school, district, or
state rows into separate tables for each reporting level before calling
either subgroup helper. The functions do not separate these levels.
Supplying \code{level_override} or \code{rtype_col} raises
\code{sitemix_error_invalid_level_override}; both must remain \code{NULL}.
}
\examples{
\dontshow{set.seed(1L)}
# Synthetic publisher file with school-by-subgroup rows
subgroups <- expand.grid(
  site_id  = paste0("S", sprintf("\%03d", 1:5)),
  year     = 2024L,
  subgroup = c("frpm_yes", "frpm_no"),
  stringsAsFactors = FALSE
)
subgroups$c_jt <- c(8, 4, 7, 5, 9, 3, 10, 6, 5, 8)
subgroups$n_jt <- c(12, 6, 11, 9, 13, 7, 15, 10, 8, 12)

pivoted <- sm_pivot_subgroups_to_sites(
  subgroups,
  subgroup_col    = "subgroup",
  numerator_col   = "c_jt",
  denominator_col = "n_jt",
  indicator       = "frpm_take_up"
)
head(pivoted)

}
\seealso{
\itemize{
\item \link[=sm_pivot_subgroups_to_indicators]{Framing Y helper}.
\item \link[=sm_estimate_from_aggregates]{Aggregate estimation} for the
default output.
\item \link[=sm_estimate_from_counts]{Estimation from counts} for composition
targets.
\item \link[=sm_suppression_report]{Suppression audit} before pivoting.
\item \code{vignette("a5-published-aggregates")} for the walkthrough.
}

Other reshape:
\code{\link[=sm_pivot_subgroups_to_indicators]{sm_pivot_subgroups_to_indicators()}}
}
\concept{reshape}
