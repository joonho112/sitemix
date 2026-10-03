% Please edit documentation in R/pivot-subgroups.R
\encoding{UTF-8}
\name{sm_pivot_subgroups_to_indicators}
\alias{sm_pivot_subgroups_to_indicators}
\title{Prepare subgroup proportions as indicators within each site}
\usage{
sm_pivot_subgroups_to_indicators(
  data,
  site_col = "site_id",
  year_col = "year",
  subgroup_col,
  numerator_col,
  denominator_col,
  indicator_set = NULL,
  na_action = c("drop_row", "keep_na"),
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

\item{indicator_set}{A character vector of at least two distinct
subgroup labels to retain in the specified order, or \code{NULL}
(default). With \code{NULL}, labels follow their first appearance in
\code{subgroup_col}. Supplied labels must occur in the data.
Recognized total labels become \code{"ALL"}; labels that then
coincide are rejected as duplicate indicators.}

\item{na_action}{A single string: \code{"drop_row"} (default) or
\code{"keep_na"}. The default removes the entire site-year group
if any selected subgroup has a missing numerator, missing denominator,
or publisher suppression flag. \code{"keep_na"} retains these
groups and inserts \code{NA} rows for absent subgroup indicators.}

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
A tibble with one row per retained site-year-subgroup,
ordered by site, year, and \code{indicator_set}. It is structured
for \code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}} with
\code{family = "multivariate"} and \code{aggregate_case = "D1"}.
It contains:
\describe{
\item{\code{site_id}}{Original source site identifier.}
\item{\code{year}}{Integer year.}
\item{\code{indicator}}{Character scalar; the subgroup label
(each subgroup becomes a marginal indicator).}
\item{\code{c_jt}, \code{n_jt}}{Numerator and denominator
from \code{numerator_col} and \code{denominator_col}.}
\item{\code{suppression_flag}}{Always-present logical. It is
\code{TRUE} for publisher-flagged rows and otherwise \code{FALSE}.
Flags come from \code{suppression_col}, or from an existing
\code{suppression_flag} column when that argument is \code{NULL}.
Without either source flag column, observed rows are \code{FALSE};
missing subgroup rows inserted by \code{na_action = "keep_na"} are
\code{TRUE}.}
\item{\code{framing}}{Character scalar; the framing label
(\code{"subgroup_as_indicator"}).}
\item{\code{source_subgroup}}{Original publisher subgroup label for
observed rows; inserted missing-subgroup rows carry
\code{NA_character_}.}
}

The \code{indicator_set} attribute records the selected subgroup
order. The \code{framing} and \code{na_action} attributes record
how the table was prepared. An error is raised if no site-year groups
remain or fewer than two indicators are retained per group.
}
\description{
\code{sm_pivot_subgroups_to_indicators()} prepares published subgroup
counts for comparing subgroup proportions within each site. It keeps the
original \code{site_id} and uses each subgroup label as an
\code{indicator}, preserving that subgroup's numerator and denominator.
The documentation calls this \strong{Framing Y}.

Pass the result to \code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}} with
\code{family = "multivariate"} for marginal aggregate estimates
(D1). This helper reshapes the counts; it does not estimate proportions,
standard errors, or dependence between subgroups.
}
\details{
\strong{Analysis unit and denominator.} Each returned site-year has
several subgroup indicators. Their proportions are conditional on membership
in each subgroup, using that subgroup's own denominator. Equal counts or
denominators do not show that the same observational units contributed to
different indicators. For example, two disjoint subgroups can have the
same number of students. Subgroups can also overlap; labels and marginal
counts alone do not identify the joint counts.

When estimating D1 proportions, set \code{sampling_relation} in
\code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}} from the source information:
\code{"same_units"} only when the marginals describe the same
observed units, \code{"different_units"} when they differ, or
\code{"unknown"} when this is not known. The pivot does not determine
this relationship. If covariance is requested, D1 uses working
independence; its off-diagonal zeros are an assumption. See
\code{\link[=sm_frechet_envelope]{sm_frechet_envelope()}} for the conditions on raw pairwise intervals
and projected stress scenarios.

Use \code{\link[=sm_pivot_subgroups_to_sites]{sm_pivot_subgroups_to_sites()}} with its default
\code{partition_target = "none"} when each site-subgroup pair should
instead have its own site identifier (Framing X). Both defaults preserve
subgroup-specific numerators and denominators. If the question concerns
subgroup shares of a common site total, use that function's explicit
composition targets; merely representing subgroups as indicators does
not create multinomial composition counts.

\strong{Subgroup selection and missing rows.} \code{indicator_set}
selects and orders subgroup labels. Each retained site-year must have
at least two indicators and the same selected indicator set. With
\code{na_action = "drop_row"}, any missing count or publisher-suppressed
indicator removes the entire site-year group. With \code{"keep_na"},
the function retains those groups and inserts missing subgroup rows with
\code{NA} counts and \code{suppression_flag = TRUE}. These are missing
values, not zero counts. Review the suppression and hidden-denominator
settings in \code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}} before estimation.

Recognized total labels, listed in \code{\link[=sm_pivot_subgroups_to_sites]{sm_pivot_subgroups_to_sites()}},
become \code{"ALL"} before duplicate and indicator-set checks.
\code{source_subgroup} retains the publisher's spelling for observed
rows. Split files containing school, district, or state rows by reporting
level before using either helper; these functions do not separate levels.
}
\examples{
\dontshow{set.seed(1L)}
# Synthetic publisher file (same as Framing X example)
subgroups <- expand.grid(
  site_id  = paste0("S", sprintf("\%03d", 1:5)),
  year     = 2024L,
  subgroup = c("frpm_yes", "frpm_no"),
  stringsAsFactors = FALSE
)
subgroups$c_jt <- c(8, 4, 7, 5, 9, 3, 10, 6, 5, 8)
subgroups$n_jt <- c(12, 6, 11, 9, 13, 7, 15, 10, 8, 12)

pivoted_y <- sm_pivot_subgroups_to_indicators(
  subgroups,
  subgroup_col    = "subgroup",
  numerator_col   = "c_jt",
  denominator_col = "n_jt"
)
head(pivoted_y)

}
\seealso{
\itemize{
\item \link[=sm_pivot_subgroups_to_sites]{Subgroup-as-site and composition counts}
for the alternative analysis units and denominators.
\item \link[=sm_estimate_from_aggregates]{Aggregate estimation} for the D1
estimator.
\item \link[=sm_frechet_envelope]{Fréchet diagnostic} for pairwise
intervals and projected stress.
\item \link[=sm_suppression_report]{Suppression audit} before pivoting.
\item \code{vignette("a5-published-aggregates")} for the walkthrough.
}

Other reshape:
\code{\link[=sm_pivot_subgroups_to_sites]{sm_pivot_subgroups_to_sites()}}
}
\concept{reshape}
