% Please edit documentation in R/suppression.R
\encoding{UTF-8}
\name{sm_suppression_report}
\alias{sm_suppression_report}
\title{Summarize suppression and reporting thresholds in aggregate data}
\usage{
sm_suppression_report(
  x,
  by = c("subgroup", "year"),
  id_cols = c("site_id", "year"),
  numerator_col = NULL,
  denominator_col = NULL,
  indicator_col = NULL,
  subgroup_col = NULL,
  suppression_col = NULL,
  suppression_flag_value = "",
  suppression_when = NULL,
  min_n = 10L,
  accountability_n = 30L
)
}
\arguments{
\item{x}{A data frame or tibble of aggregate input data. Required
columns depend on the input format; see
\code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}}.}

\item{by}{A character vector, or \code{NULL} for one overall row.
Columns to group the report by after
aggregate normalization. Defaults to
\code{c("subgroup", "year")}. Each group gets its own row in
the returned tibble.}

\item{id_cols}{Character vector of length two. Site and year
column names. Defaults to \code{c("site_id", "year")}.}

\item{numerator_col}{Character scalar or \code{NULL} (default
\code{NULL}). Source numerator column name. Required when the
publisher uses other column names.}

\item{denominator_col}{Character scalar or \code{NULL} (default
\code{NULL}). Source denominator column name. Required when the
publisher uses other column names.}

\item{indicator_col}{Character scalar or \code{NULL} (default
\code{NULL}). Source indicator column for long-form input (one
row per site-year-indicator).}

\item{subgroup_col}{Character scalar or \code{NULL} (default
\code{NULL}). Source subgroup column for publisher files with
subgroup decomposition.}

\item{suppression_col}{Character scalar or \code{NULL} (default
\code{NULL}). Source publisher suppression flag column. With
\code{NULL}, an existing \code{suppression_flag} column is used.}

\item{suppression_flag_value}{Value or vector of values marking
publisher suppression in \code{suppression_col}. Defaults to
\code{""} (the empty string).}

\item{suppression_when}{Function or \code{NULL} (default
\code{NULL}). Optional predicate returning one logical value per
normalized row; overrides flag and missing-numerator detection.}

\item{min_n}{A positive whole number stored in the report's
\code{min_n} attribute. Defaults to \code{10L}. The tier counts
use suppression status and \code{accountability_n}; changing
\code{min_n} alone does not change those counts.}

\item{accountability_n}{Positive integer scalar. Tier-2 /
Tier-3 boundary. Among rows that are not suppressed, those with
\eqn{n_{jt} <}{n_jt <} \code{accountability_n} are Tier 2;
those meeting the threshold are Tier 3.
Defaults to \code{30L}.}
}
\value{
A \code{sitemix_suppression_report} tibble with one row
per group defined by \code{by} and the following columns:
\describe{
\item{group columns}{The grouping columns from \code{by}
(e.g., \code{subgroup}, \code{year}).}
\item{\code{n_rows}}{Integer total row count in the group.}
\item{\code{n_tier1}}{Integer count of Tier 1 (publisher-
suppressed) rows in the group.}
\item{\code{n_tier2}}{Integer count of Tier 2 (observed
below \code{accountability_n}) rows.}
\item{\code{n_tier3}}{Integer count of Tier 3 (observed and
meets threshold) rows.}
\item{\code{n_suppressed_hidden_denominator}}{Integer count
of suppressed rows whose denominator is also hidden.}
\item{\code{n_denominator_missing}}{Integer count of rows
with a missing denominator value.}
\item{\code{pct_suppressed}}{Numeric share of suppressed
rows in the group (Tier 1).}
\item{\code{pct_below_accountability}}{Numeric share of
suppressed rows plus rows that are not suppressed and fall below
the threshold:
Tier 1 rows plus Tier 2 rows
below \code{accountability_n}.}
\item{\code{median_n_suppressed}}{Numeric; median denominator
of suppressed rows when observable, else \code{NA}.}
\item{\code{denominator_observed_on_suppressed}}{Logical;
\code{TRUE} when every Tier 1 row in the group carries an
observable denominator.}
\item{\code{suppression_sources}}{Character; a compact
list of the detection rules used (publisher flag,
missing numerator, or user predicate).}
\item{\code{recommended_action}}{Character; a one-line
recommendation distinguishing retained missing estimates from
an acknowledged variance sensitivity.}
\item{\code{sensitivity_role}}{Character with two values:
\itemize{
\item \code{"none"} when Tier 1 is absent.
\item \code{"nonidentified_variance_sensitivity"} otherwise.
}
}
\item{\code{sensitivity_numeric_variance_available}}{Logical;
\code{TRUE} only when Tier-1 denominators are all observed, so a
separated worst-case variance can be quantified.}
\item{\code{sensitivity_requires_acknowledgement}}{Logical;
\code{TRUE} whenever a Tier-1 row is present.}
\item{\code{upper_bound_role}}{Character; identifies the
legacy \code{"upper_bound"} option as a non-identified
variance-sensitivity scenario rather than an estimate. This older
counterpart of \code{sensitivity_role} uses \code{"not_applicable"}
when Tier 1 is absent, whereas \code{sensitivity_role} uses
\code{"none"}.}
}

Two older column names are also retained:
\code{upper_bound_numeric_variance_available} mirrors
\code{sensitivity_numeric_variance_available};
\code{upper_bound_requires_acknowledgement} mirrors
\code{sensitivity_requires_acknowledgement}.
}
\description{
\code{sm_suppression_report()} counts publisher-suppressed rows and rows
below a chosen denominator threshold. It also reports whether
suppressed denominators are observed. Use it to examine published
input data before calling \code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}}. It does
not fill in hidden values.
}
\details{
The report groups rows into three tiers:
\describe{
\item{Tier 1 -- suppressed}{Rows identified by a publisher flag or
a missing numerator with an observed positive denominator. A
\code{suppression_when} predicate overrides both rules. Missing
denominators alone do not identify suppression.}
\item{Tier 2 -- below threshold}{Rows that are not suppressed and have
an observed denominator smaller than \code{accountability_n}.}
\item{Tier 3 -- meets threshold}{Rows that are not suppressed and have
an observed denominator at least as large as \code{accountability_n}.}
}
These tiers describe suppression and the chosen size threshold.
The report retains missing-denominator counts separately. Its
\code{accountability_n} threshold can be changed; it is distinct
from the fixed 11--29 denominator range used by
\code{sm_smooth_variance(scope = "tier2")}.

Each group defined by \code{by} (for example,
\code{c("subgroup", "year")}) receives counts and proportions.
For suppressed rows, the report distinguishes retaining missing
estimates from an explicitly acknowledged variance-sensitivity
analysis. Use \code{\link[=sm_diagnose]{sm_diagnose()}} to examine the resulting estimates.
}
\examples{
\dontshow{set.seed(1L)}
# Prepare aggregate counts from the simulated data:
counts_path <- system.file(
  "extdata", "prek_sim_counts.rds",
  package = "sitemix", mustWork = TRUE
)
counts <- readRDS(counts_path)

d0 <- counts[, c("site_id", "year", "n_jt", "c_jt_frpm")]
d0$indicator <- "frpm"
d0$c_jt <- d0$c_jt_frpm
d0$subgroup <- "all"
d0 <- d0[c("site_id", "year", "subgroup", "indicator", "c_jt", "n_jt")]

report <- sm_suppression_report(
  d0,
  by              = c("subgroup", "year"),
  numerator_col   = "c_jt",
  denominator_col = "n_jt",
  indicator_col   = "indicator",
  subgroup_col    = "subgroup"
)
head(report)

}
\seealso{
\itemize{
\item \code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}}
for estimating proportions and handling suppressed input rows.
\item \code{\link[=sm_diagnose]{sm_diagnose()}} for the post-estimation
diagnostics.
\item \code{\link[=sm_pivot_subgroups_to_sites]{sm_pivot_subgroups_to_sites()}}
and \code{\link[=sm_pivot_subgroups_to_indicators]{sm_pivot_subgroups_to_indicators()}}
for reshaping subgroup data before making the report.
\item \code{vignette("a5-published-aggregates")} and
\code{vignette("a6-diagnostics-and-suppression")} for the applied
walkthroughs.
}

Other audit:
\code{\link[=sm_diagnose]{sm_diagnose()}}
}
\concept{audit}
