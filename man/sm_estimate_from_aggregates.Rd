% Please edit documentation in R/sm_estimate_from_aggregates.R
\encoding{UTF-8}
\name{sm_estimate_from_aggregates}
\alias{sm_estimate_from_aggregates}
\title{Estimate site-year rates from published aggregate rows}
\usage{
sm_estimate_from_aggregates(
  data,
  family,
  indicator = NULL,
  indicators = NULL,
  id_cols = c("site_id", "year"),
  numerator_col = NULL,
  denominator_col = NULL,
  indicator_col = NULL,
  subgroup_col = NULL,
  aggregate_case = c("auto", "D0", "D1"),
  framing = NA_character_,
  sampling_relation = c("unknown", "same_units", "different_units"),
  accountability_n = 30L,
  suppression = c("drop", "upper_bound"),
  suppression_col = NULL,
  suppression_flag_value = "",
  suppression_when = NULL,
  suppressed_theta_hat = 0.5,
  suppression_sensitivity_acknowledge = FALSE,
  suppressed_n_strategy = c("observed_n", "worst_case_bound"),
  suppressed_n_bound = NULL,
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

\item{numerator_col}{A single column name, or \code{NULL}
(default). Maps an aggregate numerator to \code{c_jt}.
With \code{NULL}, long-form input uses an existing \code{c_jt}
column; wide-form input uses \verb{c_jt_*} columns.}

\item{denominator_col}{A single column name, or \code{NULL}
(default). Maps an aggregate denominator to \code{n_jt}.
With \code{NULL}, use the standard denominator columns described
in \code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}}.}

\item{indicator_col}{A single column name, or \code{NULL}
(default). Maps long-form aggregate indicator labels to
\code{indicator}; \code{NULL} uses that name if present.}

\item{subgroup_col}{A single column name, or \code{NULL}
(default). Maps aggregate subgroup labels to \code{subgroup};
\code{NULL} uses that name if present.}

\item{aggregate_case}{A single string: \code{"auto"} (default,
inferred from the indicators), \code{"D0"}, or \code{"D1"}.
Valid values have no effect outside aggregate input; invalid values
always raise an error.}

\item{framing}{\code{NA_character_} (default),
\code{"subgroup_as_site"}, or \code{"subgroup_as_indicator"}.
Describes how aggregate subgroups are represented. Reshape the data
first with \code{\link[=sm_pivot_subgroups_to_sites]{sm_pivot_subgroups_to_sites()}} or
\code{\link[=sm_pivot_subgroups_to_indicators]{sm_pivot_subgroups_to_indicators()}}; this argument does not reshape
raw subgroup rows. Invalid values raise
\code{sitemix_error_invalid_framing}.}

\item{sampling_relation}{A single string describing the observational
units behind D1 marginals: \code{"unknown"} (default),
\code{"same_units"}, or \code{"different_units"}. These produce
\code{d1_regime} labels \code{"unknown"}, \code{"D1a"}, and
\code{"D1b"}, respectively. Equal denominators do not imply the
same units. Valid values have no effect outside D1; invalid values
raise \code{sitemix_error_invalid_sampling_relation}.}

\item{accountability_n}{A positive whole number. Rows with
\code{n < accountability_n} receive
\code{flag_below_accountability = TRUE}. Defaults to \code{30L}.}

\item{suppression}{A single string for handling publisher-suppressed
aggregate rows: \code{"drop"} (default) or \code{"upper_bound"}.
\code{"drop"} retains a row with missing estimates and standard
errors. \code{"upper_bound"} adds separately labeled Bernoulli
variance-sensitivity fields after explicit acknowledgement; it leaves
the estimate and SE columns missing. These rows cannot supply ordinary
\code{V} or Fréchet inputs. When suppressed rows are present,
\code{"upper_bound"} requires \code{vst = "arcsine"} and
\code{anscombe = FALSE}. Valid values have no effect for other
input types; invalid values always raise an error.}

\item{suppression_col}{A single column name, or \code{NULL}
(default). Names the publisher suppression flag. With \code{NULL},
an existing \code{suppression_flag} column is used if present;
missing numerators can also identify suppressed rows.}

\item{suppression_flag_value}{Values in the suppression flag column
that indicate suppression. Defaults to \code{""} (the empty string).}

\item{suppression_when}{A function, or \code{NULL} (default).
An optional predicate returning one logical value per aggregate row;
it overrides the usual flag and missing-numerator detection.}

\item{suppressed_theta_hat}{A finite numeric value strictly between
0 and 1; defaults to \code{0.5}. Used only for
\code{suppression = "upper_bound"}. When suppressed rows are
present, it must be \code{0.5}, which maximizes Bernoulli variance.
The value is stored as \code{sensitivity_probability}; it never
replaces an observed or missing point estimate.}

\item{suppression_sensitivity_acknowledge}{A single logical value,
defaulting to \code{FALSE}. Must be \code{TRUE} when
\code{suppression = "upper_bound"} encounters suppressed rows.
The additional fields describe a variance-sensitivity scenario;
they are not estimated values or ordinary covariance inputs.}

\item{suppressed_n_strategy}{A single string: \code{"observed_n"}
(default) or \code{"worst_case_bound"}. For suppressed upper-bound
rows, the default uses the observed denominator. The latter records
\code{suppressed_n_bound} as \code{n} and \code{n_eff}, even
when the input denominator is observed. On observed-denominator rows,
that chosen value is also \code{sensitivity_n} in the variance
calculation. A hidden denominator supplies no numeric sensitivity
variance; a recorded bound does not identify the actual sample size.}

\item{suppressed_n_bound}{A positive whole number, or \code{NULL}
(default). Required for active suppressed upper-bound rows with
\code{suppressed_n_strategy = "worst_case_bound"}; must be no larger
than \code{min_n}. It becomes the recorded row denominator and, when
the input denominator is observed, the sensitivity denominator.
Hidden-denominator rows retain missing sensitivity variances.}

\item{...}{Additional arguments forwarded to \code{\link[=sm_estimate]{sm_estimate()}}.}
}
\value{
A \code{sitemix_estimates} tibble with one row per
site-year-indicator. It contains proportions, standard errors,
their scales and calculation methods, and suppression flags.
With \code{vjt = TRUE}, it also includes a \code{V} list-column.
See \code{\link[=sm_estimate]{sm_estimate()}} for the returned columns and attributes.
}
\description{
\code{sm_estimate_from_aggregates()} estimates site-year proportions and
standard errors from published numerator and denominator counts. Use
\code{family = "binomial"} for one indicator (D0), or
\code{family = "multivariate"} for several marginal indicators (D1).
D1 can return a covariance matrix based on working independence;
marginal counts alone do not identify cross-indicator dependence.

This function calls \code{\link[=sm_estimate]{sm_estimate()}} with \code{from_aggregates = TRUE}.
Calling \code{sm_estimate(..., from_aggregates = TRUE)} directly
produces the same result. Do not pass \code{from_aggregates} or
\code{from_counts} through \code{...}; the latter raises
\code{sitemix_error_input_path_conflict}. Published aggregates do not
support \code{family = "multinomial"}; this raises
\code{sitemix_error_ambiguous_dispatch}. Use
\code{\link[=sm_estimate_from_counts]{sm_estimate_from_counts()}} with complete category counts for Scenario C.
}
\details{
\strong{Input columns.} Each row needs the site and year columns named
by \code{id_cols}. Counts can be arranged in either of two forms:
\itemize{
\item Long form: one row per site-year-indicator, with
\code{indicator}, \code{c_jt}, and \code{n_jt}. For D0, the
\code{indicator} argument can supply the label when the column
is absent.
\item Wide form: one row per site-year, with numerator columns named
\verb{c_jt_<indicator>}. Supply either a common \code{n_jt}
denominator or a matching \verb{n_jt_<indicator>} column for
every indicator. Do not mix these denominator forms. Use
\code{indicators} to specify the marginal indicator names.
}
Use \code{numerator_col}, \code{denominator_col},
\code{indicator_col}, \code{subgroup_col}, and \code{suppression_col}
when the corresponding columns have other names. With \code{NULL}
(the default), the function uses existing \code{c_jt}, \code{n_jt},
\code{indicator}, \code{subgroup}, and \code{suppression_flag}
columns, respectively. Wide-form numerator and per-indicator denominator
columns use the naming patterns above. Subgroup and suppression-flag
columns are optional.

\strong{One or several indicators.} Choose the family to match the
available counts:

\describe{
\item{\strong{D0}}{Use when \code{family = "binomial"} and the
input has one numerator and one denominator per site-year.}
\item{\strong{D1}}{Use when \code{family = "multivariate"}
and the input has multiple aggregate
marginals per site-year. With \code{vjt = TRUE}, the returned
\code{V} is diagonal: off-diagonal zeros express a working
independence assumption, not observed independence. For raw pairwise
Fréchet intervals and separately labeled projected stress scenarios,
see \code{\link[=sm_frechet_envelope]{sm_frechet_envelope()}}.}
}

Set \code{aggregate_case = "auto"} (default) to resolve one unique
indicator as D0 and two or more as D1; pass \code{"D0"} or
\code{"D1"} to require that case. D1 requires the same ordered
indicator set in every site-year group. Set
\code{sampling_relation = "same_units"} only when the marginal
rows are known to describe the same observational units, or
\code{"different_units"} when they are known to differ. The default
\code{"unknown"} makes no such claim. Common denominators are
recorded separately and never imply \code{"same_units"}.

\strong{Subgroups.} If each site has several subgroup rows, decide
whether to estimate each subgroup as a separate site (Framing X) or
as an indicator within the original site (Framing Y). Reshape the file
first with \code{\link[=sm_pivot_subgroups_to_sites]{sm_pivot_subgroups_to_sites()}} or
\code{\link[=sm_pivot_subgroups_to_indicators]{sm_pivot_subgroups_to_indicators()}}, respectively, then pass the
resulting table here. See
\code{vignette("a5-published-aggregates", package = "sitemix")}.

\strong{Suppression.} Use \code{\link[=sm_suppression_report]{sm_suppression_report()}} to count
publisher-suppressed rows before estimation. Detection uses the
suppression flag and missing numerators unless \code{suppression_when}
supplies a custom rule; see the suppression arguments below.
\code{suppression = "drop"} retains each suppressed row with missing
estimates and standard errors.

\code{suppression = "upper_bound"} requires
\code{suppression_sensitivity_acknowledge = TRUE} when suppressed rows
are present. It stores a worst-case Bernoulli variance scenario in
separate \code{sensitivity_*} fields, leaving the estimate and SE
columns missing. These rows cannot supply ordinary covariance or formal
Fréchet inputs. A hidden denominator cannot support a numeric sensitivity
variance; \code{suppressed_n_strategy} and \code{suppressed_n_bound}
record the available denominator information without estimating that
variance.
}
\examples{
\dontshow{set.seed(1L)}
# Select one indicator and year from the bundled simulated counts:
counts_path <- system.file(
  "extdata", "prek_sim_counts.rds",
  package = "sitemix", mustWork = TRUE
)
counts <- readRDS(counts_path)

d0_frpm <- counts[counts$year == 2024, c("site_id", "year", "n_jt", "c_jt_frpm")]
d0_frpm$indicator <- "frpm"
d0_frpm$c_jt <- d0_frpm$c_jt_frpm
d0_frpm <- d0_frpm[c("site_id", "year", "indicator", "c_jt", "n_jt")]

est <- sm_estimate_from_aggregates(
  d0_frpm,
  family    = "binomial",
  indicator = "frpm"
)
head(est, 5)
# theta_hat and se use this scale; theta_raw remains a proportion.
unique(est$estimate_scale)

}
\seealso{
\itemize{
\item \code{\link[=sm_estimate]{sm_estimate()}} for estimation options
and returned columns.
\item \code{\link[=sm_estimate_from_counts]{sm_estimate_from_counts()}}
for estimates from sufficient counts.
\item \code{\link[=sm_pivot_subgroups_to_sites]{sm_pivot_subgroups_to_sites()}}
and \code{\link[=sm_pivot_subgroups_to_indicators]{sm_pivot_subgroups_to_indicators()}}
for the Framing X and Framing Y pivots.
\item \code{\link[=sm_suppression_report]{sm_suppression_report()}} for
publisher-side suppression auditing.
\item \code{\link[=sm_frechet_envelope]{sm_frechet_envelope()}} for D1
aggregate sensitivity.
\item \code{vignette("a5-published-aggregates")} for the applied
walkthrough.
\item \code{vignette("m5-aggregate-engines")} for D0 / D1
estimation methods and assumptions.
}

Other estimation:
\code{\link[=sm_estimate]{sm_estimate()}},
\code{\link[=sm_estimate_from_counts]{sm_estimate_from_counts()}}
}
\concept{estimation}
