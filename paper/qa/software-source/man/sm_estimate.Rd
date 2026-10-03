% Please edit documentation in R/sm_estimate.R
\encoding{UTF-8}
\name{sm_estimate}
\alias{sm_estimate}
\title{Estimate site-year proportions and standard errors}
\usage{
sm_estimate(
  data,
  family,
  indicator = NULL,
  indicators = NULL,
  id_cols = c("site_id", "year"),
  vst = c("arcsine", "logit", "none"),
  boundary_method = c("wilson_floor", "agresti_coull", "none"),
  bias_correction = NULL,
  vjt = FALSE,
  min_n = 10L,
  accountability_n = 30L,
  fpc = NULL,
  anscombe = FALSE,
  from_counts = FALSE,
  na_action = c("drop_rows", "error"),
  description = NULL,
  from_aggregates = FALSE,
  aggregate_case = c("auto", "D0", "D1"),
  framing = NA_character_,
  sampling_relation = c("unknown", "same_units", "different_units"),
  suppression = c("drop", "upper_bound"),
  suppression_col = NULL,
  suppression_flag_value = "",
  suppression_when = NULL,
  suppressed_theta_hat = 0.5,
  suppression_sensitivity_acknowledge = FALSE,
  suppressed_n_strategy = c("observed_n", "worst_case_bound"),
  suppressed_n_bound = NULL,
  numerator_col = NULL,
  denominator_col = NULL,
  indicator_col = NULL,
  subgroup_col = NULL
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

\item{vst}{A single string giving the transformation used for
\code{theta_hat} and \code{se}: \code{"arcsine"} (default),
\code{"logit"}, or \code{"none"}. The returned
\code{estimate_scale} column records the chosen scale.}

\item{boundary_method}{A single string specifying how to calculate
uncertainty when \eqn{C_{jt} \in \{0, n_{jt}\}}{C_jt in {0, n_jt}}:
\code{"wilson_floor"} (default), \code{"agresti_coull"}, or
\code{"none"}. Boundary rows have \code{flag_zero_cell = TRUE}.
The Wilson and Agresti--Coull options replace boundary uncertainty
with a surrogate; they do not move the observed raw proportion
away from 0 or 1. Logit output is unavailable at these boundaries.}

\item{bias_correction}{\code{NULL} (default) or \code{"binomial_bc"}.
The latter uses the n-1 correction for interior scalar variances and
propagates it to arcsine or logit standard errors by the delta method.
B/C covariance-matrix correction is handled separately. This option
cannot be combined with \code{anscombe = TRUE}.}

\item{vjt}{A single logical value. If \code{TRUE}, include within-site
covariance matrices as a \code{V} list-column of \code{\link[=sm_vcov]{sm_vcov()}} objects.
Defaults to \code{FALSE}. Use it for analyses involving several
indicators together. Agresti--Coull boundary uncertainty is not
supported for covariance-matrix output.}

\item{min_n}{A positive whole number. Rows with \code{n < min_n}
receive \code{flag_small_n = TRUE}. Defaults to \code{10L}.}

\item{accountability_n}{A positive whole number. Rows with
\code{n < accountability_n} receive
\code{flag_below_accountability = TRUE}. Defaults to \code{30L}.}

\item{fpc}{Positive whole-number population sizes, or \code{NULL}
(default). Supply the population size \eqn{N}{N} for simple random
sampling without replacement (SRSWOR). A single value is used for
all site-year groups. A vector must align with input rows and be
constant within each \code{id_cols} group. Each retained group
requires \eqn{N \ge n}{N >= n}; equality represents a census with
zero sampling uncertainty. This argument is a population size,
not a variance multiplier. Aggregate inputs containing any suppressed
rows, including retained missing rows and sensitivity rows, cannot
currently use \code{fpc}.}

\item{anscombe}{A single logical value. If \code{TRUE}, apply the
Anscombe correction shown in \emph{Details}. Defaults to
\code{FALSE}. Requires \code{vst = "arcsine"} and cannot be
combined with \code{boundary_method = "agresti_coull"} or
\code{bias_correction = "binomial_bc"}.}

\item{from_counts}{A single logical value. If \code{TRUE}, treat
\code{data} as one row of sufficient counts per site-year.
Defaults to \code{FALSE}; cannot be combined with
\code{from_aggregates = TRUE}. \code{\link[=sm_estimate_from_counts]{sm_estimate_from_counts()}} sets
this argument automatically.}

\item{na_action}{A single string describing how to handle missing
indicator values: \code{"drop_rows"} (default) or \code{"error"}.
The latter raises \code{sitemix_error_input_missing} on any \code{NA}.}

\item{description}{A single string, or \code{NULL} (default).
An optional label stored in the returned object's
\code{description} attribute.}

\item{from_aggregates}{A single logical value. If \code{TRUE}, treat
\code{data} as published aggregates (D0/D1). Defaults to
\code{FALSE}; cannot be combined with \code{from_counts = TRUE}.
\code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}} sets this argument automatically.}

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
that indicate suppression. Defaults to \code{""} (the empty string).
For character flags, explicitly supply the publisher's marker (for
example \code{"*"}); a blank is otherwise treated as suppressed.
Logical flags use \code{TRUE} irrespective of this argument.}

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
(default) or \code{"worst_case_bound"}. Published denominators are
always retained as \code{n}, \code{n_eff}, and the sensitivity
denominator. The latter strategy records \code{suppressed_n_bound}
as \code{n} and \code{n_eff} only when the denominator is hidden.
A hidden denominator supplies no numeric sensitivity variance;
a recorded bound does not identify the actual sample size.}

\item{suppressed_n_bound}{A positive whole number, or \code{NULL}
(default). Required for suppressed upper-bound rows with hidden
denominators and \code{suppressed_n_strategy = "worst_case_bound"};
must be no larger than \code{min_n}. This recorded bound never
replaces a published denominator. Hidden-denominator rows retain
missing sensitivity variances.}

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
}
\value{
A \code{sitemix_estimates} tibble with one row per
site-year-indicator. The main columns are:

\describe{
\item{\code{site_id}}{Character site identifier.}
\item{\code{year}}{Integer year.}
\item{\code{indicator}}{Character indicator or category name.}
\item{\code{theta_raw}}{Numeric in \eqn{[0, 1]}{[0, 1]}; the
observed proportion \eqn{\hat\pi_{jt} = C_{jt}/n_{jt}}{theta_raw = C_jt/n_jt}.
Missing for suppressed rows.}
\item{\code{theta_hat}}{Numeric; the point estimate on the
scale named by the \code{estimate_scale} column.}
\item{\code{se_raw}}{Numeric; the raw-scale standard error before
any VST. Finite and non-negative when estimable; \code{NA} for
intentionally suppressed/drop rows.}
\item{\code{se}}{Numeric; the scalar standard error on the
\code{estimate_scale}. Finite and non-negative when estimable;
\code{NA} for intentionally suppressed/drop rows.}
\item{\code{n}}{Positive integer denominator. A hidden-denominator
suppression row may instead record \code{suppressed_n_bound};
that value does not establish an observed sample size.}
\item{\code{n_eff}}{Numeric denominator used for transformed
standard errors: \code{n + 1/2} with Anscombe, otherwise \code{n}.
This is distinct from the \code{n + 3/4} in the Anscombe point
transformation. Finite-population correction leaves it unchanged.}
\item{\code{estimate_scale}}{Character scalar; the scale of
\code{theta_hat} and \code{se}. One of \code{"none"},
\code{"arcsine"}, \code{"arcsine_anscombe"}, or
\code{"logit"}.}
\item{\code{transform}}{Character scalar; the VST applied
(typically equals \code{estimate_scale}).}
\item{\code{var_method}}{Character string naming the standard-error
calculation; possible values are listed in \emph{Details}.}
\item{\code{flag_small_n}}{Logical; \code{TRUE} if
\eqn{n_{jt} <}{n_jt <} \code{min_n}.}
\item{\code{flag_zero_cell}}{Logical; \code{TRUE} at an identified
boundary \eqn{C_{jt} \in \{0, n_{jt}\}}{C_jt in {0, n_jt}},
\code{FALSE} for an observed interior proportion, and \code{NA} when
publisher suppression hides the numerator.}
\item{\code{input_mode}}{Character scalar; one of
\code{"student_level"}, \code{"counts_full_suff"}, or
\code{"aggregate"}, recording the input type.}
\item{\code{flag_suppressed}}{Logical; \code{TRUE} if the
site-year was suppressed by the publisher (Tier 1 in the
reporting scheme). Always \code{FALSE} when
\code{from_aggregates = FALSE}.}
\item{\code{framing}}{Character scalar; the aggregate subgroup
framing label (\code{NA_character_} for direct D0 / non-aggregate
paths).}
\item{\code{flag_below_accountability}}{Logical; \code{TRUE} if
\eqn{n_{jt} <}{n_jt <} \code{accountability_n}.}
\item{\strong{Suppression information}}{Optional columns returned as one
set when an aggregate input contains Tier-1 rows:
\itemize{
\item Status: \code{estimate_status}.
\item Values: \code{sensitivity_probability},
\code{sensitivity_var_raw}, \code{sensitivity_var}, and
\code{sensitivity_n}.
\item Method and acknowledgement: \code{sensitivity_method} and
\code{sensitivity_acknowledged}.
}
\code{estimate_status} distinguishes identified rows,
retained suppressed-missing rows, and non-identified sensitivity
rows. Observed-denominator sensitivity uses probability 0.5 and
variance \eqn{0.25/n}{0.25/n}; hidden-denominator rows leave the
numeric sensitivity variance and denominator missing.}
\item{\strong{Finite-population information}}{Optional structured
SRSWOR columns returned together when \code{fpc} is supplied:
\itemize{
\item Design: \code{population_size}, \code{sampling_fraction}, and
\code{sampling_design}.
\item Conventional FPC multipliers: \code{fpc_variance_multiplier} and
\code{fpc_se_multiplier}.
\item Applied multipliers: \code{variance_multiplier_applied} and
\code{se_multiplier_applied}, with \code{variance_rule}.
}
\code{n_eff} remains unchanged; \code{variance_rule} distinguishes
plug-in from design-corrected \code{binomial_bc} uncertainty. The
\code{fpc_*} columns record the conventional SRSWOR \eqn{q}{q}, while
\code{*_applied} records the multiplier actually applied relative to
the same infinite-population variance rule.}
\item{\code{V}}{Optional list-column of
\code{\link[=sm_vcov]{sm_vcov}} objects. Present when
\code{vjt = TRUE}. For Scenarios A / D0 each element is
\eqn{1 \times 1}{1 x 1}; for B / C / D1 the dimension is the
indicator count.}
\item{\code{K}}{Optional integer column; present only alongside
\code{V} for Scenarios B / C / D1 when \code{vjt = TRUE}, recording
the number of indicators or categories.}
}

The returned object also carries the following object-level
attributes:

\describe{
\item{\code{description}}{Character scalar or \code{NULL};
verbatim copy of the \code{description} argument.}
\item{\code{family}}{Character scalar or \code{NULL}; the
estimation family.}
\item{\code{sitemix_role}}{Character scalar; the output role,
normally \code{"summary_uncertainty"}.}
\item{\code{aggregate_case}}{For aggregate input, the resolved
\code{"D0"} or \code{"D1"} scenario.}
\item{\code{sampling_relation}}{For D1, whether the caller stated
that the indicators describe the same or different units.}
\item{\code{denominator_pattern}}{For D1, \code{"common"},
\code{"varying"}, \code{"incomplete"}, or \code{"mixed"},
summarized across site-year groups.}
\item{\code{d1_regime}}{For D1, the object-level regime summary derived
from sampling relation and denominator pattern.}
\item{\code{d1_regime_by_group}}{For D1, a per-group table that
records indicator count, sampling relation, denominator pattern,
and the resulting regime label.}
\item{\code{suppression}}{For D0/D1, suppression detection,
whether denominators are observed, and any acknowledged
sensitivity analysis.}
\item{\code{smoothing}}{After experimental smoothing, the target,
method, fit status, and relationship to the covariance matrices.}
}
}
\description{
\code{sm_estimate()} estimates proportions and standard errors for each
site and year. It accepts individual student rows, sufficient counts,
or published aggregates. Choose \code{family = "binomial"} for one
binary indicator, \code{"multivariate"} for overlapping binary
indicators, or \code{"multinomial"} for mutually exclusive categories.

For counts or published aggregates, \code{\link[=sm_estimate_from_counts]{sm_estimate_from_counts()}} and
\code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}} set the input type for you. You can also
use \code{from_counts = TRUE} or \code{from_aggregates = TRUE} here.
The result is a \code{sitemix_estimates} tibble. Use \code{\link[=sm_diagnose]{sm_diagnose()}}
to check the estimates and their uncertainty before further analysis.
}
\details{
\strong{Input types.} The documentation uses the following scenario
labels for the supported combinations of input and family:

\tabular{llll}{
Input \tab \code{binomial} \tab \code{multivariate} \tab \code{multinomial} \cr
Student rows \tab A \tab B \tab C \cr
Sufficient counts \tab A \tab B \tab C \cr
Published aggregates \tab D0 \tab D1 \tab Not supported \cr
}

For student rows, use \code{indicator} to name a single binary column
(A) or a factor/character column of categories (C). Use \code{indicators}
to name several binary columns that can overlap (B).

Counts input for overlapping indicators requires each marginal count
and every pairwise co-occurrence count, in the order specified by
\code{indicators}. When \code{vjt = TRUE}, the covariance calculation
checks whether these counts can come from a common sample. It supports
two or three indicators; covariance requests with four or more
indicators require student rows. Counts input with four or more
indicators is accepted when \code{vjt = FALSE}.

Published aggregates use numerator/denominator pairs (D0) or several
marginal proportions (D1). D1 covariance assumes working independence;
it does not recover dependence from the marginals. The
\code{sampling_relation} argument records whether the indicators refer
to the same observational units. Equal denominators alone do not
establish this. See \code{\link[=sm_frechet_envelope]{sm_frechet_envelope()}} for dependence sensitivity.
Published marginals do not supply a multinomial composition; use
complete category counts for C instead. An aggregate request with
\code{family = "multinomial"} raises
\code{sitemix_error_ambiguous_dispatch}.

\code{from_counts} and \code{from_aggregates} cannot both be
\code{TRUE}; this raises \code{sitemix_error_input_path_conflict}.
Omitting \code{family} raises \code{sitemix_error_invalid_family}.
Argument values are checked even when they do not affect the selected
input type. Valid unused values have no effect; invalid values raise
an error. Required data columns and combinations of arguments are
checked for the input type being used.

\strong{Transformations.} By default, \code{theta_hat} and \code{se}
use the arcsine square-root scale (\code{vst = "arcsine"}). Without
bias correction, its first-order standard error, before any
finite-population correction, is
\deqn{\mathrm{SE}(\hat\theta_{jt}) = 1 / (2\sqrt{n_{jt}}).}{SE(theta_hat) = 1 / (2 * sqrt(n_jt)).}
With \code{bias_correction = "binomial_bc"}, interior arcsine and
logit rows instead propagate the n-1-corrected raw variance through
the corresponding delta method. Use \code{vst = "logit"} for the
logit scale or \code{vst = "none"} for proportions. The
\code{estimate_scale} column identifies the scale of the returned
estimates and standard errors. Any \code{V} matrix has its own
\code{vcov_scale}, which can differ; see \emph{Scale conventions} below.

With \code{anscombe = TRUE}, the transformation is
\deqn{\arcsin\sqrt{(C_{jt}+3/8)/(n_{jt}+3/4)}}{asin(sqrt((C_jt + 3/8) / (n_jt + 3/4)))}
This option requires \code{vst = "arcsine"}; other transforms raise
\code{sitemix_error_anscombe_requires_arcsine}. For derivations, see
\code{vignette("m2-scalar-se-binomial")}.

\strong{Standard-error methods.} The \code{var_method} column names
the calculation used for each row:
\itemize{
\item Arcsine: \code{"arcsine_vst"}, \code{"arcsine_anscombe"},
or \code{"arcsine_delta_binomial_bc"}.
\item Logit: \code{"logit_delta"} or
\code{"logit_delta_binomial_bc"}.
\item Untransformed: \code{"binomial"} or \code{"binomial_bc"}.
\item Boundary standard errors: \code{"wilson_boundary_surrogate"}
or \code{"agresti_coull_boundary_surrogate"}.
\item Suppressed rows: \code{"suppressed_drop"} or
\code{"suppression_sensitivity"}.
}
\code{\link[=sm_smooth_variance]{sm_smooth_variance()}} records experimental alternatives with
\code{" + gvf_smooth_loglinear"} or \code{" + gvf_smooth_gam"}
in \code{var_method_smoothed}. They replace \code{var_method} only
when an allowed overwrite is requested. Older \code{" + fh_smooth_*"}
labels can still be read. The method used for a covariance matrix is
recorded separately in its \code{vcov_method} field; see \code{\link[=sm_vcov]{sm_vcov()}}.
}
\section{Using the estimates in other analyses}{

Run \code{\link[=sm_diagnose]{sm_diagnose()}} before selecting or converting the results.
\code{as.data.frame()} changes their format but does not check whether
they are suitable for an analysis. Keep site/year/indicator identifiers,
\code{theta_hat}, \code{se}, \code{estimate_scale}, \code{var_method},
reporting flags, and any \code{estimate_status} or \code{sensitivity_*}
columns together so that the meaning of each row remains clear.

Ordinary inverse-variance weighting requires identified finite estimates
and finite, strictly positive standard errors. Census rows with zero SE,
suppressed rows, and sensitivity scenarios should remain available for
inspection, but should not receive ordinary inverse-variance weights.
\code{sensitivity_var} is not a replacement for \code{se}. If you
choose a smoothed SE, retain \code{var_method_smoothed} and the original
SE/method pair as well. For joint analyses, keep complete site-year
indicator groups and check each matrix's \code{indicator_order},
dimnames, and \code{vcov_scale}. See
\code{vignette("a8-downstream-workflows", package = "sitemix")}.
}

\section{Scale conventions}{

With a single binary indicator (A/D0), \code{V} is a
\eqn{1 \times 1}{1 x 1} matrix on the row's estimate scale. For
example, arcsine output has \code{vcov_scale = "arcsine_delta"}.
D1 working-independence matrices also follow the row's estimate scale.

For overlapping indicators (B) and mutually exclusive categories (C),
\code{V} stays on the raw probability scale even when the row estimates
are transformed. Thus \code{sqrt(diag(V))} should not be compared with
transformed \code{se}. sitemix does not transform B/C matrices to match
a different row scale. Always inspect \code{vcov_scale} before using
a matrix in a joint analysis.

The \code{diag_contract} field identifies the corresponding scalar
variance: A/D0/D1 matrices match \code{se^2}; B/C raw matrices usually
match \code{se_raw^2}. For multinomial boundary cells, scalar boundary
surrogates intentionally differ from the simplex diagonal. With
\code{fpc}, the matrix records the population size, sampling fraction,
conventional FPC, applied multiplier, and variance rule. It does not
change \code{n_eff}. See \code{\link[=sm_vcov]{sm_vcov()}} for these fields and
\code{vignette("m8-output-contract", package = "sitemix")} for
the conditions needed to use the returned estimates together.
}

\examples{
\dontshow{set.seed(1L)}
data(prek_sim, package = "sitemix")

# Estimate the FRPM proportion at each site in 2024.
est_a <- sm_estimate(
  subset(prek_sim, year == 2024),
  family    = "binomial",
  indicator = "frpm",
  vst       = "arcsine"
)
head(est_a, 5)
unique(est_a$estimate_scale) # theta_hat and se use the arcsine scale

# Estimate two overlapping indicators and retain their covariance.
est_b <- sm_estimate(
  subset(prek_sim, year == 2024),
  family     = "multivariate",
  indicators = c("frpm", "snap"),
  vst        = "arcsine",
  vjt        = TRUE
)
head(est_b, 4)
est_b$V[[1L]]$vcov_scale   # V is on the raw probability scale
est_b$V[[1L]]$vcov_method  # "sur"

}
\references{
Agresti, A. & Coull, B. A. (1998). Approximate is better than
"exact" for interval estimation of binomial proportions.
\emph{The American Statistician}, \bold{52}(2), 119--126.
\doi{10.1080/00031305.1998.10480550}

Anscombe, F. J. (1948). The transformation of Poisson, binomial
and negative-binomial data. \emph{Biometrika}, \bold{35}(3/4),
246--254. \doi{10.1093/biomet/35.3-4.246}

Wilson, E. B. (1927). Probable inference, the law of succession,
and statistical inference. \emph{Journal of the American
Statistical Association}, \bold{22}(158), 209--212.
}
\seealso{
\code{\link[=sm_estimate_from_counts]{sm_estimate_from_counts()}} for sufficient counts;
\code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}} for published aggregates;
\code{\link[=sm_diagnose]{sm_diagnose()}} for checking uncertainty;
\code{\link[=sm_vcov]{sm_vcov()}} for covariance matrices and their method and scale fields;
\code{\link[=sm_smooth_variance]{sm_smooth_variance()}} for experimental variance smoothing;
\code{\link[=sm_frechet_envelope]{sm_frechet_envelope()}} for sensitivity to unknown D1 dependence;
\code{vignette("a1-getting-started", package = "sitemix")} for a
first example;
\code{vignette("m1-statistical-foundations", package = "sitemix")}
for sampling uncertainty and notation;
\code{vignette("m2-scalar-se-binomial", package = "sitemix")} for
standard-error calculations.

Other estimation:
\code{\link[=sm_estimate_from_aggregates]{sm_estimate_from_aggregates()}},
\code{\link[=sm_estimate_from_counts]{sm_estimate_from_counts()}}
}
\concept{estimation}
