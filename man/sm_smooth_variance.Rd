% Please edit documentation in R/smooth.R
\encoding{UTF-8}
\name{sm_smooth_variance}
\alias{sm_smooth_variance}
\title{Smooth standard errors with an experimental variance model}
\usage{
sm_smooth_variance(
  x,
  method = c("loglinear", "gam"),
  scale = c("se", "se_raw"),
  scope = c("all", "tier2"),
  by = NULL,
  formula = NULL,
  bias_correct = TRUE,
  min_n = NULL,
  min_rows = 50L,
  overwrite = FALSE,
  return_diagnostics = FALSE,
  ...
)
}
\arguments{
\item{x}{A \code{sitemix_estimates} object from \code{\link[=sm_estimate]{sm_estimate()}} or one
of its wrappers. Suppressed-missing rows may be retained but are not
smoothed. An object containing non-identified suppression-sensitivity
rows is rejected; smooth identified estimates before conducting a
separate sensitivity analysis.}

\item{method}{A single string: \code{"loglinear"} (default, using
\code{stats::lm()}) or \code{"gam"} (requires the optional
\code{mgcv} package).}

\item{scale}{The standard-error column to smooth: \code{"se"}
(default, on each row's \code{estimate_scale}) or \code{"se_raw"}
(on the raw probability scale). \code{se} is also raw when
\code{estimate_scale = "none"}.}

\item{scope}{A single string: \code{"all"} (default, all eligible
rows) or \code{"tier2"} (eligible rows with \eqn{11 \le n \le 29}{11 <= n <= 29}).
This fixed interval does not follow \code{accountability_n} or
\code{flag_below_accountability}. In \code{\link[=sm_suppression_report]{sm_suppression_report()}}, Tier 2
instead depends on the report's \code{accountability_n} threshold.}

\item{by}{Column names, or \code{NULL} (default). Each column is
converted to a factor named \code{by_1}, \code{by_2}, and so on in
the model data. The default formula includes these fixed effects in
one model. A custom \code{formula} must include the desired terms
explicitly. See \emph{Details} for multiple years.}

\item{formula}{A model formula, or \code{NULL} for the default described
in \emph{Details}. The response must be \code{log_var}. Available
variables are \code{log_var}, \code{log_n}, \code{n},
\code{theta_raw}, \code{p_offset}, and the generated \code{by_*}
factors. Use ordinary model terms and \code{offset()}; GAM formulas
also allow \code{s()}, \code{te()}, \code{ti()}, and \code{t2()}.
The offset variable is \code{log(p_star * (1 - p_star))}, where
\code{p_star} uses \code{theta_raw} in the interior and the Wilson
center at 0/1 boundaries, then is clipped away from 0 and 1 for
numerical stability.}

\item{bias_correct}{A single logical value. If \code{TRUE} (default),
add half the fitted residual variance to the predicted log variance
before applying the exponential function to return to the variance scale.}

\item{min_n}{A finite numeric threshold of at least 1, or \code{NULL}
(default). Eligible rows must also have \code{n >= min_n}.}

\item{min_rows}{Minimum number of eligible rows required to fit.
Supply a finite number of at least 2; it is converted to an integer.
Defaults to \code{50L}. See \emph{Details} for skipped fits.}

\item{overwrite}{A single logical value. Defaults to \code{FALSE},
which preserves the original SE columns. If \code{TRUE} and a model
is fit, also replace the selected SEs for eligible rows, keeping
snapshots of overwritten columns. With \code{scale = "se_raw"},
rows on \code{estimate_scale = "none"} also have \code{se} updated.
Overwrite is rejected if any eligible row has a \code{V} matrix on
the selected scale, because its unchanged diagonal could then disagree
with the new SE. A \code{V} on a different scale is retained unchanged;
its relation to the selected SE is recorded in \code{smoothing}.}

\item{return_diagnostics}{A single logical value. If \code{TRUE} and
a model is fit, add \code{residual_log_var} and attach the fitted
model as \code{smoother_fit}. Defaults to \code{FALSE}. The
\code{smoother_fit_summary} and \code{smoothing} attributes are
recorded regardless of this setting.}

\item{...}{Additional arguments passed to \code{stats::lm()} or
\code{mgcv::gam()}, according to \code{method}.}
}
\value{
A \code{sitemix_estimates} tibble with the same rows and point
estimates as \code{x}. The function adds or updates these columns:
\describe{
\item{\code{se_smoothed}, \code{se_raw_smoothed}}{The alternative
SE: \code{se_smoothed} for \code{scale = "se"}, or
\code{se_raw_smoothed} for \code{scale = "se_raw"}. That column
is added or updated; other SE columns are preserved by default.
Ineligible rows keep their input values.}
\item{\code{var_method_smoothed}}{The calculation label for the
alternative SE. Fitted rows receive a \code{" + gvf_smooth_loglinear"}
or \code{" + gvf_smooth_gam"} suffix; other rows retain their
input \code{var_method}.}
\item{\code{residual_log_var}}{Fit residuals for eligible rows,
with \code{NA} elsewhere. Added only when a model is fit and
\code{return_diagnostics = TRUE}.}
\item{\code{se_pre_smoothing}, \code{se_raw_pre_smoothing}}{
Copies of the input columns that are overwritten. Added only
when a model is fit with \code{overwrite = TRUE}. Raw-scale
overwrite also saves \code{se_pre_smoothing} when it updates
\code{se} for rows with \code{estimate_scale = "none"}.}
}
With \code{overwrite = TRUE}, \code{var_method} is updated wherever
\code{se} is replaced. Overwriting \code{se_raw} on transformed rows
leaves their \code{se} and \code{var_method} unchanged. Covariance
matrices and finite-population sampling information are preserved.

The \code{smoother_fit_summary} attribute records fit status, method,
formula, number of eligible rows, and covariance-scale relation. A
completed fit also records rank, convergence, prediction checks, and
model summaries. The \code{smoothing} attribute records the settings,
selected output column, eligible row indices, and \code{v} information.
Its \code{v$relation} is \code{"absent"}, \code{"matching"},
\code{"incompatible"}, or \code{"mixed"}; \code{v$matrix_effect}
records whether matrices are absent or retained unchanged.
Both attributes are recorded for skipped fits. With
\code{return_diagnostics = TRUE}, a completed fit is additionally
attached as \code{smoother_fit}; a skipped call adds no fitted model.
}
\description{
\code{sm_smooth_variance()} models log variances across rows and adds smoothed
standard errors without changing point estimates. By default it preserves
the original standard errors and adds either \code{se_smoothed} or
\code{se_raw_smoothed}, according to \code{scale}.

This optional generalized variance-function (GVF) model is experimental.
Use its output as a sensitivity alternative; small sample size alone is
not a reason to smooth, and improved precision or downstream performance
is not guaranteed. The function does not fit a Fay--Herriot model or
estimate small-area posterior means.
}
\details{
\strong{Eligible rows.} The input is validated before fitting. A row is
eligible only when it is not suppressed and both its selected standard
error and \code{n} are finite and strictly positive. Thus an exact-census
row with zero SE is excluded. \code{scope = "tier2"} and \code{min_n}
further restrict the eligible rows. The model is fit and predictions are
applied only to those rows; the added SE column retains input values
elsewhere.

If fewer than \code{min_rows} rows qualify, fitting is skipped with
\code{sitemix_warning_smoother_skipped}. The added SE column copies the
selected input column, and \code{var_method_smoothed} copies
\code{var_method}. No overwrite is attempted, even when
\code{overwrite = TRUE}; no new pre-smoothing snapshots, fit residuals,
or fitted model are added. Both summary attributes record the skipped fit.

\strong{Model and grouping.} With \code{method = "loglinear"},
\code{stats::lm()} fits \code{log_var ~ log_n} for \code{scale = "se"},
or \code{log_var ~ log_n + offset(p_offset)} for \code{scale = "se_raw"}.
Here \code{log_var} is the log of the selected SE squared. The raw-scale
offset accounts for the rate-dependent factor \eqn{p(1-p)}{p * (1 - p)}.
With \code{method = "gam"}, \code{mgcv::gam()} uses \code{s(log_n)} in
place of \code{log_n}; the raw-scale offset is retained.

By default, eligible rows from all years enter one model. With
\code{by = "year"}, the default formula adds a year fixed effect to that
model. To fit a separate model for each year, call this function on each
year's subset.

\strong{Warnings.} In addition to the skipped-fit warning above:
\describe{
\item{\code{sitemix_warning_smoother_multi_year_default}}{Eligible
rows span multiple years and \code{by = NULL}.}
\item{\code{sitemix_warning_unexpected_slope}}{A loglinear fit to
\code{scale = "se"} on arcsine or Anscombe-arcsine rows has a
finite \code{log_n} coefficient more than 0.15 away from -1.}
\item{\code{sitemix_warning_raw_scale_smoothing}}{The first request
for \code{scale = "se_raw"} after loading the package. A custom
formula should include \code{offset(p_offset)} or another
rate-dependent term.}
}
Invalid logical controls raise \code{sitemix_error_invalid_smoothing_flag}.
}
\examples{
\dontshow{set.seed(1L)}
data(prek_sim, package = "sitemix")
est <- sm_estimate(
  subset(prek_sim, year == 2024),
  family    = "binomial",
  indicator = "frpm"
)

# Keep the experimental alternative alongside the original SE:
est_s <- sm_smooth_variance(est, method = "loglinear")
head(est_s[, c("site_id", "n", "se", "se_smoothed")], 5)

}
\references{
Wood, S. N. (2017). \emph{Generalized Additive Models: An
Introduction with R} (2nd ed.). Chapman and Hall/CRC.
}
\seealso{
\code{\link[=sm_estimate]{sm_estimate()}} for estimates and their original standard errors;
\code{\link[=sm_diagnose]{sm_diagnose()}} for checking uncertainty before smoothing;
\code{mgcv::gam()} for fitting the GAM;
\code{vignette("a7-variance-smoothing-and-frechet", package = "sitemix")}
for a worked example;
\code{vignette("m6-variance-smoothing-theory", package = "sitemix")}
for the model, assumptions, and simulation findings.
}
\concept{smoothing}
