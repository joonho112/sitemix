#' sitemix: Site- and group-level proportions, proportions, and sampling uncertainty
#'
#' @encoding UTF-8
#'
#' @description
#' `sitemix` estimates site- and group-level proportions and their sampling
#' uncertainty. Use [sm_estimate()] for student rows,
#' [sm_estimate_from_counts()] for sufficient counts, or
#' [sm_estimate_from_aggregates()] for published summaries. The results include
#' point estimates, standard errors, and optional within-site covariance
#' matrices.
#'
#' [sm_diagnose()] checks the returned estimates and uncertainty at the
#' summary, row, or covariance level. Before estimating from published data,
#' [sm_suppression_report()] summarizes suppressed rows and sample-size
#' reporting thresholds.
#'
#' @section Estimation families and aggregate inputs:
#' The package supports the following cases. Covariance matrices are included
#' when \code{vjt = TRUE}; see [sm_estimate()] for supported options and
#' input requirements.
#' \describe{
#'   \item{\strong{Scenario A — binomial}}{One binary indicator per
#'     site-year, supplied as student rows or sufficient counts.
#'     The covariance matrix is \eqn{1 \times 1}{1 x 1}.}
#'   \item{\strong{Scenario B — multivariate}}{Overlapping binary
#'     indicators per site-year with seemingly unrelated regression (SUR)
#'     covariance. Input is student rows or complete sufficient counts
#'     containing marginal and pairwise co-occurrence counts. The covariance is
#'     \eqn{K \times K}{K x K}, where \eqn{K}{K} is the number of indicators.}
#'   \item{\strong{Scenario C — multinomial}}{Mutually exclusive
#'     categories whose counts sum to the denominator. The covariance has
#'     simplex structure and analytic support rank \eqn{S - 1}{S - 1},
#'     where \eqn{S}{S} is the number of categories with positive observed
#'     counts. Input is student rows or complete category counts,
#'     not published D1 marginals.}
#'   \item{\strong{Scenario D0 — aggregate binomial}}{Published
#'     numerator and denominator for one indicator per site-year.}
#'   \item{\strong{Scenario D1 — aggregate marginal}}{Multiple
#'     published marginal proportions per site-year. Marginals alone do not identify
#'     cross-indicator covariance; the optional matrix assumes working
#'     independence. See [sm_frechet_envelope()] for pairwise
#'     intervals and projected dependence scenarios.}
#' }
#'
#' @section Examples and methods:
#' The applied vignettes (a1 to a9) show how to prepare inputs, estimate
#' proportions, check uncertainty, and use the results in further analyses.
#' Start with \code{vignette("a1-getting-started", package = "sitemix")}.
#' The methods vignettes (m1 to m8) explain the sampling assumptions,
#' standard-error and covariance calculations, and sensitivity analyses.
#' Start with \code{vignette("m1-statistical-foundations", package = "sitemix")}.
#'
#' The examples draw on [prek_sim], a fully simulated panel of 50
#' pre-kindergarten sites; the complete case study uses this simulated panel.
#' A corresponding count table is available in
#' \code{inst/extdata/prek_sim_counts.rds}; see [prek_sim] for the data
#' description and file access examples.
#'
#' @author
#' \strong{Author and maintainer}: JoonHo Lee \email{jlee296@@ua.edu}
#' (ORCID: \href{https://orcid.org/0009-0006-4019-8703}{0009-0006-4019-8703}),
#' Assistant Professor, The University of Alabama.
#'
#' @seealso
#' \itemize{
#'   \item [sm_estimate()] for estimation options and returned columns.
#'   \item [sm_vcov()] for covariance matrices and their scale information.
#'   \item [sm_frechet_envelope()] for D1 pairwise intervals and projected
#'     stress scenarios, whose interpretation depends on the sampling regime.
#'   \item [sm_smooth_variance()] for experimental variance smoothing that
#'     appends alternative standard errors by default.
#' }
#'
#' Repository: \url{https://github.com/joonho112/sitemix};
#' Issues: \url{https://github.com/joonho112/sitemix/issues}.
#'
#' @keywords internal
"_PACKAGE"
