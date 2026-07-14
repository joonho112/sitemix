#' sitemix: Site- and group-level proportions, rates, and sampling uncertainty
#'
#' @encoding UTF-8
#'
#' @description
#' `sitemix` produces site- and group-level point estimates, standard errors,
#' and optional covariance matrices from student rows, sufficient counts, or
#' published aggregates. The package covers five estimation scenarios and ships diagnostics,
#' publisher-side suppression auditing, optional variance smoothing,
#' and raw pairwise Fréchet intervals with projected stress scenarios for
#' unidentified D1 covariance.
#'
#' @section Scenarios:
#' Five estimation scenarios are dispatched by [sm_estimate()]:
#' \describe{
#'   \item{\strong{Scenario A — binomial}}{One binary indicator per
#'     site-year. Input is student rows via [sm_estimate()] or
#'     sufficient counts via [sm_estimate_from_counts()].
#'     Covariance matrix is \eqn{1 \times 1}{1 x 1}.}
#'   \item{\strong{Scenario B — multivariate}}{Overlapping binary
#'     indicators per site-year with SUR-style covariance. Input is student
#'     rows or complete sufficient counts containing marginal and pairwise
#'     co-occurrence counts.
#'     Covariance is \eqn{K \times K}{K x K}.}
#'   \item{\strong{Scenario C — multinomial}}{Mutually exclusive
#'     categories summing to the denominator. Simplex covariance with
#'     analytic rank \eqn{S - 1}{S - 1}, where \eqn{S}{S} is positive
#'     observed support. Input is student rows or complete category counts,
#'     not published D1 marginals.}
#'   \item{\strong{Scenario D0 — aggregate binomial}}{Published
#'     numerator/denominator per site-year. Dispatch via [sm_estimate()].}
#'   \item{\strong{Scenario D1 — aggregate marginal}}{Multiple
#'     published marginals per site-year with working-independence
#'     covariance. Dispatch via [sm_estimate()]. Use
#'     [sm_frechet_envelope()] for sensitivity.}
#' }
#'
#' @section Entry points:
#' \describe{
#'   \item{[sm_estimate()]}{Main dispatcher for student rows,
#'     counts, and aggregates.}
#'   \item{[sm_estimate_from_counts()]}{Sufficient-counts wrapper.}
#'   \item{[sm_estimate_from_aggregates()]}{Published-aggregates
#'     wrapper with D0 / D1 dispatch.}
#'   \item{[sm_diagnose()]}{Uncertainty audit at three levels
#'     (summary / row / vcov).}
#'   \item{[sm_suppression_report()]}{Publisher-side three-tier
#'     suppression audit.}
#' }
#'
#' @section Vignettes:
#' \strong{Applied track} (a1 — a9) walks through workflows for
#' student rows, sufficient counts, published aggregates,
#' diagnostics, smoothing, downstream workflows, and a real-data case study.
#' Start with \code{vignette("a1-getting-started", package = "sitemix")}.
#'
#' \strong{Method track} (m1 — m8) covers the formal specifications:
#' scalar SE pipelines, SUR and multinomial covariance, aggregate engines,
#' variance smoothing theory, Fréchet pairwise/stress semantics, and the output contract. Start
#' with \code{vignette("m1-statistical-foundations", package = "sitemix")}.
#'
#' @section Bundled data:
#' [alprek_subset] is an anonymized 50-site Alabama Pre-K sample
#' used throughout the documentation and tests. The package also
#' ships \code{inst/extdata/alprek_subset_counts.rds} for
#' sufficient-counts examples.
#'
#' @author
#' \strong{Maintainer}: JoonHo Lee \email{jlee296@@ua.edu}
#' (ORCID: \href{https://orcid.org/0009-0006-4019-8703}{0009-0006-4019-8703}),
#' Assistant Professor, The University of Alabama.
#'
#' @seealso
#' \itemize{
#'   \item [sm_estimate()] for the main dispatcher.
#'   \item [sm_vcov()] for the covariance-object contract.
#'   \item [sm_frechet_envelope()] for D1 dependence sensitivity.
#'   \item [sm_smooth_variance()] for optional smoothing alternatives.
#' }
#'
#' Repository: \url{https://github.com/joonho112/sitemix};
#' Issues: \url{https://github.com/joonho112/sitemix/issues}.
#'
#' @keywords internal
"_PACKAGE"
