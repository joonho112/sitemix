#' Simulated pre-kindergarten site panel
#'
#' A fully simulated 50-site panel of pre-kindergarten enrollment records with
#' four overlapping binary means-test indicators across five school years. The
#' panel is the worked example used throughout the package documentation and in
#' the regression tests.
#'
#' @format A tibble with 7,845 rows and 7 columns:
#' \describe{
#'   \item{student_id}{Generated `STxxxxx` identifier; one per row.}
#'   \item{site_id}{Generated site identifier, `"S001"` through `"S050"`.}
#'   \item{year}{School year, 2021 through 2025.}
#'   \item{frpm}{Integer 0/1. Free and reduced-price meals eligibility.}
#'   \item{snap}{Integer 0/1. SNAP enrollment.}
#'   \item{wic}{Integer 0/1. WIC enrollment.}
#'   \item{tanf}{Integer 0/1. TANF enrollment.}
#' }
#'
#' @details
#' No administrative, restricted, or person-level source data of any kind is
#' read by the builder or represented in the shipped artifacts. Every row is
#' generated from design constants recorded in
#' `inst/scripts/build-prek-sim.R`, so the panel can be regenerated from a
#' plain R installation with no special data access.
#'
#' The design targets are round numbers chosen to give the package a didactic
#' example with the features its estimators are built for: site-year cells
#' ranging from a handful of children to more than a hundred, four overlapping
#' means-tested indicators, one rare indicator that produces many zero cells,
#' and enough between-site heterogeneity for a meaningful empirical-Bayes
#' demonstration. They are not estimates of any particular program's caseload.
#'
#' Sites are drawn in three size strata; each site carries a vector of
#' correlated random effects on the logit scale, so high-need sites are
#' high-need on every indicator at once; and within a site-year cell the four
#' indicators are drawn from a Gaussian copula thresholded at that cell's
#' indicator probabilities. Design targets, calibrated latent parameters, and a
#' summary of the realized panel are stored under the `build_info` attribute.
#'
#' The same builder generates two external artifacts:
#' \itemize{
#'   \item `inst/extdata/prek_sim.csv` for non-R consumers.
#'   \item `inst/extdata/prek_sim_counts.rds` for pre-aggregated
#'     multivariate sufficient counts.
#' }
#' Access both artifacts with [system.file()].
#'
#' @source
#' Simulated by `inst/scripts/build-prek-sim.R`. See
#' `inst/extdata/prek_sim_design.txt` for the generative model and the
#' realized panel summary.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=sm_estimate]{sm_estimate()}} for the primary consumer.
#'   \item \code{\link[=sm_estimate_from_counts]{sm_estimate_from_counts()}}
#'     for the bundled `prek_sim_counts.rds` consumer.
#'   \item \code{vignette("a1-getting-started")} for the applied tutorial.
#' }
#'
#' @examples
#' data(prek_sim)
#' attr(prek_sim, "build_info")$row_count
#'
#' counts_path <- system.file(
#'   "extdata",
#'   "prek_sim_counts.rds",
#'   package = "sitemix"
#' )
#' counts <- readRDS(counts_path)
#' head(counts)
#'
#' one_year <- subset(prek_sim, year == 2024)
#' out <- sm_estimate(
#'   one_year,
#'   family = "multivariate",
#'   indicators = c("frpm", "snap", "wic", "tanf")
#' )
#' head(out)
#'
#' @docType data
#' @keywords datasets
#' @family datasets
#' @name prek_sim
"prek_sim"
