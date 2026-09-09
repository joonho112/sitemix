#' Simulated pre-kindergarten site panel
#'
#' Simulated enrollment records for 50 pre-kindergarten sites from 2021 through
#' 2025. Each row represents one simulated student and contains four overlapping
#' binary indicators of program eligibility or participation. The panel is used
#' in the package examples and regression tests; it represents no real students
#' or sites.
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
#' `inst/scripts/build-prek-sim.R` generates every row from design constants
#' without reading external records. The chosen parameters produce different
#' site sizes, correlated indicators, and many zero-count TANF cells. They
#' illustrate estimation with sparse and overlapping indicators and are not
#' estimates of any program's caseload.
#'
#' Sites have three size strata, with enrollment varying by year around each
#' site's typical size. Correlated site effects on the logit scale and a
#' Gaussian copula generate associations among the indicators. The
#' `build_info` attribute records the design targets, calibrated parameters,
#' and a summary of the resulting panel.
#'
#' The package also includes two files generated from the same panel:
#' \itemize{
#'   \item `inst/extdata/prek_sim.csv`, a CSV copy of the student rows.
#'   \item `inst/extdata/prek_sim_counts.rds`, a table of site-year
#'     denominators, marginal counts, and pairwise co-occurrence counts.
#' }
#' Locate either file with [system.file()].
#'
#' @source
#' Simulated by `inst/scripts/build-prek-sim.R`. See
#' `inst/extdata/prek_sim_design.txt` for the generative model and the
#' realized panel summary.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=sm_estimate]{sm_estimate()}} for estimating site-level
#'     proportions from the student rows.
#'   \item \code{\link[=sm_estimate_from_counts]{sm_estimate_from_counts()}}
#'     for estimation from the bundled count table.
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
