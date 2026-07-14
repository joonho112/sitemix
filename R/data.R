#' Alabama Pre-K sample panel
#'
#' A deterministic, anonymized 50-site subset of the Alabama First Class Pre-K
#' administrative panel. The data demonstrate four overlapping binary
#' means-test indicators across five school years and are used in package
#' examples and regression tests.
#'
#' @format A tibble with 7,312 rows and 7 columns:
#' \describe{
#'   \item{student_id}{Synthetic `STxxxxx` identifier, stable across years for
#'   the same selected child within this shipped sample build.}
#'   \item{site_id}{Synthetic site identifier, `"S001"` through `"S050"`.}
#'   \item{year}{School year, 2021 through 2025.}
#'   \item{frpm}{Integer 0/1. Free and reduced-price meals eligibility.}
#'   \item{snap}{Integer 0/1. SNAP enrollment.}
#'   \item{wic}{Integer 0/1. WIC enrollment.}
#'   \item{tanf}{Integer 0/1. TANF enrollment.}
#' }
#'
#' @details
#' The restricted source panel is not shipped with the package. Rebuild metadata
#' are stored under the `build_info` attribute of `alprek_subset`.
#' They include the source-file digest, sampling seed, candidate site counts,
#' selected-site digest, public schema, and disclosure-audit summary.
#'
#' The same builder generates two external artifacts:
#' \itemize{
#'   \item `inst/extdata/alprek_subset.csv` for non-R consumers.
#'   \item `inst/extdata/alprek_subset_counts.rds` for pre-aggregated
#'     multivariate sufficient counts.
#' }
#' Access both artifacts with [system.file()].
#'
#' @source
#' Stratified and anonymized subset of the Alabama First Class Pre-K
#' administrative panel. See `inst/scripts/build-alprek-subset.R` and
#' `inst/extdata/alprek_subset_provenance.txt`.
#'
#' @seealso
#' \itemize{
#'   \item \code{\link[=sm_estimate]{sm_estimate()}} for the primary consumer.
#'   \item \code{\link[=sm_estimate_from_counts]{sm_estimate_from_counts()}}
#'     for the bundled `alprek_subset_counts.rds` consumer.
#'   \item \code{vignette("a1-getting-started")} for the applied tutorial.
#' }
#'
#' @examples
#' data(alprek_subset)
#' attr(alprek_subset, "build_info")$row_count
#'
#' counts_path <- system.file(
#'   "extdata",
#'   "alprek_subset_counts.rds",
#'   package = "sitemix"
#' )
#' counts <- readRDS(counts_path)
#' head(counts)
#'
#' one_year <- subset(alprek_subset, year == 2024)
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
#' @name alprek_subset
"alprek_subset"
