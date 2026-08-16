#!/usr/bin/env Rscript

# Builder for `prek_sim`, the package's simulated example panel.
#
# The panel is FULLY SYNTHETIC. This script reads no external file: every
# quantity it needs is a design constant declared below, so any user can
# regenerate the shipped artifacts byte-for-byte with a plain R installation
# and no restricted data access.
#
# The design targets are round numbers chosen to give the package a didactic
# example with the features its estimators are built for: site-year cells that
# range from a handful of children to more than a hundred, four overlapping
# means-tested indicators, one rare indicator that produces many zero cells,
# and enough between-site heterogeneity for a meaningful empirical-Bayes
# demonstration. They are not estimates of any particular program's caseload.
#
# Usage:
#   Rscript inst/scripts/build-prek-sim.R
#   Rscript inst/scripts/build-prek-sim.R --quick   # skip the seed scan

IND <- c("frpm", "snap", "wic", "tanf")
YEARS <- 2021:2025

# ---------------------------------------------------------------------------
# Design constants
# ---------------------------------------------------------------------------

DESIGN <- list(
  # marginal participation rate for each indicator
  p = c(frpm = 0.40, snap = 0.30, wic = 0.25, tanf = 0.02),

  # between-site spread of the site-level rate, probability scale
  sd_site = c(frpm = 0.12, snap = 0.15, wic = 0.12, tanf = 0.02),

  # phi correlation between indicators within a child
  phi = rbind(
    c(1.00, 0.30, 0.20, 0.08),
    c(0.30, 1.00, 0.30, 0.12),
    c(0.20, 0.30, 1.00, 0.06),
    c(0.08, 0.12, 0.06, 1.00)
  ),

  # correlation of the site random effects across indicators: sites that serve
  # a high-need population tend to do so on every indicator at once
  R_site = rbind(
    c(1.00, 0.75, 0.70, 0.45),
    c(0.75, 1.00, 0.80, 0.65),
    c(0.70, 0.80, 1.00, 0.60),
    c(0.45, 0.65, 0.60, 1.00)
  ),

  # three site-size strata, mirroring how funded classroom counts distribute
  strata = list(
    small  = list(n = 10L, meanlog = log(11.5), sdlog = 0.20),
    medium = list(n = 20L, meanlog = log(18.5), sdlog = 0.25),
    large  = list(n = 20L, meanlog = log(46.0), sdlog = 0.62)
  ),

  year_jitter_sd = 0.16,
  size_floor = 4L,
  size_cap = 140L,
  size_seed = 909L
)
dimnames(DESIGN$phi) <- dimnames(DESIGN$R_site) <- list(IND, IND)

# structural features the shipped panel must exhibit
REQUIRE <- list(
  max_cell_min = 110L,
  n_ge_100_min = 4L,
  min_cell_max = 6L,
  tanf_zero_lo = 150L,
  tanf_zero_hi = 180L
)

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

near_pd <- function(R, eps = 1e-6) {
  e <- eigen(R, symmetric = TRUE)
  if (min(e$values) > eps) {
    return(R)
  }
  e$values[e$values < eps] <- eps
  out <- e$vectors %*% diag(e$values) %*% t(e$vectors)
  d <- sqrt(diag(out))
  out / outer(d, d)
}

site_median_sizes <- function(design = DESIGN) {
  set.seed(design$size_seed)
  unlist(
    lapply(design$strata, function(s) sort(stats::rlnorm(s$n, s$meanlog, s$sdlog))),
    use.names = FALSE
  )
}

cell_counts_matrix <- function(med, seed, design = DESIGN) {
  set.seed(seed)
  ny <- length(YEARS)
  out <- matrix(0L, length(med), ny)
  for (j in seq_along(med)) {
    out[j, ] <- as.integer(pmin(
      design$size_cap,
      pmax(
        design$size_floor,
        round(med[[j]] * exp(stats::rnorm(ny, 0, design$year_jitter_sd)))
      )
    ))
  }
  out
}

generate_panel <- function(mu, sigma, r_lat, seed, med) {
  set.seed(seed)
  ns <- length(med)
  ny <- length(YEARS)
  ni <- length(IND)
  cells <- cell_counts_matrix(med, seed + 5000L)

  l_site <- chol(near_pd(DESIGN$R_site) * outer(sigma, sigma))
  u <- matrix(stats::rnorm(ns * ni), ns, ni) %*% l_site
  colnames(u) <- IND
  l_lat <- chol(near_pd(r_lat))

  parts <- vector("list", ns * ny)
  slot <- 0L
  sid <- 0L
  for (j in seq_len(ns)) {
    for (tt in seq_len(ny)) {
      n_jt <- cells[j, tt]
      thr <- stats::qnorm(1 - stats::plogis(mu + u[j, ]))
      z <- matrix(stats::rnorm(n_jt * ni), n_jt, ni) %*% l_lat
      y <- matrix(0L, n_jt, ni, dimnames = list(NULL, IND))
      for (i in seq_len(ni)) {
        y[, i] <- as.integer(z[, i] > thr[[i]])
      }
      slot <- slot + 1L
      parts[[slot]] <- data.frame(
        student_id = sprintf("ST%05d", sid + seq_len(n_jt)),
        site_id = sprintf("S%03d", j),
        year = as.integer(YEARS[[tt]]),
        y,
        stringsAsFactors = FALSE
      )
      sid <- sid + n_jt
    }
  }
  out <- do.call(rbind, parts)
  row.names(out) <- NULL
  out
}

panel_profile <- function(d) {
  rates <- sapply(IND, function(k) tapply(d[[k]], d$site_id, mean))
  list(
    p = colMeans(d[IND]),
    sd_site = apply(rates, 2, stats::sd),
    phi = stats::cor(d[IND])
  )
}

# ---------------------------------------------------------------------------
# Calibration: solve the latent parameters that realise the design targets
# ---------------------------------------------------------------------------

calibrate <- function(med, seeds = 8001:8012, iters = 22L) {
  mu <- stats::qlogis(DESIGN$p)
  sigma <- DESIGN$sd_site / (DESIGN$p * (1 - DESIGN$p))
  r_lat <- DESIGN$phi

  for (it in seq_len(iters)) {
    profiles <- lapply(seeds, function(s) panel_profile(generate_panel(mu, sigma, r_lat, s, med)))
    p_hat <- rowMeans(sapply(profiles, `[[`, "p"))
    sd_hat <- rowMeans(sapply(profiles, `[[`, "sd_site"))
    phi_hat <- Reduce(`+`, lapply(profiles, `[[`, "phi")) / length(profiles)

    mu <- mu + 0.85 * (stats::qlogis(DESIGN$p) - stats::qlogis(p_hat))
    sigma <- pmin(pmax(sigma * (DESIGN$sd_site / sd_hat)^0.55, 0.05), 3)
    for (a in 1:3) {
      for (b in (a + 1L):4L) {
        step <- 0.75 * (DESIGN$phi[a, b] - phi_hat[a, b])
        r_lat[a, b] <- r_lat[b, a] <- min(0.95, max(-0.5, r_lat[a, b] + step))
      }
    }
    r_lat <- near_pd(r_lat)
  }

  list(
    mu = mu, sigma = sigma, r_lat = r_lat,
    residual = list(
      p = max(abs(p_hat - DESIGN$p)),
      sd_site = max(abs(sd_hat - DESIGN$sd_site)),
      phi = max(abs(phi_hat[upper.tri(phi_hat)] - DESIGN$phi[upper.tri(DESIGN$phi)]))
    )
  )
}

structural_ok <- function(d) {
  n <- as.integer(table(d$site_id, d$year))
  ag <- stats::aggregate(tanf ~ site_id + year, data = d, FUN = sum)
  zero <- sum(ag$tanf == 0L)
  max(n) >= REQUIRE$max_cell_min &&
    sum(n >= 100L) >= REQUIRE$n_ge_100_min &&
    min(n) <= REQUIRE$min_cell_max &&
    zero >= REQUIRE$tanf_zero_lo && zero <= REQUIRE$tanf_zero_hi
}

# ---------------------------------------------------------------------------
# Build
# ---------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
quick <- "--quick" %in% args
root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

option_value <- function(args, name, default) {
  prefix <- paste0("--", name, "=")
  hit <- grep(paste0("^", prefix), args, value = TRUE)
  if (length(hit)) sub(prefix, "", hit[[length(hit)]], fixed = TRUE) else default
}

# Output locations default to the package root. The provenance audit overrides
# them so a replay writes into a fresh temporary tree instead. `--source` is
# accepted and ignored: this builder has no external input.
out_data <- option_value(args, "out-data", file.path(root, "data/prek_sim.rda"))
out_extdata <- option_value(args, "out-extdata", file.path(root, "inst/extdata"))

med <- site_median_sizes()
message("calibrating latent parameters ...")
cal <- calibrate(med)
message(sprintf(
  "  residuals: p=%.5f  sd_site=%.5f  phi=%.5f",
  cal$residual$p, cal$residual$sd_site, cal$residual$phi
))

candidate_seeds <- if (quick) 20260001L else seq.int(20260001L, 20260400L)
chosen <- NA_integer_
for (s in candidate_seeds) {
  d <- generate_panel(cal$mu, cal$sigma, cal$r_lat, s, med)
  if (quick || structural_ok(d)) {
    chosen <- s
    break
  }
}
if (is.na(chosen)) {
  stop("no candidate seed satisfied the structural requirements", call. = FALSE)
}
message(sprintf("selected seed: %d", chosen))

prek_sim <- generate_panel(cal$mu, cal$sigma, cal$r_lat, chosen, med)
if (requireNamespace("tibble", quietly = TRUE)) {
  prek_sim <- tibble::as_tibble(prek_sim)
}

# --- sufficient counts artifact -------------------------------------------
groups <- split(seq_len(nrow(prek_sim)), paste(prek_sim$site_id, prek_sim$year, sep = "\r"))
groups <- groups[order(names(groups))]
pair_index <- utils::combn(IND, 2, simplify = FALSE)
count_rows <- lapply(groups, function(idx) {
  row <- data.frame(
    site_id = prek_sim$site_id[[idx[[1]]]],
    year = as.integer(prek_sim$year[[idx[[1]]]]),
    n_jt = as.integer(length(idx)),
    stringsAsFactors = FALSE
  )
  for (k in IND) {
    row[[paste0("c_jt_", k)]] <- as.integer(sum(prek_sim[[k]][idx]))
  }
  for (pr in pair_index) {
    row[[paste0("c_jt_", pr[[1]], "_", pr[[2]])]] <-
      as.integer(sum(prek_sim[[pr[[1]]]][idx] * prek_sim[[pr[[2]]]][idx]))
  }
  row
})
prek_sim_counts <- do.call(rbind, count_rows)
prek_sim_counts <- prek_sim_counts[order(prek_sim_counts$site_id, prek_sim_counts$year), , drop = FALSE]
row.names(prek_sim_counts) <- NULL
if (requireNamespace("tibble", quietly = TRUE)) {
  prek_sim_counts <- tibble::as_tibble(prek_sim_counts)
}

# --- build metadata --------------------------------------------------------
cell_n <- prek_sim_counts$n_jt
build_info <- list(
  build_script = "inst/scripts/build-prek-sim.R",
  build_script_version = 1L,
  data_kind = "fully_simulated",
  restricted_input_used = FALSE,
  seed = chosen,
  size_seed = DESIGN$size_seed,
  design_targets = list(
    p = as.list(DESIGN$p),
    sd_site = as.list(DESIGN$sd_site),
    phi = DESIGN$phi,
    R_site = DESIGN$R_site
  ),
  calibrated = list(mu = as.list(cal$mu), sigma = as.list(cal$sigma), r_latent = cal$r_lat),
  calibration_residual = cal$residual,
  public_schema = names(prek_sim),
  row_count = nrow(prek_sim),
  site_year_count = nrow(prek_sim_counts),
  count_schema = names(prek_sim_counts),
  panel_summary = list(
    sites = length(unique(prek_sim$site_id)),
    years = length(unique(prek_sim$year)),
    min_cell_n = min(cell_n),
    median_cell_n = stats::median(cell_n),
    max_cell_n = max(cell_n),
    cells_under_10 = sum(cell_n < 10L),
    cells_at_least_100 = sum(cell_n >= 100L),
    tanf_zero_cells = sum(prek_sim_counts$c_jt_tanf == 0L)
  )
)
attr(prek_sim, "build_info") <- build_info
attr(prek_sim_counts, "build_info") <- build_info

# --- assertions ------------------------------------------------------------
stopifnot(
  identical(names(prek_sim), c("student_id", "site_id", "year", IND)),
  length(unique(prek_sim$site_id)) == 50L,
  identical(sort(unique(prek_sim$year)), YEARS),
  nrow(prek_sim_counts) == 250L,
  all(prek_sim_counts$n_jt >= prek_sim_counts$c_jt_frpm),
  all(prek_sim_counts$n_jt >= prek_sim_counts$c_jt_snap),
  all(prek_sim_counts$n_jt >= prek_sim_counts$c_jt_wic),
  all(prek_sim_counts$n_jt >= prek_sim_counts$c_jt_tanf)
)

# --- write -----------------------------------------------------------------
dir.create(dirname(out_data), recursive = TRUE, showWarnings = FALSE)
dir.create(out_extdata, recursive = TRUE, showWarnings = FALSE)

save(prek_sim, file = out_data, compress = "xz")
utils::write.csv(prek_sim, file = file.path(out_extdata, "prek_sim.csv"), row.names = FALSE)
saveRDS(prek_sim_counts, file = file.path(out_extdata, "prek_sim_counts.rds"), version = 2)

design_txt <- c(
  "prek_sim design record",
  "======================",
  "",
  "prek_sim is a fully simulated panel. It is generated entirely from the",
  "design constants recorded below: the builder reads no external data file of",
  "any kind, and no real person or site is represented in any shipped artifact.",
  "The panel can therefore be regenerated by any user from a plain R",
  "installation.",
  "",
  sprintf("Builder: %s (version %d)", build_info$build_script, build_info$build_script_version),
  sprintf("Panel seed: %d", build_info$seed),
  sprintf("Site-size seed: %d", build_info$size_seed),
  "",
  "Generative model:",
  "- 50 sites in three size strata (10 small, 20 medium, 20 large); each site's",
  "  median cell size is drawn from a stratum-specific lognormal.",
  "- Site-year cell sizes vary lognormally around the site median.",
  "- Each site carries a vector of correlated random effects on the logit scale,",
  "  so high-need sites are high-need on every indicator at once.",
  "- Within a cell, the four binary indicators are drawn from a Gaussian copula",
  "  thresholded at the cell's indicator probabilities.",
  "",
  "Design targets:",
  sprintf("- marginal rates: %s", paste(sprintf("%s=%.2f", IND, DESIGN$p), collapse = ", ")),
  sprintf("- between-site sd: %s", paste(sprintf("%s=%.2f", IND, DESIGN$sd_site), collapse = ", ")),
  "- indicator phi correlations and site-effect correlations: see build_info",
  "",
  "Realised panel:",
  sprintf("- rows: %d; sites: %d; years: %d", build_info$row_count,
          build_info$panel_summary$sites, build_info$panel_summary$years),
  sprintf("- site-year cells: %d", build_info$site_year_count),
  sprintf("- cell n: min %d, median %d, max %d", build_info$panel_summary$min_cell_n,
          build_info$panel_summary$median_cell_n, build_info$panel_summary$max_cell_n),
  sprintf("- cells with n < 10: %d; cells with n >= 100: %d",
          build_info$panel_summary$cells_under_10, build_info$panel_summary$cells_at_least_100),
  sprintf("- TANF zero-count cells: %d of %d", build_info$panel_summary$tanf_zero_cells,
          build_info$site_year_count),
  "",
  "Public artifacts:",
  sprintf("- data/prek_sim.rda: %d rows, %d columns", nrow(prek_sim), ncol(prek_sim)),
  sprintf("- inst/extdata/prek_sim.csv: %d rows, %d columns", nrow(prek_sim), ncol(prek_sim)),
  sprintf("- inst/extdata/prek_sim_counts.rds: %d rows, %d columns",
          nrow(prek_sim_counts), ncol(prek_sim_counts))
)
writeLines(design_txt, con = file.path(out_extdata, "prek_sim_design.txt"), useBytes = TRUE)

message("Built prek_sim artifacts:")
message(sprintf("- %s (%d rows)", out_data, nrow(prek_sim)))
message("- inst/extdata/prek_sim.csv")
message("- inst/extdata/prek_sim_counts.rds")
message("- inst/extdata/prek_sim_design.txt")
print(unlist(build_info$panel_summary))
