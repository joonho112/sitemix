# ==============================================================================
# Analysis for the v5 SoftwareX revision (sitemix 0.3.1).
# Recomputes every number that appears in the text and figures, saves the
# objects the figure script consumes, and writes a name/value table that the
# reproduce script checks against recorded manuscript values.
#
# All computations are closed-form and deterministic.
# Run:  Rscript scripts/analysis.R   (from the manuscript folder root)
# ==============================================================================

dir_out <- normalizePath(".")
if (!dir.exists(file.path(dir_out, "scripts"))) {
  stop("Run from the manuscript folder root.")
}

if (dir.exists("qa/r-library")) .libPaths(c(normalizePath("qa/r-library"), .libPaths()))
suppressPackageStartupMessages(library(sitemix))
stopifnot(packageVersion("sitemix") == "0.3.1")
if (dir.exists("qa/r-library/sitemix")) {
  stopifnot(normalizePath(find.package("sitemix")) ==
              normalizePath("qa/r-library/sitemix"))
}

data(prek_sim, package = "sitemix")
a24 <- subset(prek_sim, year == 2024)
num <- list()  # name -> value registry for the audit table

## --- Panel facts -------------------------------------------------------------
info <- attr(prek_sim, "build_info")
cells <- as.data.frame(table(prek_sim$site_id, prek_sim$year))
num$panel_rows        <- nrow(prek_sim)
num$panel_sites       <- length(unique(prek_sim$site_id))
num$panel_years       <- length(unique(prek_sim$year))
num$panel_cells       <- nrow(cells)
num$cell_n_min        <- min(cells$Freq)
num$cell_n_max        <- max(cells$Freq)
num$tanf_zero_cells   <- info$panel_summary$tanf_zero_cells
num$cells_under_10    <- info$panel_summary$cells_under_10

## --- Scenario A (2024, frpm) -------------------------------------------------
est <- sm_estimate(a24, family = "binomial", indicator = "frpm")
e24 <- as.data.frame(est)
num$a_cells          <- nrow(e24)
num$median_n_2024    <- median(e24$n)
num$boundary_2024    <- sum(e24$flag_zero_cell)
num$s001_theta_raw   <- round(e24$theta_raw[e24$site_id == "S001"], 3)
num$s001_se_raw      <- round(e24$se_raw[e24$site_id == "S001"], 3)
num$s001_n           <- e24$n[e24$site_id == "S001"]

## Interval-overlap fact for Figure 2
# Illustrative symmetric normal intervals, clipped to [0,1]; not package CIs.
e24$lo <- pmax(0, e24$theta_raw - 1.96 * e24$se_raw)
e24$hi <- pmin(1, e24$theta_raw + 1.96 * e24$se_raw)
top <- e24[which.max(e24$theta_raw), ]
num$top_site        <- top$site_id
num$top_p           <- round(top$theta_raw, 2)
num$top_overlap     <- sum(e24$hi >= top$lo & e24$site_id != top$site_id)
num$other_sites     <- nrow(e24) - 1

## --- All-years Scenario A (Figure 3) ----------------------------------------
est_all <- sm_estimate(prek_sim, family = "binomial", indicator = "frpm")
eall <- as.data.frame(est_all)
num$all_cells        <- nrow(eall)
num$boundary_all     <- sum(eall$flag_zero_cell)

## --- Scenario B: joint covariance -------------------------------------------
est_mv <- sm_estimate(a24, family = "multivariate",
                      indicators = c("frpm", "snap", "wic", "tanf"),
                      vjt = TRUE)
v_s001 <- est_mv$V[[which(est_mv$site_id == "S001")[1]]]
v_s028 <- est_mv$V[[which(est_mv$site_id == "S028")[1]]]
m001 <- as.matrix(v_s001); m028 <- as.matrix(v_s028)
num$s001_cov_frpm_snap <- round(m001["frpm", "snap"], 6)
num$s028_cov_frpm_snap <- round(m028["frpm", "snap"], 6)
num$s028_n             <- est_mv$n[est_mv$site_id == "S028"][1]

## Worked SUR arithmetic for OSM Appendix A (S001)
p_f <- mean(a24$frpm[a24$site_id == "S001"])
p_s <- mean(a24$snap[a24$site_id == "S001"])
p_fs <- mean(a24$frpm[a24$site_id == "S001"] * a24$snap[a24$site_id == "S001"])
n_s001 <- sum(a24$site_id == "S001")
num$s001_p_frpm  <- round(p_f, 4)
num$s001_p_snap  <- round(p_s, 4)
num$s001_p_joint <- round(p_fs, 4)
sur_check <- (p_fs - p_f * p_s) / n_s001
stopifnot(abs(sur_check - m001["frpm", "snap"]) < 1e-9)
num$s001_sur_check <- round(sur_check, 6)

## --- Scenario C --------------------------------------------------------------
al <- a24
k_ind <- al$frpm + al$snap + al$wic + al$tanf
al$support_tier <- ifelse(k_ind == 0, "none",
                          ifelse(k_ind <= 2, "one_two", "three_plus"))
est_c <- sm_estimate(al, family = "multinomial",
                     indicator = "support_tier", vjt = TRUE)
num$c_s001_rank    <- est_c$V[[1]]$matrix_rank
num$c_s001_support <- est_c$V[[1]]$positive_support

## --- Scenario D0: equivalence ------------------------------------------------
agg <- aggregate(frpm ~ site_id + year, data = prek_sim,
                 FUN = function(x) c(num = sum(x), den = length(x)))
agg <- data.frame(site_id = agg$site_id, year = agg$year,
                  c_jt_frpm = agg$frpm[, "num"], n_jt = agg$frpm[, "den"])
est_d0_all <- sm_estimate_from_aggregates(agg, family = "binomial",
                                          indicator = "frpm")
d0 <- as.data.frame(est_d0_all)
key <- function(d) paste(d$site_id, d$year)
d0 <- d0[order(key(d0)), ]; ea <- eall[order(key(eall)), ]
stopifnot(identical(key(d0), key(ea)))
num$d0_max_abs_diff <- max(abs(d0$theta_hat - ea$theta_hat),
                           abs(d0$se - ea$se),
                           abs(d0$theta_raw - ea$theta_raw),
                           abs(d0$se_raw - ea$se_raw))
stopifnot(num$d0_max_abs_diff == 0)

## --- Scenario D1 + Frechet ---------------------------------------------------
agg_list <- lapply(c("frpm", "snap", "wic", "tanf"), function(v) {
  s <- aggregate(a24[[v]], by = list(site_id = a24$site_id, year = a24$year),
                 FUN = sum)
  n <- aggregate(a24[[v]], by = list(site_id = a24$site_id, year = a24$year),
                 FUN = length)
  data.frame(site_id = s$site_id, year = s$year, indicator = v,
             c_jt = s$x, n_jt = n$x)
})
d1_long <- do.call(rbind, agg_list)
est_d1 <- suppressWarnings(sm_estimate_from_aggregates(
  d1_long, family = "multivariate", indicator_col = "indicator",
  sampling_relation = "same_units", vjt = TRUE
))
env <- sm_frechet_envelope(est_d1, population_regime = "d1a")
rpi <- as.data.frame(env$raw_pairwise_intervals)
fs <- rpi[rpi$indicator_1 == "frpm" & rpi$indicator_2 == "snap",
          c("site_id", "p_1", "p_2", "n_common",
            "joint_probability_lower", "joint_probability_upper")]
joint_obs <- aggregate(I(a24$frpm * a24$snap),
                       by = list(site_id = a24$site_id), FUN = mean)
names(joint_obs) <- c("site_id", "joint_obs")
fs <- merge(fs, joint_obs, by = "site_id")
inside <- fs$joint_obs >= fs$joint_probability_lower - 1e-12 &
          fs$joint_obs <= fs$joint_probability_upper + 1e-12
at_edge <- abs(fs$joint_obs - fs$joint_probability_lower) < 1e-12 |
           abs(fs$joint_obs - fs$joint_probability_upper) < 1e-12
stopifnot(all(inside))
num$frechet_sites    <- nrow(fs)
num$frechet_at_edge  <- sum(at_edge)

## --- EB demonstration --------------------------------------------------------
d <- as.data.frame(est)
d <- d[is.finite(d$theta_hat) & is.finite(d$se) & d$se > 0, ]
tau2 <- max(0, var(d$theta_hat) - mean(d$se^2))
mu   <- weighted.mean(d$theta_hat, 1 / (tau2 + d$se^2))
lam  <- tau2 / (tau2 + d$se^2)
d$theta_eb <- mu + lam * (d$theta_hat - mu)
d$p_raw <- sin(d$theta_hat)^2
# Inverse transform of the arcsine posterior mean, not E[p | data].
d$p_eb  <- sin(d$theta_eb)^2
d$rank_raw <- rank(-d$p_raw, ties.method = "first")
d$rank_eb  <- rank(-d$p_eb,  ties.method = "first")
num$tau2      <- round(tau2, 4)
num$mu        <- round(mu, 3)
num$lam_min   <- round(min(lam), 2)
num$lam_max   <- round(max(lam), 2)
num$spearman  <- round(cor(d$rank_raw, d$rank_eb, method = "spearman"), 3)
num$top10_stay <- sum(d$site_id[d$rank_raw <= 10] %in%
                      d$site_id[d$rank_eb <= 10])
leavers <- d[d$rank_raw <= 10 & d$rank_eb > 10, ]
leavers <- leavers[order(leavers$rank_raw), ]
num$leaver_1 <- paste0(leavers$site_id[1], ":", leavers$rank_raw[1],
                       ">", leavers$rank_eb[1])
num$leaver_2 <- paste0(leavers$site_id[2], ":", leavers$rank_raw[2],
                       ">", leavers$rank_eb[2])
mv3 <- d[order(-abs(d$rank_raw - d$rank_eb)), ][1:3, ]
num$mover_top <- paste0(mv3$site_id[1], ":", mv3$rank_raw[1],
                        ">", mv3$rank_eb[1])
## Site sizes behind the rank movers: the manuscript describes each mover by
## its enrollment, so the descriptor is audited alongside the rank move.
num$leaver_1_n  <- leavers$n[1]
num$leaver_2_n  <- leavers$n[2]
num$mover_top_n <- mv3$n[1]
num$grand_mean_p <- round(sin(mu)^2, 3)

## --- Persist -----------------------------------------------------------------
saveRDS(list(e24 = e24, eall = eall, est_mv_s028 = m028, fs = fs, eb = d,
             tau2 = tau2, mu = mu, top = top, leavers = leavers,
             cells = cells, prek_sim = as.data.frame(prek_sim)),
        file.path(dir_out, "data", "results.rds"))
audit <- data.frame(name = names(num),
                    value = vapply(num, function(x) paste(x, collapse = ";"),
                                   character(1)))
write.csv(audit, file.path(dir_out, "data", "numbers.csv"), row.names = FALSE)
print(audit, row.names = FALSE)
cat("\nANALYSIS-OK\n")
