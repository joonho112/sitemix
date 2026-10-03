# ==============================================================================
# Figures for the v5 SoftwareX revision. Consumes data/results.rds written by
# scripts/analysis.R; writes main-text PDFs into figures/ and panel PDF into osm/.
#
# House style (paper-C / P01 / Geometry): minimal theme, palette
# #2166ac (blue) / #e08214 (orange) / #b2182b (red), bold (a)/(b) panel tags,
# direct labels in place of legends where possible, percent axes for proportions.
# ==============================================================================

if (dir.exists("qa/r-library")) .libPaths(c(normalizePath("qa/r-library"), .libPaths()))
suppressPackageStartupMessages({
  library(sitemix)
  library(ggplot2)
  library(patchwork)
  library(scales)
})

# Optional file stems select a subset; no arguments rebuild all figures.
figure_targets <- commandArgs(trailingOnly = TRUE)
save_revision_figure <- function(filename, plot, ...) {
  stem <- tools::file_path_sans_ext(basename(filename))
  if (length(figure_targets) && !stem %in% figure_targets) return(invisible(NULL))
  if (stem == "fig_panel") filename <- "osm/fig_panel.pdf"
  device <- if (Sys.info()[["sysname"]] == "Darwin") {
    function(filename, width, height, ...) {
      grDevices::quartz(type = "pdf", file = filename, width = width,
                       height = height, ...)
    }
  } else grDevices::cairo_pdf
  ggplot2::ggsave(filename, plot, device = device, ...)
  stopifnot(file.exists(filename), file.info(filename)$size > 0)
}
stopifnot(packageVersion("sitemix") == "0.3.1")
if (dir.exists("qa/r-library/sitemix")) {
  stopifnot(normalizePath(find.package("sitemix")) ==
              normalizePath("qa/r-library/sitemix"))
}
res <- readRDS("data/results.rds")
e24  <- res$e24
eall <- res$eall
fs   <- res$fs
eb   <- res$eb
mu   <- res$mu

BLUE   <- "#2166ac"
ORANGE <- "#e08214"
RED    <- "#b2182b"
GREY   <- "#4d4d4d"

theme_smx <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, color = "grey88"),
      axis.title = element_text(size = base_size * 0.95),
      axis.text  = element_text(color = "grey25"),
      plot.tag = element_text(face = "bold", size = base_size + 1),
      plot.tag.position = c(0.005, 0.995),
      legend.position = "none",
      plot.margin = margin(4, 8, 4, 4)
    )
}

## ============================================================================
## Figure 2: the uncertainty blind spot
## ============================================================================
e <- e24[order(e24$theta_raw), ]
e$pos <- seq_len(nrow(e))
e$grp <- ifelse(e$n < 10, "small", "large")
top <- e[which.max(e$theta_raw), ]

pal_n <- c(large = BLUE, small = RED)

p2a <- ggplot(e, aes(pos, theta_raw, color = grp)) +
  geom_point(size = 1.5) +
  scale_color_manual(values = pal_n) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1.02), expand = c(0, 0)) +
  labs(tag = "(a)", x = "sites, ranked by 2024 FRPM proportion",
       y = "FRPM proportion") +
  annotate("text", x = 4, y = 0.97, hjust = 0, size = 3, color = RED,
           label = "red: fewer than 10 children") +
  theme_smx() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

p2b <- ggplot(e, aes(pos, theta_raw, color = grp)) +
  geom_linerange(aes(ymin = lo, ymax = hi), linewidth = 0.45, alpha = 0.85) +
  geom_point(size = 1.5) +
  scale_color_manual(values = pal_n) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 1.02), expand = c(0, 0)) +
  labs(tag = "(b)", x = "sites, ranked by 2024 FRPM proportion",
       y = NULL) +
  annotate("segment", x = 38.5, xend = top$pos - 0.4,
           y = 0.175, yend = top$lo - 0.015, linewidth = 0.3, color = GREY) +
  annotate("text", x = 17, y = 0.07, hjust = 0, vjust = 0.5,
           size = 3, color = GREY, lineheight = 0.95,
           label = "the interval of the top-ranked site\noverlaps 23 of the other 49") +
  theme_smx() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_blank())

fig2 <- p2a + p2b
save_revision_figure("figures/fig_blindspot.pdf", fig2, width = 7.0, height = 3.1)

## ============================================================================
## Figure 3: two reported scales
## ============================================================================
b <- eall[eall$flag_zero_cell, ]

p3a <- ggplot(eall, aes(n, se_raw, color = theta_raw)) +
  geom_point(size = 1.25, alpha = 0.85) +
  geom_point(data = b, size = 2.3, shape = 21, stroke = 0.7,
             color = RED, fill = NA) +
  scale_color_gradient(low = BLUE, high = ORANGE,
                       labels = percent_format(accuracy = 1)) +
  scale_x_continuous(trans = "log2", breaks = c(8, 16, 32, 64, 128)) +
  annotate("text", x = 6.1, y = 0.027, hjust = 0, vjust = 0, size = 3, color = RED,
           lineheight = 0.95,
           label = "proportion 0 (circled):\nWilson boundary surrogate") +
  labs(tag = "(a)", x = "site-year enrollment (log scale)",
       y = "standard error, probability scale",
       color = "FRPM proportion") +
  theme_smx() +
  theme(legend.position = c(0.98, 0.98), legend.justification = c(1, 1),
        legend.key.height = unit(9, "pt"), legend.key.width = unit(9, "pt"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        legend.background = element_rect(fill = alpha("white", 0.7),
                                         color = NA))

curve_df <- data.frame(n = exp(seq(log(min(eall$n)), log(max(eall$n)),
                                   length.out = 200)))
curve_df$se <- 1 / (2 * sqrt(curve_df$n))

p3b <- ggplot(eall, aes(n, se)) +
  geom_line(data = curve_df, aes(n, se), color = GREY,
            linetype = "22", linewidth = 0.45) +
  geom_point(size = 1.25, alpha = 0.8, color = BLUE) +
  scale_x_continuous(trans = "log2", breaks = c(8, 16, 32, 64, 128)) +
  annotate("text", x = 40, y = 1 / (2 * sqrt(24)), hjust = 0, size = 3.1,
           color = GREY, label = "1/(2*sqrt(n))", parse = TRUE) +
  annotate("text", x = 6.1, y = 0.055, hjust = 0, size = 3, color = GREY,
           lineheight = 0.95,
           label = "all 250 cells follow the\nworking SE rule, including\nboundary cells") +
  labs(tag = "(b)", x = "site-year enrollment (log scale)",
       y = "standard error, arcsine scale") +
  theme_smx()

fig3 <- p3a + p3b
save_revision_figure("figures/fig_two_scales.pdf", fig3, width = 7.0, height = 3.1)

## ============================================================================
## Figure 4: what published tables preserve and what they only constrain
## ============================================================================
## (a) D0 identity: estimates from records vs from published counts
d0 <- data.frame(records = eall$theta_raw)
## recompute the aggregate-path estimates directly for the plot
data(prek_sim, package = "sitemix")
agg <- aggregate(frpm ~ site_id + year, data = prek_sim,
                 FUN = function(x) c(num = sum(x), den = length(x)))
agg <- data.frame(site_id = agg$site_id, year = agg$year,
                  c_jt_frpm = agg$frpm[, "num"], n_jt = agg$frpm[, "den"])
est_d0 <- sm_estimate_from_aggregates(agg, family = "binomial",
                                      indicator = "frpm")
dd0 <- merge(as.data.frame(est_d0)[, c("site_id", "year", "theta_raw")],
             eall[, c("site_id", "year", "theta_raw")],
             by = c("site_id", "year"), suffixes = c("_agg", "_rec"))

p4a <- ggplot(dd0, aes(theta_raw_rec, theta_raw_agg)) +
  geom_abline(color = GREY, linetype = "22", linewidth = 0.4) +
  geom_point(size = 1.3, alpha = 0.7, color = BLUE) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_equal() +
  annotate("text", x = 0.03, y = 0.93, hjust = 0, size = 3, color = GREY,
           lineheight = 0.95,
           label = "250 site-year cells\nmaximum difference: zero") +
  labs(tag = "(a)", x = "estimate from individual records",
       y = "estimate from published counts") +
  theme_smx()

## (b) Frechet intervals with the record-level joint share
f <- fs[order(fs$joint_obs, fs$site_id), ]
f$pos <- seq_len(nrow(f))
f$edge <- abs(f$joint_obs - f$joint_probability_lower) < 1e-12 |
          abs(f$joint_obs - f$joint_probability_upper) < 1e-12
edge_hi <- f[f$edge & f$pos > 40, ][1, ]

p4b <- ggplot(f, aes(pos)) +
  geom_linerange(aes(ymin = joint_probability_lower,
                     ymax = joint_probability_upper),
                 color = BLUE, alpha = 0.35, linewidth = 1.7) +
  geom_point(aes(y = joint_obs), color = RED, size = 1.3) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  annotate("segment", x = 16.5, xend = edge_hi$pos - 0.7,
           y = 0.555, yend = edge_hi$joint_obs + 0.012,
           linewidth = 0.3, color = GREY) +
  annotate("text", x = 1, y = 0.575, hjust = 0, vjust = 0, size = 3,
           color = GREY, lineheight = 0.95,
           label = sprintf("at %d of 50 sites the record-level\nshare lies on an endpoint",
                           sum(f$edge))) +
  labs(tag = "(b)", x = "sites, ordered by record-level joint share",
       y = "share with both FRPM and SNAP") +
  theme_smx() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

fig4 <- p4a + p4b + plot_layout(widths = c(1, 1.25))
save_revision_figure("figures/fig_aggregates.pdf", fig4, width = 7.0, height = 3.2)

## ============================================================================
## Figure 5: the downstream EB step
## ============================================================================
gm <- sin(mu)^2

p5a <- ggplot(eb, aes(x = n)) +
  geom_hline(yintercept = gm, linetype = "22", color = GREY,
             linewidth = 0.4) +
  geom_segment(aes(xend = n, y = p_raw, yend = p_eb), color = "grey62",
               linewidth = 0.35,
               arrow = arrow(length = unit(1.2, "mm"))) +
  geom_point(aes(y = p_raw), color = BLUE, size = 1.3) +
  geom_point(aes(y = p_eb), color = ORANGE, size = 0.9) +
  scale_x_continuous(trans = "log2", breaks = c(8, 16, 32, 64, 128),
                     limits = c(7, 150)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  annotate("segment", x = 135, xend = 135, y = gm + 0.14, yend = gm + 0.008,
           linewidth = 0.3, color = GREY,
           arrow = arrow(length = unit(1.2, "mm"))) +
  annotate("label", x = 145, y = gm + 0.185, hjust = 1, size = 2.7,
           color = GREY, fill = "white", alpha = 0.85, linewidth = 0,
           label.padding = unit(0.12, "lines"),
           label = "back-transformed\nfitted center") +
  annotate("text", x = 145, y = 0.86, hjust = 1, size = 2.8, lineheight = 1.05,
           color = GREY,
           label = "blue: observed proportion\norange: back-transformed EB") +
  labs(tag = "(a)", x = "site enrollment (log scale)",
       y = "FRPM proportion") +
  theme_smx()

## (b) rank slopegraph
s <- eb[, c("site_id", "n", "rank_raw", "rank_eb")]
movers <- res$leavers$site_id
big    <- "S001"
s$grp <- ifelse(s$site_id %in% movers, "leaver",
                ifelse(s$site_id == big, "big", "other"))
sl <- rbind(
  data.frame(site_id = s$site_id, x = 0, rank = s$rank_raw, grp = s$grp),
  data.frame(site_id = s$site_id, x = 1, rank = s$rank_eb,  grp = s$grp)
)

p5b <- ggplot() +
  annotate("rect", xmin = -0.03, xmax = 1.03, ymin = 0.5, ymax = 10.5,
           fill = ORANGE, alpha = 0.12) +
  geom_line(data = sl[sl$grp == "other", ],
            aes(x, rank, group = site_id), color = "grey78",
            linewidth = 0.3) +
  geom_line(data = sl[sl$grp == "big", ],
            aes(x, rank, group = site_id), color = BLUE, linewidth = 0.6) +
  geom_line(data = sl[sl$grp == "leaver", ],
            aes(x, rank, group = site_id), color = RED, linewidth = 0.6) +
  geom_point(data = sl[sl$grp != "other", ],
             aes(x, rank, color = grp), size = 1.5) +
  scale_color_manual(values = c(leaver = RED, big = BLUE)) +
  scale_y_reverse(breaks = c(1, 10, 20, 30, 40, 50)) +
  scale_x_continuous(breaks = c(0, 1), labels = c("raw rank", "EB rank"),
                     limits = c(-0.04, 1.85)) +
  annotate("text", x = 1.07, y = 13.8, hjust = 0, size = 3, color = RED,
           lineheight = 1.05,
           label = "S031: 8th to 13th\nS014: 10th to 14th") +
  annotate("text", x = 1.07, y = 44.5, hjust = 0, size = 3, color = BLUE,
           lineheight = 1.05, label = "S001 (9 children):\n50th to 45th") +
  annotate("text", x = 0.02, y = 2.6, hjust = 0, size = 3,
           color = "#b36a10", label = "top ten") +
  labs(tag = "(b)", x = NULL, y = "rank (1 = highest FRPM proportion)") +
  theme_smx() +
  theme(panel.grid.major.x = element_blank())

fig5 <- p5a + p5b
save_revision_figure("figures/fig_eb.pdf", fig5, width = 7.0, height = 3.3)

## ============================================================================
## Appendix figure C.1: the simulated panel
## ============================================================================
cells <- res$cells
names(cells) <- c("site_id", "year", "n")

pC1a <- ggplot(cells, aes(n)) +
  geom_histogram(binwidth = 6, boundary = 0, fill = BLUE, color = "white",
                 linewidth = 0.25) +
  annotate("text", x = 60, y = 52, hjust = 0, size = 3, color = GREY,
           lineheight = 0.95,
           label = "250 site-year cells\nn from 6 to 134, median 24") +
  labs(tag = "(a)", x = "children per site-year cell", y = "cells") +
  theme_smx()

pp <- res$prek_sim
yr <- do.call(rbind, lapply(c("frpm", "snap", "wic", "tanf"), function(v) {
  a <- aggregate(pp[[v]], by = list(year = pp$year), FUN = mean)
  data.frame(year = a$year, rate = a$x, indicator = toupper(v))
}))
lab <- yr[yr$year == max(yr$year), ]
pal4 <- c(FRPM = BLUE, SNAP = ORANGE, WIC = GREY, TANF = RED)

pC1b <- ggplot(yr, aes(year, rate, color = indicator)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.4) +
  geom_text(data = lab, aes(label = indicator), hjust = 0, nudge_x = 0.12,
            size = 3, fontface = "bold") +
  scale_color_manual(values = pal4) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 0.5)) +
  scale_x_continuous(breaks = 2021:2025, limits = c(2021, 2025.8)) +
  labs(tag = "(b)", x = "year", y = "panel-wide indicator proportion") +
  theme_smx()

figC1 <- pC1a + pC1b
save_revision_figure("figures/fig_panel.pdf", figC1, width = 7.0, height = 2.9)

cat("FIGURES-OK\n")
