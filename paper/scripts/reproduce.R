# ==============================================================================
# Reproduction harness for the v5 SoftwareX revision.
# Recomputes every headline number from the installed sitemix package and
# asserts it against the values printed in the manuscript sources.
#
# Run:  Rscript scripts/reproduce.R      (from the manuscript folder root; exits
#                                         nonzero on any mismatch)
# ==============================================================================

source("scripts/analysis.R")  # recomputes and reprints the audit table

aud <- read.csv("data/numbers.csv", stringsAsFactors = FALSE)
val <- setNames(aud$value, aud$name)

expect <- function(name, want) {
  got <- val[[name]]
  if (!identical(as.character(got), as.character(want))) {
    stop(sprintf("MISMATCH %s: manuscript says %s, analysis says %s",
                 name, want, got), call. = FALSE)
  }
  invisible(TRUE)
}

## Values as printed in the manuscript (sections + appendices + figures)
expect("panel_rows",       7845)
expect("panel_sites",      50)
expect("panel_years",      5)
expect("panel_cells",      250)
expect("cell_n_min",       6)
expect("cell_n_max",       134)
expect("tanf_zero_cells",  157)
expect("cells_under_10",   21)
expect("median_n_2024",    23.5)
expect("boundary_all",     2)
expect("s001_theta_raw",   0.111)
expect("s001_se_raw",      0.105)
expect("s001_n",           9)
expect("top_site",         "S005")
expect("top_p",            0.88)
expect("top_overlap",      23)
expect("other_sites",      49)
expect("s001_cov_frpm_snap", -0.002743)
expect("s028_cov_frpm_snap",  0.0033)
expect("s028_n",           26)
expect("s001_sur_check",   -0.002743)
expect("c_s001_rank",      1)
expect("c_s001_support",   2)
expect("d0_max_abs_diff",  0)
expect("frechet_sites",    50)
expect("frechet_at_edge",  13)
expect("tau2",             0.0206)
expect("mu",               0.715)
expect("lam_min",          0.4)
expect("lam_max",          0.89)
expect("spearman",         0.988)
expect("top10_stay",       8)
expect("leaver_1",         "S031:8>13")
expect("leaver_2",         "S014:10>14")
expect("mover_top",        "S001:50>45")
expect("leaver_1_n",       "12")
expect("leaver_2_n",       "14")
expect("mover_top_n",      "9")

## Version guard; full suite/check/coverage use separate validation drivers.
stopifnot(packageVersion("sitemix") == "0.3.1")

cat("\nREPRODUCE-OK: all asserted numbers match the manuscript.\n")
