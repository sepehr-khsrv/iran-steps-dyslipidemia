# 00_run_all.R : runs the whole analysis, from the survey files in data_raw/ to the tables, figures and results index.
#   Stage 1  R/00_run_stage1.R : province lookup, harmonised analytic file, weights, design checks, standard population.
#   Stage 2  R/00_run_stage2.R : estimates, changes, trends, components, sensitivity analyses, provinces, tables,
#            figures and results index, then the Stage 2b additions (R/20_stage2b_*.R) and the end-of-run checks.
# Usage, from the repository root:  Rscript R/00_run_all.R
# Each stage runs in a fresh R session, as in the original analysis. No step uses random numbers.
# The input files listed in README.md must be in data_raw/. Logs go to output/logs/; session information to output/sessionInfo.txt.
if (!file.exists(file.path("R", "00_run_all.R"))) stop("Run this script from the repository root: Rscript R/00_run_all.R")
for (d in c("output/logs", "output/tables/formatted", "output/figures", "output/rds")) dir.create(d, showWarnings = FALSE, recursive = TRUE)
rscript <- file.path(R.home("bin"), "Rscript")
t0 <- Sys.time()
for (f in c("R/00_run_stage1.R", "R/00_run_stage2.R")) {
  cat("\n######## ", f, " started ", format(Sys.time()), " ########\n", sep = "")
  status <- system2(rscript, f)
  if (status != 0) stop(f, " stopped with exit status ", status, ". See the logs in output/logs/.")
}
# Session information: attach the packages the two stages use, then record R and package versions (no locale or time zone).
suppressMessages({library(survey); library(dplyr); library(tidyr); library(readr); library(tibble); library(readxl); library(haven)
                  library(ggplot2); library(sf); library(patchwork); library(jsonlite)})
for (p in c("ragg", "viridisLite", "ggrepel")) loadNamespace(p)
writeLines(capture.output(print(sessionInfo(), locale = FALSE)), file.path("output", "sessionInfo.txt"))
cat("\nAll stages finished in", format(Sys.time() - t0), "\n")
