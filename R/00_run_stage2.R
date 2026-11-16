# 00_run_stage2.R : runs the Stage 2 analysis (and the Stage 2b additions, R/20_stage2b_*.R) in order from a clean session.
# R/20_stage2b_index.R appends to the results index and summary written by R/18, so it runs after R/18; R/19_checks.R runs last.
# Usage (from the project folder): Rscript R/00_run_stage2.R
W <- "."  # repository root: run from the root folder
t0 <- Sys.time()
for (f in c("10_design.R", "11_prevalence.R", "12_changes_trends.R", "13_components.R", "14_sensitivity.R",
            "15_provinces.R", "16_tables.R", "17_figures.R", "18_results_index.R",
            "20_stage2b_components.R", "20_stage2b_design.R", "20_stage2b_fasting.R", "20_stage2b_index.R", "19_checks.R")) {
  cat("\n==== ", f, " ", format(Sys.time()), " ====\n")
  source(file.path(W, "R", f), echo = FALSE, local = new.env())
  while (sink.number() > 0) sink()
}
library(survey); library(dplyr); library(ggplot2); library(sf); library(patchwork); library(jsonlite)
writeLines(capture.output(print(sessionInfo(), locale = FALSE)), file.path(W, "output/sessionInfo_stage2.txt"))
# checksums of every Stage 2 table and the results index (figures excluded: TIFF/PNG headers can carry timestamps)
fs <- c(list.files(file.path(W, "output/tables"), pattern = "\\.csv$", recursive = TRUE, full.names = TRUE), file.path(W, "output/results_index.json"), file.path(W, "output/results_summary.md"))
md5 <- tools::md5sum(fs); names(md5) <- sub(paste0(W, "/"), "", names(md5))
writeLines(paste(md5, names(md5)), file.path(W, "output/logs/checksums_stage2.txt"))
cat("\nStage 2 finished in", format(Sys.time() - t0), "\n")
