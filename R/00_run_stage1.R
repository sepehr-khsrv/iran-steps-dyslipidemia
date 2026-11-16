# 00_run_stage1.R : runs the Stage 1 pipeline in order from a clean session.
W <- "."  # repository root: run from the root folder
for (f in c("00_province_lookup.R", "01_harmonise.R", "01b_reconcile.R", "02_design_checks.R", "03_flow_missingness.R",
            "04_standard_population.R", "05_data_dictionary.R", "06_variable_map.R")) {
  cat("\n==== ", f, " ====\n"); source(file.path(W, "R", f), echo = FALSE, local = new.env())
}
writeLines(capture.output(print(sessionInfo(), locale = FALSE)), file.path(W, "output/logs/sessionInfo_stage1.txt"))
