# 20_stage2b_index.R : adds the Stage 2b numbers to output/results_index.json and two sections to output/results_summary.md.
# Runs after R/18_results_index.R (which writes both files from scratch) and before R/19_checks.R.
source("R/09_functions.R")  # path relative to the repository root
suppressMessages(library(jsonlite))
sink(file.path(W, "output/logs/20_stage2b_index.log"), split = TRUE)
IDX <- fromJSON(file.path(W, "output/results_index.json"), simplifyVector = FALSE); n0 <- length(IDX)
cb <- readRDS(file.path(RDS, "stage2b_components.rds")); fa <- readRDS(file.path(RDS, "stage2b_fasting.rds")); ds <- readRDS(file.path(RDS, "stage2b_design.rds"))
k <- function(...) gsub("_+", "_", gsub("[^A-Za-z0-9]+", "_", tolower(paste(..., sep = "_"))))
add <- function(key, value, unit, source) {
  stopifnot(length(value) == 1, !key %in% names(IDX), !is.na(value))
  IDX[[key]] <<- list(value = if (is.numeric(value)) unname(signif(value, 8)) else value, unit = unit, source = source)
}
add_ci <- function(stem, est, lower, upper, unit, source, p = NULL, n = NULL, se = NULL) {
  add(k(stem, "est"), est, unit, source); add(k(stem, "lower"), lower, unit, source); add(k(stem, "upper"), upper, unit, source)
  if (!is.null(se)) add(k(stem, "se"), se, unit, source)
  if (!is.null(p)) add(k(stem, "p"), p, "P value", source)
  if (!is.null(n)) add(k(stem, "n"), n, "participants (unweighted)", source)
}
SHORT <- c(dys = "composite", c_tc = "tc", c_ldl = "ldl", c_tg = "tg", c_nonhdl = "nonhdl", c_hdl = "lowhdl")
# ---- task 1 / 3: between-round changes and prevalence ratios (keys e.g. comp_tc_change_2016_2021_all_est) ----
S <- cb$between
for (i in seq_len(nrow(S))) { r <- S[i, ]
  src <- paste0("S_components_between_round.csv", ifelse(r$from == 2016, " / T5_within_lab_2016_2021.csv", ""))
  add_ci(k("comp", SHORT[[r$outcome]], "change", r$from, r$to, r$sex), r$est, r$lower, r$upper, "percentage points", src, p = r$p, se = r$se)
  add_ci(k("comp", SHORT[[r$outcome]], "pr", r$from, r$to, r$sex), r$pr, r$pr_lower, r$pr_upper, "prevalence ratio (categorical-year model)", src, p = r$pr_p)
}
T5 <- cb$t5
for (i in seq_len(nrow(T5))) { r <- T5[i, ]; for (y in c(2016, 2021))
  add_ci(k("t5", SHORT[[r$outcome]], r$sex, y), r[[paste0("est_", y)]], r[[paste0("lower_", y)]], r[[paste0("upper_", y)]], "%", "T5_within_lab_2016_2021.csv", n = r[[paste0("n_", y)]]) }
# ---- task 2: fasting ----
F <- fa$table
fd <- F %>% filter(grepl("^distribution", part))
for (i in seq_len(nrow(fd))) { r <- fd[i, ]; add_ci(k("fasting_2021_pct", r$category), r$est, r$lower, r$upper, "% (weighted)", "S_fasting_2021.csv", n = r$n_category) }
add("fasting_2021_weighted_median_hours", fa$weighted_median_hours, "hours (approximate, parsed records)", "S_fasting_2021.csv")
add("fasting_2016_variable_available", "no (Q 1395 items 165-166 not in the released data)", "text", "docs/decisions_log.md S2b-2")
fp_ <- F %>% filter(!grepl("^distribution", part))
for (i in seq_len(nrow(fp_))) { r <- fp_[i, ]
  smp <- ifelse(r$sample == "all", "all", ifelse(grepl(">= 8", r$sample), "ge8h", "ge12h"))
  if (grepl("^difference", r$part)) add_ci(k("fasting_2021", SHORT[[r$outcome]], r$sex, "diff", smp, "minus_all"), r$est, r$lower, r$upper, "percentage points", "S_fasting_2021.csv", p = r$p)
  else add_ci(k("fasting_2021", SHORT[[r$outcome]], r$sex, smp), r$est, r$lower, r$upper, "%", "S_fasting_2021.csv", n = r$n)
}
# ---- task 4b: PSU counts ----
for (i in seq_len(nrow(ds))) { r <- ds[i, ]
  add(k("design_n_psu_variance", r$round), r$n_psu_variance, "PSUs entering the variance (nest = TRUE)", "S_design_summary.csv")
  add(k("design_n_clusters_unique", r$round), r$n_clusters_unique, "unique cluster ids", "S_design_summary.csv")
  add(k("design_n_clusters_spanning_strata", r$round), r$n_clusters_spanning_strata, "clusters with urban and rural members", "S_design_summary.csv")
  add(k("design_n_lonely_strata", r$round), r$n_lonely_strata, "strata with one PSU", "S_design_summary.csv") }
cat("Keys added:", length(IDX) - n0, "; total:", length(IDX), "\n")
write_json(IDX, file.path(W, "output/results_index.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)

# ---- results_summary.md: two new sections ----
g <- function(key) { stopifnot(key %in% names(IDX)); IDX[[key]]$value }
ci <- function(stem, f = f1) paste0(f(g(paste0(stem, "_est"))), " (", f(g(paste0(stem, "_lower"))), ", ", f(g(paste0(stem, "_upper"))), ")")
f3 <- function(x) formatC(round(x, 3), format = "f", digits = 3)
NAMES <- c(composite = "Composite", tc = "TC >= 200", ldl = "LDL-C >= 130", tg = "TG >= 150", nonhdl = "Non-HDL-C >= 160", lowhdl = "Low HDL-C")
row <- function(o, sx) sprintf("| %s | %s | %s | %s | %s | %s |", NAMES[[o]], c(all = "All", women = "Women", men = "Men")[[sx]],
  ci(k("t5", o, sx, 2016)), ci(k("t5", o, sx, 2021)), ci(k("comp", o, "change_2016_2021", sx), f2), ci(k("comp", o, "pr_2016_2021", sx), f3))
md <- readLines(file.path(W, "output/results_summary.md"))
md <- c(md, "",
"## Within-laboratory change 2016 to 2021",
"",
"2016 used one central laboratory (NCDRC, Tehran; Roche Cobas C311). The local documents do not state the 2021 laboratory, so the label \"within-laboratory\" assumes the same set-up. Standardised prevalence (%), change in percentage points and prevalence ratio (categorical-year model adjusted for age group, sex and area), all with 95% CIs. Source: `T5_within_lab_2016_2021.csv`; consecutive-round changes for every component in `S_components_between_round.csv`.",
"",
"| Outcome | Group | 2016 | 2021 | Change (points) | Prevalence ratio |",
"|---|---|---|---|---|---|",
unlist(lapply(names(NAMES), function(o) sapply(c("all", "women", "men"), function(sx) row(o, sx)))),
"",
paste0("- TG >= 150, TC >= 200, LDL-C >= 130 and non-HDL-C >= 160 all rose between 2016 and 2021: TG >= 150 by ", ci("comp_tg_change_2016_2021_all", f2), " points, TC >= 200 by ", ci("comp_tc_change_2016_2021_all", f2),
       ", LDL-C >= 130 by ", ci("comp_ldl_change_2016_2021_all", f2), ", non-HDL-C >= 160 by ", ci("comp_nonhdl_change_2016_2021_all", f2), " points. Low HDL-C changed little (", ci("comp_lowhdl_change_2016_2021_all", f2),
       "), so the composite rose only ", ci("comp_composite_change_2016_2021_all", f2), " points. In women the composite rose ", ci("comp_composite_change_2016_2021_women", f2),
       " points; in men ", ci("comp_composite_change_2016_2021_men", f2), ", where a fall in low HDL-C (", ci("comp_lowhdl_change_2016_2021_men", f2), ") offset rises in the other components."),
"",
"## Fasting check (2021)",
"",
paste0("Only 2021 records fasting (clock times of the last food and of blood sampling, free text; participants were told to fast 12 hours). 2007 and 2011 have no fasting item; the 2016 items (Q 1395 items 165-166) are not in the released data. In the 2021 analytic sample, ",
       ci("fasting_2021_pct_8_to_12_h"), "% (weighted) had fasted 8 to < 12 h and ", ci("fasting_2021_pct_12_to_20_h"), "% 12 to 20 h; ", ci("fasting_2021_pct_4_to_8_h"), "% had fasted 4 to < 8 h, ",
       ci("fasting_2021_pct_implausible_4_h_or_20_h"), "% had an implausible parsed duration, and ", ci("fasting_2021_pct_unparsable"), "% and ", ci("fasting_2021_pct_not_recorded"),
       "% had unparsable or no times. Weighted median ", g("fasting_2021_weighted_median_hours"), " h."),
"",
paste0("- Restricting to participants with a parsed fast of at least 8 h (n = ", format(g("fasting_2021_tg_all_ge8h_n"), big.mark = ","), ") changed the 2021 standardised prevalence of TG >= 150 from ",
       ci("fasting_2021_tg_all_all"), "% to ", ci("fasting_2021_tg_all_ge8h"), "% (difference ", ci("fasting_2021_tg_all_diff_ge8h_minus_all", f2), " points) and of the composite from ",
       ci("fasting_2021_composite_all_all"), "% to ", ci("fasting_2021_composite_all_ge8h"), "% (difference ", ci("fasting_2021_composite_all_diff_ge8h_minus_all", f2), " points). Restricting to at least 12 h gave differences of ",
       ci("fasting_2021_tg_all_diff_ge12h_minus_all", f2), " (TG) and ", ci("fasting_2021_composite_all_diff_ge12h_minus_all", f2), " (composite) points. Non-fasting samples in 2021 do not explain the 2016 to 2021 rise in TG >= 150."),
"",
"## Methods notes (Stage 2b)",
"",
paste0("- PSUs: clusters with urban and rural members count once in each stratum (nest = TRUE). PSUs used in variance estimation: ", paste(sapply(YEARS, function(y) format(g(k("design_n_psu_variance", y)), big.mark = ",")), collapse = " / "),
       " (2007 / 2011 / 2016 / 2021); unique cluster ids: ", paste(sapply(YEARS, function(y) format(g(k("design_n_clusters_unique", y)), big.mark = ",")), collapse = " / "), ". Source: `S_design_summary.csv`."))
writeLines(md, file.path(W, "output/results_summary.md"))
sink()
