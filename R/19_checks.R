# 19_checks.R : end-of-run checks. Output: output/logs/checks_stage2.txt (stops on failure).
source("R/09_functions.R")  # path relative to the repository root
suppressMessages(library(jsonlite))
out <- character(); ok <- function(name, cond, detail = "") { out <<- c(out, sprintf("[%s] %s %s", ifelse(cond, "PASS", "FAIL"), name, detail)); cond }
prev <- readRDS(file.path(RDS, "prev_main.rds")); tr <- readRDS(file.path(RDS, "trends.rds")); comp <- readRDS(file.path(RDS, "components.rds"))
sens <- readRDS(file.path(RDS, "sensitivity.rds")); pv <- readRDS(file.path(RDS, "provinces.rds")); L <- load_data()
allp <- bind_rows(prev %>% select(est, lower, upper), comp$est %>% select(est, lower, upper), pv$full %>% filter(!is.na(est)) %>% select(est, lower, upper),
                  sens$a %>% filter(!is.na(type)) %>% select(est, lower, upper),
                  bind_rows(sens$b, sens$c, sens$d, sens$i) %>% filter(part == "prevalence") %>% select(est, lower, upper))
r <- c(
 ok("prevalence CIs contain estimate", all(allp$lower <= allp$est & allp$est <= allp$upper), sprintf("(%d estimates)", nrow(allp))),
 ok("prevalence CIs within 0-100", all(allp$lower >= 0 & allp$upper <= 100)),
 ok("no NaN prevalence", !any(is.nan(allp$est) | is.nan(allp$lower))))
apcs <- bind_rows(tr$apc %>% select(apc, apc_lower, apc_upper), comp$apc %>% select(apc, apc_lower, apc_upper), pv$apc %>% select(apc, apc_lower, apc_upper),
                  bind_rows(sens$b, sens$c, sens$d, sens$f, sens$i) %>% filter(part == "trend") %>% select(apc, apc_lower, apc_upper))
r <- c(r, ok("APC CIs contain APC", all(apcs$apc_lower <= apcs$apc & apcs$apc <= apcs$apc_upper), sprintf("(%d APCs)", nrow(apcs))))
nat <- tr$apc %>% filter(analysis == "national", link == "log")
m <- fit_trend(make_design(L$d), dys ~ year_c + agegrp + sex + area)
r <- c(r, ok("pooled year coefficient reproduces national APC", abs(100 * (exp(coef(m)[["year_c"]]) - 1) - nat$apc) < 1e-10, sprintf("APC = %.4f", nat$apc)))
cmp <- tr$apc %>% filter(analysis != "national", link == "log") %>% select(analysis, level, model, apc) %>% pivot_wider(names_from = model, values_from = apc)
d_int <- max(abs(cmp$`interaction (common covariate effects)` - cmp$`subset (stratified)`)); d_full <- max(abs(cmp$`fully interacted` - cmp$`subset (stratified)`))
r <- c(r, ok("fully interacted model APCs = subset APCs", d_full < 1e-6, sprintf("max diff %.2e", d_full)),
       ok("interaction-model (common covariates) APCs = subset APCs within 0.05", d_int < 0.05, sprintf("max diff %.3f percentage points per year (equal to 1 decimal: %s)", d_int,
          all(round(cmp$`interaction (common covariate effects)`, 1) == round(cmp$`subset (stratified)`, 1)))))
ch <- read_csv(file.path(TAB, "check_standardisation_2021.csv"), show_col_types = FALSE)
r <- c(r, ok("standardised 2021 = hand-computed 16-cell (and women 8-cell) average", all(abs(ch$difference) < 1e-5), sprintf("max diff %.1e", max(abs(ch$difference)))),
       ok("standard population sums to 1", abs(sum(stdpop$proportion) - 1) < 1e-12),
       ok("non-HDL-C criterion adds zero cases", all(L$nonhdl_check$added == 0)))
pf <- pv$full
r <- c(r, ok("Qom 2016 and Alborz 2007 are NA", is.na(pf$est[pf$prov == "Qom" & pf$round == 2016]) & is.na(pf$est[pf$prov == "Alborz" & pf$round == 2007])),
       ok("only Qom 2016, Alborz 2007 and Kurdistan 2007 are NA among provincial estimates", setequal(paste(pf$prov, pf$round)[is.na(pf$est)], c("Qom 2016", "Alborz 2007", "Kurdistan 2007"))))
J <- fromJSON(file.path(W, "output/results_index.json"), simplifyVector = FALSE)
r <- c(r, ok("results_index.json has no NA", !any(sapply(J, function(z) is.null(z$value) || (is.numeric(z$value) && !is.finite(z$value)))), sprintf("(%d keys)", length(J))))
# ---- Stage 2b tables ----
rd <- function(f) read_csv(file.path(TAB, f), show_col_types = FALSE)
cbr <- rd("S_components_between_round.csv"); t5 <- rd("T5_within_lab_2016_2021.csv"); fst <- rd("S_fasting_2021.csv"); dsm <- rd("S_design_summary.csv")
inci <- function(e, l, u) all(l <= e & e <= u)
r <- c(r,
 ok("S_components_between_round: 6 outcomes x 3 groups x 3 periods, no NA", nrow(cbr) == 54 & !anyNA(cbr)),
 ok("S_components_between_round: CIs contain estimates (difference and PR)", inci(cbr$est, cbr$lower, cbr$upper) & inci(cbr$pr, cbr$pr_lower, cbr$pr_upper)),
 ok("S_components_between_round: differences equal components_changes.csv / changes_std.csv",
    max(abs(cbr$est - c(comp$chg$est, read_csv(file.path(TAB, "changes_std.csv"), show_col_types = FALSE) %>% filter(analysis %in% c("national", "sex")) %>% pull(est))[
      match(paste(cbr$outcome, cbr$sex, cbr$from, cbr$to), c(paste(comp$chg$component, comp$chg$sex, comp$chg$from, comp$chg$to),
        read_csv(file.path(TAB, "changes_std.csv"), show_col_types = FALSE) %>% filter(analysis %in% c("national", "sex")) %>% with(paste("dys", level, from, to))))])) < 1e-10),
 ok("T5: 18 rows, no NA, CIs contain estimates, change = 2021 - 2016", nrow(t5) == 18 & !anyNA(t5) & inci(t5$est_2016, t5$lower_2016, t5$upper_2016) &
    inci(t5$est_2021, t5$lower_2021, t5$upper_2021) & inci(t5$change, t5$change_lower, t5$change_upper) & inci(t5$pr, t5$pr_lower, t5$pr_upper) &
    max(abs(t5$est_2021 - t5$est_2016 - t5$change)) < 1e-8),
 ok("S_fasting_2021: no NA estimates, CIs contain estimates, prevalence CIs within 0-100, distribution sums to 100",
    !anyNA(fst$est) & inci(fst$est, fst$lower, fst$upper) & all(fst$lower[!grepl("difference", fst$part)] >= 0 & fst$upper[!grepl("difference", fst$part)] <= 100) &
    abs(sum(fst$est[grepl("^distribution", fst$part)]) - 100) < 1e-8),
 ok("S_design_summary: PSUs in variance 1,491/675/2,866/2,899; unique clusters 1,432/641/2,850/2,899",
    identical(as.integer(dsm$n_psu_variance), c(1491L, 675L, 2866L, 2899L)) & identical(as.integer(dsm$n_clusters_unique), c(1432L, 641L, 2850L, 2899L))),
 ok("results_index.json has the Stage 2b keys", all(c("comp_tc_change_2016_2021_all_est", "comp_composite_pr_2016_2021_women_est", "fasting_2021_composite_all_diff_ge8h_minus_all_est",
    "design_n_psu_variance_2007", "t5_lowhdl_men_2021_est") %in% names(J))))
diffs <- c(prev$svyciprop_diff, comp$est$svyciprop_diff, pv$full$svyciprop_diff); diffs <- diffs[!is.na(diffs)]
out <- c(out, sprintf("[INFO] svyciprop without start value diverged (limits differ > 1e-6 from the refit) in %d of %d estimates; in national/subgroup tables: %d; components: %d; provinces: %d",
                      sum(diffs > 1e-6), length(diffs), sum(prev$svyciprop_diff > 1e-6, na.rm = TRUE), sum(comp$est$svyciprop_diff > 1e-6, na.rm = TRUE), sum(pv$full$svyciprop_diff > 1e-6, na.rm = TRUE)))
out <- c(out, sprintf("[INFO] max cross-round covariance of standardised estimates: %.1e", tr$maxcov))
writeLines(out, file.path(W, "output/logs/checks_stage2.txt")); cat(out, sep = "\n")
stopifnot(all(r))
