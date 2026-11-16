# 18_results_index.R : output/results_index.json (flat dictionary of every number a writer needs) and output/results_summary.md.
source("R/09_functions.R")  # path relative to the repository root
suppressMessages(library(jsonlite))
sink(file.path(W, "output/logs/18_results_index.log"), split = TRUE)
L <- load_data(); d <- L$d
prev <- readRDS(file.path(RDS, "prev_main.rds")); tr <- readRDS(file.path(RDS, "trends.rds"))
comp <- readRDS(file.path(RDS, "components.rds")); sens <- readRDS(file.path(RDS, "sensitivity.rds")); pv <- readRDS(file.path(RDS, "provinces.rds"))
chg <- read_csv(file.path(TAB, "changes_std.csv"), show_col_types = FALSE); dchg <- read_csv(file.path(TAB, "difference_in_change_std.csv"), show_col_types = FALSE)
T1 <- read_csv(file.path(TAB, "T1_characteristics.csv"), show_col_types = FALSE)
k <- function(...) gsub("[^A-Za-z0-9]+", "_", tolower(paste(..., sep = "_")))
IDX <- list()
add <- function(key, value, unit, source) {
  stopifnot(length(value) == 1, !key %in% names(IDX))
  IDX[[key]] <<- list(value = if (is.numeric(value)) unname(signif(value, 8)) else value, unit = unit, source = source)
}
add_ci <- function(stem, est, lower, upper, unit, source, p = NULL, n = NULL, se = NULL) {
  add(k(stem, "est"), est, unit, source); add(k(stem, "lower"), lower, unit, source); add(k(stem, "upper"), upper, unit, source)
  if (!is.null(se)) add(k(stem, "se"), se, unit, source)
  if (!is.null(p)) add(k(stem, "p"), p, "P value", source)
  if (!is.null(n)) add(k(stem, "n"), n, "participants (unweighted)", source)
}
# n
for (y in YEARS) add(k("n_analytic", y), sum(d$round == y), "participants (unweighted)", "S_flow.csv / T1_characteristics.csv")
add("n_analytic_total", nrow(d), "participants (unweighted)", "T1_characteristics.csv")
# T1
for (i in seq_len(nrow(T1))) {
  r <- T1[i, ]; if (r$characteristic == "total") next
  stem <- k("t1", r$characteristic, r$level, r$round)
  if (!is.na(r$pct_weighted)) add(k(stem, ifelse(r$level == "mean", "value", "pct_weighted")), r$pct_weighted, ifelse(r$level == "mean", "years", "% (weighted)"), "T1_characteristics.csv")
  if (r$level != "mean") add(k(stem, "n"), r$n, "participants (unweighted)", "T1_characteristics.csv")
}
# prevalence
for (i in seq_len(nrow(prev))) {
  r <- prev[i, ]; ty <- c("standardised" = "std", "crude weighted" = "crude", "unweighted" = "unweighted")[[r$type]]
  add_ci(k("prev", ty, r$analysis, r$level, r$round), r$est, r$lower, r$upper, "%", "prev_main.csv / T2_prevalence_subgroups.csv", n = r$n, se = r$se)
  add(k("deff", ty, r$analysis, r$level, r$round), r$deff, "design effect", "S_design_effects.csv")
}
# changes
for (i in seq_len(nrow(chg))) { r <- chg[i, ]; add_ci(k("change_std", r$analysis, r$level, r$from, r$to), r$est, r$lower, r$upper, "percentage points", "changes_std.csv / S_between_round.csv", p = r$p) }
for (i in seq_len(nrow(dchg))) { r <- dchg[i, ]; add_ci(k("diffchange_std", r$contrast, r$from, r$to), r$est, r$lower, r$upper, "percentage points", "difference_in_change_std.csv / S_between_round.csv", p = r$p) }
# APC
ap <- tr$apc %>% filter(outcome == "dys")
for (i in seq_len(nrow(ap))) {
  r <- ap[i, ]; mdl <- c("subset (stratified)" = "", "interaction (common covariate effects)" = "intmodel", "fully interacted" = "fullint")[[r$model]]
  lk <- ifelse(r$link == "log", "", "logit_or")
  stem <- k("apc", mdl, lk, r$analysis, r$level); stem <- gsub("_+", "_", stem)
  add_ci(stem, r$apc, r$apc_lower, r$apc_upper, ifelse(lk == "", "% per year", "(OR - 1) x 100 per year"), "trend_apc.csv / T2_prevalence_subgroups.csv", p = r$p_trend)
}
for (i in seq_len(nrow(tr$ints))) { r <- tr$ints[i, ]
  add(k("p_interaction", r$variable, ifelse(grepl("fully", r$model), "fullint", "main"), r$link), r$p_interaction, "P value (Wald)", "trend_interactions.csv / T2_prevalence_subgroups.csv") }
for (i in seq_len(nrow(tr$lin))) { r <- tr$lin[i, ]; add(k("p_nonlinearity", r$analysis, r$level), r$p_nonlinearity, "P value (Wald, 2 df)", "S_linearity.csv") }
for (i in seq_len(nrow(tr$prs))) { r <- tr$prs[i, ]; add_ci(k("pr", r$analysis, r$level, r$from, r$to), r$pr, r$pr_lower, r$pr_upper, "prevalence ratio", "S_between_round.csv", p = r$p) }
# components
ce <- comp$est %>% filter(type == "standardised")
for (i in seq_len(nrow(ce))) { r <- ce[i, ]; add_ci(k("comp", r$component, r$sex, r$round), r$est, r$lower, r$upper, "%", "T3_components.csv", n = r$n) }
for (i in seq_len(nrow(comp$chg))) { r <- comp$chg[i, ]; add_ci(k("comp_change", r$component, r$sex, r$from, r$to), r$est, r$lower, r$upper, "percentage points", "components_changes.csv / T3_components.csv", p = r$p) }
for (i in seq_len(nrow(comp$apc))) { r <- comp$apc[i, ]; add_ci(k("comp_apc", r$component, r$sex), r$apc, r$apc_lower, r$apc_upper, "% per year", "T3_components.csv", p = r$p_trend)
  if (!is.na(r$p_interaction_sex)) add(k("comp_p_interaction_sex", r$component), r$p_interaction_sex, "P value (Wald)", "T3_components.csv") }
# sensitivity headlines
sa <- sens$a
mp <- sa %>% filter(part == "proportion on medication")
for (i in seq_len(nrow(mp))) { r <- mp[i, ]; add_ci(k("med", ifelse(r$round == 2021, "h14a", "h19_statin"), r$round, ifelse(r$type == "standardised", "std", "crude"), r$group, r$level), r$est, r$lower, r$upper, "% (weighted)", "S_sensitivity_a_medication.csv", n = r$n) }
sp <- sa %>% filter(part == "prevalence")
for (i in seq_len(nrow(sp))) { r <- sp[i, ]; add_ci(k("sens_a", r$outcome, r$sex, r$round), r$est, r$lower, r$upper, "%", "S_sensitivity_a_medication.csv") }
sc <- sa %>% filter(part == "change")
for (i in seq_len(nrow(sc))) { r <- sc[i, ]; add_ci(k("sens_a", r$outcome, r$contrast, r$from, r$to), r$est, r$lower, r$upper, "percentage points", "S_sensitivity_a_medication.csv", p = r$p) }
for (lt in c("b", "c")) {
  x <- sens[[lt]]
  for (i in which(x$part == "prevalence")) { r <- x[i, ]; add_ci(k("sens", lt, r$sex, r$round), r$est, r$lower, r$upper, "%", paste0("S_sensitivity_", lt, ".csv")) }
  for (i in which(x$part == "change")) { r <- x[i, ]; add_ci(k("sens", lt, r$contrast, r$from, r$to), r$est, r$lower, r$upper, "percentage points", paste0("S_sensitivity_", lt, ".csv"), p = r$p) }
  for (i in which(x$part == "trend")) { r <- x[i, ]; add_ci(k("sens", lt, "apc", r$level), r$apc, r$apc_lower, r$apc_upper, "% per year", paste0("S_sensitivity_", lt, ".csv"), p = r$p_trend) }
  add(k("sens", lt, "p_interaction_sex"), x$p_interaction_sex[which(x$part == "trend")[1]], "P value (Wald)", paste0("S_sensitivity_", lt, ".csv"))
}
x <- sens$d
for (i in which(x$part == "prevalence")) { r <- x[i, ]; add_ci(k("sens_d", r$outcome, r$sex, r$round), r$est, r$lower, r$upper, "%", "S_sensitivity_d_bounds.csv") }
for (i in which(x$part == "trend")) { r <- x[i, ]; add_ci(k("sens_d", r$outcome, "apc", r$level), r$apc, r$apc_lower, r$apc_upper, "% per year", "S_sensitivity_d_bounds.csv", p = r$p_trend) }
for (i in which(x$part == "counts (unweighted)")) { r <- x[i, ]; add(k("sens_d_indeterminate", r$round), r$indeterminate, "participants (unweighted)", "S_sensitivity_d_bounds.csv") }
e <- sens$e
for (v in c("n", "mean_difference", "sd_difference", "loa_lower", "loa_upper", "pearson_r", "pct_agreement", "kappa", "kappa_lower", "kappa_upper", "pct_measured_ge130", "pct_friedewald_ge130", "composite_changes_if_measured"))
  add(k("sens_e", v), e[[v]], ifelse(grepl("pct", v), "%", ifelse(v %in% c("n", "composite_changes_if_measured"), "participants", ifelse(grepl("kappa|pearson", v), "coefficient", "mg/dL"))), "S_sensitivity_e_ldl_2011.csv")
x <- sens$f
for (i in which(x$part == "trend")) { r <- x[i, ]; add_ci(k("sens_f_apc", r$level), r$apc, r$apc_lower, r$apc_upper, "% per year", "S_sensitivity_f_excl_2011.csv", p = r$p_trend) }
for (i in which(x$part == "interaction")) { r <- x[i, ]; add(k("sens_f_p_interaction", r$variable), r$p_interaction, "P value (Wald)", "S_sensitivity_f_excl_2011.csv") }
x <- sens$h
for (i in seq_len(nrow(x))) { r <- x[i, ]; stem <- if (is.na(r$round)) "sens_h_apc" else k("sens_h_std_national", r$round)
  add(k(stem, "se_main"), r$se_main, ifelse(is.na(r$round), "SE (log scale)", "percentage points"), "S_sensitivity_h_strata_province.csv")
  add(k(stem, "se_alt"), r$se_alt, ifelse(is.na(r$round), "SE (log scale)", "percentage points"), "S_sensitivity_h_strata_province.csv")
  add_ci(k(stem, "alt"), r$est_alt, r$lower_alt, r$upper_alt, ifelse(is.na(r$round), "% per year", "%"), "S_sensitivity_h_strata_province.csv") }
x <- sens$i
for (i in which(x$part == "prevalence")) { r <- x[i, ]; add_ci(k("sens_i", r$sex, r$round), r$est, r$lower, r$upper, "%", "S_sensitivity_i_weights.csv") }
for (i in which(x$part == "change")) { r <- x[i, ]; add_ci(k("sens_i", r$contrast, r$from, r$to), r$est, r$lower, r$upper, "percentage points", "S_sensitivity_i_weights.csv", p = r$p) }
for (i in which(x$part == "trend")) { r <- x[i, ]; add_ci(k("sens_i_apc", r$level), r$apc, r$apc_lower, r$apc_upper, "% per year", "S_sensitivity_i_weights.csv", p = r$p_trend) }
add("sens_i_p_interaction_sex", x$p_interaction_sex[which(x$part == "trend")[1]], "P value (Wald)", "S_sensitivity_i_weights.csv")
# provinces: extremes per round, APC extremes, change extremes, correlation
pf <- pv$full %>% filter(!is.na(est))
for (y in YEARS) {
  x <- pf %>% filter(round == y); hi <- x[which.max(x$est), ]; lo <- x[which.min(x$est), ]
  add(k("prov_highest", y, "province"), as.character(hi$label), "province", "T4_provinces.csv"); add_ci(k("prov_highest", y), hi$est, hi$lower, hi$upper, "%", "T4_provinces.csv", n = hi$n)
  add(k("prov_lowest", y, "province"), as.character(lo$label), "province", "T4_provinces.csv"); add_ci(k("prov_lowest", y), lo$est, lo$lower, lo$upper, "%", "T4_provinces.csv", n = lo$n)
  add(k("prov_n_estimated", y), nrow(x), "provinces", "T4_provinces.csv")
}
pa <- pv$apc; hi <- pa[which.max(pa$apc), ]; lo <- pa[which.min(pa$apc), ]
add("prov_apc_highest_province", as.character(hi$prov), "province", "T4_provinces.csv"); add_ci("prov_apc_highest", hi$apc, hi$apc_lower, hi$apc_upper, "% per year", "T4_provinces.csv", p = hi$p_trend)
add("prov_apc_lowest_province", as.character(lo$prov), "province", "T4_provinces.csv"); add_ci("prov_apc_lowest", lo$apc, lo$apc_lower, lo$apc_upper, "% per year", "T4_provinces.csv", p = lo$p_trend)
add("prov_apc_n_p_lt_0_05", sum(pa$p_trend < 0.05), "provinces", "T4_provinces_apc.csv"); add("prov_apc_n_increase_p_lt_0_05", sum(pa$p_trend < 0.05 & pa$apc > 0), "provinces", "T4_provinces_apc.csv")
add("prov_apc_n_decrease_p_lt_0_05", sum(pa$p_trend < 0.05 & pa$apc < 0), "provinces", "T4_provinces_apc.csv"); add("prov_apc_n_holm_lt_0_05", sum(pa$p_holm < 0.05), "provinces", "T4_provinces_apc.csv")
pc <- pv$chg %>% filter(!is.na(change_2007_2021)); hi <- pc[which.max(pc$change_2007_2021), ]; lo <- pc[which.min(pc$change_2007_2021), ]
add("prov_change_largest_rise_province", as.character(hi$prov), "province", "provinces_change.csv"); add_ci("prov_change_largest_rise", hi$change_2007_2021, hi$lower, hi$upper, "percentage points", "provinces_change.csv")
add("prov_change_largest_fall_province", as.character(lo$prov), "province", "provinces_change.csv"); add_ci("prov_change_largest_fall", lo$change_2007_2021, lo$lower, lo$upper, "percentage points", "provinces_change.csv")
add("prov_corr_2007_apc_pearson_r", pv$corr$pearson_r, "correlation", "T4_provinces_notes.csv"); add("prov_corr_2007_apc_pearson_p", pv$corr$pearson_p, "P value", "T4_provinces_notes.csv")
add("prov_corr_2007_apc_spearman_rho", pv$corr$spearman_rho, "correlation", "T4_provinces_notes.csv"); add("prov_corr_2007_apc_n", pv$corr$n_provinces, "provinces", "T4_provinces_notes.csv")
for (i in seq_len(nrow(pv$full))) { r <- pv$full[i, ]; if (is.na(r$est)) next; add_ci(k("prov", r$prov, r$round), r$est, r$lower, r$upper, "%", "T4_provinces.csv", n = r$n) }
for (i in seq_len(nrow(pa))) { r <- pa[i, ]; add_ci(k("prov_apc", r$prov), r$apc, r$apc_lower, r$apc_upper, "% per year", "T4_provinces.csv", p = r$p_trend) }
# design
for (y in YEARS) { dd <- d[d$round == y, ]; add(k("design_n_psu", y), length(unique(dd$psu)), "PSUs", "S_design_effects.csv"); add(k("design_n_strata", y), length(unique(dd$strat_pooled)), "strata", "S_design_effects.csv") }
nas <- names(IDX)[sapply(IDX, function(z) is.null(z$value) || (is.numeric(z$value) && !is.finite(z$value)))]
cat("Keys with NA / non-finite values:", length(nas), "\n"); print(nas); stopifnot(length(nas) == 0)
cat("Keys:", length(IDX), "\n")
write_json(IDX, file.path(W, "output/results_index.json"), auto_unbox = TRUE, pretty = TRUE, digits = NA)

# ---- results_summary.md ----
g <- function(key) IDX[[key]]$value
ci <- function(stem, f = f1) paste0(f(g(paste0(stem, "_est"))), " (", f(g(paste0(stem, "_lower"))), ", ", f(g(paste0(stem, "_upper"))), ")")
P <- function(x) fp(x)
md <- c(
"# Dyslipidaemia trends, Iran STEPS 2007-2021: Stage 2 results summary",
"",
paste0("Generated by `R/18_results_index.R` on ", format(Sys.Date(), "%d %B %Y"), ". Every number below is in `output/results_index.json`. Prevalence is directly standardised to the 2016 census population aged 25-64 and given with 95% CIs (logit). Standardisation uses survey::svystandardize, which treats standardisation as post-stratification: the linearised scores are residualised within the standardisation cells, rather than the standardised weights being treated as fixed. APC is the annual percentage change from a survey-weighted log-link (quasi-Poisson) model on pooled records, adjusted for age group, sex and area."),
"",
"## Main results",
"",
paste0("- National: ", ci("prev_std_national_all_2007"), "% in 2007, ", ci("prev_std_national_all_2011"), "% in 2011, ", ci("prev_std_national_all_2016"), "% in 2016 and ", ci("prev_std_national_all_2021"),
       "% in 2021. Change 2007-2021: ", ci("change_std_national_all_2007_2021"), " points. APC ", ci("apc_national_all", f2), "% (P = ", P(g("apc_national_all_p")), "). Departure from a log-linear trend: P = ", P(g("p_nonlinearity_national_all")), "."),
paste0("- Women: ", ci("prev_std_sex_women_2007"), "% in 2007 and ", ci("prev_std_sex_women_2021"), "% in 2021 (APC ", ci("apc_sex_women", f2), "%). Men: ", ci("prev_std_sex_men_2007"), "% and ", ci("prev_std_sex_men_2021"),
       "% (APC ", ci("apc_sex_men", f2), "%). Year x sex interaction P = ", P(g("p_interaction_sex_main_log")), "."),
paste0("- Women's prevalence fell from 2011 to 2016 (", ci("change_std_sex_women_2011_2016"), " points) and rose from 2016 to 2021 (", ci("change_std_sex_women_2016_2021"), " points). Men changed little between 2016 and 2021 (", ci("change_std_sex_men_2016_2021"), ")."),
paste0("- Age: APC 25-34 ", ci("apc_agegrp_25_34", f2), "%; 35-44 ", ci("apc_agegrp_35_44", f2), "%; 45-54 ", ci("apc_agegrp_45_54", f2), "%; 55-64 ", ci("apc_agegrp_55_64", f2), "%. Year x age interaction P = ", P(g("p_interaction_agegrp_main_log")), "."),
paste0("- Area: APC urban ", ci("apc_area_urban", f2), "%, rural ", ci("apc_area_rural", f2), "%. Year x area interaction P = ", P(g("p_interaction_area_main_log")), "."),
paste0("- Components 2007 to 2021 (standardised): TC >= 200 ", ci("comp_c_tc_all_2007"), "% to ", ci("comp_c_tc_all_2021"), "%; LDL-C >= 130 ", ci("comp_c_ldl_all_2007"), "% to ", ci("comp_c_ldl_all_2021"),
       "%; TG >= 150 ", ci("comp_c_tg_all_2007"), "% to ", ci("comp_c_tg_all_2021"), "%; low HDL-C ", ci("comp_c_hdl_all_2007"), "% to ", ci("comp_c_hdl_all_2021"), "%. TC and LDL-C fell while low HDL-C rose, so the composite changed little."),
paste0("- Medication: ", ci("med_h14a_2021_crude_all_all"), "% (weighted) reported current lipid-lowering drugs in 2021 (h14a); ", ci("med_h19_statin_2016_crude_all_all"),
       "% reported a statin in 2016 (h19, proxy). Counting treated people as cases, 2016-2021 change: ", ci("sens_a_dys_med_change_all_2016_2021"), " points (composite alone ", ci("sens_a_dys_change_all_2016_2021"), ")."),
paste0("- Without the HDL-C criterion, the 2007-2021 change was ", ci("sens_b_change_all_2007_2021"), " points (APC ", ci("sens_b_apc_all", f2), "%). The sex difference in trend persists (women minus men change ", ci("sens_b_difference_in_change_women_minus_men_2007_2021"), ")."),
paste0("- Provinces: estimated in ", g("prov_n_estimated_2007"), " provinces in 2007 (Tehran includes Alborz; Kurdistan has no laboratory data), 31 in 2011, 30 in 2016 (Qom not surveyed) and 31 in 2021. Provincial APC ranged from ",
       ci("prov_apc_lowest", f2), "% (", g("prov_apc_lowest_province"), ") to ", ci("prov_apc_highest", f2), "% (", g("prov_apc_highest_province"), "). ", g("prov_apc_n_p_lt_0_05"), " of 31 provincial APCs had P < 0.05 (no adjustment for multiplicity).")
)
writeLines(md, file.path(W, "output/results_summary.md"))
sink()
