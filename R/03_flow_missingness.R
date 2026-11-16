# 03_flow_missingness.R : participant flow, missingness, included vs excluded, case classification.
# Outputs: output/tables/flow_by_round.csv, missingness_by_round.csv, included_vs_excluded.csv, case_classification.csv
suppressMessages({library(dplyr); library(readr); library(tidyr)})
W <- "."  # repository root: run from the root folder
a <- readRDS(file.path(W, "data_derived/analytic.rds"))
sink(file.path(W, "output/logs/03_flow_missingness.log"), split = TRUE)
a <- a %>% mutate(all3 = !is.na(tc) & !is.na(hdl) & !is.na(tg),
                  in_age = !is.na(age) & age >= 25 & age <= 64,
                  not_preg = is.na(pregnant) | !pregnant,
                  covars_ok = !is.na(sex) & !is.na(area) & !is.na(province31) & !is.na(weight) & weight > 0,
                  analytic = in_age & not_preg & all3 & covars_ok,
                  # weight for describing everyone (lab or not): full-sample / questionnaire weight
                  w_q = case_when(round %in% c(2007, 2011) ~ weight_alt, TRUE ~ w_official_q))
# ---- Flow ----
fl <- a %>% group_by(round) %>% summarise(
  n_file = n(),
  n_age_25_64 = sum(in_age),
  n_not_pregnant = sum(in_age & not_preg),
  pregnant_excluded = sum(in_age & !not_preg),
  pregnancy_recorded = ifelse(all(is.na(pregnant[sex %in% "women"])), "no", "yes (women)"),
  n_any_lipid = sum(in_age & not_preg & lab_participant),
  no_lipid_measured = sum(in_age & not_preg & !lab_participant),
  n_all3_valid = sum(in_age & not_preg & lab_participant & all3),
  lipid_partly_missing_or_implausible = sum(in_age & not_preg & lab_participant & !all3),
  n_analytic = sum(analytic),
  excl_missing_sex = sum(in_age & not_preg & all3 & is.na(sex)),
  excl_missing_area = sum(in_age & not_preg & all3 & !is.na(sex) & is.na(area)),
  excl_missing_province = sum(in_age & not_preg & all3 & !is.na(sex) & !is.na(area) & is.na(province31)),
  excl_missing_or_zero_weight = sum(in_age & not_preg & all3 & !is.na(sex) & !is.na(area) & !is.na(province31) & (is.na(weight) | weight <= 0)),
  out_of_age_or_missing_age = sum(!in_age), .groups = "drop")
print(as.data.frame(t(fl))); write_csv(fl, file.path(W, "output/tables/flow_by_round.csv"))
# ---- Missingness among aged 25-64 ----
b <- a %>% filter(in_age)
miss <- function(df, v, implaus = NULL, denom_note = "aged 25-64") {
  x <- df[[v]]; n <- nrow(df)
  tibble(variable = v, denominator = denom_note, n = n, missing = sum(is.na(x)), pct_missing = round(100 * mean(is.na(x)), 2),
         implausible = if (is.null(implaus)) NA_integer_ else sum(df[[implaus]]), pct_implausible = if (is.null(implaus)) NA_real_ else round(100 * mean(df[[implaus]]), 2))
}
mt <- bind_rows(lapply(split(b, b$round), function(df) bind_rows(
  miss(df, "tc", "tc_implausible"), miss(df, "hdl", "hdl_implausible"), miss(df, "tg", "tg_implausible"),
  miss(df %>% filter(lab_participant), "tc", "tc_implausible", "lab participants aged 25-64"),
  miss(df %>% filter(lab_participant), "hdl", "hdl_implausible", "lab participants aged 25-64"),
  miss(df %>% filter(lab_participant), "tg", "tg_implausible", "lab participants aged 25-64"),
  miss(df %>% filter(lab_participant), "ldl_measured", NULL, "lab participants aged 25-64"),
  miss(df, "sex"), miss(df, "area"), miss(df, "province31"),
  miss(df %>% filter(lab_participant), "weight", NULL, "lab participants aged 25-64"),
  miss(df, "told_raised_chol"), miss(df, "lipid_med"), miss(df, "statin")) %>% mutate(round = df$round[1]))) %>% relocate(round)
agemiss <- a %>% group_by(round) %>% summarise(variable = "age", denominator = "all records", n = n(), missing = sum(is.na(age)), pct_missing = round(100 * mean(is.na(age)), 2), .groups = "drop")
mt <- bind_rows(mt, agemiss) %>% arrange(round)
mt <- mt %>% mutate(note = case_when(variable %in% c("told_raised_chol", "lipid_med", "statin") & pct_missing == 100 ~ "item not asked or not released in this round",
                                     variable == "ldl_measured" & pct_missing == 100 ~ "no measured LDL-C in this round", TRUE ~ ""))
print(as.data.frame(mt)); write_csv(mt, file.path(W, "output/tables/missingness_by_round.csv"))
# ---- Included vs excluded (aged 25-64, not pregnant): included = analytic; excluded = any lipid missing ----
ie <- a %>% filter(in_age, not_preg) %>% mutate(group = ifelse(all3, "included (all three lipids valid)", "excluded (any lipid missing or implausible)")) %>%
  filter(!is.na(sex), !is.na(area))
desc <- function(var) ie %>% group_by(round, group, level = .data[[var]]) %>% summarise(n = n(), w = sum(w_q, na.rm = TRUE), .groups = "drop") %>%
  group_by(round, group) %>% mutate(pct_unweighted = round(100 * n / sum(n), 1), pct_weighted = round(100 * w / sum(w), 1)) %>% ungroup() %>% mutate(characteristic = var) %>% select(-w)
iv <- bind_rows(desc("agegrp"), desc("sex"), desc("area")) %>% relocate(round, characteristic, level, group)
iv <- iv %>% mutate(weight_used = case_when(round == 2007 ~ "rebuilt full-sample PS weight (census 2006)", round == 2011 ~ "Weight_90 (user, full sample)", round == 2016 ~ "weight_Q (official questionnaire)", round == 2021 ~ "W_Questionnaire (official)"))
print(as.data.frame(iv)); write_csv(iv, file.path(W, "output/tables/included_vs_excluded.csv"))
# ---- Case classification: aged 25-64, not pregnant, at least one valid lipid ----
cc <- a %>% filter(in_age, not_preg, !is.na(tc) | !is.na(hdl) | !is.na(tg)) %>% mutate(
  c_tc = tc >= 200, c_tg = tg >= 150, c_ldl = ldl_friedewald >= 130, c_nonhdl = nonhdl >= 160,
  c_hdl = case_when(is.na(hdl) ~ NA, hdl < 40 ~ TRUE, is.na(sex) ~ NA, sex == "women" & hdl < 50 ~ TRUE, TRUE ~ FALSE),
  any_abn = coalesce(c_tc, FALSE) | coalesce(c_tg, FALSE) | coalesce(c_ldl, FALSE) | coalesce(c_nonhdl, FALSE) | coalesce(c_hdl, FALSE),
  status = case_when(any_abn ~ "case", all3 & !is.na(c_hdl) ~ "non-case", TRUE ~ "indeterminate"))
ct <- cc %>% count(round, status) %>% group_by(round) %>% mutate(pct = round(100 * n / sum(n), 2)) %>% ungroup() %>% pivot_wider(names_from = status, values_from = c(n, pct))
print(as.data.frame(ct)); write_csv(ct, file.path(W, "output/tables/case_classification.csv"))
sink()
