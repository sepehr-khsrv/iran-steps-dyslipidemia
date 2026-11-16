# 01b_reconcile.R : apply the OLD definitions and filters (scripts in data_raw/old_scripts) to the harmonised file
# and reproduce the old crude estimates in "final result dyslipidemia .csv" (sex, area, age group, per round).
# Old rules reproduced: ages 25-65 inclusive (2011 on decimal age); 2007 only: TG <= 500, TC <= 400, HDL <= 100,
# complete TC/HDL/TG, non-HDL >= 0; 2011-2021: no lipid filter, and R's NA logic in the composite
# (a missing component counts as abnormal-unknown: any abnormal -> 1, otherwise NA and dropped);
# weights = the original weights (Weight_83, Weight_90, Weight_95, Weight_1400);
# age groups cut(age, c(25,34,44,54,65), right = TRUE) i.e. (25,34], (34,44], (44,54], (54,65].
# Point estimates do not depend on the variance design, so weighted means are computed directly.
suppressMessages({library(dplyr); library(readr); library(tidyr)})
W <- "."  # repository root: run from the root folder
a <- readRDS(file.path(W, "data_derived/analytic.rds"))
old <- read_csv(file.path(W, "data_raw/old_results/final result dyslipidemia .csv"), show_col_types = FALSE)
comp <- function(tc, hdl, tg, male, female) {
  d_tg <- ifelse(tg >= 150, 1, 0); d_chl <- ifelse(tc >= 200, 1, 0); d_ldl <- ifelse((tc - hdl - tg / 5) >= 130, 1, 0)
  d_hdl <- ifelse((hdl < 40 & male) | (hdl < 50 & female), 1, 0); nh <- tc - hdl; d_nh <- ifelse(nh >= 160, 1, 0)
  ifelse(d_hdl == 1 | d_nh == 1 | d_ldl == 1 | d_chl == 1 | d_tg == 1, 1, 0)
}
oldcut <- function(x) as.character(cut(x, breaks = c(25, 34, 44, 54, 65), labels = c("25-34", "35-44", "45-54", "55-65"), right = TRUE))
res <- list()
# ---- 2007 ----
x <- a %>% filter(round == 2007) %>% transmute(tc = raw_tc, hdl = raw_hdl, tg = raw_tg, age = age, w = raw_weight_user,
        Sex = case_when(raw_c1 == 1 ~ 1, raw_c1 == 2 ~ 0), area = case_when(raw_partici3 == 1 ~ 1, raw_partici3 == 2 ~ 0), c1 = raw_c1) %>%
  filter(tg <= 500, tc <= 400, hdl <= 100) %>% filter(!is.na(tg), !is.na(tc), !is.na(hdl)) %>%
  mutate(dys = comp(tc, hdl, tg, c1 == 1, c1 == 2)) %>% filter(tc - hdl >= 0) %>% filter(age >= 25, age <= 65) %>% mutate(age_cat = oldcut(age))
res[["2007"]] <- x
# Variant B: the second copy of the 2007 CSV ("STEPS wieght/steps wieght/steps86_weight_standard.csv"), whose Weight_83,
# province, cnt and cntSmpl differ (all other columns identical). Copied to data_raw/2007/steps86_weight_standard_stepswieght_copy.csv.
b7 <- read_csv(file.path(W, "data_raw/2007/steps86_weight_standard_stepswieght_copy.csv"), show_col_types = FALSE)
xB <- a %>% filter(round == 2007) %>% mutate(Ref = as.integer(sub("^2007_", "", id))) %>% left_join(b7 %>% select(Ref, wB = Weight_83), by = "Ref") %>%
  transmute(tc = raw_tc, hdl = raw_hdl, tg = raw_tg, age = age, w = wB,
        Sex = case_when(raw_c1 == 1 ~ 1, raw_c1 == 2 ~ 0), area = case_when(raw_partici3 == 1 ~ 1, raw_partici3 == 2 ~ 0), c1 = raw_c1) %>%
  filter(tg <= 500, tc <= 400, hdl <= 100) %>% filter(!is.na(tg), !is.na(tc), !is.na(hdl)) %>%
  mutate(dys = comp(tc, hdl, tg, c1 == 1, c1 == 2)) %>% filter(tc - hdl >= 0) %>% filter(age >= 25, age <= 65) %>% mutate(age_cat = oldcut(age))
# ---- 2011 ---- (old script sets age <- age2, the ROUNDED age; c1 0 = female; area as supplied in the CSV, 1 = urban, 2 = rural)
x <- a %>% filter(round == 2011) %>% transmute(tc = raw_tc, hdl = raw_hdl, tg = raw_tg, age = raw_age2, w = raw_weight_user, Sex = raw_c1, area = raw_area) %>%
  filter(age >= 25, age <= 65) %>% mutate(dys = comp(tc, hdl, tg, Sex == 1, Sex == 0), age_cat = oldcut(age))
res[["2011"]] <- x
for (r in c(2016, 2021)) {
  x <- a %>% filter(round == r) %>% transmute(tc = raw_tc, hdl = raw_hdl, tg = raw_tg, age = age, w = raw_weight_user, Sex = raw_c1, area = raw_area) %>%
    filter(age >= 25, age <= 65) %>% mutate(dys = comp(tc, hdl, tg, Sex == 1, Sex == 0), age_cat = oldcut(age))
  res[[as.character(r)]] <- x
}
est <- function(df, g) df %>% filter(!is.na(dys), !is.na(.data[[g]])) %>% group_by(Value = as.character(.data[[g]])) %>%
  summarise(new = round(100 * sum(w * dys) / sum(w), 2), n = n(), .groups = "drop") %>% mutate(Group = g)
out <- bind_rows(lapply(names(res), function(r) bind_rows(est(res[[r]], "Sex"), est(res[[r]], "area"), est(res[[r]], "age_cat")) %>% mutate(Year = as.integer(r))))
# Old file codes: Sex 0 = women, 1 = men; area 0 = rural, 1 = urban. In 2011 the CSV 'area' is 1 = urban, 2 = rural:
# map it to the old file's 0/1 coding (rural = 0, urban = 1) for comparison; the alternative mapping is reported too.
out <- out %>% mutate(Group = recode(Group, Sex = "Sex"),
                      Value_cmp = case_when(Year == 2011 & Group == "area" & Value == "1" ~ "1", Year == 2011 & Group == "area" & Value == "2" ~ "0", TRUE ~ Value))
cmp <- old %>% mutate(Value = as.character(Value)) %>% left_join(out %>% select(Year, Group, Value_cmp, new, n), by = c("Year", "Group", "Value" = "Value_cmp")) %>%
  mutate(diff_pp = round(new - Dyslipidemia, 2), within_0.1 = abs(diff_pp) <= 0.1)
# alternative 2011 area mapping (1 -> "0", 2 -> "1")
alt <- out %>% filter(Year == 2011, Group == "area") %>% mutate(Value_alt = ifelse(Value == "1", "0", "1")) %>%
  left_join(old %>% mutate(Value = as.character(Value)), by = c("Year", "Group", "Value_alt" = "Value")) %>% mutate(diff_alt = round(new - Dyslipidemia, 2))
outB <- bind_rows(est(xB, "Sex"), est(xB, "area"), est(xB, "age_cat")) %>% mutate(Year = 2007L) %>% select(Year, Group, Value, new_variantB = new)
cmp <- cmp %>% left_join(outB, by = c("Year", "Group", "Value")) %>% mutate(diff_variantB = round(new_variantB - Dyslipidemia, 2))
write_csv(cmp, file.path(W, "output/tables/reconciliation_old_estimates.csv"))
sink(file.path(W, "output/logs/01b_reconcile.log"), split = TRUE)
print(as.data.frame(cmp %>% select(Year, Group, Value, old = Dyslipidemia, new, diff_pp, within_0.1, n, new_variantB, diff_variantB)))
cat("\nAll within 0.1 pp:", all(cmp$within_0.1), "  matched:", sum(cmp$within_0.1), "of", nrow(cmp), "\n")
cat("\n2011 area under the alternative mapping (CSV area 1 -> old 0, 2 -> old 1):\n"); print(alt %>% select(Value, new, old = Dyslipidemia, diff_alt))
sink()
