# 10_design.R : analytic population, outcomes, design variables. Output: output/rds/stage2_data.rds, logs.
source("R/09_functions.R")  # path relative to the repository root
sink(file.path(W, "output/logs/10_design.log"), split = TRUE)
a <- readRDS(file.path(W, "data_derived/analytic.rds"))
b <- a %>% mutate(
  in_age = !is.na(age) & age >= 25 & age <= 64,
  not_preg = is.na(pregnant) | !pregnant,
  all3 = !is.na(tc) & !is.na(hdl) & !is.na(tg),
  any1 = !is.na(tc) | !is.na(hdl) | !is.na(tg),
  covars_ok = !is.na(sex) & !is.na(area) & !is.na(province31) & !is.na(weight) & weight > 0,
  c_tc = as.numeric(tc >= 200), c_tg = as.numeric(tg >= 150), c_ldl = as.numeric(ldl_friedewald >= 130),
  c_nonhdl = as.numeric(nonhdl >= 160),
  c_hdl = as.numeric(hdl < ifelse(sex == "women", 50, 40)), c_hdl40 = as.numeric(hdl < 40))
# ---- bounds sample (sensitivity d): at least one valid lipid ----
bnd <- b %>% filter(in_age, not_preg, any1, covars_ok) %>% mutate(
  any_abn = coalesce(c_tc, 0) + coalesce(c_tg, 0) + coalesce(c_ldl, 0) + coalesce(c_nonhdl, 0) + coalesce(c_hdl, 0) > 0,
  status = case_when(any_abn ~ "case", all3 ~ "non-case", TRUE ~ "indeterminate"),
  y_lower = as.numeric(status == "case"), y_upper = as.numeric(status != "non-case"))
d <- b %>% filter(in_age, not_preg, all3, covars_ok) %>% mutate(
  dys = as.numeric(c_tc == 1 | c_tg == 1 | coalesce(c_ldl, 0) == 1 | c_nonhdl == 1 | c_hdl == 1),
  dys_no_nonhdl = as.numeric(c_tc == 1 | c_tg == 1 | coalesce(c_ldl, 0) == 1 | c_hdl == 1),
  dys_nohdl = as.numeric(c_tc == 1 | c_tg == 1 | coalesce(c_ldl, 0) == 1 | c_nonhdl == 1),
  dys_nohdl_no_nonhdl = as.numeric(c_tc == 1 | c_tg == 1 | coalesce(c_ldl, 0) == 1),
  dys_hdl40 = as.numeric(c_tc == 1 | c_tg == 1 | coalesce(c_ldl, 0) == 1 | c_nonhdl == 1 | c_hdl40 == 1),
  med_item = case_when(round == 2021 ~ lipid_med, round == 2016 ~ statin, TRUE ~ NA),
  med_any = ifelse(round %in% c(2016, 2021), as.numeric(coalesce(med_item, FALSE)), NA_real_),
  dys_med = ifelse(round %in% c(2016, 2021), as.numeric(dys == 1 | med_any == 1), NA_real_))
prep <- function(x) x %>% mutate(
  agegrp = factor(agegrp, AGE_LEV), sex = factor(sex, SEX_LEV), area = factor(area, AREA_LEV),
  round_f = factor(round, YEARS), year_c = round - 2007, d2011 = as.numeric(round == 2011), d2016 = as.numeric(round == 2016),
  strat_pooled = paste(round, province31, area, sep = "|"), strat_prov = paste(round, province31, sep = "|"),
  prov = as.character(province31), rp = paste(round, prov, sep = "|"))
d <- prep(d); bnd <- prep(bnd)
stopifnot(!anyNA(d$agegrp), !anyNA(d$sex), !anyNA(d$area), !anyNA(d$dys), !anyNA(d$weight_alt), all(d$weight > 0))
# PSU ids must be unique within round (they are prefixed with the round)
stopifnot(all(tapply(d$round, d$psu, function(x) length(unique(x))) == 1))
n_round <- table(d$round); print(n_round)
stopifnot(identical(as.integer(n_round), c(18507L, 4311L, 16487L, 15397L)))
cat("\nNon-HDL-C criterion adds cases (composite with vs without non-HDL-C):\n")
nh <- d %>% group_by(round) %>% summarise(added = sum(dys != dys_no_nonhdl), n = n()); print(nh)
stopifnot(all(nh$added == 0))
cat("\nNon-HDL-C criterion within the no-HDL-C composite (cases it adds):\n")
print(d %>% group_by(round) %>% summarise(added = sum(dys_nohdl != dys_nohdl_no_nonhdl)))
cat("\nBounds sample status:\n"); print(table(bnd$round, bnd$status))
cat("\nMedication item missing (counted as no) in analytic sample:\n"); print(d %>% filter(round %in% c(2016, 2021)) %>% group_by(round) %>% summarise(missing = sum(is.na(med_item)), yes = sum(med_item %in% TRUE)))
cat("\nPSUs and strata per round (pooled design):\n")
print(d %>% group_by(round) %>% summarise(n = n(), strata_1psu = sum(table(strat_pooled[!duplicated(paste(strat_pooled, psu))]) == 1), n_psu = n_distinct(psu), n_strata = n_distinct(strat_pooled)))
saveRDS(list(d = d, bnd = bnd, nonhdl_check = nh), file.path(RDS, "stage2_data.rds"))
sink()
