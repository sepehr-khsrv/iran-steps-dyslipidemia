# 14_sensitivity.R : sensitivity analyses (a) to (i) of docs/SAP.md section 8. Outputs: output/tables/S_sensitivity_*.csv
source("R/09_functions.R")  # path relative to the repository root
sink(file.path(W, "output/logs/14_sensitivity.log"), split = TRUE)
L <- load_data(); d <- L$d; bnd <- L$bnd
des <- make_design(d)
prev <- readRDS(file.path(RDS, "prev_main.rds")); chg_main <- read_csv(file.path(TAB, "changes_std.csv"), show_col_types = FALSE)
dchg_main <- read_csv(file.path(TAB, "difference_in_change_std.csv"), show_col_types = FALSE)
tr <- readRDS(file.path(RDS, "trends.rds"))
sub_rounds <- function(df, rounds) df %>% filter(round %in% rounds) %>% mutate(round_f = droplevels(round_f))
yrs <- function(df) sort(unique(df$round))

# Standardised national and sex-specific estimates, changes, women-minus-men contrast and APCs for an outcome / design
sens_block <- function(dsg, df, y, label, periods = list(c(2007, 2021), c(2016, 2021)), apc = TRUE) {
  dn <- std_design(dsg, ~agegrp + sex + area, ~round_f, y = y)
  dsx <- std_design(dsg, ~agegrp + area, ~round_f + sex, y = y)
  e <- bind_rows(cell_prev(dn, y, "round_f") %>% mutate(sex = "all"), cell_prev(dsx, y, c("round_f", "sex"))) %>%
    transmute(analysis = label, outcome = y, sex, round = as.integer(as.character(round_f)), type, est, lower, upper, se, deff, n)
  bn <- by_means(dn, y, ~round_f); bs <- by_means(dsx, y, ~round_f + sex)
  ch <- list()
  for (pp in periods) if (all(pp %in% yrs(df))) {
    ch[[length(ch) + 1]] <- contr(bn, as.character(pp[2]), as.character(pp[1]), "change, all") %>% mutate(from = pp[1], to = pp[2])
    for (sx in SEX_LEV) ch[[length(ch) + 1]] <- contr(bs, paste(pp[2], sx, sep = "."), paste(pp[1], sx, sep = "."), paste0("change, ", sx)) %>% mutate(from = pp[1], to = pp[2])
    ch[[length(ch) + 1]] <- contr(bs, c(paste(pp[2], "women", sep = "."), paste(pp[1], "men", sep = ".")), c(paste(pp[1], "women", sep = "."), paste(pp[2], "men", sep = ".")),
                                  "difference in change, women minus men") %>% mutate(from = pp[1], to = pp[2])
  }
  ch <- bind_rows(ch) %>% mutate(analysis = label, outcome = y) %>% relocate(analysis, outcome, contrast, from, to)
  a <- NULL
  if (apc) {
    a <- bind_rows(
      data.frame(analysis = label, outcome = y, level = "all", apc_term(fit_trend(dsg, as.formula(paste(y, "~ year_c + agegrp + sex + area"))))),
      data.frame(analysis = label, outcome = y, level = "men", apc_term(fit_trend(dsg[df$sex == "men", ], as.formula(paste(y, "~ year_c + agegrp + area"))))),
      data.frame(analysis = label, outcome = y, level = "women", apc_term(fit_trend(dsg[df$sex == "women", ], as.formula(paste(y, "~ year_c + agegrp + area"))))))
    a$p_interaction_sex <- p_term(fit_trend(dsg, as.formula(paste(y, "~ year_c * sex + agegrp + area"))), ~year_c:sex)
  }
  list(est = e, chg = ch, apc = a)
}
long_out <- function(b, nm) {
  x <- bind_rows(b$est %>% mutate(part = "prevalence"), b$chg %>% mutate(part = "change"), if (!is.null(b$apc)) b$apc %>% mutate(part = "trend"))
  x <- x %>% relocate(part); wcsv(x, nm); print(as.data.frame(x %>% mutate(across(where(is.double), ~ signif(.x, 4))))); x
}
res <- list()

# (a) medication
cat("\n==== (a) medication ====\n")
dm <- sub_rounds(d, c(2016, 2021)); desm <- make_design(dm)
dmi <- dm %>% filter(!is.na(med_item)) %>% mutate(med_num = as.numeric(med_item)); desmi <- make_design(dmi)
mp <- bind_rows(
  cell_prev(desmi, "med_num", "round_f", "crude weighted") %>% mutate(group = "all", level = "all"),
  cell_prev(desmi, "med_num", c("round_f", "sex"), "crude weighted") %>% mutate(group = "sex", level = sex),
  cell_prev(desmi, "med_num", c("round_f", "agegrp"), "crude weighted") %>% mutate(group = "agegrp", level = as.character(agegrp)),
  cell_prev(std_design(desmi, ~agegrp + sex + area, ~round_f), "med_num", "round_f", "standardised") %>% mutate(group = "all", level = "all")) %>%
  transmute(part = "proportion on medication", item = ifelse(round_f == "2021", "h14a: currently taking oral drugs for raised cholesterol", "h19: currently taking a statin (proxy)"),
            round = as.integer(as.character(round_f)), group, level, type, est, lower, upper, se, deff, n)
ba <- sens_block(desm, dm, "dys_med", "(a) composite or lipid medication (2021 h14a; 2016 h19 statin proxy)", periods = list(c(2016, 2021)), apc = FALSE)
bm0 <- sens_block(desm, dm, "dys", "(a) reference: composite, 2016 and 2021", periods = list(c(2016, 2021)), apc = FALSE)
sa <- bind_rows(mp, ba$est %>% mutate(part = "prevalence"), ba$chg %>% mutate(part = "change"), bm0$est %>% mutate(part = "prevalence"), bm0$chg %>% mutate(part = "change"))
print(as.data.frame(sa %>% mutate(across(where(is.double), ~ signif(.x, 4))))); wcsv(sa, "S_sensitivity_a_medication"); res$a <- sa

# (b) without the HDL-C criterion; (c) HDL-C < 40 both sexes
cat("\n==== (b) ====\n"); res$b <- long_out(sens_block(des, d, "dys_nohdl", "(b) composite without the HDL-C criterion"), "S_sensitivity_b_no_hdl")
cat("\n==== (c) ====\n"); res$c <- long_out(sens_block(des, d, "dys_hdl40", "(c) composite with HDL-C < 40 for both sexes"), "S_sensitivity_c_hdl40")

# (d) available-case bounds
cat("\n==== (d) ====\n")
desb <- make_design(bnd)
bl <- sens_block(desb, bnd, "y_lower", "(d) lower bound: indeterminate = non-case", periods = list(c(2007, 2021), c(2016, 2021)))
bu <- sens_block(desb, bnd, "y_upper", "(d) upper bound: indeterminate = case", periods = list(c(2007, 2021), c(2016, 2021)))
cnt <- bnd %>% count(round, status) %>% pivot_wider(names_from = status, values_from = n) %>% mutate(part = "counts (unweighted)")
sd_ <- bind_rows(cnt, bl$est %>% mutate(part = "prevalence"), bu$est %>% mutate(part = "prevalence"), bl$chg %>% mutate(part = "change"), bu$chg %>% mutate(part = "change"),
                 bl$apc %>% mutate(part = "trend"), bu$apc %>% mutate(part = "trend"))
print(as.data.frame(sd_ %>% mutate(across(where(is.double), ~ signif(.x, 4))))); wcsv(sd_, "S_sensitivity_d_bounds"); res$d <- sd_

# (e) 2011 measured vs Friedewald LDL-C (unweighted)
cat("\n==== (e) ====\n")
e <- d %>% filter(round == 2011, !is.na(ldl_measured), !is.na(ldl_friedewald))
dif <- e$ldl_measured - e$ldl_friedewald; a1 <- e$ldl_measured >= 130; a2 <- e$ldl_friedewald >= 130
po <- mean(a1 == a2); pe <- mean(a1) * mean(a2) + mean(!a1) * mean(!a2); kap <- (po - pe) / (1 - pe)
se_k <- sqrt(po * (1 - po) / (length(a1) * (1 - pe)^2))
se_tab <- data.frame(n = nrow(e), n_analytic_2011 = sum(d$round == 2011), mean_measured = mean(e$ldl_measured), mean_friedewald = mean(e$ldl_friedewald),
  mean_difference = mean(dif), sd_difference = sd(dif), loa_lower = mean(dif) - 1.96 * sd(dif), loa_upper = mean(dif) + 1.96 * sd(dif),
  pearson_r = cor(e$ldl_measured, e$ldl_friedewald), pct_measured_ge130 = 100 * mean(a1), pct_friedewald_ge130 = 100 * mean(a2),
  both_ge130 = sum(a1 & a2), measured_only = sum(a1 & !a2), friedewald_only = sum(!a1 & a2), neither = sum(!a1 & !a2),
  pct_agreement = 100 * po, kappa = kap, kappa_lower = kap - Z * se_k, kappa_upper = kap + Z * se_k,
  composite_changes_if_measured = sum(e$dys != as.numeric(e$c_tc == 1 | e$c_tg == 1 | a1 | e$c_nonhdl == 1 | e$c_hdl == 1)),
  note = "Unweighted. Difference = measured minus Friedewald. Kappa SE: simple large-sample approximation.")
print(t(se_tab)); wcsv(se_tab, "S_sensitivity_e_ldl_2011"); res$e <- se_tab

# (f) APC excluding 2011
cat("\n==== (f) ====\n")
df_ <- sub_rounds(d, c(2007, 2016, 2021)); desf <- make_design(df_)
f_apc <- bind_rows(
  data.frame(level = "all", apc_term(fit_trend(desf, dys ~ year_c + agegrp + sex + area))),
  data.frame(level = "men", apc_term(fit_trend(desf[df_$sex == "men", ], dys ~ year_c + agegrp + area))),
  data.frame(level = "women", apc_term(fit_trend(desf[df_$sex == "women", ], dys ~ year_c + agegrp + area))),
  bind_rows(lapply(AGE_LEV, function(a) data.frame(level = a, apc_term(fit_trend(desf[df_$agegrp == a, ], dys ~ year_c + sex + area))))),
  bind_rows(lapply(AREA_LEV, function(a) data.frame(level = a, apc_term(fit_trend(desf[df_$area == a, ], dys ~ year_c + agegrp + sex)))))) %>%
  mutate(analysis = "(f) excluding 2011")
f_int <- data.frame(analysis = "(f) excluding 2011", variable = c("sex", "agegrp", "area"),
  p_interaction = c(p_term(fit_trend(desf, dys ~ year_c * sex + agegrp + area), ~year_c:sex),
                    p_term(fit_trend(desf, dys ~ year_c * agegrp + sex + area), ~year_c:agegrp),
                    p_term(fit_trend(desf, dys ~ year_c * area + agegrp + sex), ~year_c:area)))
main_ref <- tr$apc %>% filter(model == "subset (stratified)", link == "log", outcome == "dys") %>% select(level, apc_main = apc, apc_main_lower = apc_lower, apc_main_upper = apc_upper)
sf <- bind_rows(f_apc %>% left_join(main_ref, by = "level") %>% mutate(part = "trend"), f_int %>% mutate(part = "interaction"))
print(as.data.frame(sf %>% mutate(across(where(is.double), ~ signif(.x, 4))))); wcsv(sf, "S_sensitivity_f_excl_2011"); res$f <- sf

# (g) 2016-2021 change alone (standardised), national, by sex, and women minus men
cat("\n==== (g) ====\n")
sg <- bind_rows(chg_main %>% filter(from == 2016, to == 2021) %>% mutate(contrast = paste("change,", level)),
                dchg_main %>% filter(from == 2016, to == 2021)) %>% mutate(part = "2016 to 2021 change (standardised, percentage points)") %>%
  select(part, analysis, contrast, from, to, est, lower, upper, se, p)
print(as.data.frame(sg)); wcsv(sg, "S_sensitivity_g_2016_2021"); res$g <- sg

# (h) strata = province only
cat("\n==== (h) ====\n")
desh <- make_design(d, strata = "prov")
hn <- cell_prev(std_design(desh, ~agegrp + sex + area, ~round_f), "dys", "round_f")
hm <- prev %>% filter(analysis == "national", type == "standardised")
h_apc <- apc_term(fit_trend(desh, dys ~ year_c + agegrp + sex + area)); m_apc <- tr$apc %>% filter(analysis == "national", model == "subset (stratified)", link == "log") %>% slice(1)
sh <- bind_rows(
  data.frame(part = "national standardised prevalence", round = as.integer(as.character(hn$round_f)), est_main = hm$est, se_main = hm$se, lower_main = hm$lower, upper_main = hm$upper,
             est_alt = hn$est, se_alt = hn$se, lower_alt = hn$lower, upper_alt = hn$upper),
  data.frame(part = "national APC", round = NA, est_main = m_apc$apc, se_main = m_apc$se_beta, lower_main = m_apc$apc_lower, upper_main = m_apc$apc_upper,
             est_alt = h_apc$apc, se_alt = h_apc$se_beta, lower_alt = h_apc$apc_lower, upper_alt = h_apc$apc_upper)) %>%
  mutate(se_ratio = se_alt / se_main, note = "main: strata = province x area; alt: strata = province only. For APC rows, se = SE of log-scale year coefficient")
print(sh); wcsv(sh, "S_sensitivity_h_strata_province"); res$h <- sh

# (i) alternative weights
cat("\n==== (i) ====\n")
desi <- make_design(d, weight = "weight_alt")
bi <- sens_block(desi, d, "dys", "(i) alternative weights (weight_alt)")
bref <- prev %>% filter(type == "standardised", analysis %in% c("national", "sex")) %>% transmute(sex = level, round, est_main = est)
si <- bind_rows(bi$est %>% left_join(bref, by = c("sex", "round")) %>% mutate(part = "prevalence"), bi$chg %>% mutate(part = "change"), bi$apc %>% mutate(part = "trend")) %>%
  mutate(weights = "2007 full-sample PS (corrected mapping); 2011 Weight_90; 2016 Weight_95; 2021 Weight_1400")
print(as.data.frame(si %>% mutate(across(where(is.double), ~ signif(.x, 4))))); wcsv(si, "S_sensitivity_i_weights"); res$i <- si
saveRDS(res, file.path(RDS, "sensitivity.rds"))
sink()
