# 16_tables.R : manuscript and supplementary tables (CSV) plus formatted twins (output/tables/formatted/).
source("R/09_functions.R")  # path relative to the repository root
sink(file.path(W, "output/logs/16_tables.log"), split = TRUE)
L <- load_data(); d <- L$d; des <- make_design(d)
prev <- readRDS(file.path(RDS, "prev_main.rds")); tr <- readRDS(file.path(RDS, "trends.rds"))
comp <- readRDS(file.path(RDS, "components.rds")); sens <- readRDS(file.path(RDS, "sensitivity.rds")); pv <- readRDS(file.path(RDS, "provinces.rds"))
chg <- read_csv(file.path(TAB, "changes_std.csv"), show_col_types = FALSE); dchg <- read_csv(file.path(TAB, "difference_in_change_std.csv"), show_col_types = FALSE)
# generic formatter: est/lower/upper -> "est (l, u)"; apc triplet with 2 decimals; p values
fmt_generic <- function(x) {
  x <- as.data.frame(x)
  if (all(c("est", "lower", "upper") %in% names(x))) { x$est_95ci <- fci(x$est, x$lower, x$upper); x$est <- NULL; x$lower <- NULL; x$upper <- NULL }
  if (all(c("apc", "apc_lower", "apc_upper") %in% names(x))) { x$apc_95ci <- fci(x$apc, x$apc_lower, x$apc_upper, f2); x$apc <- NULL; x$apc_lower <- NULL; x$apc_upper <- NULL }
  if (all(c("pr", "pr_lower", "pr_upper") %in% names(x))) { x$pr_95ci <- fci(x$pr, x$pr_lower, x$pr_upper, function(v) formatC(round(v, 3), format = "f", digits = 3)); x$pr <- NULL; x$pr_lower <- NULL; x$pr_upper <- NULL }
  for (v in grep("^p($|_)", names(x), value = TRUE)) if (is.numeric(x[[v]])) x[[v]] <- fp(x[[v]])
  for (v in names(x)) if (is.double(x[[v]])) x[[v]] <- ifelse(is.na(x[[v]]), "NA", formatC(round(x[[v]], 2), format = "f", digits = 2))
  x
}
both <- function(x, name, fx = fmt_generic) { wcsv(x, name); wfmt(fx(x), name) }

# ---- T1 characteristics ----
t1 <- list()
for (v in c("agegrp", "sex", "area")) {
  w <- svyby(as.formula(paste0("~", v)), ~round_f, des, svymean)
  wl <- as.data.frame(w) %>% select(round_f, starts_with(v)) %>% pivot_longer(-round_f, names_to = "level", values_to = "prop") %>%
    mutate(level = sub(paste0("^", v), "", level))
  se <- wl %>% filter(grepl("^se\\.", level)) %>% mutate(level = sub(paste0("^se\\.", v), "", level)) %>% rename(se = prop)
  wl <- wl %>% filter(!grepl("^se\\.", level)) %>% left_join(se, by = c("round_f", "level"))
  n <- d %>% count(round_f, level = as.character(.data[[v]]))
  t1[[v]] <- wl %>% left_join(n, by = c("round_f", "level")) %>% transmute(characteristic = v, level, round = as.integer(as.character(round_f)), n, pct_weighted = 100 * prop, se = 100 * se)
}
ma <- svyby(~age, ~round_f, des, svymean) %>% as.data.frame()
t1$age <- data.frame(characteristic = "age, years (weighted mean)", level = "mean", round = YEARS, n = as.integer(table(d$round)), pct_weighted = ma$age, se = ma$se)
t1$tot <- data.frame(characteristic = "total", level = "analytic n", round = YEARS, n = as.integer(table(d$round)), pct_weighted = NA, se = NA)
medp <- sens$a %>% filter(part == "proportion on medication", group == "all", type == "crude weighted")
t1$med <- data.frame(characteristic = c("lipid-lowering medication, 2021 (h14a)", "statin, 2016 (h19; proxy)"), level = "yes", round = c(2021, 2016),
                     n = c(sum(d$med_item[d$round == 2021] %in% TRUE), sum(d$med_item[d$round == 2016] %in% TRUE)),
                     pct_weighted = medp$est[match(c(2021, 2016), medp$round)], se = medp$se[match(c(2021, 2016), medp$round)])
T1 <- bind_rows(t1) %>% arrange(factor(characteristic, unique(characteristic)), level, round)
T1f <- T1 %>% mutate(value = ifelse(level == "mean", paste0(f1(pct_weighted), " (SE ", f2(se), ")"),
                                    ifelse(is.na(pct_weighted), format(n, big.mark = ",", trim = TRUE), paste0(format(n, big.mark = ",", trim = TRUE), " (", f1(pct_weighted), "%)")))) %>%
  select(characteristic, level, round, value) %>% pivot_wider(names_from = round, values_from = value)
wcsv(T1, "T1_characteristics"); wfmt(T1f, "T1_characteristics"); print(as.data.frame(T1f))

# ---- T2 prevalence by subgroup ----
apc_main <- tr$apc %>% filter(model == "subset (stratified)", link == "log", outcome == "dys")
pint <- tr$ints %>% filter(model == "main effects + year_c:group", link == "log") %>% select(analysis = variable, p_interaction)
ch0721 <- chg %>% filter(from == 2007, to == 2021) %>% select(analysis, level, change = est, change_lower = lower, change_upper = upper, p_change = p)
T2 <- prev %>% filter(type %in% c("standardised", "crude weighted")) %>%
  select(analysis, level, round, type, est, lower, upper, n) %>%
  pivot_wider(names_from = c(type, round), values_from = c(est, lower, upper, n), names_glue = "{type}_{round}_{.value}") %>%
  left_join(ch0721, by = c("analysis", "level")) %>%
  left_join(apc_main %>% select(analysis, level, apc, apc_lower, apc_upper, p_trend), by = c("analysis", "level")) %>%
  left_join(pint, by = "analysis") %>%
  mutate(analysis = factor(analysis, c("national", "sex", "agegrp", "area"))) %>% arrange(analysis, level)
names(T2) <- gsub(" ", "_", gsub("crude weighted", "crude", names(T2)))
T2f <- T2 %>% transmute(analysis, level,
  across(.cols = matches("^standardised_\\d{4}_est$"), .fns = ~ fci(.x, get(sub("_est$", "_lower", cur_column())), get(sub("_est$", "_upper", cur_column()))), .names = "{sub('_est$', '', .col)}"),
  across(.cols = matches("^crude_\\d{4}_est$"), .fns = ~ fci(.x, get(sub("_est$", "_lower", cur_column())), get(sub("_est$", "_upper", cur_column()))), .names = "{sub('_est$', '', .col)}"),
  across(.cols = matches("^standardised_\\d{4}_n$"), .fns = ~ format(.x, big.mark = ",", trim = TRUE), .names = "n_{sub('standardised_', '', sub('_n$', '', .col))}"),
  change_2007_2021_pp = fci(change, change_lower, change_upper), apc_pct = fci(apc, apc_lower, apc_upper, f2), p_trend = fp(p_trend), p_interaction = fp(p_interaction))
wcsv(T2, "T2_prevalence_subgroups"); wfmt(T2f, "T2_prevalence_subgroups"); print(as.data.frame(T2f))

# ---- T3 components ----
cs <- comp$est %>% filter(type == "standardised") %>% select(component, label, sex, round, est, lower, upper, n) %>%
  pivot_wider(names_from = round, values_from = c(est, lower, upper, n), names_glue = "y{round}_{.value}")
T3 <- cs %>% left_join(comp$chg %>% filter(from == 2007, to == 2021) %>% select(component, sex, change = est, change_lower = lower, change_upper = upper, p_change = p), by = c("component", "sex")) %>%
  left_join(comp$apc %>% select(component, sex, apc, apc_lower, apc_upper, p_trend, p_nonlinearity, p_interaction_sex), by = c("component", "sex")) %>%
  mutate(sex = factor(sex, c("all", "women", "men")), component = factor(component, names(comp$labels))) %>% arrange(component, sex)
T3f <- T3 %>% transmute(component = label, sex,
  y2007 = fci(y2007_est, y2007_lower, y2007_upper), y2011 = fci(y2011_est, y2011_lower, y2011_upper),
  y2016 = fci(y2016_est, y2016_lower, y2016_upper), y2021 = fci(y2021_est, y2021_lower, y2021_upper),
  change_2007_2021_pp = fci(change, change_lower, change_upper), apc_pct = fci(apc, apc_lower, apc_upper, f2), p_trend = fp(p_trend),
  p_interaction_sex = fp(p_interaction_sex))
wcsv(T3, "T3_components"); wfmt(T3f, "T3_components"); print(as.data.frame(T3f))

# ---- T4 provinces ----
T4 <- pv$full %>% select(prov, label, round, type, est, lower, upper, n, note) %>%
  left_join(pv$apc %>% select(prov, n_rounds, apc, apc_lower, apc_upper, p_trend, flag), by = "prov")
T4f <- T4 %>% mutate(cell = ifelse(is.na(est), paste0("NA (", note, ")"), paste0(fci(est, lower, upper), "; n = ", n, ifelse(grepl("not standardised", type), "; crude", "")))) %>%
  group_by(prov) %>% mutate(province = ifelse(any(round == 2007 & prov == "Tehran"), "Tehran (2007 includes Alborz)", as.character(prov))) %>% ungroup() %>%
  select(province, round, cell, apc, apc_lower, apc_upper, p_trend, flag) %>% pivot_wider(names_from = round, values_from = cell, names_prefix = "y") %>%
  mutate(apc_pct = fci(apc, apc_lower, apc_upper, f2), p_trend = fp(p_trend)) %>% select(province, y2007, y2011, y2016, y2021, apc_pct, p_trend, flag)
wcsv(T4, "T4_provinces"); wfmt(T4f, "T4_provinces"); print(as.data.frame(T4f))
wcsv(pv$apc, "T4_provinces_apc"); wcsv(bind_rows(pv$corr, data.frame(note = pv$mult$note)), "T4_provinces_notes")

# ---- supplementary tables ----
both(read_csv(file.path(TAB, "flow_by_round.csv"), show_col_types = FALSE), "S_flow", identity)
both(read_csv(file.path(TAB, "missingness_by_round.csv"), show_col_types = FALSE), "S_missingness", identity)
both(read_csv(file.path(TAB, "weights_summary.csv"), show_col_types = FALSE), "S_weights_summary")
sb <- bind_rows(chg %>% filter(!(from == 2007 & to == 2021)) %>% mutate(measure = "difference in standardised prevalence (percentage points)"),
                dchg %>% filter(!(from == 2007 & to == 2021)) %>% rename(level = contrast) %>% mutate(measure = "difference in change (percentage points)"),
                tr$prs %>% mutate(measure = "adjusted prevalence ratio (factor(year) model)") %>% rename(est = pr, lower = pr_lower, upper = pr_upper) %>% select(-outcome)) %>%
  relocate(measure, analysis, level, from, to)
both(sb, "S_between_round", function(x) { x <- as.data.frame(x); ratio <- grepl("ratio", x$measure)
  x$est_95ci <- ifelse(ratio, fci(x$est, x$lower, x$upper, function(v) formatC(round(v, 3), format = "f", digits = 3)), fci(x$est, x$lower, x$upper))
  x %>% transmute(measure, analysis, level, from, to, est_95ci, p = fp(p)) })
both(tr$lin %>% select(-set), "S_linearity")
de <- prev %>% filter(type != "unweighted") %>% select(analysis, level, round, type, n, se, deff)
dsz <- d %>% group_by(round) %>% summarise(n = n(), n_psu = n_distinct(psu), n_strata = n_distinct(strat_pooled), kish_deff_weights = 1 + (sd(weight) / mean(weight))^2)
both(de %>% left_join(dsz %>% select(-n), by = "round"), "S_design_effects")
sp <- bind_rows(stdpop %>% transmute(standard = "national (16 cells)", agegrp = as.character(agegrp), sex = as.character(sex), area = as.character(area), population, proportion),
  stdpop %>% group_by(agegrp, area) %>% summarise(population = sum(population)) %>% ungroup() %>% transmute(standard = "sex-specific (age x area)", agegrp = as.character(agegrp), sex = "both", area = as.character(area), population, proportion = population / sum(population)),
  stdpop %>% group_by(agegrp, sex) %>% summarise(population = sum(population)) %>% ungroup() %>% transmute(standard = "area-specific and provincial (age x sex)", agegrp = as.character(agegrp), sex = as.character(sex), area = "both", population, proportion = population / sum(population)),
  stdpop %>% group_by(sex, area) %>% summarise(population = sum(population)) %>% ungroup() %>% transmute(standard = "age-group-specific (sex x area)", agegrp = "all", sex = as.character(sex), area = as.character(area), population, proportion = population / sum(population)))
stopifnot(all(abs(tapply(sp$proportion, sp$standard, sum) - 1) < 1e-12))
both(sp, "S_standard_population", function(x) x %>% mutate(proportion = formatC(proportion, format = "f", digits = 4)))
both(pv$pn, "S_provincial_n", identity)
for (nm in c("S_sensitivity_a_medication", "S_sensitivity_b_no_hdl", "S_sensitivity_c_hdl40", "S_sensitivity_d_bounds", "S_sensitivity_e_ldl_2011",
             "S_sensitivity_f_excl_2011", "S_sensitivity_g_2016_2021", "S_sensitivity_h_strata_province", "S_sensitivity_i_weights"))
  wfmt(fmt_generic(read_csv(file.path(TAB, paste0(nm, ".csv")), show_col_types = FALSE)), nm)
sink()
