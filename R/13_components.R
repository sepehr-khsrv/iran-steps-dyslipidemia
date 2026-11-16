# 13_components.R : lipid components (TC >= 200, LDL-C >= 130, TG >= 150, non-HDL-C >= 160, low HDL-C):
# standardised and crude prevalence by round, overall and by sex; changes; APC overall and by sex.
source("R/09_functions.R")  # path relative to the repository root
sink(file.path(W, "output/logs/13_components.log"), split = TRUE)
L <- load_data(); d <- L$d
des <- make_design(d)
COMP <- c(c_tc = "TC >= 200 mg/dL", c_ldl = "LDL-C >= 130 mg/dL (Friedewald; TG < 400)", c_tg = "TG >= 150 mg/dL",
          c_nonhdl = "non-HDL-C >= 160 mg/dL", c_hdl = "Low HDL-C (< 40 men, < 50 women)")
est <- list(); chg <- list(); apc <- list()
for (y in names(COMP)) {
  for (sx in c("all", SEX_LEV)) {
    if (sx == "all") { by <- ~agegrp + sex + area; over <- ~round_f; vars <- "round_f" } else { by <- ~agegrp + area; over <- ~round_f + sex; vars <- c("round_f", "sex") }
    ds <- std_design(des, by, over, y = y)
    e1 <- cell_prev(ds, y, vars, "standardised"); e2 <- cell_prev(des, y, vars, "crude weighted")
    e <- bind_rows(e1, e2) %>% mutate(sex = if (sx == "all") "all" else sex) %>% filter(sex == sx)
    est[[paste(y, sx)]] <- e %>% transmute(component = y, label = COMP[[y]], sex, round = as.integer(round_f), type, est, lower, upper, se, deff, n, svyciprop_diff)
    bm <- by_means(ds, y, over)
    nm <- function(yr) if (sx == "all") as.character(yr) else paste(yr, sx, sep = ".")
    for (pp in list(c(2007, 2021), c(2007, 2011), c(2011, 2016), c(2016, 2021)))
      chg[[length(chg) + 1]] <- contr(bm, nm(pp[2]), nm(pp[1])) %>% mutate(component = y, sex = sx, from = pp[1], to = pp[2])
    if (sx == "all") {
      m <- fit_trend(des, as.formula(paste(y, "~ year_c + agegrp + sex + area")))
      ml <- fit_trend(des, as.formula(paste(y, "~ year_c + d2011 + d2016 + agegrp + sex + area")))
      mi <- fit_trend(des, as.formula(paste(y, "~ year_c * sex + agegrp + area")))
      apc[[length(apc) + 1]] <- data.frame(component = y, sex = sx, apc_term(m), p_nonlinearity = p_term(ml, ~d2011 + d2016),
                                           p_interaction_sex = p_term(mi, ~year_c:sex))
    } else {
      m <- fit_trend(des[d$sex == sx, ], as.formula(paste(y, "~ year_c + agegrp + area")))
      apc[[length(apc) + 1]] <- data.frame(component = y, sex = sx, apc_term(m), p_nonlinearity = NA, p_interaction_sex = NA)
    }
  }
}
est <- bind_rows(est); chg <- bind_rows(chg) %>% select(component, sex, from, to, est, lower, upper, se, p); apc <- bind_rows(apc)
print(as.data.frame(est %>% filter(type == "standardised") %>% mutate(across(where(is.double), ~ round(.x, 2)))))
print(as.data.frame(chg %>% filter(from == 2007, to == 2021) %>% mutate(across(where(is.double), ~ round(.x, 3)))))
print(as.data.frame(apc %>% mutate(across(where(is.double), ~ signif(.x, 4)))))
saveRDS(list(est = est, chg = chg, apc = apc, labels = COMP), file.path(RDS, "components.rds"))
wcsv(est, "components_prevalence"); wcsv(chg, "components_changes"); wcsv(apc, "components_apc")
sink()
