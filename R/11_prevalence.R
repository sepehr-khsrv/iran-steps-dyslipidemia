# 11_prevalence.R : crude and standardised prevalence of the composite, national and by sex, area, age group.
# Outputs: output/rds/prev_main.rds, output/tables/prev_main.csv, output/tables/check_standardisation_2021.csv
source("R/09_functions.R")  # path relative to the repository root
sink(file.path(W, "output/logs/11_prevalence.log"), split = TRUE)
L <- load_data(); d <- L$d
des <- make_design(d)
SPEC <- list(
  national = list(by = ~agegrp + sex + area, over = ~round_f, vars = "round_f", grp = NULL),
  sex      = list(by = ~agegrp + area,       over = ~round_f + sex, vars = c("round_f", "sex"), grp = "sex"),
  area     = list(by = ~agegrp + sex,        over = ~round_f + area, vars = c("round_f", "area"), grp = "area"),
  agegrp   = list(by = ~sex + area,          over = ~round_f + agegrp, vars = c("round_f", "agegrp"), grp = "agegrp"))
tidy_prev <- function(x, analysis, grp) {
  x %>% mutate(analysis = analysis, level = if (is.null(grp)) "all" else .data[[grp]], round = as.integer(round_f)) %>%
    select(analysis, level, round, type, est, lower, upper, se, deff, n, svyciprop_diff)
}
out <- list()
for (nm in names(SPEC)) {
  s <- SPEC[[nm]]
  ds <- std_design(des, s$by, s$over)
  out[[paste(nm, "std")]] <- tidy_prev(cell_prev(ds, "dys", s$vars, "standardised"), nm, s$grp)
  out[[paste(nm, "crude")]] <- tidy_prev(cell_prev(des, "dys", s$vars, "crude weighted"), nm, s$grp)
}
# unweighted national (simple random sample formulae)
des_unw <- svydesign(ids = ~1, data = d)
out[["national unweighted"]] <- tidy_prev(cell_prev(des_unw, "dys", "round_f", "unweighted"), "national", NULL) %>% mutate(deff = 1)
prev <- bind_rows(out) %>% arrange(analysis, level, type, round)
print(as.data.frame(prev %>% mutate(across(where(is.numeric), ~ round(.x, 2)))))
saveRDS(prev, file.path(RDS, "prev_main.rds")); wcsv(prev, "prev_main")

# ---- Check 1: standardised national 2021 = census-weighted average of the 16 cell-specific weighted prevalences ----
cells <- svyby(~dys, ~agegrp + sex + area, subset(des, round == 2021), svymean)
cells <- as.data.frame(cells) %>% mutate(across(c(agegrp, sex, area), as.character)) %>%
  left_join(stdpop %>% mutate(across(c(agegrp, sex, area), as.character)), by = c("agegrp", "sex", "area"))
hand <- 100 * sum(cells$dys * cells$proportion)
svy <- prev %>% filter(analysis == "national", type == "standardised", round == 2021) %>% pull(est)
# women 2021: age x area standard of all adults
cw <- svyby(~dys, ~agegrp + area, subset(des, round == 2021 & sex == "women"), svymean) %>% as.data.frame() %>%
  mutate(across(c(agegrp, area), as.character)) %>%
  left_join(stdpop %>% group_by(agegrp = as.character(agegrp), area = as.character(area)) %>% summarise(p = sum(proportion)), by = c("agegrp", "area"))
hand_w <- 100 * sum(cw$dys * cw$p)
svy_w <- prev %>% filter(analysis == "sex", level == "women", type == "standardised", round == 2021) %>% pull(est)
chk <- data.frame(check = c("national 2021, 16 cells", "women 2021, 8 cells"), by_hand = c(hand, hand_w), svystandardize = c(svy, svy_w))
chk$difference <- chk$by_hand - chk$svystandardize; print(chk); stopifnot(all(abs(chk$difference) < 1e-5))
write_csv(cells %>% select(agegrp, sex, area, prevalence = dys, se, std_proportion = proportion), file.path(TAB, "check_standardisation_2021_cells.csv"))
wcsv(chk, "check_standardisation_2021")

# ---- Check 2: pooled-design domains vs round-specific designs (lonely-PSU adjustment) ----
lp <- bind_rows(lapply(YEARS, function(y) {
  dr <- d %>% filter(round == y)
  dd <- svydesign(ids = ~psu, strata = ~strat_pooled, weights = ~weight, nest = TRUE, data = dr)
  m1 <- svymean(~dys, dd); m2 <- svymean(~dys, subset(des, round == y))
  data.frame(round = y, est_round_design = 100 * as.numeric(coef(m1)), se_round_design = 100 * as.numeric(SE(m1)), est_pooled = 100 * as.numeric(coef(m2)), se_pooled = 100 * as.numeric(SE(m2)))
}))
lp$se_ratio <- lp$se_pooled / lp$se_round_design; print(lp); wcsv(lp, "check_pooled_vs_round_design")
sink()
