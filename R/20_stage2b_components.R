# 20_stage2b_components.R : Stage 2b tasks 1 and 3.
# (1) Between-round changes for each component and the composite, overall and by sex: standardised differences
#     (independent rounds, as in changes_std.csv / components_changes.csv) and prevalence ratios from the
#     categorical-year model (same specification as prevalence_ratios.csv in R/12).
# (3) Within-laboratory change 2016 to 2021 (Table 5).
# Outputs: output/tables/S_components_between_round.csv, T5_within_lab_2016_2021.csv (+ formatted), output/rds/stage2b_components.rds
source("R/09_functions.R")  # path relative to the repository root
sink(file.path(W, "output/logs/20_stage2b_components.log"), split = TRUE)
L <- load_data(); d <- L$d
des <- make_design(d)
comp <- readRDS(file.path(RDS, "components.rds")); prev <- readRDS(file.path(RDS, "prev_main.rds"))
chg_main <- read_csv(file.path(TAB, "changes_std.csv"), show_col_types = FALSE)
pr_main <- read_csv(file.path(TAB, "prevalence_ratios.csv"), show_col_types = FALSE)
OUT <- c(dys = "Composite dyslipidaemia", comp$labels)
PERIODS <- list(c(2007, 2011), c(2011, 2016), c(2016, 2021))

# ---- standardised differences: reuse the Stage 2 contrasts (same functions, same design) ----
dif <- bind_rows(
  chg_main %>% filter(analysis == "national" | analysis == "sex") %>% transmute(outcome = "dys", sex = level, from, to, est, lower, upper, se, p),
  comp$chg %>% transmute(outcome = component, sex, from, to, est, lower, upper, se, p)) %>%
  filter(paste(from, to) %in% sapply(PERIODS, paste, collapse = " "))
stopifnot(nrow(dif) == length(OUT) * 3 * length(PERIODS))

# ---- prevalence ratios from the categorical-year model (R/12 run_block specification) ----
pr_one <- function(dsg, y, covs) {
  mf <- fit_trend(dsg, as.formula(paste(y, "~", paste(c("round_f", covs), collapse = " + "))))
  b <- coef(mf); V <- vcov(mf)
  bind_rows(lapply(PERIODS, function(pp) {
    w <- setNames(rep(0, length(b)), names(b))
    if (pp[2] != 2007) w[paste0("round_f", pp[2])] <- 1
    if (pp[1] != 2007) w[paste0("round_f", pp[1])] <- -1
    e <- sum(w * b); se <- sqrt(as.numeric(t(w) %*% V %*% w))
    data.frame(from = pp[1], to = pp[2], pr = exp(e), pr_lower = exp(e - Z * se), pr_upper = exp(e + Z * se), pr_p = 2 * pnorm(-abs(e / se)))
  }))
}
prs <- list()
for (y in names(OUT)) for (sx in c("all", SEX_LEV)) {
  r <- if (sx == "all") pr_one(des, y, c("agegrp", "sex", "area")) else pr_one(des[d$sex == sx, ], y, c("agegrp", "area"))
  prs[[length(prs) + 1]] <- r %>% mutate(outcome = y, sex = sx)
}
prs <- bind_rows(prs)
# check: composite PRs equal the Stage 2 prevalence_ratios.csv
ck <- prs %>% filter(outcome == "dys") %>% left_join(pr_main %>% filter(outcome == "dys") %>% transmute(sex = level, from, to, pr0 = pr, l0 = pr_lower), by = c("sex", "from", "to"))
cat("Composite PR vs prevalence_ratios.csv, max |diff|:", max(abs(c(ck$pr - ck$pr0, ck$pr_lower - ck$l0))), "\n"); stopifnot(max(abs(ck$pr - ck$pr0)) < 1e-10)

S <- dif %>% left_join(prs, by = c("outcome", "sex", "from", "to")) %>%
  mutate(label = OUT[outcome], outcome = factor(outcome, names(OUT)), sex = factor(sex, c("all", "women", "men"))) %>%
  arrange(outcome, sex, from) %>% mutate(outcome = as.character(outcome), sex = as.character(sex)) %>%
  select(outcome, label, sex, from, to, est, lower, upper, se, p, pr, pr_lower, pr_upper, pr_p)
stopifnot(!anyNA(S), all(S$lower <= S$est & S$est <= S$upper), all(S$pr_lower <= S$pr & S$pr <= S$pr_upper))
print(as.data.frame(S %>% mutate(across(where(is.double), ~ round(.x, 3)))))
wcsv(S, "S_components_between_round")
wfmt(S %>% transmute(outcome = label, sex, period = paste(from, "to", to), difference_pp_95ci = fci(est, lower, upper), p = fp(p),
                     pr_95ci = fci(pr, pr_lower, pr_upper, function(v) formatC(round(v, 3), format = "f", digits = 3)), p_pr = fp(pr_p)),
     "S_components_between_round")

# ---- Table 5: within-laboratory change 2016 to 2021 ----
pe <- bind_rows(
  prev %>% filter(type == "standardised", analysis %in% c("national", "sex"), round %in% c(2016, 2021)) %>% transmute(outcome = "dys", sex = level, round, est, lower, upper, n),
  comp$est %>% filter(type == "standardised", round %in% c(2016, 2021)) %>% transmute(outcome = component, sex, round, est, lower, upper, n))
stopifnot(nrow(pe) == length(OUT) * 3 * 2)
wide <- pe %>% pivot_wider(names_from = round, values_from = c(est, lower, upper, n), names_glue = "{.value}_{round}")
T5 <- S %>% filter(from == 2016, to == 2021) %>% select(outcome, label, sex, change = est, change_lower = lower, change_upper = upper, change_p = p, pr, pr_lower, pr_upper, pr_p) %>%
  left_join(wide, by = c("outcome", "sex")) %>%
  select(outcome, label, sex, n_2016, est_2016, lower_2016, upper_2016, n_2021, est_2021, lower_2021, upper_2021, change, change_lower, change_upper, change_p, pr, pr_lower, pr_upper, pr_p)
stopifnot(!anyNA(T5), max(abs(T5$est_2021 - T5$est_2016 - T5$change)) < 1e-8)
wcsv(T5, "T5_within_lab_2016_2021")
wfmt(T5 %>% transmute(outcome = label, sex, prevalence_2016_95ci = fci(est_2016, lower_2016, upper_2016), prevalence_2021_95ci = fci(est_2021, lower_2021, upper_2021),
                      change_pp_95ci = fci(change, change_lower, change_upper), p_change = fp(change_p),
                      pr_95ci = fci(pr, pr_lower, pr_upper, function(v) formatC(round(v, 3), format = "f", digits = 3)), p_pr = fp(pr_p), n_2016, n_2021),
     "T5_within_lab_2016_2021")
print(as.data.frame(T5 %>% mutate(across(where(is.double), ~ round(.x, 3)))))
saveRDS(list(between = S, t5 = T5), file.path(RDS, "stage2b_components.rds"))
sink()
