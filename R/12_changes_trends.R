# 12_changes_trends.R : absolute changes, between-round differences, differences in change, trend models (APC),
# interaction tests, departure from log-linearity, between-round prevalence ratios, logit-link sensitivity.
source("R/09_functions.R")  # path relative to the repository root
sink(file.path(W, "output/logs/12_changes_trends.log"), split = TRUE)
L <- load_data(); d <- L$d
des <- make_design(d)
prev <- readRDS(file.path(RDS, "prev_main.rds"))
PERIODS <- list(c(2007, 2021), c(2007, 2011), c(2011, 2016), c(2016, 2021))
SPEC <- list(
  national = list(by = ~agegrp + sex + area, over = ~round_f, grp = NULL, ref = NULL),
  sex      = list(by = ~agegrp + area, over = ~round_f + sex, grp = "sex", ref = "men"),
  area     = list(by = ~agegrp + sex, over = ~round_f + area, grp = "area", ref = "rural"),
  agegrp   = list(by = ~sex + area, over = ~round_f + agegrp, grp = "agegrp", ref = "25-34"))
LEVELS <- list(sex = SEX_LEV, area = AREA_LEV, agegrp = AGE_LEV)

# ---- changes on the standardised scale ----
nmk <- function(y, lev) if (is.null(lev)) as.character(y) else paste(y, lev, sep = ".")
chg <- list(); dchg <- list(); maxcov <- 0
for (an in names(SPEC)) {
  s <- SPEC[[an]]
  bm <- by_means(std_design(des, s$by, s$over), "dys", s$over)
  V <- vcov(bm); yr <- sub("\\..*$", "", names(coef(bm)))
  maxcov <- max(maxcov, max(abs(V[outer(yr, yr, "!=")])))
  levs <- if (is.null(s$grp)) list(NULL) else as.list(LEVELS[[s$grp]])
  for (lv in levs) for (pp in PERIODS) {
    chg[[length(chg) + 1]] <- contr(bm, nmk(pp[2], lv), nmk(pp[1], lv)) %>%
      mutate(analysis = an, level = if (is.null(lv)) "all" else lv, from = pp[1], to = pp[2])
  }
  if (!is.null(s$grp)) for (lv in setdiff(LEVELS[[s$grp]], s$ref)) for (pp in PERIODS) {
    dchg[[length(dchg) + 1]] <- contr(bm, c(nmk(pp[2], lv), nmk(pp[1], s$ref)), c(nmk(pp[1], lv), nmk(pp[2], s$ref))) %>%
      mutate(analysis = an, contrast = paste(lv, "minus", s$ref), from = pp[1], to = pp[2])
  }
}
cat("Largest absolute cross-round covariance (should be ~0):", maxcov, "\n")
chg <- bind_rows(chg) %>% select(analysis, level, from, to, est, lower, upper, se, p)
dchg <- bind_rows(dchg) %>% select(analysis, contrast, from, to, est, lower, upper, se, p)
# check: SE of change = sqrt(se1^2 + se2^2) from the prevalence table
ps <- prev %>% filter(type == "standardised") %>% select(analysis, level, round, se)
ck <- chg %>% left_join(ps %>% rename(from = round, se1 = se), by = c("analysis", "level", "from")) %>%
  left_join(ps %>% rename(to = round, se2 = se), by = c("analysis", "level", "to")) %>% mutate(se_indep = sqrt(se1^2 + se2^2), dif = se - se_indep)
cat("Max |SE(covmat) - sqrt(se1^2+se2^2)|:", max(abs(ck$dif)), "\n"); stopifnot(max(abs(ck$dif)) < 1e-6)
print(as.data.frame(chg %>% mutate(across(where(is.double), ~ round(.x, 3)))))
print(as.data.frame(dchg %>% mutate(across(where(is.double), ~ round(.x, 3)))))
wcsv(chg, "changes_std"); wcsv(dchg, "difference_in_change_std")

# ---- trend models ----
COV <- c("agegrp", "sex", "area")
fml <- function(lhs, rhs) as.formula(paste(lhs, "~", paste(rhs, collapse = " + ")))
apc_rows <- list(); int_rows <- list(); lin_rows <- list(); pr_rows <- list()
run_block <- function(dsg, analysis, level, covs, y = "dys", tag = "main") {
  m <- fit_trend(dsg, fml(y, c("year_c", covs)))
  ml <- fit_trend(dsg, fml(y, c("year_c", "d2011", "d2016", covs)))
  mf <- fit_trend(dsg, fml(y, c("round_f", covs)))
  apc_rows[[length(apc_rows) + 1]] <<- data.frame(analysis = analysis, level = level, model = "subset (stratified)", link = "log", outcome = y, set = tag, apc_term(m))
  lin_rows[[length(lin_rows) + 1]] <<- data.frame(analysis = analysis, level = level, outcome = y, set = tag, df = 2, p_nonlinearity = p_term(ml, ~d2011 + d2016))
  b <- coef(mf); V <- vcov(mf)
  for (pp in list(c(2007, 2011), c(2011, 2016), c(2016, 2021), c(2007, 2021))) {
    w <- setNames(rep(0, length(b)), names(b))
    if (pp[2] != 2007) w[paste0("round_f", pp[2])] <- 1
    if (pp[1] != 2007) w[paste0("round_f", pp[1])] <- -1
    e <- sum(w * b); se <- sqrt(as.numeric(t(w) %*% V %*% w))
    pr_rows[[length(pr_rows) + 1]] <<- data.frame(analysis = analysis, level = level, outcome = y, from = pp[1], to = pp[2],
      pr = exp(e), pr_lower = exp(e - Z * se), pr_upper = exp(e + Z * se), p = 2 * pnorm(-abs(e / se)))
  }
  m
}
m_nat <- run_block(des, "national", "all", COV)
for (g in names(LEVELS)) for (lv in LEVELS[[g]]) {
  dsub <- des[d[[g]] == lv, ]
  run_block(dsub, g, lv, setdiff(COV, g))
}
# interaction models (spec): main model + year_c:group ; subgroup APCs via svycontrast; logit-link sensitivity
for (g in names(LEVELS)) {
  for (lk in c("log", "logit")) {
    mi <- fit_trend(des, fml("dys", c(paste0("year_c*", g), setdiff(COV, g))), link = lk)
    int_rows[[length(int_rows) + 1]] <- data.frame(variable = g, model = "main effects + year_c:group", link = lk,
      df = length(LEVELS[[g]]) - 1, p_interaction = p_term(mi, as.formula(paste0("~year_c:", g))))
    if (lk == "log") for (lv in LEVELS[[g]]) {
      w <- c(year_c = 1); if (lv != LEVELS[[g]][1]) w[paste0("year_c:", g, lv)] <- 1
      apc_rows[[length(apc_rows) + 1]] <- data.frame(analysis = g, level = lv, model = "interaction (common covariate effects)", link = "log", outcome = "dys", set = "main", apc_contrast(mi, w))
    }
    # fully interacted model: every covariate interacted with the group; its group-specific year slopes equal the subset models
    mfi <- fit_trend(des, as.formula(paste0("dys ~ ", g, " * (", paste(c("year_c", setdiff(COV, g)), collapse = " + "), ")")), link = lk)
    int_rows[[length(int_rows) + 1]] <- data.frame(variable = g, model = "fully interacted (group x all terms)", link = lk,
      df = length(LEVELS[[g]]) - 1, p_interaction = p_term(mfi, as.formula(paste0("~", g, ":year_c"))))
    if (lk == "log") for (lv in LEVELS[[g]]) {
      w <- c(year_c = 1); if (lv != LEVELS[[g]][1]) w[paste0(g, lv, ":year_c")] <- 1
      apc_rows[[length(apc_rows) + 1]] <- data.frame(analysis = g, level = lv, model = "fully interacted", link = "log", outcome = "dys", set = "main", apc_contrast(mfi, w))
    }
  }
}
# national logit-link year term (odds ratio per year) for reference
ml_nat <- fit_trend(des, fml("dys", c("year_c", COV)), link = "logit")
apc_rows[[length(apc_rows) + 1]] <- data.frame(analysis = "national", level = "all", model = "subset (stratified)", link = "logit (APC column = (OR-1)x100)", outcome = "dys", set = "main", apc_term(ml_nat))
apc <- bind_rows(apc_rows); ints <- bind_rows(int_rows); lin <- bind_rows(lin_rows); prs <- bind_rows(pr_rows)
# ---- checks ----
cat("\nNational APC from pooled year coefficient:", 100 * (exp(coef(m_nat)[["year_c"]]) - 1), "\n")
cmp <- apc %>% filter(analysis != "national", link == "log") %>% select(analysis, level, model, apc) %>%
  pivot_wider(names_from = model, values_from = apc)
print(as.data.frame(cmp))
saveRDS(list(apc = apc, ints = ints, lin = lin, prs = prs, cmp = cmp, maxcov = maxcov), file.path(RDS, "trends.rds"))
print(as.data.frame(apc %>% mutate(across(where(is.double), ~ signif(.x, 4)))))
print(ints); print(lin); print(as.data.frame(prs %>% mutate(across(where(is.double), ~ round(.x, 4)))))
wcsv(apc, "trend_apc"); wcsv(ints, "trend_interactions"); wcsv(lin, "S_linearity_raw"); wcsv(prs, "prevalence_ratios")
sink()
