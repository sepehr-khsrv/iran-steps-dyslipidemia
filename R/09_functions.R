# 09_functions.R : shared settings and helper functions for Stage 2 (sourced by R/10-18).
suppressMessages({library(survey); library(dplyr); library(tidyr); library(readr)})
options(survey.lonely.psu = "adjust", dplyr.summarise.inform = FALSE, scipen = 999)
W <- "."  # repository root: run from the root folder
Z <- qnorm(0.975)
AGE_LEV <- c("25-34", "35-44", "45-54", "55-64"); SEX_LEV <- c("men", "women"); AREA_LEV <- c("rural", "urban")
YEARS <- c(2007L, 2011L, 2016L, 2021L)
RDS <- file.path(W, "output/rds"); TAB <- file.path(W, "output/tables"); FIG <- file.path(W, "output/figures")
dir.create(RDS, showWarnings = FALSE, recursive = TRUE)

stdpop <- read_csv(file.path(TAB, "standard_population_2016.csv"), show_col_types = FALSE) %>%
  mutate(agegrp = factor(agegrp, AGE_LEV), sex = factor(sex, SEX_LEV), area = factor(area, AREA_LEV))
stopifnot(nrow(stdpop) == 16, abs(sum(stdpop$proportion) - 1) < 1e-12)

# ---- designs ----
make_design <- function(df, weight = "weight", strata = "pooled") {
  df$w_use <- df[[weight]]
  df$strat_use <- if (strata == "pooled") df$strat_pooled else df$strat_prov
  svydesign(ids = ~psu, strata = ~strat_use, weights = ~w_use, nest = TRUE, data = df)
}

# Standard population vector aligned (by name) to the order svystandardize uses: as.data.frame(svytable(by, design)).
pop_vector <- function(design, by) {
  vars <- all.vars(by)
  fm <- as.data.frame(svytable(by, design))
  sp <- stdpop %>% group_by(across(all_of(vars))) %>% summarise(pop = sum(population), .groups = "drop")
  for (v in vars) { fm[[v]] <- as.character(fm[[v]]); sp[[v]] <- as.character(sp[[v]]) }
  m <- left_join(fm[, vars, drop = FALSE], sp, by = vars)
  stopifnot(nrow(m) == nrow(fm), nrow(m) == nrow(sp), !anyNA(m$pop),
            identical(do.call(paste, m[vars]), do.call(paste, fm[vars])))
  m$pop
}
std_design <- function(design, by, over, y = NULL) {
  pv <- pop_vector(design, by)
  em <- if (is.null(y)) NULL else as.formula(paste0("~", y))
  svystandardize(design, by = by, over = over, population = pv, excluding.missing = em)
}


# svyciprop(method = "logit") reproduced with a starting value. survey::svyciprop fits svyglm(y ~ 1, quasibinomial)
# without a start value; in small domains of a design with many zero weights the IRLS fit can diverge (intercept ~1e15)
# and return NaN or wrong limits. This function runs svyciprop, then refits the same model with start = logit(p).
# The refit is used for every estimate; attr "svyciprop_diff" records the largest difference from svyciprop's limits.
ciprop_logit <- function(f, design, level = 0.95) {
  r0 <- suppressWarnings(svyciprop(f, design, method = "logit", level = level))
  mn <- svymean(f, design, na.rm = TRUE); p <- as.numeric(coef(mn))
  y <- all.vars(f)[1]
  st <- qlogis(p)
  m <- suppressWarnings(eval(bquote(svyglm(.(as.formula(paste(y, "~ 1"))), design = design, family = quasibinomial(), start = .(st)))))
  ci <- plogis(as.numeric(confint(m, 1, level = level, method = "Wald", df = degf(design))))
  stopifnot(abs(plogis(coef(m)[[1]]) - p) < 1e-8)
  d0 <- max(abs(ci - as.numeric(attr(r0, "ci"))))
  rval <- p; attr(rval, "var") <- vcov(mn); attr(rval, "ci") <- ci
  attr(rval, "method") <- "svyciprop logit (svyglm refit with start = logit(p))"; attr(rval, "svyciprop_diff") <- ifelse(is.finite(d0), d0, Inf)
  class(rval) <- "svyciprop"; rval
}

# ---- prevalence per cell: svyciprop(logit) + SE + deff + n ----
cell_prev <- function(design, y, over_vars, type = "standardised") {
  dv <- design$variables; w <- weights(design, "sampling")
  ok <- w > 0 & !is.na(dv[[y]])
  keys <- unique(as.data.frame(lapply(dv[ok, over_vars, drop = FALSE], as.character), stringsAsFactors = FALSE))
  keys <- keys[do.call(order, keys), , drop = FALSE]
  f <- as.formula(paste0("~", y))
  res <- lapply(seq_len(nrow(keys)), function(i) {
    sel <- ok; for (v in over_vars) sel <- sel & (as.character(dv[[v]]) == keys[[v]][i])
    sel[is.na(sel)] <- FALSE
    ds <- design[sel, ]
    r <- ciprop_logit(f, ds)
    m <- svymean(f, ds, deff = TRUE, na.rm = TRUE)
    data.frame(keys[i, , drop = FALSE], type = type, est = 100 * as.numeric(r), lower = 100 * attr(r, "ci")[1], upper = 100 * attr(r, "ci")[2],
               se = 100 * as.numeric(SE(r)), deff = as.numeric(deff(m)), n = sum(sel), ci_method = attr(r, "method"),
               svyciprop_diff = attr(r, "svyciprop_diff"), stringsAsFactors = FALSE, row.names = NULL)
  })
  bind_rows(res)
}

# ---- svyby means with covariance, and contrasts ----
by_means <- function(design, y, over) svyby(as.formula(paste0("~", y)), over, design, svymean, covmat = TRUE, na.rm = TRUE)
contr <- function(bm, plus, minus, label = NA_character_) {
  w <- c(setNames(rep(1, length(plus)), plus), setNames(rep(-1, length(minus)), minus))
  stopifnot(all(names(w) %in% names(coef(bm))))
  v <- rep(0, length(coef(bm))); names(v) <- names(coef(bm)); v[names(w)] <- w
  est <- sum(v * coef(bm)); se <- sqrt(as.numeric(t(v) %*% vcov(bm) %*% v))
  data.frame(contrast = label, est = 100 * est, lower = 100 * (est - Z * se), upper = 100 * (est + Z * se), se = 100 * se,
             p = 2 * pnorm(-abs(est / se)), stringsAsFactors = FALSE)
}

# ---- trend models ----
fit_trend <- function(design, fml, link = "log") {
  fam <- if (link == "log") quasipoisson(link = "log") else quasibinomial(link = "logit")
  svyglm(fml, design = design, family = fam)
}
apc_term <- function(m, term = "year_c") {
  b <- coef(m)[[term]]; se <- sqrt(vcov(m)[term, term])
  p <- as.numeric(regTermTest(m, as.formula(paste0("~", term)), method = "Wald")$p)
  data.frame(beta = b, se_beta = se, apc = 100 * (exp(b) - 1), apc_lower = 100 * (exp(b - Z * se) - 1), apc_upper = 100 * (exp(b + Z * se) - 1), p_trend = p)
}
apc_contrast <- function(m, w) {
  r <- svycontrast(m, w); b <- as.numeric(coef(r)); se <- as.numeric(SE(r))
  data.frame(beta = b, se_beta = se, apc = 100 * (exp(b) - 1), apc_lower = 100 * (exp(b - Z * se) - 1), apc_upper = 100 * (exp(b + Z * se) - 1),
             p_trend = 2 * pt(-abs(b / se), df = m$df.residual))
}
p_term <- function(m, fml) as.numeric(regTermTest(m, fml, method = "Wald")$p)

# ---- formatting ----
nz <- function(x) { x[!is.na(x) & x == 0] <- 0; x }  # drops negative zero after rounding
f1 <- function(x) ifelse(is.na(x), "NA", formatC(nz(round(x, 1)), format = "f", digits = 1))
f2 <- function(x) ifelse(is.na(x), "NA", formatC(nz(round(x, 2)), format = "f", digits = 2))
fci <- function(e, l, u, f = f1) ifelse(is.na(e), "NA", paste0(f(e), " (", f(l), ", ", f(u), ")"))
fp <- function(p) ifelse(is.na(p), "NA", ifelse(p < 0.001, "<0.001", formatC(p, format = "f", digits = 3)))
wcsv <- function(x, name) write_csv(x, file.path(TAB, paste0(name, ".csv")), na = "NA")
wfmt <- function(x, name) write_csv(x, file.path(TAB, "formatted", paste0(name, "_formatted.csv")), na = "NA")
load_data <- function() readRDS(file.path(RDS, "stage2_data.rds"))
