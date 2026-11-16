# 20_stage2b_fasting.R : Stage 2b task 2. Fasting check, 2021 only.
# 2021 records the clock time of the last food (fasting1) and of blood sampling (fasting2) as free text (Q 1400, blood-sampling form;
# participants were told to fast at least 12 hours). R/01_harmonise.R parsed these to approximate hours (fasting_hours, fasting_flag).
# 2007 and 2011 have no fasting item; 2016 asked (Q 1395 items 165-166, b1/b1f) but the items are not in the released data.
# (1) weighted distribution of fasting status in the 2021 analytic sample;
# (2) 2021 standardised prevalence of TG >= 150 and of the composite, all participants vs those with a parsed fast of >= 8 h
#     (primary) and >= 12 h (the protocol's instruction; secondary), overall and by sex. The restricted sample is a subset of the
#     full sample, so the difference (restricted minus all) uses the joint linearised score of the two standardised estimates.
# Outputs: output/tables/S_fasting_2021.csv (+ formatted), output/rds/stage2b_fasting.rds
source("R/09_functions.R")  # path relative to the repository root
sink(file.path(W, "output/logs/20_stage2b_fasting.log"), split = TRUE)
L <- load_data(); d <- L$d
cat("Fasting flag by round (analytic sample):\n"); print(table(d$round, d$fasting_flag))
stopifnot(all(d$fasting_flag[d$round != 2021] == "not recorded"))
FCAT <- c("not recorded", "unparsable", "implausible (< 4 h or > 20 h)", "4 to < 8 h", "8 to < 12 h", "12 to 20 h")
d <- d %>% mutate(
  fcat = case_when(round != 2021 ~ NA_character_,
                   fasting_flag == "not recorded" ~ FCAT[1], fasting_flag == "unparsable" ~ FCAT[2],
                   fasting_flag == "parsed, implausible duration" ~ FCAT[3],
                   fasting_hours < 8 ~ FCAT[4], fasting_hours < 12 ~ FCAT[5], TRUE ~ FCAT[6]),
  f_ge8 = round == 2021 & fasting_flag == "parsed (approximate)" & fasting_hours >= 8,
  f_ge12 = round == 2021 & fasting_flag == "parsed (approximate)" & fasting_hours >= 12,
  g8 = ifelse(round == 2021, ifelse(f_ge8, "yes", "no"), "not recorded"),
  g12 = ifelse(round == 2021, ifelse(f_ge12, "yes", "no"), "not recorded"))
stopifnot(!anyNA(d$fcat[d$round == 2021]))
for (k in seq_along(FCAT)) d[[paste0("fc", k)]] <- ifelse(d$round == 2021, as.numeric(d$fcat == FCAT[k]), NA_real_)
des <- make_design(d)

# ---- (1) distribution (crude weighted, logit CI) ----
dist <- bind_rows(lapply(seq_along(FCAT), function(k) cell_prev(des, paste0("fc", k), "round_f", "crude weighted") %>% mutate(category = FCAT[k]))) %>%
  transmute(part = "distribution of fasting status (2021)", outcome = "fasting status", category, sex = "all", sample = "2021 analytic sample",
            type, est, lower, upper, se, n_category = as.integer(sapply(FCAT, function(z) sum(d$fcat == z, na.rm = TRUE))), n = sum(d$round == 2021))
stopifnot(abs(sum(dist$est) - 100) < 1e-8)
hh <- d$fasting_hours[d$round == 2021 & d$fasting_flag == "parsed (approximate)"]; ww <- d$weight[d$round == 2021 & d$fasting_flag == "parsed (approximate)"]
o <- order(hh); wmed <- hh[o][which(cumsum(ww[o]) / sum(ww) >= 0.5)[1]]
cat("\nWeighted median fasting hours (parsed):", wmed, "; unweighted median:", median(hh), "\n")
print(as.data.frame(dist %>% mutate(across(where(is.double), ~ round(.x, 2)))))

# ---- (2) standardised prevalence, all vs restricted ----
# linearised score of a directly standardised mean in domain `dom` with cell proportions sp (svystandardize convention:
# scores residualised within the standardisation cells, i.e. post-stratification)
std_score <- function(df, y, by_vars, dom) {
  sp <- stdpop %>% group_by(across(all_of(by_vars))) %>% summarise(pop = sum(population), .groups = "drop") %>% mutate(s = pop / sum(pop)) %>%
    mutate(across(all_of(by_vars), as.character))
  cell <- do.call(paste, lapply(by_vars, function(v) as.character(df[[v]])))
  scell <- sp$s[match(cell, do.call(paste, sp[by_vars]))]
  w <- df$weight; yy <- df[[y]]; dm <- dom & !is.na(yy)
  Tc <- tapply(w[dm], cell[dm], sum); Yc <- tapply((w * yy)[dm], cell[dm], sum); ybar <- Yc / Tc
  stopifnot(length(Tc) == nrow(sp))
  z <- ifelse(dm, scell * (yy - ybar[cell]) / Tc[cell], 0); z[is.na(z)] <- 0
  list(est = sum(sp$s * ybar[do.call(paste, sp[by_vars])]), z = z)
}
OUT <- c(c_tg = "TG >= 150 mg/dL", dys = "Composite dyslipidaemia")
RES <- list()
for (y in names(OUT)) for (sx in c("all", SEX_LEV)) for (rr in c("g8", "g12")) {
  if (sx == "all") { by <- ~agegrp + sex + area; bv <- c("agegrp", "sex", "area"); ov <- c("round_f"); insx <- rep(TRUE, nrow(d)) } else {
    by <- ~agegrp + area; bv <- c("agegrp", "area"); ov <- c("round_f", "sex"); insx <- d$sex == sx }
  ea <- cell_prev(std_design(des, by, as.formula(paste("~", paste(ov, collapse = " + "))), y = y), y, ov) %>% filter(round_f == "2021", if (sx == "all") TRUE else sex == sx)
  er <- cell_prev(std_design(des, by, as.formula(paste("~", paste(c(ov, rr), collapse = " + "))), y = y), y, c(ov, rr)) %>%
    filter(round_f == "2021", .data[[rr]] == "yes", if (sx == "all") TRUE else sex == sx)
  stopifnot(nrow(ea) == 1, nrow(er) == 1)
  sa <- std_score(d, y, bv, d$round == 2021 & insx); sr <- std_score(d, y, bv, d$round == 2021 & insx & d[[rr]] == "yes")
  # checks: hand-computed standardised estimates and their linearised SEs reproduce svystandardize
  dz <- function(z) { dd <- d; dd$zz <- z; as.numeric(SE(svytotal(~zz, make_design(dd)))) * 100 }
  se_a <- dz(sa$z); se_r <- dz(sr$z)
  stopifnot(abs(100 * sa$est - ea$est) < 1e-8, abs(100 * sr$est - er$est) < 1e-8, abs(se_a / ea$se - 1) < 1e-6, abs(se_r / er$se - 1) < 1e-6)
  se_d <- dz(sr$z - sa$z); dif <- er$est - ea$est
  thr <- ifelse(rr == "g8", ">= 8 h", ">= 12 h")
  RES[[length(RES) + 1]] <- bind_rows(
    data.frame(part = "standardised prevalence 2021", outcome = y, category = "all participants", sex = sx, sample = "all", type = "standardised",
               est = ea$est, lower = ea$lower, upper = ea$upper, se = ea$se, n = ea$n, threshold = thr),
    data.frame(part = "standardised prevalence 2021", outcome = y, category = paste("fasting", thr), sex = sx, sample = paste("fasting", thr), type = "standardised",
               est = er$est, lower = er$lower, upper = er$upper, se = er$se, n = er$n, threshold = thr),
    data.frame(part = "difference (restricted minus all)", outcome = y, category = paste("fasting", thr, "minus all"), sex = sx, sample = paste("fasting", thr), type = "standardised",
               est = dif, lower = dif - Z * se_d, upper = dif + Z * se_d, se = se_d, n = er$n, threshold = thr, p = 2 * pnorm(-abs(dif / se_d))))
}
res <- bind_rows(RES) %>% distinct(part, outcome, category, sex, .keep_all = TRUE)  # "all participants" rows appear once per outcome x sex
S <- bind_rows(dist, res %>% mutate(threshold = ifelse(sample == "all", NA_character_, threshold))) %>%
  mutate(label = ifelse(outcome %in% names(OUT), OUT[outcome], outcome)) %>% relocate(part, outcome, label)
# consistency with the Stage 2 tables
chk_all <- res %>% filter(sample == "all")
comp <- readRDS(file.path(RDS, "components.rds")); prev <- readRDS(file.path(RDS, "prev_main.rds"))
ref <- bind_rows(comp$est %>% filter(component == "c_tg", type == "standardised", round == 2021) %>% transmute(outcome = "c_tg", sex, est0 = est),
                 prev %>% filter(type == "standardised", analysis %in% c("national", "sex"), round == 2021) %>% transmute(outcome = "dys", sex = level, est0 = est))
ck <- chk_all %>% left_join(ref, by = c("outcome", "sex")); stopifnot(max(abs(ck$est - ck$est0)) < 1e-8)
stopifnot(!anyNA(S$est), all(S$lower <= S$est & S$est <= S$upper))
print(as.data.frame(S %>% select(-label) %>% mutate(across(where(is.double), ~ round(.x, 3)))))
wcsv(S, "S_fasting_2021")
wfmt(S %>% transmute(part, outcome = label, category, sex, n_sample = n, n_category, estimate_95ci = ifelse(grepl("difference", part), fci(est, lower, upper, f2), fci(est, lower, upper)), p = fp(p)), "S_fasting_2021")
saveRDS(list(table = S, weighted_median_hours = wmed, n_unparsed_or_missing = sum(d$round == 2021 & d$fasting_flag != "parsed (approximate)")), file.path(RDS, "stage2b_fasting.rds"))
sink()
