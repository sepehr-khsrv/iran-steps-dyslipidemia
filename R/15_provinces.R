# 15_provinces.R : provincial standardised prevalence (age group x sex, national standard), provincial APC,
# correlation of 2007 level with APC, old-vs-new 2007 province labels, provincial n.
source("R/09_functions.R")  # path relative to the repository root
sink(file.path(W, "output/logs/15_provinces.log"), split = TRUE)
L <- load_data(); d <- L$d
des <- make_design(d)
PROV <- levels(d$province31)
# ---- empty cells ----
cells <- d %>% count(round, prov, rp, agegrp, sex, .drop = FALSE) %>% filter(!is.na(prov))
grid <- d %>% distinct(round, prov, rp) %>% tidyr::crossing(agegrp = factor(AGE_LEV, AGE_LEV), sex = factor(SEX_LEV, SEX_LEV)) %>%
  left_join(d %>% count(rp, agegrp, sex), by = c("rp", "agegrp", "sex")) %>% mutate(n = coalesce(n, 0L))
empty <- grid %>% group_by(round, prov, rp) %>% summarise(min_cell = min(n), empty_cells = sum(n == 0), .groups = "drop")
cat("Province-rounds with an empty age x sex cell:\n"); print(empty %>% filter(empty_cells > 0))
ok_rp <- empty$rp[empty$empty_cells == 0]
# ---- standardised (province-rounds without empty cells) ----
dps <- d %>% filter(rp %in% ok_rp) %>% mutate(rp = factor(rp))
ds <- std_design(make_design(dps), ~agegrp + sex, ~rp)
st <- cell_prev(ds, "dys", "rp", "standardised")
cr <- cell_prev(des, "dys", "rp", "crude weighted")
sep <- function(x) { s <- do.call(rbind, strsplit(x$rp, "|", fixed = TRUE)); x$round <- as.integer(s[, 1]); x$prov <- s[, 2]; x }
st <- sep(st); cr <- sep(cr)
main <- bind_rows(st, cr %>% filter(!rp %in% ok_rp) %>% mutate(type = "crude weighted (empty age x sex cell; not standardised)"))
full <- expand.grid(prov = PROV, round = YEARS, stringsAsFactors = FALSE) %>%
  left_join(main %>% select(prov, round, type, est, lower, upper, se, deff, n, svyciprop_diff), by = c("prov", "round")) %>%
  left_join(cr %>% select(prov, round, crude_est = est, crude_lower = lower, crude_upper = upper), by = c("prov", "round")) %>%
  left_join(empty %>% select(prov, round, min_cell), by = c("prov", "round")) %>%
  mutate(n = coalesce(n, 0L),
         label = ifelse(prov == "Tehran" & round == 2007, "Tehran (including Alborz)", prov),
         note = case_when(prov == "Alborz" & round == 2007 ~ "Not separable: part of Tehran in 2007",
                          prov == "Qom" & round == 2016 ~ "Not surveyed in 2016",
                          prov == "Kurdistan" & round == 2007 ~ "No laboratory data in the 2007 file",
                          is.na(est) ~ "No data",
                          grepl("not standardised", type) ~ "Crude: empty age x sex cell",
                          TRUE ~ ""),
         prov = factor(prov, PROV)) %>% arrange(prov, round)
print(as.data.frame(full %>% mutate(across(where(is.double), ~ round(.x, 2)))))
# ---- change 2007-2021 (independent rounds) and APC ----
wide <- full %>% select(prov, round, est, se) %>% pivot_wider(names_from = round, values_from = c(est, se))
chg <- wide %>% transmute(prov, change_2007_2021 = est_2021 - est_2007, se = sqrt(se_2021^2 + se_2007^2),
                          lower = change_2007_2021 - Z * se, upper = change_2007_2021 + Z * se)
apc <- bind_rows(lapply(PROV, function(p) {
  sel <- d$prov == p
  m <- fit_trend(des[sel, ], dys ~ year_c + agegrp + sex + area)
  data.frame(prov = p, n_rounds = length(unique(d$round[sel])), rounds = paste(sort(unique(d$round[sel])), collapse = ", "), apc_term(m))
})) %>% mutate(flag = ifelse(n_rounds < 4, "fewer than four rounds", ""),
               flag = ifelse(prov == "Tehran", "2007 value includes Alborz", flag), prov = factor(prov, PROV))
apc$p_holm <- p.adjust(apc$p_trend, "holm")
print(as.data.frame(apc %>% mutate(across(where(is.double), ~ signif(.x, 4)))))
# ---- correlation 2007 level vs APC ----
cc <- wide %>% select(prov, est_2007) %>% inner_join(apc %>% select(prov, apc), by = "prov") %>% filter(!is.na(est_2007))
pc <- cor.test(cc$est_2007, cc$apc); sc <- suppressWarnings(cor.test(cc$est_2007, cc$apc, method = "spearman"))
corr <- data.frame(n_provinces = nrow(cc), pearson_r = unname(pc$estimate), pearson_lower = pc$conf.int[1], pearson_upper = pc$conf.int[2], pearson_p = pc$p.value,
                   spearman_rho = unname(sc$estimate), spearman_p = sc$p.value,
                   note = "Provinces with a 2007 estimate (Tehran includes Alborz). A negative correlation is expected in part from regression to the mean, because the 2007 estimate enters both quantities.")
print(corr)
mult <- data.frame(note = paste0("Provincial APCs are 31 separate tests with no adjustment for multiplicity. At alpha = 0.05 about 1.6 would be expected to reach P < 0.05 by chance if no province changed. ",
                                 sum(apc$p_trend < 0.05), " of 31 provincial APCs had P < 0.05; ", sum(apc$p_holm < 0.05), " remain below 0.05 after Holm adjustment (shown for information only)."))
print(mult)
pn <- d %>% count(prov = province31, round) %>% pivot_wider(names_from = round, values_from = n, values_fill = 0) %>% arrange(prov)
saveRDS(list(full = full, apc = apc, chg = chg, corr = corr, mult = mult, pn = pn, empty = empty), file.path(RDS, "provinces.rds"))
wcsv(full, "provinces_prevalence"); wcsv(apc, "provinces_apc"); wcsv(chg, "provinces_change"); wcsv(corr, "provinces_correlation")
wcsv(pn, "S_provincial_n")
sink()
