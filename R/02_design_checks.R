# 02_design_checks.R : cluster (PSU) structure and weight diagnostics per round.
# Outputs: output/tables/cluster_sizes.csv, output/tables/cluster_candidates.csv, output/tables/weights_summary.csv
suppressMessages({library(dplyr); library(readr); library(haven); library(tidyr)})
W <- "."  # repository root: run from the root folder
a <- readRDS(file.path(W, "data_derived/analytic.rds"))
sink(file.path(W, "output/logs/02_design_checks.log"), split = TRUE)
q <- function(x) quantile(x, c(0, .05, .25, .5, .75, .95, 1), na.rm = TRUE)
# ---- PSU sizes: all records, and lab participants aged 25-64 ----
sz <- bind_rows(
  a %>% count(round, psu) %>% mutate(set = "all records"),
  a %>% filter(age >= 25, age <= 64, lab_participant) %>% count(round, psu) %>% mutate(set = "lab participants aged 25-64"))
cs <- sz %>% group_by(round, set) %>% summarise(n_psu = n(), mean = mean(n), min = min(n), p05 = q(n)[2], p25 = q(n)[3], median = median(n), p75 = q(n)[5], p95 = q(n)[6], max = max(n),
                                              share_size_20 = mean(n == 20), share_size_10 = mean(n == 10), .groups = "drop")
st <- a %>% filter(!is.na(stratum)) %>% group_by(round) %>% summarise(n_strata = n_distinct(stratum), psu_spanning_strata = n_distinct(psu[psu_spans_strata]), .groups = "drop")
cs <- cs %>% left_join(st, by = "round"); print(as.data.frame(cs)); write_csv(cs, file.path(W, "output/tables/cluster_sizes.csv"))
# PSUs per stratum (lab 25-64): singletons matter for variance estimation
ps <- a %>% filter(age >= 25, age <= 64, lab_participant, !is.na(stratum)) %>% group_by(round, stratum) %>% summarise(k = n_distinct(psu), .groups = "drop") %>%
  group_by(round) %>% summarise(strata = n(), min_psu = min(k), median_psu = median(k), strata_with_1_psu = sum(k == 1), .groups = "drop")
cat("\nPSUs per stratum (lab 25-64):\n"); print(ps)

# ---- Candidate cluster definitions (2007, 2011) ----
d7 <- read_csv(file.path(W, "data_raw/2007/steps86_weight_standard.csv"), show_col_types = FALSE)
c7 <- d7 %>% group_by(i4particip, i4partici1) %>% summarise(n = n(), men = sum(c1 == 1, na.rm = TRUE), women = sum(c1 == 2, na.rm = TRUE),
        dup_person = any(duplicated(i4partici2)), .groups = "drop")
comp <- d7 %>% filter(age >= 15, age <= 64) %>% mutate(ag = cut(age, c(15, 25, 35, 45, 55, 65), right = FALSE)) %>% count(i4particip, i4partici1, c1, ag)
cand <- bind_rows(
  tibble(round = 2007, candidate = "province x i4partici1", n_groups = nrow(c7), median_size = median(c7$n), share_size_20 = mean(c7$n == 20),
         note = sprintf("%.1f%% of clusters have exactly 10 men and 10 women; %.1f%% of cluster x sex x 10-year age cells hold 2 people; clusters per province: %s",
                        100 * mean(c7$men == 10 & c7$women == 10), 100 * mean(comp$n == 2), paste(names(table(table(c7$i4particip))), collapse = "/"))),
  tibble(round = 2007, candidate = "province x i4partici2", n_groups = n_distinct(paste(d7$i4particip, d7$i4partici2)),
         median_size = median(table(paste(d7$i4particip, d7$i4partici2))), share_size_20 = NA,
         note = sprintf("i4partici2 takes values 1-20 in %.1f%% of rows and is unique within cluster in %.1f%% of clusters: a person serial number, not a district", 100 * mean(d7$i4partici2 <= 20, na.rm = TRUE), 100 * mean(!c7$dup_person))))
o11 <- read_dta(file.path(W, "data_raw/2011/Steps90 (1).dta"), col_select = c(province_census, x1d, i08, i09f, i09i, idnum))
g <- function(df, ...) { t <- df %>% count(...); c(n = nrow(t), med = median(t$n), s20 = mean(t$n == 20)) }
for (nm in list(list("district (x1d) x cluster (i08)", quote(x1d), quote(i08)), list("district (x1d) x idnum", quote(x1d), quote(idnum)),
                list("province x cluster (i08)", quote(province_census), quote(i08)), list("district (x1d)", quote(x1d), NULL))) {
  r <- if (is.null(nm[[3]])) g(o11, !!nm[[2]]) else g(o11, !!nm[[2]], !!nm[[3]])
  cand <- bind_rows(cand, tibble(round = 2011, candidate = nm[[1]], n_groups = r[["n"]], median_size = r[["med"]], share_size_20 = r[["s20"]], note = ""))
}
cand$note[cand$round == 2011 & cand$candidate == "district (x1d) x cluster (i08)"] <- "Matches the questionnaire participant code (district / cluster / household / person); 12 or 13 clusters per district, 50 districts"
cand$note[cand$round == 2011 & cand$candidate == "district (x1d) x idnum"] <- "idnum is a running id number: mostly singletons, not a cluster"
cand$note[cand$round == 2011 & cand$candidate == "province x cluster (i08)"] <- "Merges clusters with the same number in different districts of one province: rejected"
cand$note[cand$round == 2011 & cand$candidate == "district (x1d)"] <- "Sampled county (1-5 per province): a possible first stage; not used as PSU (too few per stratum)"
print(as.data.frame(cand)); write_csv(cand, file.path(W, "output/tables/cluster_candidates.csv"))

# ---- Weights in the analytic-type sample (lab participants aged 25-64 with all three lipids valid) ----
an <- a %>% filter(age >= 25, age <= 64, lab_participant, !is.na(tc), !is.na(hdl), !is.na(tg))
ws <- bind_rows(
  an %>% transmute(round, which = weight_name, w = weight),
  an %>% transmute(round, which = weight_alt_name, w = weight_alt),
  an %>% transmute(round, which = "user original (raw_weight_user)", w = raw_weight_user)) %>%
  filter(!is.na(w)) %>% group_by(round, which) %>%
  summarise(n = n(), sum = sum(w), min = min(w), p01 = quantile(w, .01), median = median(w), p99 = quantile(w, .99), max = max(w),
            cv = sd(w) / mean(w), kish_deff = 1 + (sd(w) / mean(w))^2, n_gt_5x_median = sum(w > 5 * median(w)), n_zero = sum(w == 0), .groups = "drop") %>% distinct()
print(as.data.frame(ws)); write_csv(ws, file.path(W, "output/tables/weights_summary.csv"))
sink()
