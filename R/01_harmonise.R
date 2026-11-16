# 01_harmonise.R
# Builds the harmonised analytic file for the four Iran STEPS rounds (2007, 2011, 2016, 2021).
# One row per participant in each round's file (all ages kept; Stage 2 applies the 25-64 filter).
# No imputation. Every recode is commented. Raw codes are kept in columns prefixed raw_.
# Outputs: data_derived/analytic.rds, data_derived/analytic.csv.gz, data_derived/data_dictionary.csv,
#          output/tables/weights_construction_cells_2007_2011.csv, output/logs/01_harmonise.log (via sink)
suppressMessages({library(dplyr); library(readr); library(readxl); library(haven); library(tidyr)})
W <- "."  # repository root: run from the root folder
dir.create(file.path(W, "output/logs"), showWarnings = FALSE, recursive = TRUE)
sink(file.path(W, "output/logs/01_harmonise.log"), split = TRUE)
cat("01_harmonise.R run at", format(Sys.time()), "\nR", R.version.string, "\n\n")

lk  <- read_csv(file.path(W, "data_derived/province_lookup.csv"), show_col_types = FALSE)
p31 <- c("Markazi","Gilan","Mazandaran","East Azerbaijan","West Azerbaijan","Kermanshah","Khuzestan","Fars","Kerman",
         "Razavi Khorasan","Isfahan","Sistan and Baluchestan","Kurdistan","Hamadan","Chaharmahal and Bakhtiari","Lorestan",
         "Ilam","Kohgiluyeh and Boyer-Ahmad","Bushehr","Zanjan","Semnan","Yazd","Hormozgan","Tehran","Ardabil","Qom",
         "Qazvin","Golestan","North Khorasan","South Khorasan","Alborz")
prov_of <- function(rnd, code) { x <- lk %>% filter(round == rnd); x$province31[match(code, x$raw_code)] }
census_of <- function(rnd, code) { x <- lk %>% filter(round == rnd); x$census_code[match(code, x$raw_code)] }

# Age group used by the analysis: breaks 25, 35, 45, 55, 65, right-open; everything else "other".
agegrp_of <- function(a) {
  g <- as.character(cut(a, c(25, 35, 45, 55, 65), right = FALSE, labels = c("25-34", "35-44", "45-54", "55-64")))
  ifelse(is.na(g), "other", g)
}
# Plausibility limits (plan item 3): value outside -> analyte set to NA and flagged.
lim <- list(tc = c(50, 700), hdl = c(5, 150), tg = c(10, 2000))
clean_lipid <- function(x, nm) { bad <- !is.na(x) & (x < lim[[nm]][1] | x > lim[[nm]][2]); list(val = ifelse(bad, NA_real_, x), flag = bad) }

# ---------------- Census (2006 = 1385, 2011 = 1390; Statistical Centre of Iran tables as compiled in Iran_consensus.csv) ----
cen <- read_csv(file.path(W, "data_raw/census/Iran_consensus.csv"), show_col_types = FALSE) %>%
  filter(!is.na(age_iso), !grepl("-|all|<|>", age_iso), sex %in% c("male", "female"), area %in% c("urban", "rural")) %>%
  mutate(age = as.numeric(age_iso), sex = ifelse(sex == "female", "women", "men"))

# Post-stratification helper: weight = census count / sample count in cell. Cells with no sample member
# are collapsed over area within province x sex x age group. Returns weights and a cell table.
poststrat <- function(df, cen_yr, cellvars_age, label) {
  # df: id, pcode (census code, Alborz merged into Tehran for 2007), area, sex, ageband ; only eligible rows
  cc <- cen_yr %>% group_by(pcode, area, sex, ageband) %>% summarise(pop = sum(population), .groups = "drop")
  ns <- df %>% count(pcode, area, sex, ageband, name = "n")
  cells <- cc %>% left_join(ns, by = c("pcode", "area", "sex", "ageband")) %>% mutate(n = coalesce(n, 0L))
  empty <- cells %>% filter(n == 0) %>% distinct(pcode, sex, ageband)
  cells <- cells %>% mutate(collapsed = paste(pcode, sex, ageband) %in% paste(empty$pcode, empty$sex, empty$ageband)) %>%
    group_by(pcode, sex, ageband) %>%
    mutate(pop_c = ifelse(collapsed, sum(pop), pop), n_c = ifelse(collapsed, sum(n), n)) %>% ungroup() %>%
    mutate(w = ifelse(n_c > 0, pop_c / n_c, NA_real_), label = label)
  out <- df %>% left_join(cells %>% select(pcode, area, sex, ageband, w), by = c("pcode", "area", "sex", "ageband"))
  cat(sprintf("[%s] cells %d, empty cells %d (collapsed over area), census pop covered %.0f, sum of weights %.0f, rows without weight %d\n",
              label, nrow(cells), sum(cells$n == 0), sum(cells$pop[!is.na(cells$w)]), sum(out$w, na.rm = TRUE), sum(is.na(out$w))))
  list(w = out$w, cells = cells)
}
band10 <- function(a) as.character(cut(a, c(25, 35, 45, 55, 65), right = FALSE, labels = c("25-34", "35-44", "45-54", "55-64")))
band5  <- function(a) ifelse(a >= 15 & a <= 69, paste0(floor(a / 5) * 5, "-", floor(a / 5) * 5 + 4), NA_character_)

# ======================= 2007 (STEPS 1386) =======================
d7 <- read_csv(file.path(W, "data_raw/2007/steps86_weight_standard.csv"), show_col_types = FALSE)
h7 <- d7 %>% transmute(
  round = 2007L,
  id = paste0("2007_", Ref),                                  # Ref = row number added by the weight script (1..30000)
  raw_idnum = idnum, raw_i4particip = i4particip, raw_i4partici1 = i4partici1, raw_i4partici2 = i4partici2,
  raw_x1 = x1, raw_partici3 = partici3, raw_c1 = c1, raw_c201 = c201, raw_m5 = m5, raw_i586 = i586, raw_i587 = i587,
  raw_h101 = h101, raw_h10 = h10,
  # PSU: province x cluster code (questionnaire I4 = province code / cluster code / person code; Q 1386 p1)
  psu = ifelse(is.na(i4particip) | is.na(i4partici1), paste0("2007_ind_", Ref), sprintf("2007_p%02d_c%02d", i4particip, i4partici1)),
  province31 = prov_of(2007, i4particip),                    # by name via crosswalk (00_province_lookup.R)
  pcode = census_of(2007, i4particip),
  # area: I1 1 = city (urban), 2 = village (rural) (Q 1386 item 2); code 7 is invalid -> NA
  area = case_when(partici3 == 1 ~ "urban", partici3 == 2 ~ "rural", TRUE ~ NA_character_),
  # sex: C1 1 = man, 2 = woman (Q 1386 item 8); code 6 invalid -> NA. Confirmed: mean HDL-C higher in code 2.
  sex = case_when(c1 == 1 ~ "men", c1 == 2 ~ "women", TRUE ~ NA_character_),
  age = as.integer(age),                                     # age = 86 - birth year (c201), from steps_86_weight.R
  raw_tc = b701, raw_hdl = b9, raw_tg = b801,
  ldl_measured = NA_real_,                                   # no LDL-C variable in the 2007 CSV or .dta
  # pregnancy: M5 1 = yes, 2 = no, women only (Q 1386 item 61); recorded for men in some rows -> ignored for men
  pregnant = case_when(c1 == 2 & m5 == 1 ~ TRUE, c1 == 2 & m5 == 2 ~ FALSE, TRUE ~ NA),
  fasting_hours = NA_real_, fasting_flag = "not recorded",   # no documented fasting item in the 2007 data
  told_raised_chol = NA, lipid_med = NA, statin = NA,        # no cholesterol-history or lipid-medication item in Q 1386 (h10 = herbal remedy for diabetes)
  interview_date = ifelse(is.na(i586) | is.na(i587), NA_character_, sprintf("1386-%02d-%02d", i586, i587)),  # I5 day/month (Q 1386 item 1)
  interview_date_gregorian = NA_character_,
  raw_weight_user = Weight_83)

# ======================= 2011 (STEPS 1390) =======================
d11 <- read_csv(file.path(W, "data_raw/2011/Steps90_weighted.csv"), show_col_types = FALSE, guess_max = 20000)
o11 <- read_dta(file.path(W, "data_raw/2011/Steps90 (1).dta"), col_select = c(idnum, x1d, i08, i09f, i09i))
stopifnot(identical(as.numeric(o11$idnum), as.numeric(d11$idnum)) || all(o11$idnum == d11$idnum, na.rm = TRUE))
d11$x1d <- as.numeric(o11$x1d); d11$i09f <- as.numeric(o11$i09f); d11$i09i <- as.numeric(o11$i09i)
# x1d (district code) is missing for 14 Hamadan rows; Hamadan has a single sampled district (31 = Hamadan): fill it.
d11 <- d11 %>% mutate(x1d_filled = ifelse(is.na(x1d) & province_census == 13, 31, x1d))
h11 <- d11 %>% transmute(
  round = 2011L, id = paste0("2011_", ID),
  raw_idnum = idnum, raw_province_census = province_census, raw_x1d = x1d, raw_i08 = i08, raw_i09f = i09f, raw_i09i = i09i,
  raw_i1d2 = i1d2, raw_area = area, raw_c1 = c1, raw_age = age, raw_age2 = age2, raw_m5 = m5, raw_i5dm = i5dm, raw_i5dd = i5dd,
  # PSU: district (county) x cluster code (participant code = district/cluster/household/person; Q 1390 p1)
  psu = ifelse(is.na(x1d_filled) | is.na(i08), paste0("2011_ind_", ID), sprintf("2011_d%02d_c%02d", x1d_filled, i08)),
  province31 = prov_of(2011, province_census), pcode = province_census,
  # area: i1d2 labelled 1 = Urban, 2 = Rural (Stata label); used instead of the merged 'area' (more missing)
  area = case_when(i1d2 == 1 ~ "urban", i1d2 == 2 ~ "rural", TRUE ~ NA_character_),
  # sex: c1 labelled 0 = Female, 1 = Male. Confirmed: mean HDL-C higher in code 0.
  sex = case_when(c1 == 1 ~ "men", c1 == 0 ~ "women", TRUE ~ NA_character_),
  # age: completed years = floor(decimal age); age2 is the rounded age used by the weight script (kept as raw_age2)
  age = as.integer(ifelse(!is.na(age), floor(age), age2)),
  raw_tc = b7, raw_hdl = b9, raw_tg = b8,
  ldl_measured = b9l,                                        # labelled 'LDL'; not equal to Friedewald (r = 0.81) -> measured
  pregnant = case_when(c1 == 0 & m5 == 1 ~ TRUE, c1 == 0 & m5 == 0 ~ FALSE, TRUE ~ NA),  # m5 labelled 0 = No, 1 = Yes
  fasting_hours = NA_real_, fasting_flag = "not recorded",   # no fasting item in Q 1390 or the data
  told_raised_chol = NA, lipid_med = NA, statin = NA,        # no cholesterol-history or lipid-medication item in Q 1390
  interview_date = ifelse(is.na(i5dm) | is.na(i5dd), NA_character_, sprintf("1390-%02d-%02d", i5dm, i5dd)),
  interview_date_gregorian = NA_character_,
  raw_weight_user = Weight_90)

# ======================= 2016 (STEPS 1395) =======================
o16 <- read_dta(file.path(W, "data_raw/2016/Data_STEPs2016.dta"))
c16 <- read_csv(file.path(W, "data_raw/2016/Steps_95_F.csv"), show_col_types = FALSE, guess_max = 30000, col_select = c(code, Weight_95))
stopifnot(all(c16$code == as.character(o16$code)))
o16 <- zap_labels(o16)
h16 <- o16 %>% mutate(Weight_95 = c16$Weight_95, cl = sub("/.*$", "", code)) %>% transmute(
  round = 2016L, id = paste0("2016_", code),
  raw_code = code, raw_i07 = i07, raw_area = area, raw_c1 = c1, raw_c3y = c3y, raw_m5 = m5, raw_h12 = h12, raw_h13a = h13a, raw_h13b = h13b,
  raw_h19 = h19, raw_LDL_calculated = LDL,
  # PSU: the unique id 'code' has the form A-PP-DD-CCCC/n; the part before '/' identifies a cluster of about 10 people
  # (3,060 groups; 3,020 of size 10). No separate cluster variable is released.
  psu = paste0("2016_", cl),
  province31 = prov_of(2016, i07), pcode = i07,
  area = case_when(area == 1 ~ "urban", area == 0 ~ "rural", TRUE ~ NA_character_),   # labelled 0 = Rural, 1 = Urban
  sex  = case_when(c1 == 1 ~ "men", c1 == 0 ~ "women", TRUE ~ NA_character_),          # labelled 0 = Female, 1 = Male; HDL-C check passed
  age = as.integer(c3y),
  raw_tc = CH02l, raw_hdl = HDLC3, raw_tg = TRIGL,
  ldl_measured = NA_real_,      # variable LDL equals TC - HDL - TG/5 exactly (calculated), so not a measured LDL-C
  pregnant = case_when(c1 == 0 & m5 == 1 ~ TRUE, c1 == 0 & m5 == 0 ~ FALSE, TRUE ~ NA),
  fasting_hours = NA_real_, fasting_flag = "not recorded",  # fasting items b1/b1f are in Q 1395 but not in the released data
  told_raised_chol = case_when(h13a == 1 ~ TRUE, h13a == 0 ~ FALSE, TRUE ~ NA),        # Q 1395 item 116
  lipid_med = NA,               # h14 / h14a (Q 1395 items 118, 118.1) are not in the released data
  statin = case_when(h19 == 1 ~ TRUE, h19 == 0 ~ FALSE, TRUE ~ NA),                     # Q 1395 item 119: statin to prevent or treat heart disease
  interview_date = NA_character_, interview_date_gregorian = NA_character_,             # no date variable released
  w_official_lab = weight_lab, w_official_q = weight_Q,
  raw_weight_user = Weight_95)

# ======================= 2021 (STEPS 1400) =======================
f21 <- file.path(W, "data_raw/2021/steps_2020_7.11.2021_01_p3_final_4.1.2.dta")
o21 <- read_dta(f21, col_select = c(familymemberid, clusterid, c1, pregnant, i07, age, area, fasting1, fasting2, hours_b, date_b, createddate,
                                    h12, h13a, h13b, h14n, h14a, h14_, h14_3, h19, CH02l, HDLC3, TRIGL, LDL, LDLC3,
                                    W_Laboratory, W_Questionnaire, district_code)) %>% zap_labels()
c21 <- read_csv(file.path(W, "data_raw/2021/Steps1400_3F.csv"), show_col_types = FALSE, col_select = c(familymemberid, Weight_1400))
o21 <- o21 %>% left_join(c21, by = "familymemberid")
stopifnot(sum(is.na(o21$Weight_1400)) == 0)
# Fasting: fasting1 = clock time of last food ("from"), fasting2 = clock time fasting ended ("up to"), free text.
# Parse the first number (and optional minutes). Last-meal hours 5-11 are read as evening (add 12), 12 as midnight,
# 0-4 as after midnight. Duration = (end - start) mod 24. Unparsable -> NA with flag. Approximate by construction.
ptime <- function(s) {
  s <- gsub("[۰-۹]", "", s)  # drop Persian digits (rare); Latin digits carry the values
  m <- regmatches(s, regexpr("[0-9]{1,2}([:./][0-9]{1,2})?", s))
  out <- rep(NA_real_, length(s)); has <- regexpr("[0-9]{1,2}([:./][0-9]{1,2})?", s) > 0
  v <- m; hh <- as.numeric(sub("[:./].*$", "", v)); mm <- suppressWarnings(as.numeric(ifelse(grepl("[:./]", v), sub("^[0-9]{1,2}[:./]", "", v), "0")))
  mm[is.na(mm) | mm >= 60] <- 0
  out[has] <- hh + mm / 60; out[!is.na(out) & out > 24] <- NA; out
}
f1 <- ptime(o21$fasting1); f2 <- ptime(o21$fasting2)
f1a <- ifelse(is.na(f1), NA, ifelse(f1 >= 5 & f1 < 12, f1 + 12, ifelse(f1 == 12 | f1 == 24, 0, f1)))
fh <- (f2 - f1a) %% 24
h21 <- o21 %>% mutate(f_hours = fh) %>% transmute(
  round = 2021L, id = paste0("2021_", familymemberid),
  raw_familymemberid = familymemberid, raw_clusterid = clusterid, raw_district_code = district_code, raw_i07 = i07, raw_area = area, raw_c1 = c1,
  raw_pregnant = pregnant, raw_h12 = h12, raw_h13a = h13a, raw_h14n = h14n, raw_h14a = h14a, raw_h14_ = h14_, raw_h14_3 = h14_3, raw_h19 = h19,
  raw_fasting1 = fasting1, raw_fasting2 = fasting2, raw_LDL_calculated = LDL,
  psu = paste0("2021_", clusterid),
  province31 = prov_of(2021, i07), pcode = i07,
  area = case_when(area == 1 ~ "urban", area == 0 ~ "rural", TRUE ~ NA_character_),   # labelled 0 = rural, 1 = urban
  sex  = case_when(c1 == 1 ~ "men", c1 == 0 ~ "women", TRUE ~ NA_character_),          # labelled 0 = Female, 1 = Male; HDL-C check passed
  age = as.integer(age),
  raw_tc = CH02l, raw_hdl = HDLC3, raw_tg = TRIGL,
  ldl_measured = LDLC3,         # LDLC3 = directly measured LDL-C in a subset (n about 1,475); LDL = Friedewald (calculated)
  # pregnant: only 0 or missing in the file (no 'yes'); 0 recorded for women; missing for men and 2,002 women
  pregnant = case_when(c1 == 0 & pregnant == 0 ~ FALSE, c1 == 0 & pregnant == 1 ~ TRUE, TRUE ~ NA),
  fasting_hours = ifelse(is.na(f_hours), NA_real_, round(f_hours, 2)),
  fasting_flag = case_when(is.na(fasting1) | fasting1 == "" ~ "not recorded",
                           is.na(f_hours) ~ "unparsable",
                           f_hours < 4 | f_hours > 20 ~ "parsed, implausible duration",
                           TRUE ~ "parsed (approximate)"),
  # h13a: -555 = skipped because h12 (ever measured) = No -> not told -> FALSE (Q Code 1400 'Read me' sheet)
  told_raised_chol = case_when(h13a == 1 ~ TRUE, h13a %in% c(0, -555) ~ FALSE, TRUE ~ NA),
  # h14a: currently taking oral drugs for high cholesterol (Q 1400 p22 item 5; 'currently' = last 12 months).
  # -555 = skipped (never told / never prescribed) -> FALSE
  lipid_med = case_when(h14a == 1 ~ TRUE, h14a %in% c(0, -555) ~ FALSE, TRUE ~ NA),
  statin = case_when(h19 == 1 ~ TRUE, h19 == 0 ~ FALSE, TRUE ~ NA),                     # statin to prevent/treat heart disease
  interview_date = date_b,                                                              # date of blood sampling (Persian calendar)
  interview_date_gregorian = substr(as.character(createddate), 1, 10),                  # record creation date
  w_official_lab = W_Laboratory, w_official_q = W_Questionnaire,
  raw_weight_user = Weight_1400)

# ======================= Common derivations =======================
common <- function(h) {
  cTC <- clean_lipid(h$raw_tc, "tc"); cHDL <- clean_lipid(h$raw_hdl, "hdl"); cTG <- clean_lipid(h$raw_tg, "tg")
  h %>% mutate(
    tc = cTC$val, hdl = cHDL$val, tg = cTG$val,
    tc_implausible = cTC$flag, hdl_implausible = cHDL$flag, tg_implausible = cTG$flag,
    lab_participant = !is.na(raw_tc) | !is.na(raw_hdl) | !is.na(raw_tg),             # any lipid value recorded
    ldl_friedewald = ifelse(!is.na(tc) & !is.na(hdl) & !is.na(tg) & tg < 400, tc - hdl - tg / 5, NA_real_),
    nonhdl = tc - hdl,
    agegrp = agegrp_of(age),
    stratum = ifelse(is.na(province31) | is.na(area), NA_character_, paste(province31, area, sep = " | ")))
}
h7 <- common(h7); h11 <- common(h11); h16 <- common(h16); h21 <- common(h21)

# ======================= Weights =======================
# 2007 and 2011: the earlier cell weights (Weight_83, Weight_90) were built on the full sample (all ages).
# Weight_83 was built with a code-position province mapping that is wrong (see decisions_log): STEPS 1386 code k
# received the census counts of census province k, and code 30 got weight 1. We therefore rebuild:
#  (a) weight (chosen): post-stratification of the LABORATORY sample (any lipid measured, aged 25-64) to the census
#      nearest the round, cells province x area x sex x 10-year age group (25-34 ... 55-64);
#  (b) weight_alt for 2007: full-sample post-stratification with the corrected province mapping, cells province x area
#      x sex x 5-year age group (15-19 ... 65-69), replicating the Weight_83 method; weight_alt for 2011: Weight_90 as supplied
#      (its cell counts match the 2011 census exactly; checked).
cen85 <- cen %>% filter(year == 1385) %>% mutate(pcode = ifelse(province_iso == 30, 23, province_iso))   # Alborz merged into Tehran (2006 boundaries)
cen90 <- cen %>% filter(year == 1390) %>% mutate(pcode = province_iso)
mk_lab <- function(h, cyr, label) {
  el <- h %>% mutate(ageband = band10(age)) %>% filter(lab_participant, !is.na(ageband), !is.na(area), !is.na(sex), !is.na(pcode))
  ps <- poststrat(el %>% select(id, pcode, area, sex, ageband), cyr %>% filter(age >= 25, age <= 64) %>% mutate(ageband = band10(age)), NULL, label)
  list(w = tibble(id = el$id, w = ps$w), cells = ps$cells)
}
mk_full <- function(h, cyr, label) {
  el <- h %>% mutate(ageband = band5(age)) %>% filter(!is.na(ageband), !is.na(area), !is.na(sex), !is.na(pcode))
  ps <- poststrat(el %>% select(id, pcode, area, sex, ageband), cyr %>% filter(age >= 15, age <= 69) %>% mutate(ageband = band5(age)), NULL, label)
  list(w = tibble(id = el$id, w = ps$w), cells = ps$cells)
}
wl7 <- mk_lab(h7, cen85, "2007 lab-sample PS, census 2006"); wf7 <- mk_full(h7, cen85, "2007 full-sample PS (corrected mapping), census 2006")
wl11 <- mk_lab(h11, cen90, "2011 lab-sample PS, census 2011")
h7  <- h7  %>% left_join(wl7$w %>% rename(weight = w), by = "id") %>% left_join(wf7$w %>% rename(weight_alt = w), by = "id") %>%
  mutate(weight_name = "PS lab 25-64 (census 2006, rebuilt)", weight_alt_name = "PS full sample (census 2006, corrected Weight_83 method)")
h11 <- h11 %>% left_join(wl11$w %>% rename(weight = w), by = "id") %>%
  mutate(weight_alt = raw_weight_user, weight_name = "PS lab 25-64 (census 2011, rebuilt)", weight_alt_name = "Weight_90 (user, full sample, census 2011)")
# 2016 and 2021: official laboratory weights (chosen), user weights as alternative.
h16 <- h16 %>% mutate(weight = ifelse(lab_participant, w_official_lab, NA_real_), weight_alt = raw_weight_user,
                      weight_name = "weight_lab (official)", weight_alt_name = "Weight_95 (user)")
h21 <- h21 %>% mutate(weight = ifelse(lab_participant, w_official_lab, NA_real_), weight_alt = raw_weight_user,
                      weight_name = "W_Laboratory (official)", weight_alt_name = "Weight_1400 (user)")
cells <- bind_rows(wl7$cells, wf7$cells, wl11$cells)
write_csv(cells, file.path(W, "output/tables/weights_construction_cells_2007_2011.csv"))

# ======================= Bind, order, save =======================
core <- c("round","id","psu","stratum","province31","area","sex","age","agegrp","weight","weight_alt","weight_name","weight_alt_name",
          "tc","hdl","tg","ldl_measured","ldl_friedewald","nonhdl","tc_implausible","hdl_implausible","tg_implausible",
          "pregnant","fasting_hours","fasting_flag","told_raised_chol","lipid_med","statin","lab_participant",
          "interview_date","interview_date_gregorian","pcode","raw_tc","raw_hdl","raw_tg","raw_weight_user","w_official_lab","w_official_q")
all <- bind_rows(h7, h11, h16, h21)
all <- all %>% select(any_of(core), starts_with("raw_"))
all$province31 <- factor(all$province31, levels = p31)
# psu nested in round already (prefix); flag PSUs whose members fall in more than one stratum (mixed area)
span <- all %>% filter(!is.na(stratum)) %>% distinct(psu, stratum) %>% count(psu, name = "n_strata")
all <- all %>% left_join(span, by = "psu") %>% mutate(psu_spans_strata = coalesce(n_strata, 1L) > 1) %>% select(-n_strata)
saveRDS(all, file.path(W, "data_derived/analytic.rds"))
write_csv(all, file.path(W, "data_derived/analytic.csv.gz"))

# ======================= Checks printed to the log =======================
cat("\nRows by round:\n"); print(table(all$round))
cat("\nMean HDL-C by sex (coding check: women higher):\n"); print(all %>% group_by(round, sex) %>% summarise(hdl = mean(hdl, na.rm = TRUE), n = n(), .groups = "drop"))
cat("\nProvinces with no records, by round:\n"); for (r in c(2007, 2011, 2016, 2021)) cat(r, ":", paste(setdiff(p31, unique(as.character(all$province31[all$round == r]))), collapse = ", "), "\n")
cat("\nLab participants and weight coverage (aged 25-64):\n")
print(all %>% filter(age >= 25, age <= 64) %>% group_by(round) %>% summarise(n = n(), lab = sum(lab_participant), lab_w = sum(lab_participant & !is.na(weight)),
      sum_w_lab = sum(weight[lab_participant], na.rm = TRUE), sum_walt_all = sum(weight_alt, na.rm = TRUE), .groups = "drop"))
cat("\nPSUs and PSUs spanning strata:\n"); print(all %>% group_by(round) %>% summarise(spanning = n_distinct(psu[psu_spans_strata]), n_psu = n_distinct(psu), .groups = "drop"))
cat("\nFasting flags (2021):\n"); print(table(all$fasting_flag[all$round == 2021])); print(summary(all$fasting_hours[all$round == 2021 & all$fasting_flag == "parsed (approximate)"]))
cat("\nImplausible values:\n"); print(all %>% group_by(round) %>% summarise(tc = sum(tc_implausible), hdl = sum(hdl_implausible), tg = sum(tg_implausible)))
sink()
