# 00_province_lookup.R
# Builds data_derived/province_lookup.csv: every raw province code in every round
# mapped to the 31 current province names BY NAME (never by code position).
# Sources:
#   2007: STEPS 1386 i4particip codes (1-30, Persian alphabetical order) -> census code via
#         the authors' crosswalk "os code steps 86.xlsx"; confirmed independently by the 2004 file
#         "Total83 - new.dta" (Provincecode vs labelled province_census) and by urban-share agreement
#         with the 2006 census. Census code -> name via Ostan_83.xlsx labels.
#   2011: province_census (labelled census codes) -> name via Os90.xlsx and Stata value labels.
#   2016: i07 Stata value labels (census codes) -> name.
#   2021: i07 Stata value labels (census codes) -> name.
suppressMessages({library(dplyr); library(readr); library(readxl); library(haven)})
W <- "."  # repository root: run from the root folder
p31 <- c("Markazi","Gilan","Mazandaran","East Azerbaijan","West Azerbaijan","Kermanshah","Khuzestan",
         "Fars","Kerman","Razavi Khorasan","Isfahan","Sistan and Baluchestan","Kurdistan","Hamadan",
         "Chaharmahal and Bakhtiari","Lorestan","Ilam","Kohgiluyeh and Boyer-Ahmad","Bushehr","Zanjan",
         "Semnan","Yazd","Hormozgan","Tehran","Ardabil","Qom","Qazvin","Golestan","North Khorasan",
         "South Khorasan","Alborz")
# Synonym table: every spelling met in the sources -> manuscript name. Matching is on lower-cased,
# punctuation-stripped strings.
syn <- c(
  "markazi"="Markazi","gilan"="Gilan","mazandaran"="Mazandaran",
  "azerbaijaneast"="East Azerbaijan","azerbaijan east"="East Azerbaijan","azarshargh"="East Azerbaijan",
  "azerbaijanwest"="West Azerbaijan","azerbaijan west"="West Azerbaijan","azargharb"="West Azerbaijan",
  "kermanshah"="Kermanshah","khuzestan"="Khuzestan","fars"="Fars","kerman"="Kerman",
  "khorasanrazavi"="Razavi Khorasan","khorasan razavi"="Razavi Khorasan","khorasan"="Razavi Khorasan",
  "isfahan"="Isfahan","sistanbaluchistan"="Sistan and Baluchestan","sistanandbaluchistan"="Sistan and Baluchestan",
  "sistan"="Sistan and Baluchestan","kurdistan"="Kurdistan","kordestan"="Kurdistan","kordest"="Kurdistan",
  "hamadan"="Hamadan","hamedan"="Hamadan","chaharmbakhtiari"="Chaharmahal and Bakhtiari",
  "chaharmahaalandbakhtiari"="Chaharmahal and Bakhtiari","chaharmahal"="Chaharmahal and Bakhtiari",
  "lorestan"="Lorestan","ilam"="Ilam","kohgiluyehboyera"="Kohgiluyeh and Boyer-Ahmad",
  "kohgiluyehandboyerahmad"="Kohgiluyeh and Boyer-Ahmad","kohkiluye"="Kohgiluyeh and Boyer-Ahmad",
  "bushehr"="Bushehr","boushehr"="Bushehr","zanjan"="Zanjan","semnan"="Semnan","yazd"="Yazd",
  "hormozgan"="Hormozgan","tehran"="Tehran","ardabil"="Ardabil","ardebil"="Ardabil","qom"="Qom",
  "qazvin"="Qazvin","golestan"="Golestan","khorasannorth"="North Khorasan","khorasannourth"="North Khorasan",
  "khorasan north"="North Khorasan","khorasansouth"="South Khorasan","khorasan south"="South Khorasan",
  "alborz"="Alborz")
norm <- function(x) gsub("[^a-z]", "", tolower(x))
names(syn) <- norm(names(syn))
to31 <- function(lbl) { k <- norm(lbl); out <- unname(syn[k]); if (any(is.na(out))) stop("Unmatched label(s): ", paste(lbl[is.na(out)], collapse=", ")); out }

# ---- 2007 ----
os83 <- read_excel(file.path(W, "data_raw/2007/Ostan_83.xlsx"))           # census code -> label
cw   <- read_excel(file.path(W, "data_raw/census/os code steps 86.xlsx")) %>% rename(census_code = Province, raw_code = i4particip)
t83  <- read_dta(file.path(W, "data_raw/2007/Total83 - new.dta"), col_select = c(Provincecode, province_census))
chk  <- t83 %>% count(raw_code = Provincecode, census_code = as.integer(province_census)) %>%
  group_by(raw_code) %>% slice_max(n, n = 1, with_ties = FALSE) %>% ungroup()
cmp  <- cw %>% left_join(chk, by = "raw_code", suffix = c("", "_total83"))
stopifnot(all(cmp$census_code == cmp$census_code_total83, na.rm = TRUE))   # 28 of 30 confirmable (Total83 predates N/S Khorasan codes 29/30)
l07 <- cw %>% left_join(os83 %>% select(census_code = value, source_label = label), by = "census_code") %>%
  mutate(round = 2007L, raw_var = "i4particip", province31 = to31(source_label),
         province31 = ifelse(province31 == "Tehran", "Tehran", province31),
         note = case_when(province31 == "Tehran" ~ "2007 Tehran includes the later Alborz province (created 2010); Alborz cannot be separated (see decisions_log)",
                          raw_code %in% c(29, 30) ~ "Code not present in Total83 (2004); mapping from authors' crosswalk and urban-share check",
                          TRUE ~ "Crosswalk confirmed by Total83 (2004) Provincecode vs labelled province_census"),
         confirmed_by = ifelse(raw_code %in% c(29, 30), "crosswalk; urban share", "crosswalk; Total83 2004; urban share"))
# ---- 2011 ----
os90 <- read_excel(file.path(W, "data_raw/2011/Os90.xlsx"))
l11 <- os90 %>% transmute(round = 2011L, raw_var = "province_census", raw_code = province_census, census_code = province_census,
                          source_label = district, province31 = to31(district), note = "Census code; label from Os90.xlsx", confirmed_by = "Os90.xlsx; district names (x1d labels)")
# ---- 2016 ----
o16 <- read_dta(file.path(W, "data_raw/2016/Data_STEPs2016.dta"), col_select = i07)
lab16 <- attr(o16$i07, "labels")
l16 <- tibble(round = 2016L, raw_var = "i07", raw_code = unname(lab16), census_code = unname(lab16), source_label = names(lab16)) %>%
  mutate(province31 = to31(source_label), note = ifelse(province31 == "Qom", "Qom did not take part in 2016 (no records)", "Stata value label"), confirmed_by = "Stata value label; Metadata 1395.2 p8")
# ---- 2021 ----
f21 <- file.path(W, "data_raw/2021/steps_2020_7.11.2021_01_p3_final_4.1.2.dta")
o21 <- read_dta(f21, col_select = i07, n_max = 10)
lab21 <- attr(o21$i07, "labels")
l21 <- tibble(round = 2021L, raw_var = "i07", raw_code = unname(lab21), census_code = unname(lab21), source_label = names(lab21)) %>%
  mutate(province31 = to31(source_label), note = "Stata value label", confirmed_by = "Stata value label; Q Code 1400 codebook")
lk <- bind_rows(l07, l11, l16, l21) %>% mutate(province31_code = match(province31, p31) - 1L) %>%
  select(round, raw_var, raw_code, census_code, source_label, province31, province31_code, confirmed_by, note) %>% arrange(round, raw_code)
stopifnot(all(lk$province31 %in% p31))
write_csv(lk, file.path(W, "data_derived/province_lookup.csv"))
# Which of the 31 provinces is absent in which round (presence in the lookup; data presence checked in 01_harmonise)
abs <- lapply(split(lk, lk$round), function(x) setdiff(p31, x$province31)); print(abs)
print(table(lk$round))
