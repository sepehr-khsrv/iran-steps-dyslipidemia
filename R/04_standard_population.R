# 04_standard_population.R : 2016 census population aged 25-64 by age group (4) x sex (2) x area (2).
# Source: Iran_consensus.csv (Statistical Centre of Iran census tables for 1385/1390/1395 by province x single year of age
#         x sex x urban/rural; the same file read by NAFLD/NAFLD_data_prep.R). Year 1395 = 2016 census.
# Cross-checks: (1) national totals in the file vs the published census totals; (2) age-group totals vs
#         'rake census 1395.xlsx'; (3) the sum of official 2016 laboratory weights (weight_lab) by cell.
# Output: output/tables/standard_population_2016.csv, output/tables/standard_population_2016_crosscheck.csv
suppressMessages({library(dplyr); library(readr); library(readxl); library(tidyr)})
W <- "."  # repository root: run from the root folder
raw <- read_csv(file.path(W, "data_raw/census/Iran_consensus.csv"), show_col_types = FALSE)
tot <- raw %>% filter(age_iso == "all", sex == "both", area == "total") %>% group_by(year) %>% summarise(pop = sum(population), .groups = "drop")
cen <- raw %>% filter(year == 1395, !is.na(age_iso), !grepl("-|all|<|>", age_iso), sex %in% c("male", "female"), area %in% c("urban", "rural")) %>%
  mutate(age = as.numeric(age_iso), sex = ifelse(sex == "female", "women", "men")) %>% filter(age >= 25, age <= 64) %>%
  mutate(agegrp = as.character(cut(age, c(25, 35, 45, 55, 65), right = FALSE, labels = c("25-34", "35-44", "45-54", "55-64"))))
sp <- cen %>% group_by(agegrp, sex, area) %>% summarise(population = sum(population), .groups = "drop") %>%
  mutate(proportion = population / sum(population),
         source = "Statistical Centre of Iran, 2016 (1395) National Population and Housing Census, province x single year of age x sex x urban/rural, as compiled in Iran_consensus.csv; summed over 31 provinces")
write_csv(sp, file.path(W, "output/tables/standard_population_2016.csv"))
# Cross-check 1: grand totals by census year (published: 2006 70,495,782; 2011 75,149,669; 2016 79,926,270)
# Cross-check 2: rake census 1395.xlsx national age-category totals (agecat2..5 = 25-34 ... 55-64)
rk <- suppressMessages(read_excel(file.path(W, "data_raw/census/rake census 1395.xlsx"), .name_repair = "unique_quiet"))
rkn <- rk %>% filter(Province == "national")
c2 <- tibble(agegrp = c("25-34", "35-44", "45-54", "55-64"),
             rake_census_1395 = as.numeric(rkn[1, c("population agecat2", "population agecat3", "population agecat4", "population agecat5")]))
# Cross-check 3: sum of official 2016 weight_lab among 2016 laboratory participants aged 25-64, by cell
a <- readRDS(file.path(W, "data_derived/analytic.rds"))
wl <- a %>% filter(round == 2016, lab_participant, age >= 25, age <= 64) %>% group_by(agegrp, sex, area) %>% summarise(sum_weight_lab_2016 = sum(weight), n_lab_2016 = n(), .groups = "drop")
xc <- sp %>% left_join(wl, by = c("agegrp", "sex", "area")) %>%
  mutate(weight_lab_proportion = sum_weight_lab_2016 / sum(sum_weight_lab_2016), ratio_weightlab_to_census = sum_weight_lab_2016 / population)
xa <- sp %>% group_by(agegrp) %>% summarise(census_file = sum(population), .groups = "drop") %>% left_join(c2, by = "agegrp") %>% mutate(ratio = census_file / rake_census_1395)
write_csv(xc %>% select(-source), file.path(W, "output/tables/standard_population_2016_crosscheck.csv"))
sink(file.path(W, "output/logs/04_standard_population.log"), split = TRUE)
cat("Grand totals in Iran_consensus.csv (all ages, both sexes):\n"); print(tot)
cat("\nStandard population (16 cells):\n"); print(as.data.frame(sp %>% select(-source)))
cat("\nTotal aged 25-64:", sum(sp$population), "\n")
cat("\nAge-group totals vs rake census 1395.xlsx:\n"); print(xa)
cat("\nCell proportions: census vs 2016 weight_lab:\n"); print(as.data.frame(xc %>% select(agegrp, sex, area, proportion, weight_lab_proportion, ratio_weightlab_to_census)))
cat("\nMax absolute difference in cell proportion:", max(abs(xc$proportion - xc$weight_lab_proportion)), "\n")
sink()
