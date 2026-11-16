# 20_stage2b_design.R : Stage 2b task 4b. Design summary per round with both PSU counts:
# unique cluster ids, and PSUs as used in variance estimation (with nest = TRUE a cluster whose members fall in both the
# urban and the rural stratum of a province counts once in each stratum). Output: output/tables/S_design_summary.csv (+ formatted)
source("R/09_functions.R")  # path relative to the repository root
sink(file.path(W, "output/logs/20_stage2b_design.log"), split = TRUE)
L <- load_data(); d <- L$d
des <- make_design(d)
# PSUs per stratum as the survey package counts them (design$fpc$sampsize), one value per stratum
ps <- data.frame(round = d$round, strat = des$strata[, 1], npsu = des$fpc$sampsize[, 1]) %>% distinct(round, strat, npsu)
ds <- d %>% group_by(round) %>% summarise(
  n = n(), n_strata = n_distinct(strat_pooled), n_clusters_unique = n_distinct(psu), n_psu_variance = n_distinct(paste(strat_pooled, psu)),
  n_clusters_spanning_strata = sum(tapply(strat_pooled, psu, function(x) length(unique(x))) > 1),
  n_lonely_strata = sum(table(strat_pooled[!duplicated(paste(strat_pooled, psu))]) == 1), sum_weights = sum(weight)) %>%
  left_join(ps %>% group_by(round) %>% summarise(n_psu_survey_pkg = sum(npsu)), by = "round") %>%
  mutate(psu_definition = c("2007" = "province x i4partici1", "2011" = "county x cluster (i08)", "2016" = "prefix of code", "2021" = "clusterid")[as.character(round)])
print(as.data.frame(ds))
stopifnot(identical(as.integer(ds$n_psu_variance), c(1491L, 675L, 2866L, 2899L)), identical(as.integer(ds$n_clusters_unique), c(1432L, 641L, 2850L, 2899L)),
          all(ds$n_psu_survey_pkg == ds$n_psu_variance))
wcsv(ds %>% select(-n_psu_survey_pkg), "S_design_summary")
wfmt(ds %>% transmute(round, psu_definition, n = format(n, big.mark = ","), strata = n_strata, clusters_unique = format(n_clusters_unique, big.mark = ","),
                      psus_in_variance = format(n_psu_variance, big.mark = ","), clusters_spanning_urban_rural = n_clusters_spanning_strata,
                      strata_with_one_psu = n_lonely_strata, sum_of_weights = format(round(sum_weights), big.mark = ",")), "S_design_summary")
saveRDS(ds, file.path(RDS, "stage2b_design.rds"))
sink()
