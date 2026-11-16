# Dyslipidaemia in Iranian adults, STEPS surveys 2007 to 2021: analysis code and aggregate results

This repository, https://github.com/sepehr-khsrv/iran-steps-dyslipidemia, holds the R code and the aggregate results for the article "Prevalence of dyslipidemia and its components among Iranian adults (25-64 years): four national STEPS surveys across 2007-2021 period" (under review at PLOS ONE; the citation will be added on publication).

The code reads the participant files of the 2007, 2011, 2016 and 2021 Iranian STEPS surveys. It harmonises the four rounds, builds survey weights, estimates prevalence with design-based 95% confidence intervals, standardises to the 2016 census and fits survey-weighted trend models. It writes the tables, figures and estimates that the article reports.

## Data: not included

This repository holds no participant-level data. The survey data belong to the survey custodians:

- 2007 and 2011 rounds: Center for Non-Communicable Disease Control, Ministry of Health and Medical Education of Iran.
- 2016 and 2021 rounds: Non-Communicable Diseases Research Center, Endocrinology and Metabolism Population Sciences Institute, Tehran University of Medical Sciences.

To ask for access, contact the Non-Communicable Diseases Research Center, Endocrinology and Metabolism Population Sciences Institute, Tehran University of Medical Sciences, Second Floor, No. 10, Jalal Al-e-Ahmad Highway, Tehran 1411713137, Iran (https://ncdrc.tums.ac.ir; email: ncdrc_emri@sina.tums.ac.ir). The centre reviews each request.

The repository also leaves out the survey reports, questionnaires and protocol documents (third-party documents) and the harmonised participant-level file that the code builds.

### Input files

Put the files in `data_raw/` at the paths below. The md5 sums identify the exact files that produced the results in `output/`. To check a file, run `md5 "<path>"` on macOS or `md5sum "<path>"` on Linux.

| Path under `data_raw/` | md5 | What the code reads it for |
|---|---|---|
| `2007/steps86_weight_standard.csv` | `b51ef6c1aa7032558919d7e925dc7565` | 2007 round (STEPS 1386): participant records. Made from the round file `stps-Total86 - Original.dta` by `preprocessing/2007/steps_86_weight.R`, which adds a row number (`Ref`), age and the earlier weight `Weight_83`. |
| `2007/steps86_weight_standard_stepswieght_copy.csv` | `c7ca64b5a366ef5056acf46526dfd797` | A second copy of the 2007 file with a different `Weight_83`. Read only by `R/01b_reconcile.R`. |
| `2007/Total83 - new.dta` | `0e088b782069ada0c1bc0fcfe8591cbd` | 2004 STEPS file. Only its province codes are read, to confirm the 2007 province mapping. |
| `2007/Ostan_83.xlsx` | `9437d5685be09f7bbc4b1a0c14a4d0ad` | Census province codes and names (2007 mapping). |
| `census/os code steps 86.xlsx` | `6921f77e5cbef1f1d13fc51c9404092c` | Crosswalk from the 2007 STEPS province codes to census province codes. |
| `2011/Steps90 (1).dta` | `760c46933f46ef6a24b538be5da4ff47` | 2011 round (STEPS 1390): Stata file, read for the district, cluster, household and person codes. |
| `2011/Steps90_weighted.csv` | `fa383943854d1da7104b8dece645c401` | 2011 round: participant records. Made from `Steps90 (1).dta` by `preprocessing/2011/steps90 weight 25 Nov 2024.R`, which adds the earlier weight `Weight_90`. |
| `2011/Os90.xlsx` | `5e7081147af73b283a5c545f1a8815a8` | 2011 province codes and names. |
| `2016/Data_STEPs2016.dta` | `e1e85b3234067ce8827b4dda132051f7` | 2016 round (STEPS 1395): participant records and official weights. |
| `2016/Steps_95_F.csv` | `97453e13c8db526942e6b98b67e926a1` | 2016 round: a working file with the earlier weight `Weight_95` (used only for the reconciliation, the weight summaries and the alternative-weights sensitivity analysis). |
| `2021/steps_2020_7.11.2021_01_p3_final_4.1.2.dta` | `48adcaa26ec3282d6b6efbbff66051c2` | 2021 round (STEPS 1400): participant records and official weights. |
| `2021/Steps1400_3F.csv` | `88daa836f3e48232db45c3dceecf8e0a` | 2021 round: a working file with the earlier weight `Weight_1400` (used only for the reconciliation, the weight summaries and the alternative-weights sensitivity analysis). |
| `census/Iran_consensus.csv` | `a58093d1234ca9a8f15e95929bc886f9` | Census counts for standardisation and weighting (see below). |
| `census/rake census 1395.xlsx` | `b99b5be8e0325627480e0db71d63998c` | 2016 census age-group totals: a cross-check of the standard population. |
| `old_results/final result dyslipidemia .csv` | `ffb770486d002ed88478f89c296359f1` | Estimates from the earlier analysis scripts. Read only by `R/01b_reconcile.R`. |

`preprocessing/census_2006/Ostan_83.xlsx` holds the same cells as `2007/Ostan_83.xlsx`. Its document properties were cleared, so its md5 sum differs.

Province boundaries for S1 Fig: Natural Earth 1:10m admin-1 boundaries, version 5.1.1 (public domain), in `data_raw/maps/ne10m/`. `R/17_figures.R` downloads them from Natural Earth if they are absent. A later release can change the map slightly.

### Census cross-tabulation for standardisation

`data_raw/census/Iran_consensus.csv` holds census counts by province, single year of age, sex and area (urban or rural) for the 2006, 2011 and 2016 censuses (Persian years 1385, 1390 and 1395). Its columns are a row number, `year`, `province_iso`, `province_name`, `age_iso`, `sex`, `area` and `population`. The 2016 counts give the standard population: adults aged 25 to 64, 16 cells of age group, sex and area (`output/tables/standard_population_2016.csv`). The 2006 and 2011 counts are used to post-stratify the 2007 and 2011 samples. The grand totals equal the published census totals (70,495,782 in 2006; 75,149,669 in 2011; 79,926,270 in 2016).

Source: the Statistical Centre of Iran census tables for 2006, 2011 and 2016 (https://www.amar.org.ir), compiled into `Iran_consensus.csv`.

## How to run

The analysis used R 4.5.3 on macOS (Apple silicon) with these packages: survey 4.5, dplyr 1.2.1, tidyr 1.3.2, readr 2.2.0, tibble 3.3.1, readxl 1.5.0, haven 2.5.5, ggplot2 4.0.3, sf 1.1-2, patchwork 1.3.2, jsonlite 2.0.0, ragg 1.5.2, viridisLite 0.4.3 and ggrepel 0.9.8. `output/sessionInfo_stage2.txt` records the full session.

```r
install.packages(c("survey", "dplyr", "tidyr", "readr", "tibble", "readxl", "haven", "ggplot2",
                   "sf", "patchwork", "jsonlite", "ragg", "viridisLite", "ggrepel"))
```

Put the input files in `data_raw/`. Then, from the repository root:

```sh
Rscript R/00_run_all.R
```

`R/00_run_all.R` runs Stage 1 (`R/00_run_stage1.R`) and then Stage 2 with the Stage 2b additions (`R/00_run_stage2.R`), each in a fresh R session. All paths are relative to the repository root. The full run takes about 7 minutes on a recent laptop. Logs go to `output/logs/` and session information to `output/sessionInfo.txt`. `R/19_checks.R` runs last and stops the run if a check fails.

No step uses random numbers. There is no simulation, bootstrap or imputation, so no seed is needed and a re-run gives the same results.

Reproducibility check: on 4 November 2026 a fresh copy of this repository was run from the input files above with `Rscript R/00_run_all.R`, on the same computer and package versions as the original analysis. Every CSV file in `output/tables/` and `output/tables/formatted/` (80 files, 53,451 values), `output/results_index.json` (3,601 entries), `output/results_summary.md` and the figures came out byte for byte the same as the committed files. S1 Fig was then redrawn from the same Stage 2 results with one change: its grey legend label now reads "No estimate". Figs 1 to 3 did not change. On 16 November 2026 the run was repeated after the session-information call and two text labels in the scripts were changed. Every table, the results index and the figures again came out byte for byte the same.

| Scripts | What they do |
|---|---|
| `R/00_province_lookup.R` | Maps each round's province codes to the 31 provinces by name. |
| `R/01_harmonise.R` | Builds the harmonised participant file and rebuilds the 2007 and 2011 laboratory weights. |
| `R/01b_reconcile.R` | Applies the earlier analysis definitions, to reconcile the earlier estimates. |
| `R/02_design_checks.R`, `R/03_flow_missingness.R` | Clusters, strata and weights; participant flow and missing data. |
| `R/04_standard_population.R` | 2016 census standard population and its cross-checks. |
| `R/05_data_dictionary.R`, `R/06_variable_map.R` | Data dictionary and source-variable map. |
| `R/09_functions.R` | Shared settings and functions for Stage 2. |
| `R/10_design.R` to `R/15_provinces.R` | Survey design, prevalence, changes and trends, components, sensitivity analyses, provinces. |
| `R/16_tables.R`, `R/17_figures.R`, `R/18_results_index.R` | Tables, figures, results index and summary. |
| `R/20_stage2b_*.R` | Component changes between rounds, design summary, 2021 fasting check, index additions. |
| `R/19_checks.R` | End-of-run checks. |

## What the folders hold

- `R/`: the analysis code.
- `output/tables/`: one CSV file per table, at full precision. Files starting `T` hold the main tables and files starting `S_` the supporting tables (the article numbers some of them differently); the others are working tables that later steps read.
- `output/tables/formatted/`: the same tables rounded for publication, with intervals written as "estimate (lower, upper)".
- `output/figures/`: Figs 1 to 3 and S1 Fig as TIFF (300 dpi, LZW) with PNG previews. `README_figures.txt` gives sizes and the map source.
- `output/results_index.json`: the reported estimates and counts (3,601 entries), each with its value, unit and source table.
- `output/results_summary.md`: a short summary made from the index.
- `output/verification_report.md`: the independent check described below.
- `output/sessionInfo_stage2.txt`: the R session of the Stage 2 run.
- `data_derived/`: `data_dictionary.csv` (the variables of the harmonised file, no records) and `province_lookup.csv` (each round's province codes mapped to the 31 provinces).
- `docs/`: `SAP.md` (the statistical analysis plan; it refers to an internal decisions log that is not included) and `variable_map.csv` (source variables per round).
- `verification/`: the independent check in Python (see `verification/README.md`).
- `preprocessing/`: the original scripts that made the 2007 and 2011 input files, with the lookup tables they read (see `preprocessing/README.md`). The census tables they read are not included.

A run also writes files that are not in the repository: the participant-level file `data_derived/analytic.rds` (and `analytic.csv.gz`), participant-level objects in `output/rds/`, logs in `output/logs/`, `output/sessionInfo.txt` and `docs/map_name_crosswalk.csv`. `.gitignore` keeps `data_raw/` and the participant-level files out of git.

## How the results were checked

A separate implementation in Python (numpy, pandas and scipy) was written from the analysis specification, without reference to the R code. It recomputed 198 quantities (sample sizes, prevalences, standard errors, confidence limits, changes, APCs and interaction tests) from the harmonised file, and all 198 agreed with the R results within the tolerances stated in `output/verification_report.md`. The Python scripts are in `verification/` (see `verification/README.md`). A second run of them, against a fresh R run on macOS, again found 198 of 198 within tolerance.

## How to cite

Please cite the article. The full citation will be added here on publication.

If you use the code, please also cite this repository: https://github.com/sepehr-khsrv/iran-steps-dyslipidemia.

## Licence

The code is under the MIT licence (see `LICENSE`). The aggregate results and documentation in `output/`, `data_derived/` and `docs/` are offered under the Creative Commons Attribution 4.0 International licence (CC BY 4.0, https://creativecommons.org/licenses/by/4.0/). Neither licence covers the survey data, which stay with the custodians.
