# Preprocessing: the original scripts for the 2007 and 2011 input files

These are the original R scripts that made two of the analysis input files from the survey files supplied by the custodians. They are not part of `R/00_run_all.R`. They are kept as written, with one change: the folder paths were replaced by paths relative to this folder.

| Script | Reads | Writes |
|---|---|---|
| `2007/steps_86_weight.R` | `stps-Total86 - Original.dta` (2007 round, custodian file) and the 2006 census tables | `steps86_weight_standard.csv`, the input `data_raw/2007/steps86_weight_standard.csv`. It adds a row number (`Ref`), age (86 minus birth year `c201`), cell counts and a cell weight `Weight_83`. |
| `2011/steps90 weight 25 Nov 2024.R` | `Steps90 (1).dta` (2011 round) and the 2011 census tables | `Steps90_weighted.csv`, the input `data_raw/2011/Steps90_weighted.csv`. It adds a row number (`ID`), cell counts and a cell weight `Weight_90`. |

`2004_method/` holds four earlier scripts that apply the same weighting method to the 2004 STEPS file (`Total83.dta`). They are not needed to make the input files. They are kept for completeness.

## The weights these scripts make are not used for the estimates

The analysis rebuilds the 2007 and 2011 weights on the laboratory sample (participants aged 25 to 64 with lipid results), post-stratified to the 2006 and 2011 censuses by province, area, sex and age group (`R/01_harmonise.R`). `Weight_83` was also built with a province mapping by code position, which is wrong for 2007 (see `data_derived/province_lookup.csv`). The earlier weights stay in the data only for comparisons: the reconciliation with the earlier estimates (`R/01b_reconcile.R`), the weight summaries (`R/02_design_checks.R`) and, for 2011 only, the alternative weight (`R/03_flow_missingness.R` and the weights sensitivity analysis in `R/14_sensitivity.R`).

## Support files (no participant data)

- `census_2006/Ostan_83.xlsx`: census province codes and names.
- `census_2006/AgeGroup.xlsx`: five-year age bands.
- `census_2006/FileList.xlsx`: the names of the 2006 census files to read (`os00.xls` to `os29.xls`).
- `census_2011/Os90.xlsx` and `census_2011/FileList.xlsx`: the same lookups for 2011 (`Os00.xls` to `Os30.xls`).

The document properties of these files (author fields and saved folder paths) were cleared. The cells are unchanged.

## Census tables: not included

The scripts read the 2006 (1385) and 2011 (1390) census tables of the Statistical Centre of Iran: population by age, sex and residence, one Excel file per province. The Centre publishes them on its website (https://www.amar.org.ir). They are not redistributed here. To run the scripts, download the tables and save them in `census_2006/files/` and `census_2011/90/`, named as the two `FileList.xlsx` files list them.

## How to run them

Run each script with `preprocessing/` as the working directory. The paths in the scripts are relative to it:

| Path in the scripts | What it holds |
|---|---|
| `census_2006/` and `census_2011/` | The lookup files above. |
| `census_2006/files/` and `census_2011/90/` | The census tables (not included). |
| `../data_raw/2007/`, `../data_raw/2011/` and `../data_raw/2004/` | The survey files (not included: ask the custodians, see the main README) and the files the scripts write. |

Notes on the scripts:

- The 2007 script also writes two participant-level working files (`steps86long.xlsx` and `steps86subset_F.xlsx`) to `../data_raw/2007/`. `.gitignore` keeps `data_raw/` out of git.
- The 2011 script reads `Os90.xlsx` and then rebuilds the province list from the survey file.
- The scripts in `2004_method/` read the census tables from `census_2006/files/` and their survey and working files from `../data_raw/2004/`.
