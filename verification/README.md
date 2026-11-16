# Independent check in Python

These scripts recompute key results from the harmonised participant file without the R code. They were written from the analysis specification, without reference to the R code. `output/verification_report.md` is the report of the first run.

- `verify.py` recomputes, from `data_derived/analytic.csv.gz` and `output/tables/standard_population_2016.csv`: the analytic sample sizes; crude and standardised prevalence with design-based standard errors and logit 95% confidence limits (national, by sex, by area and by age group, for each round); the 2007 to 2021 changes and the women-minus-men difference in change; the survey-weighted Poisson (log-link) trend models (APC nationally and by sex); the interaction tests; and the design counts (PSUs and strata). It uses numpy, pandas and scipy only. The survey variance, the standardisation and the model fit are written out by hand.
- `compare.py` matches 198 of these quantities with `output/results_index.json` and checks each one against a tolerance: estimates and confidence limits 0.05 percentage points; standard errors 10% relative; APC 0.02; P values within a factor of 1.5 (or both below 0.001); counts exact.
- `make_report.py` writes the report.

Standardised standard errors differ from R by up to 4.4%, within the tolerance. The reason: `verify.py` treats the standardised weights as fixed, while `survey::svystandardize` treats standardisation as post-stratification. `verify.py` also computes the post-stratification version, which matches R to 8 significant figures (see the report).

## How to run

You need Python 3 with numpy, pandas and scipy, and a completed run of `Rscript R/00_run_all.R`, because the scripts read the participant-level file that the run creates. From the repository root:

```sh
python3 -I verification/verify.py . verification/py_results.json
python3 -I verification/compare.py verification/py_results.json output/results_index.json verification/comparison.csv
python3 -I verification/make_report.py verification/py_results.json output/results_index.json verification/comparison.csv verification/verification_report.md
```

The three output files hold aggregate results only.

## Results

The first run (4 November 2026) found all 198 quantities within tolerance (`output/verification_report.md`). A second run on the same day, on macOS with Python 3.11.15, numpy 2.4.6, pandas 2.3.3 and scipy 1.17.1, against the outputs of a fresh R run, again found 198 of 198 within tolerance.
