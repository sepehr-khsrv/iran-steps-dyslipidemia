# Stage 3 independent verification report

Dyslipidaemia prevalence trends, Iranian STEPS surveys 2007, 2011, 2016, 2021. Independent check (Stage 3), 4 November 2026.

The Python implementation was written from the specification and the participant-level file only (analytic.csv.gz, data_dictionary.csv, standard_population_2016.csv). The R code was not read. Scripts: verification/verify.py (estimation), compare.py (comparison), make_report.py (this report). numpy, pandas and scipy only; the variance, standardisation and Poisson IRLS fit are hand-written.

## Verdict

All 198 compared quantities agree with the R output within the stated tolerances (0 outside tolerance). Point estimates (prevalences, changes, APCs) agree to better than 1e-6 percentage points. Crude SEs and all model-based (APC, interaction) results agree to about 1e-5 or better. The standardised SEs differ by -0.8% to +4.4% relative under the specified fixed-weight variance (CI limits therefore by up to 0.043 points); the cause is identified below and, once the same variance convention is used, the Python SEs reproduce R to 8 significant figures.

## Methods implemented

- Population: age 25 to 64 inclusive; pregnant == TRUE excluded; tc, hdl, tg all non-missing (the Stage 1 columns are already set to NA when implausible); sex, area, province31 known; positive weight. Age group re-derived from age with breaks 25, 35, 45, 55, 65 (right-open); 0 mismatches with the supplied agegrp.
- Outcome: TC >= 200 or TG >= 150 or Friedewald LDL-C >= 130 (computed here as TC - HDL - TG/5 when TG < 400; 0 mismatches with ldl_friedewald) or HDL-C < 40 (men) / < 50 (women).
- Design per round: strata = province31 x area; PSU = psu nested in stratum (a cluster in two strata counts as two PSUs); stratified between-PSU variance with n_h/(n_h - 1); lonely PSU 'adjust' (a single-PSU stratum contributes its squared deviation from the mean of all PSU totals in the design). Domain estimates (sex, area, age group) use the full round design with scores set to zero outside the domain.
- Standardisation: weights multiplied by (standard proportion of cell) / (weighted sample proportion of cell within the group); national over 16 cells, sex over age x area, area over age x sex, age group over sex x area, the standard for a subgroup being the census margin over all adults 25-64.
- Variance of standardised prevalence, primary (as specified): ratio-estimator linearisation with the adjusted weights treated as fixed. Secondary: the same score residualised within the standardisation cells (u_i - w_i x sum_cell(u) / sum_cell(w)), i.e. treating the standardisation as post-stratification.
- 95% CI for prevalence on the logit scale; changes as differences of independent rounds with SE = sqrt(se1^2 + se2^2) and normal P; women-minus-men difference in change includes the within-round women/men covariance from the joint score vector.
- Poisson log-link IRLS on pooled records with weight; sandwich variance with bread = inverse weighted Fisher information and meat = stratified between-PSU variance of PSU-summed scores, strata = round x province x area. Sex-specific models fitted as domains (weights zero outside the sex, full design kept). P values: normal for APCs and the sex interaction; F(3, ddf) with ddf = PSUs - strata = 7,692 for the age interaction (the chi-square P is also given).

## Comparison table

Tolerances: estimates and CI limits 0.05 percentage points; SEs 10% relative; APC 0.02; P within a factor of 1.5 or both < 0.001; counts exact. 'Python (PS var.)' is the secondary post-stratification variance, shown for standardised SEs and change limits.

| Quantity | R | Python | Difference | Within tolerance | Python (PS var.) |
|---|---|---|---|---|---|
| n analytic 2007 | 18,507 | 18,507 | 0 | yes |  |
| n analytic 2011 | 4,311 | 4,311 | 0 | yes |  |
| n analytic 2016 | 16,487 | 16,487 | 0 | yes |  |
| n analytic 2021 | 15,397 | 15,397 | 0 | yes |  |
| std national 2007 est | 79.2506 | 79.2506 | -0.0000 | yes |  |
| std national 2007 se | 0.6075 | 0.6290 | +3.53% | yes | 0.6075 |
| std national 2007 lower | 78.0336 | 77.9907 | -0.0429 | yes |  |
| std national 2007 upper | 80.4172 | 80.4565 | +0.0393 | yes |  |
| crude national 2007 est | 78.7653 | 78.7653 | -0.0000 | yes |  |
| crude national 2007 se | 0.6420 | 0.6420 | -0.00% | yes |  |
| crude national 2007 lower | 77.4786 | 77.4797 | +0.0011 | yes |  |
| crude national 2007 upper | 79.9975 | 79.9965 | -0.0010 | yes |  |
| std women 2007 est | 84.5124 | 84.5124 | +0.0000 | yes |  |
| std women 2007 se | 0.7356 | 0.7392 | +0.49% | yes | 0.7356 |
| std women 2007 lower | 83.0140 | 83.0076 | -0.0063 | yes |  |
| std women 2007 upper | 85.9011 | 85.9065 | +0.0054 | yes |  |
| crude women 2007 est | 84.2374 | 84.2374 | -0.0000 | yes |  |
| crude women 2007 se | 0.7321 | 0.7321 | +0.00% | yes |  |
| crude women 2007 lower | 82.7475 | 82.7488 | +0.0013 | yes |  |
| crude women 2007 upper | 85.6209 | 85.6198 | -0.0011 | yes |  |
| std men 2007 est | 74.0770 | 74.0770 | -0.0000 | yes |  |
| std men 2007 se | 0.9678 | 0.9874 | +2.02% | yes | 0.9678 |
| std men 2007 lower | 72.1337 | 72.0953 | -0.0384 | yes |  |
| std men 2007 upper | 75.9300 | 75.9648 | +0.0349 | yes |  |
| crude men 2007 est | 73.5623 | 73.5623 | -0.0000 | yes |  |
| crude men 2007 se | 1.0015 | 1.0015 | +0.00% | yes |  |
| crude men 2007 lower | 71.5516 | 71.5534 | +0.0017 | yes |  |
| crude men 2007 upper | 75.4797 | 75.4781 | -0.0016 | yes |  |
| std urban 2007 est | 80.3237 | 80.3237 | -0.0000 | yes |  |
| std urban 2007 se | 0.7385 | 0.7711 | +4.41% | yes | 0.7385 |
| std rural 2007 est | 75.7649 | 75.7649 | -0.0000 | yes |  |
| std rural 2007 se | 0.9650 | 0.9720 | +0.72% | yes | 0.9650 |
| std age 25-34 2007 est | 73.1833 | 73.1833 | -0.0000 | yes |  |
| std age 25-34 2007 se | 1.2848 | 1.3067 | +1.70% | yes | 1.2848 |
| std age 35-44 2007 est | 81.8610 | 81.8610 | +0.0000 | yes |  |
| std age 35-44 2007 se | 0.9936 | 1.0033 | +0.98% | yes | 0.9936 |
| std age 45-54 2007 est | 83.5018 | 83.5018 | -0.0000 | yes |  |
| std age 45-54 2007 se | 0.9352 | 0.9408 | +0.60% | yes | 0.9352 |
| std age 55-64 2007 est | 84.5250 | 84.5250 | -0.0000 | yes |  |
| std age 55-64 2007 se | 0.9180 | 0.9105 | -0.82% | yes | 0.9180 |
| std national 2011 est | 77.2652 | 77.2652 | +0.0000 | yes |  |
| std national 2011 se | 0.9049 | 0.9147 | +1.09% | yes | 0.9049 |
| std national 2011 lower | 75.4393 | 75.4226 | -0.0167 | yes |  |
| std national 2011 upper | 78.9931 | 79.0080 | +0.0149 | yes |  |
| crude national 2011 est | 77.1744 | 77.1744 | +0.0000 | yes |  |
| crude national 2011 se | 0.9318 | 0.9318 | -0.00% | yes |  |
| crude national 2011 lower | 75.2930 | 75.2968 | +0.0038 | yes |  |
| crude national 2011 upper | 78.9526 | 78.9493 | -0.0034 | yes |  |
| std women 2011 est | 82.7989 | 82.7989 | +0.0000 | yes |  |
| std women 2011 se | 1.0179 | 1.0166 | -0.13% | yes | 1.0179 |
| std women 2011 lower | 80.7068 | 80.7141 | +0.0073 | yes |  |
| std women 2011 upper | 84.7071 | 84.7010 | -0.0061 | yes |  |
| crude women 2011 est | 82.9983 | 82.9983 | -0.0000 | yes |  |
| crude women 2011 se | 0.9845 | 0.9845 | -0.00% | yes |  |
| crude women 2011 lower | 80.9763 | 80.9806 | +0.0043 | yes |  |
| crude women 2011 upper | 84.8455 | 84.8419 | -0.0036 | yes |  |
| std men 2011 est | 71.8406 | 71.8406 | +0.0000 | yes |  |
| std men 2011 se | 1.4729 | 1.4790 | +0.42% | yes | 1.4729 |
| std men 2011 lower | 68.8593 | 68.8534 | -0.0059 | yes |  |
| std men 2011 upper | 74.6416 | 74.6469 | +0.0052 | yes |  |
| crude men 2011 est | 71.8186 | 71.8186 | +0.0000 | yes |  |
| crude men 2011 se | 1.5157 | 1.5157 | +0.00% | yes |  |
| crude men 2011 lower | 68.7481 | 68.7552 | +0.0071 | yes |  |
| crude men 2011 upper | 74.6984 | 74.6922 | -0.0062 | yes |  |
| std urban 2011 est | 77.7991 | 77.7991 | +0.0000 | yes |  |
| std urban 2011 se | 1.0731 | 1.0887 | +1.46% | yes | 1.0731 |
| std rural 2011 est | 75.6208 | 75.6208 | +0.0000 | yes |  |
| std rural 2011 se | 1.6131 | 1.6188 | +0.36% | yes | 1.6131 |
| std age 25-34 2011 est | 73.6417 | 73.6417 | -0.0000 | yes |  |
| std age 25-34 2011 se | 1.6558 | 1.6794 | +1.43% | yes | 1.6558 |
| std age 35-44 2011 est | 77.0887 | 77.0887 | -0.0000 | yes |  |
| std age 35-44 2011 se | 1.8144 | 1.8131 | -0.07% | yes | 1.8144 |
| std age 45-54 2011 est | 80.3345 | 80.3345 | -0.0000 | yes |  |
| std age 45-54 2011 se | 1.7060 | 1.7485 | +2.50% | yes | 1.7060 |
| std age 55-64 2011 est | 83.4862 | 83.4862 | +0.0000 | yes |  |
| std age 55-64 2011 se | 1.1106 | 1.1234 | +1.15% | yes | 1.1106 |
| std national 2016 est | 76.7650 | 76.7650 | -0.0000 | yes |  |
| std national 2016 se | 0.4447 | 0.4454 | +0.15% | yes | 0.4447 |
| std national 2016 lower | 75.8816 | 75.8807 | -0.0010 | yes |  |
| std national 2016 upper | 77.6257 | 77.6266 | +0.0009 | yes |  |
| crude national 2016 est | 77.3833 | 77.3833 | +0.0000 | yes |  |
| crude national 2016 se | 0.4115 | 0.4115 | -0.00% | yes |  |
| crude national 2016 lower | 76.5662 | 76.5665 | +0.0003 | yes |  |
| crude national 2016 upper | 78.1800 | 78.1797 | -0.0003 | yes |  |
| std women 2016 est | 78.9674 | 78.9674 | +0.0000 | yes |  |
| std women 2016 se | 0.5693 | 0.5706 | +0.22% | yes | 0.5693 |
| std women 2016 lower | 77.8294 | 77.8273 | -0.0021 | yes |  |
| std women 2016 upper | 80.0620 | 80.0639 | +0.0019 | yes |  |
| crude women 2016 est | 79.7040 | 79.7040 | +0.0000 | yes |  |
| crude women 2016 se | 0.5346 | 0.5346 | +0.00% | yes |  |
| crude women 2016 lower | 78.6355 | 78.6360 | +0.0005 | yes |  |
| crude women 2016 upper | 80.7320 | 80.7316 | -0.0004 | yes |  |
| std men 2016 est | 74.6018 | 74.6018 | -0.0000 | yes |  |
| std men 2016 se | 0.6449 | 0.6465 | +0.25% | yes | 0.6449 |
| std men 2016 lower | 73.3166 | 73.3139 | -0.0027 | yes |  |
| std men 2016 upper | 75.8456 | 75.8480 | +0.0025 | yes |  |
| crude men 2016 est | 74.5775 | 74.5775 | +0.0000 | yes |  |
| crude men 2016 se | 0.5949 | 0.5949 | -0.00% | yes |  |
| crude men 2016 lower | 73.3934 | 73.3939 | +0.0005 | yes |  |
| crude men 2016 upper | 75.7264 | 75.7259 | -0.0005 | yes |  |
| std urban 2016 est | 77.7798 | 77.7798 | -0.0000 | yes |  |
| std urban 2016 se | 0.5377 | 0.5383 | +0.10% | yes | 0.5377 |
| std rural 2016 est | 73.5167 | 73.5167 | +0.0000 | yes |  |
| std rural 2016 se | 0.7305 | 0.7335 | +0.40% | yes | 0.7305 |
| std age 25-34 2016 est | 73.3008 | 73.3008 | -0.0000 | yes |  |
| std age 25-34 2016 se | 0.8236 | 0.8258 | +0.27% | yes | 0.8236 |
| std age 35-44 2016 est | 78.9614 | 78.9614 | -0.0000 | yes |  |
| std age 35-44 2016 se | 0.7552 | 0.7565 | +0.18% | yes | 0.7552 |
| std age 45-54 2016 est | 79.2832 | 79.2832 | +0.0000 | yes |  |
| std age 45-54 2016 se | 0.8029 | 0.8013 | -0.20% | yes | 0.8029 |
| std age 55-64 2016 est | 78.1538 | 78.1538 | +0.0000 | yes |  |
| std age 55-64 2016 se | 0.8987 | 0.8995 | +0.08% | yes | 0.8987 |
| std national 2021 est | 78.0968 | 78.0968 | -0.0000 | yes |  |
| std national 2021 se | 0.5368 | 0.5380 | +0.21% | yes | 0.5368 |
| std national 2021 lower | 77.0260 | 77.0242 | -0.0019 | yes |  |
| std national 2021 upper | 79.1312 | 79.1330 | +0.0018 | yes |  |
| crude national 2021 est | 79.0313 | 79.0313 | -0.0000 | yes |  |
| crude national 2021 se | 0.4817 | 0.4817 | -0.00% | yes |  |
| crude national 2021 lower | 78.0712 | 78.0716 | +0.0004 | yes |  |
| crude national 2021 upper | 79.9602 | 79.9598 | -0.0004 | yes |  |
| std women 2021 est | 81.4242 | 81.4242 | +0.0000 | yes |  |
| std women 2021 se | 0.6341 | 0.6318 | -0.36% | yes | 0.6341 |
| std women 2021 lower | 80.1486 | 80.1539 | +0.0053 | yes |  |
| std women 2021 upper | 82.6357 | 82.6309 | -0.0048 | yes |  |
| crude women 2021 est | 81.9896 | 81.9896 | +0.0000 | yes |  |
| crude women 2021 se | 0.5934 | 0.5934 | -0.00% | yes |  |
| crude women 2021 lower | 80.7966 | 80.7972 | +0.0005 | yes |  |
| crude women 2021 upper | 83.1239 | 83.1234 | -0.0005 | yes |  |
| std men 2021 est | 74.8192 | 74.8192 | -0.0000 | yes |  |
| std men 2021 se | 0.8482 | 0.8479 | -0.03% | yes | 0.8482 |
| std men 2021 lower | 73.1198 | 73.1212 | +0.0014 | yes |  |
| std men 2021 upper | 76.4458 | 76.4445 | -0.0013 | yes |  |
| crude men 2021 est | 75.1762 | 75.1762 | +0.0000 | yes |  |
| crude men 2021 se | 0.7626 | 0.7626 | +0.00% | yes |  |
| crude men 2021 lower | 73.6509 | 73.6516 | +0.0007 | yes |  |
| crude men 2021 upper | 76.6412 | 76.6406 | -0.0006 | yes |  |
| std urban 2021 est | 78.5657 | 78.5657 | -0.0000 | yes |  |
| std urban 2021 se | 0.6605 | 0.6620 | +0.23% | yes | 0.6605 |
| std rural 2021 est | 76.5688 | 76.5688 | +0.0000 | yes |  |
| std rural 2021 se | 0.7961 | 0.7945 | -0.20% | yes | 0.7961 |
| std age 25-34 2021 est | 75.7648 | 75.7648 | -0.0000 | yes |  |
| std age 25-34 2021 se | 1.0269 | 1.0341 | +0.70% | yes | 1.0269 |
| std age 35-44 2021 est | 79.2780 | 79.2780 | -0.0000 | yes |  |
| std age 35-44 2021 se | 0.9223 | 0.9226 | +0.03% | yes | 0.9223 |
| std age 45-54 2021 est | 80.1937 | 80.1937 | +0.0000 | yes |  |
| std age 45-54 2021 se | 0.8980 | 0.8997 | +0.20% | yes | 0.8980 |
| std age 55-64 2021 est | 78.9801 | 78.9801 | +0.0000 | yes |  |
| std age 55-64 2021 se | 1.0050 | 1.0070 | +0.21% | yes | 1.0050 |
| change national 2007-2021 est | -1.1538 | -1.1538 | -0.0000 | yes |  |
| change national 2007-2021 lower | -2.7428 | -2.7761 | -0.0333 | yes | -2.7428 |
| change national 2007-2021 upper | 0.4352 | 0.4685 | +0.0333 | yes | 0.4352 |
| change national 2007-2021 P | 0.155 | 0.163 | x1.056 | yes | 0.155 |
| change women 2007-2021 est | -3.0881 | -3.0881 | -0.0000 | yes |  |
| change women 2007-2021 lower | -4.9916 | -4.9941 | -0.0025 | yes | -4.9916 |
| change women 2007-2021 upper | -1.1847 | -1.1822 | +0.0025 | yes | -1.1847 |
| change women 2007-2021 P | 0.00147 | 0.00149 | x1.014 | yes | 0.00147 |
| change men 2007-2021 est | 0.7422 | 0.7422 | +0.0000 | yes |  |
| change men 2007-2021 lower | -1.7801 | -1.8087 | -0.0286 | yes | -1.7802 |
| change men 2007-2021 upper | 3.2646 | 3.2931 | +0.0286 | yes | 3.2646 |
| change men 2007-2021 P | 0.564 | 0.568 | x1.008 | yes | 0.564 |
| change women 2016-2021 est | 2.4568 | 2.4568 | -0.0000 | yes |  |
| change women 2016-2021 lower | 0.7866 | 0.7883 | +0.0017 | yes | 0.7865 |
| change women 2016-2021 upper | 4.1271 | 4.1254 | -0.0017 | yes | 4.1271 |
| change women 2016-2021 P | 0.00394 | 0.0039 | x1.009 | yes | 0.00394 |
| women minus men change 2007-2021 est | -3.8303 | -3.8303 | -0.0000 | yes |  |
| women minus men change 2007-2021 lower | -6.9822 | -6.9935 | -0.0113 | yes | -6.9822 |
| women minus men change 2007-2021 upper | -0.6785 | -0.6672 | +0.0113 | yes | -0.6785 |
| women minus men change 2007-2021 P | 0.0172 | 0.0176 | x1.023 | yes | 0.0172 |
| APC national est | -0.1283 | -0.1283 | -0.0000 | yes |  |
| APC national lower | -0.2623 | -0.2623 | -0.0000 | yes |  |
| APC national upper | 0.0059 | 0.0059 | +0.0000 | yes |  |
| APC national P | 0.0609 | 0.0609 | x1.001 | yes |  |
| APC women est | -0.3293 | -0.3293 | -0.0000 | yes |  |
| APC women lower | -0.4801 | -0.4801 | -0.0000 | yes |  |
| APC women upper | -0.1783 | -0.1783 | +0.0000 | yes |  |
| APC women P | 1.97e-05 | 1.95e-05 | x1.013 | yes |  |
| APC men est | 0.1381 | 0.1381 | +0.0000 | yes |  |
| APC men lower | -0.0917 | -0.0917 | -0.0000 | yes |  |
| APC men upper | 0.3685 | 0.3685 | +0.0000 | yes |  |
| APC men P | 0.239 | 0.239 | x1.000 | yes |  |
| APC women (interaction model) est | -0.3375 | -0.3375 | -0.0000 | yes |  |
| APC women (interaction model) lower | -0.4909 | -0.4909 | -0.0000 | yes |  |
| APC women (interaction model) upper | -0.1839 | -0.1839 | +0.0000 | yes |  |
| APC women (interaction model) P | 1.71e-05 | 1.69e-05 | x1.012 | yes |  |
| APC men (interaction model) est | 0.1289 | 0.1289 | -0.0000 | yes |  |
| APC men (interaction model) lower | -0.1048 | -0.1048 | -0.0000 | yes |  |
| APC men (interaction model) upper | 0.3632 | 0.3632 | +0.0000 | yes |  |
| APC men (interaction model) P | 0.28 | 0.28 | x1.000 | yes |  |
| P interaction year x sex | 0.00114 | 0.00113 | x1.004 | yes |  |
| P interaction year x age group (3 df) | 0.00281 | 0.00281 | x1.000 | yes |  |
| PSUs 2007 (unique ids) | 1,432 | 1,432 | 0 | yes |  |
| strata 2007 | 58 | 58 | 0 | yes |  |
| PSUs 2011 (unique ids) | 641 | 641 | 0 | yes |  |
| strata 2011 | 59 | 59 | 0 | yes |  |
| PSUs 2016 (unique ids) | 2,850 | 2,850 | 0 | yes |  |
| strata 2016 | 60 | 60 | 0 | yes |  |
| PSUs 2021 (unique ids) | 2,899 | 2,899 | 0 | yes |  |
| strata 2021 | 62 | 62 | 0 | yes |  |

## Design counts (analytic sample)

| Round | n | Strata | PSU ids (unique) | PSUs nested in strata | Clusters spanning two strata | Lonely strata | Sum of weights |
|---|---|---|---|---|---|---|---|
| 2007 | 18,507 | 58 | 1,432 | 1,491 | 59 | 0 | 30,187,900 |
| 2011 | 4,311 | 59 | 641 | 675 | 34 | 2 (Hamadan|rural, Kermanshah|rural) | 33,546,999 |
| 2016 | 16,487 | 60 | 2,850 | 2,866 | 16 | 0 | 44,735,830 |
| 2021 | 15,397 | 62 | 2,899 | 2,899 | 0 | 0 | 41,301,939 |

R reports design_n_psu as the number of unique PSU ids (1,432; 641; 2,850; 2,899), which matches the unique count here. With nest = TRUE the variance actually uses the nested count (1,491; 675; 2,866; 2,899), because clusters with urban and rural members split into two pseudo-PSUs. Captions and the methods text should not imply that the design has 1,432 PSUs in 2007 when 1,491 enter the variance; this is a labelling point only. No PSU is an individual ('_ind_') in the analytic sample.

## Additional checks

(a) Standard population. The file has 16 cells; the proportions sum to 1.000000000000 and equal population / total to within 9.0e-17. Total aged 25-64: 43,985,183. Urban share 76.3%, men 50.5%, age shares 25-34 38.2%, 35-44 28.5%, 45-54 19.9%, 55-64 13.4%. The age gradient and sex ratio look like a census. The urban share (76.3%) is slightly above the 70-75% guide; the 2016 census urban share for the whole population was about 74%, and working-age adults are more urban than children and the elderly, so this is plausible and not a sign of error. The source cells could not be re-derived from Iran_consensus.csv because that file was not staged; only internal consistency was checked.

(b) Non-HDL-C >= 160 adds 0 cases to the composite in the analytic sample (also 0 using raw_tc - raw_hdl). No participant has non-HDL-C >= 160 with TC < 200 and HDL-C >= 40 (0), as the algebra requires.

(c) Flow and n per round after the filters:

| Round | Raw rows | Aged 25-64 | Not pregnant | Three valid lipids | Sex, area, province known | Positive weight (analytic) | Expected |
|---|---|---|---|---|---|---|---|
| 2007 | 30,000 | 23,907 | 23,661 | 18,513 | 18,507 | 18,507 | 18,507 |
| 2011 | 12,235 | 7,554 | 7,459 | 4,312 | 4,311 | 4,311 | 4,311 |
| 2016 | 30,541 | 23,738 | 23,402 | 16,487 | 16,487 | 16,487 | 16,487 |
| 2021 | 27,874 | 21,353 | 21,353 | 15,397 | 15,397 | 15,397 | 15,397 |

All four analytic n match. 2021 has no pregnancy 'yes' code, so no 2021 woman is excluded for pregnancy (stated in the data dictionary). Note that case_classification.csv totals (18,626; 4,768; 16,521; 15,402) refer to the wider available-case population, not the analytic sample. They are reproduced exactly by: aged 25-64, not pregnant, at least one valid lipid, with no requirement for known sex, area, province or weight. SAP section 8(d) says the available-case population needs complete covariates, which would give 18,620 and 4,767 in 2007 and 2011 (2016 and 2021 unchanged). Either the table or the SAP wording should be aligned; the effect on percentages is negligible.

(d) Design counts: see the table above. Lonely strata occur only in 2011 (Hamadan rural, Kermanshah rural); the 'adjust' rule was implemented and the 2011 SEs match R exactly under the matched variance convention, which confirms that R used the same rule.

(e) Women-minus-men difference in absolute change 2007-2021: -3.83 points. With the within-round covariance, fixed-weight variance: 95% CI -6.99 to -0.67, P = 0.0176; post-stratification variance: -6.98 to -0.68, P = 0.0172 (R: -6.98 to -0.68, P = 0.0172). Ignoring the covariance: -7.01 to -0.65, P = 0.0184. The within-round covariance is small and positive, so the conclusion does not depend on it. Within-round covariances (women, men; x1e4 on the % scale): 2007 0.0038, 2021 0.0136.

(f) Sex-specific APCs fitted as true subsets (PSUs without any member of that sex dropped) rather than domains: women -0.3293 (-0.4800, -0.1784), men 0.1381 (-0.0917, 0.3685). The domain fit matches R more closely, confirming that R used subset() on the design (domain semantics), as intended.

(g) Age interaction Wald statistic 14.085 on 3 df: chi-square P = 0.00279; F(3, 7692) P = 0.00281 (R 0.00281). The national APC P with t on design df is 0.06091 (R 0.06091).

## Discrepancies and diagnosis

No quantity falls outside tolerance. One systematic difference is worth recording.

1. Standardised SEs in R differ from the fixed-weight SEs specified for this check, mostly being smaller: Python minus R ranges from -0.8% to +4.4% relative (largest: urban 2007 +4.4%, national 2007 +3.5%, age 45-54 2011 +2.5%, men 2007 +2.0%; 2016 and 2021 within 0.7%). Diagnosis: survey::svystandardize does not treat the standardised weights as fixed. It behaves as post-stratification: the variance uses the linearised score residualised on the standardisation cells within each group. When the Python score is residualised in the same way (u_i - w_i x sum_cell(u) / sum_cell(w)), every standardised SE in R is reproduced to a relative difference below 1e-7, across national, sex, area and age-group estimates, and including the 2011 lonely-stratum adjustment. The change CIs and the women-minus-men contrast then match R to 4 decimals. This is consistent with svystandardize implementing standardisation through post-stratification of the design, and it is a defensible choice, but the methods text should not say the standardised weights were treated as fixed. If the authors prefer the conservative fixed-weight variance, CIs widen slightly (for example national change 2007-2021 becomes -2.78 to 0.47 instead of -2.74 to 0.44); no inference changes.

2. Labelling only: design_n_psu counts unique cluster ids, not the PSUs that enter the variance with nest = TRUE (see Design counts). Also, case_classification.csv uses the available-case population without the covariate requirement stated in SAP 8(d) (see check c).

Not verified here: provincial estimates, components, sensitivity analyses, logit-link and full-interaction variants, linearity tests, and the derivation of the standard population from the census source file.
