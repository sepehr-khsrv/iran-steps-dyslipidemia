# Statistical analysis plan (Stage 2)

Version 1.0, 4 November 2026. Written before any Stage 2 estimate was computed. It implements section 1 and section 5 of the re-analysis plan. Where this document and the plan differ, this document records the decision and the reason. Deviations found during coding are logged in `docs/decisions_log.md` under "Stage 2".

## 1. Data

- Input: `data_derived/analytic.rds` (Stage 1). One row per survey participant, all ages.
- Standard population: `output/tables/standard_population_2016.csv` (2016 census, adults aged 25-64, 16 cells of age group x sex x area).
- Rounds: 2007, 2011, 2016, 2021. `year_c` = year - 2007 (0, 4, 9, 14).

## 2. Analytic population

Adults aged 25-64 years inclusive. Women recorded as pregnant are excluded (pregnancy is recorded in 2007, 2011 and 2016; the 2021 file has no "yes" code). Participants need valid TC, HDL-C and TG (Stage 1 plausibility flags), known sex, area and province, and a positive laboratory weight. Age groups: 25-34, 35-44, 45-54, 55-64. Expected n: 18,507; 4,311; 16,487; 15,397.

## 3. Outcomes

- Composite dyslipidaemia: TC >= 200 mg/dL, or TG >= 150, or Friedewald LDL-C >= 130 (LDL-C missing when TG >= 400), or non-HDL-C >= 160, or HDL-C < 40 (men) or < 50 (women). We verify that the non-HDL-C criterion adds no case.
- Components: TC >= 200; LDL-C >= 130 (denominator: TG < 400); TG >= 150; non-HDL-C >= 160; low HDL-C (sex-specific thresholds).
- Sensitivity outcomes: composite or lipid medication (2021 h14a; 2016 h19 statin as a labelled proxy; missing item counted as "no"); composite without the HDL-C criterion; composite with HDL-C < 40 for both sexes.

## 4. Survey design

- Weights: `weight` (2007 and 2011 rebuilt laboratory weights; 2016 weight_lab; 2021 W_Laboratory).
- Per round: ids = psu, strata = province31 x area, nest = TRUE. Taylor linearisation. `options(survey.lonely.psu = "adjust")`.
- PSU counts: with nest = TRUE a cluster whose members fall in both the urban and the rural stratum of a province counts once in each stratum. PSUs used in variance estimation: 1,491 (2007), 675 (2011), 2,866 (2016), 2,899 (2021); unique cluster ids: 1,432; 641; 2,850; 2,899. Both are reported (`S_design_summary.csv`).
- Pooled design: strata = round x province31 x area; PSU identifiers already unique within round. All round-specific estimates are computed as domains of the pooled design. Because strata are nested in round, point estimates and variances equal those of the round-specific designs, except for the small effect of the lonely-PSU adjustment, which we check and report.

## 5. Prevalence

- Every prevalence (crude and standardised) uses `svyciprop(method = "logit")`. We report estimate, lower and upper 95% limits, unweighted n, SE and design effect (from `svymean(deff = TRUE)` on the same design).
- Direct standardisation with `svystandardize` to the 2016 census, aged 25-64:
  - National: by age group x sex x area (16 cells), over round.
  - Sex-specific: by age group x area, over round x sex; standard = census age x area distribution of all adults.
  - Area-specific: by age group x sex, over round x area.
  - Age-group-specific: by sex x area, over round x age group.
  - Provinces: by age group x sex (8 cells), over round x province; national standard. A province-round with an empty cell is reported crude and flagged.
  - The standard vector is aligned to the design's factor levels by name, and the alignment is asserted in code.
  - Variance: `svystandardize` treats standardisation as post-stratification. The linearised scores are residualised within the standardisation cells (within each group), rather than the standardised weights being treated as fixed. Stage 3 verification reproduced every standardised SE under this convention; treating the weights as fixed would widen the CIs slightly (by up to 4.4% relative in SE) and changes no inference.
- Secondary: national crude weighted and unweighted prevalence per round.

## 6. Changes between rounds

- Absolute change 2007 to 2021 in percentage points, for every subgroup and component. Rounds are independent samples, so SE = sqrt(se1^2 + se2^2). We compute contrasts from `svyby(covmat = TRUE)` on the standardised pooled design and check that the cross-round covariance is zero.
- Consecutive differences: 2011-2007, 2016-2011, 2021-2016.
- Difference in change between subgroups: women minus men; each age group minus 25-34; urban minus rural. Within-round covariance between subgroups is taken into account.
- 95% CI: estimate +/- 1.96 SE.

## 7. Trend models

- Pooled design, `svyglm(composite ~ year_c + agegrp + sex + area, family = quasipoisson(link = "log"))`.
- APC = (exp(beta_year) - 1) x 100; 95% CI from exp(beta +/- 1.96 SE); P for trend from the Wald test of beta_year (`regTermTest`, design degrees of freedom).
- Subgroup APC: the same model fitted in `subset(pooled, subgroup)`, dropping the subgroup variable.
- Differences in trend: separate models adding year_c:sex, year_c:agegrp (3 df), year_c:area; Wald test (`regTermTest`). Subgroup APCs from these models via `svycontrast`.
- Departure from log-linearity: the model with factor(year) against the model with year_c, 2 df Wald test, nationally and in each subgroup. Implemented by adding indicators for 2011 and 2016 to the linear model (the two models span the same space) and testing them jointly.
- Between-round prevalence ratios from the factor(year) model, with 95% CI.
- Sensitivity: the same models with a logit link (quasibinomial); interaction P values reported.
- Components: APC per component, overall and by sex.

## 8. Sensitivity analyses

(a) Medication: 2021 composite or h14a; 2016 composite or h19 (statin proxy). Weighted proportion on h14a (2021) and h19 (2016) by sex and age group. 2016-2021 change under both definitions.
(b) Composite without the HDL-C criterion: standardised prevalence by sex, all rounds, APC, women-minus-men change contrast.
(c) Composite with HDL-C < 40 for both sexes: same outputs as (b).
(d) Available-case bounds: participants aged 25-64, not pregnant, with at least one valid lipid and complete covariates (known sex, area, province and a positive laboratory weight). Indeterminate participants (no abnormal available component and at least one component missing) counted first as non-cases, then as cases. National and by sex, per round, standardised.
The descriptive table `case_classification.csv` (Stage 1, `R/03_flow_missingness.R`) uses a wider population: aged 25-64, not pregnant, at least one valid lipid, with no requirement for known covariates or a weight (totals 18,626; 4,768; 16,521; 15,402). The bounds analysis uses the covariate-complete subset (18,620; 4,767; 16,521; 15,402).
(e) 2011 measured versus Friedewald LDL-C: mean difference, SD, 95% limits of agreement, per cent agreement and Cohen's kappa for LDL-C >= 130. Unweighted.
(f) APC excluding 2011: national and by sex, with interaction P.
(g) 2016-2021 change alone: standardised difference with 95% CI, national and by sex.
(h) Strata = province only: SEs of the national round estimates and the national APC CI beside the main design.
(i) Weights: `weight_alt` in place of `weight` for all rounds; national and sex-specific standardised estimates and APC.

## 8b. Stage 2b additions (4 November 2026)

- Consecutive-round changes (2007-2011, 2011-2016, 2016-2021) for the composite and each component, overall and by sex, as in section 6; prevalence ratios from the categorical-year model of section 7 fitted for each component (overall: adjusted for age group, sex and area; by sex: the model within sex, adjusted for age group and area). `S_components_between_round.csv`.
- Table 5: 2016 and 2021 standardised estimates, change and prevalence ratio for the composite and each component, overall and by sex. `T5_within_lab_2016_2021.csv`.
- Fasting (2021 only; 2016 items not released; none in 2007 or 2011): weighted distribution of parsed fasting duration; 2021 standardised prevalence of TG >= 150 and of the composite in all participants and in those with a parsed fast of at least 8 h (primary) or at least 12 h (the protocol instruction; secondary), overall and by sex. Participants with no, unparsable or implausible times are excluded from the restricted samples. The difference (restricted minus all) uses the joint linearised score of the two estimates (overlapping samples), with the same post-stratification variance convention as section 5. `S_fasting_2021.csv`.

## 9. Provinces

Standardised prevalence (age group x sex) per province and round, with n and logit CI. 2007 Tehran is labelled "Tehran (including Alborz)"; Alborz 2007 and Qom 2016 are not available and are never imputed. Provincial APC from the pooled model within province (year_c + agegrp + sex + area). Provinces with fewer than four rounds are flagged. No adjustment for multiplicity (31 provinces); a statement is provided. Correlation (Pearson and Spearman) between the 2007 provincial estimate and the provincial APC. Table of the 2007 estimates under the old, wrong code mapping next to the corrected ones.

## 10. Participant characteristics

Per round: unweighted n and weighted % by age group, sex and area; weighted mean age with SE; weighted proportion on lipid medication (2021). Provincial n per round in a supplementary table.

## 11. Rules

No imputation, no smoothing, no simulation. All numbers come from the scripts in `R/10_*.R` to `R/20_stage2b_*.R`, run by `R/00_run_stage2.R`. Software: R 4.5.3, survey 4.5.
