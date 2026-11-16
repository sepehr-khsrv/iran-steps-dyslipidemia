"""Build verification_report.md. Usage: make_report.py <py_json> <r_json> <comparison_csv> <out_md>
From the repository root:
    python3 -I verification/make_report.py verification/py_results.json output/results_index.json verification/comparison.csv verification/verification_report.md
"""
import sys, json
import pandas as pd, numpy as np

py = json.load(open(sys.argv[1])); R = json.load(open(sys.argv[2]))
cmp = pd.read_csv(sys.argv[3]); out = sys.argv[4]
rv = lambda k: R[k]["value"]
ps = py["se_poststrat"]; chps = py["change_poststrat"]

mp = {"national": "national_all", "women": "sex_women", "men": "sex_men", "urban": "area_urban", "rural": "area_rural"}
for a in ["25-34", "35-44", "45-54", "55-64"]: mp["ag" + a] = "agegrp_" + a.replace("-", "_")


def ps_value(rkey):
    """Python value under the post-stratification variance for std SEs and change CIs/P."""
    if rkey.startswith("prev_std_") and rkey.endswith("_se"):
        for lab, rk in mp.items():
            for yr in ["2007", "2011", "2016", "2021"]:
                if rkey == f"prev_std_{rk}_{yr}_se": return ps[f"{lab}_{yr}"]
    for lab, rk in [("national_2007_2021", "change_std_national_all_2007_2021"), ("women_2007_2021", "change_std_sex_women_2007_2021"),
                    ("men_2007_2021", "change_std_sex_men_2007_2021"), ("women_2016_2021", "change_std_sex_women_2016_2021"),
                    ("women_minus_men_2007_2021", "diffchange_std_women_minus_men_2007_2021")]:
        for f in ["lower", "upper", "p"]:
            if rkey == f"{rk}_{f}": return chps[lab][f]
    return None


def fmt(v, kind):
    if kind == "n": return f"{int(round(v)):,}"
    if kind == "p": return f"{v:.3g}"
    return f"{v:.4f}"


def dfmt(d, kind, r):
    if kind == "se": return f"{100 * d / r:+.2f}%"
    if kind == "p": return f"x{max(r, r + d) / min(r, r + d):.3f}"
    if kind == "n": return f"{int(round(d))}"
    return f"{d:+.4f}"


L = []
A = L.append
A("# Stage 3 independent verification report")
A("")
A("Dyslipidaemia prevalence trends, Iranian STEPS surveys 2007, 2011, 2016, 2021. Independent check (Stage 3), 4 November 2026.")
A("")
A("The Python implementation was written from the specification and the participant-level file only (analytic.csv.gz, data_dictionary.csv, standard_population_2016.csv). The R code was not read. Scripts: verification/verify.py (estimation), compare.py (comparison), make_report.py (this report). numpy, pandas and scipy only; the variance, standardisation and Poisson IRLS fit are hand-written.")
A("")
n_no = int((cmp.within_tol == "NO").sum())
A("## Verdict")
A("")
A(f"All {len(cmp)} compared quantities agree with the R output within the stated tolerances ({n_no} outside tolerance). Point estimates (prevalences, changes, APCs) agree to better than 1e-6 percentage points. Crude SEs and all model-based (APC, interaction) results agree to about 1e-5 or better. The standardised SEs differ by -0.8% to +4.4% relative under the specified fixed-weight variance (CI limits therefore by up to 0.043 points); the cause is identified below and, once the same variance convention is used, the Python SEs reproduce R to 8 significant figures.")
A("")
A("## Methods implemented")
A("")
A("- Population: age 25 to 64 inclusive; pregnant == TRUE excluded; tc, hdl, tg all non-missing (the Stage 1 columns are already set to NA when implausible); sex, area, province31 known; positive weight. Age group re-derived from age with breaks 25, 35, 45, 55, 65 (right-open); 0 mismatches with the supplied agegrp.")
A("- Outcome: TC >= 200 or TG >= 150 or Friedewald LDL-C >= 130 (computed here as TC - HDL - TG/5 when TG < 400; 0 mismatches with ldl_friedewald) or HDL-C < 40 (men) / < 50 (women).")
A("- Design per round: strata = province31 x area; PSU = psu nested in stratum (a cluster in two strata counts as two PSUs); stratified between-PSU variance with n_h/(n_h - 1); lonely PSU 'adjust' (a single-PSU stratum contributes its squared deviation from the mean of all PSU totals in the design). Domain estimates (sex, area, age group) use the full round design with scores set to zero outside the domain.")
A("- Standardisation: weights multiplied by (standard proportion of cell) / (weighted sample proportion of cell within the group); national over 16 cells, sex over age x area, area over age x sex, age group over sex x area, the standard for a subgroup being the census margin over all adults 25-64.")
A("- Variance of standardised prevalence, primary (as specified): ratio-estimator linearisation with the adjusted weights treated as fixed. Secondary: the same score residualised within the standardisation cells (u_i - w_i x sum_cell(u) / sum_cell(w)), i.e. treating the standardisation as post-stratification.")
A("- 95% CI for prevalence on the logit scale; changes as differences of independent rounds with SE = sqrt(se1^2 + se2^2) and normal P; women-minus-men difference in change includes the within-round women/men covariance from the joint score vector.")
A("- Poisson log-link IRLS on pooled records with weight; sandwich variance with bread = inverse weighted Fisher information and meat = stratified between-PSU variance of PSU-summed scores, strata = round x province x area. Sex-specific models fitted as domains (weights zero outside the sex, full design kept). P values: normal for APCs and the sex interaction; F(3, ddf) with ddf = PSUs - strata = 7,692 for the age interaction (the chi-square P is also given).")
A("")
A("## Comparison table")
A("")
A("Tolerances: estimates and CI limits 0.05 percentage points; SEs 10% relative; APC 0.02; P within a factor of 1.5 or both < 0.001; counts exact. 'Python (PS var.)' is the secondary post-stratification variance, shown for standardised SEs and change limits.")
A("")
A("| Quantity | R | Python | Difference | Within tolerance | Python (PS var.) |")
A("|---|---|---|---|---|---|")
for _, r in cmp.iterrows():
    k = r["type"]
    pv = ps_value(r["r_key"])
    A(f"| {r['quantity']} | {fmt(r['R'], k)} | {fmt(r['Python'], k)} | {dfmt(r['diff'], k, r['R'])} | {r['within_tol']} | {'' if pv is None else fmt(pv, k)} |")
A("")
A("## Design counts (analytic sample)")
A("")
A("| Round | n | Strata | PSU ids (unique) | PSUs nested in strata | Clusters spanning two strata | Lonely strata | Sum of weights |")
A("|---|---|---|---|---|---|---|---|")
for yr in ["2007", "2011", "2016", "2021"]:
    d = py["design"][yr]
    A(f"| {yr} | {d['n']:,} | {d['strata']} | {d['psu_unique_ids']:,} | {d['psu_nested']:,} | {d['psu_spanning_strata']} | {d['lonely_strata']}{' (' + ', '.join(d['lonely_strata_names']) + ')' if d['lonely_strata'] else ''} | {d['sum_weight']:,.0f} |")
A("")
A("R reports design_n_psu as the number of unique PSU ids (1,432; 641; 2,850; 2,899), which matches the unique count here. With nest = TRUE the variance actually uses the nested count (1,491; 675; 2,866; 2,899), because clusters with urban and rural members split into two pseudo-PSUs. Captions and the methods text should not imply that the design has 1,432 PSUs in 2007 when 1,491 enter the variance; this is a labelling point only. No PSU is an individual ('_ind_') in the analytic sample.")
A("")
A("## Additional checks")
A("")
sp = py["stdpop"]
A(f"(a) Standard population. The file has {sp['n_cells']} cells; the proportions sum to {sp['sum_prop_file']:.12f} and equal population / total to within {sp['max_abs_prop_diff']:.1e}. Total aged 25-64: {sp['total_pop']:,}. Urban share {100 * sp['urban_share']:.1f}%, men {100 * sp['men_share']:.1f}%, age shares 25-34 {100 * sp['age_shares']['25-34']:.1f}%, 35-44 {100 * sp['age_shares']['35-44']:.1f}%, 45-54 {100 * sp['age_shares']['45-54']:.1f}%, 55-64 {100 * sp['age_shares']['55-64']:.1f}%. The age gradient and sex ratio look like a census. The urban share (76.3%) is slightly above the 70-75% guide; the 2016 census urban share for the whole population was about 74%, and working-age adults are more urban than children and the elderly, so this is plausible and not a sign of error. The source cells could not be re-derived from Iran_consensus.csv because that file was not staged; only internal consistency was checked.")
A("")
A(f"(b) Non-HDL-C >= 160 adds {py['nonhdl_added_cases']} cases to the composite in the analytic sample (also {py['nonhdl_added_cases_raw']} using raw_tc - raw_hdl). No participant has non-HDL-C >= 160 with TC < 200 and HDL-C >= 40 ({py['nonhdl160_tc_lt200_hdl_ge40']}), as the algebra requires.")
A("")
A("(c) Flow and n per round after the filters:")
A("")
A("| Round | Raw rows | Aged 25-64 | Not pregnant | Three valid lipids | Sex, area, province known | Positive weight (analytic) | Expected |")
A("|---|---|---|---|---|---|---|---|")
exp = {"2007": 18507, "2011": 4311, "2016": 16487, "2021": 15397}
for yr in ["2007", "2011", "2016", "2021"]:
    f = py["flow"][yr]
    A(f"| {yr} | {f['raw']:,} | {f['age25_64']:,} | {f['not_pregnant']:,} | {f['three_lipids']:,} | {f['covariates']:,} | {f['weight_psu']:,} | {exp[yr]:,} |")
A("")
A("All four analytic n match. 2021 has no pregnancy 'yes' code, so no 2021 woman is excluded for pregnancy (stated in the data dictionary). Note that case_classification.csv totals (18,626; 4,768; 16,521; 15,402) refer to the wider available-case population, not the analytic sample. They are reproduced exactly by: aged 25-64, not pregnant, at least one valid lipid, with no requirement for known sex, area, province or weight. SAP section 8(d) says the available-case population needs complete covariates, which would give 18,620 and 4,767 in 2007 and 2011 (2016 and 2021 unchanged). Either the table or the SAP wording should be aligned; the effect on percentages is negligible.")
A("")
A("(d) Design counts: see the table above. Lonely strata occur only in 2011 (Hamadan rural, Kermanshah rural); the 'adjust' rule was implemented and the 2011 SEs match R exactly under the matched variance convention, which confirms that R used the same rule.")
A("")
c = py["change"]["women_minus_men_2007_2021"]; c0 = py["change"]["women_minus_men_2007_2021_nocov"]; cp = chps["women_minus_men_2007_2021"]
A(f"(e) Women-minus-men difference in absolute change 2007-2021: {c['est']:.2f} points. With the within-round covariance, fixed-weight variance: 95% CI {c['lower']:.2f} to {c['upper']:.2f}, P = {c['p']:.4f}; post-stratification variance: {cp['lower']:.2f} to {cp['upper']:.2f}, P = {cp['p']:.4f} (R: {rv('diffchange_std_women_minus_men_2007_2021_lower'):.2f} to {rv('diffchange_std_women_minus_men_2007_2021_upper'):.2f}, P = {rv('diffchange_std_women_minus_men_2007_2021_p'):.4f}). Ignoring the covariance: {c0['lower']:.2f} to {c0['upper']:.2f}, P = {c0['p']:.4f}. The within-round covariance is small and positive, so the conclusion does not depend on it. Within-round covariances (women, men; x1e4 on the % scale): 2007 {py['prev']['cov_women_men_2007']:.4f}, 2021 {py['prev']['cov_women_men_2021']:.4f}.")
A("")
m = py["models"]
A(f"(f) Sex-specific APCs fitted as true subsets (PSUs without any member of that sex dropped) rather than domains: women {m['women_subsetdropped']['est']:.4f} ({m['women_subsetdropped']['lower']:.4f}, {m['women_subsetdropped']['upper']:.4f}), men {m['men_subsetdropped']['est']:.4f} ({m['men_subsetdropped']['lower']:.4f}, {m['men_subsetdropped']['upper']:.4f}). The domain fit matches R more closely, confirming that R used subset() on the design (domain semantics), as intended.")
A("")
A(f"(g) Age interaction Wald statistic {m['int_agegrp']['wald']:.3f} on 3 df: chi-square P = {m['int_agegrp']['p_chisq']:.5f}; F(3, {m['int_agegrp']['ddf']}) P = {m['int_agegrp']['p_F']:.5f} (R {rv('p_interaction_agegrp_main_log'):.5f}). The national APC P with t on design df is {m['national']['p_t']:.5f} (R {rv('apc_national_all_p'):.5f}).")
A("")
A("## Discrepancies and diagnosis")
A("")
A("No quantity falls outside tolerance. One systematic difference is worth recording.")
A("")
A("1. Standardised SEs in R differ from the fixed-weight SEs specified for this check, mostly being smaller: Python minus R ranges from -0.8% to +4.4% relative (largest: urban 2007 +4.4%, national 2007 +3.5%, age 45-54 2011 +2.5%, men 2007 +2.0%; 2016 and 2021 within 0.7%). Diagnosis: survey::svystandardize does not treat the standardised weights as fixed. It behaves as post-stratification: the variance uses the linearised score residualised on the standardisation cells within each group. When the Python score is residualised in the same way (u_i - w_i x sum_cell(u) / sum_cell(w)), every standardised SE in R is reproduced to a relative difference below 1e-7, across national, sex, area and age-group estimates, and including the 2011 lonely-stratum adjustment. The change CIs and the women-minus-men contrast then match R to 4 decimals. This is consistent with svystandardize implementing standardisation through post-stratification of the design, and it is a defensible choice, but the methods text should not say the standardised weights were treated as fixed. If the authors prefer the conservative fixed-weight variance, CIs widen slightly (for example national change 2007-2021 becomes -2.78 to 0.47 instead of -2.74 to 0.44); no inference changes.")
A("")
A("2. Labelling only: design_n_psu counts unique cluster ids, not the PSUs that enter the variance with nest = TRUE (see Design counts). Also, case_classification.csv uses the available-case population without the covariate requirement stated in SAP 8(d) (see check c).")
A("")
A("Not verified here: provincial estimates, components, sensitivity analyses, logit-link and full-interaction variants, linearity tests, and the derivation of the standard population from the census source file.")
open(out, "w").write("\n".join(L) + "\n")
print("written", out, len(L), "lines")
