"""Compare Python results with R results_index.json. Usage: compare.py <py_json> <r_json> <out_csv>
From the repository root:
    python3 -I verification/compare.py verification/py_results.json output/results_index.json verification/comparison.csv
"""
import sys, json
import pandas as pd, numpy as np

py = json.load(open(sys.argv[1])); R = json.load(open(sys.argv[2]))
rv = lambda k: R[k]["value"]
rows = []


def add(label, kind, rkey, pval):
    r = rv(rkey)
    diff = pval - r
    if kind == "est":   ok = abs(diff) <= 0.05
    elif kind == "se":  ok = abs(diff) / abs(r) <= 0.10
    elif kind == "apc": ok = abs(diff) <= 0.02
    elif kind == "p":   ok = (r < 0.001 and pval < 0.001) or (max(r, pval) / min(r, pval) <= 1.5)
    elif kind == "n":   ok = int(round(r)) == int(round(pval))
    rows.append(dict(quantity=label, type=kind, r_key=rkey, R=r, Python=pval, diff=diff, within_tol="yes" if ok else "NO"))


P = py["prev"]
for yr in [2007, 2011, 2016, 2021]:
    add(f"n analytic {yr}", "n", f"n_analytic_{yr}", py["design"][str(yr)]["n"])
for yr in [2007, 2011, 2016, 2021]:
    for grp, pk, rk in [("national", "national", "national_all"), ("women", "women", "sex_women"), ("men", "men", "sex_men")]:
        for typ in ["std", "crude"]:
            p = P[f"{typ}_{pk}_{yr}"]
            for f, kind in [("est", "est"), ("se", "se"), ("lower", "est"), ("upper", "est")]:
                add(f"{typ} {grp} {yr} {f}", kind, f"prev_{typ}_{rk}_{yr}_{f}", p[f])
    for a in ["urban", "rural"]:
        add(f"std {a} {yr} est", "est", f"prev_std_area_{a}_{yr}_est", P[f"std_{a}_{yr}"]["est"])
        add(f"std {a} {yr} se", "se", f"prev_std_area_{a}_{yr}_se", P[f"std_{a}_{yr}"]["se"])
    for ag in ["25-34", "35-44", "45-54", "55-64"]:
        k = ag.replace("-", "_")
        add(f"std age {ag} {yr} est", "est", f"prev_std_agegrp_{k}_{yr}_est", P[f"std_ag{ag}_{yr}"]["est"])
        add(f"std age {ag} {yr} se", "se", f"prev_std_agegrp_{k}_{yr}_se", P[f"std_ag{ag}_{yr}"]["se"])

C = py["change"]
for lab, pk, rk in [("change national 2007-2021", "national_2007_2021", "change_std_national_all_2007_2021"),
                    ("change women 2007-2021", "women_2007_2021", "change_std_sex_women_2007_2021"),
                    ("change men 2007-2021", "men_2007_2021", "change_std_sex_men_2007_2021"),
                    ("change women 2016-2021", "women_2016_2021", "change_std_sex_women_2016_2021"),
                    ("women minus men change 2007-2021", "women_minus_men_2007_2021", "diffchange_std_women_minus_men_2007_2021")]:
    for f in ["est", "lower", "upper"]:
        add(f"{lab} {f}", "est", f"{rk}_{f}", C[pk][f])
    add(f"{lab} P", "p", f"{rk}_p", C[pk]["p"])

M = py["models"]
for lab, pk, rk in [("APC national", "national", "apc_national_all"), ("APC women", "women", "apc_sex_women"),
                    ("APC men", "men", "apc_sex_men"), ("APC women (interaction model)", "intmodel_women", "apc_intmodel_sex_women"),
                    ("APC men (interaction model)", "intmodel_men", "apc_intmodel_sex_men")]:
    for f in ["est", "lower", "upper"]:
        add(f"{lab} {f}", "apc", f"{rk}_{f}", M[pk][f])
    add(f"{lab} P", "p", f"{rk}_p", M[pk]["p"])
add("P interaction year x sex", "p", "p_interaction_sex_main_log", M["int_sex"]["p"])
add("P interaction year x age group (3 df)", "p", "p_interaction_agegrp_main_log", M["int_agegrp"]["p_F"])
for yr in [2007, 2011, 2016, 2021]:
    add(f"PSUs {yr} (unique ids)", "n", f"design_n_psu_{yr}", py["design"][str(yr)]["psu_unique_ids"])
    add(f"strata {yr}", "n", f"design_n_strata_{yr}", py["design"][str(yr)]["strata"])

df = pd.DataFrame(rows)
df.to_csv(sys.argv[3], index=False)
pd.set_option("display.width", 250); pd.set_option("display.max_rows", 500)
print(df[["quantity", "R", "Python", "diff", "within_tol"]].to_string())
print("\nNOT within tolerance:", (df.within_tol == "NO").sum(), "of", len(df))
