"""Independent Stage 3 verification of dyslipidaemia prevalence and trends.
Usage: python3 -I verify.py <data_dir> <out_json>
From the repository root, after Rscript R/00_run_all.R:
    python3 -I verification/verify.py . verification/py_results.json
<data_dir> is the repository root. The script reads data_derived/analytic.csv.gz and
output/tables/standard_population_2016.csv under it, and writes aggregate results only.
"""
import sys, json
import numpy as np, pandas as pd
from scipy import stats

DATA = sys.argv[1]
OUT = sys.argv[2]

d = pd.read_csv(f"{DATA}/data_derived/analytic.csv.gz", low_memory=False)
std = pd.read_csv(f"{DATA}/output/tables/standard_population_2016.csv")
res = {}

# ---------------- population filters ----------------
AG = ["25-34", "35-44", "45-54", "55-64"]
flow = {}
x = d.copy()
for r, g in x.groupby("round"):
    flow[int(r)] = {"raw": len(g)}
x = x[(x.age >= 25) & (x.age <= 64)]
for r, g in x.groupby("round"): flow[int(r)]["age25_64"] = len(g)
x = x[~(x.pregnant == True)]
for r, g in x.groupby("round"): flow[int(r)]["not_pregnant"] = len(g)
x = x[x.tc.notna() & x.hdl.notna() & x.tg.notna()]
for r, g in x.groupby("round"): flow[int(r)]["three_lipids"] = len(g)
x = x[x.sex.notna() & x.area.notna() & x.province31.notna()]
for r, g in x.groupby("round"): flow[int(r)]["covariates"] = len(g)
x = x[x.weight.notna() & (x.weight > 0) & x.psu.notna()]
for r, g in x.groupby("round"): flow[int(r)]["weight_psu"] = len(g)
res["flow"] = flow

# independently derived age group (check against supplied)
x["ag"] = pd.cut(x.age, [25, 35, 45, 55, 65], right=False, labels=AG).astype(str)
res["agegrp_mismatch"] = int((x.ag != x.agegrp).sum())

# ---------------- outcome ----------------
ldl = np.where(x.tg < 400, x.tc - x.hdl - x.tg / 5, np.nan)
res["ldl_mismatch"] = int(np.nansum(np.abs(ldl - x.ldl_friedewald.values) > 1e-6) +
                          (np.isnan(ldl) != x.ldl_friedewald.isna().values).sum())
lowhdl = np.where(x.sex == "men", x.hdl < 40, x.hdl < 50)
comp = (x.tc >= 200) | (x.tg >= 150) | (pd.Series(ldl, index=x.index) >= 130) | pd.Series(lowhdl, index=x.index)
x["y"] = comp.astype(float)
nonhdl = x.tc - x.hdl
x["y_nonhdl"] = (comp | (nonhdl >= 160)).astype(float)
res["nonhdl_added_cases"] = int((x.y_nonhdl != x.y).sum())
# also using raw (pre-plausibility) values among the analytic sample
raw_nonhdl = x.raw_tc - x.raw_hdl
res["nonhdl_added_cases_raw"] = int(((raw_nonhdl >= 160) & (x.y == 0)).sum())
res["nonhdl160_tc_lt200_hdl_ge40"] = int(((nonhdl >= 160) & (x.tc < 200) & (x.hdl >= 40)).sum())

x["stratum2"] = x.province31.astype(str) + "|" + x.area.astype(str)
x["year_c"] = x["round"] - 2007

# ---------------- design summary ----------------
des = {}
for r, g in x.groupby("round"):
    psu_by_str = g.groupby("stratum2").psu.nunique()
    des[int(r)] = {
        "n": len(g),
        "strata": int(psu_by_str.size),
        "psu_unique_ids": int(g.psu.nunique()),
        "psu_nested": int(psu_by_str.sum()),
        "lonely_strata": int((psu_by_str == 1).sum()),
        "lonely_strata_names": list(psu_by_str[psu_by_str == 1].index),
        "psu_spanning_strata": int(g.groupby("psu").stratum2.nunique().gt(1).sum()),
        "ind_psu": int(g.psu.astype(str).str.contains("_ind_").sum()),
        "sum_weight": float(g.weight.sum()),
    }
res["design"] = des


# ---------------- variance machinery ----------------
def strat_var(U, strata, psu, lonely="adjust"):
    """U: n x k score matrix. Returns k x k between-PSU variance, survey 'adjust' rule.
    survey::svyrecvar onestage: within stratum h with n_h PSUs, centre PSU totals at stratum
    mean, scale n_h/(n_h-1). For n_h == 1 with 'adjust': centre at the grand mean of PSU totals
    (over all PSUs of the design) and scale 1."""
    U = np.asarray(U, float)
    if U.ndim == 1: U = U[:, None]
    df = pd.DataFrame(U)
    key = pd.Series(strata).astype(str).values + "#" + pd.Series(psu).astype(str).values
    df["s"] = pd.Series(strata).astype(str).values
    df["k"] = key
    tot = df.groupby(["s", "k"]).sum(numeric_only=True)
    sidx = tot.index.get_level_values(0)
    T = tot.values
    grand = T.mean(axis=0)   # mean over all PSUs of the design
    V = np.zeros((U.shape[1], U.shape[1]))
    for s in np.unique(sidx):
        Th = T[sidx == s]
        nh = Th.shape[0]
        if nh > 1:
            C = Th - Th.mean(axis=0)
            V += nh / (nh - 1) * C.T @ C
        else:
            if lonely == "adjust":
                C = Th - grand
                V += C.T @ C
    return V


def ratio_prev(g, w, ycol="y", strata_full=None, psu_full=None, mask=None):
    """weighted proportion in domain mask (all of g if None); variance computed on full design
    (domain estimation: scores zero outside domain)."""
    y = g[ycol].values
    if mask is None: mask = np.ones(len(g), bool)
    wm = np.where(mask, w, 0.0)
    p = (wm * y).sum() / wm.sum()
    u = wm * (y - p) / wm.sum()
    V = strat_var(u, g.stratum2.values, g.psu.values)[0, 0]
    se = np.sqrt(V)
    lg = np.log(p / (1 - p)); h = 1.96 * se / (p * (1 - p))
    lo, hi = 1 / (1 + np.exp(-(lg - h))), 1 / (1 + np.exp(-(lg + h)))
    return dict(est=100 * p, se=100 * se, lower=100 * lo, upper=100 * hi, n=int(mask.sum())), u


# standard proportions
std["agegrp"] = std.agegrp.astype(str)
P16 = std.set_index(["agegrp", "sex", "area"]).population
P16 = P16 / P16.sum()


def std_weights(g, mask, by):
    """svystandardize: within domain mask, scale weights so cell weighted proportion equals
    the standard proportion over 'by' variables."""
    w = g.weight.values.copy()
    w2 = np.zeros_like(w)
    stdp = P16.groupby(level=by).sum()
    stdp = stdp / stdp.sum()
    gm = g[mask]
    tot = gm.weight.sum()
    cells = gm.groupby([gm[b] for b in by]).weight.sum() / tot
    keys = list(zip(*[g[b].values for b in by])) if len(by) > 1 else list(g[by[0]].values)
    for i in np.where(mask)[0]:
        k = keys[i]
        w2[i] = w[i] * stdp.loc[k] / cells.loc[k]
    return w2


prev = {}
uscores = {}
for r, g in x.groupby("round"):
    g = g.reset_index(drop=True)
    r = int(r)
    allm = np.ones(len(g), bool)
    prev[f"crude_national_{r}"], _ = ratio_prev(g, g.weight.values)
    w_nat = std_weights(g, allm, ["agegrp", "sex", "area"])
    prev[f"std_national_{r}"], uscores[("nat", r)] = ratio_prev(g, w_nat)
    for s in ["women", "men"]:
        m = (g.sex == s).values
        prev[f"crude_{s}_{r}"], _ = ratio_prev(g, g.weight.values, mask=m)
        ws = std_weights(g, m, ["agegrp", "area"])
        prev[f"std_{s}_{r}"], uscores[(s, r)] = ratio_prev(g, ws, mask=m)
    for a in ["urban", "rural"]:
        m = (g.area == a).values
        wa = std_weights(g, m, ["agegrp", "sex"])
        prev[f"std_{a}_{r}"], _ = ratio_prev(g, wa, mask=m)
    for ag in AG:
        m = (g.agegrp == ag).values
        wa = std_weights(g, m, ["sex", "area"])
        prev[f"std_ag{ag}_{r}"], _ = ratio_prev(g, wa, mask=m)
    # women-minus-men within-round covariance (same PSUs)
    uw, um = uscores[("women", r)], uscores[("men", r)]
    V = strat_var(np.column_stack([uw, um]), g.stratum2.values, g.psu.values)
    prev[f"cov_women_men_{r}"] = 1e4 * V[0, 1]
res["prev"] = prev

# ---- secondary: variance treating standardisation as post-stratification (calibration) ----
# score residualised within standardisation cells: u_i - w_i * sum_cell(u) / sum_cell(w)
def ps_se(g, w, mask, by):
    y = g.y.values
    wm = np.where(mask, w, 0.0)
    p = (wm * y).sum() / wm.sum()
    u = wm * (y - p) / wm.sum()
    cell = pd.Series(np.where(mask, g[by].astype(str).agg("|".join, axis=1), "OUT"))
    su = pd.Series(u).groupby(cell).transform("sum").values
    sw = pd.Series(wm).groupby(cell).transform("sum").values
    u2 = np.where(mask, u - wm * su / np.where(sw > 0, sw, 1), 0.0)
    return u2

prev_ps = {}
for r, g in x.groupby("round"):
    g = g.reset_index(drop=True); r = int(r)
    specs = [("national", np.ones(len(g), bool), ["agegrp", "sex", "area"])]
    specs += [(s, (g.sex == s).values, ["agegrp", "area"]) for s in ["women", "men"]]
    specs += [(a, (g.area == a).values, ["agegrp", "sex"]) for a in ["urban", "rural"]]
    specs += [("ag" + a, (g.agegrp == a).values, ["sex", "area"]) for a in AG]
    us = {}
    for lab, m, by in specs:
        w = std_weights(g, m, by)
        us[lab] = ps_se(g, w, m, by)
        prev_ps[f"{lab}_{r}"] = 100 * np.sqrt(strat_var(us[lab], g.stratum2.values, g.psu.values)[0, 0])
    V = strat_var(np.column_stack([us["women"], us["men"]]), g.stratum2.values, g.psu.values)
    prev_ps[f"cov_women_men_{r}"] = 1e4 * V[0, 1]
res["se_poststrat"] = prev_ps
def change_ps(k1, k2, a, b):
    est = prev[b]["est"] - prev[a]["est"]; se = np.sqrt(prev_ps[k1] ** 2 + prev_ps[k2] ** 2)
    return dict(est=est, se=se, lower=est - 1.96 * se, upper=est + 1.96 * se, p=2 * stats.norm.sf(abs(est / se)))
chps = {}
chps["national_2007_2021"] = change_ps("national_2007", "national_2021", "std_national_2007", "std_national_2021")
chps["women_2007_2021"] = change_ps("women_2007", "women_2021", "std_women_2007", "std_women_2021")
chps["men_2007_2021"] = change_ps("men_2007", "men_2021", "std_men_2007", "std_men_2021")
chps["women_2016_2021"] = change_ps("women_2016", "women_2021", "std_women_2016", "std_women_2021")
e = chps["women_2007_2021"]["est"] - chps["men_2007_2021"]["est"]
v = chps["women_2007_2021"]["se"] ** 2 + chps["men_2007_2021"]["se"] ** 2 - 2 * (prev_ps["cov_women_men_2007"] + prev_ps["cov_women_men_2021"])
se = np.sqrt(v)
chps["women_minus_men_2007_2021"] = dict(est=e, se=se, lower=e - 1.96 * se, upper=e + 1.96 * se, p=2 * stats.norm.sf(abs(e / se)))
res["change_poststrat"] = chps


def change(a, b):
    est = b["est"] - a["est"]; se = np.sqrt(a["se"] ** 2 + b["se"] ** 2)
    return dict(est=est, se=se, lower=est - 1.96 * se, upper=est + 1.96 * se,
                p=2 * stats.norm.sf(abs(est / se)))


ch = {}
ch["national_2007_2021"] = change(prev["std_national_2007"], prev["std_national_2021"])
ch["women_2007_2021"] = change(prev["std_women_2007"], prev["std_women_2021"])
ch["men_2007_2021"] = change(prev["std_men_2007"], prev["std_men_2021"])
ch["women_2016_2021"] = change(prev["std_women_2016"], prev["std_women_2021"])
# women minus men difference in change, with within-round covariance
dw, dm = ch["women_2007_2021"], ch["men_2007_2021"]
est = dw["est"] - dm["est"]
var_ind = dw["se"] ** 2 + dm["se"] ** 2
var_cov = var_ind - 2 * (prev["cov_women_men_2007"] + prev["cov_women_men_2021"])
for lab, v in [("women_minus_men_2007_2021", var_cov), ("women_minus_men_2007_2021_nocov", var_ind)]:
    se = np.sqrt(v)
    ch[lab] = dict(est=est, se=se, lower=est - 1.96 * se, upper=est + 1.96 * se,
                   p=2 * stats.norm.sf(abs(est / se)))
res["change"] = ch


# ---------------- Poisson models ----------------
def design_matrix(g, terms):
    cols = {"const": np.ones(len(g))}
    for t in terms:
        if t == "year_c": cols["year_c"] = g.year_c.values.astype(float)
        elif t == "agegrp":
            for a in AG[1:]: cols[f"ag{a}"] = (g.agegrp == a).values.astype(float)
        elif t == "sex": cols["men"] = (g.sex == "men").values.astype(float)
        elif t == "area": cols["urban"] = (g.area == "urban").values.astype(float)
        elif t == "year_c:sex": cols["year_c:men"] = g.year_c.values * (g.sex == "men").values
        elif t == "year_c:agegrp":
            for a in AG[1:]: cols[f"year_c:ag{a}"] = g.year_c.values * (g.agegrp == a).values
    return pd.DataFrame(cols)


def fit_poisson(g, terms, mask=None):
    X = design_matrix(g, terms); names = list(X.columns); X = X.values
    y = g.y.values; w = g.weight.values.astype(float)
    if mask is not None: w = np.where(mask, w, 0.0)
    b = np.zeros(X.shape[1]); b[0] = np.log((w * y).sum() / w.sum())
    for it in range(100):
        mu = np.exp(X @ b)
        grad = X.T @ (w * (y - mu))
        H = X.T @ (X * (w * mu)[:, None])
        step = np.linalg.solve(H, grad)
        b += step
        if np.max(np.abs(step)) < 1e-12: break
    mu = np.exp(X @ b)
    H = X.T @ (X * (w * mu)[:, None])
    bread = np.linalg.inv(H)
    S = X * (w * (y - mu))[:, None]
    strata = g["round"].astype(str).values + "|" + g.stratum2.values
    meat = strat_var(S, strata, g.psu.values)
    V = bread @ meat @ bread
    return pd.Series(b, names), pd.DataFrame(V, names, names)


def apc(b, V, name="year_c"):
    se = np.sqrt(V.loc[name, name]); be = b[name]
    return dict(est=100 * (np.exp(be) - 1), lower=100 * (np.exp(be - 1.96 * se) - 1),
                upper=100 * (np.exp(be + 1.96 * se) - 1), se_beta=se, beta=be,
                p=2 * stats.norm.sf(abs(be / se)))


mods = {}
b, V = fit_poisson(x, ["year_c", "agegrp", "sex", "area"])
mods["national"] = apc(b, V)
for s in ["women", "men"]:
    b, V = fit_poisson(x, ["year_c", "agegrp", "area"], mask=(x.sex == s).values)
    mods[s] = apc(b, V)
    gs = x[x.sex == s]
    b, V = fit_poisson(gs, ["year_c", "agegrp", "area"])
    mods[s + "_subsetdropped"] = apc(b, V)
# domain-style subgroup APC (variance on full design, scores zero outside the domain)
b, V = fit_poisson(x, ["year_c", "agegrp", "area", "sex", "year_c:sex"])
be = b["year_c:men"]; se = np.sqrt(V.loc["year_c:men", "year_c:men"])
mods["int_sex"] = dict(beta=be, se=se, p=2 * stats.norm.sf(abs(be / se)))
c = np.zeros(len(b)); c[list(b.index).index("year_c")] = 1; c[list(b.index).index("year_c:men")] = 1
bm = c @ b.values; sm = np.sqrt(c @ V.values @ c)
mods["intmodel_women"] = apc(b, V)
mods["intmodel_men"] = dict(est=100 * (np.exp(bm) - 1), lower=100 * (np.exp(bm - 1.96 * sm) - 1),
                            upper=100 * (np.exp(bm + 1.96 * sm) - 1), p=2 * stats.norm.sf(abs(bm / sm)))
b, V = fit_poisson(x, ["year_c", "agegrp", "sex", "area", "year_c:agegrp"])
nm = [f"year_c:ag{a}" for a in AG[1:]]
bb = b[nm].values; VV = V.loc[nm, nm].values
W = float(bb @ np.linalg.solve(VV, bb))
mods["int_agegrp"] = dict(wald=W, df=3, p_chisq=stats.chi2.sf(W, 3))
# F version with design df (n_psu - n_strata), as regTermTest does
strata = x["round"].astype(str) + "|" + x.stratum2
npsu = (x.groupby(strata).psu.nunique()).sum(); nstr = strata.nunique()
ddf = npsu - nstr
mods["int_agegrp"]["p_F"] = stats.f.sf(W / 3, 3, ddf)
mods["int_agegrp"]["ddf"] = int(ddf)
mods["int_sex"]["p_t"] = 2 * stats.t.sf(abs(mods["int_sex"]["beta"] / mods["int_sex"]["se"]), ddf)
mods["national"]["p_t"] = 2 * stats.t.sf(abs(mods["national"]["beta"] / mods["national"]["se_beta"]), ddf)
res["models"] = mods

# ---------------- standard population checks ----------------
sp = {}
sp["sum_prop_file"] = float(std.proportion.sum())
sp["max_abs_prop_diff"] = float(np.max(np.abs(std.population / std.population.sum() - std.proportion)))
sp["total_pop"] = int(std.population.sum())
sp["urban_share"] = float(std[std.area == "urban"].population.sum() / std.population.sum())
sp["men_share"] = float(std[std.sex == "men"].population.sum() / std.population.sum())
sp["age_shares"] = (std.groupby("agegrp").population.sum() / std.population.sum()).to_dict()
sp["n_cells"] = int(len(std))
res["stdpop"] = sp


def conv(o):
    if isinstance(o, dict): return {str(k): conv(v) for k, v in o.items()}
    if isinstance(o, (list, tuple)): return [conv(v) for v in o]
    if isinstance(o, (np.floating,)): return float(o)
    if isinstance(o, (np.integer,)): return int(o)
    return o


json.dump(conv(res), open(OUT, "w"), indent=1)
print(json.dumps(conv(res), indent=1)[:6000])
