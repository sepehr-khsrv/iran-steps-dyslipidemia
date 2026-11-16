# 05_data_dictionary.R : writes data_derived/data_dictionary.csv from analytic.rds with a description per column.
suppressMessages({library(dplyr); library(readr)})
W <- "."  # repository root: run from the root folder
a <- readRDS(file.path(W, "data_derived/analytic.rds"))
desc <- c(
 round = "Survey round (Gregorian year): 2007 (1386), 2011 (1390), 2016 (1395), 2021 (1400)",
 id = "Participant id, unique across rounds: round prefix + source row id (2007 Ref; 2011 ID) or source id (2016 code; 2021 familymemberid)",
 psu = "Primary sampling unit (cluster), unique across rounds. 2007 province x i4partici1; 2011 district x1d x cluster i08; 2016 prefix of 'code' before '/'; 2021 clusterid. '_ind_' = no cluster code, own PSU",
 stratum = "Stratum = province31 | area (NA if either is missing)",
 province31 = "Province, 31 current provinces (factor, manuscript spelling). 2007 'Tehran' includes Alborz. Mapped by name (province_lookup.csv)",
 area = "urban / rural",
 sex = "women / men",
 age = "Age in completed years (integer). 2007 = 1386 - birth year; 2011 floor(decimal age); 2016 c3y; 2021 age",
 agegrp = "25-34, 35-44, 45-54, 55-64 (breaks 25,35,45,55,65 right-open); 'other' outside 25-64 or missing",
 weight = "Chosen weight. 2007/2011: post-stratification of laboratory participants aged 25-64 to the census (2006/2011) by province x area x sex x 10-year age (rebuilt). 2016: weight_lab; 2021: W_Laboratory (official). NA for non-laboratory participants and for 2007/2011 lab participants outside 25-64",
 weight_alt = "Alternative weight. 2007: full-sample post-stratification with corrected province mapping (Weight_83 method); 2011: Weight_90; 2016: Weight_95; 2021: Weight_1400 (user-built)",
 weight_name = "Label of 'weight'", weight_alt_name = "Label of 'weight_alt'",
 tc = "Total cholesterol, mg/dL, after plausibility check (50-700; outside -> NA)",
 hdl = "HDL cholesterol, mg/dL, after plausibility check (5-150)",
 tg = "Triglycerides, mg/dL, after plausibility check (10-2000)",
 ldl_measured = "Measured LDL-C, mg/dL: 2011 b9l; 2021 LDLC3 (subset). NA in 2007 (none) and 2016 (released LDL is Friedewald-calculated)",
 ldl_friedewald = "tc - hdl - tg/5 when tg < 400 and all three valid; else NA",
 nonhdl = "tc - hdl",
 tc_implausible = "TRUE if the raw TC was outside 50-700 (value set to NA)", hdl_implausible = "TRUE if the raw HDL-C was outside 5-150", tg_implausible = "TRUE if the raw TG was outside 10-2000",
 pregnant = "TRUE/FALSE for women where recorded; NA for men and where not recorded. 2021 has no 'yes' code in the file",
 fasting_hours = "2021 only: approximate fasting duration parsed from free-text clock times (fasting1, fasting2). NA otherwise",
 fasting_flag = "not recorded / parsed (approximate) / parsed, implausible duration / unparsable",
 told_raised_chol = "Ever told by a doctor or health worker that cholesterol is high: 2016 h13a; 2021 h13a (-555 skip -> FALSE). NA in 2007 and 2011 (not asked)",
 lipid_med = "Currently taking oral medication for raised cholesterol: 2021 h14a (-555 skip -> FALSE; 'currently' = last 12 months per interviewer guide). NA in 2007, 2011 (not asked) and 2016 (asked, not released)",
 statin = "Currently taking a statin to prevent or treat heart disease: 2016 h19, 2021 h19",
 lab_participant = "TRUE if any of TC, HDL-C, TG was recorded (before plausibility checks)",
 interview_date = "Persian-calendar date: 2007 and 2011 interview (I5); 2021 blood-sampling date (date_b); 2016 not released",
 interview_date_gregorian = "2021 record creation date (Gregorian)",
 pcode = "Census province code used for weighting (2007: census code of the STEPS code; Alborz not separable)",
 raw_tc = "Raw TC as in source", raw_hdl = "Raw HDL-C as in source", raw_tg = "Raw TG as in source",
 raw_weight_user = "User-built original weight as supplied: Weight_83 (2007; built with a wrong province mapping), Weight_90, Weight_95, Weight_1400",
 w_official_lab = "Official laboratory weight (2016 weight_lab, 2021 W_Laboratory), all records", w_official_q = "Official questionnaire weight (2016 weight_Q, 2021 W_Questionnaire)",
 psu_spans_strata = "TRUE if the PSU's members fall in more than one stratum (mixed urban/rural members)")
dd <- tibble(column = names(a), type = sapply(a, function(x) class(x)[1]),
             n_nonmissing = sapply(a, function(x) sum(!is.na(x))),
             description = ifelse(names(a) %in% names(desc), desc[names(a)], ifelse(grepl("^raw_", names(a)), paste("Raw source value of", sub("^raw_", "", names(a)), "(round-specific; see docs/variable_map.csv)"), "")))
write_csv(dd, file.path(W, "data_derived/data_dictionary.csv")); print(dd, n = 200)
