## ============================================================================
## MoDeSt — 05_pain_analyses.R
## Postoperative pain: ward (continuous, max across in-hospital days) and
## PACU (binary: pain present vs absent, max of 30-/60-min assessments),
## each for overall perception, at rest, and with movement.
## -> Manuscript: "...and in pain levels (eFigure 2 and eTable 2 in
##    Supplement 2)"; variables derived in 01_data_preparation.R.
## ============================================================================

library(mice); library(emmeans); library(dplyr)

dat.back <- readRDS("dat_back_imputed.rds")

# ---- Ward pain (linear model, adjusted for corresponding preop pain) -------
run_ward_pain <- function(outcome, preop_cat) {
  f <- as.formula(paste0(
    outcome, " ~ factor(", preop_cat, ") + factor(geschlecht) + gruppe + I((alter - mean(alter))/10)"))
  m <- with(dat.back, lm(f))
  cat("\n---", outcome, "---\n")
  print(summary(mice::pool(m), conf.int = TRUE))
  print(pairs(emmeans(m, ~ gruppe), adjust = "none", reverse = TRUE))
}
run_ward_pain("ward_pain",     "pra_op_schmerz_cat")
run_ward_pain("ward_pain_rest","pra_op_schmerz_ruh_cat")
run_ward_pain("ward_pain_mov", "pra_op_schmerz_bew_cat")

# ---- PACU pain (logistic model, binary pain present/absent) ----------------
run_pacu_pain <- function(outcome_cat, preop_cat) {
  f <- as.formula(paste0(
    "I(as.numeric(", outcome_cat, ") - 1) ~ factor(", preop_cat,
    ") + factor(geschlecht) + gruppe + I((alter - mean(alter))/10)"))
  m <- with(dat.back, glm(f, family = binomial))
  cat("\n---", outcome_cat, "---\n")
  pairs_or <- summary(pairs(emmeans(m, ~ gruppe), adjust = "none", reverse = TRUE), infer = TRUE)
  print(data.frame(
    contrast = pairs_or$contrast,
    OR    = round(exp(pairs_or$estimate), 2),
    lower = round(exp(pairs_or$asymp.LCL), 2),
    upper = round(exp(pairs_or$asymp.UCL), 2),
    p     = round(pairs_or$p.value, 3)
  ))
}
run_pacu_pain("pacu_pain_cat",      "pra_op_schmerz_cat")
run_pacu_pain("pacu_pain_rest_cat", "pra_op_schmerz_ruh_cat")
run_pacu_pain("pacu_pain_bew_cat",  "pra_op_schmerz_bew_cat")
