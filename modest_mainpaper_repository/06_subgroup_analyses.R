## ============================================================================
## MoDeSt — 06_subgroup_analyses.R
## Prespecified subgroup analyses (interaction: moderator x treatment group)
## for POD in ward (inc_pod2) and POD in PACU (delir_im_awr2).
## -> Manuscript: Results, "Prespecified Subgroup Analyses"; eTable 4 (ward)
##    and eTable 5 (PACU) in Supplement 2.
## All models: same adjusted logistic regression used for the main outcomes
## (age + sex), with an added moderator x group interaction term. Significance
## of the interaction is tested via D1() (mice), comparing the interaction
## model against the corresponding main-effects-only model.
## ============================================================================

library(mice); library(emmeans); library(dplyr); library(tidyr)

dat.back <- readRDS("dat_back_imputed.rds")

# ---- Generic subgroup-interaction test --------------------------------------
# outcome    : "inc_pod2" (ward) or "delir_im_awr2" (PACU)
# moderator  : right-hand-side term, e.g. "factor(geschlecht)",
#              "I(log(op_duration))", "moca_cat", "ppi_in_ruhe_cat"
# at         : optional named list of moderator values for emmeans (continuous
#              moderators only), matching Figure 3 duration levels (min)
run_subgroup <- function(outcome, moderator, at = NULL, log_transform = FALSE) {
  f_int <- as.formula(paste0(
    outcome, " ~ I((alter - mean(alter))/10) + factor(geschlecht) + gruppe * ", moderator))
  f_main <- as.formula(paste0(
    outcome, " ~ I((alter - mean(alter))/10) + factor(geschlecht) + gruppe + ", moderator))

  m_int  <- with(dat.back, glm(f_int,  family = binomial))
  m_main <- with(dat.back, glm(f_main, family = binomial))

  cat("\n===", outcome, "x", moderator, "===\n")
  print(D1(m_int, m_main))   # interaction test -> eTable 4 / eTable 5 OR + P

  if (!is.null(at)) {
    em <- emmeans(m_int, ~ gruppe | !!names(at), at = at)
    if (log_transform) em <- regrid(em, transform = "log")
  } else {
    em <- emmeans(m_int, as.formula(paste("~ gruppe *", moderator)))
  }
  print(pairs(em, adjust = "none", reverse = TRUE))
}

# ---- Run for both outcomes across all prespecified moderators --------------
# -> Manuscript: "age, sex, duration, and type of surgery, baseline cognitive
#    function, and preoperative pain levels"
for (outcome in c("inc_pod2", "delir_im_awr2")) {
  run_subgroup(outcome, "factor(geschlecht)")
  run_subgroup(outcome, "factor(fachrichtung2)")
  run_subgroup(outcome, "I(log(op_duration))",
               at = list(op_duration = c(60, 120, 180, 240, 300)), log_transform = TRUE)
  run_subgroup(outcome, "I(log(anest_duration))",
               at = list(anest_duration = c(60, 120, 180, 240, 300)), log_transform = TRUE)
  run_subgroup(outcome, "moca_cat")
  run_subgroup(outcome, "ppi_mittlerer_schmerz_cat")
  run_subgroup(outcome, "ppi_in_ruhe_cat")
  run_subgroup(outcome, "ppi_bei_bew_cat")
}
# Note: age itself is the moderator in the primary/secondary models
# (gruppe * age term), see 02_primary_outcome.R / 03_secondary_outcomes.R.

# ============================================================================
# Significant interaction: PACU delirium x duration of surgery (Figure 3C)
# -> "interaction term: OR, 0.15 [95% CI, 0.02-0.97]; P = .046"
# -> absolute risk difference tACS vs sham: -26.6% at 3h, -46.5% at 4h
#    (see pacu_risk_diff_table_opduration.csv for full 60-300 min grid)
# ============================================================================
model_pacu_duration <- with(dat.back, glm(
  delir_im_awr2 ~ I((alter - mean(alter)) / 10) + factor(geschlecht) +
    gruppe * I(log(op_duration)),
  family = binomial
))
summary(mice::pool(model_pacu_duration), conf.int = TRUE)  # -> interaction term OR

em_duration <- emmeans(model_pacu_duration, ~ gruppe * op_duration,
                        at = list(op_duration = c(60, 120, 180, 240, 300)))
em_probs <- as.data.frame(summary(em_duration, type = "response"))

risk_diffs <- em_probs %>%
  select(gruppe, op_duration, prob, SE) %>%
  pivot_wider(names_from = gruppe, values_from = c(prob, SE)) %>%
  mutate(
    tacs_rd    = prob_tACS - prob_sham,
    tacs_se    = sqrt(SE_tACS^2 + SE_sham^2),
    tacs_lower = tacs_rd - 1.96 * tacs_se,
    tacs_upper = tacs_rd + 1.96 * tacs_se
  )
print(risk_diffs[, c("op_duration", "tacs_rd", "tacs_lower", "tacs_upper")])
