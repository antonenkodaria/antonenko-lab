## ============================================================================
## MoDeSt — 09_historical_cohort_sensitivity.R
## Sensitivity analysis on the modified full analysis set (main cohort +
## historical cohort excluded for stimulation-device malfunction,
## flagged via stimulator_intact == 0 in the raw data).
## -> Manuscript: Figure 2 footnote a; Supplement 1 (modified full analysis
##    set results for POD, PACU delirium, CAM-S severity, ward pain, RASS).
## Data preparation identical to 01_data_preparation.R, run on the combined
## (main + historical) cohort -> produces dat_back_imputed.rds / data_prepared.rds
## used below.
## ============================================================================

library(mice); library(emmeans); library(dplyr); library(nnet)

dat.back <- readRDS("dat_back_imputed.rds")   # combined cohort, imputed
data     <- readRDS("data_prepared.rds")      # combined cohort, non-imputed

# ---- 1. POD (ward) ----------------------------------------------------------
model_pod <- with(dat.back, glm(
  inc_pod2 ~ factor(geschlecht) + gruppe * I((alter - mean(alter)) / 10),
  family = binomial
))
summary(mice::pool(model_pod), conf.int = TRUE)
pairs(emmeans(model_pod, ~ gruppe * alter, at = list(alter = c(65, 75, 85))),
      adjust = "none")[c(1, 2, 22, 23, 34, 35)]

# ---- 2. PACU delirium --------------------------------------------------------
model_pacu <- with(dat.back, glm(
  delir_im_awr2 ~ factor(geschlecht) + gruppe * I((alter - mean(alter)) / 10),
  family = binomial
))
summary(mice::pool(model_pacu), conf.int = TRUE)
pairs(emmeans(model_pacu, ~ gruppe * alter, at = list(alter = c(65, 75, 85))),
      adjust = "none", reverse = TRUE)[c(1, 2, 10, 14, 28, 35)]

# ---- 3. Max CAM-S score (ward, severity) ------------------------------------
model_cams <- with(dat.back, lm(
  log(max_cam_s + 1) ~ factor(geschlecht) + gruppe * I((alter - mean(alter)) / 10)
))
summary(mice::pool(model_cams), conf.int = TRUE)
emmeans(model_cams, ~ gruppe * alter, at = list(alter = c(65, 75, 85)),
        regrid = "log", type = "response", adjust = "none")

# ---- 4. Ward pain (overall) --------------------------------------------------
model_pain <- with(dat.back, lm(
  ward_pain ~ factor(pra_op_schmerz_cat) + factor(geschlecht) +
    gruppe * I((alter - mean(alter)) / 10)
))
summary(mice::pool(model_pain), conf.int = TRUE)
emmeans(model_pain, ~ gruppe * alter, at = list(alter = c(65, 75, 85)), adjust = "none")

# ---- 5. RASS in PACU (multinomial: agitated / calm[ref] / sedated) ----------
long <- mice::complete(dat.back, action = "long", include = TRUE)
long$rass_pacu <- relevel(factor(long$rass_pacu), ref = "awake and calm")
dat.back_rass <- as.mids(long)

model_rass <- with(dat.back_rass, multinom(
  rass_pacu ~ factor(geschlecht) + gruppe + I((alter - mean(alter)) / 10),
  trace = FALSE
))
pooled_rass <- summary(mice::pool(model_rass), conf.int = TRUE)
grp <- pooled_rass[grep("gruppe", pooled_rass$term), ]
data.frame(
  term  = grp$term,
  OR    = exp(grp$estimate),
  lower = exp(grp$estimate - 1.96 * grp$std.error),
  upper = exp(grp$estimate + 1.96 * grp$std.error),
  p     = grp$p.value
)
