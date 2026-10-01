## ============================================================================
## MoDeSt — 04_pod_severity_motor.R
## POD severity (3D-CAM-S, ward & PACU) and motor type (RASS, PACU)
## -> Manuscript: Results, POD severity paragraph ("ratio of geometric means...")
## -> eFigure 1A/B, eTable 1 (Supplement 2)
## max_cam_s / post_max_3d_cam_s / rass_pacu are derived in 01_data_preparation.R
## ============================================================================

library(mice); library(emmeans); library(dplyr); library(nnet)

dat.back <- readRDS("dat_back_imputed.rds")

## ---- POD severity: max CAM-S, ward -----------------------------------------
# -> "tACS vs sham: ratio of geometric means, 1.03 [95% CI, 0.84-1.26]; P = .79"
# -> "tDCS vs sham: ratio of geometric means, 0.92 [95% CI, 0.77-1.11]; P = .40"
model_ward_sev <- with(dat.back, lm(
  log(max_cam_s + 1) ~ factor(geschlecht) + gruppe + I(alter / 10)
))
summary(mice::pool(model_ward_sev), conf.int = TRUE)
pairs(emmeans(model_ward_sev, ~ gruppe, regrid = "log", type = "response"),
      adjust = "none", reverse = TRUE)

## ---- POD severity: max CAM-S, PACU -----------------------------------------
# -> "tACS vs sham: ratio of geometric means, 0.95 [95% CI, 0.72-1.24]; P = .68"
# -> "tDCS vs sham: ratio of geometric means, 1.01 [95% CI, 0.79-1.27]; P = .96"
model_pacu_sev <- with(dat.back, lm(
  log(post_max_3d_cam_s + 1) ~ factor(geschlecht) + gruppe + I(alter / 10)
))
summary(mice::pool(model_pacu_sev), conf.int = TRUE)
pairs(emmeans(model_pacu_sev, ~ gruppe, regrid = "log", type = "response"),
      adjust = "none", reverse = TRUE)

## ---- Motor type: RASS in PACU (multinomial; ref = "awake and calm") -------
# -> eFigure 1B / eTable 1 in Supplement 2
long <- mice::complete(dat.back, action = "long", include = TRUE)
long$rass_pacu <- relevel(factor(long$rass_pacu), ref = "awake and calm")
dat.back_rass <- as.mids(long)

model_rass <- with(dat.back_rass, multinom(
  rass_pacu ~ factor(geschlecht) + gruppe + I((alter - mean(alter)) / 10),
  trace = FALSE
))
pooled <- summary(mice::pool(model_rass), conf.int = TRUE)
grp <- pooled[grep("gruppe", pooled$term), ]
data.frame(
  term  = grp$term,
  OR    = round(exp(grp$estimate), 3),
  lower = round(exp(grp$estimate - 1.96 * grp$std.error), 3),
  upper = round(exp(grp$estimate + 1.96 * grp$std.error), 3),
  p     = round(grp$p.value, 4)
)
