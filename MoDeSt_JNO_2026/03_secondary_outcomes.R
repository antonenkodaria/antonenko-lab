## ============================================================================
## MoDeSt — 03_secondary_outcomes.R
## Secondary outcome: POD in PACU (tACS vs sham; tDCS vs sham)
## -> Manuscript: Results, "Secondary Outcome Measures" (PACU paragraph)
## ============================================================================

library(mice); library(emmeans); library(dplyr)

dat.back <- readRDS("dat_back_imputed.rds")

# ---- Descriptive incidence (Figure 3B; observed, non-imputed) --------------
# -> "14.6% in the tACS group (7 of 48), 21.4% in the tDCS group (12 of 56),
#     and 21.7% in the sham group (13 of 60)"
completed_data <- mice::complete(dat.back, action = 1)
table(completed_data$gruppe, completed_data$delir_im_awr2)
prop.table(table(completed_data$gruppe, completed_data$delir_im_awr2), 1)

# ---- Adjusted model (age + sex), no treatment×age interaction --------------
model_pacu <- with(dat.back, glm(
  delir_im_awr2 ~ factor(geschlecht) + gruppe + I((alter - mean(alter)) / 10),
  family = binomial
))
summary(mice::pool(model_pacu), conf.int = TRUE)

# Pairwise treatment contrasts
# -> "tACS vs sham: OR, 0.95 [95% CI, 0.38-2.36]; P = .91"
# -> "tDCS vs sham: OR, 0.91 [95% CI, 0.36-2.30]; P = .83"
em <- emmeans(model_pacu, ~ gruppe, adjust = "none")
pairs_or <- summary(pairs(em, adjust = "none", reverse = TRUE), infer = TRUE)
data.frame(
  contrast = pairs_or$contrast,
  OR       = round(exp(pairs_or$estimate), 2),
  lower    = round(exp(pairs_or$asymp.LCL), 2),
  upper    = round(exp(pairs_or$asymp.UCL), 2),
  p.value  = round(pairs_or$p.value, 2)
)
