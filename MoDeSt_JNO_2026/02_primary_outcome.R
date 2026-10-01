## ============================================================================
## MoDeSt — 02_primary_outcome.R
## Primary outcome: POD in the ward (tACS vs sham); Secondary: tDCS vs sham
## -> Manuscript: Abstract "RESULTS"; Results, "Primary Outcome Measure" and
##    first paragraph of "Secondary Outcome Measures"; Table not applicable
##    (numbers reported in text only).
## Requires: dat_back_imputed.rds (mids object, m = 30), produced by
##           01_data_preparation.R (group-wise multiple imputation).
## ============================================================================

library(mice)
library(emmeans)
library(dplyr)

dat.back <- readRDS("dat_back_imputed.rds")

# ---- Descriptive incidence (Figure 3A numbers; observed, non-imputed) ------
# -> "the incidence rates were 11.5% in the tACS group (6 of 52),
#     14.3% in the tDCS group (9 of 63), and 9.2% in the sham group (6 of 65)"
completed_data <- mice::complete(dat.back, action = 1)
table(completed_data$gruppe, completed_data$inc_pod2)
prop.table(table(completed_data$gruppe, completed_data$inc_pod2), 1)

# ---- Adjusted model (age + sex), no treatment×age interaction --------------
# This is the model underlying the reported primary/secondary ORs.
model_primary <- with(dat.back, glm(
  inc_pod2 ~ factor(geschlecht) + gruppe + I((alter - mean(alter)) / 10),
  family = binomial
))

pooled <- summary(mice::pool(model_primary), conf.int = TRUE) %>%
  mutate(
    OR    = exp(estimate),
    OR_lo = exp(conf.low),
    OR_hi = exp(conf.high)
  )
print(pooled[, c("term", "OR", "OR_lo", "OR_hi", "p.value")])

# Pairwise treatment contrasts (tACS vs sham; tDCS vs sham)
# -> "tACS vs sham: OR, 1.60 [95% CI, 0.47-5.46]; P = .45"
# -> "tDCS vs sham: OR, 1.51 [95% CI, 0.49-4.68]; P = .47"
em <- emmeans(model_primary, ~ gruppe, adjust = "none")
pairs_or <- summary(pairs(em, adjust = "none", reverse = TRUE), infer = TRUE)
data.frame(
  contrast = pairs_or$contrast,
  OR       = round(exp(pairs_or$estimate), 2),
  lower    = round(exp(pairs_or$asymp.LCL), 2),
  upper    = round(exp(pairs_or$asymp.UCL), 2),
  p.value  = round(pairs_or$p.value, 2)
)
