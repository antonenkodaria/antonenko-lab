## ============================================================================
## MoDeSt — 01_data_preparation.R
## Loads raw eCRF export, derives outcome/covariate variables, performs
## group-wise multiple imputation (m = 30). Produces dat_back_imputed.rds
## and data_prepared.rds used by all subsequent scripts.
## -> Manuscript: Statistical Analysis section ("Group-specific multiple
##    imputation of missing values...").
## ============================================================================

library(readxl); library(dplyr); library(tidyr); library(mice)

set.seed(246)

data <- read_excel("eCRF_MoDeSt_Daria.xlsx", sheet = 1, guess_max = 10000)

# ---- OP/anesthesia duration: joined from separate OPZeiten file ------------
# -> confirmed via script_MoDeSt_Daria_2025-10-17_add2fullsampletable.R
op_zeiten <- read_excel("OPZeiten_Modest_20250818.xlsx") %>%
  rename(op_duration_raw = Echte_OPDauer_min, anest_duration_raw = Echte_anesthesieDauer_min)
data <- data %>%
  left_join(op_zeiten, by = c("e_pod_id" = "Modest_ID")) %>%
  mutate(
    op_duration    = ifelse(op_duration_raw == -99, NA_real_, op_duration_raw),
    anest_duration = ifelse(anest_duration_raw == -99, NA_real_, anest_duration_raw)
  ) %>%
  select(-op_duration_raw, -anest_duration_raw)

# ---- Core variable transformations ------------------------------------------
data$alter      <- as.numeric(data$alter)
data$geschlecht <- factor(data$geschlecht, levels = c(1, 2), labels = c("Male", "Female"))
data$fachrichtung2 <- as.factor(data$fachrichtung2)
data$moca_cat   <- ifelse(data$mo_ca_score <= 26, "\u226426", ">26")

# 3D-CAM-S severity (max across ward assessment days 1-5, fore-/afternoon)
sev_vars <- c("day1f_3d_cam_s","day2f_3d_cam_s","day3f_3d_cam_s","day4f_3d_cam_s","day5f_3d_cam_s",
              "day1a_3d_cam_s","day2a_3d_cam_s","day3a_3d_cam_s","day4a_3d_cam_s","day5a_3d_cam_s")
data$max_cam_s <- apply(data[sev_vars], 1, function(x) {
  v <- max(as.numeric(x), na.rm = TRUE); ifelse(is.infinite(v), NA, v)
})
data$post_max_3d_cam_s <- pmax(data$post_30_3d_cam_s, data$post_60_3d_cam_s, na.rm = TRUE)

# Pain: PACU (max of 30/60 min) and ward (max across all in-hospital timepoints)
data$pacu_schmerzwahrnehmung <- pmax(data$post_op_30_schmerzwahrnehmung, data$post_op_60_schmerzwahrnehmung, na.rm = TRUE)
data$ward_pain <- pmax(
  data$post_op_verlauf_t1f_schmerzwahrnehmung, data$post_op_verlauf_t1a_schmerzwahrnehmung,
  data$post_op_verlauf_t2f_schmerzwahrnehmung, data$post_op_verlauf_t2a_schmerzwahrnehmung,
  data$post_op_verlauf_t3f_schmerzwahrnehmung, data$post_op_verlauf_t3a_schmerzwahrnehmung,
  data$post_op_verlauf_t4f_schmerzwahrnehmung, data$post_op_verlauf_t4a_schmerzwahrnehmung,
  data$post_op_verlauf_t5f_schmerzwahrnehmung, data$post_op_verlauf_t5a_schmerzwahrnehmung,
  na.rm = TRUE
)
# Preoperative pain categories (used as subgroup moderators), e.g.:
data$pra_op_schmerz_cat <- cut(data$pra_op_schmerzwahrnehmung, c(-Inf, 0.5, Inf), c("no","yes"))

# RASS in PACU: 3-level derived from 30-/60-min scores (ref = "awake and calm")
data$rass_pacu <- dplyr::case_when(
  data$post_op_30_rass_score < 0 | data$post_op_60_rass_score < 0 ~ "agitated/restless",
  data$post_op_30_rass_score > 0 | data$post_op_60_rass_score > 0 ~ "sedated/sleepy",
  TRUE ~ "awake and calm"
)
data$rass_pacu <- relevel(factor(data$rass_pacu), ref = "awake and calm")

# ---- Group-wise multiple imputation (m = 30), separately per arm -----------
# -> avoids imputing across randomization arms (see Statistical Analysis section)
m <- 30
vars <- c("alter","gruppe","geschlecht","op_duration","anest_duration","inc_pod2",
          "delir_im_awr2","fachrichtung2","moca_cat","max_cam_s","post_max_3d_cam_s",
          "ward_pain","pacu_schmerzwahrnehmung","pra_op_schmerz_cat","rass_pacu")
vars <- vars[vars %in% names(data)]

imputed_groups <- lapply(unique(data$gruppe), function(g) {
  gd <- data[data$gruppe == g, vars[vars != "gruppe"]]
  if (sum(is.na(gd)) == 0) {
    gd$.imp <- 0; gd$.id <- seq_len(nrow(gd)); gd$gruppe <- g
    return(gd)
  }
  imp <- mice::mice(gd, m = m, seed = 246, printFlag = FALSE)
  out <- mice::complete(imp, action = "long", include = TRUE)
  out$gruppe <- g
  out
})
combined_long <- do.call(rbind, imputed_groups)
combined_long <- combined_long[order(combined_long$.imp), ]
combined_long$.id <- ave(seq_along(combined_long$.imp), combined_long$.imp, FUN = seq_along)
dat.back <- as.mids(combined_long)

saveRDS(dat.back, "dat_back_imputed.rds")
saveRDS(data, "data_prepared.rds")

# ---- Shared helper: pooled model -> odds-ratio table ------------------------
convert_to_odds_ratios <- function(pairs_summary) {
  data.frame(
    contrast = pairs_summary$contrast,
    OR       = round(exp(pairs_summary$estimate), 3),
    lower    = round(exp(pairs_summary$asymp.LCL %||% pairs_summary$lower.CL), 3),
    upper    = round(exp(pairs_summary$asymp.UCL %||% pairs_summary$upper.CL), 3),
    p.value  = round(pairs_summary$p.value, 4)
  )
}
`%||%` <- function(a, b) if (is.null(a)) b else a
save(convert_to_odds_ratios, file = "analysis_functions.RData")
