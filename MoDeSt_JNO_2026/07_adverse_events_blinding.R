## ============================================================================
## MoDeSt — 07_adverse_events_blinding.R
## Adverse events (rate ratio) and blinding assessment.
## -> Manuscript: Results, "Adverse Events" paragraph; eTable 5/6 (Supplement 2)
## Per-protocol population (drop_out_per_protocol != 1)
## ============================================================================

library(readxl); library(dplyr)

data <- read_excel("eCRF_MoDeSt_Daria.xlsx")
data_pp <- data %>% filter(drop_out_per_protocol != 1 | is.na(drop_out_per_protocol))

nw_vars <- c("nw_jucken", "nw_schmerz", "nw_brennen", "nw_warme_hitze",
             "nw_metallischer_eisengeschmack", "nw_ermudung_verringerte_aufmerksamkeit",
             "nw_andere")

# ---- Adverse events: incidence rate ratio (pooled active vs sham) ----------
# -> "Nonsevere adverse events... reported by 10 patients in the tACS group,
#     17 patients in the tDCS group, and 8 patients in the sham group."
# -> "the incidence of adverse events did not differ substantially between
#     groups (incidence rate ratio, 2.3 [95% CI, 0.8-6.5])"
n_tacs <- sum(data_pp$gruppe == "tACS"); n_tdcs <- sum(data_pp$gruppe == "tDCS"); n_sham <- sum(data_pp$gruppe == "sham")

events_tacs <- sum(rowSums(data_pp[data_pp$gruppe == "tACS", nw_vars] > 0, na.rm = TRUE), na.rm = TRUE)
events_tdcs <- sum(rowSums(data_pp[data_pp$gruppe == "tDCS", nw_vars] > 0, na.rm = TRUE), na.rm = TRUE)
events_sham <- sum(rowSums(data_pp[data_pp$gruppe == "sham", nw_vars] > 0, na.rm = TRUE), na.rm = TRUE)

rate_active <- (events_tacs + events_tdcs) / (n_tacs + n_tdcs)
rate_sham   <- events_sham / n_sham
irr <- rate_active / rate_sham
se_log_irr <- sqrt(1 / events_tacs + 1 / events_tdcs + 1 / events_sham)
c(IRR = round(irr, 1),
  lower = round(exp(log(irr) - 1.96 * se_log_irr), 1),
  upper = round(exp(log(irr) + 1.96 * se_log_irr), 1))

# ---- Blinding: James blinding index (per group) -----------------------------
# CONFIRMED formula (from prior chat, ~1 year ago; verified against dat_trial1.csv
# to exactly reproduce the published 0.338 / 0.744):
#   BI_g = 0.5 + 0.5 * (P_dontknow - P_wrong)     [1 = perfect blinding, 0 = fully unblinded]
#   correct/wrong guess is group-specific: active groups "correct" = guess 1
#   ("real"), sham "correct" = guess 2 ("sham"); "don't know" = code 3.
#   SE via delta method: se = 0.5 * sqrt(var(p_dk) + var(p_wrong) - 2*cov(p_dk,p_wrong)),
#   cov(p_dk, p_wrong) = -p_dk * p_wrong / n
# -> "The James blinding index...was 0.338 (95% CI, 0.237-0.439) for the
#     active intervention groups and 0.744 (95% CI, 0.659-0.829) for the sham
#     control group"
james_index <- function(x, wrong_code) {
  x <- x[!is.na(x)]; n <- length(x)
  p_wrong <- mean(x == wrong_code)
  p_dk    <- mean(x == 3)
  bi <- 0.5 + 0.5 * (p_dk - p_wrong)
  var_diff <- p_dk*(1-p_dk)/n + p_wrong*(1-p_wrong)/n - 2*(-p_dk*p_wrong/n)
  se <- 0.5 * sqrt(var_diff)
  c(BI = round(bi, 3), lower = round(bi - 1.96*se, 3), upper = round(bi + 1.96*se, 3), n = n)
}
active <- data_pp$einschatzung_stimulation_patient[data_pp$gruppe %in% c("tACS", "tDCS")]
sham   <- data_pp$einschatzung_stimulation_patient[data_pp$gruppe == "sham"]
james_index(active, wrong_code = 2)  # -> 0.338 (0.237-0.439)
james_index(sham,   wrong_code = 1)  # -> 0.744 (0.659-0.829)
