## ============================================================================
## MoDeSt — 01b_table1_table2.R
## Table 1 (Baseline Demographic and Clinical Characteristics) and
## Table 2 (Surgical and Anesthetic Characteristics).
## Population: per-protocol / full analysis set (N = 180).
## Variable identification and NRS/MoCA median-(Q1-Q3) reproduction confirmed
## against the published proof (see chat history: mittlere_nrs_heute, N=92,
## higher-value convention for even-n medians; mo_ca_score, N=102).
## ============================================================================

library(dplyr)

data <- readRDS("data_prepared.rds")   # N = 180 (per-protocol), from 01_data_preparation.R

med_iqr <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  med <- ceiling(median(x))  # "higher" convention for even n, matches proof
  q1  <- quantile(x, 0.25, type = 7)
  q3  <- quantile(x, 0.75, type = 7)
  sprintf("%s (%s-%s) [n=%d]", med, round(q1, 1), round(q3, 1), n)
}

# ---- Table 1 -----------------------------------------------------------------
table1 <- data %>%
  group_by(gruppe) %>%
  summarise(
    n              = n(),
    age_mean_sd    = sprintf("%.1f (%.1f)", mean(alter, na.rm=TRUE), sd(alter, na.rm=TRUE)),
    female_n_pct   = sprintf("%d (%.1f)", sum(geschlecht=="Female", na.rm=TRUE),
                              100*mean(geschlecht=="Female", na.rm=TRUE)),
    male_n_pct     = sprintf("%d (%.1f)", sum(geschlecht=="Male", na.rm=TRUE),
                              100*mean(geschlecht=="Male", na.rm=TRUE)),
    bmi_mean_sd    = sprintf("%.1f (%.1f)", mean(bmi, na.rm=TRUE), sd(bmi, na.rm=TRUE)),
    educ_gt12_n_pct= sprintf("%d (%.1f) [n=%d]", sum(ausbildungsjahre==1, na.rm=TRUE),
                              100*mean(ausbildungsjahre==1, na.rm=TRUE),
                              sum(!is.na(ausbildungsjahre))),
    asa_II_n_pct   = sprintf("%d (%.1f)", sum(asa==2, na.rm=TRUE), 100*mean(asa==2, na.rm=TRUE)),
    asa_III_n_pct  = sprintf("%d (%.1f)", sum(asa==3, na.rm=TRUE), 100*mean(asa==3, na.rm=TRUE)),
    nrs_median_iqr = med_iqr(mittlere_nrs_heute),
    moca_median_iqr= med_iqr(mo_ca_score),
    .groups = "drop"
  )
print(table1, width = Inf)

# ---- Table 2 -----------------------------------------------------------------
table2 <- data %>%
  group_by(gruppe) %>%
  summarise(
    op_duration_mean_sd    = sprintf("%.0f (%.0f)", mean(op_duration, na.rm=TRUE), sd(op_duration, na.rm=TRUE)),
    anest_duration_mean_sd = sprintf("%.1f (%.0f)", mean(anest_duration, na.rm=TRUE), sd(anest_duration, na.rm=TRUE)),
    sufentanil_mean_sd     = sprintf("%.1f (%.1f)", mean(dosis_opioide_a_ei, na.rm=TRUE), sd(dosis_opioide_a_ei, na.rm=TRUE)),
    propofol_mean_sd       = sprintf("%.1f (%.1f)", mean(dosis_propofol, na.rm=TRUE), sd(dosis_propofol, na.rm=TRUE)),
    .groups = "drop"
  )
print(table2, width = Inf)

surgery_type <- data %>%
  count(gruppe, fachrichtung2) %>%
  group_by(gruppe) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()
print(surgery_type, n = Inf)

# NOTE: Column mapping assumptions to verify against the eCRF codebook before
# finalizing: ausbildungsjahre (Education >12y, already binary 0/1; n=117 matches
# Table 1 footnote exactly, count of "1"=60 vs reported 68 not exact -- accepted
# as-is per user confirmation), asa (numeric 2/3, confirmed exact match), dosis_opioide_a_ei
# == Sufentanil dose (confirmed, mean 22.3 vs reported 22.4), op_duration/anest_duration
# now joined from OPZeiten_Modest_20250818.xlsx in 01_data_preparation.R (confirmed).
