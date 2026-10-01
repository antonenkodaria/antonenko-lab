## ============================================================================
## MoDeSt — 08_missing_data.R
## Missing-data summary: N and % missing, overall and by randomization arm,
## for primary/secondary outcomes and Table 1/2 covariates.
## -> Manuscript: Statistical Analysis ("Missing data occurred predominantly
##    because study staff were unable to reach patients..."); eTable 1
##    (Supplement 2). Design agreed in reviewer-response discussion (missing-
##    at-random argument); no prior script existed for this table.
## Population: full analysis set (N = 180; excludes the 45 post-consent
## dropouts already accounted for in Figure 2, not "missing data" per se).
## ============================================================================

library(dplyr); library(tidyr)

data <- readRDS("data_prepared.rds")  # N = 180, from 01_data_preparation.R

vars <- c(
  # primary / secondary outcomes
  "inc_pod2", "delir_im_awr2", "max_cam_s", "post_max_3d_cam_s",
  "ward_pain", "pacu_pain_cat",
  # Table 1 covariates
  "alter", "geschlecht", "bmi", "ausbildung_cat", "asa", "mittlere_nrs_heute", "mo_ca_score",
  # Table 2 covariates
  "op_duration", "anest_duration", "dosis_opioide_a_ei", "dosis_propofol", "fachrichtung2"
)
vars <- vars[vars %in% names(data)]

missing_overall <- data %>%
  summarise(across(all_of(vars), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = round(100 * n_missing / nrow(data), 1))

missing_by_arm <- data %>%
  group_by(gruppe) %>%
  summarise(across(all_of(vars), ~ sum(is.na(.))), n = n(), .groups = "drop") %>%
  pivot_longer(-c(gruppe, n), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = round(100 * n_missing / n, 1)) %>%
  select(variable, gruppe, n_missing, pct_missing) %>%
  pivot_wider(names_from = gruppe, values_from = c(n_missing, pct_missing))

etable1 <- missing_overall %>%
  left_join(missing_by_arm, by = "variable") %>%
  rename(n_missing_overall = n_missing, pct_missing_overall = pct_missing)

print(etable1, n = Inf)
# write.csv(etable1, "eTable1_missing_data.csv", row.names = FALSE)
