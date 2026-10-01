# MoDeSt — Reproducibility Package

This repository accompanies:

> Antonenko D, Bublitz V, Leroy S, et al. Transcranial Electrical Stimulation and
> Delirium Among Postoperative Patients: A Randomized Clinical Trial.
> *JAMA Network Open*. 2026;9(8):e2628284. doi:10.1001/jamanetworkopen.2026.28284

Trial registration: German Clinical Trials Register, DRKS00033703.

It provides the analysis scripts used to produce every reported number, table, and
figure in the manuscript and its Supplement. **Raw participant-level data are not
shared in this repository** (see *Data availability* below); scripts are provided
so that the reported computations can be independently verified against the
underlying dataset by the authors or upon reasonable, ethics-compliant request.

## Repository structure

```
modest_reproducibility/
├── README.md                     <- this file
├── data/
│   └── (not included — see Data availability)
└── scripts/
    ├── 01_data_preparation.R      <- variable derivation & multiple imputation (m=30)
    ├── 01b_table1_table2.R        <- Table 1 (baseline) & Table 2 (surgical/anesthetic)
    ├── 02_primary_outcome.R      <- Primary outcome: POD in ward, tACS vs sham
    ├── 03_secondary_outcomes.R   <- tDCS vs sham (ward); tACS/tDCS vs sham (PACU)
    ├── 04_pod_severity_motor.R   <- POD severity (3D-CAM-S) & motor type (RASS)
    ├── 05_pain_analyses.R        <- Postoperative pain (NRS), eFigure 2 / eTable 2
    ├── 06_subgroup_analyses.R    <- Prespecified subgroup analyses incl. duration-of-
    │                                surgery × tACS interaction (Figure 3C)
    ├── 07_adverse_events_blinding.R <- Adverse events (eTable 5) & James blinding index
    ├── 08_missing_data.R         <- Missing-data summary (eTable 1)
    └── 09_historical_cohort_sensitivity.R <- Modified full analysis set
```

Each script is self-contained, reads the (locally held) raw dataset, and prints/
exports the exact values reported in the manuscript, with an inline comment
pointing to the manuscript location (e.g., `# -> Abstract, Results` or
`# -> Table 1, row "NRS score"`) for every reported statistic.

## Mapping: manuscript element → script

| Manuscript location | Script |
|---|---|
| Abstract — Results (sample size, sex, group sizes) | `01b_table1_table2.R` |
| Table 1. Baseline Demographic and Clinical Characteristics | `01b_table1_table2.R` |
| Table 2. Surgical and Anesthetic Characteristics | `01b_table1_table2.R` |
| Results — Primary Outcome Measure | `02_primary_outcome.R` |
| Results — Secondary Outcome Measures (ward tDCS; PACU) | `03_secondary_outcomes.R` |
| Results — POD severity / motor type paragraph | `04_pod_severity_motor.R` |
| eFigure 1, eTable 1 (Supplement 2) | `04_pod_severity_motor.R` |
| eFigure 2, eTable 2 (Supplement 2) — pain levels | `05_pain_analyses.R` |
| Results — Prespecified Subgroup Analyses; Figure 3C | `06_subgroup_analyses.R` |
| eTable 4, eTable 5 (Supplement 2) — subgroup interactions | `06_subgroup_analyses.R` |
| Results — Adverse Events; eTable 5 (rate ratio) | `07_adverse_events_blinding.R` |
| Results — James blinding index; eTable 6/7 | `07_adverse_events_blinding.R` |
| eTable 1 (Supplement 2) — missingness by arm | `08_missing_data.R` |
| Figure 2, footnote a; Supplement 1 (modified full analysis set) | `09_historical_cohort_sensitivity.R` |

## Data availability

Individual participant data underlying this study are not publicly deposited in
this repository due to data protection requirements of the ethics approval
(local ethics committee, University Medicine Greifswald). De-identified data may
be made available by the corresponding author upon reasonable request and subject
to a data use agreement, consistent with the Data Sharing Statement (Supplement 3)
of the published article.

The dataset used by these scripts includes both the analyzed cohort (N = 225
randomized; N = 180 per-protocol/primary analysis set) and the historical cohort
excluded due to stimulation-device malfunction (n = 172; flagged via
`stimulator_intact == 0`), consistent with Figure 2 (CONSORT flow diagram) and
footnote *a* therein.

## Software

Analyses were conducted in R software using packages including
`dplyr`, `mice` (multiple imputation), `broom`/`marginaleffects` (model-derived
estimates), and `ggplot2` (figures). 

## Statistical approach (brief)

- Missing outcome data were handled via group-specific multiple imputation
  (see manuscript Statistical Analysis section and Supplement 1/2 for details).
- Primary and secondary group comparisons: multiple binary logistic regression,
  adjusted for age and sex, on multiply imputed data.
- Subgroup analyses: interaction terms (subgroup variable × treatment group)
  within the same adjusted logistic regression models; for interactions reaching
  the prespecified significance threshold, absolute risk differences were derived
  as marginal estimates from the same model.
- All statistical tests were 2-sided; P < .05 was considered statistically
  significant. No adjustment for multiple testing was applied.

## Contact

Daria Antonenko, PhD — Department of Neurology, University Medicine Greifswald
daria.antonenko@med.uni-greifswald.de
