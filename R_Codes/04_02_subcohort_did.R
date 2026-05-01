# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Sub-Cohort DiD - Tables 9a-9d (4 outcomes by 3 sub-cohorts)
# Last Updated  : May 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions (incl. Section 14),
#           02_data_cleaning.R datasets
# OUTPUTS : tables_main/9a-9d.SubCohort_DiD.{tex,html}
#
# PURPOSE:
# Estimates the main DiD specification (Equation 1) separately for three
# childhood sub-cohorts within the treatment group, using the existing
# cohort_short variable in the data:
#
#   - "T: 0-5"   — Early childhood    (currently 21-26 in 2017)
#   - "T: 6-12"  — Middle childhood   (currently 27-33 in 2017)
#   - "T: 13-17" — Adolescence        (currently 34-38 in 2017)
#
# The bracket structure follows Nepal's school-grade structure:
#   pre-school (0-5), primary school grades 1-6 (6-12),
#   secondary school grades 7-10+ (13-17).
#
# All three sub-cohorts are compared against the SAME pooled control group:
# individuals aged 26-40 at conflict onset ("C: 26-35" + "C: 36-40";
# currently 47-61 in 2017). This isolates whether the conflict effect
# is concentrated in any specific developmental window.
#
# Reference: Akresh et al. (2012, AER P&P) on Rwanda use a similar
# age-bracket approach for childhood conflict exposure analyses.


cat("\n========================================================\n")
cat(" Tables 9a-9d: Sub-Cohort DiD (4 outcomes x 3 sub-cohorts)\n")
cat("========================================================\n")


# ==============================================================================
# Table 9a: International Migrant
# ==============================================================================

generate_subcohort_did_table(
  data           = nlss_analysis_sample,
  outcome_var    = "international_migrant",
  outcome_label  = "International Migrant (=1)",
  conflict_vars  = c("high_conflict_q3_binary", "high_conflict_casualty_binary"),
  conflict_panel_labels = c(
    "Panel A: High-Conflict Districts defined by Months of War",
    "Panel B: High-Conflict Districts defined by Casualties"),
  scale         = 100,
  file_label    = "9a",
  caption_label = "Sub-Cohort Decomposition by Developmental Window",
  output_dir    = tables_main
)

register_table(
  section    = "Regression Tables",
  subsection = "Sub-Cohort DiD",
  title      = "Table 9a — Sub-Cohort DiD: International Migrant",
  file       = "Tables_Main/9a.SubCohort_DiD.html"
)


# ==============================================================================
# Table 9b: Currently Abroad
# ==============================================================================

generate_subcohort_did_table(
  data           = nlss_analysis_sample,
  outcome_var    = "international_absentee_only",
  outcome_label  = "Currently Abroad (=1)",
  conflict_vars  = c("high_conflict_q3_binary", "high_conflict_casualty_binary"),
  conflict_panel_labels = c(
    "Panel A: High-Conflict Districts defined by Months of War",
    "Panel B: High-Conflict Districts defined by Casualties"),
  scale         = 100,
  file_label    = "9b",
  caption_label = "Sub-Cohort Decomposition by Developmental Window",
  output_dir    = tables_main
)

register_table(
  section    = "Regression Tables",
  subsection = "Sub-Cohort DiD",
  title      = "Table 9b — Sub-Cohort DiD: Currently Abroad",
  file       = "Tables_Main/9b.SubCohort_DiD.html"
)


# ==============================================================================
# Table 9c: Return Migrant
# ==============================================================================

generate_subcohort_did_table(
  data           = nlss_analysis_sample,
  outcome_var    = "present_ind_migrant",
  outcome_label  = "Return Migrant (=1)",
  conflict_vars  = c("high_conflict_q3_binary", "high_conflict_casualty_binary"),
  conflict_panel_labels = c(
    "Panel A: High-Conflict Districts defined by Months of War",
    "Panel B: High-Conflict Districts defined by Casualties"),
  scale         = 100,
  file_label    = "9c",
  caption_label = "Sub-Cohort Decomposition by Developmental Window",
  output_dir    = tables_main
)

register_table(
  section    = "Regression Tables",
  subsection = "Sub-Cohort DiD",
  title      = "Table 9c — Sub-Cohort DiD: Return Migrant",
  file       = "Tables_Main/9c.SubCohort_DiD.html"
)


# ==============================================================================
# Table 9d: Internal Migration
# ==============================================================================

generate_subcohort_did_table(
  data           = nlss_analysis_sample,
  outcome_var    = "national",
  outcome_label  = "Internal Migration (=1)",
  conflict_vars  = c("high_conflict_q3_binary", "high_conflict_casualty_binary"),
  conflict_panel_labels = c(
    "Panel A: High-Conflict Districts defined by Months of War",
    "Panel B: High-Conflict Districts defined by Casualties"),
  scale         = 100,
  file_label    = "9d",
  caption_label = "Sub-Cohort Decomposition by Developmental Window",
  output_dir    = tables_main
)

register_table(
  section    = "Regression Tables",
  subsection = "Sub-Cohort DiD",
  title      = "Table 9d — Sub-Cohort DiD: Internal Migration",
  file       = "Tables_Main/9d.SubCohort_DiD.html"
)


cat("\n========================================================\n")
cat(" All 4 sub-cohort tables complete\n")
cat("========================================================\n")