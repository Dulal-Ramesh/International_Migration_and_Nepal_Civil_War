# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Event Study - International Migration x Months of War
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R datasets
# OUTPUTS : figures_main/8a_intl_migrant_months_event_study.png
#           tables_main/8a.Event_Study.{tex,html}
#           figures_main/8a_intl_migrant_months_event_study_coefs.csv
#
# PURPOSE:
# Estimates the effect of
# being in a high-conflict district on international migration, separately
# for every age cohort in nlss_analysis_sample: treatment ages 0-17 and
# control/placebo ages 26-40 in 1996.
# The coefficients trace out the dose-response curve across childhood and
# placebo ages.
#
# Reference age: 26 (first control age). All other coefficients are
# interpreted as "effect relative to age 26".


cat("\n=== EVENT STUDY: International Migration x Months of War ===\n")

es_intl_months <- generate_event_study_plot(
  data             = nlss_analysis_sample,
  outcome_var      = "international_migrant",
  outcome_label    = "International Migrant (=1)",
  conflict_var     = "high_conflict_q3_binary",
  conflict_label   = "Months of War (Q3 cutoff)",
  age_var          = "age_at_conflict_start",
  treatment_cutoff = 17,
  ref_age          = 26,
  cluster_var      = "dist",
  birthyear_var    = "age",
  scale            = 100,
  file_label       = "8a_intl_migrant_months",
  caption_label    = "International Migrant, Months of War",
  figure_dir       = figures,
  table_dir        = tables_main
)

register_table(
  section    = "Regression Tables",
  subsection = "Event Study",
  title      = "Table 8a — Event Study: International Migrant (Months of War)",
  file       = "Tables_Main/8a_intl_migrant_months.Event_Study.html"
)
