# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Event Studies - Figures 8a-8d for all migration outcomes
#                 across two conflict-intensity measures (8 figures total)
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R datasets
# OUTPUTS : figures/<file_label>_event_study.png       (8 PNG figures)
#           tables_main/<file_label>.Event_Study.{tex,html}  (8 coef tables)
#           figures/<file_label>_event_study_coefs.csv (8 raw coefficient files)
#
# PURPOSE:
# Estimates Duflo (2001) Equation 2 separately for each combination of
# migration outcome (international migrant, currently abroad, return migrant,
# internal migration) and conflict intensity measure (months of war,
# casualties). Each event study estimates the effect of being in a high-
# conflict district on the outcome, separately for every age cohort
# (treatment ages 0-17, control/placebo ages 26-40 in 1996). Coefficients
# trace out the dose-response curve across childhood and placebo ages.
#
# Reference age: 26 (the youngest age in the control cohort). All other
# coefficients are interpreted as "effect relative to age 26".
#
# NAMING SCHEME:
#   8a — International Migrant     (corresponds to Table 7a)
#   8b — Currently Abroad           (corresponds to Table 7b)
#   8c — Return Migrant             (corresponds to Table 7c)
#   8d — Internal Migration         (corresponds to Table 7d)
# Suffix: _months   = months-of-war binary cutoff
#         _casualties = casualty-based binary cutoff


cat("\n========================================================\n")
cat(" Figures 8a-8d: Event Studies (4 outcomes x 2 measures)\n")
cat("========================================================\n")


# ==============================================================================
# Figure 8a: International Migrant
# ==============================================================================

# 8a (Months of War)
es_8a_months <- generate_event_study_plot(
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
  section    = "Event Study",
  subsection = "",
  title      = "Table 8a — Event Study: International Migrant (Months of War)",
  file       = "Tables_Main/8a_intl_migrant_months.Event_Study.html"
)

# 8a (Casualties)
es_8a_casualties <- generate_event_study_plot(
  data             = nlss_analysis_sample,
  outcome_var      = "international_migrant",
  outcome_label    = "International Migrant (=1)",
  conflict_var     = "high_conflict_casualty_binary",
  conflict_label   = "Casualties (Q3 cutoff)",
  age_var          = "age_at_conflict_start",
  treatment_cutoff = 17,
  ref_age          = 26,
  cluster_var      = "dist",
  birthyear_var    = "age",
  scale            = 100,
  file_label       = "8a_intl_migrant_casualties",
  caption_label    = "International Migrant, Casualties",
  figure_dir       = figures,
  table_dir        = tables_main
)

register_table(
  section    = "Event Study",
  subsection = "",
  title      = "Table 8a — Event Study: International Migrant (Casualties)",
  file       = "Tables_Main/8a_intl_migrant_casualties.Event_Study.html"
)


# ==============================================================================
# Figure 8b: Currently Abroad
# ==============================================================================

# 8b (Months of War)
es_8b_months <- generate_event_study_plot(
  data             = nlss_analysis_sample,
  outcome_var      = "international_absentee_only",
  outcome_label    = "Currently Abroad (=1)",
  conflict_var     = "high_conflict_q3_binary",
  conflict_label   = "Months of War (Q3 cutoff)",
  age_var          = "age_at_conflict_start",
  treatment_cutoff = 17,
  ref_age          = 26,
  cluster_var      = "dist",
  birthyear_var    = "age",
  scale            = 100,
  file_label       = "8b_currently_abroad_months",
  caption_label    = "Currently Abroad, Months of War",
  figure_dir       = figures,
  table_dir        = tables_main
)

register_table(
  section    = "Event Study",
  subsection = "",
  title      = "Table 8b — Event Study: Currently Abroad (Months of War)",
  file       = "Tables_Main/8b_currently_abroad_months.Event_Study.html"
)

# 8b (Casualties)
es_8b_casualties <- generate_event_study_plot(
  data             = nlss_analysis_sample,
  outcome_var      = "international_absentee_only",
  outcome_label    = "Currently Abroad (=1)",
  conflict_var     = "high_conflict_casualty_binary",
  conflict_label   = "Casualties (Q3 cutoff)",
  age_var          = "age_at_conflict_start",
  treatment_cutoff = 17,
  ref_age          = 26,
  cluster_var      = "dist",
  birthyear_var    = "age",
  scale            = 100,
  file_label       = "8b_currently_abroad_casualties",
  caption_label    = "Currently Abroad, Casualties",
  figure_dir       = figures,
  table_dir        = tables_main
)

register_table(
  section    = "Event Study",
  subsection = "",
  title      = "Table 8b — Event Study: Currently Abroad (Casualties)",
  file       = "Tables_Main/8b_currently_abroad_casualties.Event_Study.html"
)


# ==============================================================================
# Figure 8c: Return Migrant
# ==============================================================================

# 8c (Months of War)
es_8c_months <- generate_event_study_plot(
  data             = nlss_analysis_sample,
  outcome_var      = "present_ind_migrant",
  outcome_label    = "Return Migrant (=1)",
  conflict_var     = "high_conflict_q3_binary",
  conflict_label   = "Months of War (Q3 cutoff)",
  age_var          = "age_at_conflict_start",
  treatment_cutoff = 17,
  ref_age          = 26,
  cluster_var      = "dist",
  birthyear_var    = "age",
  scale            = 100,
  file_label       = "8c_return_migrant_months",
  caption_label    = "Return Migrant, Months of War",
  figure_dir       = figures,
  table_dir        = tables_main
)

register_table(
  section    = "Event Study",
  subsection = "",
  title      = "Table 8c — Event Study: Return Migrant (Months of War)",
  file       = "Tables_Main/8c_return_migrant_months.Event_Study.html"
)

# 8c (Casualties)
es_8c_casualties <- generate_event_study_plot(
  data             = nlss_analysis_sample,
  outcome_var      = "present_ind_migrant",
  outcome_label    = "Return Migrant (=1)",
  conflict_var     = "high_conflict_casualty_binary",
  conflict_label   = "Casualties (Q3 cutoff)",
  age_var          = "age_at_conflict_start",
  treatment_cutoff = 17,
  ref_age          = 26,
  cluster_var      = "dist",
  birthyear_var    = "age",
  scale            = 100,
  file_label       = "8c_return_migrant_casualties",
  caption_label    = "Return Migrant, Casualties",
  figure_dir       = figures,
  table_dir        = tables_main
)

register_table(
  section    = "Event Study",
  subsection = "",
  title      = "Table 8c — Event Study: Return Migrant (Casualties)",
  file       = "Tables_Main/8c_return_migrant_casualties.Event_Study.html"
)


# ==============================================================================
# Figure 8d: Internal Migration
# ==============================================================================

# 8d (Months of War)
es_8d_months <- generate_event_study_plot(
  data             = nlss_analysis_sample,
  outcome_var      = "national",
  outcome_label    = "Internal Migration (=1)",
  conflict_var     = "high_conflict_q3_binary",
  conflict_label   = "Months of War (Q3 cutoff)",
  age_var          = "age_at_conflict_start",
  treatment_cutoff = 17,
  ref_age          = 26,
  cluster_var      = "dist",
  birthyear_var    = "age",
  scale            = 100,
  file_label       = "8d_internal_migration_months",
  caption_label    = "Internal Migration, Months of War",
  figure_dir       = figures,
  table_dir        = tables_main
)

register_table(
  section    = "Event Study",
  subsection = "",
  title      = "Table 8d — Event Study: Internal Migration (Months of War)",
  file       = "Tables_Main/8d_internal_migration_months.Event_Study.html"
)

# 8d (Casualties)
es_8d_casualties <- generate_event_study_plot(
  data             = nlss_analysis_sample,
  outcome_var      = "national",
  outcome_label    = "Internal Migration (=1)",
  conflict_var     = "high_conflict_casualty_binary",
  conflict_label   = "Casualties (Q3 cutoff)",
  age_var          = "age_at_conflict_start",
  treatment_cutoff = 17,
  ref_age          = 26,
  cluster_var      = "dist",
  birthyear_var    = "age",
  scale            = 100,
  file_label       = "8d_internal_migration_casualties",
  caption_label    = "Internal Migration, Casualties",
  figure_dir       = figures,
  table_dir        = tables_main
)

register_table(
  section    = "Event Study",
  subsection = "",
  title      = "Table 8d — Event Study: Internal Migration (Casualties)",
  file       = "Tables_Main/8d_internal_migration_casualties.Event_Study.html"
)


cat("\n========================================================\n")
cat(" All 8 event study figures and coefficient tables complete\n")
cat("========================================================\n")