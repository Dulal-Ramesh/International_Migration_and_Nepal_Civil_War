# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Regression Tables - Tables 7a-7d: Main DiD by Outcome
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R datasets
# OUTPUTS : tables_main/7a.DiD_Regression.{tex,html}  (International Migrant)
#           tables_main/7b.DiD_Regression.{tex,html}  (Currently Abroad)
#           tables_main/7c.DiD_Regression.{tex,html}  (Return Migrant)
#           tables_main/7d.DiD_Regression.{tex,html}  (Internal Migration)


cat("\n========================================================\n")
cat(" Tables 7a-7d: Main DiD Regressions (4 migration outcomes)\n")
cat("========================================================\n")


# ==============================================================================
# Table 7a: International Migrant
# ==============================================================================

generate_did_regression_table(
  data           = nlss_analysis_sample,
  outcome_var    = "international_migrant",
  outcome_label  = "International Migrant (=1)",
  conflict_vars  = c("high_conflict_q3_binary", "high_conflict_casualty_binary"),
  conflict_panel_labels = c(
    "Panel A: High-Conflict Districts defined by Months of War",
    "Panel B: High-Conflict Districts defined by Casualties"),
  treat_var     = "treatment",
  cluster_var   = "dist",
  scale         = 100,
  file_label    = "7a",
  caption_label = "Treatment 0-17 vs Control 18-40 at Conflict Onset",
  output_dir    = tables_main
)

register_table(
  section    = "Regression Tables",
  subsection = "Main DiD Specification",
  title      = "Table 7a — Main DiD: International Migrant",
  file       = "Tables_Main/7a.DiD_Regression.html"
)


# ==============================================================================
# Table 7b: Currently Abroad
# ==============================================================================

generate_did_regression_table(
  data           = nlss_analysis_sample,
  outcome_var    = "international_absentee_only",
  outcome_label  = "Currently Abroad (=1)",
  conflict_vars  = c("high_conflict_q3_binary", "high_conflict_casualty_binary"),
  conflict_panel_labels = c(
    "Panel A: High-Conflict Districts defined by Months of War",
    "Panel B: High-Conflict Districts defined by Casualties"),
  treat_var     = "treatment",
  cluster_var   = "dist",
  scale         = 100,
  file_label    = "7b",
  caption_label = "Treatment 0-17 vs Control 18-40 at Conflict Onset",
  output_dir    = tables_main
)

register_table(
  section    = "Regression Tables",
  subsection = "Main DiD Specification",
  title      = "Table 7b — Main DiD: Currently Abroad",
  file       = "Tables_Main/7b.DiD_Regression.html"
)


# ==============================================================================
# Table 7c: Return Migrant
# ==============================================================================

generate_did_regression_table(
  data           = nlss_analysis_sample,
  outcome_var    = "present_ind_migrant",
  outcome_label  = "Return Migrant (=1)",
  conflict_vars  = c("high_conflict_q3_binary", "high_conflict_casualty_binary"),
  conflict_panel_labels = c(
    "Panel A: High-Conflict Districts defined by Months of War",
    "Panel B: High-Conflict Districts defined by Casualties"),
  treat_var     = "treatment",
  cluster_var   = "dist",
  scale         = 100,
  file_label    = "7c",
  caption_label = "Treatment 0-17 vs Control 18-40 at Conflict Onset",
  output_dir    = tables_main
)

register_table(
  section    = "Regression Tables",
  subsection = "Main DiD Specification",
  title      = "Table 7c — Main DiD: Return Migrant",
  file       = "Tables_Main/7c.DiD_Regression.html"
)


# ==============================================================================
# Table 7d: Internal Migration
# ==============================================================================

generate_did_regression_table(
  data           = nlss_analysis_sample,
  outcome_var    = "national",
  outcome_label  = "Internal Migration (=1)",
  conflict_vars  = c("high_conflict_q3_binary", "high_conflict_casualty_binary"),
  conflict_panel_labels = c(
    "Panel A: High-Conflict Districts defined by Months of War",
    "Panel B: High-Conflict Districts defined by Casualties"),
  treat_var     = "treatment",
  cluster_var   = "dist",
  scale         = 100,
  file_label    = "7d",
  caption_label = "Treatment 0-17 vs Control 18-40 at Conflict Onset",
  output_dir    = tables_main
)

register_table(
  section    = "Regression Tables",
  subsection = "Main DiD Specification",
  title      = "Table 7d — Main DiD: Internal Migration",
  file       = "Tables_Main/7d.DiD_Regression.html"
)