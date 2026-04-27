# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Summary Statistics - Table 6: DiD Table by Cohort and Conflict
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R datasets
# OUTPUTS : tables_summary/6.DiD_Two_Measures.{tex,html}
#
# PURPOSE:
# A unified DiD table with four panels (one per migration outcome) and two
# side-by-side conflict-measure column groups (Months of War, Casualties).
# Each panel shows raw means by cohort × district intensity, plus the DiD
# interaction coefficient in the bottom-right cell.


cat("\n=== BUILDING DiD TWO-MEASURES TABLE ===\n")


generate_did_two_measures_table(
  data            = nlss_analysis_sample,
  outcome_vars    = c("international_migrant",
                      "international_absentee_only",
                      "present_ind_migrant",
                      "national"),
  outcome_labels  = c("International Migrant (=1)",
                      "Currently Abroad (=1)",
                      "Return Migrant (=1)",
                      "Internal Migration (=1)"),
  conflict_vars   = c("high_conflict_q3_binary",
                      "high_conflict_casualty_binary"),
  conflict_labels = c("Months of War", "Casualties"),
  treat_var       = "treatment",
  cluster_var     = "dist",
  file_label      = "6",
  caption_label   = "Treatment 0-17 vs Control 18-40 at Conflict Onset",
  output_dir      = tables_summary
)

register_table(
  section    = "Difference Tables",
  subsection = "DiD Means (Two Measures)",
  title      = "Table 6 — DiD Means by Cohort and Conflict Intensity",
  file       = "Tables_Summary/6.DiD_Two_Measures.html"
)


cat("\n=== DiD two-measures table complete ===\n")