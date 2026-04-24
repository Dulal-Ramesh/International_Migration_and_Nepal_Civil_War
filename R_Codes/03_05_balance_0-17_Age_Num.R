# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Summary Statistics - Table 5: High vs Low Conflict (by Casualties)
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R datasets
# OUTPUTS : tables_summary/5a.Conflict_DiD_Table.{tex,html}
#           tables_summary/5b.Conflict_DiD_Table.{tex,html} ... (by age window)
#
# PURPOSE:
# Casualty-based counterpart to 03_04_did_high_low_conflict.R. Uses the exact
# same function (generate_conflict_did_table) and the same four cohort windows,
# but splits High vs Low using `high_conflict_casualty_binary` (casualties
# above Q3 of non-zero values) instead of months of war.
#


cat("\n=== BUILDING HIGH vs LOW CONFLICT DIFFERENCE TABLES (Casualties) ===\n")


# ==============================================================================
# TREATMENT AGE 0-17 (full treatment cohort)
# ==============================================================================

generate_conflict_did_table(
  data           = nlss_analysis_sample,
  treat_age_min  = 0,
  treat_age_max  = 17,
  conflict_var   = "high_conflict_casualty_binary",
  conflict_label = "Casualties (Q3 cutoff)",
  cluster_var    = "dist",
  file_label     = "5a",
  caption_label  = "Age 0--17 at Conflict Start",
  output_dir     = tables_summary
)

register_table(
  section    = "Difference Tables",
  subsection = "Based on Casualty Number",
  title      = "Table 5a — High vs Low Conflict (Age 0–17)",
  file       = "Tables_Summary/5a.Conflict_DiD_Table.html"
)


# ==============================================================================
# TREATMENT AGE 0-5 (early childhood)
# ==============================================================================

generate_conflict_did_table(
  data           = nlss_analysis_sample,
  treat_age_min  = 0,
  treat_age_max  = 5,
  conflict_var   = "high_conflict_casualty_binary",
  conflict_label = "Casualties (Q3 cutoff)",
  cluster_var    = "dist",
  file_label     = "5b",
  caption_label  = "Age 0--5 at Conflict Start",
  output_dir     = tables_summary
)

register_table(
  section    = "Difference Tables",
  subsection = "Based on Casualty Number",
  title      = "Table 5b — High vs Low Conflict (Age 0–5)",
  file       = "Tables_Summary/5b.Conflict_DiD_Table.html"
)


# ==============================================================================
# TREATMENT AGE 6-12 (primary school age)
# ==============================================================================

generate_conflict_did_table(
  data           = nlss_analysis_sample,
  treat_age_min  = 6,
  treat_age_max  = 12,
  conflict_var   = "high_conflict_casualty_binary",
  conflict_label = "Casualties (Q3 cutoff)",
  cluster_var    = "dist",
  file_label     = "5c",
  caption_label  = "Age 6--12 at Conflict Start",
  output_dir     = tables_summary
)

register_table(
  section    = "Difference Tables",
  subsection = "Based on Casualty Number",
  title      = "Table 5c — High vs Low Conflict (Age 6–12)",
  file       = "Tables_Summary/5c.Conflict_DiD_Table.html"
)


# ==============================================================================
# TREATMENT AGE 13-17 (adolescent exposure)
# ==============================================================================

generate_conflict_did_table(
  data           = nlss_analysis_sample,
  treat_age_min  = 13,
  treat_age_max  = 17,
  conflict_var   = "high_conflict_casualty_binary",
  conflict_label = "Casualties (Q3 cutoff)",
  cluster_var    = "dist",
  file_label     = "5d",
  caption_label  = "Age 13--17 at Conflict Start",
  output_dir     = tables_summary
)

register_table(
  section    = "Difference Tables",
  subsection = "Based on Casualty Number",
  title      = "Table 5d — High vs Low Conflict (Age 13–17)",
  file       = "Tables_Summary/5d.Conflict_DiD_Table.html"
)


cat("\n=== Casualty-based DiD tables complete ===\n")