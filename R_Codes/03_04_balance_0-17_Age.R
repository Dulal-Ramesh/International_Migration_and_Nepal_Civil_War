# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Difference Table - Table 4: High vs Low Conflict Comparison
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R datasets
# OUTPUTS : tables_summary/4a.Conflict_DiD_Table.{tex,html}
#           tables_summary/4b.Conflict_DiD_Table.{tex,html} ... (by age window)
#
# PURPOSE:
# Descriptive comparison of outcomes, conflict exposure, demographics, education,
# ethnicity, and occupation across High-Conflict vs Low-Conflict districts,
# within cohort sub-windows of the treatment group. Standard errors in the
# Diff (H-L) column are clustered at the district level, via a linear
# regression of each variable on the High-Conflict indicator.
#
# SAMPLE: uses `nlss_analysis_sample` (not `nlss_conflict_data`). .
#
# The High/Low split is based on `high_conflict_q3_binary` (months of war above
# Q3 of non-zero values) as defined in  02_data_cleaning.R.


cat("\n=== BUILDING HIGH vs LOW CONFLICT DIFFERENCE TABLES ===\n")


# ==============================================================================
# TREATMENT AGE 0-17 (full treatment cohort)
# ==============================================================================

generate_conflict_did_table(
  data           = nlss_analysis_sample,
  treat_age_min  = 0,
  treat_age_max  = 17,
  conflict_var   = "high_conflict_q3_binary",
  conflict_label = "Months of War (Q3 cutoff)",
  cluster_var    = "dist",
  file_label     = "4a",
  caption_label  = "Age 0--17 at Conflict Start",
  output_dir     = tables_summary
)

register_table(
  section = "Difference Tables",
  title   = "Table 4a — High vs Low Conflict (Age 0–17)",
  file    = "Tables_Summary/4a.Conflict_DiD_Table.html"
)


# ==============================================================================
# TREATMENT AGE 0-5 (early childhood)
# ==============================================================================

generate_conflict_did_table(
  data           = nlss_analysis_sample,
  treat_age_min  = 0,
  treat_age_max  = 5,
  conflict_var   = "high_conflict_q3_binary",
  conflict_label = "Months of War (Q3 cutoff)",
  cluster_var    = "dist",
  file_label     = "4b",
  caption_label  = "Age 0--5 at Conflict Start",
  output_dir     = tables_summary
)

register_table(
  section = "Difference Tables",
  title   = "Table 4b — High vs Low Conflict (Age 0–5)",
  file    = "Tables_Summary/4b.Conflict_DiD_Table.html"
)


# ==============================================================================
# TREATMENT AGE 6-12 (primary school age)
# ==============================================================================

generate_conflict_did_table(
  data           = nlss_analysis_sample,
  treat_age_min  = 6,
  treat_age_max  = 12,
  conflict_var   = "high_conflict_q3_binary",
  conflict_label = "Months of War (Q3 cutoff)",
  cluster_var    = "dist",
  file_label     = "4c",
  caption_label  = "Age 6--12 at Conflict Start",
  output_dir     = tables_summary
)

register_table(
  section = "Difference Tables",
  title   = "Table 4c — High vs Low Conflict (Age 6–12)",
  file    = "Tables_Summary/4c.Conflict_DiD_Table.html"
)


# ==============================================================================
# TREATMENT AGE 13-17 (adolescent exposure)
# ==============================================================================

generate_conflict_did_table(
  data           = nlss_analysis_sample,
  treat_age_min  = 13,
  treat_age_max  = 17,
  conflict_var   = "high_conflict_q3_binary",
  conflict_label = "Months of War (Q3 cutoff)",
  cluster_var    = "dist",
  file_label     = "4d",
  caption_label  = "Age 13--17 at Conflict Start",
  output_dir     = tables_summary
)

register_table(
  section = "Difference Tables",
  title   = "Table 4d — High vs Low Conflict (Age 13–17)",
  file    = "Tables_Summary/4d.Conflict_DiD_Table.html"
)


cat("\n=== Conflict Difference tables complete ===\n")