# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Balance Table - Summary Statistics by Treatment and Control
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R dataset

# ==============================================================================
# TREATMENT AGE 0-17 ----
# ==============================================================================

generate_balance_table(
  data          = nlss_conflict_data,
  treat_age_min = 0,
  treat_age_max = 17,
  file_label    = "11a",
  caption_label = "Age 0--17 at Conflict Start",
  output_dir    = tables_summary
)


# ==============================================================================
# TREATMENT AGE 0-5 ----
# ==============================================================================

generate_balance_table(
  data          = nlss_conflict_data,
  treat_age_min = 0,
  treat_age_max = 5,
  file_label    = "11b",
  caption_label = "Age 0--5 at Conflict Start",
  output_dir    = tables_summary
)