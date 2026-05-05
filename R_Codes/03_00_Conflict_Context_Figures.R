# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Conflict context figures
#                 - Figure C1: Conflict timeline (key events 1996-2006)
#                 - Figure C2: Monthly fatalities by perpetrator
#                 - Figure C3: Monthly fatalities + event markers (combined)
# Last Updated  : May 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions (Sections 15, 16, 17)
# OUTPUTS : figures/conflict_timeline.png
#           figures/conflict_casualties_monthly.png
#           figures/conflict_casualties_monthly_data.csv
#           figures/conflict_casualties_with_events.png
#           figures/conflict_casualties_with_events_data.csv
#
# PURPOSE:
# Provides visual context for the conflict in the data section / motivation
# slides:
#
#   - C1 (timeline)  — political/historical context (when things happened)
#   - C2 (casualties)— empirical context (how intense the violence was)
#   - C3 (combined)  — Option A: single chart mapping events to casualty pattern
#
# Use C3 for slides where the connection between events and violence is the
# point. Use C1 and C2 separately when you want to keep the political and
# empirical layers distinct.


cat("\n========================================================\n")
cat(" Conflict Context Figures (Timeline + Casualties + Combined)\n")
cat("========================================================\n")


# ==============================================================================
# Load INSEC raw data (one row per casualty event)
# ==============================================================================

library(haven)

insec_path <- file.path(modified_data, "conflict_data.dta")

if (!file.exists(insec_path)) {
  stop("INSEC raw casualty file not found at: ", insec_path,
       "\nPlease verify the path or update this driver script.")
}

insec_raw <- read_dta(insec_path)

cat("Loaded INSEC raw data:", nrow(insec_raw), "rows,",
    ncol(insec_raw), "columns\n")


# ==============================================================================
# Figure C1: Conflict Timeline (Major Events)
# ==============================================================================

generate_conflict_timeline(
  output_path = file.path(figures, "conflict_timeline.png")
)


# ==============================================================================
# Figure C2: Monthly Fatalities by Perpetrator (plain)
# ==============================================================================

generate_casualty_timeseries(
  data           = insec_raw,
  outcome_filter = c(1),
  outcome_label  = "Fatalities",
  title          = "Monthly Conflict Fatalities in Nepal, 1996–2006",
  subtitle       = "By perpetrator (Maoist vs. State Forces)",
  file_label     = "conflict_casualties_monthly",
  figure_dir     = figures
)


# ==============================================================================
# Figure C3: Monthly Fatalities + Event Markers (combined — Option A)
# ==============================================================================

generate_casualty_with_events(
  data           = insec_raw,
  outcome_filter = c(1),
  outcome_label  = "Fatalities",
  title          = "Monthly Conflict Fatalities with Key Events, 1996–2006",
  subtitle       = "By perpetrator (Maoist vs. State Forces)",
  file_label     = "conflict_casualties_with_events",
  figure_dir     = figures
)


cat("\n========================================================\n")
cat(" Conflict context figures complete\n")
cat("========================================================\n")