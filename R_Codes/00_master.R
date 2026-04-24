# =============================================================================
# 00_master.R - Master Script for Nepal Civil Conflict Analysis
# =============================================================================
# Project: Nepal Civil Conflict and International Migration
# Author: Ramesh Dulal
# Last Modified: April 2026
#
# DESCRIPTION:
# This master script runs all analysis scripts in sequence.
# You can run the entire analysis by sourcing this single file,
# or run individual scripts for specific tasks.
#
# =============================================================================

# Clear workspace
rm(list = ls())
cat("\014")

# Close any leftover connections from a previous failed run
closeAllConnections()

# ==============================================================================
# SECTION 1: PATH DEFINITIONS
# ==============================================================================

user <- Sys.getenv("USER")    # detects your Mac/Linux username automatically


if (user == "rameshdulal") {
  dropbox <- "/Users/rameshdulal/Library/CloudStorage/Dropbox"
  github  <- "/Users/rameshdulal/Documents/Web Portfolio/Dissertation/International_Migration_and_Nepal_Civil_War"
  
} else if (user == "paghosh") {            # ← replace with your username. In your console type Sys.getenv("USER") to find your username
                                           # <- if you are using windows, type Sys.info()["user"]
  dropbox <- "/Users/paghosh/Dropbox"      # ← replace with your actual Dropbox path
  github  <- "/Users/paghosh/..."          # ← replace with your actual GitHub repo path
  
} else {
  stop(paste(
    "ERROR: Unrecognized user '", user, "'.",
    "Please add your paths to 00_master.R"
  ))
}


# -----------------------------------------------------------------------------
# STEP 1: Setup (Paths)
# -----------------------------------------------------------------------------

# ── All other paths built from roots (same for everyone) ─────────────────── #
modified_data <- file.path(dropbox, "Nepal Civil Conflict/Data/Modified_Data")
r_scripts     <- file.path(github,  "R_Codes")
figures       <- file.path(github,  "Paper/Figures")
nepal_maps <- file.path(dropbox, "Nepal Civil Conflict/Data/Raw_Data/Nepal_maps")
logs        <- file.path(dropbox, "Nepal Civil Conflict/Logs")

# ── Table output folders by category ─────────────────────────────────────── #
tables_summary    <- file.path(github, "Paper/Tables/Tables_Summary")
tables_main       <- file.path(github, "Paper/Tables/Tables_Main")
tables_robustness <- file.path(github, "Paper/Tables/Tables_Robustness")
tables_heterogen  <- file.path(github, "Paper/Tables/Tables_Heterogeneity")
tables_mechanism  <- file.path(github, "Paper/Tables/Tables_Mechanisms")


# ==============================================================================
# SECTION 2: LOG FILE
# ==============================================================================

today    <- format(Sys.Date(), "%m-%d-%Y")
log_file <- file.path(logs, paste0("log_master_", user, "_", today, ".txt"))
con      <- file(log_file, open = "wt")
sink(con, append = FALSE, split = TRUE)   # prints to both console and log
cat("00_master.R started:", format(Sys.time()), "\n\n")

# ==============================================================================
# SECTION 3: RUN EACH R SCRIPT IN SEQUENCE
# ==============================================================================

cat("========================================================\n")
cat(" STEP 1: Setup (Packages)\n")
cat("========================================================\n")
source(file.path(r_scripts, "01_setup.R"))


cat("========================================================\n")
cat(" STEP 2: Data Cleaning & Variable Creation\n")
cat("========================================================\n")
source(file.path(r_scripts, "02_data_cleaning.R"))

# 
cat("========================================================\n")
cat(" STEP 3: Summary Statistics & Descriptive Tables\n")
cat("========================================================\n")
source(file.path(r_scripts, "03_01_summary_overall.R"))

source(file.path(r_scripts, "03_02_sample_construction.R"))

source(file.path(r_scripts, "03_03_summary_multigroup.R"))

source(file.path(r_scripts, "03_04_balance_0-17_Age.R"))

source(file.path(r_scripts, "03_05_balance_0-17_Age_Num.R"))

# 
# cat("========================================================\n")
# cat(" STEP 4: Main Regression Analysis\n")
# cat("========================================================\n")
# source(file.path(r_scripts, "04_regression_main.R"))
# 
# 
# cat("========================================================\n")
# cat(" STEP 5: Robustness Checks\n")
# cat("========================================================\n")
# source(file.path(r_scripts, "05_robustness.R"))
# 
# 
# cat("========================================================\n")
# cat(" STEP 6: Mechanism Analysis (Occupation & Education Channels)\n")
# cat("========================================================\n")
# source(file.path(r_scripts, "06_mechanism_analysis.R"))
# 
# 

# ==============================================================================
# SECTION 4: AUTO-GENERATE tables.js FOR GITHUB PAGES NAVIGATION----
# ==============================================================================
# Uses the tables_registry populated by register_table() calls inside each
# table script. Writes tables.js into the repo root (same folder as
# index.html and viewer.html).
# ------------------------------------------------------------------------------

cat("========================================================\n")
cat(" STEP X: Generating tables.js for GitHub Pages navigation\n")
cat("========================================================\n")

base_url <- "https://dulal-ramesh.github.io/International_Migration_and_Nepal_Civil_War/Paper/Tables/"

# Escape any double quotes in titles or sections so the JS stays valid
esc <- function(x) gsub('"', '\\\\"', x)

js_entries <- sprintf(
  '  { section: "%s", subsection: "%s", title: "%s", url: BASE + "%s" }',
  esc(tables_registry$section),
  esc(tables_registry$subsection),
  esc(tables_registry$title),
  tables_registry$file
)

js_content <- c(
  "// AUTO-GENERATED by R. Do not edit by hand.",
  "// Re-run 00_master.R to update.",
  "",
  paste0('const BASE = "', base_url, '";'),
  "",
  "const TABLES = [",
  paste(js_entries, collapse = ",\n"),
  "];"
)

tables_js_path <- file.path(github, "tables.js")
writeLines(js_content, tables_js_path)

cat("✔ tables.js updated with", nrow(tables_registry), "tables at:\n  ",
    tables_js_path, "\n")
# # ==============================================================================
# # END OF MASTER R FILE
# # ==============================================================================

cat("\n✓ All R steps completed successfully.\n")
cat("Session ended:", format(Sys.time()), "\n")
sink()
close(con)