# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Summary Statistics - Table 2: Sample Construction / Attrition
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions,
#           02_data_cleaning.R datasets (nlss_conflict_data, nlss_analysis_sample)
# OUTPUTS : tables_summary/2.Sample_Construction.tex / .html
#
# PURPOSE:
# Documents every step from raw NLSS roster to the main estimation sample.
# Each row: a sample-construction step, the resulting N, the number dropped
# at that step, and a brief reason. 


# ==============================================================================
# SECTION 1: RECOMPUTE N AT EACH STEP
# ==============================================================================


raw_data <- read_dta(file.path(modified_data, "1_conflict_present_absentee_data.dta"))

# Step 1: Raw roster (starting point)
n_raw <- nrow(raw_data)

# Step 2: After dropping missing district
n_after_district <- raw_data %>%
  drop_na(dist) %>%
  nrow()

# Step 3: Full cleaned data (as produced by 02_data_cleaning.R)
n_full_clean <- nrow(nlss_conflict_data)

# Step 4: Breakdown of who gets excluded by the cohort restriction
excl_counts <- nlss_conflict_data %>%
  count(cohort_group, name = "n")

# Pull specific excluded-group counts (safe extraction — returns 0 if absent)
get_cohort_n <- function(grp) {
  val <- excl_counts$n[excl_counts$cohort_group == grp]
  if (length(val) == 0) 0 else val
}

n_too_young_2017   <- get_cohort_n("Excluded: Too Young in 2017")
n_too_old_2017     <- get_cohort_n("Excluded: Too Old in 2017")
n_too_young_conf   <- get_cohort_n("Excluded: Too Young During Conflict")
n_too_old_1996     <- get_cohort_n("Excluded: Age 41+ in 1996")
n_overlap_age      <- get_cohort_n("Excluded: Overlap Age")
n_other_excluded   <- get_cohort_n("Excluded: Other")
n_total_excluded   <- sum(c(n_too_young_2017, n_too_old_2017, n_too_young_conf,
                            n_too_old_1996,  n_overlap_age,   n_other_excluded))

# Step 5: Treatment cohort (any age at conflict onset 0-17)
n_treatment <- sum(nlss_analysis_sample$treatment == 1, na.rm = TRUE)

# Step 6: Control cohort (age at conflict onset 18-40)
n_control <- sum(nlss_analysis_sample$treatment == 0, na.rm = TRUE)

# Step 7: Final analysis sample
n_analysis <- nrow(nlss_analysis_sample)


# ==============================================================================
# SECTION 2: BUILD THE TABLE
# ==============================================================================

# Helper: format an integer with commas
fmt_n <- function(x) format(x, big.mark = ",")

# Row builder. A step row has Step text, N, Dropped count, and Reason.
# Section-header rows leave numeric cells blank.
row_step <- function(step, n = NA, dropped = NA, reason = "") {
  data.frame(
    Step    = step,
    N       = if (is.na(n))       "" else fmt_n(n),
    Dropped = if (is.na(dropped)) "" else fmt_n(dropped),
    Reason  = reason,
    stringsAsFactors = FALSE
  )
}

row_header <- function(label) {
  data.frame(Step = label, N = "", Dropped = "", Reason = "",
             stringsAsFactors = FALSE)
}

row_blank <- function() {
  data.frame(Step = "", N = "", Dropped = "", Reason = "",
             stringsAsFactors = FALSE)
}

# Compute step-by-step drops
drop_district  <- n_raw - n_after_district
drop_at_clean  <- n_after_district - n_full_clean   # usually 0 — just belt and suspenders
drop_cohort    <- n_full_clean - n_analysis

# Assemble the table
sample_construction <- bind_rows(
  
  row_header("Panel A: From raw data to cleaned sample"),
  row_step("1. Full NLSS 2017/18 individual roster",
           n = n_raw, dropped = NA,
           reason = "Starting point"),
  row_step("2. Drop observations missing district code",
           n = n_after_district, dropped = drop_district,
           reason = "District required to merge INSEC conflict data"),
  row_step("3. Cleaned full sample",
           n = n_full_clean, dropped = drop_at_clean,
           reason = "After variable construction (demographics, conflict, outcomes)"),
  row_blank(),
  
  row_header("Panel B: Cohort restriction (defining the analysis sample)"),
  row_step("4a. Exclude: too young in 2017 (age < 18)",
           dropped = n_too_young_2017,
           reason  = "Not yet working-age adults"),
  row_step("4b. Exclude: too old in 2017 (age > 65)",
           dropped = n_too_old_2017,
           reason  = "Retirement-age; labor market decisions non-comparable"),
  row_step("4c. Exclude: too young during conflict (age < 6 at conflict end)",
           dropped = n_too_young_conf,
           reason  = "Insufficient exposure window during formative years"),
  row_step("4d. Exclude: too old at conflict start (age 41+ in 1996)",
           dropped = n_too_old_1996,
           reason  = "Already past career-formation stage when conflict began"),
  row_step("4e. Exclude: overlap age cohort",
           dropped = n_overlap_age,
           reason  = "Age 18 at conflict start and 41-46 in 2017; ambiguous exposure"),
  row_step("4f. Other exclusions",
           dropped = n_other_excluded,
           reason  = "Age combinations outside treatment/control definitions"),
  row_blank(),
  
  row_header("Panel C: Main estimation sample"),
  row_step("5. Treatment cohort (age 0-17 at conflict start, 18-45 in 2017)",
           n = n_treatment,
           reason = "Exposed to conflict during childhood/adolescence"),
  row_step("6. Control cohort (age 18-40 at conflict start, 47-65 in 2017)",
           n = n_control,
           reason = "Fully formed adults when conflict began"),
  row_step("7. MAIN ESTIMATION SAMPLE (Treatment + Control)",
           n = n_analysis, dropped = drop_cohort,
           reason = "Used in balance table and all main regressions")
)


# ==============================================================================
# SECTION 3: EXPORT OUTPUTS
# ==============================================================================

notes_sample <- c(
  paste0("Starting sample: Nepal Labour Force Survey 2017/18 full individual ",
         "roster (", fmt_n(n_raw), " observations)."),
  paste0("Conflict exposure data from INSEC covering 1996-2006. District ",
         "information required to merge conflict and survey data."),
  paste0("Cohort definitions are based on age at conflict onset (1996): ",
         "treatment = age 0-17, control = age 18-40. Current-age windows in ",
         "2017 (18-45 for treatment; 47-65 for control) ensure both cohorts ",
         "are working-age adults at survey."),
  paste0("Regressions that additionally control for education or occupation ",
         "will have slightly smaller samples due to listwise deletion of ",
         "missing covariates. The final analysis sample of ", fmt_n(n_analysis),
         " refers to the main (no additional controls) specification."),
  "Source: Nepal Labour Force Survey 2017/18; INSEC conflict database."
)


# --- LaTeX ---
sample_latex <- sample_construction %>%
  mutate(across(everything(), sanitize_latex))

# Identify which rows are panel headers (for bold formatting)
panel_rows <- which(grepl("^Panel [A-C]:", sample_construction$Step))
# Identify the "MAIN ESTIMATION SAMPLE" row (for emphasis)
main_row   <- which(grepl("^7\\.", sample_construction$Step))

latex_sample <- kable(sample_latex,
                      format    = "latex",
                      booktabs  = TRUE,
                      caption   = "Sample Construction",
                      label     = "tab:sample_construction",
                      col.names = c("Step", "N", "Dropped", "Reason"),
                      escape    = FALSE,
                      align     = c("l", "r", "r", "l")) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"),
                font_size = 9) %>%
  row_spec(panel_rows, bold = TRUE) %>%
  row_spec(main_row,   bold = TRUE) %>%
  footnote(general           = notes_sample,
           general_title     = "Notes:",
           footnote_as_chunk = FALSE,
           threeparttable    = TRUE,
           escape            = FALSE)

writeLines(as.character(latex_sample),
           file.path(tables_summary, "2.Sample_Construction.tex"))


# --- HTML ---
html_sample <- kable(sample_construction,
                     format    = "html",
                     col.names = c("Step", "N", "Dropped", "Reason"),
                     caption   = "Sample Construction",
                     align     = c("l", "r", "r", "l")) %>%
  style_html_table(font_size = 13) %>%
  row_spec(panel_rows, bold = TRUE, background = "#f5f5f5") %>%
  row_spec(main_row,   bold = TRUE, background = "#fff2cc") %>%  # soft highlight
  column_spec(1, width = "30em") %>%
  column_spec(2, width = "10em") %>%
  column_spec(3, width = "10em") %>%
  column_spec(4, width = "35em", extra_css = "padding-left: 1.5em;", include_thead = TRUE) %>%
  footnote(general           = notes_sample,
           general_title     = "Notes:",
           footnote_as_chunk = FALSE)

writeLines(as.character(html_sample),
           file.path(tables_summary, "2.Sample_Construction.html"))


# --- Console summary ---
cat("\n=== Sample construction summary ===\n")
cat("  Raw:                      ", fmt_n(n_raw),             "\n")
cat("  After district drop:      ", fmt_n(n_after_district),  "\n")
cat("  Cleaned full sample:      ", fmt_n(n_full_clean),      "\n")
cat("  Treatment cohort:         ", fmt_n(n_treatment),       "\n")
cat("  Control cohort:           ", fmt_n(n_control),         "\n")
cat("  Final analysis sample:    ", fmt_n(n_analysis),        "\n")
cat("\n=== Exported 2.Sample_Construction (.tex / .html) ===\n")