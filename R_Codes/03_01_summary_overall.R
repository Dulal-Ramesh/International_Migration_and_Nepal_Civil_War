# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Summary Statistics - Table 1: Overall Descriptive Statistics
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R dataset
# OUTPUTS : tables_summary/1.Overall_Summary.tex / .html


# ==============================================================================
# SECTION 1: HELPER FUNCTIONS (row builders)
# ==============================================================================
# Each helper returns a data.frame with columns: Variable, Mean, SD, N.
# row_continuous   — one row per continuous variable
# row_binary       — one row per 0/1 variable (mean × 100 = %)
# block_categorical — one row per category of a factor/character variable
# row_panel        — panel header row (bold stats columns blank)
# blank_row        — visual spacer between panels
# -----------------------------------------------------------------------------

# Continuous variable: Mean, SD, N of non-missing values
row_continuous <- function(data, var_name, label) {
  vals <- data[[var_name]]
  data.frame(
    Variable = label,
    Mean     = sprintf("%.2f", round(mean(vals, na.rm = TRUE), 2)),
    SD       = sprintf("%.2f", round(sd(vals,   na.rm = TRUE), 2)),
    N        = format(sum(!is.na(vals)), big.mark = ","),
    stringsAsFactors = FALSE
  )
}

# Binary 0/1 variable: mean reported as % (×100), SD of the binary in pp units
row_binary <- function(data, var_name, label) {
  vals <- data[[var_name]]
  p    <- mean(vals == 1, na.rm = TRUE)
  data.frame(
    Variable = label,
    Mean     = sprintf("%.2f", round(p * 100, 2)),
    SD       = sprintf("%.2f", round(sd(vals == 1, na.rm = TRUE) * 100, 2)),
    N        = format(sum(!is.na(vals)), big.mark = ","),
    stringsAsFactors = FALSE
  )
}

# Categorical variable: one row per category with within-category % and count
block_categorical <- function(data, var_name, indent = 2) {
  # Use non-breaking space (U+00A0) so HTML does not collapse the indentation.
  # LaTeX renders \u00A0 as a regular space, which works fine there too.
  indent_str <- strrep("\u00A0", indent)
  data %>%
    select(category = all_of(var_name)) %>%
    filter(!is.na(category)) %>%
    count(category, name = "n") %>%
    mutate(
      Variable = paste0(indent_str, category),
      Mean     = sprintf("%.2f", round(n / sum(n) * 100, 2)),
      SD       = "",
      N        = format(n, big.mark = ",")
    ) %>%
    select(Variable, Mean, SD, N)
}

# Panel header (bolded later via row_spec)
row_panel <- function(label) {
  data.frame(Variable = label, Mean = "", SD = "", N = "", stringsAsFactors = FALSE)
}

# Blank spacer between panels
blank_row <- function() {
  data.frame(Variable = "", Mean = "", SD = "", N = "", stringsAsFactors = FALSE)
}


# ==============================================================================
# SECTION 2: MISSING-DATA COUNTS (for footnote transparency)
# ==============================================================================

n_edu_missing <- sum(is.na(nlss_conflict_data$education_category))
n_eth_missing <- sum(is.na(nlss_conflict_data$Ethnicity))
n_mar_missing <- sum(is.na(nlss_conflict_data$marital_label))
n_occ_missing <- sum(is.na(nlss_conflict_data$occupation_category))


# ==============================================================================
# SECTION 3: BUILD THE TABLE (six thematic panels)
# ==============================================================================

table1_overall <- bind_rows(
  
  # -------- Panel A: Migration outcomes -------------------------------------
  row_panel("Panel A: Migration outcomes"),
  row_binary(nlss_conflict_data, "international_migrant",       "International Migrant (=1)"),
  row_binary(nlss_conflict_data, "international_absentee_only", "Currently Abroad (=1)"),
  row_binary(nlss_conflict_data, "present_ind_migrant",         "Return Migrant (=1)"),
  row_binary(nlss_conflict_data, "national",                    "Internal Migrant (=1)"),
  row_binary(nlss_conflict_data, "absent",                      "Absent from Household (=1)"),
  blank_row(),
  
  # -------- Panel B: Exposure to conflict (1996-2006) -----------------------
  row_panel("Panel B: Exposure to conflict (1996-2006)"),
  row_continuous(nlss_conflict_data, "mwar_own_any",   "Months of War (any)"),
  row_continuous(nlss_conflict_data, "mwar_own_fatal", "Months of War (fatal)"),
  row_continuous(nlss_conflict_data, "cas_own_any",    "Casualties (any)"),
  row_continuous(nlss_conflict_data, "cas_own_fatal",  "Casualties (fatal)"),
  blank_row(),
  
  # -------- Panel C: Demographics -------------------------------------------
  row_panel("Panel C: Demographics"),
  row_continuous(nlss_conflict_data, "age",                   "Age in 2017"),
  row_continuous(nlss_conflict_data, "age_at_conflict_start", "Age at Conflict Start (1996)"),
  row_binary(nlss_conflict_data,     "male",                  "Male (=1)"),
  row_panel("Marital Status (%):"),
  block_categorical(nlss_conflict_data, "marital_label", indent = 4),
  blank_row(),
  
  # -------- Panel D: Education ----------------------------------------------
  row_panel("Panel D: Education"),
  row_continuous(nlss_conflict_data, "grade_comp", "Years of Education"),
  row_panel("\u00A0Education Distribution (%):"),
  block_categorical(nlss_conflict_data, "education_category", indent = 4),
  blank_row(),
  
  # -------- Panel E: Ethnicity (%) ------------------------------------------
  row_panel("Panel E: Ethnicity (%)"),
  block_categorical(nlss_conflict_data, "Ethnicity", indent = 4),
  blank_row(),
  
  # -------- Panel F: Occupation (%) -----------------------------------------
  row_panel("Panel F: Occupation (%)"),
  block_categorical(nlss_conflict_data, "occupation_category", indent = 4)
)


# ==============================================================================
# SECTION 4: EXPORT OUTPUTS
# ==============================================================================

# Shared footnote text used by both LaTeX and HTML outputs.
notes_common <- c(
  "International Migrant includes individuals abroad at time of survey and those who had been abroad for at least 3 months in the past.",
  "Currently Abroad includes individuals abroad at the time of survey.",
  "Return Migrant includes only individuals who travelled abroad for at least 3 months.",
  "Internal Migrant includes individuals migrating inside the country at the time of survey.",
  "Absent from Household includes all absent individuals at the time of survey.",
  
  paste0(
    "Distribution percentages within Panels C-F are computed on non-missing observations. ",
    "Missing counts: Education = ",  format(n_edu_missing, big.mark = ","),
    "; Ethnicity = ",                 format(n_eth_missing, big.mark = ","),
    "; Marital Status = ",            format(n_mar_missing, big.mark = ","),
    "; Occupation = ",                format(n_occ_missing, big.mark = ","), "."
  ),
  paste0(
    "Occupation percentages exclude 2,334 observations coded 'Not Stated' (NSCO code 9999) ",
    "and 1,726 absent household members whose occupation was not reported by the household respondent."
  ),
  "Source: Nepal Labour Force Survey 2017/18; conflict exposure data from INSEC (1996-2006)."
)

# Identify which rows are main-panel headers (for bold formatting)
panel_rows <- which(grepl("^Panel [A-F]:", table1_overall$Variable))


# --- LaTeX ---
table1_latex <- table1_overall %>%
  mutate(Variable = sanitize_latex(Variable))

latex_table1 <- kable(table1_latex,
                      format    = "latex",
                      booktabs  = TRUE,
                      caption   = "Descriptive Statistics: Overall Sample",
                      label     = "tab:overall_summary",
                      col.names = c("Variable", "Mean / \\%", "SD", "N"),
                      escape    = FALSE,
                      align     = c("l", "r", "r", "r")) %>%
  kable_styling(latex_options = c("hold_position"),
                font_size     = 10) %>%
  row_spec(panel_rows, bold = TRUE) %>%
  footnote(general           = notes_common,
           general_title     = "Notes:",
           footnote_as_chunk = FALSE,
           threeparttable    = TRUE,
           escape            = FALSE)

writeLines(as.character(latex_table1),
           file.path(tables_summary, "1.Overall_Summary.tex"))


# --- HTML ---
# ---- EDIT (2026-04): Uses style_html_table() from 01_setup.R (Section 8)
# for house-style consistency with all other HTML tables in the project.
html_table1 <- kable(table1_overall,
                     format    = "html",
                     col.names = c("Variable", "Mean / %", "SD", "N"),
                     caption   = "Descriptive Statistics: Overall Sample",
                     align     = c("l", "r", "r", "r")) %>%
  style_html_table(font_size = 13) %>%
  row_spec(panel_rows, bold = TRUE, background = "#f5f5f5") %>%
  footnote(general           = notes_common,
           general_title     = "Notes:",
           footnote_as_chunk = FALSE)

writeLines(as.character(html_table1),
           file.path(tables_summary, "1.Overall_Summary.html"))