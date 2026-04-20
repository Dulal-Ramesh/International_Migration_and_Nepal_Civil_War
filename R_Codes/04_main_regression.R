# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Main Regressions - DiD LPM with District and Birth Year FE
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R dataset
# OUTPUTS : tables_main/13a-d.Main_Regression.tex / .html

options(modelsummary_factory_default  = "kableExtra")
options(modelsummary_format_numeric_latex = "plain")
# ==============================================================================
# SECTION 1: PREPARE SAMPLE
# ==============================================================================

reg_data <- nlss_conflict_data %>%
  filter(treatment %in% c(0, 1)) %>%
  filter(!is.na(childhood_exposed),
         !is.na(age),
         !is.na(male),
         !is.na(edu_no_education),
         !is.na(eth_hill_high),
         !is.na(occ_agriculture),
         !is.na(dist),
         !is.na(birth_year))

cat("=== Regression Sample Size ===\n")
cat("N:", nrow(reg_data), "\n")


# ==============================================================================
# SECTION 2: DEFINE CONTROLS, FIXED EFFECTS, AND OUTCOMES
# ==============================================================================

# Controls (base: No Education, Hill High Caste, Agriculture)
controls <- c(
  "age", "male",
  "edu_primary", "edu_secondary", "edu_tertiary",
  "eth_janajati", "eth_terai", "eth_dalit", "eth_muslim",
  "occ_high_skilled", "occ_service", "occ_craft",
  "occ_elementary", "occ_armed"
)

controls_str <- paste(controls, collapse = " + ")
fe_str       <- "dist + birth_year"
cluster_str  <- "dist"

# Outcome variables
outcomes <- c(
  "international_migrant",
  "international_absentee_only",
  "present_ind_migrant",
  "national"
)

# Column labels
col_labels_html <- c(
  "International Migration",
  "Currently Abroad",
  "Return Migrant",
  "Internal Migration"
)


# ==============================================================================
# SECTION 3: REGRESSION RUNNER FUNCTION
# ==============================================================================

run_did_models <- function(conflict_var) {
  models <- lapply(outcomes, function(outcome) {
    fml <- as.formula(
      paste0(outcome,
             " ~ childhood_exposed + ", conflict_var,
             " + childhood_exposed:", conflict_var,
             " + ", controls_str,
             " | ", fe_str)
    )
    feols(fml,
          data    = reg_data,
          cluster = cluster_str)
  })
  names(models) <- col_labels_html
  return(models)
}


# ==============================================================================
# SECTION 4: TABLE EXPORTER FUNCTION
# ==============================================================================

export_did_table <- function(models,
                             conflict_var,
                             conflict_label,
                             file_label,
                             caption,
                             output_dir) {
  
  # Coefficient map — show only key coefficients
  coef_map <- c(
    "childhood_exposed"                        = "Childhood Exposed (=1)",
    conflict_var                               = conflict_label
  )
  coef_map[paste0("childhood_exposed:", conflict_var)] <- paste0("Childhood Exposed × ", conflict_label)
  
  # GOF rows
  gof_map <- tribble(
    ~raw,          ~clean,       ~fmt,
    "nobs",        "N",           0,
    "r.squared",   "R-squared",   3
  )
  
  # Additional rows for FE and controls
  add_rows_tbl <- tribble(
    ~term,           ~`1`,   ~`2`,   ~`3`,   ~`4`,
    "Controls",      "Yes",  "Yes",  "Yes",  "Yes",
    "District FE",   "Yes",  "Yes",  "Yes",  "Yes",
    "Birth Year FE", "Yes",  "Yes",  "Yes",  "Yes"
  )
  
  # Notes
  notes_latex <- paste(
    "OLS estimates (LPM). Outcome variables are binary (0/1).",
    paste0("Conflict intensity measured by: ", conflict_label, "."),
    "Treatment = childhood exposed to conflict (aged 0--17 at conflict start in 1996).",
    "Controls include age, sex, education, ethnicity, and occupation.",
    "District and birth year fixed effects included.",
    "Standard errors clustered at the district level in parentheses.",
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.10."
  )
  
  notes_html <- paste(
    "OLS estimates (LPM). Outcome variables are binary (0/1).",
    paste0("Conflict intensity measured by: ", conflict_label, "."),
    "Treatment = childhood exposed to conflict (aged 0-17 at conflict start in 1996).",
    "Controls include age, sex, education, ethnicity, and occupation.",
    "District and birth year fixed effects included.",
    "Standard errors clustered at the district level in parentheses.",
    "*** p<0.01, ** p<0.05, * p<0.10."
  )
  
  # --- LaTeX ---
  modelsummary(
    models,
    output   = file.path(output_dir, paste0(file_label, ".tex")),
    coef_map = coef_map,
    gof_map  = gof_map,
    stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    title    = caption,
    notes    = notes_latex,
    escape   = FALSE,
    booktabs = TRUE,
    add_rows = add_rows_tbl
  )
  
  # --- HTML ---
  html_table <- modelsummary(
    models,
    output   = "kableExtra",
    coef_map = coef_map,
    gof_map  = gof_map,
    stars    = c("*" = 0.10, "**" = 0.05, "***" = 0.01),
    title    = caption,
    notes    = notes_html,
    fmt      = 4,
    escape = FALSE,
    add_rows = add_rows_tbl
  ) %>%
    kable_styling(
      bootstrap_options = c("hover", "condensed", "bordered"),
      full_width        = FALSE,
      position          = "center",
      font_size         = 13,
      html_font         = "Arial"
    ) %>%
    row_spec(0, background = "#2c3e50", color = "white", bold = TRUE)%>%
    column_spec(1, width = "16em") %>%   # ← variable name column
    column_spec(2:5, width = "10em")     # ← result columns
  
  save_kable(html_table,
             file = file.path(output_dir, paste0(file_label, ".html")))
  
  cat("=== Exported:", file_label, "(.tex / .html) ===\n")
}


# ==============================================================================
# SECTION 5: RUN AND EXPORT ALL FOUR TABLES
# ==============================================================================

# --- 13a: Continuous — Months of War ---
models_13a <- run_did_models("mwar_own_any")
export_did_table(
  models         = models_13a,
  conflict_var   = "mwar_own_any",
  conflict_label = "Months of War",
  file_label     = "13a.Main_Regression_MonthsWar",
  caption        = "Effect of Childhood Conflict Exposure on Migration — Months of War (LPM)",
  output_dir     = tables_main
)

# --- 13b: Continuous — Casualties ---
models_13b <- run_did_models("cas_own_any")
export_did_table(
  models         = models_13b,
  conflict_var   = "cas_own_any",
  conflict_label = "Casualties",
  file_label     = "13b.Main_Regression_Casualties",
  caption        = "Effect of Childhood Conflict Exposure on Migration — Casualties (LPM)",
  output_dir     = tables_main
)

# --- 13c: Binary — Months of War (Q3) ---
models_13c <- run_did_models("high_conflict_q3_binary")
export_did_table(
  models         = models_13c,
  conflict_var   = "high_conflict_q3_binary",
  conflict_label = "High Conflict (Months of War, Q3)",
  file_label     = "13c.Main_Regression_BinaryWar",
  caption        = "Effect of Childhood Conflict Exposure on Migration — High Conflict Binary, Months of War (LPM)",
  output_dir     = tables_main
)

# --- 13d: Binary — Casualties (Q3) ---
models_13d <- run_did_models("high_conflict_casualty_binary")
export_did_table(
  models         = models_13d,
  conflict_var   = "high_conflict_casualty_binary",
  conflict_label = "High Conflict (Casualties, Q3)",
  file_label     = "13d.Main_Regression_BinaryCasualty",
  caption        = "Effect of Childhood Conflict Exposure on Migration — High Conflict Binary, Casualties (LPM)",
  output_dir     = tables_main
)