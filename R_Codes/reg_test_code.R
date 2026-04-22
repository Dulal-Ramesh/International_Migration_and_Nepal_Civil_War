# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Main Regressions - DiD LPM with Progressive Specifications
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R dataset
# OUTPUTS : tables_main/13*.tex / .html

options(modelsummary_factory_default       = "kableExtra")
options(modelsummary_format_numeric_latex  = "plain")


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
# SECTION 2: DEFINE SPECIFICATIONS
# ==============================================================================

# Quadratic age + sex
demo_controls    <- "sex + age + I(age^2)"

# Education dummies (base: No Education)
edu_controls     <- "edu_primary + edu_secondary + edu_tertiary"

# Ethnicity dummies (base: Hill High Caste)
eth_controls     <- "eth_janajati + eth_terai + eth_dalit + eth_muslim"

# Occupation dummies (base: Agriculture)
occ_controls     <- "occ_high_skilled + occ_service + occ_craft + occ_elementary + occ_armed"

# All controls combined
all_controls     <- paste(demo_controls, edu_controls, eth_controls, occ_controls, sep = " + ")

# Fixed effects
fe_str           <- "dist + birth_year"
cluster_str      <- "dist"

# Column headers
col_headers      <- c("(1) Basic", "(2) +Demo", "(3) +Edu", "(4) +Ethnicity", "(5) +FE")

# GOF rows
gof_map <- tribble(
  ~raw,          ~clean,        ~fmt,
  "nobs",        "N",            0,
  "r.squared",   "R-squared",    3
)


# ==============================================================================
# SECTION 3: REGRESSION RUNNER FUNCTION
# ==============================================================================

run_progressive_models <- function(outcome, conflict_var) {
  
  interact <- paste0("childhood_exposed * ", conflict_var)
  
  # Model 1: Basic — interaction only
  m1 <- feols(as.formula(paste0(outcome, " ~ ", interact)),
              data = reg_data, cluster = cluster_str)
  
  # Model 2: + Demographics
  m2 <- feols(as.formula(paste0(outcome, " ~ ", interact, " + ", demo_controls)),
              data = reg_data, cluster = cluster_str)
  
  # Model 3: + Education
  m3 <- feols(as.formula(paste0(outcome, " ~ ", interact, " + ", demo_controls,
                                " + ", edu_controls)),
              data = reg_data, cluster = cluster_str)
  
  # Model 4: + Ethnicity & Occupation
  m4 <- feols(as.formula(paste0(outcome, " ~ ", interact, " + ", demo_controls,
                                " + ", edu_controls, " + ", eth_controls,
                                " + ", occ_controls)),
              data = reg_data, cluster = cluster_str)
  
  # Model 5: + District & Birth Year FE
  m5 <- feols(as.formula(paste0(outcome, " ~ ", interact, " + ", all_controls,
                                " | ", fe_str)),
              data = reg_data, cluster = cluster_str)
  
  models <- list(m1, m2, m3, m4, m5)
  names(models) <- col_headers
  return(models)
}


# ==============================================================================
# SECTION 4: TABLE EXPORTER FUNCTION
# ==============================================================================

export_progressive_table <- function(models,
                                     conflict_var,
                                     conflict_label,
                                     file_label,
                                     caption,
                                     output_dir) {
  
  # Coefficient map — show only key coefficients
  coef_map <- c(
    "childhood_exposed" = "Childhood Exposed (=1)",
    conflict_var        = conflict_label
  )
  names(coef_map)[2] <- conflict_var
  coef_map[paste0("childhood_exposed:", conflict_var)] <- paste0("Childhood Exposed × ", conflict_label)
  
  # Add rows
  add_rows_tbl <- tribble(
    ~term,           ~`(1) Basic`, ~`(2) +Demo`, ~`(3) +Edu`, ~`(4) +Ethnicity`, ~`(5) +FE`,
    "Controls",      "No",         "Yes",         "Yes",        "Yes",             "Yes",
    "District FE",   "No",         "No",          "No",         "No",              "Yes",
    "Birth Year FE", "No",         "No",          "No",         "No",              "Yes"
  )
  
  notes_latex <- paste(
    "OLS estimates (LPM). Outcome variable is binary (0/1).",
    paste0("Conflict intensity measured by: ", conflict_label, "."),
    "Treatment = childhood exposed to conflict (aged 0--17 at conflict start in 1996).",
    "Column (2) adds sex and quadratic age.",
    "Column (3) adds education dummies.",
    "Column (4) adds ethnicity and occupation dummies.",
    "Column (5) adds district and birth year fixed effects.",
    "Standard errors clustered at the district level in parentheses.",
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.10."
  )
  
  notes_html <- paste(
    "OLS estimates (LPM). Outcome variable is binary (0/1).",
    paste0("Conflict intensity measured by: ", conflict_label, "."),
    "Treatment = childhood exposed to conflict (aged 0-17 at conflict start in 1996).",
    "Column (2) adds sex and quadratic age.",
    "Column (3) adds education dummies.",
    "Column (4) adds ethnicity and occupation dummies.",
    "Column (5) adds district and birth year fixed effects.",
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
    escape   = FALSE,
    add_rows = add_rows_tbl
  ) %>%
    kable_styling(
      bootstrap_options = c("hover", "condensed", "bordered"),
      full_width        = FALSE,
      position          = "center",
      font_size         = 13,
      html_font         = "Arial"
    ) %>%
    row_spec(0, background = "#2c3e50", color = "white", bold = TRUE) %>%
    column_spec(1, width = "16em") %>%
    column_spec(2:6, width = "8em")
  
  save_kable(html_table,
             file = file.path(output_dir, paste0(file_label, ".html")))
  
  cat("=== Exported:", file_label, "(.tex / .html) ===\n")
}


# ==============================================================================
# SECTION 5: RUN ALL REGRESSIONS AND EXPORT
# ==============================================================================

# Outcome labels for file naming
outcomes <- list(
  list(var = "international_migrant",       label = "IntMigrant"),
  list(var = "international_absentee_only", label = "CurrentlyAbroad"),
  list(var = "present_ind_migrant",         label = "ReturnMigrant"),
  list(var = "national",                    label = "InternalMigrant")
)

# Conflict measures
conflict_measures <- list(
  list(var = "mwar_own_any",                label = "Months of War",                  suffix = "MonthsWar"),
  list(var = "cas_own_any",                 label = "Casualties",                     suffix = "Casualties"),
  list(var = "high_conflict_q3_binary",     label = "High Conflict (Months of War, Q3)",  suffix = "BinaryWar"),
  list(var = "high_conflict_casualty_binary", label = "High Conflict (Casualties, Q3)", suffix = "BinaryCasualty")
)

# Loop over all outcomes and conflict measures
for (i in seq_along(outcomes)) {
  for (j in seq_along(conflict_measures)) {
    
    outcome  <- outcomes[[i]]
    conflict <- conflict_measures[[j]]
    
    # Table number: 13a-d per outcome, 1-4 per conflict
    table_num  <- paste0("13", letters[i], j)
    file_label <- paste0(table_num, ".Regression_", outcome$label, "_", conflict$suffix)
    caption    <- paste0("Effect of Childhood Conflict Exposure on ",
                         gsub("_", " ", outcome$label),
                         " — ", conflict$label, " (LPM)")
    
    cat("Running:", file_label, "\n")
    
    models <- run_progressive_models(
      outcome      = outcome$var,
      conflict_var = conflict$var
    )
    
    export_progressive_table(
      models         = models,
      conflict_var   = conflict$var,
      conflict_label = conflict$label,
      file_label     = file_label,
      caption        = caption,
      output_dir     = tables_main
    )
  }
}

cat("\n=== All regressions complete! ===\n")