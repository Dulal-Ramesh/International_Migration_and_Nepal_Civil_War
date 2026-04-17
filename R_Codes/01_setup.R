# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Setup - Packages and Global Settings and Helper Function
# Last Updated  : April 2026
# ==============================================================================


# ==============================================================================
# SECTION 1: PACKAGE INSTALLATION
# (Run once, then comment out)
# ==============================================================================

# install.packages(c(
#   "haven",        # Import Stata files
#   "dplyr",        # Data manipulation
#   "tidyr",        # Data reshaping
#   "ggplot2",      # Visualization
#   "labelled",     # Variable labels
#   "stringr",      # String manipulation
#   "knitr",        # Tables for LaTeX
#   "kableExtra",   # Enhanced tables
#   "writexl",      # Export to Excel
#   "fixest",       # Fixed effects regression
#   "stargazer",    # Regression tables
#   "modelsummary", # Model summary tables
#   "tinytex",      # LaTeX compilation
#   "tinytable",    # Tables
#   "webshot2"      # Save HTML as PNG
# ))

# For LaTeX packages (run once)
# tinytex::install_tinytex()
# tinytex::tlmgr_install(c("booktabs", "float", "colortbl", "xcolor"))


# ==============================================================================
# SECTION 2: LOAD PACKAGES
# ==============================================================================

# Data import/export
library(haven)
library(writexl)

# Data manipulation
library(dplyr)
library(tidyr)
library(stringr)
library(labelled)

# Visualization
library(ggplot2)

# Tables and output
library(knitr)
library(kableExtra)
library(tinytex)
library(tinytable)
library(webshot2)

# Regression analysis
library(fixest)
library(stargazer)
library(modelsummary)


# ==============================================================================
# SECTION 3: GLOBAL SETTINGS
# ==============================================================================

# Survey year for age calculations
SURVEY_YEAR <- 2017

# Conflict period
CONFLICT_START <- 1996
CONFLICT_END   <- 2006

# ==============================================================================
# SECTION 4: SHARED HELPER FUNCTIONS----
# ==============================================================================

# Reusable function to compute group-level summary stats
compute_group_stats <- function(data_subset) {
  data_subset %>%
    summarise(
      N = n(),
      Age_Mean          = round(mean(age,                   na.rm = TRUE), 2),
      Age_SD            = round(sd(age,                     na.rm = TRUE), 2),
      Age_Conflict_Mean = round(mean(age_at_conflict_start, na.rm = TRUE), 2),
      Age_Conflict_SD   = round(sd(age_at_conflict_start,   na.rm = TRUE), 2),
      Male_Pct          = round(mean(sex == 1,              na.rm = TRUE) * 100, 2),
      No_Edu_Pct        = round(mean(education_category == "No Education",     na.rm = TRUE) * 100, 2),
      Primary_Pct       = round(mean(education_category == "Primary (1-5)",    na.rm = TRUE) * 100, 2),
      Secondary_Pct     = round(mean(education_category == "Secondary (6-12)", na.rm = TRUE) * 100, 2),
      Tertiary_Pct      = round(mean(education_category == "Tertiary",         na.rm = TRUE) * 100, 2),
      High_Caste_Pct    = round(mean(Ethnicity == "Hill High Caste", na.rm = TRUE) * 100, 2),
      Janajati_Pct      = round(mean(Ethnicity == "Hill Janajati",   na.rm = TRUE) * 100, 2),
      Terai_Pct         = round(mean(Ethnicity == "Terai/Madhesi",   na.rm = TRUE) * 100, 2),
      Dalit_Pct         = round(mean(Ethnicity == "Dalit",           na.rm = TRUE) * 100, 2),
      Muslim_Pct        = round(mean(Ethnicity == "Muslim",          na.rm = TRUE) * 100, 2),
      Agri_Pct          = round(mean(occupation_category == "Agriculture",            na.rm = TRUE) * 100, 2),
      HighSkill_Pct     = round(mean(occupation_category == "High Skilled",           na.rm = TRUE) * 100, 2),
      Service_Pct       = round(mean(occupation_category == "Service & Clerical",     na.rm = TRUE) * 100, 2),
      Craft_Pct         = round(mean(occupation_category == "Craft & Manufacturing",  na.rm = TRUE) * 100, 2),
      Elementary_Pct    = round(mean(occupation_category == "Elementary/Low Skilled", na.rm = TRUE) * 100, 2),
      Armed_Pct         = round(mean(occupation_category == "Armed Forces",           na.rm = TRUE) * 100, 2),
      .groups = "drop"
    )
}

# Shortcut: pull one stat from a stats data frame
g <- function(stats_df, col) stats_df[[col]]



# ==============================================================================
# SECTION 5: CORRECTING THE LATEX OUTPUT-----
# ==============================================================================

# Sanitize strings for LaTeX output
sanitize_latex <- function(x) {
  x %>%
    str_replace_all("&",  "\\\\&") %>%   # & → \&
    str_replace_all("%",  "\\\\%")        # % → \%
}



# ==============================================================================
# SECTION 6: BUILDING STANDART COVARIATE SUMMARY TABLE-----
# ==============================================================================

# Format mean (SD) — latex = TRUE puts SD on new line using \makecell
format_mean_sd <- function(mean_val, sd_val, latex = FALSE) {
  if (latex) {
    paste0("\\makecell[c]{", mean_val, " \\\\ (", sd_val, ")}")
  } else {
    paste0(mean_val, " (", sd_val, ")")
  }
}

# Format mean (SE) — same pattern
format_mean_se <- function(mean_val, se_val, latex = FALSE) {
  if (latex) {
    paste0("\\makecell[c]{", mean_val, " \\\\ (", se_val, ")}")
  } else {
    paste0(mean_val, " (", se_val, ")")
  }
}

build_covariate_table <- function(groups, latex = FALSE) {
  
  # Standard Variable column (same for every summary table)
  var_col <- c(
    "Sample Size",
    "",
    "Age",
    "  Age in 2017",
    "",
    "  Age at Conflict Start",
    "",
    "Male (%)",
    "",
    "Education Distribution (%):",
    "  No Education",
    "  Primary (1-5)",
    "  Secondary (6-12)",
    "  Tertiary",
    "",
    "Ethnicity Distribution (%):",
    "  Hill High Caste",
    "  Hill Janajati",
    "  Terai/Madhesi",
    "  Dalit",
    "  Muslim",
    "",
    "Occupation Type (%):",
    "  Agriculture",
    "  High Skilled",
    "  Service & Clerical",
    "  Craft & Manufacturing",
    "  Elementary/Low Skilled",
    "  Armed Forces"
  )
  
  # Build one data column per group using its value-puller function
  group_cols <- lapply(groups, function(val) {
    c(
      as.character(val("N")),
      "",
      "",
      format_mean_sd(val("Age_Mean"),          val("Age_SD"),          latex = latex),  # ← passed here
      "",
      format_mean_sd(val("Age_Conflict_Mean"), val("Age_Conflict_SD"), latex = latex),  # ← and here
      "",
      as.character(val("Male_Pct")),
      "",
      "",
      as.character(val("No_Edu_Pct")),
      as.character(val("Primary_Pct")),
      as.character(val("Secondary_Pct")),
      as.character(val("Tertiary_Pct")),
      "",
      "",
      as.character(val("High_Caste_Pct")),
      as.character(val("Janajati_Pct")),
      as.character(val("Terai_Pct")),
      as.character(val("Dalit_Pct")),
      as.character(val("Muslim_Pct")),
      "",
      "",
      as.character(val("Agri_Pct")),
      as.character(val("HighSkill_Pct")),
      as.character(val("Service_Pct")),
      as.character(val("Craft_Pct")),
      as.character(val("Elementary_Pct")),
      as.character(val("Armed_Pct"))
    )
  })
  
  # Combine Variable column with all group columns
  result <- data.frame(Variable = var_col, stringsAsFactors = FALSE)
  for (col_name in names(group_cols)) {
    result[[col_name]] <- group_cols[[col_name]]
  }
  
  return(result)
}


# ==============================================================================
# SECTION 7: BUILDING ttest BALANCE TABLE-----
# ==============================================================================

# Welch's t-test: Treatment minus Control with stars and p-value
compute_ttest_diff <- function(treat_df, ctrl_df, var,
                               is_continuous = FALSE,
                               latex         = FALSE) {
  
  t_vals <- treat_df[[var]][!is.na(treat_df[[var]])]
  c_vals <- ctrl_df[[var]][!is.na(ctrl_df[[var]])]
  
  if (length(t_vals) < 2 | length(c_vals) < 2) return("")
  
  test <- t.test(t_vals, c_vals, var.equal = FALSE)
  
  diff <- if (is_continuous) {
    round(mean(t_vals) - mean(c_vals), 2)
  } else {
    round((mean(t_vals) - mean(c_vals)) * 100, 2)
  }
  
  stars <- case_when(
    test$p.value < 0.01 ~ "***",
    test$p.value < 0.05 ~ "**",
    test$p.value < 0.10 ~ "*",
    TRUE                ~ ""
  )
  
  pval <- paste0("(p=", formatC(test$p.value, format = "f", digits = 3), ")")
  
  # LaTeX: stars on estimate, p-value on new line via \makecell----
  if (latex) {
    paste0("\\makecell[c]{", diff, stars, " \\\\ ", pval, "}")
  } else {
    paste0(diff, stars, " ", pval)
  }
}


# Build the Diff (T-C) column — row order matches build_covariate_table()
build_diff_column <- function(treat_df, ctrl_df, latex = FALSE) {
  d <- function(var, continuous = FALSE) {
    compute_ttest_diff(treat_df, ctrl_df, var,
                       is_continuous = continuous,
                       latex         = latex)
  }
  
  c(
    "",   # Sample Size
    "",
    "",   # Age header
    d("age",                   continuous = TRUE),
    "",
    d("age_at_conflict_start", continuous = TRUE),
    "",
    d("male"),
    "",
    "",   # Education header
    d("edu_no_education"),
    d("edu_primary"),
    d("edu_secondary"),
    d("edu_tertiary"),
    "",
    "",   # Ethnicity header
    d("eth_hill_high"),
    d("eth_janajati"),
    d("eth_terai"),
    d("eth_dalit"),
    d("eth_muslim"),
    "",
    "",   # Occupation header
    d("occ_agriculture"),
    d("occ_high_skilled"),
    d("occ_service"),
    d("occ_craft"),
    d("occ_elementary"),
    d("occ_armed")
  )
}


# ==============================================================================
# SECTION 8: BALANCE TABLE GENERATOR ----
# ==============================================================================

generate_balance_table <- function(data,
                                   treat_age_min,
                                   treat_age_max,
                                   ctrl_age_min  = 18,
                                   ctrl_age_max  = 40,
                                   file_label,
                                   caption_label,
                                   output_dir) {
  
  # --- Define subsets ---
  treat_df <- data %>%
    filter(age_at_conflict_start >= treat_age_min,
           age_at_conflict_start <= treat_age_max,
           age >= 18, age <= 45)
  
  ctrl_df  <- data %>%
    filter(age_at_conflict_start >= ctrl_age_min,
           age_at_conflict_start <= ctrl_age_max,
           age >= 47, age <= 65)
  
  all_df   <- bind_rows(treat_df, ctrl_df)
  
  cat("=== Group Size Check (Age", treat_age_min, "-", treat_age_max, "vs",
      ctrl_age_min, "-", ctrl_age_max, ") ===\n")
  cat("All:       ", nrow(all_df),   "\n")
  cat("Treatment: ", nrow(treat_df), "\n")
  cat("Control:   ", nrow(ctrl_df),  "\n")
  
  # --- Helper functions ---
  get_mean     <- function(df, var) round(mean(df[[var]], na.rm = TRUE), 2)
  get_sd       <- function(df, var) paste0("(", round(sd(df[[var]], na.rm = TRUE), 2), ")")
  get_mean_pct <- function(df, var) round(mean(df[[var]], na.rm = TRUE) * 100, 2)
  get_sd_pct   <- function(df, var) paste0("(", round(sd(df[[var]], na.rm = TRUE) * 100, 2), ")")
  
  get_diff <- function(var, is_continuous = FALSE) {
    t_vals <- treat_df[[var]][!is.na(treat_df[[var]])]
    c_vals <- ctrl_df[[var]][!is.na(ctrl_df[[var]])]
    if (length(t_vals) < 2 | length(c_vals) < 2) return("")
    test  <- t.test(t_vals, c_vals, var.equal = FALSE)
    diff  <- if (is_continuous) round(mean(t_vals) - mean(c_vals), 2) else
      round((mean(t_vals) - mean(c_vals)) * 100, 2)
    stars <- case_when(test$p.value < 0.01 ~ "***",
                       test$p.value < 0.05 ~ "**",
                       test$p.value < 0.10 ~ "*",
                       TRUE                ~ "")
    paste0(diff, stars)
  }
  
  get_se <- function(var, is_continuous = FALSE) {
    t_vals <- treat_df[[var]][!is.na(treat_df[[var]])]
    c_vals <- ctrl_df[[var]][!is.na(ctrl_df[[var]])]
    if (length(t_vals) < 2 | length(c_vals) < 2) return("")
    se <- sqrt(var(t_vals) / length(t_vals) + var(c_vals) / length(c_vals))
    if (!is_continuous) se <- se * 100
    paste0("(", round(se, 2), ")")
  }
  
  # --- Row builders ---
  row_cont  <- function(label, var) data.frame(
    Variable = label,
    All_Mean = as.character(get_mean(all_df, var)),    All_SD = get_sd(all_df, var),
    Treat_Mean = as.character(get_mean(treat_df, var)), Treat_SD = get_sd(treat_df, var),
    Control_Mean = as.character(get_mean(ctrl_df, var)), Control_SD = get_sd(ctrl_df, var),
    Diff_TC = get_diff(var, TRUE), SE = get_se(var, TRUE), stringsAsFactors = FALSE)
  
  row_bin   <- function(label, var) data.frame(
    Variable = label,
    All_Mean = as.character(get_mean_pct(all_df, var)),    All_SD = get_sd_pct(all_df, var),
    Treat_Mean = as.character(get_mean_pct(treat_df, var)), Treat_SD = get_sd_pct(treat_df, var),
    Control_Mean = as.character(get_mean_pct(ctrl_df, var)), Control_SD = get_sd_pct(ctrl_df, var),
    Diff_TC = get_diff(var, FALSE), SE = get_se(var, FALSE), stringsAsFactors = FALSE)
  
  row_blank <- function() data.frame(Variable="", All_Mean="", All_SD="",
                                     Treat_Mean="", Treat_SD="", Control_Mean="", Control_SD="", Diff_TC="", SE="",
                                     stringsAsFactors=FALSE)
  
  row_panel <- function(label) data.frame(Variable=label, All_Mean="", All_SD="",
                                          Treat_Mean="", Treat_SD="", Control_Mean="", Control_SD="", Diff_TC="", SE="",
                                          stringsAsFactors=FALSE)
  
  # --- Build table ---
  build_tbl <- function(latex = FALSE) {
    tbl <- bind_rows(
      row_panel("Panel A: Outcomes"),
      row_bin("  International Migration (=1)", "international_migrant"),
      row_bin("  Currently Abroad (=1)",         "international_absentee_only"),
      row_bin("  Return Migrant (=1)",            "present_ind_migrant"),
      row_bin("  Internal Migration (=1)",        "national"),
      row_blank(),
      row_panel("Panel B: Exposure to Conflict"),
      row_cont("  Months of War (own district)",       "mwar_own_any"),
      row_cont("  Casualties (own district)",           "cas_own_any"),
      row_cont("  Months of War (own district, fatal)", "mwar_own_fatal"),
      row_cont("  Casualties (own district, fatal)",    "cas_own_fatal"),
      row_blank(),
      row_panel("Panel C: Demographics"),
      row_cont("  Current Age",           "age"),
      row_cont("  Age at Conflict Start", "age_at_conflict_start"),
      row_bin("  Male (=1)",              "male"),
      row_blank(),
      row_panel("Panel D: Education (%)"),
      row_bin("  No Education",     "edu_no_education"),
      row_bin("  Primary (1-5)",    "edu_primary"),
      row_bin("  Secondary (6-12)", "edu_secondary"),
      row_bin("  Tertiary",         "edu_tertiary"),
      row_blank(),
      row_panel("Panel E: Ethnicity (%)"),
      row_bin("  Hill High Caste", "eth_hill_high"),
      row_bin("  Hill Janajati",   "eth_janajati"),
      row_bin("  Terai/Madhesi",   "eth_terai"),
      row_bin("  Dalit",           "eth_dalit"),
      row_bin("  Muslim",          "eth_muslim"),
      row_blank(),
      row_panel("Panel F: Occupation (%)"),
      row_bin("  Agriculture",            "occ_agriculture"),
      row_bin("  High Skilled",           "occ_high_skilled"),
      row_bin("  Service & Clerical",     "occ_service"),
      row_bin("  Craft & Manufacturing",  "occ_craft"),
      row_bin("  Elementary/Low Skilled", "occ_elementary"),
      row_bin("  Armed Forces",           "occ_armed"),
      row_blank(),
      data.frame(Variable="N", All_Mean=as.character(nrow(all_df)), All_SD="",
                 Treat_Mean=as.character(nrow(treat_df)), Treat_SD="",
                 Control_Mean=as.character(nrow(ctrl_df)), Control_SD="",
                 Diff_TC="", SE="", stringsAsFactors=FALSE)
    )
    if (latex) tbl <- tbl %>% mutate(Variable = sanitize_latex(Variable))
    return(tbl)
  }
  
  tbl_plain <- build_tbl(latex = FALSE)
  tbl_latex <- build_tbl(latex = TRUE)
  
  col_names <- c("Variable", "All Mean", "All (SD)",
                 "Treatment Mean", "(SD)", "Control Mean", "(SD)",
                 "Diff (T-C)", "(SE)")
  col_align <- c("l","r","r","r","r","r","r","r","r")
  
  treat_label <- paste0("Treatment (Age ", treat_age_min, "--", treat_age_max, ")")
  ctrl_label  <- paste0("Control (Age ",   ctrl_age_min,  "--", ctrl_age_max,  ")")
  treat_label_html <- paste0("Treatment (Age ", treat_age_min, "-", treat_age_max, ")")
  ctrl_label_html  <- paste0("Control (Age ",   ctrl_age_min,  "-", ctrl_age_max,  ")")
  
  panel_rows <- which(tbl_latex$Variable %in% c(
    "Panel A: Outcomes", "Panel B: Exposure to Conflict",
    "Panel C: Demographics", "Panel D: Education (\\%)",
    "Panel E: Ethnicity (\\%)", "Panel F: Occupation (\\%)"))
  
  footnote_text_latex <- paste(
    paste0("Treatment = aged ", treat_age_min, "--", treat_age_max, " at conflict start (1996)."),
    paste0("Control = aged ", ctrl_age_min, "--", ctrl_age_max, " at conflict start (1996)."),
    "Standard deviations in parentheses.",
    "Standard errors of the difference in parentheses in SE column.",
    "Education, ethnicity, and occupation percentages conditional on non-missing values.",
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.10.",
    "Source: Nepal Labor Force Survey; conflict data from INSEC."
  )
  
  footnote_text_html <- paste(
    paste0("Treatment = aged ", treat_age_min, "-", treat_age_max, " at conflict start (1996)."),
    paste0("Control = aged ", ctrl_age_min, "-", ctrl_age_max, " at conflict start (1996)."),
    "Standard deviations in parentheses.",
    "Standard errors of the difference in parentheses in SE column.",
    "*** p<0.01, ** p<0.05, * p<0.10.",
    "Source: Nepal Labor Force Survey; conflict data from INSEC."
  )
  
  # --- LaTeX ---
  latex_out <- kable(tbl_latex, format="latex", booktabs=TRUE,
                     caption=paste0("Summary Statistics of Individuals (", caption_label, ")"),
                     label=paste0("balance_table_", file_label),
                     col.names=col_names, escape=FALSE, align=col_align) %>%
    kable_styling(latex_options=c("hold_position","scale_down"), font_size=9) %>%
    add_header_above(c(" "=1, "All"=2, setNames(2, treat_label), setNames(2, ctrl_label), " "=2)) %>%
    row_spec(panel_rows, bold=TRUE, italic=TRUE) %>%
    footnote(general=footnote_text_latex, footnote_as_chunk=TRUE, escape=FALSE) %>%
    landscape()
  
  writeLines(as.character(latex_out),
             file.path(output_dir, paste0(file_label, ".Balance_Table.tex")))
  
  
  # --- HTML ---
  html_out <- kable(tbl_plain, format="html", col.names=col_names, align=col_align,
                    caption=paste0("Summary Statistics of Individuals (", caption_label, ")")) %>%
    kable_styling(bootstrap_options=c("hover","condensed","bordered"),
                  full_width=FALSE, position="center", fixed_thead=TRUE,
                  font_size=13, html_font="Arial") %>%
    add_header_above(c(" "=1, "All"=2,
                       setNames(2, treat_label_html),
                       setNames(2, ctrl_label_html), " "=2)) %>%
    row_spec(0, background="#2c3e50", color="white", bold=TRUE) %>%
    row_spec(panel_rows, background="#f0f4f8", bold=TRUE, italic=TRUE) %>%
    row_spec(1:nrow(tbl_plain), color="#1a1a1a") %>%
    column_spec(1, width="20em") %>%
    column_spec(2:9, width="6em") %>%
    footnote(general=footnote_text_html, footnote_as_chunk=TRUE)
  
  save_kable(html_out,
             file=file.path(output_dir, paste0(file_label, ".Balance_Table.html")))
  
  cat("=== Exported:", file_label, "(.tex / .md / .html) ===\n")
}