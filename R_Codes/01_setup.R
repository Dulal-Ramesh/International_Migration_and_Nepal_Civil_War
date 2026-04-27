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
      Male_Pct          = round(mean(male,                  na.rm = TRUE) * 100, 2),
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

sanitize_latex <- function(x) {
  x <- gsub("&", "\\&", x, fixed = TRUE)   # & → \&
  x <- gsub("%", "\\%", x, fixed = TRUE)   # % → \%
  x <- gsub("$", "\\$", x, fixed = TRUE)   # $ → \$
  x <- gsub("#", "\\#", x, fixed = TRUE)   # # → \#
  x <- gsub("_", "\\_", x, fixed = TRUE)   # _ → \_
  x
}



# ==============================================================================
# SECTION 6: BUILDING STANDARD COVARIATE SUMMARY TABLE-----
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
      format_mean_sd(val("Age_Mean"),          val("Age_SD"),          latex = latex),
      "",
      format_mean_sd(val("Age_Conflict_Mean"), val("Age_Conflict_SD"), latex = latex),
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

# Core t-test helper: returns raw components (diff, se, stars, pval) as a list.
# Returns NULL if either group has fewer than 2 non-missing observations.
compute_ttest_stats <- function(treat_df, ctrl_df, var, is_continuous = FALSE) {
  t_vals <- treat_df[[var]][!is.na(treat_df[[var]])]
  c_vals <- ctrl_df[[var]][!is.na(ctrl_df[[var]])]
  
  if (length(t_vals) < 2 || length(c_vals) < 2) return(NULL)
  
  test <- t.test(t_vals, c_vals, var.equal = FALSE)
  
  scale <- if (is_continuous) 1 else 100
  diff  <- round((mean(t_vals) - mean(c_vals)) * scale, 2)
  se    <- round(sqrt(var(t_vals) / length(t_vals) +
                        var(c_vals) / length(c_vals)) * scale, 2)
  
  stars <- case_when(
    test$p.value < 0.01 ~ "***",
    test$p.value < 0.05 ~ "**",
    test$p.value < 0.10 ~ "*",
    TRUE                ~ ""
  )
  
  list(diff = diff, se = se, stars = stars, pval = test$p.value)
}


format_ttest_diff <- function(stats, include_pval = FALSE, latex = FALSE) {
  if (is.null(stats)) return("")
  diff_str <- paste0(stats$diff, stats$stars)
  
  if (!include_pval) return(diff_str)
  
  pval_str <- paste0("(p=", formatC(stats$pval, format = "f", digits = 3), ")")
  if (latex) {
    paste0("\\makecell[c]{", diff_str, " \\\\ ", pval_str, "}")
  } else {
    paste0(diff_str, " ", pval_str)
  }
}


format_ttest_se <- function(stats) {
  if (is.null(stats)) return("")
  paste0("(", stats$se, ")")
}


compute_ttest_diff <- function(treat_df, ctrl_df, var,
                               is_continuous = FALSE,
                               latex         = FALSE) {
  stats <- compute_ttest_stats(treat_df, ctrl_df, var, is_continuous)
  format_ttest_diff(stats, include_pval = TRUE, latex = latex)
}


build_diff_column <- function(treat_df, ctrl_df, latex = FALSE) {
  d <- function(var, continuous = FALSE) {
    compute_ttest_diff(treat_df, ctrl_df, var,
                       is_continuous = continuous,
                       latex         = latex)
  }
  
  c(
    "",
    "",
    "",
    d("age",                   continuous = TRUE),
    "",
    d("age_at_conflict_start", continuous = TRUE),
    "",
    d("male"),
    "",
    "",
    d("edu_no_education"),
    d("edu_primary"),
    d("edu_secondary"),
    d("edu_tertiary"),
    "",
    "",
    d("eth_hill_high"),
    d("eth_janajati"),
    d("eth_terai"),
    d("eth_dalit"),
    d("eth_muslim"),
    "",
    "",
    d("occ_agriculture"),
    d("occ_high_skilled"),
    d("occ_service"),
    d("occ_craft"),
    d("occ_elementary"),
    d("occ_armed")
  )
}


# ==============================================================================
# SECTION 8: SHARED HTML TABLE STYLING ----
# ==============================================================================
# 
# style_html_table(): Wrapper around kable_styling() that applies the project's
# house style to every HTML table. Use this in every export script so all HTML
# tables share the same font, sizing, and striping. Change the defaults here
# once and every HTML table in the project updates.
#
# House style:
#   - Sans-serif body font (Helvetica Neue / Arial family)
#   - Striped + hover + condensed Bootstrap options
#   - Centered, not full-width (tables size to content)
#   - Panel highlight color by convention: #f5f5f5 (light gray)
#     → Apply via row_spec(panel_rows, background = "#f5f5f5") in caller
#
# Usage:
#   kable(df, format = "html", ...) %>%
#     style_html_table() %>%
#     row_spec(panel_rows, background = "#f5f5f5", bold = TRUE) %>%
#     footnote(...)
#
# Declared BEFORE generate_balance_table() so that function can use it.
# -----------------------------------------------------------------------------

style_html_table <- function(kbl,
                             font_size   = 13,
                             full_width  = FALSE,
                             striped     = TRUE,
                             fixed_thead = FALSE,
                             html_font   = "'Helvetica Neue', Helvetica, Arial, sans-serif") {
  
  bootstrap_opts <- c("hover", "condensed")
  if (striped) bootstrap_opts <- c("striped", bootstrap_opts)
  
  kbl %>%
    kable_styling(bootstrap_options = bootstrap_opts,
                  full_width        = full_width,
                  font_size         = font_size,
                  position          = "center",
                  fixed_thead       = fixed_thead,
                  html_font         = html_font)
}


# ==============================================================================
# SECTION 9: BALANCE TABLE GENERATOR ----
# ==============================================================================

generate_balance_table <- function(data,
                                   treat_age_min,
                                   treat_age_max,
                                   ctrl_age_min       = 18,
                                   ctrl_age_max       = 40,
                                   treat_curr_age_min = 18,
                                   treat_curr_age_max = 45,
                                   ctrl_curr_age_min  = 47,
                                   ctrl_curr_age_max  = 65,
                                   file_label,
                                   caption_label,
                                   output_dir) {
  
  # --- Define subsets ---
  treat_df <- data %>%
    filter(age_at_conflict_start >= treat_age_min,
           age_at_conflict_start <= treat_age_max,
           age >= treat_curr_age_min, age <= treat_curr_age_max)
  
  ctrl_df  <- data %>%
    filter(age_at_conflict_start >= ctrl_age_min,
           age_at_conflict_start <= ctrl_age_max,
           age >= ctrl_curr_age_min, age <= ctrl_curr_age_max)
  
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
    stats <- compute_ttest_stats(treat_df, ctrl_df, var, is_continuous)
    format_ttest_diff(stats, include_pval = FALSE, latex = FALSE)
  }
  
  get_se <- function(var, is_continuous = FALSE) {
    stats <- compute_ttest_stats(treat_df, ctrl_df, var, is_continuous)
    format_ttest_se(stats)
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
  # ---- EDIT (2026-04): Refactored to use shared style_html_table() helper.
  # Previously used inline kable_styling(..., html_font = "Arial", ...) with a
  # dark #2c3e50 header, which differed from other HTML tables in the project.
  # Now uses the shared house style for visual consistency across all tables.
  # Balance-table-specific touches (column widths, add_header_above) stay here.
  html_out <- kable(tbl_plain, format="html", col.names=col_names, align=col_align,
                    caption=paste0("Summary Statistics of Individuals (", caption_label, ")")) %>%
    style_html_table(font_size = 13, fixed_thead = TRUE) %>%
    add_header_above(c(" "=1, "All"=2,
                       setNames(2, treat_label_html),
                       setNames(2, ctrl_label_html), " "=2)) %>%
    row_spec(0,          bold = TRUE) %>%
    row_spec(panel_rows, background = "#f5f5f5", bold = TRUE) %>%
    column_spec(1,   width = "20em") %>%
    column_spec(2:9, width = "6em") %>%
    footnote(general = footnote_text_html, footnote_as_chunk = TRUE)
  
  # ---- EDIT (2026-04): Replaced save_kable() with writeLines() — save_kable
  # for HTML can still invoke webshot2 for rendering checks, which we don't need.
  writeLines(as.character(html_out),
             file.path(output_dir, paste0(file_label, ".Balance_Table.html")))
  
  cat("=== Exported:", file_label, "(.tex / .html) ===\n")
}


# ==============================================================================
# SECTION 10: HIGH vs LOW CONFLICT DiD TABLE GENERATOR ----
# ==============================================================================
# Produces a descriptive table comparing High-Conflict and Low-Conflict districts
# for a specified age cohort (default: 0-17 at conflict onset). Row structure
# mirrors generate_balance_table() exactly (6 panels, 28 variable rows) so the
# two tables can sit side-by-side in the paper. Column structure is simpler:
#   Variable | High Mean | (SD) | Low Mean | (SD) | Diff (H-L) | (SE)
#
# Standard errors in the Diff column come from a district-clustered linear
# regression (via feols) — the correct approach when the exposure variable
# varies only at the district level. This is more defensible than a Welch
# t-test and matches the SE treatment in the main regression pipeline.
# ------------------------------------------------------------------------------

# Clustered-SE helper: fit Y ~ D with district clusters, return diff/se/stars.
# Returns NULL if the regression cannot be fit (e.g. no variation in D).
# `scale` is applied to both the coefficient and SE (use 100 for binary vars
# to report percentage-point differences, 1 for continuous).
compute_clustered_diff_stats <- function(df, outcome_var, exposure_var,
                                         cluster_var = "dist",
                                         is_continuous = FALSE) {
  # Drop rows with missing outcome, exposure, or cluster
  sub <- df[, c(outcome_var, exposure_var, cluster_var)]
  sub <- sub[complete.cases(sub), ]
  
  # Need variation in both outcome and exposure
  if (length(unique(sub[[exposure_var]])) < 2) return(NULL)
  if (nrow(sub) < 10)                          return(NULL)
  
  fml <- as.formula(paste(outcome_var, "~", exposure_var))
  clust_fml <- as.formula(paste("~", cluster_var))
  
  fit <- tryCatch(
    feols(fml, data = sub, cluster = clust_fml, warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  
  est <- tryCatch(summary(fit)$coeftable, error = function(e) NULL)
  if (is.null(est) || !(exposure_var %in% rownames(est))) return(NULL)
  
  scale <- if (is_continuous) 1 else 100
  diff  <- round(est[exposure_var, "Estimate"]   * scale, 2)
  se    <- round(est[exposure_var, "Std. Error"] * scale, 2)
  pval  <- est[exposure_var, "Pr(>|t|)"]
  
  stars <- case_when(
    pval < 0.01 ~ "***",
    pval < 0.05 ~ "**",
    pval < 0.10 ~ "*",
    TRUE        ~ ""
  )
  
  list(diff = diff, se = se, stars = stars, pval = pval)
}

# Format the Diff column — "0.12***" or "" if stats are NULL.
format_clustered_diff <- function(stats) {
  if (is.null(stats)) return("")
  paste0(stats$diff, stats$stars)
}

# Format standard error as "(0.05)" or "" if stats are NULL.
format_clustered_se <- function(stats) {
  if (is.null(stats)) return("")
  paste0("(", stats$se, ")")
}


# -----------------------------------------------------------------------------
# Main function. Mirrors generate_balance_table() but splits on a conflict-
# intensity binary instead of a treat/control cohort.
#
# Arguments:
#   data           : cleaned dataset (typically nlss_conflict_data)
#   treat_age_min  : min age at conflict onset for the sample (default 0)
#   treat_age_max  : max age at conflict onset for the sample (default 17)
#   conflict_var   : name of the High/Low binary (e.g. "high_conflict_q3_binary")
#   conflict_label : short label for the footnote (e.g. "Months of War (Q3)")
#   cluster_var    : cluster variable for SEs (default "dist")
#   file_label     : output filename prefix (e.g. "4a")
#   caption_label  : caption text for the table (e.g. "Age 0-17 at Conflict Start")
#   output_dir     : folder to write .tex and .html
# -----------------------------------------------------------------------------

generate_conflict_did_table <- function(data,
                                        treat_age_min  = 0,
                                        treat_age_max  = 17,
                                        conflict_var   = "high_conflict_q3_binary",
                                        conflict_label = "Months of War (Q3 cutoff)",
                                        cluster_var    = "dist",
                                        file_label,
                                        caption_label,
                                        output_dir) {
  
  # --- Restrict sample to the chosen cohort, require non-missing conflict var ---
  samp <- data %>%
    filter(age_at_conflict_start >= treat_age_min,
           age_at_conflict_start <= treat_age_max,
           !is.na(.data[[conflict_var]]))
  
  high_df <- samp %>% filter(.data[[conflict_var]] == 1)
  low_df  <- samp %>% filter(.data[[conflict_var]] == 0)
  
  cat("=== Conflict DiD Sample Check (Age", treat_age_min, "-", treat_age_max, ") ===\n")
  cat("  Total:         ", nrow(samp),    "\n")
  cat("  High-Conflict: ", nrow(high_df), "\n")
  cat("  Low-Conflict:  ", nrow(low_df),  "\n")
  
  # --- Single-group helpers ---
  get_mean     <- function(df, var) round(mean(df[[var]], na.rm = TRUE), 2)
  get_sd       <- function(df, var) paste0("(", round(sd(df[[var]], na.rm = TRUE), 2), ")")
  get_mean_pct <- function(df, var) round(mean(df[[var]], na.rm = TRUE) * 100, 2)
  get_sd_pct   <- function(df, var) paste0("(", round(sd(df[[var]], na.rm = TRUE) * 100, 2), ")")
  
  # --- Diff + SE from clustered regression on the FULL sample ---
  get_diff <- function(var, is_continuous = FALSE) {
    stats <- compute_clustered_diff_stats(samp, var, conflict_var,
                                          cluster_var   = cluster_var,
                                          is_continuous = is_continuous)
    format_clustered_diff(stats)
  }
  
  get_se <- function(var, is_continuous = FALSE) {
    stats <- compute_clustered_diff_stats(samp, var, conflict_var,
                                          cluster_var   = cluster_var,
                                          is_continuous = is_continuous)
    format_clustered_se(stats)
  }
  
  # --- Row builders (7-column layout: Variable | H Mean | H SD | L Mean | L SD | Diff | SE) ---
  row_cont <- function(label, var) data.frame(
    Variable = label,
    High_Mean = as.character(get_mean(high_df, var)), High_SD = get_sd(high_df, var),
    Low_Mean  = as.character(get_mean(low_df,  var)), Low_SD  = get_sd(low_df,  var),
    Diff_HL = get_diff(var, TRUE), SE = get_se(var, TRUE),
    stringsAsFactors = FALSE)
  
  row_bin <- function(label, var) data.frame(
    Variable = label,
    High_Mean = as.character(get_mean_pct(high_df, var)), High_SD = get_sd_pct(high_df, var),
    Low_Mean  = as.character(get_mean_pct(low_df,  var)), Low_SD  = get_sd_pct(low_df,  var),
    Diff_HL = get_diff(var, FALSE), SE = get_se(var, FALSE),
    stringsAsFactors = FALSE)
  
  row_blank <- function() data.frame(
    Variable="", High_Mean="", High_SD="", Low_Mean="", Low_SD="", Diff_HL="", SE="",
    stringsAsFactors = FALSE)
  
  row_panel <- function(label) data.frame(
    Variable = label, High_Mean="", High_SD="", Low_Mean="", Low_SD="", Diff_HL="", SE="",
    stringsAsFactors = FALSE)
  
  # --- Build table (row content identical to generate_balance_table()) ---
  build_tbl <- function(latex = FALSE) {
    tbl <- bind_rows(
      row_panel("Panel A: Outcomes"),
      row_bin("  International Migration (=1)",  "international_migrant"),
      row_bin("  Currently Abroad (=1)",           "international_absentee_only"),
      row_bin("  Return Migrant (=1)",             "present_ind_migrant"),
      row_bin("  Internal Migration (=1)",         "national"),
      row_blank(),
      row_panel("Panel B: Exposure to Conflict"),
      row_cont("  Months of War (own district)",        "mwar_own_any"),
      row_cont("  Casualties (own district)",            "cas_own_any"),
      row_cont("  Months of War (own district, fatal)",  "mwar_own_fatal"),
      row_cont("  Casualties (own district, fatal)",     "cas_own_fatal"),
      row_blank(),
      row_panel("Panel C: Demographics"),
      row_cont("  Current Age",           "age"),
      row_cont("  Age at Conflict Start", "age_at_conflict_start"),
      row_bin("  Male (=1)",              "male"),
      row_blank(),
      row_panel("Panel D: Education (%)"),
      row_bin("  No Education",      "edu_no_education"),
      row_bin("  Primary (1-5)",     "edu_primary"),
      row_bin("  Secondary (6-12)",  "edu_secondary"),
      row_bin("  Tertiary",          "edu_tertiary"),
      row_blank(),
      row_panel("Panel E: Ethnicity (%)"),
      row_bin("  Hill High Caste", "eth_hill_high"),
      row_bin("  Hill Janajati",   "eth_janajati"),
      row_bin("  Terai/Madhesi",   "eth_terai"),
      row_bin("  Dalit",           "eth_dalit"),
      row_bin("  Muslim",          "eth_muslim"),
      row_blank(),
      row_panel("Panel F: Occupation (%)"),
      row_bin("  Agriculture",             "occ_agriculture"),
      row_bin("  High Skilled",            "occ_high_skilled"),
      row_bin("  Service & Clerical",      "occ_service"),
      row_bin("  Craft & Manufacturing",   "occ_craft"),
      row_bin("  Elementary/Low Skilled",  "occ_elementary"),
      row_bin("  Armed Forces",            "occ_armed"),
      row_blank(),
      data.frame(Variable = "N",
                 High_Mean = as.character(nrow(high_df)), High_SD = "",
                 Low_Mean  = as.character(nrow(low_df)),  Low_SD  = "",
                 Diff_HL = "", SE = "",
                 stringsAsFactors = FALSE)
    )
    if (latex) tbl <- tbl %>% mutate(Variable = sanitize_latex(Variable))
    return(tbl)
  }
  
  tbl_plain <- build_tbl(latex = FALSE)
  tbl_latex <- build_tbl(latex = TRUE)
  
  col_names <- c("Variable",
                 "High Mean", "(SD)",
                 "Low Mean",  "(SD)",
                 "Diff (H-L)", "(SE)")
  col_align <- c("l", "r", "r", "r", "r", "r", "r")
  
  panel_rows <- which(tbl_latex$Variable %in% c(
    "Panel A: Outcomes", "Panel B: Exposure to Conflict",
    "Panel C: Demographics", "Panel D: Education (\\%)",
    "Panel E: Ethnicity (\\%)", "Panel F: Occupation (\\%)"))
  
  footnote_text_latex <- paste(
    paste0("Sample restricted to individuals aged ", treat_age_min, "--",
           treat_age_max, " at conflict start (1996)."),
    paste0("High-Conflict: districts above the 75th percentile of ",
           conflict_label, "; Low-Conflict: at or below (including zeros)."),
    "Standard deviations in parentheses.",
    paste0("Diff (H-L) reports the coefficient $b$ from $Y = a + b \\times \\text{HighConflict}$; ",
           "standard errors in parentheses are clustered at the district level."),
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.10.",
    "Source: Nepal Labor Force Survey; conflict data from INSEC."
  )
  
  footnote_text_html <- c(
    paste0("Sample restricted to individuals aged ", treat_age_min, "-",
           treat_age_max, " at conflict start (1996)."),
    paste0("High-Conflict: districts above the 75th percentile of ",
           conflict_label, "; Low-Conflict: at or below (including zeros)."),
    "Standard deviations in parentheses.",
    paste0("Diff (H-L) reports the coefficient from Y = a + b \u00D7 HighConflict; ",
           "standard errors in parentheses are clustered at the district level."),
    "*** p<0.01, ** p<0.05, * p<0.10.",
    "Source: Nepal Labor Force Survey; conflict data from INSEC."
  )
  
  # --- LaTeX ---
  latex_out <- kable(tbl_latex, format = "latex", booktabs = TRUE,
                     caption = paste0("High- vs. Low-Conflict Comparison (", caption_label, ")"),
                     label   = paste0("conflict_did_", file_label),
                     col.names = col_names, escape = FALSE, align = col_align) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 9) %>%
    add_header_above(c(" " = 1,
                       "High-Conflict" = 2,
                       "Low-Conflict"  = 2,
                       " " = 2)) %>%
    row_spec(panel_rows, bold = TRUE, italic = TRUE) %>%
    footnote(general = footnote_text_latex, footnote_as_chunk = TRUE,
             escape = FALSE) %>%
    landscape()
  
  writeLines(as.character(latex_out),
             file.path(output_dir, paste0(file_label, ".Conflict_DiD_Table.tex")))
  
  # --- HTML ---
  html_out <- kable(tbl_plain, format = "html", col.names = col_names,
                    align = col_align,
                    caption = paste0("High- vs. Low-Conflict Comparison (",
                                     caption_label, ")")) %>%
    style_html_table(font_size = 13, fixed_thead = TRUE) %>%
    add_header_above(c(" " = 1,
                       "High-Conflict" = 2,
                       "Low-Conflict"  = 2,
                       " " = 2)) %>%
    row_spec(0,          bold = TRUE) %>%
    row_spec(panel_rows, background = "#f5f5f5", bold = TRUE) %>%
    column_spec(1,   width = "20em") %>%
    column_spec(2:7, width = "6em") %>%
    footnote(general = footnote_text_html, footnote_as_chunk = FALSE)
  
  writeLines(as.character(html_out),
             file.path(output_dir, paste0(file_label, ".Conflict_DiD_Table.html")))
  
  cat("=== Exported:", file_label, "Conflict_DiD_Table (.tex / .html) ===\n")
}


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
      Male_Pct          = round(mean(male,                  na.rm = TRUE) * 100, 2),
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

sanitize_latex <- function(x) {
  x <- gsub("&", "\\&", x, fixed = TRUE)   # & → \&
  x <- gsub("%", "\\%", x, fixed = TRUE)   # % → \%
  x <- gsub("$", "\\$", x, fixed = TRUE)   # $ → \$
  x <- gsub("#", "\\#", x, fixed = TRUE)   # # → \#
  x <- gsub("_", "\\_", x, fixed = TRUE)   # _ → \_
  x
}



# ==============================================================================
# SECTION 6: BUILDING STANDARD COVARIATE SUMMARY TABLE-----
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
      format_mean_sd(val("Age_Mean"),          val("Age_SD"),          latex = latex),
      "",
      format_mean_sd(val("Age_Conflict_Mean"), val("Age_Conflict_SD"), latex = latex),
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

# Core t-test helper: returns raw components (diff, se, stars, pval) as a list.
# Returns NULL if either group has fewer than 2 non-missing observations.
compute_ttest_stats <- function(treat_df, ctrl_df, var, is_continuous = FALSE) {
  t_vals <- treat_df[[var]][!is.na(treat_df[[var]])]
  c_vals <- ctrl_df[[var]][!is.na(ctrl_df[[var]])]
  
  if (length(t_vals) < 2 || length(c_vals) < 2) return(NULL)
  
  test <- t.test(t_vals, c_vals, var.equal = FALSE)
  
  scale <- if (is_continuous) 1 else 100
  diff  <- round((mean(t_vals) - mean(c_vals)) * scale, 2)
  se    <- round(sqrt(var(t_vals) / length(t_vals) +
                        var(c_vals) / length(c_vals)) * scale, 2)
  
  stars <- case_when(
    test$p.value < 0.01 ~ "***",
    test$p.value < 0.05 ~ "**",
    test$p.value < 0.10 ~ "*",
    TRUE                ~ ""
  )
  
  list(diff = diff, se = se, stars = stars, pval = test$p.value)
}


format_ttest_diff <- function(stats, include_pval = FALSE, latex = FALSE) {
  if (is.null(stats)) return("")
  diff_str <- paste0(stats$diff, stats$stars)
  
  if (!include_pval) return(diff_str)
  
  pval_str <- paste0("(p=", formatC(stats$pval, format = "f", digits = 3), ")")
  if (latex) {
    paste0("\\makecell[c]{", diff_str, " \\\\ ", pval_str, "}")
  } else {
    paste0(diff_str, " ", pval_str)
  }
}


format_ttest_se <- function(stats) {
  if (is.null(stats)) return("")
  paste0("(", stats$se, ")")
}


compute_ttest_diff <- function(treat_df, ctrl_df, var,
                               is_continuous = FALSE,
                               latex         = FALSE) {
  stats <- compute_ttest_stats(treat_df, ctrl_df, var, is_continuous)
  format_ttest_diff(stats, include_pval = TRUE, latex = latex)
}


build_diff_column <- function(treat_df, ctrl_df, latex = FALSE) {
  d <- function(var, continuous = FALSE) {
    compute_ttest_diff(treat_df, ctrl_df, var,
                       is_continuous = continuous,
                       latex         = latex)
  }
  
  c(
    "",
    "",
    "",
    d("age",                   continuous = TRUE),
    "",
    d("age_at_conflict_start", continuous = TRUE),
    "",
    d("male"),
    "",
    "",
    d("edu_no_education"),
    d("edu_primary"),
    d("edu_secondary"),
    d("edu_tertiary"),
    "",
    "",
    d("eth_hill_high"),
    d("eth_janajati"),
    d("eth_terai"),
    d("eth_dalit"),
    d("eth_muslim"),
    "",
    "",
    d("occ_agriculture"),
    d("occ_high_skilled"),
    d("occ_service"),
    d("occ_craft"),
    d("occ_elementary"),
    d("occ_armed")
  )
}


# ==============================================================================
# SECTION 8: SHARED HTML TABLE STYLING ----
# ==============================================================================
# 
# style_html_table(): Wrapper around kable_styling() that applies the project's
# house style to every HTML table. Use this in every export script so all HTML
# tables share the same font, sizing, and striping. Change the defaults here
# once and every HTML table in the project updates.
#
# House style:
#   - Sans-serif body font (Helvetica Neue / Arial family)
#   - Striped + hover + condensed Bootstrap options
#   - Centered, not full-width (tables size to content)
#   - Panel highlight color by convention: #f5f5f5 (light gray)
#     → Apply via row_spec(panel_rows, background = "#f5f5f5") in caller
#
# Usage:
#   kable(df, format = "html", ...) %>%
#     style_html_table() %>%
#     row_spec(panel_rows, background = "#f5f5f5", bold = TRUE) %>%
#     footnote(...)
#
# Declared BEFORE generate_balance_table() so that function can use it.
# -----------------------------------------------------------------------------

style_html_table <- function(kbl,
                             font_size   = 13,
                             full_width  = FALSE,
                             striped     = TRUE,
                             fixed_thead = FALSE,
                             html_font   = "'Helvetica Neue', Helvetica, Arial, sans-serif") {
  
  bootstrap_opts <- c("hover", "condensed")
  if (striped) bootstrap_opts <- c("striped", bootstrap_opts)
  
  kbl %>%
    kable_styling(bootstrap_options = bootstrap_opts,
                  full_width        = full_width,
                  font_size         = font_size,
                  position          = "center",
                  fixed_thead       = fixed_thead,
                  html_font         = html_font)
}


# ==============================================================================
# SECTION 9: BALANCE TABLE GENERATOR ----
# ==============================================================================

generate_balance_table <- function(data,
                                   treat_age_min,
                                   treat_age_max,
                                   ctrl_age_min       = 18,
                                   ctrl_age_max       = 40,
                                   treat_curr_age_min = 18,
                                   treat_curr_age_max = 45,
                                   ctrl_curr_age_min  = 47,
                                   ctrl_curr_age_max  = 65,
                                   file_label,
                                   caption_label,
                                   output_dir) {
  
  # --- Define subsets ---
  treat_df <- data %>%
    filter(age_at_conflict_start >= treat_age_min,
           age_at_conflict_start <= treat_age_max,
           age >= treat_curr_age_min, age <= treat_curr_age_max)
  
  ctrl_df  <- data %>%
    filter(age_at_conflict_start >= ctrl_age_min,
           age_at_conflict_start <= ctrl_age_max,
           age >= ctrl_curr_age_min, age <= ctrl_curr_age_max)
  
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
    stats <- compute_ttest_stats(treat_df, ctrl_df, var, is_continuous)
    format_ttest_diff(stats, include_pval = FALSE, latex = FALSE)
  }
  
  get_se <- function(var, is_continuous = FALSE) {
    stats <- compute_ttest_stats(treat_df, ctrl_df, var, is_continuous)
    format_ttest_se(stats)
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
  # ---- EDIT (2026-04): Refactored to use shared style_html_table() helper.
  # Previously used inline kable_styling(..., html_font = "Arial", ...) with a
  # dark #2c3e50 header, which differed from other HTML tables in the project.
  # Now uses the shared house style for visual consistency across all tables.
  # Balance-table-specific touches (column widths, add_header_above) stay here.
  html_out <- kable(tbl_plain, format="html", col.names=col_names, align=col_align,
                    caption=paste0("Summary Statistics of Individuals (", caption_label, ")")) %>%
    style_html_table(font_size = 13, fixed_thead = TRUE) %>%
    add_header_above(c(" "=1, "All"=2,
                       setNames(2, treat_label_html),
                       setNames(2, ctrl_label_html), " "=2)) %>%
    row_spec(0,          bold = TRUE) %>%
    row_spec(panel_rows, background = "#f5f5f5", bold = TRUE) %>%
    column_spec(1,   width = "20em") %>%
    column_spec(2:9, width = "6em") %>%
    footnote(general = footnote_text_html, footnote_as_chunk = TRUE)
  
  # ---- EDIT (2026-04): Replaced save_kable() with writeLines() — save_kable
  # for HTML can still invoke webshot2 for rendering checks, which we don't need.
  writeLines(as.character(html_out),
             file.path(output_dir, paste0(file_label, ".Balance_Table.html")))
  
  cat("=== Exported:", file_label, "(.tex / .html) ===\n")
}


# ==============================================================================
# SECTION 10: HIGH vs LOW CONFLICT DiD TABLE GENERATOR ----
# ==============================================================================
# Produces a descriptive table comparing High-Conflict and Low-Conflict districts
# for a specified age cohort (default: 0-17 at conflict onset). Row structure
# mirrors generate_balance_table() exactly (6 panels, 28 variable rows) so the
# two tables can sit side-by-side in the paper. Column structure is simpler:
#   Variable | High Mean | (SD) | Low Mean | (SD) | Diff (H-L) | (SE)
#
# Standard errors in the Diff column come from a district-clustered linear
# regression (via feols) — the correct approach when the exposure variable
# varies only at the district level. This is more defensible than a Welch
# t-test and matches the SE treatment in the main regression pipeline.
# ------------------------------------------------------------------------------

# Clustered-SE helper: fit Y ~ D with district clusters, return diff/se/stars.
# Returns NULL if the regression cannot be fit (e.g. no variation in D).
# `scale` is applied to both the coefficient and SE (use 100 for binary vars
# to report percentage-point differences, 1 for continuous).
compute_clustered_diff_stats <- function(df, outcome_var, exposure_var,
                                         cluster_var = "dist",
                                         is_continuous = FALSE) {
  # Drop rows with missing outcome, exposure, or cluster
  sub <- df[, c(outcome_var, exposure_var, cluster_var)]
  sub <- sub[complete.cases(sub), ]
  
  # Need variation in both outcome and exposure
  if (length(unique(sub[[exposure_var]])) < 2) return(NULL)
  if (nrow(sub) < 10)                          return(NULL)
  
  fml <- as.formula(paste(outcome_var, "~", exposure_var))
  clust_fml <- as.formula(paste("~", cluster_var))
  
  fit <- tryCatch(
    feols(fml, data = sub, cluster = clust_fml, warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  
  est <- tryCatch(summary(fit)$coeftable, error = function(e) NULL)
  if (is.null(est) || !(exposure_var %in% rownames(est))) return(NULL)
  
  scale <- if (is_continuous) 1 else 100
  diff  <- round(est[exposure_var, "Estimate"]   * scale, 2)
  se    <- round(est[exposure_var, "Std. Error"] * scale, 2)
  pval  <- est[exposure_var, "Pr(>|t|)"]
  
  stars <- case_when(
    pval < 0.01 ~ "***",
    pval < 0.05 ~ "**",
    pval < 0.10 ~ "*",
    TRUE        ~ ""
  )
  
  list(diff = diff, se = se, stars = stars, pval = pval)
}

# Format the Diff column — "0.12***" or "" if stats are NULL.
format_clustered_diff <- function(stats) {
  if (is.null(stats)) return("")
  paste0(stats$diff, stats$stars)
}

# Format standard error as "(0.05)" or "" if stats are NULL.
format_clustered_se <- function(stats) {
  if (is.null(stats)) return("")
  paste0("(", stats$se, ")")
}


# -----------------------------------------------------------------------------
# Main function. Mirrors generate_balance_table() but splits on a conflict-
# intensity binary instead of a treat/control cohort.
#
# Arguments:
#   data           : cleaned dataset (typically nlss_conflict_data)
#   treat_age_min  : min age at conflict onset for the sample (default 0)
#   treat_age_max  : max age at conflict onset for the sample (default 17)
#   conflict_var   : name of the High/Low binary (e.g. "high_conflict_q3_binary")
#   conflict_label : short label for the footnote (e.g. "Months of War (Q3)")
#   cluster_var    : cluster variable for SEs (default "dist")
#   file_label     : output filename prefix (e.g. "4a")
#   caption_label  : caption text for the table (e.g. "Age 0-17 at Conflict Start")
#   output_dir     : folder to write .tex and .html
# -----------------------------------------------------------------------------

generate_conflict_did_table <- function(data,
                                        treat_age_min  = 0,
                                        treat_age_max  = 17,
                                        conflict_var   = "high_conflict_q3_binary",
                                        conflict_label = "Months of War (Q3 cutoff)",
                                        cluster_var    = "dist",
                                        file_label,
                                        caption_label,
                                        output_dir) {
  
  # --- Restrict sample to the chosen cohort, require non-missing conflict var ---
  samp <- data %>%
    filter(age_at_conflict_start >= treat_age_min,
           age_at_conflict_start <= treat_age_max,
           !is.na(.data[[conflict_var]]))
  
  high_df <- samp %>% filter(.data[[conflict_var]] == 1)
  low_df  <- samp %>% filter(.data[[conflict_var]] == 0)
  
  cat("=== Conflict DiD Sample Check (Age", treat_age_min, "-", treat_age_max, ") ===\n")
  cat("  Total:         ", nrow(samp),    "\n")
  cat("  High-Conflict: ", nrow(high_df), "\n")
  cat("  Low-Conflict:  ", nrow(low_df),  "\n")
  
  # --- Single-group helpers ---
  get_mean     <- function(df, var) round(mean(df[[var]], na.rm = TRUE), 2)
  get_sd       <- function(df, var) paste0("(", round(sd(df[[var]], na.rm = TRUE), 2), ")")
  get_mean_pct <- function(df, var) round(mean(df[[var]], na.rm = TRUE) * 100, 2)
  get_sd_pct   <- function(df, var) paste0("(", round(sd(df[[var]], na.rm = TRUE) * 100, 2), ")")
  
  # --- Diff + SE from clustered regression on the FULL sample ---
  get_diff <- function(var, is_continuous = FALSE) {
    stats <- compute_clustered_diff_stats(samp, var, conflict_var,
                                          cluster_var   = cluster_var,
                                          is_continuous = is_continuous)
    format_clustered_diff(stats)
  }
  
  get_se <- function(var, is_continuous = FALSE) {
    stats <- compute_clustered_diff_stats(samp, var, conflict_var,
                                          cluster_var   = cluster_var,
                                          is_continuous = is_continuous)
    format_clustered_se(stats)
  }
  
  # --- Row builders (7-column layout: Variable | H Mean | H SD | L Mean | L SD | Diff | SE) ---
  row_cont <- function(label, var) data.frame(
    Variable = label,
    High_Mean = as.character(get_mean(high_df, var)), High_SD = get_sd(high_df, var),
    Low_Mean  = as.character(get_mean(low_df,  var)), Low_SD  = get_sd(low_df,  var),
    Diff_HL = get_diff(var, TRUE), SE = get_se(var, TRUE),
    stringsAsFactors = FALSE)
  
  row_bin <- function(label, var) data.frame(
    Variable = label,
    High_Mean = as.character(get_mean_pct(high_df, var)), High_SD = get_sd_pct(high_df, var),
    Low_Mean  = as.character(get_mean_pct(low_df,  var)), Low_SD  = get_sd_pct(low_df,  var),
    Diff_HL = get_diff(var, FALSE), SE = get_se(var, FALSE),
    stringsAsFactors = FALSE)
  
  row_blank <- function() data.frame(
    Variable="", High_Mean="", High_SD="", Low_Mean="", Low_SD="", Diff_HL="", SE="",
    stringsAsFactors = FALSE)
  
  row_panel <- function(label) data.frame(
    Variable = label, High_Mean="", High_SD="", Low_Mean="", Low_SD="", Diff_HL="", SE="",
    stringsAsFactors = FALSE)
  
  # --- Build table (row content identical to generate_balance_table()) ---
  build_tbl <- function(latex = FALSE) {
    tbl <- bind_rows(
      row_panel("Panel A: Outcomes"),
      row_bin("  International Migration (=1)",  "international_migrant"),
      row_bin("  Currently Abroad (=1)",           "international_absentee_only"),
      row_bin("  Return Migrant (=1)",             "present_ind_migrant"),
      row_bin("  Internal Migration (=1)",         "national"),
      row_blank(),
      row_panel("Panel B: Exposure to Conflict"),
      row_cont("  Months of War (own district)",        "mwar_own_any"),
      row_cont("  Casualties (own district)",            "cas_own_any"),
      row_cont("  Months of War (own district, fatal)",  "mwar_own_fatal"),
      row_cont("  Casualties (own district, fatal)",     "cas_own_fatal"),
      row_blank(),
      row_panel("Panel C: Demographics"),
      row_cont("  Current Age",           "age"),
      row_cont("  Age at Conflict Start", "age_at_conflict_start"),
      row_bin("  Male (=1)",              "male"),
      row_blank(),
      row_panel("Panel D: Education (%)"),
      row_bin("  No Education",      "edu_no_education"),
      row_bin("  Primary (1-5)",     "edu_primary"),
      row_bin("  Secondary (6-12)",  "edu_secondary"),
      row_bin("  Tertiary",          "edu_tertiary"),
      row_blank(),
      row_panel("Panel E: Ethnicity (%)"),
      row_bin("  Hill High Caste", "eth_hill_high"),
      row_bin("  Hill Janajati",   "eth_janajati"),
      row_bin("  Terai/Madhesi",   "eth_terai"),
      row_bin("  Dalit",           "eth_dalit"),
      row_bin("  Muslim",          "eth_muslim"),
      row_blank(),
      row_panel("Panel F: Occupation (%)"),
      row_bin("  Agriculture",             "occ_agriculture"),
      row_bin("  High Skilled",            "occ_high_skilled"),
      row_bin("  Service & Clerical",      "occ_service"),
      row_bin("  Craft & Manufacturing",   "occ_craft"),
      row_bin("  Elementary/Low Skilled",  "occ_elementary"),
      row_bin("  Armed Forces",            "occ_armed"),
      row_blank(),
      data.frame(Variable = "N",
                 High_Mean = as.character(nrow(high_df)), High_SD = "",
                 Low_Mean  = as.character(nrow(low_df)),  Low_SD  = "",
                 Diff_HL = "", SE = "",
                 stringsAsFactors = FALSE)
    )
    if (latex) tbl <- tbl %>% mutate(Variable = sanitize_latex(Variable))
    return(tbl)
  }
  
  tbl_plain <- build_tbl(latex = FALSE)
  tbl_latex <- build_tbl(latex = TRUE)
  
  col_names <- c("Variable",
                 "High Mean", "(SD)",
                 "Low Mean",  "(SD)",
                 "Diff (H-L)", "(SE)")
  col_align <- c("l", "r", "r", "r", "r", "r", "r")
  
  panel_rows <- which(tbl_latex$Variable %in% c(
    "Panel A: Outcomes", "Panel B: Exposure to Conflict",
    "Panel C: Demographics", "Panel D: Education (\\%)",
    "Panel E: Ethnicity (\\%)", "Panel F: Occupation (\\%)"))
  
  footnote_text_latex <- paste(
    paste0("Sample restricted to individuals aged ", treat_age_min, "--",
           treat_age_max, " at conflict start (1996)."),
    paste0("High-Conflict: districts above the 75th percentile of ",
           conflict_label, "; Low-Conflict: at or below (including zeros)."),
    "Standard deviations in parentheses.",
    paste0("Diff (H-L) reports the coefficient from Y = a + b*HighConflict; ",
           "standard errors in parentheses are clustered at the district level."),
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.10.",
    "Source: Nepal Labor Force Survey; conflict data from INSEC."
  )
  
  footnote_text_html <- paste(
    paste0("Sample restricted to individuals aged ", treat_age_min, "-",
           treat_age_max, " at conflict start (1996)."),
    paste0("High-Conflict: districts above the 75th percentile of ",
           conflict_label, "; Low-Conflict: at or below (including zeros)."),
    "Standard deviations in parentheses.",
    paste0("Diff (H-L) reports the coefficient from Y = a + b*HighConflict; ",
           "standard errors in parentheses are clustered at the district level."),
    "*** p<0.01, ** p<0.05, * p<0.10.",
    "Source: Nepal Labor Force Survey; conflict data from INSEC."
  )
  
  # --- LaTeX ---
  latex_out <- kable(tbl_latex, format = "latex", booktabs = TRUE,
                     caption = paste0("High- vs. Low-Conflict Comparison (", caption_label, ")"),
                     label   = paste0("conflict_did_", file_label),
                     col.names = col_names, escape = FALSE, align = col_align) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 9) %>%
    add_header_above(c(" " = 1,
                       "High-Conflict" = 2,
                       "Low-Conflict"  = 2,
                       " " = 2)) %>%
    row_spec(panel_rows, bold = TRUE, italic = TRUE) %>%
    footnote(general = footnote_text_latex, footnote_as_chunk = TRUE,
             escape = FALSE) %>%
    landscape()
  
  writeLines(as.character(latex_out),
             file.path(output_dir, paste0(file_label, ".Conflict_DiD_Table.tex")))
  
  # --- HTML ---
  html_out <- kable(tbl_plain, format = "html", col.names = col_names,
                    align = col_align,
                    caption = paste0("High- vs. Low-Conflict Comparison (",
                                     caption_label, ")")) %>%
    style_html_table(font_size = 13, fixed_thead = TRUE) %>%
    add_header_above(c(" " = 1,
                       "High-Conflict" = 2,
                       "Low-Conflict"  = 2,
                       " " = 2)) %>%
    row_spec(0,          bold = TRUE) %>%
    row_spec(panel_rows, background = "#f5f5f5", bold = TRUE) %>%
    column_spec(1,   width = "20em") %>%
    column_spec(2:7, width = "6em") %>%
    footnote(general = footnote_text_html, footnote_as_chunk = TRUE)
  
  writeLines(as.character(html_out),
             file.path(output_dir, paste0(file_label, ".Conflict_DiD_Table.html")))
  
  cat("=== Exported:", file_label, "Conflict_DiD_Table (.tex / .html) ===\n")
}


# ==============================================================================
# SECTION 11: DiD TABLE WITH TWO CONFLICT MEASURES  ----
# ==============================================================================
# Produces a single DiD table with N panels (one per outcome) and 6 numeric
# columns (High / Low / Difference) × 2 conflict measures. Each panel has
# 3 rows: Treatment cohort, Control cohort, and a Difference row.
#
# The bottom-right cell of each panel × measure combination (cell where
# "Difference" row meets "Difference" column) reports the DiD coefficient
# from feols(Y ~ Treatment * HighConflict | dist + age, cluster = ~dist).

# Design choices:
#   - SEs in row/column means: simple SE of the mean within the cell
#   - SEs in single-difference rows/columns: from a regression of Y on the
#     binary, clustered at district level
#   - SEs in the bottom-right DiD cell: from the interaction-term regression
#   - All SEs use district-level clustering for consistency
# ------------------------------------------------------------------------------

# Helper: SE of the mean within a single group (clustered at district)
# Returns a numeric SE on the same scale as the mean (×100 for binary).
compute_group_mean_se <- function(df, var, cluster_var = "dist", is_continuous = FALSE) {
  sub <- df[, c(var, cluster_var)]
  sub <- sub[complete.cases(sub), ]
  if (nrow(sub) < 5) return(NA_real_)
  
  fit <- tryCatch(
    feols(as.formula(paste(var, "~ 1")), data = sub,
          cluster = as.formula(paste("~", cluster_var)),
          warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NA_real_)
  
  est <- summary(fit)$coeftable
  scale <- if (is_continuous) 1 else 100
  round(est["(Intercept)", "Std. Error"] * scale, 2)
}

# Helper: DiD coefficient from Y ~ T * HighConflict (treats both as binary).
# Returns list with diff, se, stars, pval.
compute_did_interaction <- function(df, outcome_var, treat_var, conflict_var,
                                    cluster_var = "dist", is_continuous = FALSE) {
  sub <- df[, c(outcome_var, treat_var, conflict_var, cluster_var)]
  sub <- sub[complete.cases(sub), ]
  if (nrow(sub) < 20) return(NULL)
  
  # Need variation in both binaries
  if (length(unique(sub[[treat_var]])) < 2)    return(NULL)
  if (length(unique(sub[[conflict_var]])) < 2) return(NULL)
  
  fml <- as.formula(paste0(outcome_var, " ~ ", treat_var, " * ", conflict_var))
  clust_fml <- as.formula(paste("~", cluster_var))
  
  fit <- tryCatch(
    feols(fml, data = sub, cluster = clust_fml, warn = FALSE, notes = FALSE),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  
  interaction_name <- paste0(treat_var, ":", conflict_var)
  est <- tryCatch(summary(fit)$coeftable, error = function(e) NULL)
  if (is.null(est) || !(interaction_name %in% rownames(est))) return(NULL)
  
  scale <- if (is_continuous) 1 else 100
  diff  <- round(est[interaction_name, "Estimate"]   * scale, 2)
  se    <- round(est[interaction_name, "Std. Error"] * scale, 2)
  pval  <- est[interaction_name, "Pr(>|t|)"]
  
  stars <- case_when(
    pval < 0.01 ~ "***",
    pval < 0.05 ~ "**",
    pval < 0.10 ~ "*",
    TRUE        ~ ""
  )
  
  list(diff = diff, se = se, stars = stars, pval = pval)
}


# -----------------------------------------------------------------------------
# Main function. One panel per outcome; two side-by-side column groups, one
# per conflict measure.
# -----------------------------------------------------------------------------

generate_did_two_measures_table <- function(data,
                                            outcome_vars,
                                            outcome_labels,
                                            conflict_vars,
                                            conflict_labels,
                                            treat_var       = "treatment",
                                            cluster_var     = "dist",
                                            file_label,
                                            caption_label,
                                            output_dir) {
  
  stopifnot(length(outcome_vars)  == length(outcome_labels))
  stopifnot(length(conflict_vars) == length(conflict_labels))
  stopifnot(length(conflict_vars) == 2)   # this layout is built for 2 measures
  
  cat("=== Building DiD Two-Measures Table:", file_label, "===\n")
  
  # ---- Helpers tied to the chosen conflict_var ------------------------------
  # For a given conflict_var, return a 3-row data frame:
  #   Row 1: Treatment cohort cells [High, Low, Diff]
  #   Row 2: Control cohort cells   [High, Low, Diff]
  #   Row 3: Difference (T-C) cells [Diff_H, Diff_L, DiD]
  # Each cell is "mean\n(se)" (or LaTeX equivalent).
  build_panel_block <- function(outcome_var, conflict_var, latex = FALSE) {
    
    is_continuous <- !all(stats::na.omit(unique(data[[outcome_var]])) %in% c(0, 1))
    
    # ---- Identify the 4 cohort × district subsets ---------------------------
    treat_high <- data %>% filter(.data[[treat_var]]    == 1, .data[[conflict_var]] == 1)
    treat_low  <- data %>% filter(.data[[treat_var]]    == 1, .data[[conflict_var]] == 0)
    ctrl_high  <- data %>% filter(.data[[treat_var]]    == 0, .data[[conflict_var]] == 1)
    ctrl_low   <- data %>% filter(.data[[treat_var]]    == 0, .data[[conflict_var]] == 0)
    
    treat_all  <- data %>% filter(.data[[treat_var]] == 1)
    ctrl_all   <- data %>% filter(.data[[treat_var]] == 0)
    high_all   <- data %>% filter(.data[[conflict_var]] == 1)
    low_all    <- data %>% filter(.data[[conflict_var]] == 0)
    
    # ---- Scale ----
    scale <- if (is_continuous) 1 else 100
    fmt   <- function(v) if (is.na(v)) "" else sprintf("%.2f", v)
    
    # ---- Means ----
    m <- function(df) {
      v <- mean(df[[outcome_var]], na.rm = TRUE) * scale
      round(v, 2)
    }
    m_treat_high <- m(treat_high); m_treat_low <- m(treat_low)
    m_ctrl_high  <- m(ctrl_high);  m_ctrl_low  <- m(ctrl_low)
    
    # ---- SEs of group means (clustered) ----
    se_cell <- function(df) compute_group_mean_se(df, outcome_var, cluster_var, is_continuous)
    se_treat_high <- se_cell(treat_high); se_treat_low <- se_cell(treat_low)
    se_ctrl_high  <- se_cell(ctrl_high);  se_ctrl_low  <- se_cell(ctrl_low)
    
    # ---- Single differences (within-cohort H-L; within-district T-C) ----
    # Within-cohort H-L: regress Y on HighConflict using only that cohort
    diff_treat_HL <- compute_clustered_diff_stats(
      treat_all, outcome_var, conflict_var, cluster_var, is_continuous)
    diff_ctrl_HL  <- compute_clustered_diff_stats(
      ctrl_all,  outcome_var, conflict_var, cluster_var, is_continuous)
    # Within-district T-C: regress Y on Treatment using only that district group
    diff_high_TC  <- compute_clustered_diff_stats(
      high_all,  outcome_var, treat_var,    cluster_var, is_continuous)
    diff_low_TC   <- compute_clustered_diff_stats(
      low_all,   outcome_var, treat_var,    cluster_var, is_continuous)
    
    # ---- DiD (interaction) ----
    did_stats <- compute_did_interaction(
      data, outcome_var, treat_var, conflict_var, cluster_var, is_continuous)
    
    # ---- Format cells: mean over (SE) ----
    # latex = TRUE uses \makecell to stack mean and SE
    fmt_cell <- function(mean_val, se_val) {
      if (is.na(mean_val)) return("")
      mean_str <- fmt(mean_val)
      se_str   <- if (is.na(se_val)) "" else paste0("(", fmt(se_val), ")")
      if (latex) {
        paste0("\\makecell[c]{", mean_str, " \\\\ ", se_str, "}")
      } else {
        paste0(mean_str, " ", se_str)   # plain text — single line
      }
    }
    
    fmt_diff <- function(stats) {
      if (is.null(stats)) return("")
      mean_str <- paste0(fmt(stats$diff), stats$stars)
      se_str   <- paste0("(", fmt(stats$se), ")")
      if (latex) {
        paste0("\\makecell[c]{", mean_str, " \\\\ ", se_str, "}")
      } else {
        paste0(mean_str, " ", se_str)
      }
    }
    
    # ---- Build 3 rows × 3 columns ----
    data.frame(
      High = c(fmt_cell(m_treat_high, se_treat_high),
               fmt_cell(m_ctrl_high,  se_ctrl_high),
               fmt_diff(diff_high_TC)),
      Low  = c(fmt_cell(m_treat_low,  se_treat_low),
               fmt_cell(m_ctrl_low,   se_ctrl_low),
               fmt_diff(diff_low_TC)),
      Diff = c(fmt_diff(diff_treat_HL),
               fmt_diff(diff_ctrl_HL),
               fmt_diff(did_stats)),
      stringsAsFactors = FALSE
    )
  }
  
  # Row labels for every panel (3 rows)
  row_labels <- c("  Aged 0-17 in 1996 (Treatment)",
                  "  Aged 18-40 in 1996 (Control)",
                  "  Difference")
  
  # Build the full table — one panel header + 3 rows × N outcomes
  build_full_table <- function(latex = FALSE) {
    panel_letters <- LETTERS[1:length(outcome_vars)]
    
    rows <- list()
    for (i in seq_along(outcome_vars)) {
      panel_label <- paste0("Panel ", panel_letters[i], ": ", outcome_labels[i])
      
      # Panel header row (spans all 6 numeric columns visually; we put empty cells)
      header_row <- data.frame(
        Variable = panel_label,
        H1 = "", L1 = "", D1 = "",
        H2 = "", L2 = "", D2 = "",
        stringsAsFactors = FALSE
      )
      
      # Build the two 3×3 panel blocks (one per conflict measure)
      blk1 <- build_panel_block(outcome_vars[i], conflict_vars[1], latex = latex)
      blk2 <- build_panel_block(outcome_vars[i], conflict_vars[2], latex = latex)
      
      # Stitch into a 3-row data frame with label column
      panel_body <- data.frame(
        Variable = row_labels,
        H1 = blk1$High, L1 = blk1$Low, D1 = blk1$Diff,
        H2 = blk2$High, L2 = blk2$Low, D2 = blk2$Diff,
        stringsAsFactors = FALSE
      )
      
      rows[[length(rows) + 1]] <- header_row
      rows[[length(rows) + 1]] <- panel_body
    }
    
    tbl <- do.call(rbind, rows)
    if (latex) tbl <- tbl %>% mutate(Variable = sanitize_latex(Variable))
    tbl
  }
  
  tbl_plain <- build_full_table(latex = FALSE)
  tbl_latex <- build_full_table(latex = TRUE)
  
  col_names <- c("Variable",
                 "High", "Low", "Difference",
                 "High", "Low", "Difference")
  col_align <- c("l", "c", "c", "c", "c", "c", "c")
  
  # Identify panel header rows for bold formatting
  panel_rows <- which(grepl("^Panel [A-Z]:", tbl_latex$Variable))
  
  footnote_text_latex <- c(
    paste0("Treatment cohort: aged 0--17 at conflict onset (1996), currently 18--45 in NLSS 2017/18."),
    paste0("Control cohort: aged 18--40 at conflict onset, currently 47--65."),
    paste0(conflict_labels[1], ": districts above the 75th percentile of months with any violent incident, 1996--2006."),
    paste0(conflict_labels[2], ": districts above the 75th percentile of total casualties, 1996--2006."),
    "Cell entries: mean above standard error in parentheses; standard errors clustered at the district level.",
    paste0("The bottom-right cell of each panel reports $\\beta_3$ from ",
           "$Y = \\alpha + \\beta_1 \\cdot \\text{Treatment} + ",
           "\\beta_2 \\cdot \\text{HighConflict} + ",
           "\\beta_3 \\cdot (\\text{Treatment} \\times \\text{HighConflict})$."),
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.10.",
    "Source: Nepal Labour Force Survey 2017/18; conflict data from INSEC."
  )
  
  footnote_text_html <- c(
    "Treatment cohort: aged 0-17 at conflict onset (1996), currently 18-45 in NLSS 2017/18.",
    "Control cohort: aged 18-40 at conflict onset, currently 47-65.",
    paste0(conflict_labels[1], ": districts above the 75th percentile of months with any violent incident, 1996-2006."),
    paste0(conflict_labels[2], ": districts above the 75th percentile of total casualties, 1996-2006."),
    "Cell entries: mean above standard error in parentheses; standard errors clustered at the district level.",
    paste0("The bottom-right cell of each panel reports the interaction coefficient from ",
           "Y = a + b\u2081 \u00D7 Treatment + b\u2082 \u00D7 HighConflict + b\u2083 \u00D7 (Treatment \u00D7 HighConflict)."),
    "*** p<0.01, ** p<0.05, * p<0.10.",
    "Source: Nepal Labour Force Survey 2017/18; conflict data from INSEC."
  )
  
  # --- LaTeX ---
  latex_out <- kable(tbl_latex, format = "latex", booktabs = TRUE,
                     caption   = paste0("Means of migration outcomes by cohort and district conflict intensity (",
                                        caption_label, ")"),
                     label     = paste0("did_two_measures_", file_label),
                     col.names = col_names, escape = FALSE, align = col_align) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"), font_size = 9) %>%
    add_header_above(c(" " = 1,
                       setNames(3, conflict_labels[1]),
                       setNames(3, conflict_labels[2]))) %>%
    row_spec(panel_rows, bold = TRUE, italic = TRUE) %>%
    footnote(general = footnote_text_latex, general_title = "Notes:",
             footnote_as_chunk = FALSE, threeparttable = TRUE, escape = FALSE) %>%
    landscape()
  
  writeLines(as.character(latex_out),
             file.path(output_dir, paste0(file_label, ".DiD_Two_Measures.tex")))
  
  # --- HTML ---
  html_out <- kable(tbl_plain, format = "html", col.names = col_names,
                    align = col_align,
                    caption = paste0("Means of migration outcomes by cohort and district conflict intensity (",
                                     caption_label, ")")) %>%
    style_html_table(font_size = 13, fixed_thead = TRUE) %>%
    add_header_above(c(" " = 1,
                       setNames(3, conflict_labels[1]),
                       setNames(3, conflict_labels[2]))) %>%
    row_spec(0,          bold = TRUE) %>%
    row_spec(panel_rows, background = "#f5f5f5", bold = TRUE) %>%
    column_spec(1,   width = "16em",
                extra_css = "padding-right: 1em;",
                include_thead = TRUE) %>%
    column_spec(2:7, width = "8em") %>%
    footnote(general = footnote_text_html, general_title = "Notes:",
             footnote_as_chunk = FALSE)
  
  writeLines(as.character(html_out),
             file.path(output_dir, paste0(file_label, ".DiD_Two_Measures.html")))
  
  cat("=== Exported:", file_label, "DiD_Two_Measures (.tex / .html) ===\n")
}


# ==============================================================================
# SECTION 12: TABLES REGISTRY (for auto-generating tables.js)----
# ==============================================================================
# Populated by register_table() calls inside each table script, right after
# writeLines(...) for the HTML output. At the end of 00_master.R, we write
# tables.js using this registry. This drives the index.html and viewer.html
# navigation on the GitHub Pages site.
# ------------------------------------------------------------------------------

tables_registry <- data.frame(
  section    = character(),
  subsection = character(),   # ---- EDIT (2026-04): optional sub-heading below section
  title      = character(),
  file       = character(),   # path relative to Paper/Tables/
  stringsAsFactors = FALSE
)

# Helper: call RIGHT AFTER writeLines() for each HTML table.
# subsection is optional; if omitted, the entry appears directly under the section.
# Example:
#   register_table(
#     section    = "Difference Tables",
#     subsection = "Based on Month of War",
#     title      = "Table 4a — High vs Low Conflict (Age 0-17)",
#     file       = "Tables_Summary/4a.Conflict_DiD_Table.html"
#   )
register_table <- function(section, title, file, subsection = "") {
  tables_registry <<- rbind(
    tables_registry,
    data.frame(section    = section,
               subsection = subsection,
               title      = title,
               file       = file,
               stringsAsFactors = FALSE)
  )
}

# ==============================================================================
# SECTION 12: TABLES REGISTRY (for auto-generating tables.js)----
# ==============================================================================
# Populated by register_table() calls inside each table script, right after
# writeLines(...) for the HTML output. At the end of 00_master.R, we write
# tables.js using this registry. This drives the index.html and viewer.html
# navigation on the GitHub Pages site.
# ------------------------------------------------------------------------------

tables_registry <- data.frame(
  section = character(),
  subsection = character(),
  title   = character(),
  file    = character(),   # path relative to Paper/Tables/
  stringsAsFactors = FALSE
)

# Helper: call RIGHT AFTER writeLines() for each HTML table.
# Example:
#   register_table(
#     section = "Summary Tables",
#     title   = "Table 1 — Descriptive Statistics",
#     file    = "Tables_Summary/1.Overall_Summary.html"
#   )
register_table <- function(section, title, file, subsection = "") {
  tables_registry <<- rbind(
    tables_registry,
    data.frame(section = section, subsection = subsection, title = title, file = file,
               stringsAsFactors = FALSE)
  )
}