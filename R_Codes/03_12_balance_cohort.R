# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Balance Table by Treatment Cohort vs Control (Age 18-40)
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R dataset
# OUTPUTS : tables_summary/12a.Balance_Table_Cohort_0_5.tex
#           tables_summary/12b.Balance_Table_Cohort_6_12.tex
#           tables_summary/12c.Balance_Table_Cohort_13_17.tex


# ==============================================================================
# SECTION 1: DEFINE SUBSETS -----------------------------------------------
# ==============================================================================

# Control group — same for all three tables
ctrl_df <- nlss_conflict_data %>% filter(treatment == 0)

# Treatment cohorts
treat_0_5   <- nlss_conflict_data %>%
  filter(cohort_group == "Treatment: Age 0-5 in 1996")

treat_6_12  <- nlss_conflict_data %>%
  filter(cohort_group == "Treatment: Age 6-12 in 1996")

treat_13_17 <- nlss_conflict_data %>%
  filter(cohort_group == "Treatment: Age 13-17 in 1996")

# All = each cohort + control combined
all_0_5   <- bind_rows(treat_0_5,   ctrl_df)
all_6_12  <- bind_rows(treat_6_12,  ctrl_df)
all_13_17 <- bind_rows(treat_13_17, ctrl_df)

cat("=== Group Size Check ===\n")
cat("Treatment Age 0-5:   ", nrow(treat_0_5),   "\n")
cat("Treatment Age 6-12:  ", nrow(treat_6_12),  "\n")
cat("Treatment Age 13-17: ", nrow(treat_13_17), "\n")
cat("Control (Age 18-40): ", nrow(ctrl_df),     "\n")


# ==============================================================================
# SECTION 2: HELPER FUNCTIONS -----------------------------------------------
# ==============================================================================

# Mean for continuous variable
get_mean_c <- function(df, var) {
  round(mean(df[[var]], na.rm = TRUE), 2)
}

# SD in brackets for continuous variable
get_sd_c <- function(df, var) {
  paste0("[", round(sd(df[[var]], na.rm = TRUE), 2), "]")
}

# Mean for binary variable — proportions
get_mean_b <- function(df, var) {
  round(mean(df[[var]], na.rm = TRUE), 2)
}

# SD in brackets for binary variable — proportions
get_sd_b <- function(df, var) {
  paste0("[", round(sd(df[[var]], na.rm = TRUE), 2), "]")
}

# Mean for percentage variable (education, ethnicity, occupation)
get_mean_pct <- function(df, var) {
  round(mean(df[[var]], na.rm = TRUE) * 100, 2)
}

# SD in brackets for percentage variable
get_sd_pct <- function(df, var) {
  paste0("[", round(sd(df[[var]], na.rm = TRUE) * 100, 2), "]")
}

# Diff (T-C) with stars
get_diff <- function(treat_df, var, is_pct = FALSE) {
  t_vals <- treat_df[[var]][!is.na(treat_df[[var]])]
  c_vals <- ctrl_df[[var]][!is.na(ctrl_df[[var]])]
  
  if (length(t_vals) < 2 | length(c_vals) < 2) return("")
  
  test <- t.test(t_vals, c_vals, var.equal = FALSE)
  
  diff <- if (is_pct) {
    round((mean(t_vals) - mean(c_vals)) * 100, 2)
  } else {
    round(mean(t_vals) - mean(c_vals), 2)
  }
  
  stars <- case_when(
    test$p.value < 0.01 ~ "***",
    test$p.value < 0.05 ~ "**",
    test$p.value < 0.10 ~ "*",
    TRUE                ~ ""
  )
  
  paste0(diff, stars)
}

# SE of the difference
get_se <- function(treat_df, var, is_pct = FALSE) {
  t_vals <- treat_df[[var]][!is.na(treat_df[[var]])]
  c_vals <- ctrl_df[[var]][!is.na(ctrl_df[[var]])]
  
  if (length(t_vals) < 2 | length(c_vals) < 2) return("")
  
  se <- sqrt(var(t_vals) / length(t_vals) + var(c_vals) / length(c_vals))
  if (is_pct) se <- se * 100
  
  paste0("(", round(se, 2), ")")
}


# ==============================================================================
# SECTION 3: BUILD TABLE FUNCTION -------------------------------------------
# ==============================================================================

build_cohort_table <- function(treat_df, all_df, latex = FALSE) {
  
  # Row builders
  row_cont <- function(label, var) {
    data.frame(
      Variable     = label,
      All_Mean     = as.character(get_mean_c(all_df,   var)),
      All_SD       = get_sd_c(all_df,   var),
      Treat_Mean   = as.character(get_mean_c(treat_df, var)),
      Treat_SD     = get_sd_c(treat_df, var),
      Control_Mean = as.character(get_mean_c(ctrl_df,  var)),
      Control_SD   = get_sd_c(ctrl_df,  var),
      Diff_TC      = get_diff(treat_df, var, is_pct = FALSE),
      SE           = get_se(treat_df,   var, is_pct = FALSE),
      stringsAsFactors = FALSE
    )
  }
  
  row_bin <- function(label, var) {
    data.frame(
      Variable     = label,
      All_Mean     = as.character(get_mean_b(all_df,   var)),
      All_SD       = get_sd_b(all_df,   var),
      Treat_Mean   = as.character(get_mean_b(treat_df, var)),
      Treat_SD     = get_sd_b(treat_df, var),
      Control_Mean = as.character(get_mean_b(ctrl_df,  var)),
      Control_SD   = get_sd_b(ctrl_df,  var),
      Diff_TC      = get_diff(treat_df, var, is_pct = FALSE),
      SE           = get_se(treat_df,   var, is_pct = FALSE),
      stringsAsFactors = FALSE
    )
  }
  
  row_pct <- function(label, var) {
    data.frame(
      Variable     = label,
      All_Mean     = as.character(get_mean_pct(all_df,   var)),
      All_SD       = get_sd_pct(all_df,   var),
      Treat_Mean   = as.character(get_mean_pct(treat_df, var)),
      Treat_SD     = get_sd_pct(treat_df, var),
      Control_Mean = as.character(get_mean_pct(ctrl_df,  var)),
      Control_SD   = get_sd_pct(ctrl_df,  var),
      Diff_TC      = get_diff(treat_df, var, is_pct = TRUE),
      SE           = get_se(treat_df,   var, is_pct = TRUE),
      stringsAsFactors = FALSE
    )
  }
  
  row_blank <- function() {
    data.frame(Variable = "", All_Mean = "", All_SD = "",
               Treat_Mean = "", Treat_SD = "", Control_Mean = "",
               Control_SD = "", Diff_TC = "", SE = "",
               stringsAsFactors = FALSE)
  }
  
  row_panel <- function(label) {
    data.frame(Variable = label, All_Mean = "", All_SD = "",
               Treat_Mean = "", Treat_SD = "", Control_Mean = "",
               Control_SD = "", Diff_TC = "", SE = "",
               stringsAsFactors = FALSE)
  }
  
  # Assemble table — plain text labels
  tbl <- bind_rows(
    
    # Panel A: Outcomes
    row_panel("Panel A: Outcomes"),
    row_bin("  International Migration (=1)", "international_migrant"),
    row_bin("  Currently Abroad (=1)",         "international_absentee_only"),
    row_bin("  Return Migrant (=1)",            "present_ind_migrant"),
    row_bin("  Internal Migration (=1)",        "national"),
    row_blank(),
    
    # Panel B: Conflict Exposure
    row_panel("Panel B: Exposure to Conflict"),
    row_cont("  Months of War (own district)",       "mwar_own_any"),
    row_cont("  Casualties (own district)",           "cas_own_any"),
    row_cont("  Months of War (own district, fatal)", "mwar_own_fatal"),
    row_cont("  Casualties (own district, fatal)",    "cas_own_fatal"),
    row_blank(),
    
    # Panel C: Controls
    row_panel("Panel C: Controls"),
    row_cont("  Current Age",           "age"),
    row_bin("  Male (=1)",              "male"),
    row_cont("  Years of Education",    "grade_comp"),
    row_blank(),
    
    # Panel D: Education
    row_panel("Panel D: Education (%)"),
    row_pct("  No Education",     "edu_no_education"),
    row_pct("  Primary (1-5)",    "edu_primary"),
    row_pct("  Secondary (6-12)", "edu_secondary"),
    row_pct("  Tertiary",         "edu_tertiary"),
    row_blank(),
    
    # Panel E: Ethnicity
    row_panel("Panel E: Ethnicity (%)"),
    row_pct("  Hill High Caste", "eth_hill_high"),
    row_pct("  Hill Janajati",   "eth_janajati"),
    row_pct("  Terai/Madhesi",   "eth_terai"),
    row_pct("  Dalit",           "eth_dalit"),
    row_pct("  Muslim",          "eth_muslim"),
    row_blank(),
    
    # Panel F: Occupation
    row_panel("Panel F: Occupation (%)"),
    row_pct("  Agriculture",            "occ_agriculture"),
    row_pct("  High Skilled",           "occ_high_skilled"),
    row_pct("  Service & Clerical",     "occ_service"),
    row_pct("  Craft & Manufacturing",  "occ_craft"),
    row_pct("  Elementary/Low Skilled", "occ_elementary"),
    row_pct("  Armed Forces",           "occ_armed"),
    row_blank(),
    
    # N at bottom
    data.frame(
      Variable     = "N",
      All_Mean     = as.character(nrow(all_df)),
      All_SD       = "",
      Treat_Mean   = as.character(nrow(treat_df)),
      Treat_SD     = "",
      Control_Mean = as.character(nrow(ctrl_df)),
      Control_SD   = "",
      Diff_TC      = "",
      SE           = "",
      stringsAsFactors = FALSE
    )
  )
  
  # Apply sanitize_latex() and panel bold formatting for LaTeX
  if (latex) {
    tbl <- tbl %>%
      mutate(Variable = sanitize_latex(Variable)) %>%
      mutate(Variable = case_when(
        Variable == "Panel A: Outcomes"               ~ "\\textbf{Panel A: Outcomes}",
        Variable == "Panel B: Exposure to Conflict"   ~ "\\textbf{Panel B: Exposure to Conflict}",
        Variable == "Panel C: Controls"               ~ "\\textbf{Panel C: Controls}",
        Variable == "Panel D: Education (\\%)"        ~ "\\textbf{Panel D: Education (\\%)}",
        Variable == "Panel E: Ethnicity (\\%)"        ~ "\\textbf{Panel E: Ethnicity (\\%)}",
        Variable == "Panel F: Occupation (\\%)"       ~ "\\textbf{Panel F: Occupation (\\%)}",
        TRUE                                          ~ Variable
      ))
  }
  
  return(tbl)
}


# ==============================================================================
# SECTION 4: BUILD ALL THREE TABLES -----------------------------------------
# ==============================================================================

# Plain versions — for Markdown
table_0_5_formatted   <- build_cohort_table(treat_0_5,   all_0_5,   latex = FALSE)
table_6_12_formatted  <- build_cohort_table(treat_6_12,  all_6_12,  latex = FALSE)
table_13_17_formatted <- build_cohort_table(treat_13_17, all_13_17, latex = FALSE)

# LaTeX versions
table_0_5_latex   <- build_cohort_table(treat_0_5,   all_0_5,   latex = TRUE)
table_6_12_latex  <- build_cohort_table(treat_6_12,  all_6_12,  latex = TRUE)
table_13_17_latex <- build_cohort_table(treat_13_17, all_13_17, latex = TRUE)


# ==============================================================================
# SECTION 5: EXPORT FUNCTION ------------------------------------------------
# ==============================================================================

export_cohort_table <- function(tbl_latex, tbl_md, cohort_label,
                                treat_label, file_prefix) {
  
  # --- LaTeX ---
  latex_out <- kable(tbl_latex,
                     format    = "latex",
                     booktabs  = TRUE,
                     caption   = paste0("Summary Statistics of Individuals (",
                                        cohort_label, ")"),
                     label     = paste0("balance_", file_prefix),
                     col.names = c("Variable",
                                   "All Mean", "[SD]",
                                   "Treatment Mean", "[SD]",
                                   "Control Mean",   "[SD]",
                                   "Diff (T-C)", "(SE)"),
                     escape    = FALSE,
                     align     = c("l", "r", "r", "r", "r", "r", "r", "r", "r")) %>%
    kable_styling(
      latex_options = c("hold_position", "scale_down"),
      font_size     = 9
    ) %>%
    add_header_above(c(" "                         = 1,
                       "All"                       = 2,
                       paste0(treat_label)         = 2,
                       "Control (Age 18--40)"      = 2,
                       " "                         = 2)) %>%
    footnote(
      general = paste(
        paste0("Treatment = ", cohort_label, "."),
        "Control = aged 18--40 at conflict start (1996).",
        "Standard deviations in brackets.",
        "Standard errors of the difference in parentheses in SE column.",
        "Education, ethnicity, and occupation percentages conditional on non-missing values.",
        "Binary outcomes (Panel A and C) reported as proportions.",
        "*** p$<$0.01, ** p$<$0.05, * p$<$0.10.",
        "Source: Nepal Labor Force Survey; conflict data from INSEC."
      ),
      footnote_as_chunk = TRUE,
      escape            = FALSE
    )
  
  writeLines(as.character(latex_out),
             file.path(tables_summary,
                       paste0("12", file_prefix, ".Balance_Table_Cohort_",
                              gsub(" ", "_", treat_label), ".tex")))
  
  # --- Markdown ---
  md_out <- kable(tbl_md,
                  format    = "markdown",
                  col.names = c("Variable",
                                "All Mean", "[SD]",
                                "Treatment Mean", "[SD]",
                                "Control Mean",   "[SD]",
                                "Diff (T-C)", "(SE)"),
                  align     = c("l", "r", "r", "r", "r", "r", "r", "r", "r"))
  
  writeLines(
    c(md_out, "",
      "*Notes:*",
      paste0("- Treatment = ", cohort_label, "."),
      "- Control = aged 18-40 at conflict start (1996).",
      "- Standard deviations in brackets.",
      "- Binary outcomes (Panel A and C) reported as proportions.",
      "- *** p<0.01, ** p<0.05, * p<0.10.",
      "- Source: Nepal Labor Force Survey; conflict data from INSEC."),
    file.path(tables_summary,
              paste0("12", file_prefix, ".Balance_Table_Cohort_",
                     gsub(" ", "_", treat_label), ".md"))
  )
  
  cat("✓ Saved:", file_prefix, "-", cohort_label, "\n")
}


# ==============================================================================
# SECTION 6: EXPORT ALL THREE TABLES ----------------------------------------
# ==============================================================================

export_cohort_table(
  tbl_latex    = table_0_5_latex,
  tbl_md       = table_0_5_formatted,
  cohort_label = "Treatment: Age 0-5 at Conflict Start (1996)",
  treat_label  = "Treatment (Age 0--5)",
  file_prefix  = "a"
)

export_cohort_table(
  tbl_latex    = table_6_12_latex,
  tbl_md       = table_6_12_formatted,
  cohort_label = "Treatment: Age 6-12 at Conflict Start (1996)",
  treat_label  = "Treatment (Age 6--12)",
  file_prefix  = "b"
)

export_cohort_table(
  tbl_latex    = table_13_17_latex,
  tbl_md       = table_13_17_formatted,
  cohort_label = "Treatment: Age 13-17 at Conflict Start (1996)",
  treat_label  = "Treatment (Age 13--17)",
  file_prefix  = "c"
)