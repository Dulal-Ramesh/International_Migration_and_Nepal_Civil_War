# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Summary Statistics - Table 4: Covariate Summary by Migration Group
# Last Updated  : April 2026
# ==============================================================================

# REQUIRES: 00_master.R paths, 01_setup.R functions, 02_data_cleaning.R dataset
# OUTPUTS : tables_summary/4.Multigroup_Summary.tex / .html
#
# PURPOSE:
# Side-by-side descriptive statistics across five migration groups drawn from
# the FULL cleaned sample (nlss_conflict_data, not the analysis sample). Lets
# the reader see how migrants and non-migrants differ on observables, and
# how the three absentee subtypes (international, internal, returnees) compare.
#
# Groups are NOT mutually exclusive: "Total Absent" contains the intersection
# of "Intl. Absentee" and "National Absent" subgroups. This is noted in the
# table footnote.


# ==============================================================================
# SECTION 1: COMPUTE GROUP STATISTICS
# ==============================================================================

stats_baseline <- nlss_conflict_data %>% filter(baseline == 1)                   %>% compute_group_stats()
stats_absent   <- nlss_conflict_data %>% filter(absent == 1)                     %>% compute_group_stats()
stats_intl     <- nlss_conflict_data %>% filter(international_absentee_only == 1) %>% compute_group_stats()
stats_national <- nlss_conflict_data %>% filter(national == 1)                   %>% compute_group_stats()
stats_returnee <- nlss_conflict_data %>% filter(present_ind_migrant == 1)        %>% compute_group_stats()

# Shortcut helpers — each returns a value-pulling closure for build_covariate_table()
baseline_val <- function(col) g(stats_baseline, col)
absent_val   <- function(col) g(stats_absent,   col)
intl_val     <- function(col) g(stats_intl,     col)
national_val <- function(col) g(stats_national, col)
returnee_val <- function(col) g(stats_returnee, col)


# ==============================================================================
# SECTION 2: FORMAT TABLE
# ==============================================================================

groups_list <- list(
  "Baseline"        = baseline_val,
  "Total_Absent"    = absent_val,
  "Intl_Absentee"   = intl_val,
  "National_Absent" = national_val,
  "Returnees"       = returnee_val
)

# Plain version — for HTML
table_multigroup_plain <- build_covariate_table(groups_list)

# LaTeX version — SD stacked on new line via \makecell, special chars escaped
table_multigroup_latex <- build_covariate_table(groups_list, latex = TRUE) %>%
  mutate(Variable = sanitize_latex(Variable))


# ==============================================================================
# SECTION 3: EXPORT OUTPUTS
# ==============================================================================

col_names <- c("Variable", "Baseline", "Total Absent",
               "Intl. Absentee", "National Absent", "Returnees")
col_align <- c("l", "c", "c", "c", "c", "c")

# Shared footnote text — split into separate bullets for readability.
notes_common <- c(
  "Standard deviations in parentheses for continuous variables.",
  "Groups are NOT mutually exclusive: Total Absent contains the Intl. Absentee and National Absent subgroups.",
  "Baseline: present at survey and never migrated internationally.",
  "Total Absent: all absent individuals (abroad + internal).",
  "Intl. Absentee: absent and currently abroad.",
  "National Absent: absent and inside Nepal.",
  "Returnees: present at survey but was abroad for work >= 3 months.",
  "Occupation percentages exclude 2,334 observations coded 'Not Stated' (NLSS code 9999) and 1,726 absent household members whose occupation was not reported.",
  "Source: Nepal Labour Force Survey 2017/18; conflict exposure data from INSEC (1996-2006)."
)

# LaTeX-flavored footnote (math mode for >=)
notes_latex <- notes_common
notes_latex[7] <- "Returnees: present at survey but was abroad for work $\\geq$3 months."


# --- LaTeX (landscape) ---
latex_multigroup <- kable(table_multigroup_latex,
                          format    = "latex",
                          booktabs  = TRUE,
                          caption   = "Covariate Summary by Migration Group",
                          label     = "tab:multigroup_summary",
                          col.names = col_names,
                          escape    = FALSE,
                          align     = col_align) %>%
  kable_styling(latex_options = c("hold_position", "scale_down"),
                font_size     = 9) %>%
  footnote(general           = notes_latex,
           general_title     = "Notes:",
           footnote_as_chunk = FALSE,
           threeparttable    = TRUE,
           escape            = FALSE) %>%
  landscape()

writeLines(as.character(latex_multigroup),
           file.path(tables_summary, "3.Multigroup_Summary.tex"))


# --- HTML ---
html_multigroup <- kable(table_multigroup_plain,
                         format    = "html",
                         col.names = col_names,
                         caption   = "Covariate Summary by Migration Group",
                         align     = col_align) %>%
  style_html_table(font_size = 13) %>%
  column_spec(1, width = "20em",
              extra_css = "padding-right: 1.5em;",
              include_thead = TRUE) %>%
  column_spec(2:6, width = "9em") %>%
  footnote(general           = notes_common,
           general_title     = "Notes:",
           footnote_as_chunk = FALSE)

writeLines(as.character(html_multigroup),
           file.path(tables_summary, "3.Multigroup_Summary.html"))


cat("\n=== Exported 4.Multigroup_Summary (.tex / .html) ===\n")