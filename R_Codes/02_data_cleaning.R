# ==============================================================================
# Project Title : Nepal Civil Conflict and International Migration
# Author        : Ramesh Dulal
# Description   : Data Import, Cleaning, and Variable Creation
# Last Updated  : April 2026
# ==============================================================================

# CREATES:
# - nlss_conflict_data: Main analysis dataset with all variables


# ==============================================================================
# SECTION 1: IMPORT DATA
# ==============================================================================

nlss_conflict_data <- read_dta(file.path(modified_data, "1_conflict_present_absentee_data.dta"))


# ==============================================================================
# SECTION 2: REORDER VARIABLES FOR CLARITY
# ==============================================================================

nlss_conflict_data <- nlss_conflict_data %>%
  select(
    # Identifiers
    psu, hhld, personid, id, sn, season,
    
    # Geographic
    dist, district_name_std, vdcmun, ward,
    
    # Conflict measures
    incident_district_num,
    mwar_own_any, mwar_own_fatal, cas_own_any, cas_own_fatal,
    mwar_nbr_any, mwar_nbr_fatal, cas_nbr_any, cas_nbr_fatal,
    
    # Demographics
    sex, age, rel_hhh, caste, marital,
    
    # Migration outcomes
    international_migrant, international_absentee_only, present_ind_migrant, national,
    occupation_types, absent, baseline, travelled5, rsn_travel, abs_rsn,
    abs_nummonth, abs_living, abs_id,
    
    # Education
    grade_comp, can_read, can_write, current_school, ever_school,
    
    # Everything else
    everything()
  )


# ==============================================================================
# SECTION 3: HANDLE MISSING VALUES
# ==============================================================================

# Drop observations with missing district
nlss_conflict_data <- nlss_conflict_data %>%
  drop_na(dist)

# Fill missing conflict values with district-level values
nlss_conflict_data <- nlss_conflict_data %>%
  group_by(dist) %>%
  mutate(
    mwar_own_any   = first(na.omit(mwar_own_any)),
    mwar_own_fatal = first(na.omit(mwar_own_fatal)),
    cas_own_any    = first(na.omit(cas_own_any)),
    cas_own_fatal  = first(na.omit(cas_own_fatal)),
    mwar_nbr_any   = first(na.omit(mwar_nbr_any)),
    mwar_nbr_fatal = first(na.omit(mwar_nbr_fatal)),
    cas_nbr_any    = first(na.omit(cas_nbr_any)),
    cas_nbr_fatal  = first(na.omit(cas_nbr_fatal))
  ) %>%
  ungroup()

# Assign absentees the same caste as household
nlss_conflict_data <- nlss_conflict_data %>%
  group_by(psu, hhld) %>%
  mutate(caste = first(na.omit(caste))) %>%
  ungroup()

# Create value labels
nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    
    # Absent
    absent_label = factor(ifelse(absent == 1, "Absent", "Non-Absent"),
                          levels = c("Non-Absent", "Absent")),
    
    # International migrant
    migrant_label = factor(ifelse(international_migrant == 1, "Migrant", "Non-Migrant"),
                           levels = c("Non-Migrant", "Migrant")),
    
    # International absentee only
    international_absentee_only_label = factor(
      ifelse(international_absentee_only == 1, "International Absentee", "Non-International Absentee"),
      levels = c("Non-International Absentee", "International Absentee")),
    
    # Present individual migrant
    present_ind_migrant_label = factor(
      ifelse(present_ind_migrant == 1, "Respondent Migrant", "Respondent Non-Migrant"),
      levels = c("Respondent Non-Migrant", "Respondent Migrant")),
    
    # Internal migrant
    national_migrant_label = factor(ifelse(national == 1, "Internal Migrant", "Non-Internal Migrant"),
                                    levels = c("Non-Internal Migrant", "Internal Migrant")),
    
    # Present non-migrant (baseline)
    baseline_label = factor(ifelse(baseline == 1, "Present Non-Migrant", "Absent Migrant"),
                            levels = c("Absent Migrant", "Non-Internal Migrant"))
  )


# ==============================================================================
# SECTION 4: CREATE SEX CATEGORIES
# ==============================================================================

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(male = case_when(
    sex == 1 ~ 1,
    sex == 2 ~ 0,
    TRUE     ~ NA_real_
  ))


# ==============================================================================
# SECTION 5: CREATE ETHNICITY CATEGORIES
# ==============================================================================

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    Ethnicity = case_when(
      # Hill High Caste (Tagadhari)
      caste %in% c(1, 2, 14, 20, 27, 48, 49) ~ "Hill High Caste",
      
      # Hill Janajati (Indigenous)
      caste %in% c(3, 5, 6, 10, 11, 13, 24, 29, 32, 36, 45, 46, 60, 61, 62,
                   66, 67, 69, 74, 77, 78, 79, 80, 81, 89, 90, 91, 92, 94,
                   97, 98, 100, 110, 119, 120, 121, 124, 125, 126, 127, 130,
                   131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 992) ~ "Hill Janajati",
      
      # Terai/Madhesi Caste
      caste %in% c(4, 9, 16, 18, 19, 21, 26, 28, 30, 31, 33, 34, 35, 37,
                   42, 43, 44, 47, 51, 52, 53, 54, 55, 56, 57, 58, 59,
                   63, 64, 65, 68, 71, 72, 73, 84, 85, 86, 88, 96, 99,
                   115, 116, 117, 118, 122, 123, 128, 129, 993) ~ "Terai/Madhesi",
      
      # Dalit
      caste %in% c(8, 12, 15, 17, 22, 23, 25, 38, 39, 40, 41, 50, 70,
                   75, 76, 83, 93, 991) ~ "Dalit",
      
      # Muslim
      caste == 7 ~ "Muslim",
      
      TRUE ~ NA_character_
    )
  )


# ==============================================================================
# SECTION 6: CREATE MARITAL STATUS CATEGORIES
# ==============================================================================

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    marital_label = case_when(
      marital == 1 ~ "Never Married",
      marital == 2 ~ "Married",
      marital == 3 ~ "Single",
      marital == 4 ~ "Separated",
      marital == 5 ~ "Divorced",
      TRUE         ~ NA_character_
    )
  )


# ==============================================================================
# SECTION 7: CREATE EDUCATION CATEGORIES
# ==============================================================================

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    education_category = case_when(
      grade_comp %in% c(16, 17)            ~ "No Education",
      grade_comp >= 0 & grade_comp <= 5    ~ "Primary (1-5)",
      grade_comp >= 6 & grade_comp <= 12   ~ "Secondary (6-12)",
      grade_comp >= 13                     ~ "Tertiary",
      TRUE                                 ~ NA_character_
    )
  )


# ==============================================================================
# SECTION 8: CREATE OCCUPATION CATEGORIES
# ==============================================================================

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(occupation_types = as.numeric(occupation_types))

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    nsco_major = case_when(
      occupation_types < 1000  ~ floor(occupation_types / 100),
      occupation_types >= 1000 ~ floor(occupation_types / 1000),
      TRUE                     ~ NA_real_
    ),
    occupation_category = case_when(
      # ---- EDIT (2026-04): Moved the 9999 sentinel-code check to the TOP of the
      # case_when. Previously it sat at the bottom, but since floor(9999/1000) = 9,
      # the `nsco_major == 9` clause below was firing first and mislabeling 9999
      # respondents as "Elementary/Low Skilled". Now they correctly become NA.
      occupation_types == 9999                ~ NA_character_,
      occupation_types %in% c(110, 210, 310) ~ "Armed Forces",
      nsco_major %in% c(1, 2, 3)             ~ "High Skilled",
      nsco_major %in% c(4, 5)                ~ "Service & Clerical",
      nsco_major == 6                         ~ "Agriculture",
      nsco_major %in% c(7, 8)                ~ "Craft & Manufacturing",
      nsco_major == 9                         ~ "Elementary/Low Skilled",
      TRUE                                    ~ NA_character_
    )
  )


# ==============================================================================
# SECTION 9: CREATE TREATMENT/CONTROL COHORTS
# ==============================================================================

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    # Calculate ages
    birth_year             = SURVEY_YEAR - age,
    age_at_conflict_start  = CONFLICT_START - birth_year,
    age_at_conflict_end    = CONFLICT_END - birth_year,
    
    # Detailed cohort labels
    cohort_group = case_when(
      # TREATMENT: Childhood during conflict
      age_at_conflict_start >= 0  & age_at_conflict_start <= 5  & age >= 18 & age <= 45 ~ "Treatment: Age 0-5 in 1996",
      age_at_conflict_start >= 6  & age_at_conflict_start <= 12 & age >= 18 & age <= 45 ~ "Treatment: Age 6-12 in 1996",
      age_at_conflict_start >= 13 & age_at_conflict_start <= 17 & age >= 18 & age <= 45 ~ "Treatment: Age 13-17 in 1996",
      
      # CONTROL: Adult during conflict
      age_at_conflict_start >= 26 & age_at_conflict_start <= 35 & age >= 47 & age <= 65 ~ "Control: Age 26-35 in 1996",
      age_at_conflict_start >= 36 & age_at_conflict_start <= 40 & age >= 47 & age <= 65 ~ "Control: Age 36-40 in 1996",
      
      # EXCLUDED
      age < 18                                              ~ "Excluded: Too Young in 2017",
      age > 65                                              ~ "Excluded: Too Old in 2017",
      age_at_conflict_end < 6                               ~ "Excluded: Too Young During Conflict",
      age_at_conflict_start >= 41                           ~ "Excluded: Age 41+ in 1996",
      age_at_conflict_start >= 18 & age >= 18 & age <= 46  ~ "Excluded: Overlap Age",
      TRUE                                                  ~ "Excluded: Other"
    ),
    
    # Binary treatment indicator
    childhood_exposed = case_when(
      grepl("^Treatment", cohort_group) ~ 1,
      grepl("^Control",   cohort_group) ~ 0,
      TRUE                              ~ NA_real_
    ),
    
    # Simple label
    treatment_label = case_when(
      grepl("^Treatment", cohort_group) ~ "Treatment",
      grepl("^Control",   cohort_group) ~ "Control",
      TRUE                              ~ "Excluded"
    ),
    
    # Short label for graphs
    cohort_short = case_when(
      cohort_group == "Treatment: Age 0-5 in 1996"  ~ "T: 0-5",
      cohort_group == "Treatment: Age 6-12 in 1996" ~ "T: 6-12",
      cohort_group == "Treatment: Age 13-17 in 1996"~ "T: 13-17",
      cohort_group == "Control: Age 26-35 in 1996"  ~ "C: 26-35",
      cohort_group == "Control: Age 36-40 in 1996"  ~ "C: 36-40",
      TRUE                                           ~ "Excluded"
    ),
    
    # Treatment variable for regressions
    treatment = childhood_exposed
  )


# ==============================================================================
# SECTION 10: CREATE CONFLICT INTENSITY MEASURES----
# ==============================================================================

# Q3 cutoff for months of war
q3_war      <- quantile(nlss_conflict_data$mwar_own_any[nlss_conflict_data$mwar_own_any > 0],
                        probs = 0.75, na.rm = TRUE)

# Q3 cutoff for casualties
q3_casualty <- quantile(nlss_conflict_data$cas_own_any[nlss_conflict_data$cas_own_any > 0],
                        probs = 0.75, na.rm = TRUE)

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    # War-based binary
    high_conflict_q3_binary = case_when(
      mwar_own_any >  q3_war ~ 1,
      mwar_own_any <= q3_war ~ 0,
      TRUE                   ~ NA_real_
    ),
    high_conflict_q3_label = factor(
      case_when(
        mwar_own_any >  q3_war ~ "High Conflict",
        mwar_own_any <= q3_war ~ "Low Conflict",
        TRUE                   ~ NA_character_
      ),
      levels = c("Low Conflict", "High Conflict")
    ),
    
    # Casualty-based binary
    high_conflict_casualty_binary = case_when(
      cas_own_any >  q3_casualty ~ 1,
      cas_own_any <= q3_casualty ~ 0,
      TRUE                       ~ NA_real_
    ),
    high_conflict_casualty_label = factor(
      case_when(
        cas_own_any >  q3_casualty ~ "High Conflict",
        cas_own_any <= q3_casualty ~ "Low Conflict",
        TRUE                       ~ NA_character_
      ),
      levels = c("Low Conflict", "High Conflict")
    )
  )


# ==============================================================================
# SECTION 11: CREATE BINARY DUMMIES FOR T-TESTS----
# ==============================================================================

nlss_conflict_data <- nlss_conflict_data %>%
  mutate(
    # Sex
    male             = ifelse(sex == 1, 1, 0),
    
    # Education dummies
    edu_no_education = ifelse(education_category == "No Education",     1, 0),
    edu_primary      = ifelse(education_category == "Primary (1-5)",    1, 0),
    edu_secondary    = ifelse(education_category == "Secondary (6-12)", 1, 0),
    edu_tertiary     = ifelse(education_category == "Tertiary",         1, 0),
    
    # Ethnicity dummies
    eth_hill_high    = ifelse(Ethnicity == "Hill High Caste", 1, 0),
    eth_janajati     = ifelse(Ethnicity == "Hill Janajati",   1, 0),
    eth_terai        = ifelse(Ethnicity == "Terai/Madhesi",   1, 0),
    eth_dalit        = ifelse(Ethnicity == "Dalit",           1, 0),
    eth_muslim       = ifelse(Ethnicity == "Muslim",          1, 0),
    
    # Occupation dummies
    occ_agriculture  = ifelse(occupation_category == "Agriculture",            1, 0),
    occ_high_skilled = ifelse(occupation_category == "High Skilled",           1, 0),
    occ_service      = ifelse(occupation_category == "Service & Clerical",     1, 0),
    occ_craft        = ifelse(occupation_category == "Craft & Manufacturing",  1, 0),
    occ_elementary   = ifelse(occupation_category == "Elementary/Low Skilled", 1, 0),
    occ_armed        = ifelse(occupation_category == "Armed Forces",           1, 0),
    
    # ---- EDIT (2026-04): Added two indicators that flag the distinct
    # occupation-missingness categories separately from each other and from
    # "true" system missings. This preserves flexibility in the regression
    # scripts to include or exclude these groups as needed — without these
    # indicators, listwise deletion would silently drop 1,726 absentees
    # (mostly international migrants) from any regression that includes
    # occupation_category as a control.
    #
    # absent_occ_unknown:    occupation_types == 0   (N = 1,726)
    #                        Absent household member; occupation not reported
    #                        by the household respondent. 1,167 of these are
    #                        international migrants — exactly the outcome
    #                        of interest.
    #
    # occupation_not_stated: occupation_types == 9999 (N = 2,334)
    #                        Present respondent; occupation question answered
    #                        but code is "Not Stated" per NLSS coding.
    # -----------------------------------------------------------------------
    absent_occ_unknown    = ifelse(occupation_types == 0,    1, 0),
    occupation_not_stated = ifelse(occupation_types == 9999, 1, 0)
  )


# ==============================================================================
# SECTION 12: CREATE ANALYSIS SAMPLE ----
# ==============================================================================
# ---- EDIT (2026-04): Added a second, cohort-restricted dataset for use in
# balance tables and regressions. Two datasets are now available downstream:
#
#   nlss_conflict_data      — Full cleaned data (~90,109 obs). Used for
#                             whole-population descriptive statistics
#                             (Table 1, migration-type summaries, maps).
#
#   nlss_analysis_sample    — Cohort-restricted analysis sample (~37k obs).
#                             Keeps only individuals in Treatment (age 0-17
#                             at conflict start) or Control (age 26-40 at
#                             conflict start). This is the sample for the
#                             balance table, main regressions, and all
#                             treatment-vs-control comparisons.
#
# The filter below is the single definitional line of the analysis sample —
# referenced in the paper's sample-construction discussion and by the
# sample-construction table (03_02_sample_construction.R).
# -----------------------------------------------------------------------------

nlss_analysis_sample <- nlss_conflict_data %>%
  filter(!is.na(treatment))   # treatment is NA for all Excluded cohorts

cat("\n=== Sample sizes after cleaning ===\n")
cat("  Full sample (nlss_conflict_data):     ", nrow(nlss_conflict_data),     "\n")
cat("  Analysis sample (nlss_analysis_sample):", nrow(nlss_analysis_sample), "\n\n")
