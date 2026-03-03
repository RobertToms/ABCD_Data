####################
# Thelarche-to-Menarche_Tempo_6.0.R
# Author: Robert Toms
# Date: 1/13/2025
####################

# The purpose of this code is to calculate thelarche-to-menarche tempo for ABCD females.

##############
# Import Libraries and Data
##############

library(tidyverse)

sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/6.0/"

thelarche_youth <- read_csv(paste0(derivative_data, "Youth_Thelarche_Age_Status_12_16_25.csv"))
thelarche_parent <- read_csv(paste0(derivative_data, "Parent_Thelarche_Age_Status_12_16_25.csv"))

menarche <- read_csv(paste0(derivative_data, "abcd_6.0_menarche_ReadableSubset_1_9_26.csv"))

#####################
# IN THE BELOW CODE BLOCK
# Combine Thelarche and Menarche Data
#####################

# split menarche into parent and youth reports
parent <- menarche %>%
  select(participant_id, ab_g_stc__cohort_sex, baseline_age, AgeOfMenarche_NoImputations_ParentReport, AgeOfMenarche_ConsistentOnly_ParentReport, AgeOfMenarche_RawFirstReported_ParentReport,
         notes_p, SingleUniqueReport_ParentReport_Y1N0, MultipleReportsUnder1Yr_ParentReport_Y1N0, AgeImputedFromStatus_Y1N0_ParentReport, AgeImputed_WideRange_ParentReport_Y1N0,
         AgeImputed_ConsistentStatus_ParentReport_Y1N0, AgeImputed_InconsistentStatus_ParentReport_Y1N0, ReportingInconsistent_Y1N0_ParentReport, PremenarcheAtLastTP_Y1N0_ParentReport,
         PostmenarcheAtBaseline_Y1N0_ParentReport, FailedValidation_ParentReport_Y1N0, lastvisit_p, 
         `ph_p_pds__f_002_ses-00A`, `ph_p_pds__f_002_ses-01A`, `ph_p_pds__f_002_ses-02A`, `ph_p_pds__f_002_ses-03A`, `ph_p_pds__f_002_ses-04A`, `ph_p_pds__f_002_ses-05A`, `ph_p_pds__f_002_ses-06A`,
         `ph_p_pds__f_002__01_ses-00A`, `ph_p_pds__f_002__01_ses-01A`, `ph_p_pds__f_002__01_ses-02A`, `ph_p_pds__f_002__01_ses-03A`, `ph_p_pds__f_002__01_ses-04A`, `ph_p_pds__f_002__01_ses-05A`, `ph_p_pds__f_002__01_ses-06A`)
youth <- menarche %>%
  select(participant_id, ab_g_stc__cohort_sex, baseline_age, AgeOfMenarche_NoImputations_YouthReport, AgeOfMenarche_ConsistentOnly_YouthReport, AgeOfMenarche_RawFirstReported_YouthReport,
         notes_y, SingleUniqueReport_YouthReport_Y1N0, MultipleReportsUnder1Yr_YouthReport_Y1N0, AgeImputedFromStatus_Y1N0_YouthReport, AgeImputed_WideRange_YouthReport_Y1N0,
         AgeImputed_ConsistentStatus_YouthReport_Y1N0, AgeImputed_InconsistentStatus_YouthReport_Y1N0, ReportingInconsistent_Y1N0_YouthReport, PremenarcheAtLastTP_Y1N0_YouthReport,
         PostmenarcheAtBaseline_Y1N0_YouthReport, FailedValidation_YouthReport_Y1N0, lastvisit_y,
         `ph_y_pds__f_002_ses-00A`, `ph_y_pds__f_002_ses-01A`, `ph_y_pds__f_002_ses-02A`, `ph_y_pds__f_002_ses-03A`, `ph_y_pds__f_002_ses-04A`, `ph_y_pds__f_002_ses-05A`, `ph_y_pds__f_002_ses-06A`,
         `ph_y_pds__f_002__01_ses-00A`, `ph_y_pds__f_002__01_ses-01A`, `ph_y_pds__f_002__01_ses-02A`, `ph_y_pds__f_002__01_ses-03A`, `ph_y_pds__f_002__01_ses-04A`, `ph_y_pds__f_002__01_ses-05A`, `ph_y_pds__f_002__01_ses-06A`)

# merge thelarche data into menarche data
parent <- parent %>%
  left_join(thelarche_parent, by = "participant_id")
youth <- youth %>%
  left_join(thelarche_youth, by = "participant_id")

#####################
# IN THE BELOW CODE BLOCK
# Calculate Thelarche/Menarche Pubertal Tempo
#####################

# calculate tempos
parent <- parent %>%
  mutate(Tempo_NoMenarcheImputations = AgeOfMenarche_NoImputations_ParentReport - Age_of_Thelarche) %>% # Non-Imputed Menarche Ages Only
  mutate(Tempo_ConsistentOnly = AgeOfMenarche_ConsistentOnly_ParentReport - Age_of_Thelarche)           # Consistently Reported/Imputed Ages
youth <- youth %>%
  mutate(Tempo_NoMenarcheImputations = AgeOfMenarche_NoImputations_YouthReport - Age_of_Thelarche) %>% # Non-Imputed Menarche Ages Only
  mutate(Tempo_ConsistentOnly = AgeOfMenarche_ConsistentOnly_YouthReport - Age_of_Thelarche)           # Consistently Reported/Imputed Ages

# Z-score tempos
parent <- parent %>%
  mutate(ZScore_Tempo_NoMenarcheImputations = as.numeric(scale(Tempo_NoMenarcheImputations))) %>% # Non-Imputed Menarche Ages Only
  mutate(ZScore_Tempo_ConsistentOnly = as.numeric(scale(Tempo_ConsistentOnly)))                   # Consistently Reported/Imputed Ages
youth <- youth %>%
  mutate(ZScore_Tempo_NoMenarcheImputations = as.numeric(scale(Tempo_NoMenarcheImputations))) %>% # Non-Imputed Menarche Ages Only
  mutate(ZScore_Tempo_ConsistentOnly = as.numeric(scale(Tempo_ConsistentOnly)))                   # Consistently Reported/Imputed Ages

#####################
# IN THE BELOW CODE BLOCK
# Reorganize for export to csv
#####################

# reorganize to put variables of interest at the front
parent <- parent %>%
  select(participant_id, Tempo_NoMenarcheImputations, ZScore_Tempo_NoMenarcheImputations, Tempo_ConsistentOnly, ZScore_Tempo_ConsistentOnly, everything())
youth <- youth %>%
  select(participant_id, Tempo_NoMenarcheImputations, ZScore_Tempo_NoMenarcheImputations, Tempo_ConsistentOnly, ZScore_Tempo_ConsistentOnly, everything())

#####################
# IN THE BELOW CODE BLOCK
# Write to CSV
#####################

write_csv(parent, paste0(derivative_data, "Parent_6.0_Thelarche-to-Menarche_Tempo_1_13_26.csv"))
write_csv(youth, paste0(derivative_data, "Youth_6.0_Thelarche-to-Menarche_Tempo_1_13_26.csv"))

#####################
# IN THE BELOW CODE BLOCK
# Get Exclusion Counts
#####################

parent_count <- parent %>%
  select(participant_id, AgeOfMenarche_NoImputations_ParentReport, AgeOfMenarche_ConsistentOnly_ParentReport, Age_of_Thelarche) %>%
  mutate(has_menarche_age_imputed = !is.na(AgeOfMenarche_ConsistentOnly_ParentReport),
         has_menarche_age_noImputed = !is.na(AgeOfMenarche_NoImputations_ParentReport),
         has_thelarche_age = !is.na(Age_of_Thelarche))

table(parent_count$has_menarche_age_imputed) # 4859
table(parent_count$has_menarche_age_noImputed) # 4385
table(parent_count$has_thelarche_age) # 2100

parent_count <- parent_count %>%
  mutate(has_men_imputed_no_Thel = if_else(has_menarche_age_imputed, TRUE, NA),
         has_men_nonimputed_no_Thel = if_else(has_menarche_age_noImputed, TRUE, NA),
         has_thel_no_men_imputed = if_else(has_thelarche_age, TRUE, NA),
         has_thel_no_men_nonImputed = if_else(has_thelarche_age, TRUE, NA),
         has_both_imputed = (has_thelarche_age & has_menarche_age_imputed),
         has_both_nonimputed = (has_thelarche_age & has_menarche_age_noImputed)) %>%
  mutate(has_men_imputed_no_Thel = if_else(has_thelarche_age, FALSE, has_men_imputed_no_Thel),
         has_men_nonimputed_no_Thel = if_else(has_thelarche_age, FALSE, has_men_nonimputed_no_Thel),
         has_thel_no_men_imputed = if_else(has_menarche_age_imputed, FALSE, has_thel_no_men_imputed),
         has_thel_no_men_nonImputed = if_else(has_menarche_age_noImputed, FALSE, has_thel_no_men_nonImputed),
         has_both_imputed = (has_thelarche_age & has_menarche_age_imputed),
         has_both_nonimputed = (has_thelarche_age & has_menarche_age_noImputed))

youth_count <- youth %>%
  select(participant_id, AgeOfMenarche_NoImputations_YouthReport, AgeOfMenarche_ConsistentOnly_YouthReport, Age_of_Thelarche) %>%
  mutate(has_menarche_age_imputed = !is.na(AgeOfMenarche_ConsistentOnly_YouthReport),
         has_menarche_age_noImputed = !is.na(AgeOfMenarche_NoImputations_YouthReport),
         has_thelarche_age = !is.na(Age_of_Thelarche))

table(youth_count$has_menarche_age_imputed) # 4823
table(youth_count$has_menarche_age_noImputed) # 4444
table(youth_count$has_thelarche_age) # 1830

youth_count <- youth_count %>%
  mutate(has_men_imputed_no_Thel = if_else(has_menarche_age_imputed, TRUE, NA),
         has_men_nonimputed_no_Thel = if_else(has_menarche_age_noImputed, TRUE, NA),
         has_thel_no_men_imputed = if_else(has_thelarche_age, TRUE, NA),
         has_thel_no_men_nonImputed = if_else(has_thelarche_age, TRUE, NA),
         has_both_imputed = (has_thelarche_age & has_menarche_age_imputed),
         has_both_nonimputed = (has_thelarche_age & has_menarche_age_noImputed)) %>%
  mutate(has_men_imputed_no_Thel = if_else(has_thelarche_age, FALSE, has_men_imputed_no_Thel),
         has_men_nonimputed_no_Thel = if_else(has_thelarche_age, FALSE, has_men_nonimputed_no_Thel),
         has_thel_no_men_imputed = if_else(has_menarche_age_imputed, FALSE, has_thel_no_men_imputed),
         has_thel_no_men_nonImputed = if_else(has_menarche_age_noImputed, FALSE, has_thel_no_men_nonImputed),
         has_both_imputed = (has_thelarche_age & has_menarche_age_imputed),
         has_both_nonimputed = (has_thelarche_age & has_menarche_age_noImputed))


# Has valid Menarche Age, no valid Thelarche Age
table(parent_count$has_men_imputed_no_Thel) # 2989
table(parent_count$has_men_nonimputed_no_Thel) # 2619

table(youth_count$has_men_imputed_no_Thel) # 3186
table(youth_count$has_men_nonimputed_no_Thel) # 2903

# Has valid Thelarche Age, no valid Menarche Age
table(parent_count$has_thel_no_men_imputed) # 230
table(parent_count$has_thel_no_men_nonImputed) # 334

table(youth_count$has_thel_no_men_imputed) # 193
table(youth_count$has_thel_no_men_nonImputed) # 289

# has both
table(parent_count$has_both_imputed) # 1870
table(parent_count$has_both_nonimputed) # 1766

table(youth_count$has_both_imputed) # 1637
table(youth_count$has_both_nonimputed) # 1541
