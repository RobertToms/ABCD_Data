####################
# Thelarche-to-Menarche_Tempo_5.0.R
# Author: Robert Toms
# Date: 2/19/2025
####################

# The purpose of this code is to calculate thelarche-to-menarche tempo for ABCD females.

##############
# Import Libraries and Data
##############

library(tidyverse)

sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"

thelarche_youth <- read_csv(paste0(derivative_data, "Youth_5.0_Thelarche_Age_Status_1_22_26.csv"))
thelarche_parent <- read_csv(paste0(derivative_data, "Parent_5.0_Thelarche_Age_Status_1_22_26.csv"))

menarche <- read_csv(paste0(derivative_data, "abcd_menarche_ReadableSubset_2_18_26.csv"))

#####################
# IN THE BELOW CODE BLOCK
# Combine Thelarche and Menarche Data
#####################

# split menarche into parent and youth reports
parent <- menarche %>%
  select(src_subject_id, demo_sex_v2, baseline_age, AgeOfMenarche_NoImputations_ParentReport, AgeOfMenarche_ConsistentOnly_ParentReport, AgeOfMenarche_RawFirstReported_ParentReport,
         notes_p, SingleUniqueReport_ParentReport_Y1N0, MultipleReportsUnder1Yr_ParentReport_Y1N0, AgeImputedFromStatus_Y1N0_ParentReport, AgeImputed_WideRange_ParentReport_Y1N0,
         AgeImputed_ConsistentStatus_ParentReport_Y1N0, AgeImputed_InconsistentStatus_ParentReport_Y1N0, ReportingInconsistent_Y1N0_ParentReport, PremenarcheAtLastTP_Y1N0_ParentReport,
         PostmenarcheAtBaseline_Y1N0_ParentReport, FailedValidation_ParentReport_Y1N0, lastvisit_p, 
         pds_f5b_p_baseline_year_1_arm_1, pds_f5b_p_1_year_follow_up_y_arm_1, pds_f5b_p_2_year_follow_up_y_arm_1, pds_f5b_p_3_year_follow_up_y_arm_1, pds_f5b_p_4_year_follow_up_y_arm_1,
         pds_f6_p_baseline_year_1_arm_1, pds_f6_p_1_year_follow_up_y_arm_1, pds_f6_p_2_year_follow_up_y_arm_1, pds_f6_p_3_year_follow_up_y_arm_1, pds_f6_p_4_year_follow_up_y_arm_1)
youth <- menarche %>%
  select(src_subject_id, demo_sex_v2, baseline_age, AgeOfMenarche_NoImputations_YouthReport, AgeOfMenarche_ConsistentOnly_YouthReport, AgeOfMenarche_RawFirstReported_YouthReport,
         notes_y, SingleUniqueReport_YouthReport_Y1N0, MultipleReportsUnder1Yr_YouthReport_Y1N0, AgeImputedFromStatus_Y1N0_YouthReport, AgeImputed_WideRange_YouthReport_Y1N0,
         AgeImputed_ConsistentStatus_YouthReport_Y1N0, AgeImputed_InconsistentStatus_YouthReport_Y1N0, ReportingInconsistent_Y1N0_YouthReport, PremenarcheAtLastTP_Y1N0_YouthReport,
         PostmenarcheAtBaseline_Y1N0_YouthReport, FailedValidation_YouthReport_Y1N0, lastvisit_y,
         pds_f5_y_baseline_year_1_arm_1, pds_f5_y_1_year_follow_up_y_arm_1, pds_f5_y_2_year_follow_up_y_arm_1, pds_f5_y_3_year_follow_up_y_arm_1, pds_f5_y_4_year_follow_up_y_arm_1,
         pds_f6_y_baseline_year_1_arm_1, pds_f6_y_1_year_follow_up_y_arm_1, pds_f6_y_2_year_follow_up_y_arm_1, pds_f6_y_3_year_follow_up_y_arm_1, pds_f6_y_4_year_follow_up_y_arm_1)

# merge thelarche data into menarche data
parent <- parent %>%
  left_join(thelarche_parent, by = "src_subject_id")
youth <- youth %>%
  left_join(thelarche_youth, by = "src_subject_id")

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

# rename baseline_age
parent <- parent %>%
  rename(baseline_age = baseline_age.x,
         year0_age = baseline_age.y)
youth <- youth %>%
  rename(baseline_age = baseline_age.x,
         year0_age = baseline_age.y)

# reorganize to put variables of interest at the front
parent <- parent %>%
  select(src_subject_id, Tempo_NoMenarcheImputations, ZScore_Tempo_NoMenarcheImputations, Tempo_ConsistentOnly, ZScore_Tempo_ConsistentOnly, everything())
youth <- youth %>%
  select(src_subject_id, Tempo_NoMenarcheImputations, ZScore_Tempo_NoMenarcheImputations, Tempo_ConsistentOnly, ZScore_Tempo_ConsistentOnly, everything())

#####################
# IN THE BELOW CODE BLOCK
# Write to CSV
#####################

write_csv(parent, paste0(derivative_data, "Parent_5.0_Thelarche-to-Menarche_Tempo_2_19_26.csv"))
write_csv(youth, paste0(derivative_data, "Youth_5.0_Thelarche-to-Menarche_Tempo_2_19_26.csv"))

#####################
# IN THE BELOW CODE BLOCK
# Get Exclusion Counts
#####################

parent_count <- parent %>%
  select(src_subject_id, AgeOfMenarche_NoImputations_ParentReport, AgeOfMenarche_ConsistentOnly_ParentReport, Age_of_Thelarche) %>%
  mutate(has_menarche_age_imputed = !is.na(AgeOfMenarche_ConsistentOnly_ParentReport),
         has_menarche_age_noImputed = !is.na(AgeOfMenarche_NoImputations_ParentReport),
         has_thelarche_age = !is.na(Age_of_Thelarche))

table(parent_count$has_menarche_age_imputed) # 3958
table(parent_count$has_menarche_age_noImputed) # 3806
table(parent_count$has_thelarche_age) # 2052

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
  select(src_subject_id, AgeOfMenarche_NoImputations_YouthReport, AgeOfMenarche_ConsistentOnly_YouthReport, Age_of_Thelarche) %>%
  mutate(has_menarche_age_imputed = !is.na(AgeOfMenarche_ConsistentOnly_YouthReport),
         has_menarche_age_noImputed = !is.na(AgeOfMenarche_NoImputations_YouthReport),
         has_thelarche_age = !is.na(Age_of_Thelarche))

table(youth_count$has_menarche_age_imputed) # 3846
table(youth_count$has_menarche_age_noImputed) # 3708
table(youth_count$has_thelarche_age) # 1763

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
table(parent_count$has_men_imputed_no_Thel) # 2677
table(parent_count$has_men_nonimputed_no_Thel) # 2559

table(youth_count$has_men_imputed_no_Thel) # 2742
table(youth_count$has_men_nonimputed_no_Thel) # 2640

# Has valid Thelarche Age, no valid Menarche Age
table(parent_count$has_thel_no_men_imputed) # 771
table(parent_count$has_thel_no_men_nonImputed) # 805

table(youth_count$has_thel_no_men_imputed) # 659
table(youth_count$has_thel_no_men_nonImputed) # 695

# has both
table(parent_count$has_both_imputed) # 1281
table(parent_count$has_both_nonimputed) # 1247

table(youth_count$has_both_imputed) # 1104
table(youth_count$has_both_nonimputed) # 1068
