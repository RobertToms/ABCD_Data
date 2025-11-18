############################
# PrePost_Menarche_5.0.R
# Author: Robert Toms
# Date: 11/17/2025
############################

# Import everything
library(tidyverse)
library(lubridate)

sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"

youth <- read_csv(paste0(sourcepath, '/physical-health/ph_y_pds.csv'))
parent <- read_csv(paste0(sourcepath, '/physical-health/ph_p_pds.csv'))
sex <- read_csv(paste0(sourcepath, '/abcd-general/abcd_p_demo.csv'))

#################
# IN THE BELOW CODE BLOCK
# Clean and Prepare Data
#################

# keep only necessary columns
sex <- sex %>%
  select(src_subject_id, demo_sex_v2) %>%
  filter(!is.na(demo_sex_v2))

youth <- youth %>%
  select(src_subject_id, eventname, 
         pds_f5_y) # Have you begun to menstruate (started to have your period)? No = 0; Yes = 1; Decline to Answer = 777, I don't know = 999
parent <- parent %>%
  select(src_subject_id, eventname, 
         pds_f5b_p) # Has your child begun to menstruate? No = 0; Yes = 1; Decline to Answer = 777, I don't know = 999

# merge
youth <- youth %>%
  left_join(sex, by = 'src_subject_id')
parent <- parent %>%
  left_join(sex, by = 'src_subject_id')

# keep only females, as only females had menarche data collected
youth <- youth %>%
  filter(demo_sex_v2 == 2) # 49151 -> 23417, 25734 tps dropped
parent <- parent %>%
  filter(demo_sex_v2 == 2) # 49151 -> 23417, 25734 tps dropped
sex <- sex %>%
  filter(demo_sex_v2 == 2) # 11868 -> 5677, 6191 subjects dropped (6188 Male, 3 Intersex Male)

# remove sex column
youth <- youth %>%
  select(-demo_sex_v2)
parent <- parent %>%
  select(-demo_sex_v2)

# count 999s and 777s
table(youth$pds_f5_y) # 777s = 787, 999s = 680; 21818 usable
table(parent$pds_f5b_p) # 777s = 0, 999s = 362; 22783 usable

## Count subjects with 999s or 777s
youth_999 <- youth %>%            
  filter(pds_f5_y == 999) # 680 tps NA
youth_777 <- youth %>%            
  filter(pds_f5_y == 777) # 787 tps NA
parent_999 <- parent %>%
  filter(pds_f5b_p == 999) # 362 tps NA
y_999_subjects <- length(table(youth_999$src_subject_id)) # 680 subjects affected
y_777_subjects <- length(table(youth_777$src_subject_id)) # 628 subjects affected
p_999_subjects <- length(table(parent_999$src_subject_id)) # 298 subjects affected

# set 999s and 777s to NA, per best practices of (Herting et al., 2021)
youth <- youth %>%
  mutate(pds_f5_y = if_else(pds_f5_y == '999' | pds_f5_y == '777', NA, pds_f5_y))
parent <- parent %>%
  mutate(pds_f5b_p = if_else(pds_f5b_p == '999' | pds_f5b_p == '777', NA, pds_f5b_p))

# pivot wide
youth <- youth %>%
  pivot_wider(names_from = eventname, values_from = pds_f5_y)
parent <- parent %>%
  pivot_wider(names_from = eventname, values_from = pds_f5b_p)

# make list vector of menarche reports
youth <- youth %>%
  group_by(src_subject_id) %>%
  mutate(menarche_reports = list(c(baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`,
                                   `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)))
parent <- parent %>%
  group_by(src_subject_id) %>%
  mutate(menarche_reports = list(c(baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`,
                                   `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)))

# count subjects with no data
youth_na_subs <- youth %>%
  rowwise() %>%
  filter(is.na(baseline_year_1_arm_1) & is.na(`1_year_follow_up_y_arm_1`) &
           is.na(`2_year_follow_up_y_arm_1`) & is.na(`3_year_follow_up_y_arm_1`) & is.na(`4_year_follow_up_y_arm_1`)) # 33 subjects with no usable data
parent_na_subs <- parent %>%
  rowwise() %>%
  filter(is.na(baseline_year_1_arm_1) & is.na(`1_year_follow_up_y_arm_1`) &
           is.na(`2_year_follow_up_y_arm_1`) & is.na(`3_year_follow_up_y_arm_1`) & is.na(`4_year_follow_up_y_arm_1`)) # 17 subjects with no usable data

# Youth: 5677 females - 33 = 5644 females with any usable data
# Parent: 5677 females - 17 = 5660 females with any usable data

# remove subjects with no data
youth <- youth %>%
  rowwise() %>%
  filter(!is.na(baseline_year_1_arm_1) | !is.na(`1_year_follow_up_y_arm_1`) |
           !is.na(`2_year_follow_up_y_arm_1`) | !is.na(`3_year_follow_up_y_arm_1`) | !is.na(`4_year_follow_up_y_arm_1`)) # 5644 subjects remaining
parent <- parent %>%
  rowwise() %>%
  filter(!is.na(baseline_year_1_arm_1) | !is.na(`1_year_follow_up_y_arm_1`) |
           !is.na(`2_year_follow_up_y_arm_1`) | !is.na(`3_year_follow_up_y_arm_1`) | !is.na(`4_year_follow_up_y_arm_1`)) # 5660 subjects remaining

#################
# IN THE BELOW CODE BLOCK
# Find Last PreMenarche / First PostMenarche Timepoints
#################

## Find Last Premenarche Timepoint
# Youth
youth$last_pre_index <- sapply(youth$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(1))  # Find pre-menarche "1" occurrences
  if (length(post_men_indices) > 0) {
    index <- tail(post_men_indices, 1)  # Get last occurrence
    return(index)
  } else {
    return(NA)  # No pre-menarche occurrence
  }
})

# Parent
parent$last_pre_index <- sapply(parent$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(1))  # Find pre-menarche "1" occurrences
  if (length(post_men_indices) > 0) {
    index <- tail(post_men_indices, 1)  # Get last occurrence
    return(index)
  } else {
    return(NA)  # No pre-menarche occurrence
  }
})

## Find First Postmenarche timepoint
# Parent
youth$first_post_index <- sapply(youth$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(4))  # Find post-menarche "4" occurrences
  if (length(post_men_indices) > 0) {
    index <- post_men_indices[1]  # Get first occurrence
    return(index)
  } else {
    return(NA)  # No post-menarche occurrence
  }
})
# Youth
parent$first_post_index <- sapply(parent$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(4))  # Find post-menarche "4" occurrences
  if (length(post_men_indices) > 0) {
    index <- post_men_indices[1]  # Get first occurrence
    return(index)
  } else {
    return(NA)  # No post-menarche occurrence
  }
})

# Convert indexes to timepoints by subtracting 1
youth$last_pre_event <- youth$last_pre_index - 1
parent$last_pre_event <- parent$last_pre_index - 1
youth$first_post_event <- youth$first_post_index - 1
parent$first_post_event <- parent$first_post_index - 1

#################
# IN THE BELOW CODE BLOCK
# Flag for PreMenarche at Last report, Postmenarche at first report, and Inconsistent reporting (premenarche after postmenarche)
#################

# POSTMENARCHE AT FIRST REPORT: If no premenarche reports or baseline is postmenarche, postmenarche at all reports (including first) are assumed
# PREMENARCHE AT LAST REPORT: If no postmenarche reports, premenarche at all reports (including last) are assumed
# INCONSISTENT REPORTING: If premenarche status is reported after a postmenarche report (0 after a 1), this is considered Inconsistent reporting

youth <- youth %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(is.na(last_pre_event) | menarche_reports[1] == "4", 1, 0)) %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(first_post_event == 0, 1, PostMenarche_at_Baseline_Y1N0)) %>%
  mutate(PreMenarche_at_LastReport_Y1N0 = if_else(is.na(first_post_event), 1, 0)) %>%
  mutate(Inconsistent_Reporting_Y1N0 = if_else(first_post_event < last_pre_event, 1, 0))
parent <- parent %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(is.na(last_pre_event) | menarche_reports[1] == "4", 1, 0)) %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(first_post_event == 0, 1, PostMenarche_at_Baseline_Y1N0)) %>%
  mutate(PreMenarche_at_LastReport_Y1N0 = if_else(is.na(first_post_event), 1, 0)) %>%
  mutate(Inconsistent_Reporting_Y1N0 = if_else(first_post_event < last_pre_event, 1, 0))

# count flags
table(youth$PostMenarche_at_Baseline_Y1N0)  # 251
table(youth$PreMenarche_at_LastReport_Y1N0) # 1553
table(youth$Inconsistent_Reporting_Y1N0)    # 89
table(parent$PostMenarche_at_Baseline_Y1N0)  # 172
table(parent$PreMenarche_at_LastReport_Y1N0) # 1477
table(parent$Inconsistent_Reporting_Y1N0)    # 56

#################
# IN THE BELOW CODE BLOCK
# Find adjusted premenarche timepoints for Inconsistent reporters
#################

## Define function that finds most recent premenarche timepoint with menarche data prior to first postmenarche report
find_premenarche <- function(menarche_reports) {
  first_postmenarche <- which(menarche_reports == 4)[1]  # Find the first occurrence of 4 (postmenarche)
  if (is.na(first_postmenarche)) return(NA) # No 4 found
  preceding_1s <- which(menarche_reports[1:(first_postmenarche - 1)] == 1) # Find all 1s before that
  if (length(preceding_1s) == 0) return(NA) # No 1s before the 4
  return(tail(preceding_1s, 1)) # Return the last 1 before the 4
}

# apply function
youth <- youth %>%
  rowwise() %>%
  mutate(Inconsistent_Pre = find_premenarche(menarche_reports)) %>%
  # Convert index to timepoint by subtracting 1
  mutate(Inconsistent_Pre = Inconsistent_Pre - 1)
parent <- parent %>%
  rowwise() %>%
  mutate(Inconsistent_Pre = find_premenarche(menarche_reports)) %>%
  # Convert index to timepoint by subtracting 1
  mutate(Inconsistent_Pre = Inconsistent_Pre - 1)

# count changes
table(youth$Inconsistent_Reporting_Y1N0, youth$Inconsistent_Pre) # 28 + 17 + 4 = 49 to be changed/corrected
table(parent$Inconsistent_Reporting_Y1N0, parent$Inconsistent_Pre) # 23 + 19 + 3 = 45 to be changed/corrected

# set Inconsistent reporter's pre sessions to corrected versions
youth <- youth %>%
  mutate(last_pre_event = if_else(Inconsistent_Reporting_Y1N0 == 1, Inconsistent_Pre, last_pre_event, missing = last_pre_event))
parent <- parent %>%
  mutate(last_pre_event = if_else(Inconsistent_Reporting_Y1N0 == 1, Inconsistent_Pre, last_pre_event, missing = last_pre_event))

# count subjects affected by missing premenarche and missing postmenarche events
y_no_pre <- youth %>% filter(is.na(last_pre_event))
y_no_pre_num <- length(table(y_no_pre$src_subject_id)) # 256 subjects affected
y_no_post <- youth %>% filter(is.na(first_post_event))
y_no_post_num <- length(table(y_no_post$src_subject_id)) # 1553 subjects affected

p_no_pre <- parent %>% filter(is.na(last_pre_event))
p_no_pre_num <- length(table(p_no_pre$src_subject_id)) # 172 subjects affected
p_no_post <- parent %>% filter(is.na(first_post_event))
p_no_post_num <- length(table(p_no_post$src_subject_id)) # 1477 subjects affected


# Recalculate premenarche last report and postmenarche baseline
youth <- youth %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(is.na(last_pre_event) | menarche_reports[1] == "4", 1, 0)) %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(first_post_event == 0, 1, PostMenarche_at_Baseline_Y1N0)) %>%
  mutate(PreMenarche_at_LastReport_Y1N0 = if_else(is.na(first_post_event), 1, 0))
parent <- parent %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(is.na(last_pre_event) | menarche_reports[1] == "4", 1, 0)) %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(first_post_event == 0, 1, PostMenarche_at_Baseline_Y1N0)) %>%
  mutate(PreMenarche_at_LastReport_Y1N0 = if_else(is.na(first_post_event), 1, 0))

#################
# IN THE BELOW CODE BLOCK
# Clean for Export
#################

# keep and order useful columns
youth <- youth %>%
  select(src_subject_id, last_pre_event, first_post_event, menarche_reports,
         baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`,
         `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`,
         PostMenarche_at_Baseline_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)
parent <- parent %>%
  select(src_subject_id, last_pre_event, first_post_event, menarche_reports,
         baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`,
         `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`,
         PostMenarche_at_Baseline_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)

# convert menarche_reports to string so it can be written into a csv
youth <- youth %>%
  mutate(menarche_reports = paste(menarche_reports, collapse = ", "))
parent <- parent %>%
  mutate(menarche_reports = paste(menarche_reports, collapse = ", "))

# count final pre/post people
counter_p <- parent %>%
  mutate(valid = if_else(!is.na(last_pre_event) & !is.na(first_post_event), 1, 0))
table(counter_p$valid) # 4011 valid
table(counter_p$valid, counter_p$PreMenarche_at_LastReport_Y1N0) # 1477 not valid because premenarche at last report
table(counter_p$valid, counter_p$PostMenarche_at_Baseline_Y1N0) # 172 not valid because postmenarche at baseline
table(counter_p$valid, counter_p$Inconsistent_Reporting_Y1N0) # Inconsistent Reporters: 11 not valid, 45 valid/corrected
# sanity check that 11 not valid are postmenarche at baseline
table(counter_p$PostMenarche_at_Baseline_Y1N0, counter_p$Inconsistent_Reporting_Y1N0) # 11 inconsistent and postmenarche at baseline, sanity checked

counter_y <- youth %>%
  mutate(valid = if_else(!is.na(last_pre_event) & !is.na(first_post_event), 1, 0))
table(counter_y$valid) # 3835 valid
table(counter_y$valid, counter_y$PreMenarche_at_LastReport_Y1N0) # 1553 not valid because premenarche at last report
table(counter_y$valid, counter_y$PostMenarche_at_Baseline_Y1N0) # 256 not valid because postmenarche at baseline
table(counter_y$valid, counter_y$Inconsistent_Reporting_Y1N0) # Inconsistent Reporters: 40 not valid, 49 valid
# sanity check that 40 not valid inconsistents are invalid because they are postmenarche at baseline
table(counter_y$PostMenarche_at_Baseline_Y1N0, counter_y$Inconsistent_Reporting_Y1N0) # 40 inconsistent and not postmenarche at baseline, sanity checked

#################
# IN THE BELOW CODE BLOCK
# Write to CSV
#################

write_csv(youth, paste0(derivative_data, '5.0_Youth_PrePost_Menarche_11_17_25.csv'))
write_csv(parent, paste0(derivative_data, '5.0_Parent_PrePost_Menarche_11_17_25.csv'))
