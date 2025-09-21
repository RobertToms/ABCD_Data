############################
# PrePost_Menarche.R
# Author: Robert Toms
# Date: 9/15/2025
############################

# Import everything
library(tidyverse)
library(lubridate)

sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/"

youth <- read_tsv(paste0(sourcepath, '/Physical_Health/ph_y_pds.tsv'))
parent <- read_tsv(paste0(sourcepath, '/Physical_Health/ph_p_pds.tsv'))
sex <- read_tsv(paste0(sourcepath, '/abcd_general/ab_g_stc.tsv'))

#################
# IN THE BELOW CODE BLOCK
# Clean and Prepare Data
#################

sex <- sex %>%
  select(participant_id, ab_g_stc__cohort_sex)

# keep only necessary columns
youth <- youth %>%
  select(participant_id, session_id, 
         ph_y_pds__f_002) # Have you begun to menstruate (started to have your period)? No = 0; Yes = 1; Decline to Answer = 777, I don't know = 999
parent <- parent %>%
  select(participant_id, session_id, 
         ph_p_pds__f_002) # Has your child begun to menstruate? No = 0; Yes = 1; Decline to Answer = 777, I don't know = 999

# merge
youth <- youth %>%
  left_join(sex, by = 'participant_id')
parent <- parent %>%
  left_join(sex, by = 'participant_id')

# keep only females
youth <- youth %>%
  filter(ab_g_stc__cohort_sex == 2)
parent <- parent %>%
  filter(ab_g_stc__cohort_sex == 2)

# remove sex column
youth <- youth %>%
  select(-ab_g_stc__cohort_sex)
parent <- parent %>%
  select(-ab_g_stc__cohort_sex)

# set 999s and 777s to NA (Herting et al., 2021), set 'n/a' to NA
youth <- youth %>%
  mutate(ph_y_pds__f_002 = if_else(ph_y_pds__f_002 == '999' | ph_y_pds__f_002 == '777' | ph_y_pds__f_002 == 'n/a', NA, ph_y_pds__f_002))
parent <- parent %>%
  mutate(ph_p_pds__f_002 = if_else(ph_p_pds__f_002 == '999' | ph_p_pds__f_002 == '777'| ph_p_pds__f_002 == 'n/a', NA, ph_p_pds__f_002))

# pivot wide
youth <- youth %>%
  pivot_wider(names_from = session_id, values_from = ph_y_pds__f_002)
parent <- parent %>%
  pivot_wider(names_from = session_id, values_from = ph_p_pds__f_002)

# make list vector
youth <- youth %>%
  group_by(participant_id) %>%
  mutate(menarche_reports = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)))
parent <- parent %>%
  group_by(participant_id) %>%
  mutate(menarche_reports = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)))

#################
# IN THE BELOW CODE BLOCK
# Find Last PreMenarche / First PostMenarche Timepoints
#################

## Find Last Premenarche Timepoint
# Youth
youth$last_pre_index <- sapply(youth$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(0))  # Find pre-menarche occurrences
  if (length(post_men_indices) > 0) {
    index <- tail(post_men_indices, 1)  # Get last occurrence
    return(index)
  } else {
    return(NA)  # No pre-menarche occurrence
  }
})

# Parent
parent$last_pre_index <- sapply(parent$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(0))  # Find pre-menarche occurrences
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
  post_men_indices <- which(i %in% c(1))  # Find post-menarche occurrences
  if (length(post_men_indices) > 0) {
    index <- post_men_indices[1]  # Get first occurrence
    return(index)
  } else {
    return(NA)  # No post-menarche occurrence
  }
})
# Youth
parent$first_post_index <- sapply(parent$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(1))  # Find post-menarche occurrences
  if (length(post_men_indices) > 0) {
    index <- post_men_indices[1]  # Get first occurrence
    return(index)
  } else {
    return(NA)  # No post-menarche occurrence
  }
})

# Convert indexes to timepoints by subtracting 1
youth$last_pre_session <- youth$last_pre_index - 1
parent$last_pre_session <- parent$last_pre_index - 1
youth$first_post_session <- youth$first_post_index - 1
parent$first_post_session <- parent$first_post_index - 1

#################
# IN THE BELOW CODE BLOCK
# Flag for PreMenarche at Last report, Postmenarche at first report, and Inconsistent reporting (premenarche after postmenarche)
#################

# POSTMENARCHE AT FIRST REPORT: If no premenarche reports, postmenarche at all reports (including first) are assumed
# PREMENARCHE AT LAST REPORT: If no postmenarche reports, premenarche at all reports (including last) are assumed
# INCONSISTENT REPORTING: If premenarche status is reported after a postmenarche report (0 after a 1), this is considered Inconsistent reporting

youth <- youth %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(is.na(last_pre_session), 1, 0)) %>%
  mutate(PreMenarche_at_LastReport_Y1N0 = if_else(is.na(first_post_session), 1, 0)) %>%
  mutate(Inconsistent_Reporting_Y1N0 = if_else(first_post_session < last_pre_session, 1, 0))
parent <- parent %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(is.na(last_pre_session), 1, 0)) %>%
  mutate(PreMenarche_at_LastReport_Y1N0 = if_else(is.na(first_post_session), 1, 0)) %>%
  mutate(Inconsistent_Reporting_Y1N0 = if_else(first_post_session < last_pre_session, 1, 0))


#################
# IN THE BELOW CODE BLOCK
# Find adjusted premenarche timepoints for Inconsistent reporters
#################

## Define function that finds most recent premenarche timepoint with menarche data prior to first postmenarche report
find_premenarche <- function(menarche_reports) {
  first_postmenarche <- which(menarche_reports == 1)[1]  # Find the first occurrence of 1
  if (is.na(first_postmenarche)) return(NA) # No 1 found
  preceding_0s <- which(menarche_reports[1:(first_postmenarche - 1)] == 0) # Find all 0s before that
  if (length(preceding_0s) == 0) return(NA) # No 0s before the 1
  return(tail(preceding_0s, 1)) # Return index of the last 0 before the 1
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

# set Inconsistent reporter's pre sessions to corrected versions
youth <- youth %>%
  mutate(last_pre_session = if_else(Inconsistent_Reporting_Y1N0 == 1, Inconsistent_Pre, last_pre_session, missing = last_pre_session))
parent <- parent %>%
  mutate(last_pre_session = if_else(Inconsistent_Reporting_Y1N0 == 1, Inconsistent_Pre, last_pre_session, missing = last_pre_session))

#################
# IN THE BELOW CODE BLOCK
# Clean for Export
#################

# keep and order useful columns
youth <- youth %>%
  select(participant_id, last_pre_session, first_post_session, 
         `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`,
         PostMenarche_at_Baseline_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)
parent <- parent %>%
  select(participant_id, last_pre_session, first_post_session, 
         `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`,
         PostMenarche_at_Baseline_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)

#################
# IN THE BELOW CODE BLOCK
# Write to CSV
#################

write_csv(youth, paste0(derivative_data, 'Youth_PrePost_Menarche_9_15_25.csv'))
write_csv(parent, paste0(derivative_data, 'Parent_PrePost_Menarche_9_15_25.csv'))
