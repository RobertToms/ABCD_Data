############################
# PrePost_Menarche_6.0.R
# Author: Robert Toms
# Date: 11/17/2025
############################

# Import everything
library(tidyverse)
library(lubridate)

sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/6.0/"

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

# keep only females, as only females had menarche data collected
youth <- youth %>%
  filter(ab_g_stc__cohort_sex == 2) # 67908 -> 32365, 35543 tps dropped
parent <- parent %>%
  filter(ab_g_stc__cohort_sex == 2) # 67223 -> 31978, 35245 tps dropped
sex <- sex %>%
  filter(ab_g_stc__cohort_sex == 2) # 11868 -> 5678, 6190 subjects dropped

# remove sex column
youth <- youth %>%
  select(-ab_g_stc__cohort_sex)
parent <- parent %>%
  select(-ab_g_stc__cohort_sex)

## Count subjects with 999s or 777s
youth_999 <- youth %>%            
  filter(ph_y_pds__f_002 == 999) # 680 tps to be set to NA
youth_777 <- youth %>%            
  filter(ph_y_pds__f_002 == 777) # 1049 tps to be set to NA
parent_999 <- parent %>%
  filter(ph_p_pds__f_002 == 999) # 511 tps to be set to NA
y_999_subjects <- length(table(youth_999$participant_id)) # 680 subjects affected
y_777_subjects <- length(table(youth_777$participant_id)) # 735 subjects affected
p_999_subjects <- length(table(parent_999$participant_id)) # 386 subjects affected

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

# Youth: 5678 females
# Parent: 5677 females

# who's missing and why?
parent_subs <- parent %>% pull(participant_id)
youth_subs <- youth %>% pull(participant_id)
youth_missing <- youth %>% filter(!(participant_id %in% parent_subs))
# sub-FZWKRBW7, reason unclear after manual inspection. only has a baseline report in youth, not enough data to determine pre/post menarche transition.

# make list vector
youth <- youth %>%
  group_by(participant_id) %>%
  mutate(menarche_reports = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)))
parent <- parent %>%
  group_by(participant_id) %>%
  mutate(menarche_reports = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)))

# count subjects with no data
youth_na_subs <- youth %>%
  rowwise() %>%
  filter(is.na(`ses-00A`) & is.na(`ses-01A`) &
           is.na(`ses-02A`) & is.na(`ses-03A`) & is.na(`ses-04A`) & is.na(`ses-05A`) & is.na(`ses-06A`)) # 27 subjects with no usable data
parent_na_subs <- parent %>%
  rowwise() %>%
  filter(is.na(`ses-00A`) & is.na(`ses-01A`) &
           is.na(`ses-02A`) & is.na(`ses-03A`) & is.na(`ses-04A`) & is.na(`ses-05A`) & is.na(`ses-06A`)) # 14 subjects with no usable data

# Youth: 5678 females - 27 = 5651 females with any usable data
# Parent: 5677 females - 14 = 5663 females with any usable data

# remove subjects with no data
youth <- youth %>%
  rowwise() %>%
  filter(!is.na(`ses-00A`) | !is.na(`ses-01A`) |!is.na(`ses-02A`) | !is.na(`ses-03A`) | !is.na(`ses-04A`) | !is.na(`ses-05A`) | !is.na(`ses-06A`)) # 5651 subjects remaining
parent <- parent %>%
  rowwise() %>%
  filter(!is.na(`ses-00A`) | !is.na(`ses-01A`) |!is.na(`ses-02A`) | !is.na(`ses-03A`) | !is.na(`ses-04A`) | !is.na(`ses-05A`) | !is.na(`ses-06A`)) # 5663 subjects remaining

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

# POSTMENARCHE AT FIRST REPORT: If no premenarche reports or baseline is postmenarche, postmenarche at all reports (including first) are assumed
# PREMENARCHE AT LAST REPORT: If no postmenarche reports, premenarche at all reports (including last) are assumed
# INCONSISTENT REPORTING: If premenarche status is reported after a postmenarche report (0 after a 1), this is considered Inconsistent reporting

youth <- youth %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(is.na(last_pre_session) | menarche_reports[1] == "1", 1, 0)) %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(first_post_session == 0, 1, PostMenarche_at_Baseline_Y1N0)) %>%
  mutate(PreMenarche_at_LastReport_Y1N0 = if_else(is.na(first_post_session), 1, 0)) %>%
  mutate(Inconsistent_Reporting_Y1N0 = if_else(first_post_session < last_pre_session, 1, 0))
parent <- parent %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(is.na(last_pre_session) | menarche_reports[1] == "1", 1, 0)) %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(first_post_session == 0, 1, PostMenarche_at_Baseline_Y1N0)) %>%
  mutate(PreMenarche_at_LastReport_Y1N0 = if_else(is.na(first_post_session), 1, 0)) %>%
  mutate(Inconsistent_Reporting_Y1N0 = if_else(first_post_session < last_pre_session, 1, 0))

# count flags
table(youth$PostMenarche_at_Baseline_Y1N0)  # 256
table(youth$PreMenarche_at_LastReport_Y1N0) # 561
table(youth$Inconsistent_Reporting_Y1N0)    # 120
table(parent$PostMenarche_at_Baseline_Y1N0)  # 174
table(parent$PreMenarche_at_LastReport_Y1N0) # 565
table(parent$Inconsistent_Reporting_Y1N0)    # 86

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
  return(tail(preceding_0s, 1)) # Return the last one before the 1
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
table(youth$Inconsistent_Reporting_Y1N0, youth$Inconsistent_Pre) # 34+23+11+9+2 = 79 to be changed/corrected
table(parent$Inconsistent_Reporting_Y1N0, parent$Inconsistent_Pre) # 28+29+12+4 = 73 to be changed/corrected

# set Inconsistent reporter's pre sessions to corrected versions
youth <- youth %>%
  mutate(last_pre_session = if_else(Inconsistent_Reporting_Y1N0 == 1, Inconsistent_Pre, last_pre_session, missing = last_pre_session))
parent <- parent %>%
  mutate(last_pre_session = if_else(Inconsistent_Reporting_Y1N0 == 1, Inconsistent_Pre, last_pre_session, missing = last_pre_session))


# count subjects affected by missing premenarche and missing postmenarche events
y_no_pre <- youth %>% filter(is.na(last_pre_session))
y_no_pre_num <- length(table(y_no_pre$participant_id)) # 262 subjects affected
y_no_post <- youth %>% filter(is.na(first_post_session))
y_no_post_num <- length(table(y_no_post$participant_id)) # 561 subjects affected

p_no_pre <- parent %>% filter(is.na(last_pre_session))
p_no_pre_num <- length(table(p_no_pre$participant_id)) # 174 subjects affected
p_no_post <- parent %>% filter(is.na(first_post_session))
p_no_post_num <- length(table(p_no_post$participant_id)) # 565 subjects affected


# Recalculate premenarche last report and postmenarche baseline
youth <- youth %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(is.na(last_pre_session) | menarche_reports[1] == "4", 1, 0)) %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(first_post_session == 0, 1, PostMenarche_at_Baseline_Y1N0)) %>%
  mutate(PreMenarche_at_LastReport_Y1N0 = if_else(is.na(first_post_session), 1, 0))
parent <- parent %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(is.na(last_pre_session) | menarche_reports[1] == "4", 1, 0)) %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(first_post_session == 0, 1, PostMenarche_at_Baseline_Y1N0)) %>%
  mutate(PreMenarche_at_LastReport_Y1N0 = if_else(is.na(first_post_session), 1, 0))

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

# count final pre/post people
counter_p <- parent %>%
  mutate(valid = if_else(!is.na(last_pre_session) & !is.na(first_post_session), 1, 0))
table(counter_p$valid) # 4924 valid
table(counter_p$valid, counter_p$PreMenarche_at_LastReport_Y1N0) # 565 not valid because premenarche at last report
table(counter_p$valid, counter_p$PostMenarche_at_Baseline_Y1N0) # 174 not valid because postmenarche at baseline
table(counter_p$valid, counter_p$Inconsistent_Reporting_Y1N0) # Inconsistent Reporters: 13 not valid, 73 valid/corrected
# sanity check that those 13 invalid are because they are also postmenarche at baseline
table(counter_p$PostMenarche_at_Baseline_Y1N0, counter_p$Inconsistent_Reporting_Y1N0) # 13, sanity checked

counter_y <- youth %>%
  mutate(valid = if_else(!is.na(last_pre_session) & !is.na(first_post_session), 1, 0))
table(counter_y$valid) # 4828 valid
table(counter_y$valid, counter_y$PreMenarche_at_LastReport_Y1N0) # 561 not valid because premenarche at last report
table(counter_y$valid, counter_y$PostMenarche_at_Baseline_Y1N0) # 262 not valid because postmenarche at baseline
table(counter_y$valid, counter_y$Inconsistent_Reporting_Y1N0) # Inconsistent Reporters: 41 not valid, 79 valid/corrected
# sanity check that those 41 invalid are because they are also postmenarche at baseline
table(counter_y$PostMenarche_at_Baseline_Y1N0, counter_y$Inconsistent_Reporting_Y1N0) # 41, sanity checked


#################
# IN THE BELOW CODE BLOCK
# Write to CSV
#################

write_csv(youth, paste0(derivative_data, 'Youth_PrePost_Menarche_11_17_25.csv'))
write_csv(parent, paste0(derivative_data, 'Parent_PrePost_Menarche_11_17_25.csv'))
