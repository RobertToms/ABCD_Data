##########################
# PrePost_CBCL_5.0.R
# Author: Robert Toms
# 6.0 -> 5.0 Update: Emma Lambert
# Date: 12/18/2025
##########################

# The purpose of this code is to calculate change scores in Internalizing and Externalizing symptoms on the Child Behavioral Checklist (CBCL) from pre to post menarche. 

# libraries
library(tidyverse)
library(lubridate)

# define paths
sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"

# import data
parent <- read_csv(paste0(derivative_data, '5.0_Parent_PrePost_Menarche_12_2_25.csv'))
youth <- read_csv(paste0(derivative_data, '5.0_Youth_PrePost_Menarche_12_2_25.csv'))
cbcl <- read_csv(paste0(sourcepath, 'mental-health/mh_p_cbcl.csv')) 
dates <- read_csv(paste0(sourcepath, 'abcd-general/abcd_y_lt.csv'))

demo <- read_csv(paste0(sourcepath, 'abcd-general/abcd_p_demo.csv'))

#########################
# IN THE BELOW CODE BLOCK
# Prepare data
#########################

# keep only useful columns
parent_simple <- parent %>%
  select(src_subject_id, last_pre_event, first_post_event, PostMenarche_at_Baseline_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)
youth_simple <- youth %>%
  select(src_subject_id, last_pre_event, first_post_event, PostMenarche_at_Baseline_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)
cbcl <- cbcl %>%
  select(src_subject_id, eventname, 
         cbcl_scr_syn_internal_r,  # Internal CBCL Syndrome Scale (sum)
         cbcl_scr_syn_external_r   # External CBCL Syndrome Scale (sum)
  )

demo <- demo %>%
  select(src_subject_id, demo_sex_v2) %>%
  filter(!is.na(demo_sex_v2)) # 11868

sex <- demo %>%
  select(src_subject_id, demo_sex_v2) %>%
  rename(sex = demo_sex_v2) %>%
  filter(sex == 2) # keep only females - 5677 remain. 6188 Males and 3 intersex males removed.

# make numeric versions of eventname, will be made into a searchable chronological list vector later
cbcl <- cbcl %>%
  mutate(event_num = case_when(
    eventname == 'baseline_year_1_arm_1' ~ 0,
    eventname == '1_year_follow_up_y_arm_1' ~ 1,
    eventname == '2_year_follow_up_y_arm_1' ~ 2,
    eventname == '3_year_follow_up_y_arm_1' ~ 3,
    eventname == '4_year_follow_up_y_arm_1' ~ 4,))

# remove males
cbcl <- cbcl %>%
  left_join(sex, by = 'src_subject_id') %>%
  filter(sex == 2) %>%
  select(-sex)

# Split internalizing and externalizing
cbcl_int <- cbcl %>%
  select(src_subject_id, eventname, cbcl_scr_syn_internal_r, event_num)
cbcl_ext <- cbcl %>%
  select(src_subject_id, eventname, cbcl_scr_syn_external_r, event_num)

# count subjects
int_subs <- cbcl_int %>% distinct(src_subject_id) %>% pull(src_subject_id) # 5676
ext_subs <- cbcl_ext %>% distinct(src_subject_id) %>% pull(src_subject_id) # 5676

# 1 subject missing, who and why?
all_females <- sex %>% pull(src_subject_id)
int_missing <- setdiff(all_females, int_subs)
ext_missing <- setdiff(all_females, ext_subs)
# NDAR_INVFZWKRBW7, no CBCL data, no parent menarche reports, no postmenarche youth report

# remove sessions with no cbcl data
cbcl_int <- cbcl_int %>%
  # Before: 23199
  filter(!is.na(cbcl_scr_syn_internal_r))
# After: 20730
cbcl_ext <- cbcl_ext %>%
  # Before: 23199
  filter(!is.na(cbcl_scr_syn_external_r))
# After: 20730

# count subjects
int_subs <- cbcl_int %>% distinct(src_subject_id) %>% pull(src_subject_id) # 5676
ext_subs <- cbcl_ext %>% distinct(src_subject_id) %>% pull(src_subject_id) # 5676
# no subjects removed

# Make chronological list vector of sessions with cbcl data
cbcl_int <- cbcl_int %>%
  group_by(src_subject_id) %>%
  mutate(has_cbcl_data = list(event_num))
cbcl_ext <- cbcl_ext %>%
  group_by(src_subject_id) %>%
  mutate(has_cbcl_data = list(event_num))

# pivot cbcl scores and make a chronological list vector of cbcl scores
cbcl_int <- cbcl_int %>%
  pivot_wider(names_from = eventname, values_from = cbcl_scr_syn_internal_r) %>%
  fill(c(`baseline_year_1_arm_1`, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`), .direction = "downup") %>%
  select(-event_num) %>%
  unique() %>%
  group_by(src_subject_id) %>%
  mutate(cbcl_scores = list(c(`baseline_year_1_arm_1`, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)))
cbcl_ext <- cbcl_ext %>%
  pivot_wider(names_from = eventname, values_from = cbcl_scr_syn_external_r) %>%
  fill(c(`baseline_year_1_arm_1`, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`), .direction = "downup") %>%
  select(-event_num) %>%
  unique() %>%
  group_by(src_subject_id) %>%
  mutate(cbcl_scores = list(c(`baseline_year_1_arm_1`, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)))

# keep useful columns
cbcl_int <- cbcl_int %>%
  select(src_subject_id, has_cbcl_data, cbcl_scores)
cbcl_ext <- cbcl_ext %>%
  select(src_subject_id, has_cbcl_data, cbcl_scores)

# merge with menarche data
int_parent <- cbcl_int %>%
  left_join(parent_simple, by = 'src_subject_id')
ext_parent <- cbcl_ext %>%
  left_join(parent_simple, by = 'src_subject_id')
int_youth <- cbcl_int %>%
  left_join(youth_simple, by = 'src_subject_id')
ext_youth <- cbcl_ext %>%
  left_join(youth_simple, by = 'src_subject_id')

#########################
# IN THE BELOW CODE BLOCK
# Find Best-Fit PreMenarche and PostMenarche Sessions with CBCL data
#########################

# define functions to find matching or best-fit event with ple scores
find_pre_cbcl_event <- function(pre_event_name, cbcl_session_list) {
  if (is.na(pre_event_name)) return(NA_integer_) # if no premenarche report
  pre_cbcl <- which(cbcl_session_list <= pre_event_name) # find cbcl scores that happen at or before last premenarche timepoint
  if (length(pre_cbcl) == 0) return(NA_integer_) # no premenarche cbcl scores
  return(tail(pre_cbcl, 1)) # output index of latest premenarche timepoint with a cbcl score
}

find_post_cbcl_event <- function(post_event_name, cbcl_session_list) {
  if (is.na(post_event_name)) return(NA_integer_) # if no postmenarche report
  post_cbcl <- which(cbcl_session_list >= post_event_name) # find cbcl scores that happen at or after first postmenarche timepoint
  if (length(post_cbcl) == 0) return(NA_integer_) # no postmenarche cbcl scores
  return(post_cbcl[1]) # output index of first postmenarche timepoint with a cbcl score
}

# Apply functions, pull event numbers
int_youth <- int_youth %>%
  rowwise() %>%
  mutate(CBCL_PreEvent_index = find_pre_cbcl_event(last_pre_event, has_cbcl_data)) %>%
  mutate(CBCL_PostEvent_index = find_post_cbcl_event(first_post_event, has_cbcl_data)) %>%
  mutate(CBCL_PreEvent = if_else(!is.na(CBCL_PreEvent_index), has_cbcl_data[CBCL_PreEvent_index], NA)) %>%
  mutate(CBCL_PostEvent = if_else(!is.na(CBCL_PostEvent_index), has_cbcl_data[CBCL_PostEvent_index], NA))
ext_youth <- ext_youth %>%
  rowwise() %>%
  mutate(CBCL_PreEvent_index = find_pre_cbcl_event(last_pre_event, has_cbcl_data)) %>%
  mutate(CBCL_PostEvent_index = find_post_cbcl_event(first_post_event, has_cbcl_data)) %>%
  mutate(CBCL_PreEvent = if_else(!is.na(CBCL_PreEvent_index), has_cbcl_data[CBCL_PreEvent_index], NA)) %>%
  mutate(CBCL_PostEvent = if_else(!is.na(CBCL_PostEvent_index), has_cbcl_data[CBCL_PostEvent_index], NA))
int_parent <- int_parent %>%
  rowwise() %>%
  mutate(CBCL_PreEvent_index = find_pre_cbcl_event(last_pre_event, has_cbcl_data)) %>%
  mutate(CBCL_PostEvent_index = find_post_cbcl_event(first_post_event, has_cbcl_data)) %>%
  mutate(CBCL_PreEvent = if_else(!is.na(CBCL_PreEvent_index), has_cbcl_data[CBCL_PreEvent_index], NA)) %>%
  mutate(CBCL_PostEvent = if_else(!is.na(CBCL_PostEvent_index), has_cbcl_data[CBCL_PostEvent_index], NA))
ext_parent <- ext_parent %>%
  rowwise() %>%
  mutate(CBCL_PreEvent_index = find_pre_cbcl_event(last_pre_event, has_cbcl_data)) %>%
  mutate(CBCL_PostEvent_index = find_post_cbcl_event(first_post_event, has_cbcl_data)) %>%
  mutate(CBCL_PreEvent = if_else(!is.na(CBCL_PreEvent_index), has_cbcl_data[CBCL_PreEvent_index], NA)) %>%
  mutate(CBCL_PostEvent = if_else(!is.na(CBCL_PostEvent_index), has_cbcl_data[CBCL_PostEvent_index], NA))

# Pull Scores
int_youth <- int_youth %>%
  rowwise() %>%
  mutate(Pre_CBCL_Score = if_else(!is.na(CBCL_PreEvent), cbcl_scores[CBCL_PreEvent + 1], NA),
         Post_CBCL_Score = if_else(!is.na(CBCL_PostEvent), cbcl_scores[CBCL_PostEvent + 1], NA))
ext_youth <- ext_youth %>%
  rowwise() %>%
  mutate(Pre_CBCL_Score = if_else(!is.na(CBCL_PreEvent), cbcl_scores[CBCL_PreEvent + 1], NA),
         Post_CBCL_Score = if_else(!is.na(CBCL_PostEvent), cbcl_scores[CBCL_PostEvent + 1], NA))
int_parent <- int_parent %>%
  rowwise() %>%
  mutate(Pre_CBCL_Score = if_else(!is.na(CBCL_PreEvent), cbcl_scores[CBCL_PreEvent + 1], NA),
         Post_CBCL_Score = if_else(!is.na(CBCL_PostEvent), cbcl_scores[CBCL_PostEvent + 1], NA))
ext_parent <- ext_parent %>%
  rowwise() %>%
  mutate(Pre_CBCL_Score = if_else(!is.na(CBCL_PreEvent), cbcl_scores[CBCL_PreEvent + 1], NA),
         Post_CBCL_Score = if_else(!is.na(CBCL_PostEvent), cbcl_scores[CBCL_PostEvent + 1], NA))


#########################
# IN THE BELOW CODE BLOCK
# Calculate Pre/Post Difference Scores
#########################

# sanity check -- are there any subjects whose pre/post menarche report timepoints don't have PLE data?
int_youth <- int_youth %>%
  mutate(PreFlag = if_else((is.na(Pre_CBCL_Score) & !is.na(last_pre_event)) |
                             (!is.na(Pre_CBCL_Score) & is.na(last_pre_event)), 1, 0)) %>% # n = 1, no premenarche cbcl scores
  mutate(PostFlag = if_else((is.na(Post_CBCL_Score) & !is.na(first_post_event)) |
                              (!is.na(Post_CBCL_Score) & is.na(first_post_event)), 1, 0)) # n = 41, no postmenarche cbcl scores for any
ext_youth <- ext_youth %>%
  mutate(PreFlag = if_else((is.na(Pre_CBCL_Score) & !is.na(last_pre_event)) |
                             (!is.na(Pre_CBCL_Score) & is.na(last_pre_event)), 1, 0)) %>% # n = 1, no premenarche cbcl scores
  mutate(PostFlag = if_else((is.na(Post_CBCL_Score) & !is.na(first_post_event)) |
                              (!is.na(Post_CBCL_Score) & is.na(first_post_event)), 1, 0)) # n = 41, no postmenarche cbcl scores for any
int_parent <- int_parent %>%
  mutate(PreFlag = if_else((is.na(Pre_CBCL_Score) & !is.na(last_pre_event)) |
                             (!is.na(Pre_CBCL_Score) & is.na(last_pre_event)), 1, 0)) %>% # n = 0
  mutate(PostFlag = if_else((is.na(Post_CBCL_Score) & !is.na(first_post_event)) |
                              (!is.na(Post_CBCL_Score) & is.na(first_post_event)), 1, 0)) # n = 2, no postmenarche cbcl scores for any
ext_parent <- ext_parent %>%
  mutate(PreFlag = if_else((is.na(Pre_CBCL_Score) & !is.na(last_pre_event)) |
                             (!is.na(Pre_CBCL_Score) & is.na(last_pre_event)), 1, 0)) %>% # n = 0
  mutate(PostFlag = if_else((is.na(Post_CBCL_Score) & !is.na(first_post_event)) |
                              (!is.na(Post_CBCL_Score) & is.na(first_post_event)), 1, 0)) # n = 2, no postmenarche cbcl scores for any

# Calculate Change Scores
int_youth <- int_youth %>%
  mutate(ChangeScore = Post_CBCL_Score - Pre_CBCL_Score)
ext_youth <- ext_youth %>%
  mutate(ChangeScore = Post_CBCL_Score - Pre_CBCL_Score)
int_parent <- int_parent %>%
  mutate(ChangeScore = Post_CBCL_Score - Pre_CBCL_Score)
ext_parent <- ext_parent %>%
  mutate(ChangeScore = Post_CBCL_Score - Pre_CBCL_Score)


#########################
# IN THE BELOW CODE BLOCK
# Calculate Gap in Days between Pre and Post Menarche Reports
#########################

## Clean and Prepare Interview Date Data
# Keep only useful columns
dates <- dates %>%
  select(src_subject_id, eventname, interview_date) %>%
  # cut out hms so we can subtract in units of days
  mutate(interviewdate = mdy(interview_date)) %>%
  select(-interview_date) %>%
  # pivot wide so we can make a list of interview dates
  pivot_wider(names_from = eventname, values_from = interviewdate)

# make a list of interview dates
dates <- dates %>%
  rowwise() %>%
  mutate(interviewdates = list(c(`baseline_year_1_arm_1`, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`))) %>%
  select(src_subject_id, interviewdates)

# merge dates in
int_youth <- int_youth %>%
  left_join(dates, by = 'src_subject_id')
ext_youth <- ext_youth %>%
  left_join(dates, by = 'src_subject_id')
int_parent <- int_parent %>%
  left_join(dates, by = 'src_subject_id')
ext_parent <- ext_parent %>%
  left_join(dates, by = 'src_subject_id')

# Number of days between best-fit premenarche and postmenarche scores, saved to “between_scores”
int_youth <- int_youth %>%
  rowwise() %>%
  mutate(between_scores = interviewdates[CBCL_PostEvent + 1] - interviewdates[CBCL_PreEvent + 1])
ext_youth <- ext_youth %>%
  rowwise() %>%
  mutate(between_scores = interviewdates[CBCL_PostEvent + 1] - interviewdates[CBCL_PreEvent + 1])
int_parent <- int_parent %>%
  rowwise() %>%
  mutate(between_scores = interviewdates[CBCL_PostEvent + 1] - interviewdates[CBCL_PreEvent + 1])
ext_parent <- ext_parent %>%
  rowwise() %>%
  mutate(between_scores = interviewdates[CBCL_PostEvent + 1] - interviewdates[CBCL_PreEvent + 1])

# Number of days between last premenarche and first postmenarche reports, saved to “PreMen->PostMen”
int_youth <- int_youth %>%
  rowwise() %>%
  mutate('PreMen->PostMen' = interviewdates[first_post_event + 1] - interviewdates[last_pre_event + 1])
ext_youth <- ext_youth %>%
  rowwise() %>%
  mutate('PreMen->PostMen' = interviewdates[first_post_event + 1] - interviewdates[last_pre_event + 1])
int_parent <- int_parent %>%
  rowwise() %>%
  mutate('PreMen->PostMen' = interviewdates[first_post_event + 1] - interviewdates[last_pre_event + 1])
ext_parent <- ext_parent %>%
  rowwise() %>%
  mutate('PreMen->PostMen' = interviewdates[first_post_event + 1] - interviewdates[last_pre_event + 1])

#	Number of days from best-fit premenarche Score to last premenarche report, saved to “PreMenScore->PreMenReport”
int_youth <- int_youth %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interviewdates[last_pre_event + 1] - interviewdates[CBCL_PreEvent + 1])
ext_youth <- ext_youth %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interviewdates[last_pre_event + 1] - interviewdates[CBCL_PreEvent + 1])
int_parent <- int_parent %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interviewdates[last_pre_event + 1] - interviewdates[CBCL_PreEvent + 1])
ext_parent <- ext_parent %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interviewdates[last_pre_event + 1] - interviewdates[CBCL_PreEvent + 1])

#	Number of days from first postmenarche report to best-fit postmenarche Score, saved to “PostMenReport->PostMenScore”
int_youth <- int_youth %>%
  rowwise() %>%
  mutate('PostMenReport->PostMenScore' = interviewdates[CBCL_PostEvent + 1] - interviewdates[first_post_event + 1])
ext_youth <- ext_youth %>%
  rowwise() %>%
  mutate('PostMenReport->PostMenScore' = interviewdates[CBCL_PostEvent + 1] - interviewdates[first_post_event + 1])
int_parent <- int_parent %>%
  rowwise() %>%
  mutate('PostMenReport->PostMenScore' = interviewdates[CBCL_PostEvent + 1] - interviewdates[first_post_event + 1])
ext_parent <- ext_parent %>%
  rowwise() %>%
  mutate('PostMenReport->PostMenScore' = interviewdates[CBCL_PostEvent + 1] - interviewdates[first_post_event + 1])


#########################
# IN THE BELOW CODE BLOCK
# Clean data for Export
#########################

# convert cbcl_scores to string so it can be written into a csv
int_youth <- int_youth %>%
  mutate(cbcl_scores = paste(cbcl_scores, collapse = ", "))
ext_youth <- ext_youth %>%
  mutate(cbcl_scores = paste(cbcl_scores, collapse = ", "))
int_parent <- int_parent %>%
  mutate(cbcl_scores = paste(cbcl_scores, collapse = ", "))
ext_parent <- ext_parent %>%
  mutate(cbcl_scores = paste(cbcl_scores, collapse = ", "))

# isolate itemized menarche reports for reference
youth <- youth %>%
  select(src_subject_id, `baseline_year_1_arm_1`, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`) %>%
  rename('baseline_menarche_report' = `baseline_year_1_arm_1`,
         'year1_menarche_report' = `1_year_follow_up_y_arm_1`,
         'year2_menarche_report' = `2_year_follow_up_y_arm_1`,
         'year3_menarche_report' = `3_year_follow_up_y_arm_1`,
         'year4_menarche_report' = `4_year_follow_up_y_arm_1`) %>%
  rowwise() %>%
  mutate(menarche_reports = paste(c(baseline_menarche_report, year1_menarche_report, year2_menarche_report, year3_menarche_report, year4_menarche_report), collapse = ", "))
parent <- parent %>%
  select(src_subject_id, `baseline_year_1_arm_1`, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`) %>%
  rename('baseline_menarche_report' = `baseline_year_1_arm_1`,
         'year1_menarche_report' = `1_year_follow_up_y_arm_1`,
         'year2_menarche_report' = `2_year_follow_up_y_arm_1`,
         'year3_menarche_report' = `3_year_follow_up_y_arm_1`,
         'year4_menarche_report' = `4_year_follow_up_y_arm_1`) %>%
  rowwise() %>%
  mutate(menarche_reports = paste(c(baseline_menarche_report, year1_menarche_report, year2_menarche_report, year3_menarche_report, year4_menarche_report), collapse = ", "))

# merge in itemized menarche reports for reference
int_youth <- int_youth %>%
  left_join(youth, by = 'src_subject_id')
ext_youth <- ext_youth %>%
  left_join(youth, by = 'src_subject_id')
int_parent <- int_parent %>%
  left_join(parent, by = 'src_subject_id')
ext_parent <- ext_parent %>%
  left_join(parent, by = 'src_subject_id')

# reorder and keep useful columns
int_youth <- int_youth %>%
  select(src_subject_id, ChangeScore, Pre_CBCL_Score, Post_CBCL_Score, 
         cbcl_scores, menarche_reports, last_pre_event, first_post_event, CBCL_PreEvent, CBCL_PostEvent,
         between_scores, `PreMen->PostMen`, `PreMenScore->PreMenReport`, `PostMenReport->PostMenScore`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0,
         baseline_menarche_report, year1_menarche_report, year2_menarche_report, year3_menarche_report, year4_menarche_report)
ext_youth <- ext_youth %>%
  select(src_subject_id, ChangeScore, Pre_CBCL_Score, Post_CBCL_Score, 
         cbcl_scores, menarche_reports, last_pre_event, first_post_event, CBCL_PreEvent, CBCL_PostEvent,
         between_scores, `PreMen->PostMen`, `PreMenScore->PreMenReport`, `PostMenReport->PostMenScore`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0,
         baseline_menarche_report, year1_menarche_report, year2_menarche_report, year3_menarche_report, year4_menarche_report)
int_parent <- int_parent %>%
  select(src_subject_id, ChangeScore, Pre_CBCL_Score, Post_CBCL_Score, 
         cbcl_scores, menarche_reports, last_pre_event, first_post_event, CBCL_PreEvent, CBCL_PostEvent,
         between_scores, `PreMen->PostMen`, `PreMenScore->PreMenReport`, `PostMenReport->PostMenScore`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0,
         baseline_menarche_report, year1_menarche_report, year2_menarche_report, year3_menarche_report, year4_menarche_report)
ext_parent <- ext_parent %>%
  select(src_subject_id, ChangeScore, Pre_CBCL_Score, Post_CBCL_Score, 
         cbcl_scores, menarche_reports, last_pre_event, first_post_event, CBCL_PreEvent, CBCL_PostEvent,
         between_scores, `PreMen->PostMen`, `PreMenScore->PreMenReport`, `PostMenReport->PostMenScore`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0,
         baseline_menarche_report, year1_menarche_report, year2_menarche_report, year3_menarche_report, year4_menarche_report)

#########################
# IN THE BELOW CODE BLOCK
# Export to csv
#########################

write_csv(int_youth, paste0(derivative_data, 'Youth_5.0_PrePost_IntCBCL_12_18_25.csv'))
write_csv(ext_youth, paste0(derivative_data, 'Youth_5.0_PrePost_ExtCBCL_12_18_25.csv'))
write_csv(int_parent, paste0(derivative_data, 'Parent_5.0_PrePost_IntCBCL_12_18_25.csv'))
write_csv(ext_parent, paste0(derivative_data, 'Parent_5.0_PrePost_ExtCBCL_12_18_25.csv'))

#########################
# IN THE BELOW CODE BLOCK
# Get Counts
#########################

# Make counting dataframes of those with both valid pre/post menarche timepoints
int_y_counter <- int_youth %>%
  filter(!is.na(last_pre_event) & !is.na(first_post_event)) %>%
  mutate(has_changescore = !is.na(ChangeScore))
ext_y_counter <- ext_youth %>%
  filter(!is.na(last_pre_event) & !is.na(first_post_event)) %>%
  mutate(has_changescore = !is.na(ChangeScore))
int_p_counter <- int_parent %>%
  filter(!is.na(last_pre_event) & !is.na(first_post_event)) %>%
  mutate(has_changescore = !is.na(ChangeScore))
ext_p_counter <- ext_parent %>%
  filter(!is.na(last_pre_event) & !is.na(first_post_event)) %>%
  mutate(has_changescore = !is.na(ChangeScore))

table(int_y_counter$has_changescore) # 3053 with, 780 without; 3833 total
table(ext_y_counter$has_changescore) # 3053 with, 780 without; 3833 total
table(int_p_counter$has_changescore) # 3217 with, 794 without; 4011 total
table(ext_p_counter$has_changescore) # 3217 with, 794 without; 4011 total

# Youth: 3833 -> 3053
# Parent: 4011 -> 3217

# determine reasons for no changescore
int_y_excluded <- int_y_counter %>% filter(has_changescore == FALSE) # 780 subjects, 779 no PostMenarche CBCL data, 1 no PreMenarche CBCL data
ext_y_excluded <- ext_y_counter %>% filter(has_changescore == FALSE) # 780 subjects, 779 no PostMenarche CBCL data, 1 no PreMenarche CBCL data
int_p_excluded <- int_p_counter %>% filter(has_changescore == FALSE) # 794 subjects, no PostMenarche CBCL Data for all
ext_p_excluded <- ext_p_counter %>% filter(has_changescore == FALSE) # 794 subjects, no PostMenarche CBCL Data for all

