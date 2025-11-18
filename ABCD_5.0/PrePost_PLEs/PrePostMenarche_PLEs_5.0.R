##########################
# PrePostMenarche_PLEs_5.0.R
# Author: Robert Toms
# Date: 11/17/2025
##########################

# The purpose of this code is to calculate changes in PLEs from pre to post menarche.

# libraries
library(tidyverse)
library(lubridate)

# define paths
sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"

# import data
parent <- read_csv(paste0(derivative_data, '5.0_Parent_PrePost_Menarche_11_17_25.csv'))
youth <- read_csv(paste0(derivative_data, '5.0_Youth_PrePost_Menarche_11_17_25.csv'))
ples <- read_csv(paste0(sourcepath, '/mental-health/mh_y_pps.csv')) 
dates <- read_csv(paste0(sourcepath, '/abcd-general/abcd_y_lt.csv'))

#########################
# IN THE BELOW CODE BLOCK
# Prepare data
#########################

# keep only useful columns
parent_simple <- parent %>%
  select(src_subject_id, last_pre_event, first_post_event, PostMenarche_at_Baseline_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)
youth_simple <- youth %>%
  select(src_subject_id, last_pre_event, first_post_event, PostMenarche_at_Baseline_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)
ples <- ples %>%
  select(src_subject_id, eventname, pps_y_ss_severity_score)

# make list vector of sessions with ple data
ples <- ples %>%
  mutate(eventnum = case_when(
    eventname == 'baseline_year_1_arm_1' ~ 0,
    eventname == '1_year_follow_up_y_arm_1' ~ 1,
    eventname == '2_year_follow_up_y_arm_1' ~ 2,
    eventname == '3_year_follow_up_y_arm_1' ~ 3,
    eventname == '4_year_follow_up_y_arm_1' ~ 4)) %>%
  # remove rows with no ple data (n = 4)
  filter(!is.na(pps_y_ss_severity_score)) %>%
  group_by(src_subject_id) %>%
  mutate(has_ple_data = list(eventnum)) %>%
  select(-eventnum)

# Parent: 4011 subjects with Pre/Post Menarche Timepoints
# Youth: 3835 subjects with Pre/Post Menarche Timepoints

# pivot ple scores and make a chronological list vector of ple scores
ples <- ples %>%
  pivot_wider(names_from = eventname, values_from = pps_y_ss_severity_score)
ples <- ples %>%
  group_by(src_subject_id) %>%
  mutate(ple_scores = list(c(baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`))) %>%
  select(src_subject_id, ple_scores, has_ple_data)

# merge ple and menarche data
parent_ples <- parent_simple %>%
  left_join(ples, by = 'src_subject_id')
youth_ples <- youth_simple %>%
  left_join(ples, by = 'src_subject_id')

#########################
# IN THE BELOW CODE BLOCK
# Find Best-Fit PreMenarche and PostMenarche Sessions with PLE data
#########################

# define functions to find matching or best-fit sessions with ple scores
find_pre_ple_event <- function(pre_event_num, ple_event_list) {
  if (is.na(pre_event_num)) return(NA_integer_) # if no premenarche report
  pre_ple <- which(ple_event_list <= pre_event_num) # find ple scores that happen at or before last premenarche timepoint
  if (length(pre_ple) == 0) return(NA_integer_) # no premenarche PLE scores
  return(tail(pre_ple, 1)) # output index of latest premenarche timepoint with a ple score
}

find_post_ple_event <- function(post_event_num, ple_event_list) {
  if (is.na(post_event_num)) return(NA_integer_) # if no postmenarche report
  post_ple <- which(ple_event_list >= post_event_num) # find ple scores that happen at or after first postmenarche timepoint
  if (length(post_ple) == 0) return(NA_integer_) # no postmenarche PLE scores
  return(post_ple[1]) # output index of first postmenarche timepoint with a ple score
}

# Apply functions, pull session numbers
parent_ples <- parent_ples %>%
  rowwise() %>%
  mutate(PLE_PreEvent_index = find_pre_ple_event(last_pre_event, has_ple_data)) %>%
  mutate(PLE_PostEvent_index = find_post_ple_event(first_post_event, has_ple_data)) %>%
  mutate(PLE_PreEvent = if_else(!is.na(PLE_PreEvent_index), has_ple_data[PLE_PreEvent_index], NA)) %>%
  mutate(PLE_PostEvent = if_else(!is.na(PLE_PostEvent_index), has_ple_data[PLE_PostEvent_index], NA))
youth_ples <- youth_ples %>%
  rowwise() %>%
  mutate(PLE_PreEvent_index = find_pre_ple_event(last_pre_event, has_ple_data)) %>%
  mutate(PLE_PostEvent_index = find_post_ple_event(first_post_event, has_ple_data)) %>%
  mutate(PLE_PreEvent = if_else(!is.na(PLE_PreEvent_index), has_ple_data[PLE_PreEvent_index], NA)) %>%
  mutate(PLE_PostEvent = if_else(!is.na(PLE_PostEvent_index), has_ple_data[PLE_PostEvent_index], NA))

# Pull Scores
parent_ples <- parent_ples %>%
  rowwise() %>%
  mutate(Pre_PLE_Score = if_else(!is.na(PLE_PreEvent), ple_scores[PLE_PreEvent + 1], NA),
         Post_PLE_Score = if_else(!is.na(PLE_PostEvent), ple_scores[PLE_PostEvent + 1], NA))
youth_ples <- youth_ples %>%
  rowwise() %>%
  mutate(Pre_PLE_Score = if_else(!is.na(PLE_PreEvent), ple_scores[PLE_PreEvent + 1], NA),
         Post_PLE_Score = if_else(!is.na(PLE_PostEvent), ple_scores[PLE_PostEvent + 1], NA))

#########################
# IN THE BELOW CODE BLOCK
# Calculate Pre/Post Difference Scores
#########################


# sanity check -- are there any subjects whose pre/post menarche report timepoints don't have PLE data?
parent_ples <- parent_ples %>%
  mutate(PreFlag = if_else((is.na(Pre_PLE_Score) & !is.na(last_pre_event)) |
                             (!is.na(Pre_PLE_Score) & is.na(last_pre_event)), 1, 0)) %>%
  mutate(PostFlag = if_else((is.na(Post_PLE_Score) & !is.na(first_post_event)) |
                              (!is.na(Post_PLE_Score) & is.na(first_post_event)), 1, 0))
youth_ples <- youth_ples %>%
  mutate(PreFlag = if_else((is.na(Pre_PLE_Score) & !is.na(last_pre_event)) |
                             (!is.na(Pre_PLE_Score) & is.na(last_pre_event)), 1, 0)) %>%
  mutate(PostFlag = if_else((is.na(Post_PLE_Score) & !is.na(first_post_event)) |
                              (!is.na(Post_PLE_Score) & is.na(first_post_event)), 1, 0))

# Parent: n = 0 premenarche flags, n = 2 postmenarche flags, both are postmenarche after last ple score report
# Youth: n = 1 premenarche flag, missing any premenarche PLE data, n = 3 postmenarche flag, all are postmenarche after last ple score report

### Calculate Pre/Post Difference
parent_ples <- parent_ples %>%
  mutate(ChangeScore = Post_PLE_Score - Pre_PLE_Score)
youth_ples <- youth_ples %>%
  mutate(ChangeScore = Post_PLE_Score - Pre_PLE_Score)

#########################
# IN THE BELOW CODE BLOCK
# Calculate Gap in Days between Pre and Post Menarche Reports
#########################

## Clean and Prepare Interview Date Data
# Keep only
dates <- dates %>%
  select(src_subject_id, eventname, interview_date) %>%
  # convert to mdy so they can be subtracted
  mutate(interview_date = mdy(interview_date)) %>%
  # pivot wide so we can make a list of interview dates
  pivot_wider(names_from = eventname, values_from = interview_date)

# make a list of useful interview dates
dates <- dates %>%
  rowwise() %>%
  mutate(interview_dates = list(c(baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, 
                                  `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`))) %>%
  select(src_subject_id, interview_dates)

## merge data
parent_ples_dates <- parent_ples %>%
  left_join(dates, by = 'src_subject_id')
youth_ples_dates <- youth_ples %>%
  left_join(dates, by = 'src_subject_id')

# Number of days between best-fit premenarche and postmenarche scores, saved to “between_scores”
parent_ples_dates <- parent_ples_dates %>%
  rowwise() %>%
  mutate(between_scores = interview_dates[PLE_PostEvent + 1] - interview_dates[PLE_PreEvent + 1])
youth_ples_dates <- youth_ples_dates %>%
  rowwise() %>%
  mutate(between_scores = interview_dates[PLE_PostEvent + 1] - interview_dates[PLE_PreEvent + 1])

# Number of days between last premenarche and first postmenarche reports, saved to “PreMen->PostMen”
parent_ples_dates <- parent_ples_dates %>%
  rowwise() %>%
  mutate('PreMen->PostMen' = interview_dates[first_post_event + 1] - interview_dates[last_pre_event + 1])
youth_ples_dates <- youth_ples_dates %>%
  rowwise() %>%
  mutate('PreMen->PostMen' = interview_dates[first_post_event + 1] - interview_dates[last_pre_event + 1])

#	Number of days from best-fit premenarche Score to last premenarche report, saved to “PreMenScore->PreMenReport”
parent_ples_dates <- parent_ples_dates %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interview_dates[last_pre_event + 1] - interview_dates[PLE_PreEvent + 1])
youth_ples_dates <- youth_ples_dates %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interview_dates[last_pre_event + 1] - interview_dates[PLE_PreEvent + 1])

#	Number of days from first postmenarche report to best-fit postmenarche Score, saved to “PostMenReport->PostMenScore”
parent_ples_dates <- parent_ples_dates %>%
  rowwise() %>%
  mutate('PostMenReport->PostMenScore' = interview_dates[PLE_PostEvent + 1] - interview_dates[first_post_event + 1])
youth_ples_dates <- youth_ples_dates %>%
  rowwise() %>%
  mutate('PostMenReport->PostMenScore' = interview_dates[PLE_PostEvent + 1] - interview_dates[first_post_event + 1])

#########################
# IN THE BELOW CODE BLOCK
# Clean data for Export
#########################

# convert ple_scores to string so it can be written into a csv
parent_ples_dates <- parent_ples_dates %>%
  mutate(ple_scores = paste(ple_scores, collapse = ", "))
youth_ples_dates <- youth_ples_dates %>%
  mutate(ple_scores = paste(ple_scores, collapse = ", "))

# reorder and keep useful columns
parent_ples_dates <- parent_ples_dates %>%
  select(src_subject_id, Pre_PLE_Score, Post_PLE_Score, ChangeScore, last_pre_event, first_post_event,
         ple_scores, PLE_PreEvent, PLE_PostEvent,
         between_scores, `PreMen->PostMen`, `PreMenScore->PreMenReport`, `PostMenReport->PostMenScore`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0)
youth_ples_dates <- youth_ples_dates %>%
  select(src_subject_id, Pre_PLE_Score, Post_PLE_Score, ChangeScore, last_pre_event, first_post_event,
         ple_scores, PLE_PreEvent, PLE_PostEvent,
         between_scores, `PreMen->PostMen`, `PreMenScore->PreMenReport`, `PostMenReport->PostMenScore`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0)

# isolate itemized menarche reports for reference
youth <- youth %>%
  select(src_subject_id, baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, 
         `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`) %>%
  rename('baseline_menarche_report' = `baseline_year_1_arm_1`,
         'year1_menarche_report' = `1_year_follow_up_y_arm_1`,
         'year2_menarche_report' = `2_year_follow_up_y_arm_1`,
         'year3_menarche_report' = `3_year_follow_up_y_arm_1`,
         'year4_menarche_report' = `4_year_follow_up_y_arm_1`)
parent <- parent %>%
  select(src_subject_id, baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, 
         `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`) %>%
  rename('baseline_menarche_report' = `baseline_year_1_arm_1`,
         'year1_menarche_report' = `1_year_follow_up_y_arm_1`,
         'year2_menarche_report' = `2_year_follow_up_y_arm_1`,
         'year3_menarche_report' = `3_year_follow_up_y_arm_1`,
         'year4_menarche_report' = `4_year_follow_up_y_arm_1`)

# merge in itemized menarche reports for reference
parent_ples_dates <- parent_ples_dates %>%
  left_join(parent, by = 'src_subject_id')
youth_ples_dates <- youth_ples_dates %>%
  left_join(youth, by = 'src_subject_id')

# Count Pre/Post Menarche PLE data availability
p_counter <- parent_ples %>%
  mutate(has_changescore = !is.na(ChangeScore))
table(p_counter$has_changescore) # 4009

y_counter <- youth_ples %>%
  mutate(has_changescore = !is.na(ChangeScore))
table(y_counter$has_changescore) # 3831

#########################
# IN THE BELOW CODE BLOCK
# Export to csv
#########################

write_csv(parent_ples_dates, paste0(derivative_data, 'Parent_PrePost_PLEs_11_17_25.csv'))
write_csv(youth_ples_dates, paste0(derivative_data, 'Youth_PrePost_PLEs_11_17_25.csv'))
