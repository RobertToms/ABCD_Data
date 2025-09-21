############################
# PrePost_NIH_ListSorting.R
# Author: Robert Toms
# Date: 9/16/2025
############################

# The purpose of this code is to match Pre & Post menarche timepoints for each subject 
#             with available NIH Toolbox data for the List Sorting Working Memory Task.

library(tidyverse)
library(lubridate)

sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/"

# Pre/Post Menarche Timepoint data
### These are made via PrePost_Menarche.R ###
parent <- read_csv(paste0(derivative_data, 'Parent_PrePost_Menarche_9_15_25.csv'))
youth <- read_csv(paste0(derivative_data, 'Youth_PrePost_Menarche_9_15_25.csv'))

# NIH Toolbox Data
nih <- read_tsv(paste0(sourcepath, 'Neurocognition/nc_y_nihtb.tsv'))

# Interview datetimes
dates <- read_tsv(paste0(sourcepath, 'abcd_general/ab_g_dyn.tsv'))

#################
# IN THE BELOW CODE BLOCK
# Clean and Prepare Data
#################

# keep useful columns, rename if necessary
nih <- nih %>%
  select(participant_id, session_id,
         nc_y_nihtb__lswmt_dtt,            # datetime of when list-sorting task was completed 
         nc_y_nihtb__lswmt__agecor_score)  # NIH Toolbox, List Sorting Working Memory Task, age corrected score

# also convert data type from character to mdy date, so they can be subtracted from each other
dates <- dates %>%
  mutate(interview_date = as.Date(ab_g_dyn__visit_dtt)) %>%
  select(participant_id, session_id, interview_date)

# convert 'n/a' to NA
nih <- nih %>%
  mutate(across(c(nc_y_nihtb__lswmt_dtt, nc_y_nihtb__lswmt__agecor_score), 
                ~if_else(. == 'n/a', NA, .)))

# remove sessions with no score data
nih <- nih %>%
  # Before: 37308 timepoints
  filter(!is.na(nc_y_nihtb__lswmt__agecor_score))
  # After: 25842 timepoints

# make list vectors of sessions with score data
nih <- nih %>%
  mutate(session_num = case_when(
    session_id == 'ses-00A' ~ 0,
    session_id == 'ses-02A' ~ 2,
    session_id == 'ses-04A' ~ 4,
    session_id == 'ses-06A' ~ 6)) %>%
    group_by(participant_id) %>%
  mutate(has_scores = list(session_num))

# make list vector of interview dates for menarche report dates
dates_wide <- dates %>%
  pivot_wider(names_from = session_id, values_from = interview_date) %>%
  rowwise() %>%
  mutate(interview_dates = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)))

# pivot datetimes and scores wide
nih <- nih %>%
  mutate(ses_id = session_id) %>%
  pivot_wider(names_from = ses_id, values_from = nc_y_nihtb__lswmt_dtt) %>%
  rename(ses_00A_date = 'ses-00A',
         ses_02A_date = 'ses-02A',
         ses_04A_date = 'ses-04A',
         ses_06A_date = 'ses-06A')
nih <- nih %>%
  pivot_wider(names_from = session_id, values_from = nc_y_nihtb__lswmt__agecor_score) %>%
  rename(ses_00A_score = 'ses-00A',
         ses_02A_score = 'ses-02A',
         ses_04A_score = 'ses-04A',
         ses_06A_score = 'ses-06A')

# format 
nih <- nih %>%
  group_by(participant_id) %>%
  fill(c(ses_00A_date, ses_02A_date, ses_04A_date, ses_06A_date, ses_00A_score, ses_02A_score, ses_04A_score, ses_06A_score), .direction = "downup") %>%
  select(-session_num) %>%
  unique()

# remove time from datetime, so dates are subtractable from other dates
nih <- nih %>%
  mutate('00A_date' = as.Date(ses_00A_date),
       '02A_date' = as.Date(ses_02A_date),
       '04A_date' = as.Date(ses_04A_date),
       '06A_date' = as.Date(ses_06A_date))

# reorder to put matching scores and datetimes next to each other
nih <- nih %>%
  select(participant_id, has_scores, 
         '00A_date', ses_00A_score, '02A_date', ses_02A_score, '04A_date', ses_04A_score, '06A_date', ses_06A_score)

# merge interview dates and menarche data to get interview list vectors in the same place as menarche data
dates_wide <- dates_wide %>%
  select(participant_id, interview_dates)
parent <- parent %>%
  left_join(dates_wide, by = 'participant_id')
youth <- youth %>%
  left_join(dates_wide, by = 'participant_id')

#################
# IN THE BELOW CODE BLOCK
# Merge NIH Data with Menarche Data
#################

parent_nih <- parent %>%
  select(participant_id, last_pre_session, first_post_session, interview_dates,
         PostMenarche_at_Baseline_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0) %>%
  left_join(nih, by = 'participant_id')

youth_nih <- youth %>%
  select(participant_id, last_pre_session, first_post_session, interview_dates,
         PostMenarche_at_Baseline_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0) %>%
  left_join(nih, by = 'participant_id')

#################
# IN THE BELOW CODE BLOCK
# Find PreMenarche and PostMenarche Scores
#################

# Write a function that finds matching or best-fit premenarche score
find_premenarche_score <- function(premenarche_tp, nih_scores) {
  if (is.na(premenarche_tp)) return(NA_integer_) # No premenarche timepoint
  premenarche <- which(nih_scores <= premenarche_tp) # Find the latest scan that happens at or before last premenarche timepoint
  if (length(premenarche) == 0) return(NA_integer_) # No premenarche scores
  return(tail(premenarche, 1))
}

# Write a function that finds matching or best-fit postmenarche score
find_postmenarche_score <- function(postmenarche_tp, nih_scores) {
  if (is.na(postmenarche_tp)) return(NA_integer_) # No postmenarche timepoint
  postmenarche <- which(nih_scores >= postmenarche_tp) # Find first scan that happens at or after first postmenarche timepoint
  if (length(postmenarche) == 0) return(NA_integer_) # No postmenarche scores
  return(postmenarche[1])
}

# Apply functions
parent_nih <- parent_nih %>%
  rowwise() %>%
  mutate(PreScore_index = find_premenarche_score(last_pre_session, has_scores)) %>%
  mutate(PostScore_index = find_postmenarche_score(first_post_session, has_scores))
youth_nih <- youth_nih %>%
  rowwise() %>%
  mutate(PreScore_index = find_premenarche_score(last_pre_session, has_scores)) %>%
  mutate(PostScore_index = find_postmenarche_score(first_post_session, has_scores))

# remove subject(s) with menarche data but no usable scores
parent_nih <- parent_nih %>%
  filter(length(has_scores) > 0) # 5677 -> 5657
youth_nih <- youth_nih %>%
  filter(length(has_scores) > 0) # 5678 -> 5658

# Get Actual session #s
parent_nih <- parent_nih %>%
  rowwise() %>%
  mutate(PreScore_session = has_scores[PreScore_index],
         PostScore_session = has_scores[PostScore_index])
youth_nih <- youth_nih %>%
  rowwise() %>%
  mutate(PreScore_session = has_scores[PreScore_index],
         PostScore_session = has_scores[PostScore_index])

# Pull Scores
parent_nih <- parent_nih %>%
  mutate(PreScore = case_when(
    PreScore_session == 0 ~ ses_00A_score,
    PreScore_session == 2 ~ ses_02A_score,
    PreScore_session == 4 ~ ses_04A_score,
    PreScore_session == 6 ~ ses_06A_score)) %>%
  mutate(PostScore = case_when(
    PostScore_session == 0 ~ ses_00A_score,
    PostScore_session == 2 ~ ses_02A_score,
    PostScore_session == 4 ~ ses_04A_score,
    PostScore_session == 6 ~ ses_06A_score))

youth_nih <- youth_nih %>%
  mutate(PreScore = case_when(
    PreScore_session == 0 ~ ses_00A_score,
    PreScore_session == 2 ~ ses_02A_score,
    PreScore_session == 4 ~ ses_04A_score,
    PreScore_session == 6 ~ ses_06A_score)) %>%
  mutate(PostScore = case_when(
    PostScore_session == 0 ~ ses_00A_score,
    PostScore_session == 2 ~ ses_02A_score,
    PostScore_session == 4 ~ ses_04A_score,
    PostScore_session == 6 ~ ses_06A_score))

# Calculate Change in Score
parent_nih <- parent_nih %>%
  rowwise() %>%
  mutate(ChangeScore = as.numeric(PostScore) - as.numeric(PreScore))
youth_nih <- youth_nih %>%
  rowwise() %>%
  mutate(ChangeScore = as.numeric(PostScore) - as.numeric(PreScore))

#################
# IN THE BELOW CODE BLOCK
# Calculate Days between Data collection and/or Days between menarche report and data collection
#################

# Find PreScore and PostScore dates
parent_nih <- parent_nih %>%
  mutate(PreScore_date = case_when(
    PreScore_session == 0 ~ `00A_date`,
    PreScore_session == 2 ~ `02A_date`,
    PreScore_session == 4 ~ `04A_date`,
    PreScore_session == 6 ~ `06A_date`),
    PostScore_date = case_when(
      PostScore_session == 0 ~ `00A_date`,
      PostScore_session == 2 ~ `02A_date`,
      PostScore_session == 4 ~ `04A_date`,
      PostScore_session == 6 ~ `06A_date`))
youth_nih <- youth_nih %>%
  mutate(PreScore_date = case_when(
    PreScore_session == 0 ~ `00A_date`,
    PreScore_session == 2 ~ `02A_date`,
    PreScore_session == 4 ~ `04A_date`,
    PreScore_session == 6 ~ `06A_date`),
    PostScore_date = case_when(
      PostScore_session == 0 ~ `00A_date`,
      PostScore_session == 2 ~ `02A_date`,
      PostScore_session == 4 ~ `04A_date`,
      PostScore_session == 6 ~ `06A_date`))

# Number of days between best-fit premenarche and postmenarche scores, saved to “between_scores”
parent_nih <- parent_nih %>%
  rowwise() %>%
  mutate(between_scores = PostScore_date - PreScore_date)
youth_nih <- youth_nih %>%
  rowwise() %>%
  mutate(between_scores = PostScore_date - PreScore_date)

# Number of days between last premenarche and first postmenarche reports, saved to “PreMen->PostMen”
parent_nih <- parent_nih %>%
  rowwise() %>%
  mutate('PreMen->PostMen' = interview_dates[first_post_session + 1] - interview_dates[last_pre_session + 1])
youth_nih <- youth_nih %>%
  rowwise() %>%
  mutate('PreMen->PostMen' = interview_dates[first_post_session + 1] - interview_dates[last_pre_session + 1])

#	Number of days from best-fit premenarche Score to last premenarche report, saved to “PreMenScore->PreMenReport”
parent_nih <- parent_nih %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interview_dates[last_pre_session + 1] - PreScore_date)
youth_nih <- youth_nih %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interview_dates[last_pre_session + 1] - PreScore_date)
  
#	Number of days from first postmenarche report to best-fit postmenarche Score, saved to “PostMenReport->PostMenScore”
parent_nih <- parent_nih %>%
    rowwise() %>%
    mutate('PostMenReport->PostMenScore' = PostScore_date - interview_dates[first_post_session + 1])
youth_nih <- youth_nih %>%
    rowwise() %>%
    mutate('PostMenReport->PostMenScore' = PostScore_date - interview_dates[first_post_session + 1])
  

#################
# IN THE BELOW CODE BLOCK
# Clean/Reorder dataframes for CSV Export
#################

# convert has_scores to string so it can be written into a csv
parent_nih <- parent_nih %>%
  mutate(has_scores = paste(has_scores, collapse = ", "))
youth_nih <- youth_nih %>%
  mutate(has_scores = paste(has_scores, collapse = ", "))

# keep and order necessary columns
parent_nih <- parent_nih %>%
  select(participant_id, PreScore, PostScore, ChangeScore, last_pre_session, first_post_session,
         has_scores, PreScore_session, PostScore_session,
         between_scores, `PreMen->PostMen`, `PreMenScore->PreMenReport`, `PostMenReport->PostMenScore`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0,
         ses_00A_score, `00A_date`, ses_02A_score, `02A_date`, ses_04A_score, `04A_date`, ses_06A_score, `06A_date`)
youth_nih <- youth_nih %>%
  select(participant_id, PreScore, PostScore, ChangeScore, last_pre_session, first_post_session,
         has_scores, PreScore_session, PostScore_session,
         between_scores, `PreMen->PostMen`, `PreMenScore->PreMenReport`, `PostMenReport->PostMenScore`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0,
         ses_00A_score, `00A_date`, ses_02A_score, `02A_date`, ses_04A_score, `04A_date`, ses_06A_score, `06A_date`)

# isolate itemized menarche reports for reference
youth <- youth %>%
  select(participant_id, `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`) %>%
  rename('00A_menarche_report' = `ses-00A`,
         '01A_menarche_report' = `ses-01A`,
         '02A_menarche_report' = `ses-02A`,
         '03A_menarche_report' = `ses-03A`,
         '04A_menarche_report' = `ses-04A`,
         '05A_menarche_report' = `ses-05A`,
         '06A_menarche_report' = `ses-06A`)
parent <- parent %>%
  select(participant_id, `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`) %>%
  rename('00A_menarche_report' = `ses-00A`,
         '01A_menarche_report' = `ses-01A`,
         '02A_menarche_report' = `ses-02A`,
         '03A_menarche_report' = `ses-03A`,
         '04A_menarche_report' = `ses-04A`,
         '05A_menarche_report' = `ses-05A`,
         '06A_menarche_report' = `ses-06A`)

# add in itemized menarche reports for reference
parent_nih <- parent_nih %>%
  left_join(parent, by = 'participant_id')
youth_nih <- youth_nih %>%
  left_join(youth, by = 'participant_id')


#################
# IN THE BELOW CODE BLOCK
# Write to CSV
#################

write_csv(parent_nih, paste0(derivative_data, 'Parent_PrePost_NIHtbx_Menarche_9_16_25.csv'))
write_csv(youth_nih, paste0(derivative_data, 'Youth_PrePost_NIHtbx_Menarche_9_16_25.csv'))
