##########################
# PrePostMenarche_PLEs.R
# Author: Robert Toms
# Date: 9/17/2025
##########################

# The purpose of this code is to calculate changes in PLEs from pre to post menarche.

# libraries
library(tidyverse)
library(lubridate)

# define paths
sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/"

# import data
### These are made via PrePost_Menarche.R ###
parent <- read_csv(paste0(derivative_data, 'Parent_PrePost_Menarche_9_15_25.csv'))
youth <- read_csv(paste0(derivative_data, 'Youth_PrePost_Menarche_9_15_25.csv'))

ples <- read_tsv(paste0(sourcepath, '/Mental_Health/mh_y_pps.tsv')) 
dates <- read_tsv(paste0(sourcepath, '/abcd_general/ab_g_dyn.tsv'))

#########################
# IN THE BELOW CODE BLOCK
# Prepare data
#########################

# keep only useful columns
parent_simple <- parent %>%
  select(participant_id, last_pre_session, first_post_session, PostMenarche_at_Baseline_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)
youth_simple <- youth %>%
  select(participant_id, last_pre_session, first_post_session, PostMenarche_at_Baseline_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)
ples <- ples %>%
  select(participant_id, session_id, mh_y_pps__severity_score)

# make list vector of sessions with ple data
ples <- ples %>%
  mutate(sesnum = case_when(
      session_id == 'ses-00A' ~ 0,
      session_id == 'ses-01A' ~ 1,
      session_id == 'ses-02A' ~ 2,
      session_id == 'ses-03A' ~ 3,
      session_id == 'ses-04A' ~ 4,
      session_id == 'ses-05A' ~ 5,
      session_id == 'ses-06A' ~ 6)) %>%
  # remove rows with no ple data (n = 4)
  filter(!is.na(mh_y_pps__severity_score)) %>%
  group_by(participant_id) %>%
  mutate(has_ple_data = list(sesnum)) %>%
  select(-sesnum)

# pivot ple scores and make a chronological list vector of ple scores
ples <- ples %>%
  pivot_wider(names_from = session_id, values_from = mh_y_pps__severity_score)
ples <- ples %>%
  group_by(participant_id) %>%
  mutate(ple_scores = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`))) %>%
  select(participant_id, ple_scores, has_ple_data)

# merge ple and menarche data
parent_ples <- parent_simple %>%
  left_join(ples, by = 'participant_id')
youth_ples <- youth_simple %>%
  left_join(ples, by = 'participant_id')

#########################
# IN THE BELOW CODE BLOCK
# Find Best-Fit PreMenarche and PostMenarche Sessions with PLE data
#########################

# define functions to find matching or best-fit sessions with ple scores
find_pre_ple_session <- function(pre_session_num, ple_session_list) {
  if (is.na(pre_session_num)) return(NA_integer_) # if no premenarche report
  pre_ple <- which(ple_session_list <= pre_session_num) # find ple scores that happen at or before last premenarche timepoint
  if (length(pre_ple) == 0) return(NA_integer_) # no premenarche PLE scores
  return(tail(pre_ple, 1)) # output index of latest premenarche timepoint with a ple score
}

find_post_ple_session <- function(post_session_num, ple_session_list) {
  if (is.na(post_session_num)) return(NA_integer_) # if no postmenarche report
  post_ple <- which(ple_session_list >= post_session_num) # find ple scores that happen at or after first postmenarche timepoint
  if (length(post_ple) == 0) return(NA_integer_) # no postmenarche PLE scores
  return(post_ple[1]) # output index of first postmenarche timepoint with a ple score
}

# Apply functions, pull session numbers
parent_ples <- parent_ples %>%
  rowwise() %>%
  mutate(PLE_PreSession_index = find_pre_ple_session(last_pre_session, has_ple_data)) %>%
  mutate(PLE_PostSession_index = find_post_ple_session(first_post_session, has_ple_data)) %>%
  mutate(PLE_PreSession = if_else(!is.na(PLE_PreSession_index), has_ple_data[PLE_PreSession_index], NA)) %>%
  mutate(PLE_PostSession = if_else(!is.na(PLE_PostSession_index), has_ple_data[PLE_PostSession_index], NA))
youth_ples <- youth_ples %>%
  rowwise() %>%
  mutate(PLE_PreSession_index = find_pre_ple_session(last_pre_session, has_ple_data)) %>%
  mutate(PLE_PostSession_index = find_post_ple_session(first_post_session, has_ple_data)) %>%
  mutate(PLE_PreSession = if_else(!is.na(PLE_PreSession_index), has_ple_data[PLE_PreSession_index], NA)) %>%
  mutate(PLE_PostSession = if_else(!is.na(PLE_PostSession_index), has_ple_data[PLE_PostSession_index], NA))

# Pull Scores
parent_ples <- parent_ples %>%
  rowwise() %>%
  mutate(Pre_PLE_Score = if_else(!is.na(PLE_PreSession), ple_scores[PLE_PreSession + 1], NA),
         Post_PLE_Score = if_else(!is.na(PLE_PostSession), ple_scores[PLE_PostSession + 1], NA))
youth_ples <- youth_ples %>%
  rowwise() %>%
  mutate(Pre_PLE_Score = if_else(!is.na(PLE_PreSession), ple_scores[PLE_PreSession + 1], NA),
         Post_PLE_Score = if_else(!is.na(PLE_PostSession), ple_scores[PLE_PostSession + 1], NA))

#########################
# IN THE BELOW CODE BLOCK
# Calculate Pre/Post Difference Scores
#########################


# sanity check -- are there any subjects whose pre/post menarche report timepoints don't have PLE data?
parent_ples <- parent_ples %>%
  mutate(PreFlag = if_else((is.na(Pre_PLE_Score) & !is.na(last_pre_session)) |
                           (!is.na(Pre_PLE_Score) & is.na(last_pre_session)), 1, 0)) %>%
  mutate(PostFlag = if_else((is.na(Post_PLE_Score) & !is.na(first_post_session)) |
                           (!is.na(Post_PLE_Score) & is.na(first_post_session)), 1, 0))
youth_ples <- youth_ples %>%
  mutate(PreFlag = if_else((is.na(Pre_PLE_Score) & !is.na(last_pre_session)) |
                             (!is.na(Pre_PLE_Score) & is.na(last_pre_session)), 1, 0)) %>%
  mutate(PostFlag = if_else((is.na(Post_PLE_Score) & !is.na(first_post_session)) |
                              (!is.na(Post_PLE_Score) & is.na(first_post_session)), 1, 0))
# n = 0 premenarche flags, n = 3 postmenarche flags, all are postmenarche after last ple score report


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
  select(participant_id, session_id, ab_g_dyn__visit_dtt) %>%
  # cut out hms so we can subtract in units of days
  mutate(interview_date = as.Date(ab_g_dyn__visit_dtt)) %>%
  select(-ab_g_dyn__visit_dtt) %>%
  # pivot wide so we can make a list of interview dates
  pivot_wider(names_from = session_id, values_from = interview_date)

# make a list of interview dates
dates <- dates %>%
  rowwise() %>%
  mutate(interview_dates = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`))) %>%
  select(participant_id, interview_dates)

## merge data
parent_ples_dates <- parent_ples %>%
  left_join(dates, by = 'participant_id')
youth_ples_dates <- youth_ples %>%
  left_join(dates, by = 'participant_id')

# Number of days between best-fit premenarche and postmenarche scores, saved to “between_scores”
parent_ples_dates <- parent_ples_dates %>%
  rowwise() %>%
  mutate(between_scores = interview_dates[PLE_PostSession + 1] - interview_dates[PLE_PreSession + 1])
youth_ples_dates <- youth_ples_dates %>%
  rowwise() %>%
  mutate(between_scores = interview_dates[PLE_PostSession + 1] - interview_dates[PLE_PreSession + 1])

# Number of days between last premenarche and first postmenarche reports, saved to “PreMen->PostMen”
parent_ples_dates <- parent_ples_dates %>%
  rowwise() %>%
  mutate('PreMen->PostMen' = interview_dates[first_post_session + 1] - interview_dates[last_pre_session + 1])
youth_ples_dates <- youth_ples_dates %>%
  rowwise() %>%
  mutate('PreMen->PostMen' = interview_dates[first_post_session + 1] - interview_dates[last_pre_session + 1])

#	Number of days from best-fit premenarche Score to last premenarche report, saved to “PreMenScore->PreMenReport”
parent_ples_dates <- parent_ples_dates %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interview_dates[last_pre_session + 1] - interview_dates[PLE_PreSession + 1])
youth_ples_dates <- youth_ples_dates %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interview_dates[last_pre_session + 1] - interview_dates[PLE_PreSession + 1])

#	Number of days from first postmenarche report to best-fit postmenarche Score, saved to “PostMenReport->PostMenScore”
parent_ples_dates <- parent_ples_dates %>%
  rowwise() %>%
  mutate('PostMenReport->PostMenScore' = interview_dates[PLE_PostSession + 1] - interview_dates[first_post_session + 1])
youth_ples_dates <- youth_ples_dates %>%
  rowwise() %>%
  mutate('PostMenReport->PostMenScore' = interview_dates[PLE_PostSession + 1] - interview_dates[first_post_session + 1])

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
  select(participant_id, Pre_PLE_Score, Post_PLE_Score, ChangeScore, last_pre_session, first_post_session,
         ple_scores, PLE_PreSession, PLE_PostSession,
         between_scores, `PreMen->PostMen`, `PreMenScore->PreMenReport`, `PostMenReport->PostMenScore`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0)
youth_ples_dates <- youth_ples_dates %>%
  select(participant_id, Pre_PLE_Score, Post_PLE_Score, ChangeScore, last_pre_session, first_post_session,
         ple_scores, PLE_PreSession, PLE_PostSession,
         between_scores, `PreMen->PostMen`, `PreMenScore->PreMenReport`, `PostMenReport->PostMenScore`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0)
  
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

# merge in itemized menarche reports for reference
parent_ples_dates <- parent_ples_dates %>%
  left_join(parent, by = 'participant_id')
youth_ples_dates <- youth_ples_dates %>%
  left_join(youth, by = 'participant_id')

#########################
# IN THE BELOW CODE BLOCK
# Export to csv
#########################

write_csv(parent_ples_dates, paste0(derivative_data, 'Parent_PrePost_PLEs_9_17_25.csv'))
write_csv(youth_ples_dates, paste0(derivative_data, 'Youth_PrePost_PLEs_9_17_25.csv'))
