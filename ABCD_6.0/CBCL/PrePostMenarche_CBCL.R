##########################
# PrePostMenarche_CBCL.R
# Author: Robert Toms
# Date: 9/18/2025
##########################

# The purpose of this code is to calculate changes in PLEs from pre to post menarche.

# libraries
library(tidyverse)
library(lubridate)

# define paths
sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/6.0/"

# import data
parent <- read_csv(paste0(derivative_data, 'Parent_PrePost_Menarche_9_15_25.csv'))
youth <- read_csv(paste0(derivative_data, 'Youth_PrePost_Menarche_9_15_25.csv'))
cbcl <- read_tsv(paste0(sourcepath, 'Mental_Health/mh_p_cbcl.tsv')) 
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
cbcl <- cbcl %>%
  select(participant_id, session_id, 
         mh_p_cbcl__synd__int_sum,  # Internal CBCL Syndrome Scale (sum)
         mh_p_cbcl__synd__ext_sum   # External CBCL Syndrome Scale (sum)
         )

# make numeric versions of session_id, will be made into a searchable chronological list vector later
cbcl <- cbcl %>%
  mutate(sesnum = case_when(
    session_id == 'ses-00A' ~ 0,
    session_id == 'ses-01A' ~ 1,
    session_id == 'ses-02A' ~ 2,
    session_id == 'ses-03A' ~ 3,
    session_id == 'ses-04A' ~ 4,
    session_id == 'ses-05A' ~ 5,
    session_id == 'ses-06A' ~ 6))

# Split internalizing and externalizing
cbcl_int <- cbcl %>%
  select(participant_id, session_id, mh_p_cbcl__synd__int_sum, sesnum)
cbcl_ext <- cbcl %>%
  select(participant_id, session_id, mh_p_cbcl__synd__ext_sum, sesnum)

# remove sessions with no cbcl data
cbcl_int <- cbcl_int %>%
  # Before: 67295
  filter(!is.na(mh_p_cbcl__synd__int_sum))
  # After: 67288
cbcl_ext <- cbcl_ext %>%
  # Before: 67295
  filter(!is.na(mh_p_cbcl__synd__ext_sum))
  # After: 67288

# Make chronological list vector of sessions with cbcl data
cbcl_int <- cbcl_int %>%
  group_by(participant_id) %>%
  mutate(has_cbcl_data = list(sesnum))
cbcl_ext <- cbcl_ext %>%
  group_by(participant_id) %>%
  mutate(has_cbcl_data = list(sesnum))

# pivot cbcl scores and make a chronological list vector of cbcl scores
cbcl_int <- cbcl_int %>%
  pivot_wider(names_from = session_id, values_from = mh_p_cbcl__synd__int_sum) %>%
  fill(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`), .direction = "downup") %>%
  select(-sesnum) %>%
  unique() %>%
  group_by(participant_id) %>%
  mutate(cbcl_scores = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)))
cbcl_ext <- cbcl_ext %>%
  pivot_wider(names_from = session_id, values_from = mh_p_cbcl__synd__ext_sum) %>%
  fill(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`), .direction = "downup") %>%
  select(-sesnum) %>%
  unique() %>%
  group_by(participant_id) %>%
  mutate(cbcl_scores = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)))

# keep useful columns
cbcl_int <- cbcl_int %>%
  select(participant_id, has_cbcl_data, cbcl_scores)
cbcl_ext <- cbcl_ext %>%
  select(participant_id, has_cbcl_data, cbcl_scores)

# merge with menarche data
int_parent <- cbcl_int %>%
  left_join(parent_simple, by = 'participant_id')
ext_parent <- cbcl_ext %>%
  left_join(parent_simple, by = 'participant_id')
int_youth <- cbcl_int %>%
  left_join(youth_simple, by = 'participant_id')
ext_youth <- cbcl_ext %>%
  left_join(youth_simple, by = 'participant_id')

#########################
# IN THE BELOW CODE BLOCK
# Find Best-Fit PreMenarche and PostMenarche Sessions with CBCL data
#########################

# define functions to find matching or best-fit sessions with ple scores
find_pre_cbcl_session <- function(pre_session_num, cbcl_session_list) {
  if (is.na(pre_session_num)) return(NA_integer_) # if no premenarche report
  pre_cbcl <- which(cbcl_session_list <= pre_session_num) # find cbcl scores that happen at or before last premenarche timepoint
  if (length(pre_cbcl) == 0) return(NA_integer_) # no premenarche cbcl scores
  return(tail(pre_cbcl, 1)) # output index of latest premenarche timepoint with a cbcl score
}

find_post_cbcl_session <- function(post_session_num, cbcl_session_list) {
  if (is.na(post_session_num)) return(NA_integer_) # if no postmenarche report
  post_cbcl <- which(cbcl_session_list >= post_session_num) # find cbcl scores that happen at or after first postmenarche timepoint
  if (length(post_cbcl) == 0) return(NA_integer_) # no postmenarche cbcl scores
  return(post_cbcl[1]) # output index of first postmenarche timepoint with a cbcl score
}

# Apply functions, pull session numbers
int_youth <- int_youth %>%
  rowwise() %>%
  mutate(CBCL_PreSession_index = find_pre_cbcl_session(last_pre_session, has_cbcl_data)) %>%
  mutate(CBCL_PostSession_index = find_post_cbcl_session(first_post_session, has_cbcl_data)) %>%
  mutate(CBCL_PreSession = if_else(!is.na(CBCL_PreSession_index), has_cbcl_data[CBCL_PreSession_index], NA)) %>%
  mutate(CBCL_PostSession = if_else(!is.na(CBCL_PostSession_index), has_cbcl_data[CBCL_PostSession_index], NA))
ext_youth <- ext_youth %>%
  rowwise() %>%
  mutate(CBCL_PreSession_index = find_pre_cbcl_session(last_pre_session, has_cbcl_data)) %>%
  mutate(CBCL_PostSession_index = find_post_cbcl_session(first_post_session, has_cbcl_data)) %>%
  mutate(CBCL_PreSession = if_else(!is.na(CBCL_PreSession_index), has_cbcl_data[CBCL_PreSession_index], NA)) %>%
  mutate(CBCL_PostSession = if_else(!is.na(CBCL_PostSession_index), has_cbcl_data[CBCL_PostSession_index], NA))
int_parent <- int_parent %>%
  rowwise() %>%
  mutate(CBCL_PreSession_index = find_pre_cbcl_session(last_pre_session, has_cbcl_data)) %>%
  mutate(CBCL_PostSession_index = find_post_cbcl_session(first_post_session, has_cbcl_data)) %>%
  mutate(CBCL_PreSession = if_else(!is.na(CBCL_PreSession_index), has_cbcl_data[CBCL_PreSession_index], NA)) %>%
  mutate(CBCL_PostSession = if_else(!is.na(CBCL_PostSession_index), has_cbcl_data[CBCL_PostSession_index], NA))
ext_parent <- ext_parent %>%
  rowwise() %>%
  mutate(CBCL_PreSession_index = find_pre_cbcl_session(last_pre_session, has_cbcl_data)) %>%
  mutate(CBCL_PostSession_index = find_post_cbcl_session(first_post_session, has_cbcl_data)) %>%
  mutate(CBCL_PreSession = if_else(!is.na(CBCL_PreSession_index), has_cbcl_data[CBCL_PreSession_index], NA)) %>%
  mutate(CBCL_PostSession = if_else(!is.na(CBCL_PostSession_index), has_cbcl_data[CBCL_PostSession_index], NA))

# Pull Scores
int_youth <- int_youth %>%
  rowwise() %>%
  mutate(Pre_CBCL_Score = if_else(!is.na(CBCL_PreSession), cbcl_scores[CBCL_PreSession + 1], NA),
         Post_CBCL_Score = if_else(!is.na(CBCL_PostSession), cbcl_scores[CBCL_PostSession + 1], NA))
ext_youth <- ext_youth %>%
  rowwise() %>%
  mutate(Pre_CBCL_Score = if_else(!is.na(CBCL_PreSession), cbcl_scores[CBCL_PreSession + 1], NA),
         Post_CBCL_Score = if_else(!is.na(CBCL_PostSession), cbcl_scores[CBCL_PostSession + 1], NA))
int_parent <- int_parent %>%
  rowwise() %>%
  mutate(Pre_CBCL_Score = if_else(!is.na(CBCL_PreSession), cbcl_scores[CBCL_PreSession + 1], NA),
         Post_CBCL_Score = if_else(!is.na(CBCL_PostSession), cbcl_scores[CBCL_PostSession + 1], NA))
ext_parent <- ext_parent %>%
  rowwise() %>%
  mutate(Pre_CBCL_Score = if_else(!is.na(CBCL_PreSession), cbcl_scores[CBCL_PreSession + 1], NA),
         Post_CBCL_Score = if_else(!is.na(CBCL_PostSession), cbcl_scores[CBCL_PostSession + 1], NA))
  

#########################
# IN THE BELOW CODE BLOCK
# Calculate Pre/Post Difference Scores
#########################

# sanity check -- are there any subjects whose pre/post menarche report timepoints don't have PLE data?
int_youth <- int_youth %>%
  mutate(PreFlag = if_else((is.na(Pre_CBCL_Score) & !is.na(last_pre_session)) |
                             (!is.na(Pre_CBCL_Score) & is.na(last_pre_session)), 1, 0)) %>% # n = 1, no premenarche cbcl scores
  mutate(PostFlag = if_else((is.na(Post_CBCL_Score) & !is.na(first_post_session)) |
                              (!is.na(Post_CBCL_Score) & is.na(first_post_session)), 1, 0)) # n = 41, no postmenarche cbcl scores for any
ext_youth <- ext_youth %>%
  mutate(PreFlag = if_else((is.na(Pre_CBCL_Score) & !is.na(last_pre_session)) |
                             (!is.na(Pre_CBCL_Score) & is.na(last_pre_session)), 1, 0)) %>% # n = 1, no premenarche cbcl scores
  mutate(PostFlag = if_else((is.na(Post_CBCL_Score) & !is.na(first_post_session)) |
                              (!is.na(Post_CBCL_Score) & is.na(first_post_session)), 1, 0)) # n = 41, no postmenarche cbcl scores for any
int_parent <- int_parent %>%
  mutate(PreFlag = if_else((is.na(Pre_CBCL_Score) & !is.na(last_pre_session)) |
                             (!is.na(Pre_CBCL_Score) & is.na(last_pre_session)), 1, 0)) %>% # n = 0
  mutate(PostFlag = if_else((is.na(Post_CBCL_Score) & !is.na(first_post_session)) |
                              (!is.na(Post_CBCL_Score) & is.na(first_post_session)), 1, 0)) # n = 2, no postmenarche cbcl scores for any
ext_parent <- ext_parent %>%
  mutate(PreFlag = if_else((is.na(Pre_CBCL_Score) & !is.na(last_pre_session)) |
                             (!is.na(Pre_CBCL_Score) & is.na(last_pre_session)), 1, 0)) %>% # n = 0
  mutate(PostFlag = if_else((is.na(Post_CBCL_Score) & !is.na(first_post_session)) |
                              (!is.na(Post_CBCL_Score) & is.na(first_post_session)), 1, 0)) # n = 2, no postmenarche cbcl scores for any

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

# merge dates in
int_youth <- int_youth %>%
  left_join(dates, by = 'participant_id')
ext_youth <- ext_youth %>%
  left_join(dates, by = 'participant_id')
int_parent <- int_parent %>%
  left_join(dates, by = 'participant_id')
ext_parent <- ext_parent %>%
  left_join(dates, by = 'participant_id')
  
# Number of days between best-fit premenarche and postmenarche scores, saved to “between_scores”
int_youth <- int_youth %>%
  rowwise() %>%
  mutate(between_scores = interview_dates[CBCL_PostSession + 1] - interview_dates[CBCL_PreSession + 1])
ext_youth <- ext_youth %>%
  rowwise() %>%
  mutate(between_scores = interview_dates[CBCL_PostSession + 1] - interview_dates[CBCL_PreSession + 1])
int_parent <- int_parent %>%
  rowwise() %>%
  mutate(between_scores = interview_dates[CBCL_PostSession + 1] - interview_dates[CBCL_PreSession + 1])
ext_parent <- ext_parent %>%
  rowwise() %>%
  mutate(between_scores = interview_dates[CBCL_PostSession + 1] - interview_dates[CBCL_PreSession + 1])

# Number of days between last premenarche and first postmenarche reports, saved to “PreMen->PostMen”
int_youth <- int_youth %>%
  rowwise() %>%
  mutate('PreMen->PostMen' = interview_dates[first_post_session + 1] - interview_dates[last_pre_session + 1])
ext_youth <- ext_youth %>%
  rowwise() %>%
  mutate('PreMen->PostMen' = interview_dates[first_post_session + 1] - interview_dates[last_pre_session + 1])
int_parent <- int_parent %>%
  rowwise() %>%
  mutate('PreMen->PostMen' = interview_dates[first_post_session + 1] - interview_dates[last_pre_session + 1])
ext_parent <- ext_parent %>%
  rowwise() %>%
  mutate('PreMen->PostMen' = interview_dates[first_post_session + 1] - interview_dates[last_pre_session + 1])

#	Number of days from best-fit premenarche Score to last premenarche report, saved to “PreMenScore->PreMenReport”
int_youth <- int_youth %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interview_dates[last_pre_session + 1] - interview_dates[CBCL_PreSession + 1])
ext_youth <- ext_youth %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interview_dates[last_pre_session + 1] - interview_dates[CBCL_PreSession + 1])
int_parent <- int_parent %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interview_dates[last_pre_session + 1] - interview_dates[CBCL_PreSession + 1])
ext_parent <- ext_parent %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interview_dates[last_pre_session + 1] - interview_dates[CBCL_PreSession + 1])

#	Number of days from first postmenarche report to best-fit postmenarche Score, saved to “PostMenReport->PostMenScore”
int_youth <- int_youth %>%
  rowwise() %>%
  mutate('PostMenReport->PostMenScore' = interview_dates[CBCL_PostSession + 1] - interview_dates[first_post_session + 1])
ext_youth <- ext_youth %>%
  rowwise() %>%
  mutate('PostMenReport->PostMenScore' = interview_dates[CBCL_PostSession + 1] - interview_dates[first_post_session + 1])
int_parent <- int_parent %>%
  rowwise() %>%
  mutate('PostMenReport->PostMenScore' = interview_dates[CBCL_PostSession + 1] - interview_dates[first_post_session + 1])
ext_parent <- ext_parent %>%
  rowwise() %>%
  mutate('PostMenReport->PostMenScore' = interview_dates[CBCL_PostSession + 1] - interview_dates[first_post_session + 1])


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
  
# reorder and keep useful columns
int_youth <- int_youth %>%
  select(participant_id, Pre_CBCL_Score, Post_CBCL_Score, ChangeScore, last_pre_session, first_post_session,
         cbcl_scores, CBCL_PreSession, CBCL_PostSession,
         between_scores, `PreMen->PostMen`, `PreMenScore->PreMenReport`, `PostMenReport->PostMenScore`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0)
ext_youth <- ext_youth %>%
  select(participant_id, Pre_CBCL_Score, Post_CBCL_Score, ChangeScore, last_pre_session, first_post_session,
         cbcl_scores, CBCL_PreSession, CBCL_PostSession,
         between_scores, `PreMen->PostMen`, `PreMenScore->PreMenReport`, `PostMenReport->PostMenScore`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0)
int_parent <- int_parent %>%
  select(participant_id, Pre_CBCL_Score, Post_CBCL_Score, ChangeScore, last_pre_session, first_post_session,
         cbcl_scores, CBCL_PreSession, CBCL_PostSession,
         between_scores, `PreMen->PostMen`, `PreMenScore->PreMenReport`, `PostMenReport->PostMenScore`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0)
ext_parent <- ext_parent %>%
  select(participant_id, Pre_CBCL_Score, Post_CBCL_Score, ChangeScore, last_pre_session, first_post_session,
         cbcl_scores, CBCL_PreSession, CBCL_PostSession,
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
int_youth <- int_youth %>%
  left_join(youth, by = 'participant_id')
ext_youth <- ext_youth %>%
  left_join(youth, by = 'participant_id')
int_parent <- int_parent %>%
  left_join(parent, by = 'participant_id')
ext_parent <- ext_parent %>%
  left_join(parent, by = 'participant_id')


#########################
# IN THE BELOW CODE BLOCK
# Export to csv
#########################

write_csv(int_youth, paste0(derivative_data, 'Youth_PrePost_IntCBCL_9_18_25.csv'))
write_csv(ext_youth, paste0(derivative_data, 'Youth_PrePost_ExtCBCL_9_18_25.csv'))
write_csv(int_parent, paste0(derivative_data, 'Parent_PrePost_IntCBCL_9_18_25.csv'))
write_csv(ext_parent, paste0(derivative_data, 'Parent_PrePost_ExtCBCL_9_18_25.csv'))
