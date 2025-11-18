############################
# PrePost_5.0_NIH_ListSorting.R
# Author: Robert Toms
# Date: 10/9/2025
############################

# The purpose of this code is to match Pre & Post menarche timepoints for each subject 
#             with available NIH Toolbox data for the List Sorting Working Memory Task.

library(tidyverse)
library(lubridate)

sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"

# Pre/Post Menarche Timepoint data
parent <- read_csv(paste0(derivative_data, '/5.0_Parent_PrePost_Menarche_10_1_25.csv'))
youth <- read_csv(paste0(derivative_data, '/5.0_Youth_PrePost_Menarche_10_1_25.csv'))

# NIH Toolbox Data
nih <- read_csv(paste0(sourcepath, 'neurocognition/nc_y_nihtb.csv'))

# Interview datetimes
dates <- read_csv(paste0(sourcepath, 'abcd-general/abcd_y_lt.csv'))

#################
# IN THE BELOW CODE BLOCK
# Clean and Prepare Data
#################

# keep useful columns, rename if necessary
nih <- nih %>%
  select(src_subject_id, eventname,
         nihtbx_list_date,            # datetime of when list-sorting task was completed 
         nihtbx_list_agecorrected)  # NIH Toolbox, List Sorting Working Memory Task, age corrected score

# also convert data type from character to mdy date, so they can be subtracted from each other
dates <- dates %>%
  mutate(interview_date = mdy(interview_date)) %>%
  select(src_subject_id, eventname, interview_date)

# remove sessions with no score data
nih <- nih %>%
  # Before: 27028 timepoints
  filter(!is.na(nihtbx_list_agecorrected))
# After: 16293 timepoints

# make list vector of interview dates for menarche report dates
dates_wide <- dates %>%
  pivot_wider(names_from = eventname, values_from = interview_date) %>%
  rowwise() %>%
  mutate(interview_dates = list(c(baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`,`2_year_follow_up_y_arm_1`, 
                                  `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)))

# pivot datetimes and scores wide
nih <- nih %>%
  mutate(event_id = eventname) %>%
  pivot_wider(names_from = event_id, values_from = nihtbx_list_date) %>%
  rename(baseline_date = 'baseline_year_1_arm_1',
         year2_date = '2_year_follow_up_y_arm_1',
         year4_date = '4_year_follow_up_y_arm_1')
nih <- nih %>%
  pivot_wider(names_from = eventname, values_from = nihtbx_list_agecorrected) %>%
  rename(baseline_score = 'baseline_year_1_arm_1',
         year2_score = '2_year_follow_up_y_arm_1',
         year4_score = '4_year_follow_up_y_arm_1')

# format 
nih <- nih %>%
  group_by(src_subject_id) %>%
  fill(c(baseline_date, year2_date, year4_date, 
         baseline_score, year2_score, year4_score), .direction = "downup") %>%
  unique()

# remove time from datetime, so dates are subtractable from other dates
nih <- nih %>%
  mutate('baseline_date' = as.Date(baseline_date),
         'year2_date' = as.Date(year2_date),
         'year4_date' = as.Date(year4_date))

# reorder to put matching scores and datetimes next to each other
nih <- nih %>%
  select(src_subject_id, 'baseline_date', baseline_score, 'year2_date', year2_score, 'year4_date', year4_score)

# merge interview dates and menarche data to get interview list vectors in the same place as menarche data
dates_wide <- dates_wide %>%
  select(src_subject_id, interview_dates)
parent <- parent %>%
  left_join(dates_wide, by = 'src_subject_id')
youth <- youth %>%
  left_join(dates_wide, by = 'src_subject_id')

#################
# IN THE BELOW CODE BLOCK
# Sanity Check Year 2 Scores and Dates
#################

###keep only rows with year 2 data
# Before: 11732 rows
year2 <- nih %>%
  filter(!is.na(year2_date) | !is.na(year2_score))
# After: only 82 rows

# see if dates are the same as baseline or year 4
year2 <- year2 %>%
  mutate(baseline_same_Y1N0 = if_else(baseline_date == year2_date, 1, 0),
         year4_same_Y1N0 = if_else(year2_date == year4_date, 1, 0))

# set NA year4_same_Y1N0 to 0, they had no year 4 data
year2 <- year2 %>%
  mutate(year4_same_Y1N0 = if_else(is.na(year4_same_Y1N0), 0, year4_same_Y1N0))

# aggregate sameness flags
year2 <- year2 %>%
  mutate(same = if_else(baseline_same_Y1N0 == 1 | year4_same_Y1N0 == 1, 1, 0))

table(year2$same)
# 13 of the 82 year 2 scores are not repeats of baseline or year 4

### Because there are so few valid year 2 scores, all will be removed for uniformity of analysis.
### This has not been corrected for ABCD 5.0/5.1, but has been corrected for ABCD 6.0.

# remove year 2 columns
nih <- nih %>%
  select(-year2_date, -year2_score)

#################
# IN THE BELOW CODE BLOCK
# Merge NIH Data with Menarche Data
#################

parent_nih <- parent %>%
  select(src_subject_id, last_pre_event, first_post_event, interview_dates,
         PostMenarche_at_Baseline_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0) %>%
  left_join(nih, by = 'src_subject_id')

youth_nih <- youth %>%
  select(src_subject_id, last_pre_event, first_post_event, interview_dates,
         PostMenarche_at_Baseline_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0) %>%
  left_join(nih, by = 'src_subject_id')

#################
# IN THE BELOW CODE BLOCK
# Find PreMenarche and PostMenarche Scores
#################

# Because change scores are the primary outcome variable, both baseline and year 4 scores will be needed, if a change score is possible.
# Therefore if change scores are possible, baseline NIH toolbox scores will be the premenarche, and year 4 scores will be postmenarche.

# Find Baseline/Pre and Year4/Post scores, as applicable
parent_nih <- parent_nih %>%
  # last premenarche timepoint is at or after baseline, but before year 4
  mutate(PreScore = if_else(last_pre_event >= 0 & last_pre_event < 4, baseline_score, NA),
         # first postmenarche timepoint is after baseline, but at or before year 4
         PostScore = if_else(first_post_event > 0 & first_post_event <= 4, year4_score, NA))
youth_nih <- youth_nih %>%
  # last premenarche timepoint is at or after baseline, but before year 4
  mutate(PreScore = if_else(last_pre_event >= 0 & last_pre_event < 4, baseline_score, NA),
         # first postmenarche timepoint is after baseline, but at or before year 4
         PostScore = if_else(first_post_event > 0 & first_post_event <= 4, year4_score, NA))

# Calculate Change Scores
parent_nih <- parent_nih %>%
  rowwise() %>%
  mutate(ChangeScore = PostScore - PreScore)
youth_nih <- youth_nih %>%
  rowwise() %>%
  mutate(ChangeScore = PostScore - PreScore)

#################
# IN THE BELOW CODE BLOCK
# Calculate Days between Data collection and/or Days between menarche report and data collection
#################

# Find PreScore and PostScore dates
parent_nih <- parent_nih %>%
  mutate(PreScore_date = if_else(!is.na(PreScore), baseline_date, NA),
         PostScore_date = if_else(!is.na(PostScore), year4_date, NA))
youth_nih <- youth_nih %>%
  mutate(PreScore_date = if_else(!is.na(PreScore), baseline_date, NA),
         PostScore_date = if_else(!is.na(PostScore), year4_date, NA))

### Calculating Date Differences
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
  mutate('PreMen->PostMen' = interview_dates[first_post_event + 1] - interview_dates[last_pre_event + 1])
youth_nih <- youth_nih %>%
  rowwise() %>%
  mutate('PreMen->PostMen' = interview_dates[first_post_event + 1] - interview_dates[last_pre_event + 1])

#	Number of days from best-fit premenarche Score to last premenarche report, saved to “PreMenScore->PreMenReport”
parent_nih <- parent_nih %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interview_dates[last_pre_event + 1] - PreScore_date)
youth_nih <- youth_nih %>%
  rowwise() %>%
  mutate('PreMenScore->PreMenReport' = interview_dates[last_pre_event + 1] - PreScore_date)

#	Number of days from first postmenarche report to best-fit postmenarche Score, saved to “PostMenReport->PostMenScore”
parent_nih <- parent_nih %>%
  rowwise() %>%
  mutate('PostMenReport->PostMenScore' = PostScore_date - interview_dates[first_post_event + 1])
youth_nih <- youth_nih %>%
  rowwise() %>%
  mutate('PostMenReport->PostMenScore' = PostScore_date - interview_dates[first_post_event + 1])


#################
# IN THE BELOW CODE BLOCK
# Clean/Reorder dataframes for CSV Export
#################

# convert interview dates to string so they can be written into a csv for reference
parent_nih <- parent_nih %>%
  mutate(interview_dates = paste(interview_dates, collapse = ", "))
youth_nih <- youth_nih %>%
  mutate(interview_dates = paste(interview_dates, collapse = ", "))

# keep and order necessary columns
parent_nih <- parent_nih %>%
  select(src_subject_id, ChangeScore, PreScore, PostScore, last_pre_event, first_post_event,
         between_scores, `PreMen->PostMen`, `PreMenScore->PreMenReport`, `PostMenReport->PostMenScore`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0,
         baseline_score, baseline_date, year4_score, year4_date, interview_dates)
youth_nih <- youth_nih %>%
  select(src_subject_id, ChangeScore, PreScore, PostScore, last_pre_event, first_post_event,
         between_scores, `PreMen->PostMen`, `PreMenScore->PreMenReport`, `PostMenReport->PostMenScore`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0,
         baseline_score, baseline_date, year4_score, year4_date, interview_dates)

# isolate itemized menarche reports for reference
youth <- youth %>%
  select(src_subject_id, baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`) %>%
  rename('baseline_menarche_report' = baseline_year_1_arm_1,
         'year1_menarche_report' = `1_year_follow_up_y_arm_1`,
         'year2_menarche_report' = `2_year_follow_up_y_arm_1`,
         'year3_menarche_report' = `3_year_follow_up_y_arm_1`,
         'year4_menarche_report' = `4_year_follow_up_y_arm_1`)
parent <- parent %>%
  select(src_subject_id, baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`) %>%
  rename('baseline_menarche_report' = baseline_year_1_arm_1,
         'year1_menarche_report' = `1_year_follow_up_y_arm_1`,
         'year2_menarche_report' = `2_year_follow_up_y_arm_1`,
         'year3_menarche_report' = `3_year_follow_up_y_arm_1`,
         'year4_menarche_report' = `4_year_follow_up_y_arm_1`)

# add in itemized menarche reports for reference
parent_nih <- parent_nih %>%
  left_join(parent, by = 'src_subject_id')
youth_nih <- youth_nih %>%
  left_join(youth, by = 'src_subject_id')

#################
# IN THE BELOW CODE BLOCK
# Write to CSV
#################

write_csv(parent_nih, paste0(derivative_data, 'Parent_5.0_PrePost_NIH_LSWM_10_9_25.csv'))
write_csv(youth_nih, paste0(derivative_data, 'Youth_5.0_PrePost_NIH_LSWM_10_9_25.csv'))
