############################
# MRI_PrePost_Menarche_5.0.R
# Author: Robert Toms
# Date: 10/3/2025
############################

# The purpose of this code is to match Pre & Post menarche timepoints for each subject with available neuroimaging data.

library(tidyverse)
library(lubridate)

sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"

# Pre/Post Menarche Timepoint data
parent <- read_csv(paste0(derivative_data, '5.0_Parent_PrePost_Menarche_10_1_25.csv'))
youth <- read_csv(paste0(derivative_data, '5.0_Youth_PrePost_Menarche_10_1_25.csv'))

# Baseline & year 2 QC data, Neuroimaging: Structural, Resting State fMRI, NBack (Task)
mri <- read_csv(paste0(sourcepath, '/imaging/mri_y_qc_incl.csv'))
# QC Data for years 3-4
year3and4 <- read_csv(paste0(derivative_data, 'mri_availability_4_7_25.csv'))

# interview dates
dates <- read_csv(paste0(sourcepath, '/abcd-general/abcd_y_lt.csv'))

#################
# IN THE BELOW CODE BLOCK
# Clean and Prepare Data
#################

# Keep necessary columns
mri <- mri %>%
  select(src_subject_id, eventname, imgincl_t1w_include, imgincl_rsfmri_include, imgincl_nback_include)

dates <- dates %>%
  mutate(interview_date = mdy(interview_date)) %>%
  select(src_subject_id, eventname, interview_date)

# remove mri timepoints without any usable data
mri <- mri %>%
  # 19649 total timepoints
  filter(imgincl_t1w_include == 1 | imgincl_rsfmri_include == 1 | imgincl_nback_include == 1)
  # 19090 timepoints remain


#################
# IN THE BELOW CODE BLOCK
# Incorporate Inferred Year 3/4 QC Data
#################

######### As of 5.1, there's only QC data for baseline and year 2 MRI scans
#### Aaron and Dr. Damme parsed eventnames from available tgz's to automatically catalog and include year 3 and year 4 scans, see README
#### Use year 3 and 4 availability data from mri_availability_4_7_25.csv (6/11/2025)

# keep useful columns
year3and4 <- year3and4 %>%
  select(src_subject_id, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)

# pivot long to match format of mri dataframe
year3and4 <- year3and4 %>%
  pivot_longer(c(`3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`), names_to = "eventname", values_to = "include")

# remove NA "include" rows
year3and4 <- year3and4 %>%
  filter(!is.na(include))

# Expand usability to infer t1w, rsfmri, and nback usability data. "2" used rather than "1" to delineate inferred qc status
year3and4 <- year3and4 %>%
  mutate(imgincl_t1w_include = if_else(include == 1, 2, 0),
         imgincl_rsfmri_include = if_else(include == 1, 2, 0),
         imgincl_nback_include = if_else(include == 1, 2, 0))

# remove superfluous column, merge with qc data
year3and4 <- year3and4 %>%
  select(-include)

mri <- mri %>%
  rbind(year3and4) %>%
  arrange(src_subject_id)

#################
# IN THE BELOW CODE BLOCK
# Make a human-readable vector of timepoints with available scan data
#################

# Make simple numeric timepoint column
mri <- mri %>%
  mutate(event_num = case_when(
    eventname == 'baseline_year_1_arm_1' ~ 0,
    eventname == '2_year_follow_up_y_arm_1' ~ 2,
    eventname == '3_year_follow_up_y_arm_1' ~ 3,
    eventname == '4_year_follow_up_y_arm_1' ~ 4)) %>%
  arrange(src_subject_id, event_num)

# Make a human-readable chronological list vector of timepoints with available scan data
mri <- mri %>%
  group_by(src_subject_id) %>%
  arrange(event_num) %>%
  mutate(has_scan = list(event_num))

# simplify mri data
has_scan <- mri %>%
  select(src_subject_id, has_scan) %>%
  unique()

#################
# IN THE BELOW CODE BLOCK
# Merge Menarche and MRI/Dates data
#################

### format Menarche Data
## Parent
parent_simple <- parent %>%
  rowwise() %>%
  mutate(menarche_reports = list(c(baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`))) %>%
  select(src_subject_id, menarche_reports, last_pre_event, first_post_event, PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0) %>%
  unique()
youth_simple <- youth %>%
  rowwise() %>%
  mutate(menarche_reports = list(c(baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`))) %>%
  select(src_subject_id, menarche_reports, last_pre_event, first_post_event, PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0) %>%
  unique()

# Merge has_scan data with Menarche data
parent_img <- parent_simple %>%
  left_join(has_scan, by = "src_subject_id")
youth_img <- youth_simple %>%
  left_join(has_scan, by = "src_subject_id")

#################
# IN THE BELOW CODE BLOCK
# Determine Pre/Post Menarche Scan Timepoints
#################

# Write a function that finds matching or best-fit premenarche scan
find_premenarche_scan <- function(premenarche_tp, mri_scans) {
  if (is.na(premenarche_tp)) return(NA_integer_) # No premenarche timepoint
  premenarche <- which(mri_scans <= premenarche_tp) # Find the latest scan that happens at or before last premenarche timepoint
  if (length(premenarche) == 0) return(NA_integer_) # No premenarche scans
  return(tail(premenarche, 1))
}

# Write a function that finds matching or best-fit postmenarche scan
find_postmenarche_scan <- function(postmenarche_tp, mri_scans) {
  if (is.na(postmenarche_tp)) return(NA_integer_) # No postmenarche timepoint
  postmenarche <- which(mri_scans >= postmenarche_tp) # Find first scan that happens at or after first postmenarche timepoint
  if (length(postmenarche) == 0) return(NA_integer_) # No postmenarche scans
  return(postmenarche[1])
}

# Apply functions
parent_img <- parent_img %>%
  mutate(PreScan_index = find_premenarche_scan(last_pre_event, has_scan)) %>%
  mutate(PostScan_index = find_postmenarche_scan(first_post_event, has_scan))
youth_img <- youth_img %>%
  mutate(PreScan_index = find_premenarche_scan(last_pre_event, has_scan)) %>%
  mutate(PostScan_index = find_postmenarche_scan(first_post_event, has_scan))

# remove subject(s) with menarche data but no usable scans
parent_img <- parent_img %>%
  filter(length(has_scan) > 0) # 5677 -> 5594
youth_img <- youth_img %>%
  filter(length(has_scan) > 0) # 5677 -> 5594


# Parse PreMenarche scan and PostMenarche scan from has_scan
parent_img <- parent_img %>%
  rowwise() %>%
  mutate(PreScan = has_scan[PreScan_index],
         PostScan = has_scan[PostScan_index])
youth_img <- youth_img %>%
  rowwise() %>%
  mutate(PreScan = has_scan[PreScan_index],
         PostScan = has_scan[PostScan_index])

#################
# IN THE BELOW CODE BLOCK
# Calculate Days between Data collection and/or Days between menarche report and data collection
#################

# convert data type from character to mdy date, so they can be subtracted from each other
dates_wide <- dates %>%
  pivot_wider(names_from = eventname, values_from = interview_date) %>%
  # make searchable chronological list vector for dates
  group_by(src_subject_id) %>%
  mutate(dates = list(c(baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, 
                      `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`))) %>%
  # keep useful columns
  select(src_subject_id, dates, baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, 
         `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)

# merge
youth_img_dates <- youth_img %>%
  left_join(dates_wide, by = "src_subject_id")
parent_img_dates <- parent_img %>%
  left_join(dates_wide, by = "src_subject_id")


## days difference in scan data collection
# calculate days between scans - add 1 to change from visit to index
youth_img_dates <- youth_img_dates %>%
  mutate(between_scans = dates[PostScan + 1] - dates[PreScan + 1])
parent_img_dates <- parent_img_dates %>%
  mutate(between_scans = dates[PostScan + 1] - dates[PreScan + 1])

# Days difference in pre/post menarche reports
youth_img_dates <- youth_img_dates %>%
  mutate('PreMen->PostMen' = dates[first_post_event + 1] - dates[last_pre_event + 1])
parent_img_dates <- parent_img_dates %>%
  mutate('PreMen->PostMen' = dates[first_post_event + 1] - dates[last_pre_event + 1])

# Days difference between PreMenarche report and Premenarche Scan
youth_img_dates <- youth_img_dates %>%
  mutate('PreMenScan->PreMenReport' = dates[last_pre_event + 1] - dates[PreScan + 1])
parent_img_dates <- parent_img_dates %>%
  mutate('PreMenScan->PreMenReport' = dates[last_pre_event + 1] - dates[PreScan + 1])

# Days difference between PostMenarche Report and Postmenarche Scan
youth_img_dates <- youth_img_dates %>%
  mutate('PostMenReport->PostMenScan' = dates[PostScan + 1] - dates[first_post_event + 1])
parent_img_dates <- parent_img_dates %>%
  mutate('PostMenReport->PostMenScan' = dates[PostScan + 1] - dates[first_post_event + 1])

#################
# IN THE BELOW CODE BLOCK
# Clean/Reorder dataframes for CSV Export
#################

# rename interview dates
youth_img_dates <- youth_img_dates %>%
  rename('baseline_date' = baseline_year_1_arm_1,
         'year1_date' = `1_year_follow_up_y_arm_1`,
         'year2_date' = `2_year_follow_up_y_arm_1`,
         'year3_date' = `3_year_follow_up_y_arm_1`,
         'year4_date' = `4_year_follow_up_y_arm_1`)
parent_img_dates <- parent_img_dates %>%
  rename('baseline_date' = baseline_year_1_arm_1,
         'year1_date' = `1_year_follow_up_y_arm_1`,
         'year2_date' = `2_year_follow_up_y_arm_1`,
         'year3_date' = `3_year_follow_up_y_arm_1`,
         'year4_date' = `4_year_follow_up_y_arm_1`)

# rename menarche reports and merge
youth <- youth %>%
  select(src_subject_id, baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, 
         `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`) %>%
  rename('baseline_menarche_report' = baseline_year_1_arm_1,
         'year1_menarche_report' = `1_year_follow_up_y_arm_1`,
         'year2_menarche_report' = `2_year_follow_up_y_arm_1`,
         'year3_menarche_report' = `3_year_follow_up_y_arm_1`,
         'year4_menarche_report' = `4_year_follow_up_y_arm_1`)
parent <- parent %>%
  select(src_subject_id, baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, 
         `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`) %>%
  rename('baseline_menarche_report' = baseline_year_1_arm_1,
         'year1_menarche_report' = `1_year_follow_up_y_arm_1`,
         'year2_menarche_report' = `2_year_follow_up_y_arm_1`,
         'year3_menarche_report' = `3_year_follow_up_y_arm_1`,
         'year4_menarche_report' = `4_year_follow_up_y_arm_1`)
# merge
youth_img_dates <- youth_img_dates %>%
  left_join(youth, by = 'src_subject_id')
parent_img_dates <- parent_img_dates %>%
  left_join(parent, by = 'src_subject_id')

# convert has_scan to string so it can be written into a csv
youth_img_dates <- youth_img_dates %>%
  mutate(has_scan = paste(has_scan, collapse = ", "))
parent_img_dates <- parent_img_dates %>%
  mutate(has_scan = paste(has_scan, collapse = ", "))

# keep and order necessary columns
youth_img_dates <- youth_img_dates %>%
  select(src_subject_id, PreScan, PostScan, last_pre_event, first_post_event,
         has_scan, between_scans, `PreMen->PostMen`, `PreMenScan->PreMenReport`, `PostMenReport->PostMenScan`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0,
         baseline_menarche_report, year1_menarche_report, year2_menarche_report, year3_menarche_report, year4_menarche_report,
         baseline_date, year1_date, year2_date, year3_date, year4_date)
parent_img_dates <- parent_img_dates %>%
  select(src_subject_id, PreScan, PostScan, last_pre_event, first_post_event,
         has_scan, between_scans, `PreMen->PostMen`, `PreMenScan->PreMenReport`, `PostMenReport->PostMenScan`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0,
         baseline_menarche_report, year1_menarche_report, year2_menarche_report, year3_menarche_report, year4_menarche_report,
         baseline_date, year1_date, year2_date, year3_date, year4_date)

#################
# IN THE BELOW CODE BLOCK
# Write to CSV
#################

write_csv(parent_img_dates, paste0(derivative_data, 'Parent_5.0_PrePost_MRI_10_3_25.csv'))
write_csv(youth_img_dates, paste0(derivative_data, 'Youth_5.0_PrePost_MRI_10_3_25.csv'))
