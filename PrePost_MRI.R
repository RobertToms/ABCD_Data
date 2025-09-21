############################
# PrePost_MRI.R
# Author: Robert Toms
# Date: 9/16/2025
############################

# The purpose of this code is to match Pre & Post menarche timepoints for each subject with available neuroimaging data.

library(tidyverse)
library(lubridate)

sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/"

# Pre/Post Menarche Timepoint data
### These are made via PrePost_Menarche.R ###
parent <- read_csv(paste0(derivative_data, 'Parent_PrePost_Menarche_9_15_25.csv'))
youth <- read_csv(paste0(derivative_data, 'Youth_PrePost_Menarche_9_15_25.csv'))

# Neuroimaging: Structural, Resting State fMRI, NBack (Task)
mri <- read_tsv(paste0(sourcepath, '/Imaging/QC/mr_y_qc__incl.tsv'))

# interview dates
dates <- read_tsv(paste0(sourcepath, '/abcd_general/ab_g_dyn.tsv'))

#################
# IN THE BELOW CODE BLOCK
# Clean and Prepare Data
#################

# Keep necessary columns
mri <- mri %>%
  select(participant_id, session_id, mr_y_qc__incl__smri__t1_indicator, mr_y_qc__incl__rsfmri_indicator, mr_y_qc__incl__tfmri__nback_indicator)

dates <- dates %>%
  mutate(interview_date = as.Date(ab_g_dyn__visit_dtt)) %>%
  select(participant_id, session_id, interview_date)

# remove mri timepoints without any usable data
mri <- mri %>%
  # 30391 total timepoints
  filter(mr_y_qc__incl__smri__t1_indicator == 1 | mr_y_qc__incl__rsfmri_indicator == 1 | mr_y_qc__incl__tfmri__nback_indicator == 1)
  # 29602 timepoints remain

#################
# IN THE BELOW CODE BLOCK
# Make a human-readable vector of timepoints with available scan data
#################

# Make simple numeric timepoint column
mri <- mri %>%
  mutate(session_num = case_when(
    session_id == 'ses-00A' ~ 0,
    session_id == 'ses-02A' ~ 2,
    session_id == 'ses-04A' ~ 4,
    session_id == 'ses-06A' ~ 6)) %>%
  arrange(participant_id, session_num)

# Make a human-readable vector of timepoints with available scan data
mri <- mri %>%
  group_by(participant_id) %>%
  mutate(has_scan = list(session_num))

#################
# IN THE BELOW CODE BLOCK
# Simplify and merge Menarche and MRI/Dates data
#################

# simplify mri data
has_scan <- mri %>%
  select(participant_id, has_scan) %>%
  unique()

## Parent
parent_simple <- parent %>%
  rowwise() %>%
  mutate(menarche_reports = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`))) %>%
  select(participant_id, menarche_reports, last_pre_session, first_post_session, PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0) %>%
  unique()
youth_simple <- youth %>%
  rowwise() %>%
  mutate(menarche_reports = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`))) %>%
  select(participant_id, menarche_reports, last_pre_session, first_post_session, PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0) %>%
  unique()

# Merge has_scan data with Menarche data
parent_img <- parent_simple %>%
  left_join(has_scan, by = "participant_id")
youth_img <- youth_simple %>%
  left_join(has_scan, by = "participant_id")

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
  mutate(PreScan_index = find_premenarche_scan(last_pre_session, has_scan)) %>%
  mutate(PostScan_index = find_postmenarche_scan(first_post_session, has_scan))
youth_img <- youth_img %>%
  mutate(PreScan_index = find_premenarche_scan(last_pre_session, has_scan)) %>%
  mutate(PostScan_index = find_postmenarche_scan(first_post_session, has_scan))
  
# remove subject(s) with menarche data but no usable scans
parent_img <- parent_img %>%
  filter(length(has_scan) > 0) # 5677 -> 5619
youth_img <- youth_img %>%
  filter(length(has_scan) > 0) # 5678 -> 5619


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
  pivot_wider(names_from = session_id, values_from = interview_date)

# merge
youth_img_dates <- youth_img %>%
  left_join(dates_wide, by = "participant_id")
parent_img_dates <- parent_img %>%
  left_join(dates_wide, by = "participant_id")

# make list vector for dates
youth_img_dates <- youth_img_dates %>%
  mutate(dates = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)))
parent_img_dates <- parent_img_dates %>%
  mutate(dates = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)))

## days difference in scan data collection
# calculate days between scans - add 1 to change from visit to index
youth_img_dates <- youth_img_dates %>%
  mutate(between_scans = dates[PostScan + 1] - dates[PreScan + 1])
parent_img_dates <- parent_img_dates %>%
  mutate(between_scans = dates[PostScan + 1] - dates[PreScan + 1])

# Days difference in pre/post menarche reports
youth_img_dates <- youth_img_dates %>%
  mutate('PreMen->PostMen' = dates[first_post_session + 1] - dates[last_pre_session + 1])
parent_img_dates <- parent_img_dates %>%
  mutate('PreMen->PostMen' = dates[first_post_session + 1] - dates[last_pre_session + 1])

# Days difference between PreMenarche report and Premenarche Scan
youth_img_dates <- youth_img_dates %>%
  mutate('PreMenScan->PreMenReport' = dates[last_pre_session + 1] - dates[PreScan + 1])
parent_img_dates <- parent_img_dates %>%
  mutate('PreMenScan->PreMenReport' = dates[last_pre_session + 1] - dates[PreScan + 1])

# Days difference between PostMenarche Report and Postmenarche Scan
youth_img_dates <- youth_img_dates %>%
  mutate('PostMenReport->PostMenScan' = dates[PostScan + 1] - dates[first_post_session + 1])
parent_img_dates <- parent_img_dates %>%
  mutate('PostMenReport->PostMenScan' = dates[PostScan + 1] - dates[first_post_session + 1])

#################
# IN THE BELOW CODE BLOCK
# Clean/Reorder dataframes for CSV Export
#################

# rename interview dates
youth_img_dates <- youth_img_dates %>%
  rename('00A_date' = `ses-00A`,
         '01A_date' = `ses-01A`,
         '02A_date' = `ses-02A`,
         '03A_date' = `ses-03A`,
         '04A_date' = `ses-04A`,
         '05A_date' = `ses-05A`,
         '06A_date' = `ses-06A`)
parent_img_dates <- parent_img_dates %>%
  rename('00A_date' = `ses-00A`,
         '01A_date' = `ses-01A`,
         '02A_date' = `ses-02A`,
         '03A_date' = `ses-03A`,
         '04A_date' = `ses-04A`,
         '05A_date' = `ses-05A`,
         '06A_date' = `ses-06A`)

# rename menarche reports and merge
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
# merge
youth_img_dates <- youth_img_dates %>%
  left_join(youth, by = 'participant_id')
parent_img_dates <- parent_img_dates %>%
  left_join(parent, by = 'participant_id')

# convert has_scan to string so it can be written into a csv
youth_img_dates <- youth_img_dates %>%
  mutate(has_scan = paste(has_scan, collapse = ", "))
parent_img_dates <- parent_img_dates %>%
  mutate(has_scan = paste(has_scan, collapse = ", "))

# keep and order necessary columns
youth_img_dates <- youth_img_dates %>%
  select(participant_id, PreScan, PostScan, last_pre_session, first_post_session,
         has_scan, between_scans, `PreMen->PostMen`, `PreMenScan->PreMenReport`, `PostMenReport->PostMenScan`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0,
         `00A_menarche_report`, `01A_menarche_report`, `02A_menarche_report`, `03A_menarche_report`, `04A_menarche_report`, `05A_menarche_report`, `06A_menarche_report`,
         `00A_date`, `01A_date`, `02A_date`, `03A_date`, `04A_date`, `05A_date`, `06A_date`)
parent_img_dates <- parent_img_dates %>%
  select(participant_id, PreScan, PostScan, last_pre_session, first_post_session,
         has_scan, between_scans, `PreMen->PostMen`, `PreMenScan->PreMenReport`, `PostMenReport->PostMenScan`,
         PreMenarche_at_LastReport_Y1N0, PostMenarche_at_Baseline_Y1N0, Inconsistent_Reporting_Y1N0,
         `00A_menarche_report`, `01A_menarche_report`, `02A_menarche_report`, `03A_menarche_report`, `04A_menarche_report`, `05A_menarche_report`, `06A_menarche_report`,
         `00A_date`, `01A_date`, `02A_date`, `03A_date`, `04A_date`, `05A_date`, `06A_date`)
  
#################
# IN THE BELOW CODE BLOCK
# Write to CSV
#################

write_csv(parent_img_dates, paste0(derivative_data, 'Parent_PrePost_MRI_Menarche_9_16_25.csv'))
write_csv(youth_img_dates, paste0(derivative_data, 'Youth_PrePost_MRI_Menarche_9_16_25.csv'))
