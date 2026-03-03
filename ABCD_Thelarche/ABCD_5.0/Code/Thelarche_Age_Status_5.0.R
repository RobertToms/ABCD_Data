##########################
# Thelarche_Age_Status_5.0.R
# Author: Robert Toms
# Some code adapted from: Aaron Clouse
# Date: 1/22/2026
##########################

# define paths
datapath <- "/path/to/ABCD_5.0/core"
derivative_data <- "/path/to/derivative_data/5.0/"

# The purpose of this code is to determine both Thelarche Status and Age at Thelarche for all relevant participants.

library(tidyverse)
library(lubridate)

# read in data
parent <- read_csv(paste0(datapath, "/physical-health/ph_p_pds.csv"))
youth <- read_csv(paste0(datapath, "/physical-health/ph_y_pds.csv"))
sex <- read_csv(paste0(datapath, '/abcd-general/abcd_p_demo.csv'))
age_date <- read_csv(paste0(datapath, "/abcd-general/abcd_y_lt.csv"))

##########################
# IN THE BELOW CODE BLOCK
# Clean, rename, and merge data
##########################

# isolate interview dates to calculate report gaps later
dates <- age_date %>%
  mutate(interview_date = mdy(interview_date)) %>%
  select(src_subject_id, eventname, interview_date)

# convert interview age from months to years
ages <- age_date %>%
  mutate(interview_age = interview_age / 12) %>%
  select(src_subject_id, eventname, interview_age)

# pivot interview dates and make a searchable vector of dates
dates <- dates %>%
  pivot_wider(names_from = eventname, values_from = interview_date) %>%
  group_by(src_subject_id) %>%
  mutate(dates = list(c(`baseline_year_1_arm_1`, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`))) %>%
  select(src_subject_id, dates)

# Make Sex data human readable
sex <- sex %>%
  select(src_subject_id, demo_sex_v2) %>% # Biological Sex
  rename(sex = demo_sex_v2) %>%
  mutate(sex = case_when(
    sex == 1 ~ "M",
    sex == 2 ~ "F",
    sex == 3 ~ "IM")) %>%
  filter(!is.na(sex))

# Merge in Sex Data
parent <- parent %>%
  left_join(sex, by = "src_subject_id")
youth <- youth %>%
  left_join(sex, by = "src_subject_id")

# Count Subjects
parent_counter <- parent %>% distinct(src_subject_id) %>% pull(src_subject_id) # 11868
youth_counter <- youth %>% distinct(src_subject_id) %>% pull(src_subject_id) # 11868

# Remove Males and Intersex Males
parent <- parent %>% filter(sex == 'F')
youth <- youth %>% filter(sex == 'F')

# Count Subjects
parent_counter <- parent %>% distinct(src_subject_id) %>% pull(src_subject_id) # 5677
youth_counter <- youth %>% distinct(src_subject_id) %>% pull(src_subject_id) # 5677

### VARIABLE MEANINGS for pds_f4_p and pds_f4_2_y ###
# Have you noticed that your (child's) breasts have begun to grow?
# 1 = have not yet started growing
# 2 = have barely started growing
# 3 = breast growth is definitely underway
# 4 = breast growth seems complete
# 777 = Refuse to answer
# 999 = I don't know

# Isolate and rename relevant variables for readability
parent <- parent %>%
  select(src_subject_id, 
         eventname,
         pds_f4_p) %>% # parent Thelarche Report
  rename(thelarche_report = pds_f4_p)

youth <- youth %>%
  select(src_subject_id, 
         eventname,
         pds_f4_2_y) %>% # youth Thelarche Report
  rename(thelarche_report = pds_f4_2_y)

# count 777s and 999s
youth_999 <- youth %>%            
  filter(thelarche_report == 999) # 1131 tps to be set to NA
youth_777 <- youth %>%            
  filter(thelarche_report == 777) # 725 tps to be set to NA
parent_999 <- parent %>%
  filter(thelarche_report == 999) # 227 tps to be set to NA
y_999_subjects <- length(table(youth_999$src_subject_id)) # 1073 subjects affected
y_777_subjects <- length(table(youth_777$src_subject_id)) # 565 subjects affected
p_999_subjects <- length(table(parent_999$src_subject_id)) # 171 subjects affected


# Set 777s and 999s to NA
parent <- parent %>%
  mutate(thelarche_report = if_else(thelarche_report == "777" | thelarche_report == "999", NA, thelarche_report))
youth <- youth %>%
  mutate(thelarche_report = if_else(thelarche_report == "777" | thelarche_report == "999", NA, thelarche_report))

# Count Subjects
parent_counter <- parent %>% filter(!is.na(thelarche_report)) %>% distinct(src_subject_id) %>% pull(src_subject_id) # 5677 -> 5655, -22
youth_counter <- youth %>% filter(!is.na(thelarche_report)) %>% distinct(src_subject_id) %>% pull(src_subject_id) # 5677 -> 5627, -50

# officially remove those with no usable Thelarche reports
parent <- parent %>% filter(!is.na(thelarche_report))
youth <- youth %>% filter(!is.na(thelarche_report))

# merge in age data
parent <- parent %>%
  left_join(ages, by = c("src_subject_id", "eventname"))
youth <- youth %>%
  left_join(ages, by = c("src_subject_id", "eventname"))

#########################
# IN THE BELOW CODE BLOCK
# Pivot and Format Thelarche Data for Thelarche search
#########################

# Rename/Add another age-specific eventname column to anticipate renaming itemized interview age and thelarche report columns
parent <- parent %>%
  mutate(agecols = case_when(
    eventname == "baseline_year_1_arm_1" ~ "baseline_age",
    eventname == "1_year_follow_up_y_arm_1" ~ 'year1_age',
    eventname == "2_year_follow_up_y_arm_1" ~ 'year2_age',
    eventname == "3_year_follow_up_y_arm_1" ~ 'year3_age',
    eventname == "4_year_follow_up_y_arm_1" ~ 'year4_age')) %>%
  mutate(eventname = case_when(
    eventname == "baseline_year_1_arm_1" ~ "baseline_thel_report",
    eventname == "1_year_follow_up_y_arm_1" ~ 'year1_thel_report',
    eventname == "2_year_follow_up_y_arm_1" ~ 'year2_thel_report',
    eventname == "3_year_follow_up_y_arm_1" ~ 'year3_thel_report',
    eventname == "4_year_follow_up_y_arm_1" ~ 'year4_thel_report'))
youth <- youth %>%
  mutate(agecols = case_when(
    eventname == "baseline_year_1_arm_1" ~ "baseline_age",
    eventname == "1_year_follow_up_y_arm_1" ~ 'year1_age',
    eventname == "2_year_follow_up_y_arm_1" ~ 'year2_age',
    eventname == "3_year_follow_up_y_arm_1" ~ 'year3_age',
    eventname == "4_year_follow_up_y_arm_1" ~ 'year4_age')) %>%
  mutate(eventname = case_when(
    eventname == "baseline_year_1_arm_1" ~ "baseline_thel_report",
    eventname == "1_year_follow_up_y_arm_1" ~ 'year1_thel_report',
    eventname == "2_year_follow_up_y_arm_1" ~ 'year2_thel_report',
    eventname == "3_year_follow_up_y_arm_1" ~ 'year3_thel_report',
    eventname == "4_year_follow_up_y_arm_1" ~ 'year4_thel_report'))

# pivot thelarche reports and interview ages
parent <- parent %>%
  pivot_wider(names_from = eventname, values_from = thelarche_report) %>%
  pivot_wider(names_from = agecols, values_from = interview_age)
youth <- youth %>%
  pivot_wider(names_from = eventname, values_from = thelarche_report) %>%
  pivot_wider(names_from = agecols, values_from = interview_age)

# fill and keep one row per participant
parent <- parent %>%
  group_by(src_subject_id) %>%
  fill(c(`baseline_thel_report`, `year1_thel_report`, `year2_thel_report`, `year3_thel_report`, `year4_thel_report`,
         baseline_age, year1_age, year2_age, year3_age, year4_age), .direction = "downup") %>%
  unique()

youth <- youth %>%
  group_by(src_subject_id) %>%
  fill(c(`baseline_thel_report`, `year1_thel_report`, `year2_thel_report`, `year3_thel_report`, `year4_thel_report`,
         baseline_age, year1_age, year2_age, year3_age, year4_age), .direction = "downup") %>%
  unique()

# make searchable chronological list vectors of Thelarche and age values
parent <- parent %>%
  mutate(thelarche_reports = list(c(`baseline_thel_report`, `year1_thel_report`, `year2_thel_report`, `year3_thel_report`, `year4_thel_report`))) %>%
  mutate(ages = list(c(baseline_age, year1_age, year2_age, year3_age, year4_age)))
youth <- youth %>%
  mutate(thelarche_reports = list(c(`baseline_thel_report`, `year1_thel_report`, `year2_thel_report`, `year3_thel_report`, `year4_thel_report`))) %>%
  mutate(ages = list(c(baseline_age, year1_age, year2_age, year3_age, year4_age)))


##########################
# IN THE BELOW CODE BLOCK
# Check for Pre-Thelarche Throughout, Post-Thelarche at baseline, and Inconsistent Reporting
##########################

### Pre-Thelarche Throughout
# If there exists at least 1 parent report of no Thelarchal development (1), 
# and no reports of confirmed Thelarchal development (2, 3, 4), 
# the subject is considered Pre-Thelarche as of ABCD 6.0, and a flag is added to the Pre_Thelarche_ThruOut_Y1N0 column.
parent$Pre_Thelarche_ThruOut_Y1N0 <- ifelse(sapply(parent$thelarche_reports, 
                                                   function(j) all(is.na(j) | j == 1) & # checks there are ONLY NA's and 1s
                                                     any(j == 1, na.rm = TRUE)),        # checks there is at least one 1
                                            1, 0)                 # if both of the above are true, add flag, else 0      
youth$Pre_Thelarche_ThruOut_Y1N0 <- ifelse(sapply(youth$thelarche_reports, 
                                                  function(j) all(is.na(j) | j == 1) & # checks there are ONLY NA's and 1s
                                                    any(j == 1, na.rm = TRUE)),        # checks there is at least one 1
                                           1, 0)                 # if both of the above are true, add flag, else 0   

### Post-Thelarche at Baseline
# If the list vector starts with a report of confirmed Thelarchal development (2, 3, 4),
# the subject is considered Post-Thelarche at Baseline and a flag is added to the PostThelarche_at_Baseline_Y1N0 column.
parent <- parent %>% mutate(PostThelarche_at_Baseline_Y1N0 = if_else(baseline_thel_report == 2 | baseline_thel_report == 3 | baseline_thel_report == 4, 1, 0))
youth <- youth %>% mutate(PostThelarche_at_Baseline_Y1N0 = if_else(baseline_thel_report == 2 | baseline_thel_report == 3 | baseline_thel_report == 4, 1, 0))
# if missing Baseline, this flag will be NA - set to 0
parent <- parent %>% mutate(PostThelarche_at_Baseline_Y1N0 = if_else(is.na(PostThelarche_at_Baseline_Y1N0), 0, PostThelarche_at_Baseline_Y1N0))
youth <- youth %>% mutate(PostThelarche_at_Baseline_Y1N0 = if_else(is.na(PostThelarche_at_Baseline_Y1N0), 0, PostThelarche_at_Baseline_Y1N0))

### Post-Thelarche at First Report
# If the list vector starts with a report of confirmed Thelarchal development (2, 3, 4),
# the subject is considered Post-Thelarche at Baseline and a flag is added to the PostThelarche_at_Baseline_Y1N0 column.
parent <- parent %>% rowwise() %>% mutate(PostThelarche_at_FirstReport_Y1N0 = if_else(na.exclude(thelarche_reports)[1] %in% c("2", "3", "4"), 1, 0))
youth <- youth %>% rowwise() %>% mutate(PostThelarche_at_FirstReport_Y1N0 = if_else(na.exclude(thelarche_reports)[1] %in% c("2", "3", "4"), 1, 0))

### No Baseline Report
# Flagged if baseline report is NA
parent <- parent %>% mutate(No_Baseline_Y1N0 = if_else(is.na(baseline_thel_report), 1, 0))
youth <- youth %>% mutate(No_Baseline_Y1N0 = if_else(is.na(baseline_thel_report), 1, 0))


### Inconsistent Reporting
# If there is any inconsistent/retro-thelarchal reporting (ocurrence of a 1 after a 2, 3, or 4),
# a flag for inconsistent reporting is added to the Inconsistent_Y1N0 column.

# define function to flag inconsistent reports
find_inconsistent <- function(thelarche_reports) {
  thelarche_reports <- unlist(thelarche_reports) # convert to searchable vector from list
  is_inconsistent <- 0 # instantiate output variable
  first_thelarche <- which(thelarche_reports %in% c(2, 3, 4))[1] # find first occurrence of post-thelarche report
  last_pre_thelarche <- max(which(thelarche_reports == 1)) # find last occurrence of pre-thelarche report
  is_inconsistent <- if (is.na(last_pre_thelarche) | is.na(first_thelarche)) {0} # if missing any pre or post thelarche data, not inconsistent
  is_inconsistent <- if (!is.na(last_pre_thelarche) && !is.na(first_thelarche) && last_pre_thelarche > first_thelarche) {1} else {0} # flag inconsistent with "1"
  return(is_inconsistent)
}

# apply function
parent <- parent %>%
  group_by(src_subject_id) %>%
  mutate(Inconsistent_Y1N0 = find_inconsistent(thelarche_reports))
youth <- youth %>%
  group_by(src_subject_id) %>%
  mutate(Inconsistent_Y1N0 = find_inconsistent(thelarche_reports))


##########################
# IN THE BELOW CODE BLOCK
# Find Visit of last pre-thelarche endorsement
# Find Visit of first post-thelarche endorsement
##########################

# find last pre-thelarche and first post-thelarche timepoints
parent <- parent %>%
  group_by(src_subject_id) %>%
  mutate(Last_PreThelarche = max(which(unlist(thelarche_reports) == 1), na.rm = TRUE) - 1) %>% # get session number of last prethelarche report [index - 1]
  mutate(First_PostThelarche = min(which(unlist(thelarche_reports) %in% c(2, 3, 4)), na.rm = TRUE) - 1) %>% # get session number of first post-thelarche report [index - 1]
  mutate(Last_PreThelarche = if_else(is.infinite(Last_PreThelarche), NA, Last_PreThelarche)) %>% # set Inf's back to NA
  mutate(First_PostThelarche = if_else(is.infinite(First_PostThelarche), NA, First_PostThelarche))

youth <- youth %>%
  group_by(src_subject_id) %>%
  mutate(Last_PreThelarche = max(which(unlist(thelarche_reports) == 1), na.rm = TRUE) - 1) %>% # get session number of last prethelarche report [index - 1]
  mutate(First_PostThelarche = min(which(unlist(thelarche_reports) %in% c(2, 3, 4)), na.rm = TRUE) - 1) %>% # get session number of first post-thelarche report [index - 1]
  mutate(Last_PreThelarche = if_else(is.infinite(Last_PreThelarche), NA, Last_PreThelarche)) %>% # set Inf's back to NA
  mutate(First_PostThelarche = if_else(is.infinite(First_PostThelarche), NA, First_PostThelarche))

##########################
# IN THE BELOW CODE BLOCK
# Count Exclusions
# Validity-Check Data
##########################

# identify all reporting cases present in the data
counter <- parent %>%
  rowwise() %>%
  mutate(num_flags = sum(Pre_Thelarche_ThruOut_Y1N0, PostThelarche_at_FirstReport_Y1N0, No_Baseline_Y1N0, Inconsistent_Y1N0, na.rm = TRUE)) %>%
  filter(num_flags > 1)

### The following are all the reporting cases present in the data, and if we can determine a valid pre/post thelarche transition from them ###
#          *If a subject has both a valid prethelarche and a valid postthelarche timepoint, they will have an imputed age of thelarche
#
#       Normal/No Flags - Will have valid pre/post thelarche timepoints and thelarche age
#
#       Prethelarche ThruOut/at Last Report - has no postthelarche timepoints, so will not have a valid pre/post transition
#               - Same for PreThelarche ThruOut + No Baseline Report
#
#       Postthelarche at Baseline/First Report - has no clear prethelarche report, cannot determine valid pre/post transition due to limits of self-report data
#               - Same for PostThelarche at Baseline and Inconsistent Reporting
#
#       Inconsistent Reports - Does not have reliable reports, will not have a valid pre/post thelarche transition
#       
#       No Baseline Report
#               Consistent/no other flags - Has reliable reports, will have a valid pre/post thelarche transition
#               PostThelarche at First Report - see above
#               Inconsistent - Does not have reliable reports, will not have a valid pre/post thelarche transition
# 

# Count before exclusions
p_counter <- parent 
p_counter %>% distinct(src_subject_id) %>% nrow() # Parent: 5655 subjects
y_counter <- youth 
y_counter %>% distinct(src_subject_id) %>% nrow() # Youth: 5627 subjects

# PreThelarche ThruOut/at Last Report
parent_exclude <- parent %>%
  filter(Pre_Thelarche_ThruOut_Y1N0 == 0)
youth_exclude <- youth %>%
  filter(Pre_Thelarche_ThruOut_Y1N0 == 0)

# Count after exclusions
p_counter <- parent_exclude
p_counter %>% distinct(src_subject_id) %>% nrow() # Parent: 5655 -> 5527 subjects, 128 removed
y_counter <- youth_exclude 
y_counter %>% distinct(src_subject_id) %>% nrow() # Parent: 5627 -> 5450 subjects, 177 removed

# PostThelarche at Baseline
parent_exclude <- parent_exclude %>%
  filter(PostThelarche_at_Baseline_Y1N0 == 0)
youth_exclude <- youth_exclude %>%
  filter(PostThelarche_at_Baseline_Y1N0 == 0)

# Count after exclusions
p_counter <- parent_exclude
p_counter %>% distinct(src_subject_id) %>% nrow() # Parent: 5527 -> 2134 subjects, 3393 removed
y_counter <- youth_exclude 
y_counter %>% distinct(src_subject_id) %>% nrow() # Parent: 5450 -> 2588 subjects, 2862 removed

# Inconsistent
parent_exclude <- parent_exclude %>%
  filter(Inconsistent_Y1N0 == 0)
youth_exclude <- youth_exclude %>%
  filter(Inconsistent_Y1N0 == 0)

# Count after exclusions
p_counter <- parent_exclude
p_counter %>% distinct(src_subject_id) %>% nrow() # Parent: 2134 -> 2096 subjects, 38 removed
y_counter <- youth_exclude
y_counter %>% distinct(src_subject_id) %>% nrow() # Parent: 2588 -> 2447 subjects, 141 removed

# PostThelarche at First Report -- (remaining unusable subjects with no baseline report)
parent_exclude <- parent_exclude %>%
  filter(PostThelarche_at_FirstReport_Y1N0 == 0)
youth_exclude <- youth_exclude %>%
  filter(PostThelarche_at_FirstReport_Y1N0 == 0)

# Count after exclusions
p_counter <- parent_exclude
p_counter %>% distinct(src_subject_id) %>% nrow() # Parent: 2096 -> 2052 subjects, 44 removed
y_counter <- youth_exclude
y_counter %>% distinct(src_subject_id) %>% nrow() # Parent: 2447 -> 1763 subjects, 684 removed


# Reset Last_PreThelarche and First_PostThelarche to NA if flagged as Inconsistent
parent <- parent %>%
  mutate(Last_PreThelarche = if_else(Inconsistent_Y1N0 == 1, NA, Last_PreThelarche),
         First_PostThelarche = if_else(Inconsistent_Y1N0 == 1, NA, First_PostThelarche))
youth <- youth %>%
  mutate(Last_PreThelarche = if_else(Inconsistent_Y1N0 == 1, NA, Last_PreThelarche),
         First_PostThelarche = if_else(Inconsistent_Y1N0 == 1, NA, First_PostThelarche))

# Sanity Check --
sanity_p <- parent %>% mutate(has_both = (!is.na(Last_PreThelarche) & !is.na(First_PostThelarche)))
table(sanity_p$has_both) # 2052 subjects
sanity_y <- youth %>% mutate(has_both = (!is.na(Last_PreThelarche) & !is.na(First_PostThelarche)))
table(sanity_y$has_both) # 1763 subjects

##########################
# IN THE BELOW CODE BLOCK
# Calculate Age at Thelarche
##########################

# Get Pre/Post Thelarche Ages
parent <- parent %>%
  rowwise() %>%
  mutate(pre_thelarche_age = if (!is.na(Last_PreThelarche)) ages[[Last_PreThelarche + 1]] else NA,
         post_thelarche_age = if (!is.na(First_PostThelarche)) ages[[First_PostThelarche + 1]] else NA)
youth <- youth %>%
  rowwise() %>%
  mutate(pre_thelarche_age = if (!is.na(Last_PreThelarche)) ages[[Last_PreThelarche + 1]] else NA,
         post_thelarche_age = if (!is.na(First_PostThelarche)) ages[[First_PostThelarche + 1]] else NA)

# Impute probable age of Thelarche by finding midpoint between pre/post ages
parent <- parent %>%
  mutate(Age_of_Thelarche = (post_thelarche_age + pre_thelarche_age) / 2)
youth <- youth %>%
  mutate(Age_of_Thelarche = (post_thelarche_age + pre_thelarche_age) / 2)

##########################
# IN THE BELOW CODE BLOCK
# Handle timepoint gaps
# Calculate timepoint gap in days
# If there is an atypically large timepoint gap between pre/post, flag
##########################

# count gaps
parent <- parent %>%
  mutate(gap_length = First_PostThelarche - Last_PreThelarche)
youth <- youth %>%
  mutate(gap_length = First_PostThelarche - Last_PreThelarche)

## Add Flags for 1+ timepoint gaps in pre/post thelarche reports
parent <- parent %>%
  mutate(Reporting_Gap_Y1N0 = if_else(gap_length > 1, 1, 0))
youth <- youth %>%
  mutate(Reporting_Gap_Y1N0 = if_else(gap_length > 1, 1, 0))

# merge in interview dates to calculate gap in days
parent <- parent %>%
  left_join(dates, by = "src_subject_id")
youth <- youth %>% 
  left_join(dates, by = "src_subject_id")

# calculate gap in days
parent <- parent %>%
  mutate(PrePost_gap_in_days = dates[First_PostThelarche + 1] - dates[Last_PreThelarche + 1])
youth <- youth %>%
  mutate(PrePost_gap_in_days = dates[First_PostThelarche + 1] - dates[Last_PreThelarche + 1])

##########################
# IN THE BELOW CODE BLOCK
# Identify Dropouts/Find Last Visits
##########################

# Find last visit
parent <- parent %>%
  mutate(last_visit = if_else(all(is.na(thelarche_reports)), NA, max(which(!is.na(thelarche_reports))) - 1))
youth <- youth %>%
  mutate(last_visit = if_else(all(is.na(thelarche_reports)), NA, max(which(!is.na(thelarche_reports))) - 1))


##########################
# IN THE BELOW CODE BLOCK
# Clean DataFrames for export
##########################

# Add notes column summarizing flags that are present for a participant
parent <- parent %>%
  mutate(flag_summary = paste0(ifelse(Pre_Thelarche_ThruOut_Y1N0 == 1, "Pre_ThruOut, ", ""),
                               ifelse(PostThelarche_at_Baseline_Y1N0 == 1, "Post_at_Baseline, ", ""),
                               ifelse(No_Baseline_Y1N0 == 1, "No_Baseline_Report, ", ""),
                               ifelse(PostThelarche_at_FirstReport_Y1N0 == 1 & PostThelarche_at_Baseline_Y1N0 == 0, "Post_At_FirstReport, ", ""),
                               ifelse(Inconsistent_Y1N0 == 1, "Inconsistent, ", ""),
                               ifelse(!is.na(Reporting_Gap_Y1N0) & Reporting_Gap_Y1N0 == 1, "Reporting_Gap", "")))
youth <- youth %>%
  mutate(flag_summary = paste0(ifelse(Pre_Thelarche_ThruOut_Y1N0 == 1, "Pre_ThruOut, ", ""),
                               ifelse(PostThelarche_at_Baseline_Y1N0 == 1, "Post_at_Baseline, ", ""),
                               ifelse(No_Baseline_Y1N0 == 1, "No_Baseline_Report, ", ""),
                               ifelse(PostThelarche_at_FirstReport_Y1N0 == 1 & PostThelarche_at_Baseline_Y1N0 == 0, "Post_At_FirstReport, ", ""),
                               ifelse(Inconsistent_Y1N0 == 1, "Inconsistent, ", ""),
                               ifelse(!is.na(Reporting_Gap_Y1N0) & Reporting_Gap_Y1N0 == 1, "Reporting_Gap", "")))

# Keep and Reorder useful columns
parent <- parent %>%
  select(src_subject_id, Age_of_Thelarche, Last_PreThelarche, First_PostThelarche, flag_summary,
         pre_thelarche_age, post_thelarche_age,
         thelarche_reports, gap_length, PrePost_gap_in_days, last_visit,
         Reporting_Gap_Y1N0, Pre_Thelarche_ThruOut_Y1N0, PostThelarche_at_Baseline_Y1N0, No_Baseline_Y1N0, PostThelarche_at_FirstReport_Y1N0, Inconsistent_Y1N0, 
         baseline_age, year1_age, year2_age, year3_age, year4_age,
         `baseline_thel_report`, `year1_thel_report`, `year2_thel_report`, `year3_thel_report`, `year4_thel_report`)
youth <- youth %>%
  select(src_subject_id, Age_of_Thelarche, Last_PreThelarche, First_PostThelarche, flag_summary,
         pre_thelarche_age, post_thelarche_age,
         thelarche_reports, gap_length, PrePost_gap_in_days, last_visit,
         Reporting_Gap_Y1N0, Pre_Thelarche_ThruOut_Y1N0, PostThelarche_at_Baseline_Y1N0, No_Baseline_Y1N0, PostThelarche_at_FirstReport_Y1N0, Inconsistent_Y1N0, 
         baseline_age, year1_age, year2_age, year3_age, year4_age,
         `baseline_thel_report`, `year1_thel_report`, `year2_thel_report`, `year3_thel_report`, `year4_thel_report`)

# Convert thelarche reports list to character type so it can be written into a csv
parent <- parent %>%
  mutate(thelarche_reports = paste(thelarche_reports, collapse = ", "))
youth <- youth %>%
  mutate(thelarche_reports = paste(thelarche_reports, collapse = ", "))

##########################
# IN THE BELOW CODE BLOCK
# Write to CSVs
##########################

write_csv(parent, paste0(derivative_data, "Parent_5.0_Thelarche_Age_Status_1_22_26.csv"))
write_csv(youth, paste0(derivative_data, "Youth_5.0_Thelarche_Age_Status_1_22_26.csv"))
