##########################
# Thelarche_Age_Status_6.0.R
# Author: Robert Toms
# Some code adapted from: Aaron Clouse
# Date: 12/16/2025
##########################

# define paths
sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/6.0/"

# The purpose of this code is to determine both Thelarche Status and Age at Thelarche for all relevant participants.

library(tidyverse)
library(lubridate)

# read in data
parent <- read_tsv(paste0(sourcepath, '/Physical_Health/ph_p_pds.tsv'))
youth <- read_tsv(paste0(sourcepath, '/Physical_Health/ph_y_pds.tsv'))
sex <- read_tsv(paste0(sourcepath, '/abcd_general/ab_g_stc.tsv'))
dates <- read_tsv(paste0(sourcepath, '/abcd_general/ab_g_dyn.tsv'))

##########################
# IN THE BELOW CODE BLOCK
# Clean, rename, and merge data
##########################

# isolate interview dates to calculate report gaps later
dates <- dates %>%
  mutate(interview_date = as.Date(ab_g_dyn__visit_dtt, format = "%Y-%m-%d")) %>%
  select(participant_id, session_id, interview_date)

# pivot interview dates and make a searchable vector of dates
dates <- dates %>%
  pivot_wider(names_from = session_id, values_from = interview_date) %>%
  group_by(participant_id) %>%
  mutate(dates = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`))) %>%
  select(participant_id, dates)

# Make Sex data human readable
sex <- sex %>%
  select(participant_id, ab_g_stc__cohort_sex) %>% # Biological Sex
  rename(sex = ab_g_stc__cohort_sex) %>%
  mutate(sex = case_when(
    sex == 1 ~ "M",
    sex == 2 ~ "F"))

# Merge in Sex Data
parent <- parent %>%
  left_join(sex, by = "participant_id")
youth <- youth %>%
  left_join(sex, by = "participant_id")

# remove males
parent <- parent %>%
  # before: 11868
  filter(sex != "M") %>%
  select(-sex)
length(unique(parent$participant_id)) # remaining: 5677
youth <- youth %>%
  # before: 11868
  filter(sex != "M") %>%
  select(-sex)
length(unique(youth$participant_id)) # remaining: 5678

## 1 missing from parent report, who and why?
parent_subs <- parent %>% distinct(participant_id) %>% pull(participant_id)
all_females <- sex %>% filter(sex == 'F') %>% distinct(participant_id) %>% pull(participant_id)
missing <- setdiff(all_females, parent_subs)
# sub-FZWKRBW7, reason unclear after manual inspection. Only has a baseline report in youth.

### VARIABLE MEANINGS for ph_p_pds__f_001 and ph_y_pds__f_001 ###
# Have you noticed that your (child's) breasts have begun to grow?
# 1 = have not yet started growing
# 2 = have barely started growing
# 3 = breast growth is definitely underway
# 4 = breast growth seems complete
# 777 = Refuse to answer
# 999 = I don't know

# Isolate and rename relevant variables for readability
parent <- parent %>%
  select(participant_id, 
         session_id, ph_p_pds_age,
         ph_p_pds__f_001) %>% # parent Thelarche Report
  rename(age = ph_p_pds_age,
         thelarche_report = ph_p_pds__f_001)

youth <- youth %>%
  select(participant_id, 
         session_id, ph_y_pds_age,
         ph_y_pds__f_001) %>% # youth Thelarche Report
  rename(age = ph_y_pds_age,
         thelarche_report = ph_y_pds__f_001)

# Count 777s and 999s
youth_999 <- youth %>%            
  filter(thelarche_report == 999) # 1132 tps to be set to NA
youth_777 <- youth %>%            
  filter(thelarche_report == 777) # 923 tps to be set to NA
parent_999 <- parent %>%
  filter(thelarche_report == 999) # 329 tps to be set to NA
y_999_subjects <- length(table(youth_999$participant_id)) # 1074 subjects affected
y_777_subjects <- length(table(youth_777$participant_id)) # 650 subjects affected
p_999_subjects <- length(table(parent_999$participant_id)) # 240 subjects affected

# Set 777s and 999s to NA, set 'n/a' as NA, remaining values to numeric so they can be numerically compared
parent <- parent %>%
  mutate(thelarche_report = if_else(thelarche_report == "777" | thelarche_report == "999" | thelarche_report == 'n/a', NA, as.numeric(thelarche_report)))
youth <- youth %>%
  mutate(thelarche_report = if_else(thelarche_report == "777" | thelarche_report == "999" | thelarche_report == 'n/a', NA, as.numeric(thelarche_report)))

# Count subjects with no/insufficient data
parent <- parent %>%
  group_by(participant_id) %>%
  mutate(num_reports = sum(!is.na(thelarche_report))) %>%
  ungroup()
youth <- youth %>%
  group_by(participant_id) %>%
  mutate(num_reports = sum(!is.na(thelarche_report))) %>%
  ungroup()

counter <- parent %>% select(participant_id, num_reports) %>% unique()
table(counter$num_reports) # 18 subjects with no data, 179 with only 1 timepoint of data
counter <- youth %>% select(participant_id, num_reports) %>% unique()
table(counter$num_reports) # 38 subjects with no data, 207 with only 1 timepoint of data

# remove subjects with 0 timepoints of valid data
parent <- parent %>% filter(num_reports > 0)
youth <- youth %>% filter(num_reports > 0)

#########################
# IN THE BELOW CODE BLOCK
# Pivot and Format Thelarche Data for Thelarche search
#########################

# Add another session ID column for pivoting both ages and thelarche reports
parent <- parent %>%
  mutate(ses_id = str_replace(session_id, "ses-", "age_"))
youth <- youth %>%
  mutate(ses_id = str_replace(session_id, "ses-", "age_"))

# pivot thelarche reports and interview ages
parent <- parent %>%
  pivot_wider(names_from = session_id, values_from = thelarche_report) %>%
  pivot_wider(names_from = ses_id, values_from = age)
youth <- youth %>%
  pivot_wider(names_from = session_id, values_from = thelarche_report) %>%
  pivot_wider(names_from = ses_id, values_from = age)

# fill and keep one row per participant
parent <- parent %>%
  group_by(participant_id) %>%
  fill(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`,
         age_00A, age_01A, age_02A, age_03A, age_04A, age_05A, age_06A), .direction = "downup") %>%
  unique()

youth <- youth %>%
  group_by(participant_id) %>%
  fill(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`,
         age_00A, age_01A, age_02A, age_03A, age_04A, age_05A, age_06A), .direction = "downup") %>%
  unique()  

# make searchable chronological list vectors of Thelarche and age values
parent <- parent %>%
  mutate(thelarche_reports = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`))) %>%
  mutate(ages = list(c(age_00A, age_01A, age_02A, age_03A, age_04A, age_05A, age_06A)))
youth <- youth %>%
  mutate(thelarche_reports = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`))) %>%
  mutate(ages = list(c(age_00A, age_01A, age_02A, age_03A, age_04A, age_05A, age_06A)))  

##########################
# IN THE BELOW CODE BLOCK
# Check for Pre-Thelarche Throughout, Post-Thelarche at baseline/First Report, Missing Baseline Report and Inconsistent Reporting
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
parent <- parent %>% mutate(PostThelarche_at_Baseline_Y1N0 = if_else(`ses-00A` == 2 | `ses-00A` == 3 | `ses-00A` == 4, 1, 0))
youth <- youth %>% mutate(PostThelarche_at_Baseline_Y1N0 = if_else(`ses-00A` == 2 | `ses-00A` == 3 | `ses-00A` == 4, 1, 0))
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
parent <- parent %>% mutate(No_Baseline_Y1N0 = if_else(is.na(`ses-00A`), 1, 0))
youth <- youth %>% mutate(No_Baseline_Y1N0 = if_else(is.na(`ses-00A`), 1, 0))

### Inconsistent Reporting
# If there is any inconsistent/retro-thelarchal reporting (occurrence of a 1 after a 2, 3, or 4),
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
  group_by(participant_id) %>%
  mutate(Inconsistent_Y1N0 = find_inconsistent(thelarche_reports))
youth <- youth %>%
  group_by(participant_id) %>%
  mutate(Inconsistent_Y1N0 = find_inconsistent(thelarche_reports))


##########################
# IN THE BELOW CODE BLOCK
# Find Visit of last pre-thelarche endorsement
# Find Visit of first post-thelarche endorsement
##########################

# find last pre-thelarche and first post-thelarche timepoints
parent <- parent %>%
  group_by(participant_id) %>%
  mutate(Last_PreThelarche = max(which(unlist(thelarche_reports) == 1), na.rm = TRUE) - 1) %>% # get session number of last prethelarche report [index - 1]
  mutate(First_PostThelarche = min(which(unlist(thelarche_reports) %in% c(2, 3, 4)), na.rm = TRUE) - 1) %>% # get session number of first post-thelarche report [index - 1]
  mutate(Last_PreThelarche = if_else(is.infinite(Last_PreThelarche), NA, Last_PreThelarche)) %>% # set Inf's back to NA
  mutate(First_PostThelarche = if_else(is.infinite(First_PostThelarche), NA, First_PostThelarche))

youth <- youth %>%
  group_by(participant_id) %>%
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
p_counter %>% distinct(participant_id) %>% nrow() # Parent: 5659 subjects
y_counter <- youth
y_counter %>% distinct(participant_id) %>% nrow() # Youth: 5640 subjects

# PreThelarche ThruOut/at Last Report
parent_exclude <- parent %>%
  filter(Pre_Thelarche_ThruOut_Y1N0 == 0)
youth_exclude <- youth %>%
  filter(Pre_Thelarche_ThruOut_Y1N0 == 0)

# Count after exclusions
p_counter <- parent_exclude
p_counter %>% distinct(participant_id) %>% nrow() # Parent: 5659 -> 5585 subjects, 74 removed
y_counter <- youth_exclude 
y_counter %>% distinct(participant_id) %>% nrow() # Parent: 5640 -> 5542 subjects, 98 removed

# PostThelarche at Baseline
parent_exclude <- parent_exclude %>%
  filter(PostThelarche_at_Baseline_Y1N0 == 0)
youth_exclude <- youth_exclude %>%
  filter(PostThelarche_at_Baseline_Y1N0 == 0)

# Count after exclusions
p_counter <- parent_exclude
p_counter %>% distinct(participant_id) %>% nrow() # Parent: 5585 -> 2192 subjects, 3393 removed
y_counter <- youth_exclude 
y_counter %>% distinct(participant_id) %>% nrow() # Parent: 5542 -> 2680 subjects, 2862 removed

# Inconsistent
parent_exclude <- parent_exclude %>%
  filter(Inconsistent_Y1N0 == 0)
youth_exclude <- youth_exclude %>%
  filter(Inconsistent_Y1N0 == 0)

# Count after exclusions
p_counter <- parent_exclude
p_counter %>% distinct(participant_id) %>% nrow() # Parent: 2192 -> 2146 subjects, 46 removed
y_counter <- youth_exclude
y_counter %>% distinct(participant_id) %>% nrow() # Parent: 2680 -> 2523 subjects, 157 removed

# PostThelarche at First Report -- (remaining unusable subjects with no baseline report)
parent_exclude <- parent_exclude %>%
  filter(PostThelarche_at_FirstReport_Y1N0 == 0)
youth_exclude <- youth_exclude %>%
  filter(PostThelarche_at_FirstReport_Y1N0 == 0)

# Count after exclusions
p_counter <- parent_exclude
p_counter %>% distinct(participant_id) %>% nrow() # Parent: 2146 -> 2100 subjects, 46 removed
y_counter <- youth_exclude
y_counter %>% distinct(participant_id) %>% nrow() # Parent: 2523 -> 1830 subjects, 693 removed


# Reset Last_PreThelarche and First_PostThelarche to NA if flagged as Inconsistent
parent <- parent %>%
  mutate(Last_PreThelarche = if_else(Inconsistent_Y1N0 == 1, NA, Last_PreThelarche),
         First_PostThelarche = if_else(Inconsistent_Y1N0 == 1, NA, First_PostThelarche))
youth <- youth %>%
  mutate(Last_PreThelarche = if_else(Inconsistent_Y1N0 == 1, NA, Last_PreThelarche),
         First_PostThelarche = if_else(Inconsistent_Y1N0 == 1, NA, First_PostThelarche))

# Sanity Check --
sanity_p <- parent %>% mutate(has_both = (!is.na(Last_PreThelarche) & !is.na(First_PostThelarche)))
table(sanity_p$has_both) # 2100 subjects
sanity_y <- youth %>% mutate(has_both = (!is.na(Last_PreThelarche) & !is.na(First_PostThelarche)))
table(sanity_y$has_both) # 1830 subjects

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

# Find length of gaps between Pre/Post timepoints
parent <- parent %>%
  mutate(Age_of_Thelarche = (post_thelarche_age + pre_thelarche_age) / 2)
youth <- youth %>%
  mutate(Age_of_Thelarche = (post_thelarche_age + pre_thelarche_age) / 2)

# Count Ages
sum(!is.na(parent$Age_of_Thelarche)) # 2100
sum(!is.na(youth$Age_of_Thelarche)) # 1830

##########################
# IN THE BELOW CODE BLOCK
# Count timepoint gaps
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
  left_join(dates, by = "participant_id")
youth <- youth %>% 
  left_join(dates, by = "participant_id")

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
  select(participant_id, Age_of_Thelarche, Last_PreThelarche, First_PostThelarche, flag_summary,
         pre_thelarche_age, post_thelarche_age,
         thelarche_reports, gap_length, PrePost_gap_in_days, last_visit,
         Reporting_Gap_Y1N0, Pre_Thelarche_ThruOut_Y1N0, PostThelarche_at_Baseline_Y1N0, No_Baseline_Y1N0, PostThelarche_at_FirstReport_Y1N0, Inconsistent_Y1N0, 
         age_00A, age_01A, age_02A, age_03A, age_04A, age_05A, age_06A,
         `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)
youth <- youth %>%
  select(participant_id, Age_of_Thelarche, Last_PreThelarche, First_PostThelarche, flag_summary,
         pre_thelarche_age, post_thelarche_age,
         thelarche_reports, gap_length, PrePost_gap_in_days, last_visit,
         Reporting_Gap_Y1N0, Pre_Thelarche_ThruOut_Y1N0, PostThelarche_at_Baseline_Y1N0, No_Baseline_Y1N0, PostThelarche_at_FirstReport_Y1N0, Inconsistent_Y1N0, 
         age_00A, age_01A, age_02A, age_03A, age_04A, age_05A, age_06A,
         `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)

# Convert thelarche reports list to character type so it can be written into a csv
parent <- parent %>%
  mutate(thelarche_reports = paste(thelarche_reports, collapse = ", "))
youth <- youth %>%
  mutate(thelarche_reports = paste(thelarche_reports, collapse = ", "))

##########################
# IN THE BELOW CODE BLOCK
# Write to CSVs
##########################

write_csv(parent, paste0(derivative_data, "Parent_Thelarche_Age_Status_12_16_25.csv"))
write_csv(youth, paste0(derivative_data, "Youth_Thelarche_Age_Status_12_16_25.csv"))

