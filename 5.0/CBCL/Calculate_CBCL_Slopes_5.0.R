##########################
# Calculate_CBCL_Slopes_5.0.R
# Author: Robert Toms
# Date: 09/11/2025
##########################

# The purpose of this code is to calculate changes/slopes in CBCL Internalizing and Externalizing.

library(tidyverse)
library(lubridate)

sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"
Ages <- read_csv(paste0(sourcepath, 'abcd-general/abcd_y_lt.csv'))
CBCL <- read_csv(paste0(sourcepath, 'mental-health/mh_p_cbcl.csv'))


#########################
# IN THE BELOW CODE BLOCK
# Organize Data
#########################

# keep only relevant columns
CBCL <- CBCL %>%
  select(src_subject_id, eventname,
         cbcl_scr_syn_internal_r,  # Internal CBCL Syndrome Scale (raw score)
         cbcl_scr_syn_internal_t,  # Internal CBCL Syndrome Scale (t-score)
         cbcl_scr_syn_internal_m,  # Internal CBCL Syndrome Scale (missing values)
         cbcl_scr_syn_internal_nm, # Internal CBCL Syndrome Scale (number of missing values)
         cbcl_scr_syn_external_r,  # External CBCL Syndrome Scale (raw score)
         cbcl_scr_syn_external_t,  # External CBCL Syndrome Scale (t-score)
         cbcl_scr_syn_external_m,  # External CBCL Syndrome Scale (missing values)
         cbcl_scr_syn_external_nm) # External CBCL Syndrome Scale (number of missing values)

Ages <- Ages %>%
  select(src_subject_id, eventname, interview_age, interview_date)

# Split CBCL Internalizing and Externalizing
# Raw scores will be used, to maintain consistency with existing ABCD literature
Internalizing_CBCL <- CBCL %>%
  select(src_subject_id, eventname, cbcl_scr_syn_internal_r)

Externalizing_CBCL <- CBCL %>%
  select(src_subject_id, eventname, cbcl_scr_syn_external_r)

# Pivot column of interest wider
Internalizing_CBCL <- Internalizing_CBCL %>%
  pivot_wider(names_from = eventname, values_from = cbcl_scr_syn_internal_r)

Externalizing_CBCL <- Externalizing_CBCL %>%
  pivot_wider(names_from = eventname, values_from = cbcl_scr_syn_external_r)

## Split and pivot Interview Ages and Dates
# Split
Dates <- Ages %>%
  select(src_subject_id, eventname, interview_date) %>%
  mutate(interview_date = mdy(interview_date))
# keep age column, convert from months to years
Ages <- Ages %>%
  select(src_subject_id, eventname, interview_age) %>%
  mutate(interview_age = interview_age/12)

# Pivot
Dates <- Dates %>%
  pivot_wider(names_from = eventname, values_from = interview_date)

## Clean and Clear Ages and Dates
# keep only useful columns
Dates <- Dates %>%
  select(src_subject_id, baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, 
         `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)

# rename
Dates <- Dates %>%
  rename(Baseline_Date = baseline_year_1_arm_1, 
         Year1_Date = `1_year_follow_up_y_arm_1`, 
         Year2_Date = `2_year_follow_up_y_arm_1`, 
         Year3_Date = `3_year_follow_up_y_arm_1`, 
         Year4_Date = `4_year_follow_up_y_arm_1`)

#########################
# IN THE BELOW CODE BLOCK
# Prep Data for Calculation
#########################

# Make a vector of CBCL Values, count number of valid timepoints per subject
Internalizing_CBCL <- Internalizing_CBCL %>%
  group_by(src_subject_id) %>%
  mutate(Int_CBCL_Values = list(c(baseline_year_1_arm_1, 
                                  `1_year_follow_up_y_arm_1`, 
                                  `2_year_follow_up_y_arm_1`, 
                                  `3_year_follow_up_y_arm_1`, 
                                  `4_year_follow_up_y_arm_1`))) %>%
  mutate(Timepoint_Count = rowSums(!is.na(across(c(baseline_year_1_arm_1, 
                                                   `1_year_follow_up_y_arm_1`, 
                                                   `2_year_follow_up_y_arm_1`, 
                                                   `3_year_follow_up_y_arm_1`, 
                                                   `4_year_follow_up_y_arm_1`)))))
Externalizing_CBCL <- Externalizing_CBCL %>%
  group_by(src_subject_id) %>%
  mutate(Ext_CBCL_Values = list(c(baseline_year_1_arm_1, 
                                  `1_year_follow_up_y_arm_1`, 
                                  `2_year_follow_up_y_arm_1`, 
                                  `3_year_follow_up_y_arm_1`, 
                                  `4_year_follow_up_y_arm_1`))) %>%
  mutate(Timepoint_Count = rowSums(!is.na(across(c(baseline_year_1_arm_1, 
                                                   `1_year_follow_up_y_arm_1`, 
                                                   `2_year_follow_up_y_arm_1`, 
                                                   `3_year_follow_up_y_arm_1`, 
                                                   `4_year_follow_up_y_arm_1`)))))

# Split off 2 timepoint and 3+ timepoint
Int_2TP <- Internalizing_CBCL %>%
  filter(Timepoint_Count == 2)
Ext_2TP <- Externalizing_CBCL %>%
  filter(Timepoint_Count == 2)
Int_3PlusTP <- Internalizing_CBCL %>%
  filter(Timepoint_Count >= 3)
Ext_3PlusTP <- Externalizing_CBCL %>%
  filter(Timepoint_Count >= 3)

#########################
# IN THE BELOW CODE BLOCK
# Calculate Differences/Slopes
#########################

## Calculate 2TP Differences
# Instantiate Differences Columns
Int_2TP <- Int_2TP %>%
  mutate(Int_CBCL_Raw_Difference = NA)
Ext_2TP <- Ext_2TP %>%
  mutate(Ext_CBCL_Raw_Difference = NA)

# Calculate Raw Differences for consecutive timepoints
Int_2TP <- Int_2TP %>%
  mutate(Int_CBCL_Raw_Difference = if_else(!is.na(baseline_year_1_arm_1) & !is.na(`1_year_follow_up_y_arm_1`),
                                      `1_year_follow_up_y_arm_1` - baseline_year_1_arm_1, Int_CBCL_Raw_Difference)) %>%
  mutate(Int_CBCL_Raw_Difference = if_else(!is.na(`2_year_follow_up_y_arm_1`) & !is.na(`1_year_follow_up_y_arm_1`),
                                      `2_year_follow_up_y_arm_1` - `1_year_follow_up_y_arm_1`, Int_CBCL_Raw_Difference)) %>%
  mutate(Int_CBCL_Raw_Difference = if_else(!is.na(`2_year_follow_up_y_arm_1`) & !is.na(`3_year_follow_up_y_arm_1`),
                                      `3_year_follow_up_y_arm_1` - `2_year_follow_up_y_arm_1`, Int_CBCL_Raw_Difference)) %>%
  mutate(Int_CBCL_Raw_Difference = if_else(!is.na(`3_year_follow_up_y_arm_1`) & !is.na(`4_year_follow_up_y_arm_1`),
                                      `4_year_follow_up_y_arm_1` - `3_year_follow_up_y_arm_1`, Int_CBCL_Raw_Difference)) %>%
  arrange(Int_CBCL_Raw_Difference)

Ext_2TP <- Ext_2TP %>%
  mutate(Ext_CBCL_Raw_Difference = if_else(!is.na(baseline_year_1_arm_1) & !is.na(`1_year_follow_up_y_arm_1`),
                                           `1_year_follow_up_y_arm_1` - baseline_year_1_arm_1, Ext_CBCL_Raw_Difference)) %>%
  mutate(Ext_CBCL_Raw_Difference = if_else(!is.na(`2_year_follow_up_y_arm_1`) & !is.na(`1_year_follow_up_y_arm_1`),
                                           `2_year_follow_up_y_arm_1` - `1_year_follow_up_y_arm_1`, Ext_CBCL_Raw_Difference)) %>%
  mutate(Ext_CBCL_Raw_Difference = if_else(!is.na(`2_year_follow_up_y_arm_1`) & !is.na(`3_year_follow_up_y_arm_1`),
                                           `3_year_follow_up_y_arm_1` - `2_year_follow_up_y_arm_1`, Ext_CBCL_Raw_Difference)) %>%
  mutate(Ext_CBCL_Raw_Difference = if_else(!is.na(`3_year_follow_up_y_arm_1`) & !is.na(`4_year_follow_up_y_arm_1`),
                                           `4_year_follow_up_y_arm_1` - `3_year_follow_up_y_arm_1`, Ext_CBCL_Raw_Difference)) %>%
  arrange(Ext_CBCL_Raw_Difference)

# Flag timepoint gaps
Int_2TP <- Int_2TP %>%
  mutate(Int_TimepointGapY1N0 = if_else(is.na(Int_CBCL_Raw_Difference), 1, 0))
Ext_2TP <- Ext_2TP %>%
  mutate(Ext_TimepointGapY1N0 = if_else(is.na(Ext_CBCL_Raw_Difference), 1, 0))

### Calculate raw differences for those with 2 timepoint gaps
# Find First and Last Timepoint
Int_2TP <- Int_2TP %>%
  rowwise() %>%
  mutate(FirstTP = first(which(!is.na(Int_CBCL_Values))),
         LastTP = last(which(!is.na(Int_CBCL_Values))))
Ext_2TP <- Ext_2TP %>%
  rowwise() %>%
  mutate(FirstTP = first(which(!is.na(Ext_CBCL_Values))),
         LastTP = last(which(!is.na(Ext_CBCL_Values))))

# Calculate differences
Int_2TP <- Int_2TP %>%
  mutate(Int_CBCL_Raw_Difference = if_else(Int_TimepointGapY1N0 == 1, 
                                           Int_CBCL_Values[LastTP] - Int_CBCL_Values[FirstTP],
                                           Int_CBCL_Raw_Difference))
Ext_2TP <- Ext_2TP %>%
  mutate(Ext_CBCL_Raw_Difference = if_else(Ext_TimepointGapY1N0 == 1, 
                                           Ext_CBCL_Values[LastTP] - Ext_CBCL_Values[FirstTP],
                                           Ext_CBCL_Raw_Difference))

## Calculate number of days between reports
# merge in interview dates
Int_2TP <- Int_2TP %>%
  left_join(Dates, by = 'src_subject_id')
Ext_2TP <- Ext_2TP %>%
  left_join(Dates, by = 'src_subject_id')

## Calculate days between reports
# make vector of dates
Int_2TP <- Int_2TP %>%
  mutate(interview_days = list(c(Baseline_Date, `Year1_Date`, `Year2_Date`, `Year3_Date`, `Year4_Date`)))
Ext_2TP <- Ext_2TP %>%
  mutate(interview_days = list(c(Baseline_Date, `Year1_Date`, `Year2_Date`, `Year3_Date`, `Year4_Date`)))
# calculate day difference between dates
Int_2TP <- Int_2TP %>%
  mutate(Int_Days_Between = if_else(Int_TimepointGapY1N0 == 1, interview_days[LastTP] - interview_days[FirstTP], NA))
Ext_2TP <- Ext_2TP %>%
  mutate(Ext_Days_Between = if_else(Ext_TimepointGapY1N0 == 1, interview_days[LastTP] - interview_days[FirstTP], NA))

### 3+ Timepoints
# Instantiate Slopes Columns
Int_3PlusTP <- Int_3PlusTP %>%
  mutate(Int_CBCL_Slope = NA)
Ext_3PlusTP <- Ext_3PlusTP %>%
  mutate(Ext_CBCL_Slope = NA)

# Pivot Longer
Int_3PlusTP <- Int_3PlusTP %>%
  pivot_longer(c(baseline_year_1_arm_1, 
                 `1_year_follow_up_y_arm_1`, 
                 `2_year_follow_up_y_arm_1`, 
                 `3_year_follow_up_y_arm_1`, 
                 `4_year_follow_up_y_arm_1`), names_to = "eventname", values_to = "Int_CBCL_Value")
Ext_3PlusTP <- Ext_3PlusTP %>%
  pivot_longer(c(baseline_year_1_arm_1, 
                 `1_year_follow_up_y_arm_1`, 
                 `2_year_follow_up_y_arm_1`, 
                 `3_year_follow_up_y_arm_1`, 
                 `4_year_follow_up_y_arm_1`), names_to = "eventname", values_to = "Ext_CBCL_Value")

## Merge in Ages
Int_3PlusTP <- Int_3PlusTP %>%
 left_join(Ages, by = 'src_subject_id')
Ext_3PlusTP <- Ext_3PlusTP %>%
 left_join(Ages, by = 'src_subject_id')
# Keep matching columns
Int_3PlusTP <- Int_3PlusTP %>%
  filter(eventname.x == eventname.y) %>%
  rename(eventname = eventname.x) %>%
  select(-eventname.y)
Ext_3PlusTP <- Ext_3PlusTP %>%
  filter(eventname.x == eventname.y) %>%
  rename(eventname = eventname.x) %>%
  select(-eventname.y)

# Remove NA CBCL Value rows
Int_3PlusTP <- Int_3PlusTP %>%
  # Before: 36727
  filter(!is.na(Int_CBCL_Value))
# After: 36462
Ext_3PlusTP <- Ext_3PlusTP %>%
  # Before: 36727
  filter(!is.na(Ext_CBCL_Value))
# After: 36462

# Replace NA ages with 0, 1, 2, 3, 4 years respective to eventname
Int_3PlusTP <- Int_3PlusTP %>%
  mutate(interview_age = if_else(is.na(interview_age), case_when(
    eventname == "baseline_year_1_arm_1" ~ 0,
    eventname == "1_year_follow_up_y_arm_1" ~ 1,
    eventname == "2_year_follow_up_y_arm_1" ~ 2,
    eventname == "3_year_follow_up_y_arm_1" ~ 3,
    eventname == "4_year_follow_up_y_arm_1" ~ 4), interview_age))
Ext_3PlusTP <- Ext_3PlusTP %>%
  mutate(interview_age = if_else(is.na(interview_age), case_when(
    eventname == "baseline_year_1_arm_1" ~ 0,
    eventname == "1_year_follow_up_y_arm_1" ~ 1,
    eventname == "2_year_follow_up_y_arm_1" ~ 2,
    eventname == "3_year_follow_up_y_arm_1" ~ 3,
    eventname == "4_year_follow_up_y_arm_1" ~ 4), interview_age))


# Calculate slopes
Int_3PlusTP_Slopes <- Int_3PlusTP %>%
  group_by(src_subject_id) %>%
  group_modify(~ {model <- lm(Int_CBCL_Value ~ interview_age, data = .)
  tibble(Int_CBCL_Slope = coef(model)[2]) })
Ext_3PlusTP_Slopes <- Ext_3PlusTP %>%
  group_by(src_subject_id) %>%
  group_modify(~ {model <- lm(Ext_CBCL_Value ~ interview_age, data = .)
  tibble(Ext_CBCL_Slope = coef(model)[2]) })


## Merge Difference/Slope Data back in
# Clean 2TPs
Int_2TP <- Int_2TP %>%
  select(src_subject_id, Int_CBCL_Raw_Difference, Int_TimepointGapY1N0, Int_Days_Between)
Ext_2TP <- Ext_2TP %>%
  select(src_subject_id, Ext_CBCL_Raw_Difference, Ext_TimepointGapY1N0, Ext_Days_Between)

# 3+ TPs
Internalizing_CBCL <- Internalizing_CBCL %>%
  left_join(Int_3PlusTP_Slopes, by = 'src_subject_id')
Externalizing_CBCL <- Externalizing_CBCL %>%
  left_join(Ext_3PlusTP_Slopes, by = 'src_subject_id')
# 2 TPs
Internalizing_CBCL <- Internalizing_CBCL %>%
  left_join(Int_2TP, by = 'src_subject_id')
Externalizing_CBCL <- Externalizing_CBCL %>%
  left_join(Ext_2TP, by = 'src_subject_id')

#########################
# IN THE BELOW CODE BLOCK
# Add Age vector for reference
#########################

## make vector of ages
# Pivot
Ages <- Ages %>%
  pivot_wider(names_from = eventname, values_from = interview_age)
# keep useful columns
Ages <- Ages %>%
  select(src_subject_id, baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, 
         `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)
# make vector
Ages <- Ages %>%
  rowwise() %>%
  mutate(Timepoint_Ages = list(c(baseline_year_1_arm_1, 
                                 `1_year_follow_up_y_arm_1`, 
                                 `2_year_follow_up_y_arm_1`, 
                                 `3_year_follow_up_y_arm_1`, 
                                 `4_year_follow_up_y_arm_1`)))

# remove redundant columns
Ages <- Ages %>%
  select(src_subject_id, Timepoint_Ages)

# Merge in age data
Internalizing_CBCL <- Internalizing_CBCL %>%
  left_join(Ages, by = "src_subject_id")
Externalizing_CBCL <- Externalizing_CBCL %>%
  left_join(Ages, by = "src_subject_id")

#########################
# IN THE BELOW CODE BLOCK
# Merge and Clean Data for export
#########################

# Make combined Slope/Difference column
Internalizing_CBCL <- Internalizing_CBCL %>%
  mutate(Int_Slope_or_Diff = case_when(
    !is.na(Int_CBCL_Slope) ~ Int_CBCL_Slope,
    !is.na(Int_CBCL_Raw_Difference) ~ Int_CBCL_Raw_Difference,
    is.na(Int_CBCL_Slope) & is.na(Int_CBCL_Raw_Difference) ~ NA))
Externalizing_CBCL <- Externalizing_CBCL %>%
  mutate(Ext_Slope_or_Diff = case_when(
    !is.na(Ext_CBCL_Slope) ~ Ext_CBCL_Slope,
    !is.na(Ext_CBCL_Raw_Difference) ~ Ext_CBCL_Raw_Difference,
    is.na(Ext_CBCL_Slope) & is.na(Ext_CBCL_Raw_Difference) ~ NA))

# Organize and keep useful columns
Internalizing_CBCL <- Internalizing_CBCL %>%
  select(src_subject_id, Timepoint_Ages,
         Int_CBCL_Values, Timepoint_Count, Int_Slope_or_Diff, Int_CBCL_Slope, Int_CBCL_Raw_Difference, Int_TimepointGapY1N0, Int_Days_Between,
         baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)
Externalizing_CBCL <- Externalizing_CBCL %>%
  select(src_subject_id, Timepoint_Ages,
         Ext_CBCL_Values, Timepoint_Count, Ext_Slope_or_Diff, Ext_CBCL_Slope, Ext_CBCL_Raw_Difference, Ext_TimepointGapY1N0, Ext_Days_Between,
         baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)

# Convert list vector formatting so they can be written into csvs
Internalizing_CBCL <- Internalizing_CBCL %>%
  rowwise() %>%
  mutate(Timepoint_Ages = paste(Timepoint_Ages, collapse = ", "),
         Int_CBCL_Values = paste(Int_CBCL_Values, collapse = ", "))
Externalizing_CBCL <- Externalizing_CBCL %>%
  rowwise() %>%
  mutate(Timepoint_Ages = paste(Timepoint_Ages, collapse = ", "),
         Ext_CBCL_Values = paste(Ext_CBCL_Values, collapse = ", "))

#########################
# IN THE BELOW CODE BLOCK
# Write to CSV
#########################

write_csv(Internalizing_CBCL, paste0(derivative_data, 'Int_CBCL_SlopeDiff_9_11_25.csv'))
write_csv(Externalizing_CBCL, paste0(derivative_data, 'Ext_CBCL_SlopeDiff_9_11_25.csv'))
