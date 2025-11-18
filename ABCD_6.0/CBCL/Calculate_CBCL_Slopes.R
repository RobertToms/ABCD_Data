##########################
# Calculate_CBCL_Slopes.R
# Author: Robert Toms 
# 5.0 -> 6.0 Update: Emma Lambert
# Date: 09/16/2025
##########################

# The purpose of this code is to calculate changes/slopes in CBCL Internalizing and Externalizing.

library(tidyverse)
library(lubridate)

sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/6.0/"
Ages <- read_tsv(paste0(sourcepath, 'abcd_general/ab_g_dyn.tsv'))
CBCL <- read_tsv(paste0(sourcepath, 'Mental_Health/mh_p_cbcl.tsv'))


#########################
# IN THE BELOW CODE BLOCK
# Organize Data
#########################

# keep only relevant columns
CBCL <- CBCL %>%
  select(participant_id, session_id,
         mh_p_cbcl__synd__int_sum,  # Internal CBCL Syndrome Scale (sum)
         mh_p_cbcl__synd__int_tscore,  # Internal CBCL Syndrome Scale (t-score)
         mh_p_cbcl__synd__int_nm, # Internal CBCL Syndrome Scale (number of missing values)
         mh_p_cbcl__synd__ext_sum,  # External CBCL Syndrome Scale (sum)
         mh_p_cbcl__synd__ext_tscore,  # External CBCL Syndrome Scale (t-score)
         mh_p_cbcl__synd__ext_nm) # External CBCL Syndrome Scale (number of missing values)
# Missing values no longer exist in 6.0 
Ages <- Ages %>%
  select(participant_id, session_id, ab_g_dyn__visit_age, ab_g_dyn__visit_dtt)

# Split CBCL Internalizing and Externalizing
# Raw scores will be used, to maintain consistency with existing ABCD literature
Internalizing_CBCL <- CBCL %>%
  select(participant_id, session_id, mh_p_cbcl__synd__int_sum)

Externalizing_CBCL <- CBCL %>%
  select(participant_id, session_id, mh_p_cbcl__synd__ext_sum)

# Pivot column of interest wider
Internalizing_CBCL <- Internalizing_CBCL %>%
  pivot_wider(names_from = session_id, values_from = mh_p_cbcl__synd__int_sum)

Externalizing_CBCL <- Externalizing_CBCL %>%
  pivot_wider(names_from = session_id, values_from = mh_p_cbcl__synd__ext_sum)

## Split and pivot Interview Ages and Dates
# Split
Dates <- Ages %>%
  mutate(interview_date = as.Date(ab_g_dyn__visit_dtt)) %>%
  select(participant_id, session_id, interview_date)

# Pivot
Dates <- Dates %>%
  pivot_wider(names_from = session_id, values_from = interview_date)

## Clean and Clear Ages and Dates
# rename
Dates <- Dates %>%
  rename(Baseline_Date = `ses-00A`, 
         Year1_Date = `ses-01A`, 
         Year2_Date = `ses-02A`, 
         Year3_Date = `ses-03A`, 
         Year4_Date = `ses-04A`,
         Year5_Date = `ses-05A`,
         Year6_Date = `ses-06A`)

#########################
# IN THE BELOW CODE BLOCK
# Prep Data for Calculation
#########################

# Make a vector of CBCL Values, count number of valid timepoints per subject
Internalizing_CBCL <- Internalizing_CBCL %>%
  group_by(participant_id) %>%
  mutate(Int_CBCL_Values = list(c(`ses-00A`, 
                                  `ses-01A`, 
                                  `ses-02A`, 
                                  `ses-03A`, 
                                  `ses-04A`,
                                  `ses-05A`,
                                  `ses-06A`))) %>%
  mutate(Timepoint_Count = rowSums(!is.na(across(c(`ses-00A`, 
                                                   `ses-01A`, 
                                                   `ses-02A`, 
                                                   `ses-03A`, 
                                                   `ses-04A`,
                                                   `ses-05A`,
                                                   `ses-06A`)))))
Externalizing_CBCL <- Externalizing_CBCL %>%
  group_by(participant_id) %>%
  mutate(Ext_CBCL_Values = list(c(`ses-00A`, 
                                  `ses-01A`, 
                                  `ses-02A`, 
                                  `ses-03A`, 
                                  `ses-04A`,
                                  `ses-05A`,
                                  `ses-06A`))) %>%
  mutate(Timepoint_Count = rowSums(!is.na(across(c(`ses-00A`, 
                                                   `ses-01A`, 
                                                   `ses-02A`, 
                                                   `ses-03A`, 
                                                   `ses-04A`,
                                                   `ses-05A`,
                                                  `ses-06A` )))))

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
  mutate(Int_CBCL_Raw_Difference = if_else(!is.na(`ses-00A`) & !is.na(`ses-01A`),
                                           `ses-01A` - `ses-00A`, Int_CBCL_Raw_Difference)) %>%
  mutate(Int_CBCL_Raw_Difference = if_else(!is.na(`ses-02A`) & !is.na(`ses-01A`),
                                           `ses-02A` - `ses-01A`, Int_CBCL_Raw_Difference)) %>%
  mutate(Int_CBCL_Raw_Difference = if_else(!is.na(`ses-02A`) & !is.na(`ses-03A`),
                                           `ses-03A` - `ses-02A`, Int_CBCL_Raw_Difference)) %>%
  mutate(Int_CBCL_Raw_Difference = if_else(!is.na(`ses-03A`) & !is.na(`ses-04A`),
                                           `ses-04A` - `ses-03A`, Int_CBCL_Raw_Difference)) %>%
  mutate(Int_CBCL_Raw_Difference = if_else(!is.na(`ses-04A`) & !is.na(`ses-05A`),
                                           `ses-05A` - `ses-04A`, Int_CBCL_Raw_Difference)) %>%
  mutate(Int_CBCL_Raw_Difference = if_else(!is.na(`ses-05A`) & !is.na(`ses-06A`),
                                           `ses-06A` - `ses-05A`, Int_CBCL_Raw_Difference)) %>%
  arrange(Int_CBCL_Raw_Difference)

Ext_2TP <- Ext_2TP %>%
  mutate(Ext_CBCL_Raw_Difference = if_else(!is.na(`ses-00A`) & !is.na(`ses-01A`),
                                           `ses-01A` - `ses-00A`, Ext_CBCL_Raw_Difference)) %>%
  mutate(Ext_CBCL_Raw_Difference = if_else(!is.na(`ses-02A`) & !is.na(`ses-01A`),
                                           `ses-02A` - `ses-01A`, Ext_CBCL_Raw_Difference)) %>%
  mutate(Ext_CBCL_Raw_Difference = if_else(!is.na(`ses-02A`) & !is.na(`ses-03A`),
                                           `ses-03A` - `ses-02A`, Ext_CBCL_Raw_Difference)) %>%
  mutate(Ext_CBCL_Raw_Difference = if_else(!is.na(`ses-03A`) & !is.na(`ses-04A`),
                                           `ses-04A` - `ses-03A`, Ext_CBCL_Raw_Difference)) %>%
  mutate(Ext_CBCL_Raw_Difference = if_else(!is.na(`ses-04A`) & !is.na(`ses-05A`),
                                           `ses-05A` - `ses-04A`, Ext_CBCL_Raw_Difference)) %>%
  mutate(Ext_CBCL_Raw_Difference = if_else(!is.na(`ses-05A`) & !is.na(`ses-06A`),
                                           `ses-06A` - `ses-05A`, Ext_CBCL_Raw_Difference)) %>%
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
  left_join(Dates, by = 'participant_id')
Ext_2TP <- Ext_2TP %>%
  left_join(Dates, by = 'participant_id')

## Calculate days between reports
# make vector of dates
Int_2TP <- Int_2TP %>%
  mutate(interview_days = list(c(Baseline_Date, `Year1_Date`, `Year2_Date`, `Year3_Date`, `Year4_Date`, `Year5_Date`, `Year6_Date`)))
Ext_2TP <- Ext_2TP %>%
  mutate(interview_days = list(c(Baseline_Date, `Year1_Date`, `Year2_Date`, `Year3_Date`, `Year4_Date`, `Year5_Date`, `Year6_Date`)))
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
  pivot_longer(c(`ses-00A`, 
                 `ses-01A`, 
                 `ses-02A`, 
                 `ses-03A`, 
                 `ses-04A`,
                 `ses-05A`,
                 `ses-06A`), names_to = "session_id", values_to = "Int_CBCL_Value")
Ext_3PlusTP <- Ext_3PlusTP %>%
  pivot_longer(c(`ses-00A`, 
                 `ses-01A`, 
                 `ses-02A`, 
                 `ses-03A`, 
                 `ses-04A`,
                 `ses-05A`,
                 `ses-06A`), names_to = "session_id", values_to = "Ext_CBCL_Value")

## Merge in Ages
Int_3PlusTP <- Int_3PlusTP %>%
  left_join(Ages, by = 'participant_id')
Ext_3PlusTP <- Ext_3PlusTP %>%
  left_join(Ages, by = 'participant_id')
# Keep matching columns
Int_3PlusTP <- Int_3PlusTP %>%
  filter(session_id.x == session_id.y) %>%
  rename(session_id = session_id.x) %>%
  select(-session_id.y)
Ext_3PlusTP <- Ext_3PlusTP %>%
  filter(session_id.x == session_id.y) %>%
  rename(session_id = session_id.x) %>%
  select(-session_id.y)

# Remove NA CBCL Value rows
Int_3PlusTP <- Int_3PlusTP %>%
  # Before: 67017
  filter(!is.na(Int_CBCL_Value))
  # After: 66197
Ext_3PlusTP <- Ext_3PlusTP %>%
  # Before: 67017
  filter(!is.na(Ext_CBCL_Value))
  # After: 66197

# Calculate slopes
Int_3PlusTP_Slopes <- Int_3PlusTP %>%
  group_by(participant_id) %>%
  group_modify(~ {model <- lm(Int_CBCL_Value ~ ab_g_dyn__visit_age, data = .)
  tibble(Int_CBCL_Slope = coef(model)[2]) })
Ext_3PlusTP_Slopes <- Ext_3PlusTP %>%
  group_by(participant_id) %>%
  group_modify(~ {model <- lm(Ext_CBCL_Value ~ ab_g_dyn__visit_age, data = .)
  tibble(Ext_CBCL_Slope = coef(model)[2]) })


## Merge Difference/Slope Data back in
# Clean 2TPs
Int_2TP <- Int_2TP %>%
  select(participant_id, Int_CBCL_Raw_Difference, Int_TimepointGapY1N0, Int_Days_Between)
Ext_2TP <- Ext_2TP %>%
  select(participant_id, Ext_CBCL_Raw_Difference, Ext_TimepointGapY1N0, Ext_Days_Between)

# 3+ TPs
Internalizing_CBCL <- Internalizing_CBCL %>%
  left_join(Int_3PlusTP_Slopes, by = 'participant_id')
Externalizing_CBCL <- Externalizing_CBCL %>%
  left_join(Ext_3PlusTP_Slopes, by = 'participant_id')
# 2 TPs
Internalizing_CBCL <- Internalizing_CBCL %>%
  left_join(Int_2TP, by = 'participant_id')
Externalizing_CBCL <- Externalizing_CBCL %>%
  left_join(Ext_2TP, by = 'participant_id')

#########################
# IN THE BELOW CODE BLOCK
# Add Age vector for reference
#########################

## make vector of ages
# Pivot
Ages <- Ages %>%
  pivot_wider(names_from = session_id, values_from = ab_g_dyn__visit_age)
# keep useful columns
Ages <- Ages %>%
  select(participant_id, `ses-00A`, `ses-01A`, 
         `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)

# Fill and collapse each subject into a single row
Ages <- Ages %>%
  group_by(participant_id) %>%
  fill(c(`ses-00A`, `ses-01A`, 
         `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`), .direction = "downup") %>%
  unique()

# make vector
Ages <- Ages %>%
  rowwise() %>%
  mutate(Timepoint_Ages = list(c(`ses-00A`, 
                                 `ses-01A`, 
                                 `ses-02A`, 
                                 `ses-03A`, 
                                 `ses-04A`,
                                 `ses-05A`,
                                 `ses-06A`)))

# remove redundant columns
Ages <- Ages %>%
  select(participant_id, Timepoint_Ages)

# Merge in age data
Internalizing_CBCL <- Internalizing_CBCL %>%
  left_join(Ages, by = "participant_id")
Externalizing_CBCL <- Externalizing_CBCL %>%
  left_join(Ages, by = "participant_id")

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
  select(participant_id, Timepoint_Ages,
         Int_CBCL_Values, Timepoint_Count, Int_Slope_or_Diff, Int_CBCL_Slope, Int_CBCL_Raw_Difference, Int_TimepointGapY1N0, Int_Days_Between,
         `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)
Externalizing_CBCL <- Externalizing_CBCL %>%
  select(participant_id, Timepoint_Ages,
         Ext_CBCL_Values, Timepoint_Count, Ext_Slope_or_Diff, Ext_CBCL_Slope, Ext_CBCL_Raw_Difference, Ext_TimepointGapY1N0, Ext_Days_Between,
         `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)

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

write_csv(Internalizing_CBCL, paste0(derivative_data, 'Int_CBCL_SlopeDiff_9_17_25.csv'))
write_csv(Externalizing_CBCL, paste0(derivative_data, 'Ext_CBCL_SlopeDiff_9_17_25.csv'))



