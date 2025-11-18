##########################
# PLE_SlopesDiffs_6.0.R
# Author: Robert Toms
# 5.0 -> 6.0 Update: Dylan Seay 
# Date: 09/26/2025
##########################

# The purpose of this code is to calculate changes/slopes in PLEs, across the course of data collection.

sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/6.0/"

library(tidyverse)
library(lubridate)

PLEs <- read_tsv(paste0(sourcepath, '/Mental_Health/mh_y_pps.tsv'))
Ages <- read_tsv(paste0(sourcepath, '/abcd_general/ab_g_dyn.tsv'))

#########################
# IN THE BELOW CODE BLOCK
# Clean and prep data for calculation
#########################

# Isolate useful elements and totals
Ages <- Ages %>%
  select(participant_id, session_id, ab_g_dyn__visit_age, ab_g_dyn__visit_dtt)

PLEs <- PLEs %>%
  select(participant_id, session_id,
         mh_y_pps_001,	# Did places that you know well, such as your bedroom, or other rooms in your home, your classroom or school yard, suddenly seem weird, strange or confusing to you; like not the real world?
         mh_y_pps_002,	# Did you hear strange sounds that you never noticed before like banging, clicking, hissing, clapping, or ringing in your ears?
         mh_y_pps_003,	# Do things that you see appear different from the way they usually do (brighter or duller, larger or smaller, or changed in some other way)?
         mh_y_pps_004,	# Did you feel like you had special, unusual powers like you could make things happen by magic, or that you could magically know what was inside another person's mind, or magically know what was going to happen in the future when other people could not?
         mh_y_pps_005,	# Did you feel that someone else, who is not you, has taken control over the private, personal, thoughts or ideas inside your head?
         mh_y_pps_006,	# Did you suddenly find it hard to figure out how to say something quickly and easily so that other people would understand what you meant?
         mh_y_pps_007,	# Did you ever feel very certain that you have very special abilities or magical talents that other people do not have?
         mh_y_pps_008,	# Did you suddenly feel that you could not trust other people because they seemed to be watching you or talking about you in an unfriendly way?
         mh_y_pps_009,	# Do you sometimes get strange feelings on or just beneath your skin, like bugs crawling?
         mh_y_pps_010,	# Did you lose concentration because you noticed sounds in the distance that you usually don't hear?
         mh_y_pps_011,	# Although you could not see anything or anyone, did you suddenly start to feel that an invisible energy, creature, or some person was around you?
         mh_y_pps_012,	# Did you start to worry at times that your mind was trying to trick you or was not working right?
         mh_y_pps_013,	# Did you feel that the world is not real, you are not real, or that you are dead?
         mh_y_pps_014,	# Did you feel confused because something you experienced didn't seem real or it seemed imaginary to you?
         mh_y_pps_015,	# Did you honestly believe in things that other people would say are unusual or weird?
         mh_y_pps_016,	# Did you feel that parts of your body had suddenly changed or worked differently than before; like your legs had suddenly turned to something else or your nose could suddenly smell things you'd never actually smelled before?
         mh_y_pps_017,	# Did you feel that sometimes your thoughts were so strong you could almost hear them, as if another person, NOT you, spoke them?
         mh_y_pps_018,	# Did you feel that other people might want something bad to happen to you or that you could not trust other people?
         mh_y_pps_019,	# Did you suddenly start to see unusual things that you never saw before like flashes, flames, blinding light, or shapes floating in front of you?
         mh_y_pps_020,	# Have you seen things that other people can't see or don't seem to see?
         mh_y_pps_021,	# Did you suddenly start to notice that people sometimes had a hard time understanding what you were saying, even though they used to understand you well?
         mh_y_pps_count, # Number of Yes responses
         mh_y_pps__severity_score, # Sum of how much each Yes response bothered the subject
         mh_y_pps__severity_mean) # Mean Endorsed Severity Score -- mh_y_pps__severity_score / mh_y_pps_count

# Isolate Just total PLE score
Just_Total_PLEs <- PLEs %>%
  select(participant_id, session_id, mh_y_pps__severity_score)

# Pivot Wide
Just_Total_PLEs <- Just_Total_PLEs %>%
  pivot_wider(names_from = session_id, values_from = c(mh_y_pps__severity_score))

# Make Vector of Values
Total_PLEs <- Just_Total_PLEs %>%
  group_by(participant_id) %>%
  mutate(PLE_Counts = list(c(`ses-00A`, 
                             `ses-01A`, 
                             `ses-02A`, 
                             `ses-03A`, 
                             `ses-04A`,
                             `ses-05A`,
                             `ses-06A`)))

# Count number of valid Timepoints per participant
Total_PLEs <- Total_PLEs %>%
  group_by(participant_id) %>%
  mutate(Timepoint_Count = rowSums(!is.na(across(c(`ses-00A`, 
                                                   `ses-01A`, 
                                                   `ses-02A`, 
                                                   `ses-03A`, 
                                                   `ses-04A`,
                                                   `ses-05A`,
                                                   `ses-06A`)))))

# Now that we have searchable vectors and number of timepoints (2 timepoints get differences, 3+ get slopes), 
# we want to merge in the Ages, so we can calculate slopes -- change in PLE score per year (age)

# merge in ages, reorder for clarity
SessionWise_PLEs <- Total_PLEs %>%
  left_join(Ages, by = 'participant_id') %>%
  select(participant_id, session_id, ab_g_dyn__visit_age, everything())

# We now have session_id's as their own column, and as column names for each session's respective PLE score.
# We want a longform column of individual PLE scores, to extract slopes via a linear model, 
# and if there are any NA values in that column, the linear model function will throw an error.

#isolate only matching session_id row/column values, to populate the longform PLE score column
SessionWise_PLEs <- SessionWise_PLEs %>%
  mutate(PLE_Value = case_when(
    session_id == "ses-00A" ~ `ses-00A`,
    session_id == "ses-01A" ~ `ses-01A`,
    session_id == "ses-02A" ~ `ses-02A`,
    session_id == "ses-03A" ~ `ses-03A`,
    session_id == "ses-04A" ~ `ses-04A`,
    session_id == "ses-05A" ~ `ses-05A`,
    session_id == "ses-06A" ~ `ses-06A`))

# remove NA rows, keep only cols necessary for lm
SessionWise_PLEs <- SessionWise_PLEs %>%
  filter(!is.na(PLE_Value)) %>%
  select(participant_id, session_id, ab_g_dyn__visit_age, PLE_Value, PLE_Counts, Timepoint_Count)

# Now we can split those who will get differences and those who will have slopes calculated.

# Isolate 3+ Timepoints to calculate slopes, and <3 for raw differences
ThreePlusTimepoints <- SessionWise_PLEs %>%
  filter(Timepoint_Count >= 3)

TwoTP <- Total_PLEs %>%
  filter(Timepoint_Count == 2)

#########################
# IN THE BELOW CODE BLOCK
# Calculate Differences/Slopes
#########################

## 2 timepoints
# Instantiate Difference Column
TwoTP <- TwoTP %>%
  mutate(PLE_Raw_Difference = NA)

# Calculate Raw Differences for consecutive timepoints
TwoTP <- TwoTP %>%
  mutate(PLE_Raw_Difference = if_else(!is.na(`ses-00A`) & !is.na(`ses-01A`),
                                      `ses-01A` - `ses-00A`, PLE_Raw_Difference)) %>%
  mutate(PLE_Raw_Difference = if_else(!is.na(`ses-01A`) & !is.na(`ses-02A`),
                                      `ses-02A` - `ses-01A`, PLE_Raw_Difference)) %>%
  mutate(PLE_Raw_Difference = if_else(!is.na(`ses-02A`) & !is.na(`ses-03A`),
                                      `ses-03A` - `ses-02A`, PLE_Raw_Difference)) %>%
  mutate(PLE_Raw_Difference = if_else(!is.na(`ses-03A`) & !is.na(`ses-04A`),
                                      `ses-04A` - `ses-03A`, PLE_Raw_Difference)) %>%
  mutate(PLE_Raw_Difference = if_else(!is.na(`ses-04A`) & !is.na(`ses-05A`),
                                      `ses-05A` - `ses-04A`, PLE_Raw_Difference)) %>%
  mutate(PLE_Raw_Difference = if_else(!is.na(`ses-05A`) & !is.na(`ses-06A`),
                                      `ses-06A` - `ses-05A`, PLE_Raw_Difference)) %>%
  arrange(PLE_Raw_Difference)

# Flag timepoint gaps
TwoTP <- TwoTP %>%
  mutate(TimepointGapY1N0 = if_else(is.na(PLE_Raw_Difference), 1, 0))

### Calculate raw differences for those with 2 timepoint gaps
# Find First and Last Timepoint
TwoTP <- TwoTP %>%
  rowwise() %>%
  mutate(FirstTP = first(which(!is.na(PLE_Counts))),
         LastTP = last(which(!is.na(PLE_Counts))))

# Calculate differences
TwoTP <- TwoTP %>%
  mutate(PLE_Raw_Difference = if_else(TimepointGapY1N0 == 1, 
                                      PLE_Counts[LastTP] - PLE_Counts[FirstTP],
                                      PLE_Raw_Difference))

## Calculate number of days between reports
# merge in interview dates
Dates <- Ages %>%
  select(participant_id, session_id, ab_g_dyn__visit_dtt) %>%
  pivot_wider(names_from = session_id, values_from = ab_g_dyn__visit_dtt) %>%
  rename(baseline = `ses-00A`,
         Year1 = `ses-01A`,
         Year2 = `ses-02A`, 
         Year3 = `ses-03A`, 
         Year4 = `ses-04A`,
         Year5 = `ses-05A`,
         Year6 = `ses-06A`) %>%
  select(participant_id, baseline, Year1, Year2, Year3, Year4, Year5, Year6) %>%
  mutate(baseline =as_date(baseline),
         `Year1` = as_date(`Year1`), 
         `Year2` = as_date(`Year2`), 
         `Year3` = as_date(`Year3`), 
         `Year4` = as_date(`Year4`),
         `Year5` = as_date(`Year5`),
         `Year6` = as_date(`Year6`))

TwoTP <- TwoTP %>%
  left_join(Dates, by = 'participant_id')

# Calculate days between reports
TwoTP <- TwoTP %>%
  mutate(interview_days = list(c(baseline, 
                                 `Year1`, 
                                 `Year2`, 
                                 `Year3`, 
                                 `Year4`,
                                 `Year5`,
                                 `Year6`)))

TwoTP <- TwoTP %>%
  mutate(Days_Between = interview_days[LastTP] - interview_days[FirstTP])

## 3+ Timepoints
# Instantiate Slopes Column
ThreePlusTimepoints <- ThreePlusTimepoints %>%
  mutate(PLE_Slope = NA)

# Calculate slopes
ThreePlusSlopes <- ThreePlusTimepoints %>%
  group_by(participant_id) %>%
  group_modify(~ {model <- lm(PLE_Value ~ ab_g_dyn__visit_age, data = .)
  tibble(PLE_Slope = coef(model)[2]) })


## Merge Difference/Slope Data back in
# Clean 2TPs
TwoTP <- TwoTP %>%
  select(participant_id, PLE_Raw_Difference, TimepointGapY1N0, Days_Between)

# 3+ TPs
Total_PLEs <- Total_PLEs %>%
  left_join(ThreePlusSlopes, by = 'participant_id')
# 2 TPs
Total_PLEs <- Total_PLEs %>%
  left_join(TwoTP, by = c('participant_id'))

# Make combined Slopes/Differences column
Total_PLEs <- Total_PLEs %>%
  mutate(PLE_SlopeOrDiff = if_else(!is.na(PLE_Slope), PLE_Slope, PLE_Raw_Difference))

#########################
# IN THE BELOW CODE BLOCK
# Add explanatory flags and Age vector for reference,
# Organize & Format Data for CSV Export
#########################

## make Age Vector
Ages <- Ages %>%
  select(-ab_g_dyn__visit_dtt)

# pivot wider so we cna make the chronological list vector
Ages <- Ages %>%
  pivot_wider(names_from = session_id, values_from = ab_g_dyn__visit_age)

# make chronological list vector
Ages <- Ages %>%
  group_by(participant_id) %>%
  mutate(Ages_List = list(c(`ses-00A`, `ses-01A`,
                            `ses-02A`, `ses-03A`,
                            `ses-04A`, `ses-05A`,
                            `ses-06A`)))

# remove superfluous columns
Age_list <- Ages %>%
  select(participant_id, Ages_List)

# Merge into PLE data
Total_PLEs <- Total_PLEs %>%
  left_join(Age_list, by = c("participant_id"))

# Reorder data for clarity
Total_PLEs <- Total_PLEs %>%
  select(participant_id, Ages_List, PLE_Counts, Timepoint_Count, PLE_SlopeOrDiff, PLE_Slope, PLE_Raw_Difference, TimepointGapY1N0, Days_Between,
         `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)

# convert list vectors to character type so they can get written into a csv
Total_PLEs <- Total_PLEs %>%
  rowwise() %>%
  mutate(Ages_List = paste(Ages_List, collapse = ", "),
         PLE_Counts = paste(PLE_Counts, collapse = ", "))

#########################
# IN THE BELOW CODE BLOCK
# Write to csv
#########################

write_csv(Total_PLEs, paste0(derivative_data, 'PLE_SlopesDiffs_9_26_25.csv'))
