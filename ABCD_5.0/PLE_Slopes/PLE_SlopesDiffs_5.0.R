##########################
# PLE_SlopesDiffs_5.0.R
# Author: Robert Toms
# Date: 10/2/2025
##########################

# The purpose of this code is to calculate changes/slopes in PLEs, across the course of data collection.

sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"

library(tidyverse)
library(lubridate)

PLEs <- read_csv(paste0(sourcepath, '/mental-health/mh_y_pps.csv'))
Ages <- read_csv(paste0(sourcepath, '/abcd-general/abcd_y_lt.csv'))

#########################
# IN THE BELOW CODE BLOCK
# Clean and prep data for calculation
#########################

# Isolate useful elements and totals
Ages <- Ages %>%
  select(src_subject_id, eventname, interview_age, interview_date)

# keep only useful eventnames, convert ages to years, set missing ages and dates to 1 year apart, dates to mdy
Ages <- Ages %>%
  # keep only useful eventnames
  filter(eventname == 'baseline_year_1_arm_1' | 
           eventname == '1_year_follow_up_y_arm_1' |
           eventname == '2_year_follow_up_y_arm_1' | 
           eventname == '3_year_follow_up_y_arm_1' | 
           eventname == '4_year_follow_up_y_arm_1') %>%
  # convert ages to years
  mutate(interview_age = interview_age / 12) %>%
  # NA ages to 1 year apart -- 0, 1, 2, 3, 4
  mutate(interview_age = if_else(is.na(interview_age), case_when(
    eventname == "baseline_year_1_arm_1" ~ 0,
    eventname == "1_year_follow_up_y_arm_1" ~ 1,
    eventname == "2_year_follow_up_y_arm_1" ~ 2,
    eventname == "3_year_follow_up_y_arm_1" ~ 3,
    eventname == "4_year_follow_up_y_arm_1" ~ 4), interview_age)) %>%
  # NA interview dates 1 year apart
  mutate(interview_date = if_else(is.na(interview_date), case_when(
    eventname == "baseline_year_1_arm_1" ~ '12/31/2000',
    eventname == "1_year_follow_up_y_arm_1" ~ '12/31/2001',
    eventname == "2_year_follow_up_y_arm_1" ~ '12/31/2002',
    eventname == "3_year_follow_up_y_arm_1" ~ '12/31/2003',
    eventname == "4_year_follow_up_y_arm_1" ~ '12/31/2004'), interview_date)) %>%
  # dates to mdy so that can be subtracted
  mutate(interview_date = mdy(interview_date))

# Individual PLE items & summaries, for reference
PLEs <- PLEs %>%
  select(src_subject_id, eventname,
         prodromal_1_y,	# Did places that you know well, such as your bedroom, or other rooms in your home, your classroom or school yard, suddenly seem weird, strange or confusing to you; like not the real world?
         prodromal_2_y,	# Did you hear strange sounds that you never noticed before like banging, clicking, hissing, clapping, or ringing in your ears?
         prodromal_3_y,	# Do things that you see appear different from the way they usually do (brighter or duller, larger or smaller, or changed in some other way)?
         prodromal_4_y,	# Did you feel like you had special, unusual powers like you could make things happen by magic, or that you could magically know what was inside another person's mind, or magically know what was going to happen in the future when other people could not?
         prodromal_5_y,	# Did you feel that someone else, who is not you, has taken control over the private, personal, thoughts or ideas inside your head?
         prodromal_6_y,	# Did you suddenly find it hard to figure out how to say something quickly and easily so that other people would understand what you meant?
         prodromal_7_y,	# Did you ever feel very certain that you have very special abilities or magical talents that other people do not have?
         prodromal_8_y,	# Did you suddenly feel that you could not trust other people because they seemed to be watching you or talking about you in an unfriendly way?
         prodromal_9_y,	# Do you sometimes get strange feelings on or just beneath your skin, like bugs crawling?
         prodromal_10_y,	# Did you lose concentration because you noticed sounds in the distance that you usually don't hear?
         prodromal_11_y,	# Although you could not see anything or anyone, did you suddenly start to feel that an invisible energy, creature, or some person was around you?
         prodromal_12_y,	# Did you start to worry at times that your mind was trying to trick you or was not working right?
         prodromal_13_y,	# Did you feel that the world is not real, you are not real, or that you are dead?
         prodromal_14_y,	# Did you feel confused because something you experienced didn't seem real or it seemed imaginary to you?
         prodromal_15_y,	# Did you honestly believe in things that other people would say are unusual or weird?
         prodromal_16_y,	# Did you feel that parts of your body had suddenly changed or worked differently than before; like your legs had suddenly turned to something else or your nose could suddenly smell things you'd never actually smelled before?
         prodromal_17_y,	# Did you feel that sometimes your thoughts were so strong you could almost hear them, as if another person, NOT you, spoke them?
         prodromal_18_y,	# Did you feel that other people might want something bad to happen to you or that you could not trust other people?
         prodromal_19_y,	# Did you suddenly start to see unusual things that you never saw before like flashes, flames, blinding light, or shapes floating in front of you?
         prodromal_20_y,	# Have you seen things that other people can't see or don't seem to see?
         prodromal_21_y,	# Did you suddenly start to notice that people sometimes had a hard time understanding what you were saying, even though they used to understand you well?
         pps_y_ss_number, # Number of Yes responses
         pps_y_ss_severity_score, # Sum of how much each Yes response bothered the subject
         pps_ss_mean_severity) # Mean Endorsed Severity Score -- pps_y_ss_severity_score / pps_y_ss_number

# Isolate Just total PLE score
Just_Total_PLEs <- PLEs %>%
  select(src_subject_id, eventname, pps_y_ss_severity_score)

# Pivot Wide
Just_Total_PLEs <- Just_Total_PLEs %>%
  pivot_wider(names_from = eventname, values_from = c(pps_y_ss_severity_score))

# Make Vector of Values
Total_PLEs <- Just_Total_PLEs %>%
  group_by(src_subject_id) %>%
  mutate(PLE_Counts = list(c(baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, 
                             `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)))

# Count number of valid Timepoints per participant
Total_PLEs <- Total_PLEs %>%
  group_by(src_subject_id) %>%
  mutate(Timepoint_Count = rowSums(!is.na(across(c(baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, 
                                                   `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)))))

# Now that we have searchable vectors and number of timepoints (2 timepoints get differences, 3+ get slopes), 
# we want to merge in the Ages, so we can calculate slopes -- change in PLE score per year (age)

# merge in ages, reorder for clarity
EventWise_PLEs <- Total_PLEs %>%
  left_join(Ages, by = 'src_subject_id') %>%  
  select(src_subject_id, eventname, interview_age, everything())

# We now have eventname's as their own column, and as column names for each session's respective PLE score.
# We want a longform column of individual PLE scores, to extract slopes via a linear model, 
# and if there are any NA values in that column, the linear model function will throw an error.

#isolate only matching eventname row/column values, to populate the longform PLE score column
EventWise_PLEs <- EventWise_PLEs %>%
  mutate(PLE_Value = case_when(
    eventname == "baseline_year_1_arm_1" ~ baseline_year_1_arm_1,
    eventname == "1_year_follow_up_y_arm_1" ~ `1_year_follow_up_y_arm_1`,
    eventname == "2_year_follow_up_y_arm_1" ~ `2_year_follow_up_y_arm_1`,
    eventname == "3_year_follow_up_y_arm_1" ~ `3_year_follow_up_y_arm_1`,
    eventname == "4_year_follow_up_y_arm_1" ~ `4_year_follow_up_y_arm_1`))

# remove NA rows, keep only cols necessary for lm
EventWise_PLEs <- EventWise_PLEs %>%
  filter(!is.na(PLE_Value)) %>%
  select(src_subject_id, eventname, interview_age, PLE_Value, PLE_Counts, Timepoint_Count)

# Now we can split those who will get differences and those who will have slopes calculated.

# Isolate 3+ Timepoints to calculate slopes, and <3 for raw differences
ThreePlusTimepoints <- EventWise_PLEs %>%
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
  mutate(PLE_Raw_Difference = if_else(!is.na(baseline_year_1_arm_1) & !is.na(`1_year_follow_up_y_arm_1`),
                                      `1_year_follow_up_y_arm_1` - baseline_year_1_arm_1, PLE_Raw_Difference)) %>%
  mutate(PLE_Raw_Difference = if_else(!is.na(`1_year_follow_up_y_arm_1`) & !is.na(`2_year_follow_up_y_arm_1`),
                                      `2_year_follow_up_y_arm_1` - `1_year_follow_up_y_arm_1`, PLE_Raw_Difference)) %>%
  mutate(PLE_Raw_Difference = if_else(!is.na(`2_year_follow_up_y_arm_1`) & !is.na(`3_year_follow_up_y_arm_1`),
                                      `3_year_follow_up_y_arm_1` - `2_year_follow_up_y_arm_1`, PLE_Raw_Difference)) %>%
  mutate(PLE_Raw_Difference = if_else(!is.na(`3_year_follow_up_y_arm_1`) & !is.na(`4_year_follow_up_y_arm_1`),
                                      `4_year_follow_up_y_arm_1` - `3_year_follow_up_y_arm_1`, PLE_Raw_Difference))%>%
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
  select(src_subject_id, eventname, interview_date) %>%
  pivot_wider(names_from = eventname, values_from = interview_date) %>%
  rename(baseline = baseline_year_1_arm_1,
         Year1 = `1_year_follow_up_y_arm_1`,
         Year2 = `2_year_follow_up_y_arm_1`, 
         Year3 = `3_year_follow_up_y_arm_1`, 
         Year4 = `4_year_follow_up_y_arm_1`) %>%
  select(src_subject_id, baseline, Year1, Year2, Year3, Year4) %>%
  mutate(baseline =as_date(baseline),
         `Year1` = as_date(`Year1`), 
         `Year2` = as_date(`Year2`), 
         `Year3` = as_date(`Year3`), 
         `Year4` = as_date(`Year4`))

TwoTP <- TwoTP %>%
  left_join(Dates, by = 'src_subject_id')

# Calculate days between reports
TwoTP <- TwoTP %>%
  mutate(interview_days = list(c(baseline, 
                                 `Year1`, 
                                 `Year2`, 
                                 `Year3`, 
                                 `Year4`)))

TwoTP <- TwoTP %>%
  mutate(Days_Between = interview_days[LastTP] - interview_days[FirstTP])

## 3+ Timepoints
# Instantiate Slopes Column
ThreePlusTimepoints <- ThreePlusTimepoints %>%
  mutate(PLE_Slope = NA)

# Calculate slopes
ThreePlusSlopes <- ThreePlusTimepoints %>%
  group_by(src_subject_id) %>%
  group_modify(~ {model <- lm(PLE_Value ~ interview_age, data = .)
  tibble(PLE_Slope = coef(model)[2]) })


## Merge Difference/Slope Data back in
# Clean 2TPs
TwoTP <- TwoTP %>%
  select(src_subject_id, PLE_Raw_Difference, TimepointGapY1N0, Days_Between)

# 3+ TPs
Total_PLEs <- Total_PLEs %>%
  left_join(ThreePlusSlopes, by = 'src_subject_id')
# 2 TPs
Total_PLEs <- Total_PLEs %>%
  left_join(TwoTP, by = c('src_subject_id'))

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
  select(-interview_date)

# pivot wider so we cna make the chronological list vector
Ages <- Ages %>%
  pivot_wider(names_from = eventname, values_from = interview_age)

# make chronological list vector
Ages <- Ages %>%
  group_by(src_subject_id) %>%
  mutate(Ages_List = list(c(baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`,
                            `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`,
                            `4_year_follow_up_y_arm_1`)))

# remove superfluous columns
Age_list <- Ages %>%
  select(src_subject_id, Ages_List)

# Merge into PLE data
Total_PLEs <- Total_PLEs %>%
  left_join(Age_list, by = c("src_subject_id"))

# Reorder data for clarity
Total_PLEs <- Total_PLEs %>%
  select(src_subject_id, Ages_List, PLE_Counts, Timepoint_Count, PLE_SlopeOrDiff, PLE_Slope, PLE_Raw_Difference, TimepointGapY1N0, Days_Between,
         baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)

# convert list vectors to character type so they can get written into a csv
Total_PLEs <- Total_PLEs %>%
  rowwise() %>%
  mutate(Ages_List = paste(Ages_List, collapse = ", "),
         PLE_Counts = paste(PLE_Counts, collapse = ", "))

#########################
# IN THE BELOW CODE BLOCK
# Write to csv
#########################

write_csv(Total_PLEs, paste0(derivative_data, 'PLE_SlopesDiffs_10_2_25.csv'))
