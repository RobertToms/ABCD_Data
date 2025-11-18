#########################
# Model_Pubertal_Cycle_5.0.R
# Author: Robert Toms
# 6.0 -> 5.0 Update: Dylan Seay
# Date: 10/7/2025
#########################

library(tidyverse)
library(lubridate)

sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"

# import data
pds_p <- read_csv(paste0(sourcepath, '/physical-health/ph_p_pds.csv'))
pds_y <- read_csv(paste0(sourcepath, '/physical-health/ph_y_pds.csv'))
sex <- read_csv(paste0(sourcepath, '/abcd-general/abcd_p_demo.csv'))
interview_date <- read_csv(paste0(sourcepath, '/abcd-general/abcd_y_lt.csv'))

##########################
# IN THE BELOW CODE BLOCK
# Clean and Merge Data
##########################

# Keep only relevant columns
pds_p <- pds_p %>%
  select(src_subject_id, 
         eventname, 
         pds_p_ss_female_category,   # Approximate Tanner Stages; Pre-Pubertal = 1, Early Pubertal = 2, Mid Pubertal = 3, Late Pubertal = 4, Post-Pubertal = 5
         menstrualcycle1_p, # What was the date of the first day of your child's last period?
         menstrualcycle2_p, # On average, how many days are there between the first day of their and the first day of their next period? (eg., 30 days)
         menstrualcycle3_p, # Is your child's menstrual cycle regular? Yes = 1, No = 0, Decline to Answer = 777, Don't Know = 999
         menstrualcycle4_p) # Is your child currently using hormonal birth control (eg. the pill, hormone patch, hormone injection)? Yes = 1, No = 0, Decline to Answer = 777, Don't Know = 999
pds_y <- pds_y %>%
  select(src_subject_id, 
         eventname, 
         pds_y_ss_female_category,   # Approximate Tanner Stages; Pre-Pubertal = 1, Early Pubertal = 2, Mid Pubertal = 3, Late Pubertal = 4, Post-Pubertal = 5
         menstrualcycle1_y, # What was the date of the first day of your last period?
         menstrualcycle2_y, # On average, how many days are there between the first day of your period and the first day of your next period? For example, if the first day of bleeding is typically at the beginning of each month, the interval between periods is 30 days.
         menstrualcycle3_y, # Is your menstrual cycle regular? Yes = 1, No = 0, Don't Know = 999
         menstrualcycle4_y) # Are you currently using hormonal birth control (eg. the pill, hormone patch, hormone injection)? Yes = 1, No = 0, Don't Know = 999

interview_date <- interview_date %>%
  select(src_subject_id, eventname, interview_date)

sex <- sex %>%
  select(src_subject_id, demo_sex_v2)

# remove males
pds_p <- pds_p %>%
  left_join(sex, by = 'src_subject_id') %>%
  filter(demo_sex_v2 == 2) %>% # before: 49,151, after: 23,417
  select(-demo_sex_v2)
pds_y <- pds_y %>%
  left_join(sex, by = 'src_subject_id') %>%
  filter(demo_sex_v2 == 2) %>% # before: 49,151, after: 23,417
  select(-demo_sex_v2)

# rename for human readability
pds_p <- pds_p %>%
  rename(tanner_stage = pds_p_ss_female_category, 
         firstdatelastcycle = menstrualcycle1_p, 
         cyclelength = menstrualcycle2_p, 
         cycleregular = menstrualcycle3_p, 
         childbirthcontrol = menstrualcycle4_p)
pds_y <- pds_y %>%
  rename(tanner_stage = pds_y_ss_female_category, 
         firstdatelastcycle = menstrualcycle1_y, 
         cyclelength = menstrualcycle2_y, 
         cycleregular = menstrualcycle3_y, 
         childbirthcontrol = menstrualcycle4_y)

# correct data types for character to their appropriate type for usability
pds_p <- pds_p %>%
  mutate(tanner_stage = as.numeric(tanner_stage),
         firstdatelastcycle = ymd(firstdatelastcycle),
         cyclelength = as.numeric(cyclelength),
         cycleregular = as.numeric(cycleregular),
         childbirthcontrol = as.numeric(childbirthcontrol))
pds_y <- pds_y %>%
  mutate(tanner_stage = as.numeric(tanner_stage),
         firstdatelastcycle = ymd(firstdatelastcycle),
         cyclelength = as.numeric(cyclelength),
         cycleregular = as.numeric(cycleregular),
         childbirthcontrol = as.numeric(childbirthcontrol))

# Merge in interview dates
pds_p <- pds_p %>%
  left_join(interview_date, by = c('src_subject_id', 'eventname'))
pds_y <- pds_y %>%
  left_join(interview_date, by = c('src_subject_id', 'eventname'))

##########################
# IN THE BELOW CODE BLOCK
# Remove Clinically Abnormal outliers
##########################

# check spread of data
table(pds_p$cyclelength)
table(pds_y$cyclelength)
mean(pds_p$cyclelength, na.rm = TRUE)
mean(pds_y$cyclelength, na.rm = TRUE)
sd(pds_p$cyclelength, na.rm = TRUE)
sd(pds_y$cyclelength, na.rm = TRUE)

# Set <14 day cycles and >90 day cycles to NA
pds_p <- pds_p %>%
  mutate(cyclelength = if_else(cyclelength < 14 | cyclelength > 90, NA, cyclelength))
pds_y <- pds_y %>%
  mutate(cyclelength = if_else(cyclelength < 14 | cyclelength > 90, NA, cyclelength))

# recheck spread of data
table(pds_p$cyclelength)
table(pds_y$cyclelength)
mean(pds_p$cyclelength, na.rm = TRUE)
mean(pds_y$cyclelength, na.rm = TRUE)
sd(pds_p$cyclelength, na.rm = TRUE)
sd(pds_y$cyclelength, na.rm = TRUE)

##########################
# IN THE BELOW CODE BLOCK
# Calculate "Days Since First Day of Cycle"
##########################

# convert interview date to yyyy-mm-dd
pds_p <- pds_p %>%
  mutate(interview_date = as.Date(interview_date, format = "%m/%d/%Y"))
pds_y <- pds_y %>%
 mutate(interview_date = as.Date(interview_date, format = "%m/%d/%Y"))

pds_p <- pds_p %>%
  mutate(Days_Since_Cycle_start = interview_date - firstdatelastcycle)
pds_y <- pds_y %>%
  mutate(Days_Since_Cycle_start = interview_date - firstdatelastcycle)

##########################
# IN THE BELOW CODE BLOCK
# Calculate Ratio/Percentage through current cycle
##########################

pds_p <- pds_p %>%
  mutate(DaysSince_CycleLength_Ratio = as.numeric(Days_Since_Cycle_start / cyclelength))
pds_y <- pds_y %>%
  mutate(DaysSince_CycleLength_Ratio = as.numeric(Days_Since_Cycle_start / cyclelength))

pds_p <- pds_p %>%
  # Make rounded ratio
  mutate(rounded_ratio = round(DaysSince_CycleLength_Ratio)) %>%
  # Find remainder
  mutate(remainder = DaysSince_CycleLength_Ratio - rounded_ratio)
pds_y <- pds_y %>%
  # Make rounded ratio
  mutate(rounded_ratio = round(DaysSince_CycleLength_Ratio)) %>%
  # Find remainder
  mutate(remainder = DaysSince_CycleLength_Ratio - rounded_ratio)

##########################
# IN THE BELOW CODE BLOCK
# Make rough categorical estimates:  (follicular -0.5 to 0; luteal 0.00001 to 0.5)
##########################

# Make rough categorical estimates: remainder (+) = follicular ; remainder (-) = luteal
pds_p <- pds_p %>%
  mutate(Phase_Estimate = if_else(remainder >= 0, "follicular", NA)) %>%
  mutate(Phase_Estimate = if_else(remainder < 0, "luteal", Phase_Estimate))
pds_y <- pds_y %>%
  mutate(Phase_Estimate = if_else(remainder >= 0, "follicular", NA)) %>%
  mutate(Phase_Estimate = if_else(remainder < 0, "luteal", Phase_Estimate))


##########################
# IN THE BELOW CODE BLOCK
# Flag for Overdue cycles and implausible data
##########################

# Some individuals report their last menstruation to be further in the past than their average cycle length. Flag these cases as Overdue_Y1N0.
# Others report the start of their most recent cycle to be in the future, in relation to their interview date. Flag these cases as TimeTravelers_Y1N0.
pds_p <- pds_p %>%
  mutate(Overdue_Y1N0 = if_else(DaysSince_CycleLength_Ratio > 1, 1, 0),
         TimeTravelers_Y1N0 = if_else(Days_Since_Cycle_start < 0, 1, 0))
pds_y <- pds_y %>%
  mutate(Overdue_Y1N0 = if_else(DaysSince_CycleLength_Ratio > 1, 1, 0),
         TimeTravelers_Y1N0 = if_else(Days_Since_Cycle_start < 0, 1, 0))


##########################
# IN THE BELOW CODE BLOCK
# Clean Data for export
##########################

# reorder columns
pds_p <- pds_p %>%
  select(src_subject_id, eventname, interview_date,
         tanner_stage, Phase_Estimate, firstdatelastcycle, cyclelength, cycleregular, childbirthcontrol, 
         Overdue_Y1N0, TimeTravelers_Y1N0,
         Days_Since_Cycle_start, DaysSince_CycleLength_Ratio, rounded_ratio, remainder)
pds_y <- pds_y %>%
  select(src_subject_id, eventname, interview_date,
         tanner_stage, Phase_Estimate, firstdatelastcycle, cyclelength, cycleregular, childbirthcontrol,
         Overdue_Y1N0, TimeTravelers_Y1N0,
         Days_Since_Cycle_start, DaysSince_CycleLength_Ratio, rounded_ratio, remainder)

##########################
# IN THE BELOW CODE BLOCK
# write to csv
##########################

write_csv(pds_p, paste0(derivative_data, 'Pubertal_Phases_5.0_ParentRep_10_7_25.csv'))
write_csv(pds_y, paste0(derivative_data, 'Pubertal_Phases_5.0_YouthRep_10_7_25.csv'))

