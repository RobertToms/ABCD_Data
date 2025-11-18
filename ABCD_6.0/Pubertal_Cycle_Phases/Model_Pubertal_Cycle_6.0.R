#########################
# Model_Pubertal_Cycle_6.0.R
# Author: Robert Toms
# 5.0 -> 6.0 Update: Dylan Seay
# Date: 10/3/2025
#########################

library(tidyverse)
library(lubridate)

sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/6.0/"

# import data
pds_p <- read_tsv(paste0(sourcepath, '/Physical_Health/ph_p_pds.tsv'))
pds_y <- read_tsv(paste0(sourcepath, '/Physical_Health/ph_y_pds.tsv'))
sex <- read_tsv(paste0(sourcepath, '/abcd_general/ab_g_stc.tsv'))
interview_date <- read_tsv(paste0(sourcepath, '/abcd_general/ab_g_dyn.tsv'))

##########################
# IN THE BELOW CODE BLOCK
# Clean and Merge Data
##########################

# Keep only relevant columns
pds_p <- pds_p %>%
  select(participant_id, 
         session_id, 
         ph_p_pds__f_categ,   # Approximate Tanner Stages; Pre-Pubertal = 1, Early Pubertal = 2, Mid Pubertal = 3, Late Pubertal = 4, Post-Pubertal = 5
         ph_p_pds__f_002__02, # What was the date of the first day of your child's last period?
         ph_p_pds__f_002__03, # On average, how many days are there between the first day of their and the first day of their next period? (eg., 30 days)
         ph_p_pds__f_002__04, # Is your child's menstrual cycle regular? Yes = 1, No = 0, Decline to Answer = 777, Don't Know = 999
         ph_p_pds__f_002__05) # Is your child currently using hormonal birth control (eg. the pill, hormone patch, hormone injection)? Yes = 1, No = 0, Decline to Answer = 777, Don't Know = 999
pds_y <- pds_y %>%
  select(participant_id, 
         session_id, 
         ph_y_pds__f_categ,   # Approximate Tanner Stages; Pre-Pubertal = 1, Early Pubertal = 2, Mid Pubertal = 3, Late Pubertal = 4, Post-Pubertal = 5
         ph_y_pds__f_002__02, # What was the date of the first day of your last period?
         ph_y_pds__f_002__03, # On average, how many days are there between the first day of your period and the first day of your next period? For example, if the first day of bleeding is typically at the beginning of each month, the interval between periods is 30 days.
         ph_y_pds__f_002__04, # Is your menstrual cycle regular? Yes = 1, No = 0, Don't Know = 999
         ph_y_pds__f_002__05) # Are you currently using hormonal birth control (eg. the pill, hormone patch, hormone injection)? Yes = 1, No = 0, Don't Know = 999

interview_date <- interview_date %>%
  select(participant_id, session_id, ab_g_dyn__visit_dtt)

sex <- sex %>%
  select(participant_id, ab_g_stc__cohort_sex)

# remove males
pds_p <- pds_p %>%
  left_join(sex, by = 'participant_id') %>%
  filter(ab_g_stc__cohort_sex == 2) %>% # before: 67223, after: 31978
  select(-ab_g_stc__cohort_sex)
pds_y <- pds_y %>%
  left_join(sex, by = 'participant_id') %>%
  filter(ab_g_stc__cohort_sex == 2) %>% # before: 67908, after: 32365
  select(-ab_g_stc__cohort_sex)

# rename for human readability
pds_p <- pds_p %>%
  rename(tanner_stage = ph_p_pds__f_categ, 
         firstdatelastcycle = ph_p_pds__f_002__02, 
         cyclelength = ph_p_pds__f_002__03, 
         cycleregular = ph_p_pds__f_002__04, 
         childbirthcontrol = ph_p_pds__f_002__05)
pds_y <- pds_y %>%
  rename(tanner_stage = ph_y_pds__f_categ, 
         firstdatelastcycle = ph_y_pds__f_002__02, 
         cyclelength = ph_y_pds__f_002__03, 
         cycleregular = ph_y_pds__f_002__04, 
         childbirthcontrol = ph_y_pds__f_002__05)
interview_date <- interview_date %>%
  rename(interview_date = ab_g_dyn__visit_dtt)

# set 'n/a's to NA
pds_p <- pds_p %>%
  mutate(across(c(tanner_stage, firstdatelastcycle, cyclelength, cycleregular, childbirthcontrol), ~ na_if(., 'n/a')))
pds_y <- pds_y %>%
  mutate(across(c(tanner_stage, firstdatelastcycle, cyclelength, cycleregular, childbirthcontrol), ~ na_if(., 'n/a')))

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
  left_join(interview_date, by = c('participant_id', 'session_id'))
pds_y <- pds_y %>%
  left_join(interview_date, by = c('participant_id', 'session_id'))

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
  mutate(interview_date = as.Date(interview_date, format = "%Y/%m/%d"))
pds_y <- pds_y %>%
  mutate(interview_date = as.Date(interview_date, format = "%Y/%m/%d"))

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
  select(participant_id, session_id, interview_date,
         tanner_stage, Phase_Estimate, firstdatelastcycle, cyclelength, cycleregular, childbirthcontrol, 
         Overdue_Y1N0, TimeTravelers_Y1N0,
         Days_Since_Cycle_start, DaysSince_CycleLength_Ratio, rounded_ratio, remainder)
pds_y <- pds_y %>%
  select(participant_id, session_id, interview_date,
         tanner_stage, Phase_Estimate, firstdatelastcycle, cyclelength, cycleregular, childbirthcontrol,
         Overdue_Y1N0, TimeTravelers_Y1N0,
         Days_Since_Cycle_start, DaysSince_CycleLength_Ratio, rounded_ratio, remainder)

##########################
# IN THE BELOW CODE BLOCK
# write to csv
##########################

write_csv(pds_p, paste0(derivative_data, 'Pubertal_Phases_ParentRep_10_3_25.csv'))
write_csv(pds_y, paste0(derivative_data, 'Pubertal_Phases_YouthRep_10_3_25.csv'))

