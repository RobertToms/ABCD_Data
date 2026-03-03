#########################
# Model_Pubertal_Cycle_5.0.R
# Author: Robert Toms
# 6.0 -> 5.0 Update: Dylan Seay & Faith Crighton
# Date: 02/06/2026
#########################

library(tidyverse)
library(lubridate)

sourcepath <- '/path/to/ABCD_5.0/core'
derivative_data <- '/path/to/derivative_data/5.0/'

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

# count subjects
p_subject_counter <- pds_p %>% select(src_subject_id) %>% unique() %>% left_join(sex, by = 'src_subject_id')
table(p_subject_counter$demo_sex_v2) #5677 females 
y_subject_counter <- pds_y %>% select(src_subject_id) %>% unique() %>% left_join(sex, by = 'src_subject_id')
table(y_subject_counter$demo_sex_v2) #5677 females

# rename for human readability
pds_p <- pds_p %>%
  rename(pubertal_stage = pds_p_ss_female_category, 
         firstdatelastcycle = menstrualcycle1_p, 
         cyclelength = menstrualcycle2_p, 
         cycleregular = menstrualcycle3_p, 
         childbirthcontrol = menstrualcycle4_p)
pds_y <- pds_y %>%
  rename(pubertal_stage = pds_y_ss_female_category, 
         firstdatelastcycle = menstrualcycle1_y, 
         cyclelength = menstrualcycle2_y, 
         cycleregular = menstrualcycle3_y, 
         childbirthcontrol = menstrualcycle4_y)

# correct data types for character to their appropriate type for usability
pds_p <- pds_p %>%
  mutate(pubertal_stage = as.numeric(pubertal_stage),
         firstdatelastcycle = ymd(firstdatelastcycle),
         cyclelength = as.numeric(cyclelength),
         cycleregular = as.numeric(cycleregular),
         childbirthcontrol = as.numeric(childbirthcontrol))
pds_y <- pds_y %>%
  mutate(pubertal_stage = as.numeric(pubertal_stage),
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

# Remove and count subjects/timepoints with no cycle length data
y_usable <- pds_y %>% filter(!is.na(cyclelength))
p_usable <- pds_p %>% filter(!is.na(cyclelength))
y_usable_subs <- y_usable %>% distinct(src_subject_id) %>% pull(src_subject_id) #5677 -> 3473, 2,204 total 
p_usable_subs <- p_usable %>% distinct(src_subject_id) %>% pull(src_subject_id) #5677 -> 3105, 2,572 total

# count usable subjects again (sanity check!)
p_use_subs <- p_usable %>% filter(!is.na(cyclelength)) %>% distinct(src_subject_id) %>% pull(src_subject_id) #3105, same  
y_use_subs <- y_usable %>% filter(!is.na(cyclelength)) %>% distinct(src_subject_id) %>% pull(src_subject_id) #3473, same

# Set <14 day cycles and >90 day cycles to NA
pds_p <- pds_p %>%
  mutate(cyclelength = if_else(cyclelength < 14 | cyclelength > 90, NA, cyclelength))
pds_y <- pds_y %>%
  mutate(cyclelength = if_else(cyclelength < 14 | cyclelength > 90, NA, cyclelength))

# Remove and count timepoints/subjects with clinically abnormally short cycles (<14 days) or clinical amenorrhea (>90 days)

# less than 14 days
y_usable <- y_usable %>% mutate(lt14 = (cyclelength < 14)) %>% filter(lt14 == FALSE)
p_usable <- p_usable %>% mutate(lt14 = (cyclelength < 14)) %>% filter(lt14 == FALSE)
y_usable_subs <- y_usable %>% distinct(src_subject_id) %>% pull(src_subject_id) #3473 -> 3369, 104 total
p_usable_subs <- p_usable %>% distinct(src_subject_id) %>% pull(src_subject_id) #3105 -> 2765, 340 total

# count usable subjects again (sanity check!)
p_use_subs <- p_usable %>% filter(!is.na(cyclelength)) %>% distinct(src_subject_id) %>% pull(src_subject_id) #2765, same  
y_use_subs <- y_usable %>% filter(!is.na(cyclelength)) %>% distinct(src_subject_id) %>% pull(src_subject_id) #3369, same

# greater than 90 days
y_usable <- y_usable %>% mutate(gr90 = (cyclelength > 90)) %>% filter(gr90 == FALSE)
p_usable <- p_usable %>% mutate(gr90 = (cyclelength > 90)) %>% filter(gr90 == FALSE)
y_usable_subs <- y_usable %>% distinct(src_subject_id) %>% pull(src_subject_id) # 3369 -> 3364, 5 total
p_usable_subs <- p_usable %>% distinct(src_subject_id) %>% pull(src_subject_id) # 2765 -> 2759, 6 total

# count usable subjects again (sanity check!)
p_use_subs <- p_usable %>% filter(!is.na(cyclelength)) %>% distinct(src_subject_id) %>% pull(src_subject_id) #2759, same  
y_use_subs <- y_usable %>% filter(!is.na(cyclelength)) %>% distinct(src_subject_id) %>% pull(src_subject_id) #3364, same

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
# Flag for Overdue cycles, implausible data, and missing data
##########################

# Overdue_Y1N0 -- Yes "1" if Last menstruation reported to be further in the past than their average cycle length, else No "0"
# TimeTravelers_Y1N0 -- Yes "1" if reported start of their most recent cycle is in the future in relation to their interview date, else No "0"
# Missing_CycleStartDate_Y1N0 -- Yes "1" if missing the start date of their last cycle, else No "0"
# Not_Regular_Y1N0 -- Yes "1" if cycle not regular, else No "0". Cycle estimates are only valid if they report a regular cycle.
# BirthControl_Yes_Or_Unclear_Y1N0 -- Yes "1" if they don't report not being on birth control, else No "0". Optional screening flag for if subjects report being on birth control, refuse to answer, or don't know

pds_p <- pds_p %>%
  mutate(Overdue_Y1N0 = if_else(DaysSince_CycleLength_Ratio > 1, 1, 0),
         TimeTravelers_Y1N0 = if_else(Days_Since_Cycle_start < 0, 1, 0),
         Missing_CycleStartDate_Y1N0 = if_else(is.na(firstdatelastcycle), 1, 0),
         Not_Regular_Y1N0 = if_else(cycleregular != 1, 1, 0),
         BirthControl_Yes_Or_Unclear_Y1N0 = if_else(childbirthcontrol != 0, 1, 0))
pds_y <- pds_y %>%
  mutate(Overdue_Y1N0 = if_else(DaysSince_CycleLength_Ratio > 1, 1, 0),
         TimeTravelers_Y1N0 = if_else(Days_Since_Cycle_start < 0, 1, 0),
         Missing_CycleStartDate_Y1N0 = if_else(is.na(firstdatelastcycle), 1, 0),
         Not_Regular_Y1N0 = if_else(cycleregular != 1, 1, 0),
         BirthControl_Yes_Or_Unclear_Y1N0 = if_else(childbirthcontrol != 0, 1, 0))

##########################
# IN THE BELOW CODE BLOCK
# Clean Data for export
##########################

# reorder columns
pds_p <- pds_p %>%
  select(src_subject_id, eventname, interview_date,
         pubertal_stage, Phase_Estimate, firstdatelastcycle, cyclelength, cycleregular, childbirthcontrol, 
         BirthControl_Yes_Or_Unclear_Y1N0, Overdue_Y1N0, TimeTravelers_Y1N0, Missing_CycleStartDate_Y1N0, Not_Regular_Y1N0, 
         Days_Since_Cycle_start, DaysSince_CycleLength_Ratio, rounded_ratio, remainder)
pds_y <- pds_y %>%
  select(src_subject_id, eventname, interview_date,
         pubertal_stage, Phase_Estimate, firstdatelastcycle, cyclelength, cycleregular, childbirthcontrol,
         BirthControl_Yes_Or_Unclear_Y1N0, Overdue_Y1N0, TimeTravelers_Y1N0, Missing_CycleStartDate_Y1N0, Not_Regular_Y1N0,
         Days_Since_Cycle_start, DaysSince_CycleLength_Ratio, rounded_ratio, remainder)


##########################
# IN THE BELOW CODE BLOCK
# Reset Flagged phases to NA and Get Counts
##########################

#Counting for missing cycle start date 
p_usable_subs <- pds_p %>% filter(!is.na(Phase_Estimate)) %>% distinct(src_subject_id) %>% pull(src_subject_id) #2759 -> 2751, 8 total
y_usable_subs <- pds_y %>% filter(!is.na(Phase_Estimate)) %>% distinct(src_subject_id) %>% pull(src_subject_id) #3364 -> 3350, 14 total

# Reset Phases for people who reported the start date of their most recent cycle being in the future in relation to the date they were interviewed (impossible)
pds_p <- pds_p %>%
  mutate(Phase_Estimate = if_else(TimeTravelers_Y1N0 == 1, NA, Phase_Estimate))
pds_y <- pds_y %>%
  mutate(Phase_Estimate = if_else(TimeTravelers_Y1N0 == 1, NA, Phase_Estimate))

p_usable_subs <- pds_p %>% filter(!is.na(Phase_Estimate)) %>% distinct(src_subject_id) %>% pull(src_subject_id) #2751 -> 2664, 87 total
y_usable_subs <- pds_y %>% filter(!is.na(Phase_Estimate)) %>% distinct(src_subject_id) %>% pull(src_subject_id) #3350 -> 3318, 32 total

# Reset phases for people whose cycles aren't regular yet, can't have reliable phase estimates
pds_p <- pds_p %>%
  mutate(Phase_Estimate = if_else(Not_Regular_Y1N0 == 1, NA, Phase_Estimate))
pds_y <- pds_y %>%
  mutate(Phase_Estimate = if_else(Not_Regular_Y1N0 == 1, NA, Phase_Estimate))

p_usable_subs <- pds_p %>% filter(!is.na(Phase_Estimate)) %>% distinct(src_subject_id) %>% pull(src_subject_id) #2664 -> 2168, 496 total 
y_usable_subs <- pds_y %>% filter(!is.na(Phase_Estimate)) %>% distinct(src_subject_id) %>% pull(src_subject_id) #3318 -> 2785, 533 total

# Reset Phases for people who are overdue for their cycle
pds_p <- pds_p %>%
  mutate(Phase_Estimate = if_else(Overdue_Y1N0 == 1, NA, Phase_Estimate))
pds_y <- pds_y %>%
  mutate(Phase_Estimate = if_else(Overdue_Y1N0 == 1, NA, Phase_Estimate))

p_usable_subs <- pds_p %>% filter(!is.na(Phase_Estimate)) %>% distinct(src_subject_id) %>% pull(src_subject_id) #2168 -> 1776, 392 total
y_usable_subs <- pds_y %>% filter(!is.na(Phase_Estimate)) %>% distinct(src_subject_id) %>% pull(src_subject_id) #2785 -> 2212, 573 total

##########################
# IN THE BELOW CODE BLOCK
# write to csv
##########################

write_csv(pds_p, paste0(derivative_data, 'Pubertal_Phases_5.0_ParentRep_2_6_26.csv'))
write_csv(pds_y, paste0(derivative_data, 'Pubertal_Phases_5.0_YouthRep_2_6_26.csv'))


##########################
# IN THE BELOW CODE BLOCK
# Run to remove subjects that do not affirm not being on birth control
##########################

pds_p <- pds_p %>%
  mutate(Phase_Estimate = if_else(BirthControl_Yes_Or_Unclear_Y1N0 == 1, NA, Phase_Estimate))
pds_y <- pds_y %>%
  mutate(Phase_Estimate = if_else(BirthControl_Yes_Or_Unclear_Y1N0 == 1, NA, Phase_Estimate))

p_usable_subs <- pds_p %>% filter(!is.na(Phase_Estimate)) %>% distinct(src_subject_id) %>% pull(src_subject_id) # 1776 -> 1734; 42 subjects removed
y_usable_subs <- pds_y %>% filter(!is.na(Phase_Estimate)) %>% distinct(src_subject_id) %>% pull(src_subject_id) # 2212 -> 2165; 47 subjects removed

