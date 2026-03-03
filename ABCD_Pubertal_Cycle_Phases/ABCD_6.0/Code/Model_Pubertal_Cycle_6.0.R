#########################
# Model_Pubertal_Cycle_6.0.R
# Author: Robert Toms
# 5.0 -> 6.0 Update: Dylan Seay
# Date: 12/4/2025
#########################

library(tidyverse)
library(lubridate)

sourcepath <- '/path/to/ABCD_6.0/Data/'
derivative_data <- '/path/to/derivative_data/6.0/'

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
  select(participant_id, ab_g_stc__cohort_sex) %>% rename(sex = ab_g_stc__cohort_sex) # 11868 total subjects enrolled

# remove males - 6190 subjects removed
pds_p <- pds_p %>%
  left_join(sex, by = 'participant_id') %>%
  filter(sex == 2) %>% # before: 67223, after: 31978
  select(-sex)
pds_y <- pds_y %>%
  left_join(sex, by = 'participant_id') %>%
  filter(sex == 2) %>% # before: 67908, after: 32365
  select(-sex)

# count subjects
p_subject_counter <- pds_p %>% select(participant_id) %>% unique() %>% left_join(sex, by = 'participant_id')
table(p_subject_counter$sex) # 5677 Females -- # sub-FZWKRBW7 missing from spreadsheet, reason unclear after redownload and manual inspection.
y_subject_counter <- pds_y %>% select(participant_id) %>% unique() %>% left_join(sex, by = 'participant_id')
table(y_subject_counter$sex) # 5678 Females

# rename for human readability
pds_p <- pds_p %>%
  rename(pubertal_stage = ph_p_pds__f_categ, 
         firstdatelastcycle = ph_p_pds__f_002__02, 
         cyclelength = ph_p_pds__f_002__03, 
         cycleregular = ph_p_pds__f_002__04, 
         childbirthcontrol = ph_p_pds__f_002__05)
pds_y <- pds_y %>%
  rename(pubertal_stage = ph_y_pds__f_categ, 
         firstdatelastcycle = ph_y_pds__f_002__02, 
         cyclelength = ph_y_pds__f_002__03, 
         cycleregular = ph_y_pds__f_002__04, 
         childbirthcontrol = ph_y_pds__f_002__05)
interview_date <- interview_date %>%
  rename(interview_date = ab_g_dyn__visit_dtt)

# set 'n/a's to NA
pds_p <- pds_p %>%
  mutate(across(c(pubertal_stage, firstdatelastcycle, cyclelength, cycleregular, childbirthcontrol), ~ na_if(., 'n/a')))
pds_y <- pds_y %>%
  mutate(across(c(pubertal_stage, firstdatelastcycle, cyclelength, cycleregular, childbirthcontrol), ~ na_if(., 'n/a')))

# correct data types from character to their appropriate type for usability
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

# Remove and count subjects/timepoints with no cycle length data
y_usable <- pds_y %>% filter(!is.na(cyclelength))
p_usable <- pds_p %>% filter(!is.na(cyclelength))
y_usable_subs <- y_usable %>% distinct(participant_id) %>% pull(participant_id) # 5678 -> 4647, 1031 total
p_usable_subs <- p_usable %>% distinct(participant_id) %>% pull(participant_id) # 5677 -> 4193, 1484 total

# Normative cycle lengths and clinical categorization for the population were taken from (Bull et al., 2019):
# https://www.nature.com/articles/s41746-019-0152-7  

# Flag subjects with clinically abnormally short cycles (<14 days) or clinical amenorrhea (>90 days)
lt14_y <- y_usable %>% mutate(lt14 = (cyclelength < 14)) %>% filter(lt14 == TRUE) %>% distinct(participant_id) %>% pull(participant_id) # 417 subjects affected
gr90_y <- y_usable %>% mutate(gr90 = (cyclelength > 90)) %>% filter(gr90 == TRUE) %>% distinct(participant_id) %>% pull(participant_id) # 20 subjects affected
lt14_p <- p_usable %>% mutate(lt14 = (cyclelength < 14)) %>% filter(lt14 == TRUE) %>% distinct(participant_id) %>% pull(participant_id) # 1053 subjects affected
gr90_p <- p_usable %>% mutate(gr90 = (cyclelength > 90)) %>% filter(gr90 == TRUE) %>% distinct(participant_id) %>% pull(participant_id) # 28 subjects affected

# Remove and count timepoints/subjects with clinically abnormally short cycles (<14 days) or clinical amenorrhea (>90 days)
# less than 14 days
y_usable <- y_usable %>% mutate(lt14 = (cyclelength < 14)) %>% filter(lt14 == FALSE)
p_usable <- p_usable %>% mutate(lt14 = (cyclelength < 14)) %>% filter(lt14 == FALSE)
y_usable_subs <- y_usable %>% distinct(participant_id) %>% pull(participant_id) # 4647 -> 4578, 69 total
p_usable_subs <- p_usable %>% distinct(participant_id) %>% pull(participant_id) # 4193 -> 3855, 338 total

# greater than 90 days
y_usable <- y_usable %>% mutate(gr90 = (cyclelength > 90)) %>% filter(gr90 == FALSE)
p_usable <- p_usable %>% mutate(gr90 = (cyclelength > 90)) %>% filter(gr90 == FALSE)
y_usable_subs <- y_usable %>% distinct(participant_id) %>% pull(participant_id) # 4578 -> 4568, 10 total
p_usable_subs <- p_usable %>% distinct(participant_id) %>% pull(participant_id) # 3855 -> 3847, 8 total

### SANITY CHECK ###
# Set <14 day cycles to NA
p_usable <- p_usable %>%
  mutate(cyclelength = if_else(cyclelength < 14, NA, cyclelength))
y_usable <- y_usable %>%
  mutate(cyclelength = if_else(cyclelength < 14, NA, cyclelength))
# count usable subjects again
p_use_subs <- p_usable %>% filter(!is.na(cyclelength)) %>% distinct(participant_id) %>% pull(participant_id) # 3847, same amount
y_use_subs <- y_usable %>% filter(!is.na(cyclelength)) %>% distinct(participant_id) %>% pull(participant_id) # 4568, same amount

# Set >90 day cycles to NA
p_usable <- p_usable %>%
  mutate(cyclelength = if_else(cyclelength > 90, NA, cyclelength))
y_usable <- y_usable %>%
  mutate(cyclelength = if_else(cyclelength > 90, NA, cyclelength))
# count usable subjects again
p_use_subs <- p_usable %>% filter(!is.na(cyclelength)) %>% distinct(participant_id) %>% pull(participant_id) # 3847, same amount
y_use_subs <- y_usable %>% filter(!is.na(cyclelength)) %>% distinct(participant_id) %>% pull(participant_id) # 4568, same amount
### END SANITY CHECK ###

# recheck spread of data
table(p_usable$cyclelength)
table(y_usable$cyclelength)
mean(p_usable$cyclelength, na.rm = TRUE)
mean(y_usable$cyclelength, na.rm = TRUE)
sd(p_usable$cyclelength, na.rm = TRUE)
sd(y_usable$cyclelength, na.rm = TRUE)

##########################
# IN THE BELOW CODE BLOCK
# Calculate "Days Since First Day of Cycle"
##########################

# convert interview date to yyyy-mm-dd
p_usable <- p_usable %>%
  mutate(interview_date = as.Date(interview_date, format = "%Y/%m/%d"))
y_usable <- y_usable %>%
  mutate(interview_date = as.Date(interview_date, format = "%Y/%m/%d"))

p_usable <- p_usable %>%
  mutate(Days_Since_Cycle_start = interview_date - firstdatelastcycle)
y_usable <- y_usable %>%
  mutate(Days_Since_Cycle_start = interview_date - firstdatelastcycle)

##########################
# IN THE BELOW CODE BLOCK
# Calculate Ratio/Percentage through current cycle
##########################

p_usable <- p_usable %>%
  mutate(DaysSince_CycleLength_Ratio = as.numeric(Days_Since_Cycle_start / cyclelength))
y_usable <- y_usable %>%
  mutate(DaysSince_CycleLength_Ratio = as.numeric(Days_Since_Cycle_start / cyclelength))

p_usable <- p_usable %>%
  # Make rounded ratio
  mutate(rounded_ratio = round(DaysSince_CycleLength_Ratio)) %>%
  # Find remainder
  mutate(remainder = DaysSince_CycleLength_Ratio - rounded_ratio)
y_usable <- y_usable %>%
  # Make rounded ratio
  mutate(rounded_ratio = round(DaysSince_CycleLength_Ratio)) %>%
  # Find remainder
  mutate(remainder = DaysSince_CycleLength_Ratio - rounded_ratio)

##########################
# IN THE BELOW CODE BLOCK
# Make rough categorical estimates:  (follicular -0.5 to 0; luteal 0.00001 to 0.5)
##########################

# Make rough categorical estimates: remainder (+) = follicular ; remainder (-) = luteal
p_usable <- p_usable %>%
  mutate(Phase_Estimate = if_else(remainder >= 0, "follicular", NA)) %>%
  mutate(Phase_Estimate = if_else(remainder < 0, "luteal", Phase_Estimate))
y_usable <- y_usable %>%
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

p_usable <- p_usable %>%
  mutate(Overdue_Y1N0 = if_else(DaysSince_CycleLength_Ratio > 1, 1, 0),
         TimeTravelers_Y1N0 = if_else(Days_Since_Cycle_start < 0, 1, 0),
         Missing_CycleStartDate_Y1N0 = if_else(is.na(firstdatelastcycle), 1, 0),
         Not_Regular_Y1N0 = if_else(cycleregular != 1, 1, 0),
         BirthControl_Yes_Or_Unclear_Y1N0 = if_else(childbirthcontrol != 0, 1, 0))
y_usable <- y_usable %>%
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
p_usable <- p_usable %>%
  select(participant_id, session_id, interview_date,
         pubertal_stage, Phase_Estimate, firstdatelastcycle, cyclelength, cycleregular, childbirthcontrol, 
         BirthControl_Yes_Or_Unclear_Y1N0, Overdue_Y1N0, TimeTravelers_Y1N0, Missing_CycleStartDate_Y1N0, Not_Regular_Y1N0, 
         Days_Since_Cycle_start, DaysSince_CycleLength_Ratio, rounded_ratio, remainder)
y_usable <- y_usable %>%
  select(participant_id, session_id, interview_date,
         pubertal_stage, Phase_Estimate, firstdatelastcycle, cyclelength, cycleregular, childbirthcontrol,
         BirthControl_Yes_Or_Unclear_Y1N0, Overdue_Y1N0, TimeTravelers_Y1N0, Missing_CycleStartDate_Y1N0, Not_Regular_Y1N0,
         Days_Since_Cycle_start, DaysSince_CycleLength_Ratio, rounded_ratio, remainder)

##########################
# IN THE BELOW CODE BLOCK
# Reset Flagged phases to NA and Get Counts
##########################

p_use_subs <- p_usable %>% filter(!is.na(Phase_Estimate)) %>% distinct(participant_id) %>% pull(participant_id) # 3847 -> 3825; 22 subjects missing cycle start date
y_use_subs <- y_usable %>% filter(!is.na(Phase_Estimate)) %>% distinct(participant_id) %>% pull(participant_id) # 4568 -> 4517; 51 subjects missing cycle start date

# Reset Phases for people who reported the start date of their most recent cycle being in the future in relation to the date they were interviewed (impossible)
p_usable <- p_usable %>%
  mutate(Phase_Estimate = if_else(TimeTravelers_Y1N0 == 1, NA, Phase_Estimate))
y_usable <- y_usable %>%
  mutate(Phase_Estimate = if_else(TimeTravelers_Y1N0 == 1, NA, Phase_Estimate))

p_use_subs <- p_usable %>% filter(!is.na(Phase_Estimate)) %>% distinct(participant_id) %>% pull(participant_id) # 3825 -> 3713; 112 subjects overdue for cycle
y_use_subs <- y_usable %>% filter(!is.na(Phase_Estimate)) %>% distinct(participant_id) %>% pull(participant_id) # 4517 -> 4495; 22 subjects overdue for cycle

# Reset phases for people whose cycles aren't regular yet, can't have reliable phase estimates
p_usable <- p_usable %>%
  mutate(Phase_Estimate = if_else(Not_Regular_Y1N0 == 1, NA, Phase_Estimate))
y_usable <- y_usable %>%
  mutate(Phase_Estimate = if_else(Not_Regular_Y1N0 == 1, NA, Phase_Estimate))

p_use_subs <- p_usable %>% filter(!is.na(Phase_Estimate)) %>% distinct(participant_id) %>% pull(participant_id) # 3713 -> 3117; 596 subjects removed
y_use_subs <- y_usable %>% filter(!is.na(Phase_Estimate)) %>% distinct(participant_id) %>% pull(participant_id) # 4495 -> 3918; 577 subjects removed

# Reset Phases for people who are overdue for their cycle
p_usable <- p_usable %>%
  mutate(Phase_Estimate = if_else(Overdue_Y1N0 == 1, NA, Phase_Estimate))
y_usable <- y_usable %>%
  mutate(Phase_Estimate = if_else(Overdue_Y1N0 == 1, NA, Phase_Estimate))

p_use_subs <- p_usable %>% filter(!is.na(Phase_Estimate)) %>% distinct(participant_id) %>% pull(participant_id) # 3117 -> 2699; 418 subjects removed
y_use_subs <- y_usable %>% filter(!is.na(Phase_Estimate)) %>% distinct(participant_id) %>% pull(participant_id) # 3918 -> 3491; 427 subjects removed

##########################
# IN THE BELOW CODE BLOCK
# Write to CSV
##########################

write_csv(p_usable, paste0(derivative_data, 'Pubertal_Phases_ParentRep_12_4_25.csv'))
write_csv(y_usable, paste0(derivative_data, 'Pubertal_Phases_YouthRep_12_4_25.csv'))


##########################
# IN THE BELOW CODE BLOCK
# Sanity Checks
##########################

p_sanity <- p_usable %>%  filter(!is.na(Phase_Estimate))
y_sanity <- y_usable %>%  filter(!is.na(Phase_Estimate))

# Are Days since start of most recent cycle normative? (Below 90 days, re: Bull et al. 2019; https://www.nature.com/articles/s41746-019-0152-7)
table(p_sanity$Days_Since_Cycle_start) # yes
table(y_sanity$Days_Since_Cycle_start) # yes


##########################
# IN THE BELOW CODE BLOCK
# Run to remove subjects that do not affirm not being on birth control
##########################

p_usable <- p_usable %>%
  mutate(Phase_Estimate = if_else(BirthControl_Yes_Or_Unclear_Y1N0 == 1, NA, Phase_Estimate))
y_usable <- y_usable %>%
  mutate(Phase_Estimate = if_else(BirthControl_Yes_Or_Unclear_Y1N0 == 1, NA, Phase_Estimate))

p_use_subs <- p_usable %>% filter(!is.na(Phase_Estimate)) %>% distinct(participant_id) %>% pull(participant_id) # 2699 -> 2598; 101 subjects removed
y_use_subs <- y_usable %>% filter(!is.na(Phase_Estimate)) %>% distinct(participant_id) %>% pull(participant_id) # 3491 -> 3382; 109 subjects removed

