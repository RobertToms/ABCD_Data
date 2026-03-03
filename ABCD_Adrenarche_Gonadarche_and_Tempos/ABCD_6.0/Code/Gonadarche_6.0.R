################
# Gonadarche_6.0.R
# Robert Toms, robert.toms@utdallas.edu
# 2/23/2026
################

# The purpose of this code is to calculate age at Gonadarche and Gonadarche Tempo in males and females.

library(tidyverse)
library(dplyr)
library(tidyverse)
library(dplyr)

datapath <- "/path/to/ABCD_6.0/Data"
derivative_data <- "/path/to/derivative_data/6.0/"

pds <- read_tsv(paste0(datapath, "/Physical_Health/ph_y_pds.tsv"))
sex <- read_tsv(paste0(datapath, "/abcd_general/ab_g_stc.tsv"))

#################
# IN THE BELOW CODE BLOCK
# Clean and Prepare Data
#################

# remove irrelevant columns
sex <- sex %>%
  select(participant_id, ab_g_stc__cohort_sex) %>% # Biological Sex
  rename(sex = ab_g_stc__cohort_sex)

# make human readable
sex <- sex %>%
  mutate(sex = case_when(
    sex == 1 ~ "M",
    sex == 2 ~ "F"))

# remove irrelevant columns
pds <- pds %>%
  select(participant_id,   # subject ID
         session_id,       # visit
         ph_y_pds_dtt,     # interview datetime
         ph_y_pds_age,     # age at interview (in years)
         ph_y_pds_001,     # Growth in Height: Has not yet begun to spurt (spurt means grow faster than usual) = 1; Has barely started = 2; Is definitely underway (has definitely started) = 3; Seems complete = 4; I don't know = 999; Decline to answer = 777
         ph_y_pds_002,     # Body Hair Growth: Has not yet begun to grow = 1; Has barely started = 2; Is definitely underway (has definitely started) = 3; Seems complete = 4; I don't know = 999; Decline to answer = 777
         ph_y_pds_003,     # Skin changes/Pimples: skin has not yet started changing = 1; skin has barely started changing = 2; skin changes are definitely underway (has definitely started) = 3; skin changes seem complete = 4; I don't know = 999; Decline to answer = 777
         ph_y_pds__f_001,  # (Female) Breasts begun to grow? : have not yet started growing = 1; have barely started growing = 2; breast growth is definitely underway (has definitely started) = 3; breast growth seems complete = 4; I don't know = 999; Decline to answer = 777
         ph_y_pds__f_002,  # (Female) Begun to Menstruate? : Yes = 1; No = 0; I don't know = 999; Refuse to answer = 777
         ph_y_pds__m_001,  # (Male) Deepening Voice? : voice has not yet started changing = 1; voice has barely started changing = 2; voice changes are definitely underway (has definitely started) = 3; voice changes seem complete = 4; I don't know = 999; Decline to answer = 777
         ph_y_pds__m_002,  # (Male) Facial Hair Growth? : Facial hair not yet started growing = 1; Facial hair has barely started growing = 2; Facial hair has definitely started = 3; Facial hair growth seems complete = 4; I don't know = 999; Decline to answer = 777
         ph_y_pds__f_mean, #, # (Female) PDS value : mean of ph_y_pds 001, 002, 003 and f_001 & f_002. f_002 Yes = 4, No = 1
         ph_y_pds__m_mean) # (Male) PDS Value : mean of ph_y_pds 001, 002, 003 and m_001 & m_002

# make human readable
pds <- pds %>%
  rename(interview_datetime = ph_y_pds_dtt,
         interview_age_yrs = ph_y_pds_age,
         growth_spurt = ph_y_pds_001,
         body_hair = ph_y_pds_002,
         skin_pimples = ph_y_pds_003,
         thelarche = ph_y_pds__f_001,
         menarche = ph_y_pds__f_002,
         voice_deep = ph_y_pds__m_001,
         facial_hair = ph_y_pds__m_002,
         ABCD_PDS = ph_y_pds__f_mean,
         male_PDS = ph_y_pds__m_mean)

# merge in sex data
pds_sex <- pds %>%
  left_join(sex, by = "participant_id")

# count 999s (Don't Know) and 777s (Refuse to Answer)
table(pds_sex$thelarche == 999, pds_sex$sex) # 1132 total Thelarche "Don't Know" reports; 1132 F
table(pds_sex$menarche == 999, pds_sex$sex) # 680 total Menarche "Don't Know" reports; 680 F
table(pds_sex$voice_deep == 999, pds_sex$sex) # 506 total Deepening Voice "Don't Know" reports; 506 M
table(pds_sex$facial_hair == 999, pds_sex$sex) # 402 total Facial Hair Growth "Don't Know" reports; 402 M
# 1132 + 680 = 1812 total Female "Don't Know" reports
# 506 + 402 = 908 total Male "Don't Know" reports
# 1812 + 908 = 2720 total "Don't Know" reports

pds_999s <- pds_sex %>% filter((pds_sex$thelarche == 999 | pds_sex$menarche == 999) | 
                                 (pds_sex$voice_deep == 999 | pds_sex$facial_hair == 999))
table(pds_999s$sex) # 1579 F / 763 M; 2342 total timepoints affected by "Don't Know" reports
subs_999 <- pds_999s %>% distinct(participant_id) %>% pull(participant_id) # 2342 timepoints, 2252 subjects affected
sex_999 <- pds_999s %>% select(participant_id, sex) %>% unique()
table(sex_999$sex) # 2252 subjects affected by "Don't Know" reports; 1489 F / 763 M

table(pds_sex$thelarche == 777, pds_sex$sex) # 923 total Thelarche "Refuse to Answer" reports; 923 F
table(pds_sex$menarche == 777, pds_sex$sex) # 1049 total Menarche "Refuse to Answer" reports; 1049 F
table(pds_sex$voice_deep == 777, pds_sex$sex) # 186 total Deepening Voice "Refuse to Answer" reports; 186 M
table(pds_sex$facial_hair == 777, pds_sex$sex) # 231 total Facial Hair "Refuse to Answer" reports; 231 M
# 923 + 1049 = 1972 total Female "Refuse to Answer" reports
# 186 + 231 = 417 total Male "Refuse to Answer" reports
# 1972 + 417 = 2389 total "Refuse to Answer" reports

pds_777s <- pds_sex %>% filter((pds_sex$thelarche == 777 | pds_sex$menarche == 777) | 
                                 (pds_sex$voice_deep == 777 | pds_sex$facial_hair == 777))
table(pds_777s$sex) # 1584 F / 314 M, 1898 total timepoints affected by "Refuse to Answer" reports
subs_777 <- pds_777s %>% distinct(participant_id) %>% pull(participant_id) # 1898 timepoints, 1299 subjects affected
sex_777 <- pds_777s %>% select(participant_id, sex) %>% unique()
table(sex_777$sex) # 1299 subjects affected by "Refuse to Answer" reports; 1031 F / 268 M

# Count subjects
subjects_counter <- pds_sex %>% distinct(participant_id) %>% pull(participant_id) # 11866

# 2 subjects missing, who and why?
all_subjects <- sex %>% distinct(participant_id) %>% pull(participant_id) 
missing <- setdiff(all_subjects, subjects_counter)
# sub-C2ZW147D -- male, no parent or youth pds data, only baseline data in ab_g_dyn
# sub-V4AXRCZ6 -- male, no youth pds data, only baseline parent pds and ab_g_dyn data

# Set 999s and 777s to NA, per Herting et al., 2021 best practices
pds_sex <- pds_sex %>%
  mutate(across(c(thelarche, 
                  menarche,
                  voice_deep,
                  facial_hair
  ), ~case_when(
    . == 999 ~ NA,
    . == 777 ~ NA,
    . == 'n/a' ~ NA,
    TRUE ~ as.numeric(.))))

# Count remaining subjects
subjects_counter <- pds_sex %>% distinct(participant_id) %>% pull(participant_id)

#################
# IN THE BELOW CODE BLOCK
# Manually Calculate PDS Values
#################

# Gonadarche PDS: 
# Males: Average of "Voice Deepening" and "Facial Hair Growth" PDS scores, must have both to have a valid Gonadarche PDS score.
# Females: Average of "Thelarche" and "Menarche" PDS scores, must have both to have a valid Gonadarche PDS score.

# reset Menarche values from 1 -> 4 (postmenarche) and 0 -> 1 (premenarche) to calculate PDS values
pds_sex <- pds_sex %>%
  mutate(menarche = case_when(
    menarche == 1 ~ 4,
    menarche == 0 ~ 1))

# calculate PDS values
pds_sex <- pds_sex %>%
  rowwise() %>%
  # Gonadarche PDS
  mutate(gonadarche_pds = case_when(
    sex == 'M' ~ sum(voice_deep, facial_hair),
    sex == 'F' ~ sum(thelarche, menarche))) %>% # if either is NA, the sum will be NA
  mutate(gonadarche_pds = gonadarche_pds / 2)

#################
# IN THE BELOW CODE BLOCK
# Count subjects with no data or insufficient data to impute ages/tempos (< 2 timepoints of data)
#################

# Gonadarche
pds_counts <- pds_sex %>% group_by(participant_id) %>% mutate(num_timepoints = sum(!is.na(gonadarche_pds))) %>% ungroup() %>% select(participant_id, num_timepoints, sex) %>% unique()
table(pds_counts$num_timepoints, pds_counts$sex) # 75 total, 63F/12M subjects with no gonadarche data; 415 total, 239F/176M with 1 timepoint; 490 total
pds_counts <- pds_counts %>% filter(num_timepoints > 1)
table(pds_counts$sex) # 5376 F / 6000 M; 11376 total subjects remain with 2+ timepoints of gonadarche data

# incorporate exclusions
pds_sex <- pds_sex %>% group_by(participant_id) %>% mutate(num_timepoints = sum(!is.na(gonadarche_pds))) %>% ungroup() %>% filter(num_timepoints > 1)
subs_count <- pds_sex %>% distinct(participant_id) %>% pull(participant_id) # 11376 subjects remain, sanity checked

#################
# IN THE BELOW CODE BLOCK
# Determine Each Subject's Number of Categories with Inconsistent Reports
#################

# According to the best practices of (Larson, Chaku, & Moussa-Tooks, 2025), 
# only adolescents with at least 2 increasing assessments of pubertal development are to be included in analysis.
# This was because they were using 3 timepoints of data, so essentially they were excluding those who reported inconsistently, e.g. reported reverse pubertal development.

# write function to determine if reported values decrease at any point (impossible reverse puberty)
determine_decreasing <- function(data) {    # takes in a list of values, e.g. c(1, 2, 2, 2, 3, 3, 4)
  decreasing <- 0                          # set inconsistent flag to 0
  num_values <- length(data)                    # count number of values to iterate thru
  if (num_values < 2) {return(0)}               # if less than 2, there's not enough data to compare, cannot be increasing or decreasing
  for (i in 1:(num_values - 1))                 # iterate thru values
    if (data[i] > data[i+1]) {                  # if current timepoint's value is greater than next timepoint's value
      decreasing <- 1                        # set is_inconsistent flag to 1
      break                                       # if an inconsistency is detected, break and report it
    }
  return(decreasing)                       # return inconsistent flag
}

# isolate pds reports
pds_decreasing <- pds_sex %>%
  select(participant_id, session_id, thelarche, menarche, voice_deep, facial_hair)

# male list values for is_inconsistent to iterate thru
pds_decreasing <- pds_decreasing %>%
  group_by(participant_id) %>%
  mutate(thelarche_values = list(na.omit(thelarche)),
         menarche_values = list(na.omit(menarche)),
         voice_values = list(na.omit(voice_deep)),
         facial_values = list(na.omit(facial_hair)))

# apply determine_decreasing function
pds_decreasing <- pds_decreasing %>%
  rowwise() %>%
  mutate(thelarche_decreasing_Y1N0 = determine_decreasing(thelarche_values),
         menarche_decreasing_Y1N0 = determine_decreasing(menarche_values),
         voice_decreasing_Y1N0 = determine_decreasing(voice_values),
         facial_decreasing_Y1N0 = determine_decreasing(facial_values))

# count inconsistencies in each pubertal category
pds_decreasing <- pds_decreasing %>%
  mutate(num_gonad_decreasing = sum(thelarche_decreasing_Y1N0, menarche_decreasing_Y1N0,
                                    voice_decreasing_Y1N0, facial_decreasing_Y1N0, na.rm = TRUE))

# keep only useful columns
pds_decreasing <- pds_decreasing %>%
  select(participant_id, num_gonad_decreasing) %>%
  unique()

#################
# IN THE BELOW CODE BLOCK
# Calculate Age at Gonadarche
#################

# define function to get slopes
find_slope <- function(data, y) {                   # takes in dataframe and specific y column
  model_data <- data %>%                                                         
    select(interview_age_yrs, all_of(y)) %>%        # keep x and y
    drop_na()                                       # drop NAs
  if (nrow(model_data) < 2) return(NA_real_)        # return NA in the column if insufficent data to calculate slopes
  model <- lm(reformulate("interview_age_yrs", response = y), data = model_data)   # calculate linear model
  coef(model)[["interview_age_yrs"]]                # pull slope value from linear model
}

# define function to get intercepts
find_intercept <- function(data, y) {                   # takes in dataframe and specific y column
  model_data <- data %>%                                                         
    select(interview_age_yrs, all_of(y)) %>%        # keep x and y
    drop_na()                                       # drop NAs
  if (nrow(model_data) < 2) return(NA_real_)        # return NA in the column if insufficent data to calculate a linear model
  model <- lm(reformulate("interview_age_yrs", response = y), data = model_data)   # calculate linear model
  coef(model)[[1]]                # pull intercept value from linear model
}

# Calculate slopes and intercepts for onsets of interest
arche_slopes <- pds_sex %>%
  group_by(participant_id) %>%
  group_modify(~ {
    tibble(
      gonadarche_slope = find_slope(.x, "gonadarche_pds"),
      gonadarche_intercept = find_intercept(.x, "gonadarche_pds")
    )
  })

# Calculate Onset Ages: Age where PDS = 2.0
arche_slopes <- arche_slopes %>%
  mutate(gonadarche_age = (2.0 - gonadarche_intercept) / gonadarche_slope)

#################
# IN THE BELOW CODE BLOCK
# Remove Ages and Slopes based on faulty self-report data
#################

# isolate gonadarche ages/tempos and rename gonadarche tempos
age_tempo <- arche_slopes %>%
  select(participant_id, 
         gonadarche_age, gonadarche_slope) %>%
  rename(gonadarche_tempo = gonadarche_slope)

# merge in decreasing counts
age_tempo <- age_tempo %>%
  left_join(pds_decreasing, by = 'participant_id')

# flag if any relevant self reports indicate reverse puberty
age_tempo <- age_tempo %>%
  mutate(Has_Decrease_Y1N0 = if_else(num_gonad_decreasing > 0, 1, 0))

## COUNT EXCLUSIONS
age_tempo <- age_tempo %>% left_join(sex, by = 'participant_id')

table(age_tempo$Has_Decrease_Y1N0, age_tempo$sex)
#     F    M     total: 11376
# 0 3659 2037  - 5696 subjects with valid Gonadarche data
# 1 1717 3963  - 5680 subjects excluded for inconsistent reporting/reported pubertal regression

## Set relevant exclusions to NA
age_tempo <- age_tempo %>%
  rowwise() %>%
  mutate(gonadarche_age = if_else(Has_Decrease_Y1N0 == 1, NA, gonadarche_age),
         gonadarche_tempo = if_else(Has_Decrease_Y1N0 == 1, NA, gonadarche_tempo))

# count
age_tempo %>%  filter(!is.na(gonadarche_age)) %>%  distinct(participant_id) %>%  nrow()    # 11375 -> 5696
age_tempo %>%  filter(!is.na(gonadarche_tempo)) %>%  distinct(participant_id) %>%  nrow()  # 11376 -> 5696

#################
# IN THE BELOW CODE BLOCK
# Clean and Organize
#################

# isolate pds items and counts
pubertal_items <- pds_sex %>%
  select(participant_id, session_id, sex, interview_age_yrs,
         thelarche, menarche, voice_deep, facial_hair, gonadarche_pds)

# merge items and ages
pubertal_timing <- pubertal_items %>%
  select(-sex) %>%
  left_join(age_tempo, by = "participant_id")

# reorder for clarity and readability
pubertal_timing <- pubertal_timing %>%
  select(participant_id, session_id, sex, interview_age_yrs,
         gonadarche_age, gonadarche_tempo,
         everything())

#################
# IN THE BELOW CODE BLOCK
# Validity-Check Data
#################

### AGES

# count Inf's - Ages would be calculated as "infinite" if there is no pubertal progress, i.e. a score of 2 at every report
sum(is.infinite(pubertal_timing$gonadarche_age)) # 251 rows
sum(pubertal_timing$gonadarche_tempo == 0, na.rm = TRUE) # 251 rows
inf_count <- pubertal_timing %>% select(participant_id, gonadarche_age, sex) %>% filter(is.infinite(gonadarche_age)) %>% unique() 
inf_count %>% distinct(participant_id) %>% nrow() # 53 subjects have "infinite" gonadarche ages, indicating no pubertal progress
inf_count <- pubertal_timing %>% select(participant_id, gonadarche_tempo, sex) %>% filter(gonadarche_tempo == 0) %>% unique() 
inf_count %>% distinct(participant_id) %>% nrow() # 53 subjects have flat gonadarche tempos, indicating no pubertal progress
table(inf_count$sex) # 26 F / 27 M reporting no pubertal progress

# set Inf's to NA for Age, and 0s to NA for slope (these mathematically co-occur for subjects, when there is no pubertal progression at all)
pubertal_timing <- pubertal_timing %>%
  mutate(gonadarche_age = if_else(is.infinite(gonadarche_age) | is.nan(gonadarche_age), NA, gonadarche_age)) %>%
  mutate(gonadarche_tempo = if_else(gonadarche_tempo == 0, NA, gonadarche_tempo)) 
# just as puberty does not go in reverse, it does not stagnate

# check age mean
mean(pubertal_timing$gonadarche_age, na.rm = TRUE)

# count before resets/exclusions
pubertal_timing %>%  filter(!is.na(gonadarche_age)) %>%  distinct(participant_id) %>%  nrow()    # 5696 -> 5643, 53 subjects dropped
pubertal_timing %>%  filter(!is.na(gonadarche_tempo)) %>%  distinct(participant_id) %>%  nrow()  # 5696 -> 5643, 53 subjects dropped

# count sex
subject_counter <- pubertal_timing %>% filter(!is.na(gonadarche_age)) %>% select(participant_id, gonadarche_age, sex) %>% unique()
table(subject_counter$sex) # 3633 F, 2010 M ; 5643 total subjects remain


### Reset Age at Gonadarche, Gonadarche, and General Pubertal Onsets to NA if ages are negative or have not yet occurred.
# In an effort to remove impossible and unconfirmed pubertal onset ages, 
# those occurring before birth or in the future (in relation to inteview age) are reset to NA.

# NEGATIVE AGES
# count negative and post-interview ages
subject_counter <- pubertal_timing %>% filter(gonadarche_age < 0) %>% select(participant_id, gonadarche_age, sex) %>% unique()
table(subject_counter$sex) # 100 F, 54 M; 154 total subjects have negative calculated Gonadarche ages

# reset to NA
pubertal_timing <- pubertal_timing %>%
  mutate(gonadarche_age = if_else(gonadarche_age < 0, NA, gonadarche_age))

# check age means
mean(pubertal_timing$gonadarche_age, na.rm = TRUE)

# count after resets/exclusions
pubertal_timing %>% filter(!is.na(gonadarche_age)) %>% distinct(participant_id) %>% nrow() # 5643 -> 5489; 154 removed; 100 F, 54 M

# AGES IMPUTED TO BE AFTER LAST INTERVIEW
subject_counter <- pubertal_timing %>% filter(gonadarche_age > interview_age_yrs) %>% select(participant_id, sex) %>% unique()
table(subject_counter$sex) # 2991 F, 1901 M subjects affected (not all will be excluded, some may have a later report after their calculated gonadarche age)

# count M/F before
subject_counter <- pubertal_timing %>% filter(!is.na(gonadarche_age)) %>% select(participant_id, sex) %>% unique()
table(subject_counter$sex) # 3533 F, 1956 M; 5489

# reset to NA
pubertal_timing <- pubertal_timing %>%
  mutate(gonadarche_age = if_else(gonadarche_age > interview_age_yrs, NA, gonadarche_age))

# count M/F after
subject_counter <- pubertal_timing %>% filter(!is.na(gonadarche_age)) %>% select(participant_id, sex) %>% unique()
table(subject_counter$sex) # 3533 -> 3350 F, 1956 -> 1619 M; 183F / 337M subjects removed for their imputed age of Gonadarche being after their last age at interview

# count subjects remaining
pubertal_timing %>% filter(!is.na(gonadarche_age)) %>% distinct(participant_id) %>% nrow()       # 5489 -> 4969; 520 total subjects removed
# 183 + 337 = 520

### TEMPOS
# Negative Tempos
# count before resets/exclusionss
pubertal_timing %>%  filter(!is.na(gonadarche_tempo)) %>%  distinct(participant_id) %>% nrow() # 5643 total subjects with tempos remaining

# count negative tempos
pubertal_timing %>% filter(gonadarche_tempo < 0) %>% distinct(participant_id) %>% nrow() # 90 subjects with negative tempos

# count M/F before
subject_counter <- pubertal_timing %>% filter(!is.na(gonadarche_tempo)) %>% select(participant_id, sex) %>% unique()
table(subject_counter$sex) # 3633 F, 2010 M

# Reset Tempos to NA if they are negative, which would indicate reverse puberty
pubertal_timing <- pubertal_timing %>%
  mutate(gonadarche_tempo = if_else(gonadarche_tempo < 0, NA, gonadarche_tempo))

# count M/F after
subject_counter <- pubertal_timing %>% filter(!is.na(gonadarche_tempo)) %>% select(participant_id, sex) %>% unique()
table(subject_counter$sex) # 3596 F, 1957 M; removed for negative tempos indicating pubertal regression: 37 F, 53 M

# count after resets/exclusions
pubertal_timing %>% filter(!is.na(gonadarche_tempo)) %>%  distinct(participant_id) %>% nrow() # 5643 -> 5553; 90 removed

#################
# IN THE BELOW CODE BLOCK
# Make Flags for Post-Gonadarche at first report and Pre-Gonadarche at last report
#################

## Gonadarche at First Report
# If first report had a PDS score of 2.0 or greater, flag as post gonadarche at first report
pubertal_timing <- pubertal_timing %>%
  group_by(participant_id) %>%
  mutate(first_gonad_pds = na.omit(gonadarche_pds)[1]) %>%
  mutate(PostGonadarche_FirstReport_Y1N0 = if_else(first_gonad_pds >= 2.0, 1, 0))

# PrePubertal at Last Report
pubertal_timing <- pubertal_timing %>%
  group_by(participant_id) %>%
  mutate(last_gonad_pds = gonadarche_pds[max(which(!is.na(gonadarche_pds)))]) %>%
  mutate(PreGonadarche_Y1N0 = if_else(last_gonad_pds < 2.0, 1, 0))

# Sanity check -- is anyone both?
table(pubertal_timing$PostGonadarche_FirstReport_Y1N0, pubertal_timing$PreGonadarche_Y1N0)
# yes -- but none of them have valid tempos or ages remaining

#################
# IN THE BELOW CODE BLOCK
# Clean & Organize
#################

# keep only useful columns
gonadarche <- pubertal_timing %>%
  select(participant_id, session_id, sex, interview_age_yrs, gonadarche_age, gonadarche_tempo, 
         gonadarche_pds, thelarche, menarche, voice_deep, facial_hair, num_gonad_decreasing, Has_Decrease_Y1N0, PostGonadarche_FirstReport_Y1N0, PreGonadarche_Y1N0)

#################
# IN THE BELOW CODE BLOCK
# Make Dataframe that removes exclusions and get counts
#################

# count subjects
counter <- gonadarche %>% filter(!is.na(gonadarche_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 3350 F / 1619 M; 4969 ages remain
counter <- gonadarche %>% filter(!is.na(gonadarche_tempo)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 3596 F / 1957 M; 5553 ages remain

# has a relevant decreasing value/inconsistently reported (already reset to NA, shouldn't change counts)
Gonadarche_Exclude <- gonadarche %>%
  mutate(gonadarche_age = if_else(Has_Decrease_Y1N0 == 1, NA, gonadarche_age)) %>% 
  mutate(gonadarche_tempo = if_else(Has_Decrease_Y1N0 == 1, NA, gonadarche_tempo))

# count subjects
counter <- Gonadarche_Exclude %>% filter(!is.na(gonadarche_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 3350 F / 1619 M; 4969 ages remain
counter <- Gonadarche_Exclude %>% filter(!is.na(gonadarche_tempo)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 3596 F / 1957 M; 5553 ages remain

### TEMPOS will not be excluded for being Post-Gonadarche at First Report or Pre-Gonadarche at Last Report.
###        Even if they do not experience a Gonadarche transition during the course of data collection, the rate of pubertal change is still valid.
### AGES will be excluded for these cases -- imputed ages are not valid if the subject has not yet experienced the event to which they refer
###                                       -- imputed ages are not valid if we cannot determine the age just prior to the occurrence of the event.

# post at first report
Gonadarche_Exclude <- Gonadarche_Exclude %>%
  mutate(gonadarche_age = if_else(PostGonadarche_FirstReport_Y1N0 == 1, NA, gonadarche_age)) 

# count subjects
counter <- Gonadarche_Exclude %>% filter(!is.na(gonadarche_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 2794 F / 1535 M; 4969 -> 4329 ages remain; 556 F / 84 M removed; 640 total

# pre at last report
Gonadarche_Exclude <- Gonadarche_Exclude %>%
  mutate(gonadarche_age = if_else(PreGonadarche_Y1N0 == 1, NA, gonadarche_age))

# count subjects
counter <- Gonadarche_Exclude %>% filter(!is.na(gonadarche_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 2788 F / 1535 M; 4329 -> 4323 ages remain; 6 F removed, 6 total

#################
# IN THE BELOW CODE BLOCK
# Make Z-Scores of Age and Tempo, merge in
#################

# must be split off and have 1 row per subject, so distributions are accurate when calculating Z-scores
age_zscore_maker <- Gonadarche_Exclude %>%
  select(participant_id, gonadarche_age) %>%
  unique() %>%
  filter(!is.na(gonadarche_age))
tempo_zscore_maker <- Gonadarche_Exclude %>%
  select(participant_id, gonadarche_tempo) %>%
  unique() %>%
  filter(!is.na(gonadarche_tempo))

# calculate z-scores
age_zscore_maker$gonadarche_age_zscore <- as.numeric(scale(age_zscore_maker$gonadarche_age))
tempo_zscore_maker$gonadarche_tempo_zscore <- as.numeric(scale(tempo_zscore_maker$gonadarche_tempo))

# remove superfluous column before merging back in
age_zscore_maker <- age_zscore_maker %>% select(-gonadarche_age)
tempo_zscore_maker <- tempo_zscore_maker %>% select(-gonadarche_tempo)

# merge in
Gonadarche_Exclude <- Gonadarche_Exclude %>%
  left_join(age_zscore_maker, by = 'participant_id') %>%
  left_join(tempo_zscore_maker, by = 'participant_id')

# Reorganize for export
Gonadarche_Exclude <- Gonadarche_Exclude %>%
  select(participant_id, session_id, sex, interview_age_yrs, gonadarche_age, gonadarche_age_zscore, gonadarche_tempo, gonadarche_tempo_zscore, everything())

#################
# IN THE BELOW CODE BLOCK
# Export to CSV
#################

write_csv(Gonadarche_Exclude, paste0(derivative_data, "Gonadarche_6.0_Age_Tempo_2_23_26.csv"))
