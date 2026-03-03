################
# Pubertal_Onsets_Tempos_6.0_WC.R
# Robert Toms, robert.toms@utdallas.edu
# 2/23/2026
################

# The purpose of this code is to calculate age at General Pubertal Onset and Pubertal Tempo in males and females.

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

# count 999s and 777s
table(pds_sex$growth_spurt == 999, pds_sex$sex) #5577; 2986 F / 2591 M
table(pds_sex$body_hair == 999, pds_sex$sex) #859; 460 F / 399 M
table(pds_sex$skin_pimples == 999, pds_sex$sex) #1996; 885 F / 1111 M
table(pds_sex$thelarche == 999, pds_sex$sex) # 1132; 1132 F
table(pds_sex$menarche == 999, pds_sex$sex) #680; 680 F
table(pds_sex$voice_deep == 999, pds_sex$sex) #506; 506 M
table(pds_sex$facial_hair == 999, pds_sex$sex) #402; 402 M
# 5577+859+1996+1132+680+506+402 = 11152

pds_999s <- pds_sex %>% filter(pds_sex$growth_spurt == 999 | pds_sex$body_hair == 999 | pds_sex$skin_pimples == 999 | pds_sex$thelarche == 999 | pds_sex$menarche == 999 | pds_sex$voice_deep == 999 | pds_sex$facial_hair == 999)
table(pds_999s$sex) # 3877 F / 3408 M
subs_999 <- pds_999s %>% distinct(participant_id) %>% pull(participant_id) # 7285 timepoints, 6303 subjects affected
sex_999 <- pds_999s %>% select(participant_id, sex) %>% unique()
table(sex_999$sex) # 3313 F / 2990 M

table(pds_sex$growth_spurt == 777, pds_sex$sex) #588; 296 F / 292 M
table(pds_sex$body_hair == 777, pds_sex$sex) #985; 610 F / 375 M
table(pds_sex$skin_pimples == 777, pds_sex$sex) #507; 214 F / 293 M
table(pds_sex$thelarche == 777, pds_sex$sex) #923; 923 F
table(pds_sex$menarche == 777, pds_sex$sex) #1049; 1049 F
table(pds_sex$voice_deep == 777, pds_sex$sex) #186; 186 M
table(pds_sex$facial_hair == 777, pds_sex$sex) #231; 231 M
# 588+985+507+923+1049+186+231 = 4469

pds_777s <- pds_sex %>% filter(pds_sex$growth_spurt == 777 | pds_sex$body_hair == 777 | pds_sex$skin_pimples == 777 | pds_sex$thelarche == 777 | pds_sex$menarche == 777 | pds_sex$voice_deep == 777 | pds_sex$facial_hair == 777)
table(pds_777s$sex) # 1906 F / 833 M
subs_777 <- pds_777s %>% distinct(participant_id) %>% pull(participant_id) # 2739 timepoints, 1838 subjects affected
sex_777 <- pds_777s %>% select(participant_id, sex) %>% unique()
table(sex_777$sex) # 1204 F / 634 M

# Count subjects
subjects_counter <- pds_sex %>% distinct(participant_id) %>% pull(participant_id) 

# 2 subjects missing, who and why?
all_subjects <- sex %>% distinct(participant_id) %>% pull(participant_id) 
missing <- setdiff(all_subjects, subjects_counter)
# sub-C2ZW147D -- male, no parent or youth pds data, only baseline data in ab_g_dyn
# sub-V4AXRCZ6 -- male, no youth pds data, only baseline parent pds and ab_g_dyn data

# Set 999s and 777s to NA, per Herting et al., 2021 best practices
pds_sex <- pds_sex %>%
  mutate(across(c(growth_spurt, 
                  body_hair, 
                  skin_pimples, 
                  thelarche, 
                  menarche, 
                  voice_deep, 
                  facial_hair,
                  ABCD_PDS,
                  male_PDS,
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

# Set premenarche to 1, postmenarche to 4 for PDS calculation
# reset Menarche values from 1 -> 4 (postmenarche) and 0 -> 1 (premenarche) to calculate PDS values
pds_sex <- pds_sex %>%
  mutate(menarche = case_when(
    menarche == 1 ~ 4,
    menarche == 0 ~ 1))

# calculate PDS values
pds_sex <- pds_sex %>%
  rowwise() %>%
  # General PDS
  mutate(manual_pds = case_when(
    sex == 'F' ~ sum(growth_spurt, body_hair, skin_pimples, thelarche, menarche, na.rm = TRUE),
    sex == 'M' ~ sum(growth_spurt, body_hair, skin_pimples, voice_deep, facial_hair, na.rm = TRUE))) %>%
  mutate(num_items = sum(!is.na(growth_spurt),
                         !is.na(body_hair),
                         !is.na(skin_pimples),
                         !is.na(thelarche),
                         !is.na(menarche),
                         !is.na(voice_deep),
                         !is.na(facial_hair))) %>%
  mutate(manual_pds = manual_pds / num_items) %>%
  mutate(manual_pds = if_else(is.nan(manual_pds), NA, manual_pds))

# merge male/female ABCD precalculated values into the same column
pds_sex <- pds_sex %>%
  mutate(ABCD_PDS = if_else(is.na(ABCD_PDS) & sex == 'M', male_PDS, ABCD_PDS)) %>%
  select(-male_PDS)

#################
# IN THE BELOW CODE BLOCK
# Count subjects with no/insufficient data
#################

# Manually Calculated PDS
pds_counts <- pds_sex %>% group_by(participant_id) %>% mutate(num_timepoints = sum(!is.na(manual_pds))) %>% ungroup() %>% select(participant_id, num_timepoints, sex) %>% unique()
table(pds_counts$num_timepoints, pds_counts$sex) # 1 subject (male) with no manual pds data; 330 with 1 timepoint, 180 F / 150 M

subject_counts <- pds_counts %>% filter(num_timepoints > 1)
table(subject_counts$sex) # 5498 F / 6037 M; 11535 total remain

# ABCD Precalculated PDS
pds_counts <- pds_sex %>% group_by(participant_id) %>% mutate(num_timepoints = sum(!is.na(ABCD_PDS))) %>% ungroup() %>% select(participant_id, num_timepoints, sex) %>% unique()
table(pds_counts$num_timepoints, pds_counts$sex) # 85 subjects (52 F / 33 M) with no ABCD precalculated pds data; 372 with 1 timepoint, 212 F / 160 M

subject_counts <- pds_counts %>% filter(num_timepoints > 1)
table(subject_counts$sex) # 5414 F / 5995 M; 11409 total remain

#################
# IN THE BELOW CODE BLOCK
# Determine Categories with Inconsistent Reports
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

# isolate male pds reports
pds_decreasing <- pds_sex %>%
  select(participant_id, session_id, growth_spurt, body_hair, skin_pimples, thelarche, menarche, voice_deep, facial_hair)

# make list values for is_inconsistent to iterate thru
pds_decreasing <- pds_decreasing %>%
  group_by(participant_id) %>%
  mutate(growth_values = list(na.omit(growth_spurt)),
         body_values = list(na.omit(body_hair)),
         skin_values = list(na.omit(skin_pimples)),
         voice_values = list(na.omit(voice_deep)),
         facial_values = list(na.omit(facial_hair)),
         thelarche_values = list(na.omit(thelarche)),
         menarche_values = list(na.omit(menarche)))

# apply determine_decreasing function
pds_decreasing <- pds_decreasing %>%
  rowwise() %>%
  mutate(growth_decreasing_Y1N0 = determine_decreasing(growth_values),
         body_decreasing_Y1N0 = determine_decreasing(body_values),
         skin_decreasing_Y1N0 = determine_decreasing(skin_values),
         thelarche_decreasing_Y1N0 = determine_decreasing(thelarche_values),
         menarche_decreasing_Y1N0 = determine_decreasing(menarche_values),
         voice_decreasing_Y1N0 = determine_decreasing(voice_values),
         facial_decreasing_Y1N0 = determine_decreasing(facial_values))

# count inconsistencies in pubertal categories
pds_decreasing <- pds_decreasing %>%
  mutate(num_general_decreasing = sum(growth_decreasing_Y1N0, body_decreasing_Y1N0, skin_decreasing_Y1N0, 
                                      thelarche_decreasing_Y1N0, menarche_decreasing_Y1N0,
                                      voice_decreasing_Y1N0, facial_decreasing_Y1N0, na.rm = TRUE))

# keep only useful columns
pds_decreasing <- pds_decreasing %>%
  select(participant_id, num_general_decreasing) %>%
  unique()

#################
# IN THE BELOW CODE BLOCK
# Calculate Age at General Pubertal onset
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
      ABCD_slope = find_slope(.x, "ABCD_PDS"),
      ABCD_intercept = find_intercept(.x, "ABCD_PDS"),
      manual_slope = find_slope(.x, "manual_pds"),
      manual_intercept = find_intercept(.x, "manual_pds")
    )
  })

# Calculate Onset Ages: Age where PDS = 2.0
arche_slopes <- arche_slopes %>%
  mutate(ABCD_pubertal_age = (2.0 - ABCD_intercept) / ABCD_slope) %>%
  mutate(manual_pubertal_age = (2.0 - manual_intercept) / manual_slope)

#################
# IN THE BELOW CODE BLOCK
# Remove Ages and Slopes based on faulty self-report data
#################

# isolate ages and rename tempos for each pubertal category
age_tempo <- arche_slopes %>%
  select(participant_id,
         manual_pubertal_age, manual_slope,
         ABCD_pubertal_age, ABCD_slope) %>%
  rename(manual_pubertal_tempo = manual_slope,
         ABCD_pubertal_tempo = ABCD_slope)

# merge in decreasing counts
age_tempo <- age_tempo %>%
  left_join(pds_decreasing, by = 'participant_id')

# flag if any relevant self reports indicate reverse puberty
age_tempo <- age_tempo %>%
  mutate(Has_Decrease_Y1N0 = if_else(num_general_decreasing > 0, 1, 0))

## COUNT EXCLUSIONS
age_tempo <- age_tempo %>% 
  mutate(has_manual = if_else(!is.na(manual_pubertal_age), 'has manual', 'no manual score')) %>% 
  mutate(has_abcd = if_else(!is.na(ABCD_pubertal_age), 'has abcd', 'no abcd score'))

age_tempo <- age_tempo %>% left_join(sex, by = 'participant_id')

# sub-K808H4UV has NaN ages and 0 slope, 3 decreases, reset to "has_manual" and "has abcd" for counts, will get excluded
age_tempo <- age_tempo %>%
  mutate(has_manual = if_else(participant_id == 'sub-K808H4UV', 'has manual', has_manual),
         has_abcd = if_else(participant_id == 'sub-K808H4UV', 'has abcd', has_abcd))

table(age_tempo$Has_Decrease_Y1N0, age_tempo$sex, age_tempo$has_manual)
# HAS MANUAL DATA
#     F    M     total: 11535
# 0 1129 520  - 1649 subjects with valid manual pubertal data
# 1 4369 5517  - 9886 subjects excluded for inconsistent reporting/reported pubertal regression

table(age_tempo$Has_Decrease_Y1N0, age_tempo$sex, age_tempo$has_abcd)
# HAS ABCD DATA
#     F    M     total: 11409
# 0 1076 494  - 1570 subjects with valid abcd pubertal data
# 1 4338 5501  - 9839 subjects excluded for inconsistent reporting/reported pubertal regression

## Set relevant exclusions to NA
age_tempo <- age_tempo %>%
  rowwise() %>%
  mutate(manual_pubertal_age = if_else(Has_Decrease_Y1N0 == 1, NA, manual_pubertal_age),
         manual_pubertal_tempo = if_else(Has_Decrease_Y1N0 == 1, NA, manual_pubertal_tempo),
         ABCD_pubertal_age = if_else(Has_Decrease_Y1N0 == 1, NA, ABCD_pubertal_age),
         ABCD_pubertal_tempo = if_else(Has_Decrease_Y1N0 == 1, NA, ABCD_pubertal_tempo))

#################
# IN THE BELOW CODE BLOCK
# Clean and Organize
#################

# isolate pds items and counts
pubertal_items <- pds_sex %>%
  select(participant_id, session_id, interview_age_yrs,
         growth_spurt, body_hair, skin_pimples, thelarche, menarche, voice_deep, facial_hair,
         manual_pds, ABCD_PDS, num_items)

# merge items and ages
pubertal_timing <- pubertal_items %>%
  left_join(age_tempo, by = "participant_id")

# reorder for clarity and readability
pubertal_timing <- pubertal_timing %>%
  select(participant_id, session_id, sex, interview_age_yrs,
         manual_pubertal_age, manual_pubertal_tempo,
         ABCD_pubertal_age, ABCD_pubertal_tempo,
         everything())

#################
# IN THE BELOW CODE BLOCK
# Validity-Check Data
#################

### AGES
# check age means
mean(pubertal_timing$manual_pubertal_age, na.rm = TRUE)
mean(pubertal_timing$ABCD_pubertal_age, na.rm = TRUE)

# count before resets/exclusions
pubertal_timing %>%  filter(!is.na(manual_pubertal_age)) %>%  distinct(participant_id) %>%  nrow()  # 11535 -> 1649 from excluding inconsistent reporters
pubertal_timing %>%  filter(!is.na(ABCD_pubertal_age)) %>%  distinct(participant_id) %>%  nrow()    # 11409 -> 1570 from excluding inconsistent reporters

# count sex
subject_counter <- pubertal_timing %>% filter(!is.na(manual_pubertal_age)) %>% select(participant_id, manual_pubertal_age, sex) %>% unique()
table(subject_counter$sex) # 1129 F, 520 M ; 1649 total subjects remain
subject_counter <- pubertal_timing %>% filter(!is.na(ABCD_pubertal_age)) %>% select(participant_id, ABCD_pubertal_age, sex) %>% unique()
table(subject_counter$sex) # 1076 F, 494 M ; 1570 total subjects remain

# Reset Age at General Pubertal Onset to NA if ages are negative or have not yet occurred.
# In an effort to remove impossible and unconfirmed pubertal onset ages, those occurring before birth or in the future (in relation to inteview age) are reset to NA.
# NEGATIVE AGES
pubertal_timing <- pubertal_timing %>%
  mutate(manual_pubertal_age = if_else(manual_pubertal_age < 0, NA, manual_pubertal_age),
         ABCD_pubertal_age = if_else(ABCD_pubertal_age < 0, NA, ABCD_pubertal_age))

# count after resets/exclusions
pubertal_timing %>%  filter(!is.na(manual_pubertal_age)) %>%  distinct(participant_id) %>%  nrow()  # 1649 -> 1640; 9 removed
pubertal_timing %>%  filter(!is.na(ABCD_pubertal_age)) %>%  distinct(participant_id) %>%  nrow()    # 1570 -> 1553; 17 removed

# count sex
subject_counter <- pubertal_timing %>% filter(!is.na(manual_pubertal_age)) %>% select(participant_id, manual_pubertal_age, sex) %>% unique()
table(subject_counter$sex) # 1121 F, 519 M ; 8 F / 1 M removed, 1640 total subjects remain
subject_counter <- pubertal_timing %>% filter(!is.na(ABCD_pubertal_age)) %>% select(participant_id, ABCD_pubertal_age, sex) %>% unique()
table(subject_counter$sex) # 1060 F, 493 M ; 16 F / 1 M removed, 1553 total subjects remain

# check age means
mean(pubertal_timing$manual_pubertal_age, na.rm = TRUE)
mean(pubertal_timing$ABCD_pubertal_age, na.rm = TRUE)

# AGES IMPUTED TO BE AFTER LAST INTERVIEW
pubertal_timing <- pubertal_timing %>%
  mutate(manual_pubertal_age = if_else(manual_pubertal_age > interview_age_yrs, NA, manual_pubertal_age),
         ABCD_pubertal_age = if_else(ABCD_pubertal_age > interview_age_yrs, NA, ABCD_pubertal_age))

# count after resets/exclusions
pubertal_timing %>%  filter(!is.na(manual_pubertal_age)) %>%  distinct(participant_id) %>%  nrow()  # 1640 -> 1536; 104 removed
pubertal_timing %>%  filter(!is.na(ABCD_pubertal_age)) %>%  distinct(participant_id) %>%  nrow()    # 1553 -> 1467; 86 removed

# count sex
subject_counter <- pubertal_timing %>% filter(!is.na(manual_pubertal_age)) %>% select(participant_id, manual_pubertal_age, sex) %>% unique()
table(subject_counter$sex) # 1077 F, 459 M ; 44 F / 60 M removed, 1536 total subjects remain
subject_counter <- pubertal_timing %>% filter(!is.na(ABCD_pubertal_age)) %>% select(participant_id, ABCD_pubertal_age, sex) %>% unique()
table(subject_counter$sex) # 1027 F, 440 M ; 33 F / 53 M removed, 1467 total subjects remain

# check age means
mean(pubertal_timing$manual_pubertal_age, na.rm = TRUE)
mean(pubertal_timing$ABCD_pubertal_age, na.rm = TRUE)



### TEMPOS

# count before resets/exclusions
pubertal_timing %>%  filter(!is.na(manual_pubertal_tempo)) %>%  distinct(participant_id) %>%  nrow()  # 1649
pubertal_timing %>%  filter(!is.na(ABCD_pubertal_tempo)) %>%  distinct(participant_id) %>%  nrow()    # 1570

# count sex
subject_counter <- pubertal_timing %>% filter(!is.na(manual_pubertal_tempo)) %>% select(participant_id, manual_pubertal_tempo, sex) %>% unique()
table(subject_counter$sex) # 1129 F, 520 M ; 1679 total subjects remain
subject_counter <- pubertal_timing %>% filter(!is.na(ABCD_pubertal_tempo)) %>% select(participant_id, ABCD_pubertal_tempo, sex) %>% unique()
table(subject_counter$sex) # 1076 F, 494 M ; 1570 total subjects remain

# Reset Tempos to NA if they are negative, which would indicate reverse puberty
pubertal_timing <- pubertal_timing %>%
  mutate(manual_pubertal_tempo = if_else(manual_pubertal_tempo < 0, NA, manual_pubertal_tempo),
         ABCD_pubertal_tempo = if_else(ABCD_pubertal_tempo < 0, NA, ABCD_pubertal_tempo))

# count after resets/exclusions
pubertal_timing %>%  filter(!is.na(manual_pubertal_tempo)) %>%  distinct(participant_id) %>%  nrow()  # 1649 -> 1639, -10
pubertal_timing %>%  filter(!is.na(ABCD_pubertal_tempo)) %>%  distinct(participant_id) %>%  nrow()    # 1570 -> 1563, -7

# count sex
subject_counter <- pubertal_timing %>% filter(!is.na(manual_pubertal_tempo)) %>% select(participant_id, manual_pubertal_tempo, sex) %>% unique()
table(subject_counter$sex) # 1122 F, 517 M ; 7 F / 3 M removed, 1679 total subjects remain
subject_counter <- pubertal_timing %>% filter(!is.na(ABCD_pubertal_tempo)) %>% select(participant_id, ABCD_pubertal_tempo, sex) %>% unique()
table(subject_counter$sex) # 1070 F, 493 M ; 6 F / 1 M removed, 1563 total subjects remain

#################
# IN THE BELOW CODE BLOCK
# Make Flags for Post-Puberty at baseline and pre-puberty at last report
#################

## Arche at First Report
# If first report had a PDS score of 2.0 or greater, flag as the appropriate Arche at first report
pubertal_timing <- pubertal_timing %>%
  group_by(participant_id) %>%
  # General
  mutate(first_manual_pds = na.omit(manual_pds)[1]) %>%
  mutate(PostPubertal_FirstReport_Y1N0 = if_else(first_manual_pds >= 2.0, 1, 0)) %>%
  # ABCD Precalculated General
  mutate(first_abcd_pds = na.omit(ABCD_PDS)[1]) %>%
  mutate(ABCD_PostPubertal_FirstReport_Y1N0 = if_else(first_abcd_pds >= 2.0, 1, 0))

# PrePubertal at Last Report
pubertal_timing <- pubertal_timing %>%
  group_by(participant_id) %>%
  # General
  mutate(last_manual_pds = manual_pds[max(which(!is.na(manual_pds)))]) %>%
  mutate(PrePubertal_Y1N0 = if_else(last_manual_pds < 2.0, 1, 0)) %>%
  # ABCD Precalculated General
  mutate(last_abcd_pds = manual_pds[max(which(!is.na(ABCD_PDS)))]) %>%
  mutate(ABCD_PrePubertal_Y1N0 = if_else(last_abcd_pds < 2.0, 1, 0))

#################
# IN THE BELOW CODE BLOCK
# Clean & Organize
#################

# keep useful columns
general <- pubertal_timing %>%
  select(participant_id, session_id, sex, interview_age_yrs, manual_pds, manual_pubertal_age, manual_pubertal_tempo, ABCD_PDS, ABCD_pubertal_age, ABCD_pubertal_tempo,
         growth_spurt, body_hair, skin_pimples, thelarche, menarche, voice_deep, facial_hair, num_general_decreasing,
         Has_Decrease_Y1N0, PostPubertal_FirstReport_Y1N0, PrePubertal_Y1N0, ABCD_PostPubertal_FirstReport_Y1N0, ABCD_PrePubertal_Y1N0)

#################
# IN THE BELOW CODE BLOCK
# Make Dataframe that removes exclusions and get counts
#################

### TEMPOS will not be excluded for being Post-Pubertal at First Report or Pre-Pubertal at Last Report.
###        Even if they do not experience a Pubertal transition during the course of data collection, the rate of pubertal change is still valid.
### AGES will be excluded for these cases -- imputed ages are not valid if the subject has not yet experienced the event to which they refer
###                                       -- imputed ages are not valid if we cannot determine the age just prior to the occurrence of the event.

### MANUAL PUBERTAL SCORES
# count subjects
counter <- general %>% filter(!is.na(manual_pubertal_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 1077 F / 459 M; 1536 ages remain
counter <- general %>% filter(!is.na(manual_pubertal_tempo)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 1122 F / 517 M; 1639 ages remain

# has a relevant decreasing value/inconsistently reported (already reset to NA, shouldn't change counts)
General_Exclude <- general %>% 
  mutate(manual_pubertal_age = if_else(Has_Decrease_Y1N0 == 1, NA, manual_pubertal_age)) %>% 
  mutate(manual_pubertal_tempo = if_else(Has_Decrease_Y1N0 == 1, NA, manual_pubertal_tempo))

# count subjects
counter <- General_Exclude %>% filter(!is.na(manual_pubertal_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 1077 F / 459 M; 1536 ages remain
counter <- General_Exclude %>% filter(!is.na(manual_pubertal_tempo)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 1122 F / 517 M; 1639 ages remain

# Post Pubertal at first report
General_Exclude <- General_Exclude %>% 
  mutate(manual_pubertal_age = if_else(PostPubertal_FirstReport_Y1N0 == 1, NA, manual_pubertal_age))

# count subjects
counter <- General_Exclude %>% filter(!is.na(manual_pubertal_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 865 F / 426 M; 245 (212F/33M) removed, 1291 ages remain

# pre at last report
General_Exclude <- General_Exclude %>% 
  mutate(manual_pubertal_age = if_else(PrePubertal_Y1N0 == 1, NA, manual_pubertal_age))

# count subjects
counter <- General_Exclude %>% filter(!is.na(manual_pubertal_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 863 F / 424 M; 4 (2F/2M) removed, 1287 ages remain


### ABCD PRECALCULATED PUBERTAL SCORES
# count subjects
counter <- general %>% filter(!is.na(ABCD_pubertal_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 1027 F / 440 M; 1467 ages remain
counter <- general %>% filter(!is.na(ABCD_pubertal_tempo)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 1070 F / 493 M; 1563 tempos remain

# has a relevant decreasing value/inconsistently reported (already reset to NA, shouldn't change counts)
ABCD_Exclude <- general %>% 
  mutate(ABCD_pubertal_age = if_else(Has_Decrease_Y1N0 == 1, NA, ABCD_pubertal_age)) %>% 
  mutate(ABCD_pubertal_tempo = if_else(Has_Decrease_Y1N0 == 1, NA, ABCD_pubertal_tempo))

# count subjects
counter <- ABCD_Exclude %>% filter(!is.na(ABCD_pubertal_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 1027 F / 440 M; 1467 ages remain
counter <- ABCD_Exclude %>% filter(!is.na(ABCD_pubertal_tempo)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 1070 F / 493 M; 1563 tempos remain

# post at first report
ABCD_Exclude <- ABCD_Exclude %>% 
  mutate(ABCD_pubertal_age = if_else(ABCD_PostPubertal_FirstReport_Y1N0 == 1, NA, ABCD_pubertal_age))

# count subjects
counter <- ABCD_Exclude %>% filter(!is.na(ABCD_pubertal_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 769 F / 401 M; 297 (258F/39M) removed, 1170 ages remain

# pre at last report
ABCD_Exclude <- ABCD_Exclude %>%
  mutate(ABCD_pubertal_age = if_else(ABCD_PrePubertal_Y1N0 == 1, NA, ABCD_pubertal_age))

# count subjects
counter <- ABCD_Exclude %>% filter(!is.na(ABCD_pubertal_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 766 F / 401 M; 3 (3F/0M) removed, 1167 ages remain


#################
# IN THE BELOW CODE BLOCK
# Make Z-Scores of Age and Tempo, merge in
#################

### MANUAL
# must be split off and have 1 row per subject, so distributions are accurate when calculating Z-scores
age_zscore_maker <- General_Exclude %>%
  select(participant_id, manual_pubertal_age) %>%
  unique() %>%
  filter(!is.na(manual_pubertal_age))
tempo_zscore_maker <- General_Exclude %>%
  select(participant_id, manual_pubertal_tempo) %>%
  unique() %>%
  filter(!is.na(manual_pubertal_tempo))

# calculate z-scores
age_zscore_maker$manual_pubertal_age_zscore <- as.numeric(scale(age_zscore_maker$manual_pubertal_age))
tempo_zscore_maker$manual_pubertal_tempo_zscore <- as.numeric(scale(tempo_zscore_maker$manual_pubertal_tempo))

# remove superfluous column before merging back in
age_zscore_maker <- age_zscore_maker %>% select(-manual_pubertal_age)
tempo_zscore_maker <- tempo_zscore_maker %>% select(-manual_pubertal_tempo)

# merge in
General_Exclude <- General_Exclude %>%
  left_join(age_zscore_maker, by = 'participant_id') %>%
  left_join(tempo_zscore_maker, by = 'participant_id')

# Reorganize for export
General_Exclude <- General_Exclude %>%
  select(participant_id, session_id, sex, interview_age_yrs, manual_pubertal_age, manual_pubertal_age_zscore, manual_pubertal_tempo, manual_pubertal_tempo_zscore, everything())


### ABCD PRECALCULATED
# must be split off and have 1 row per subject, so distributions are accurate when calculating Z-scores
age_zscore_maker <- ABCD_Exclude %>%
  select(participant_id, ABCD_pubertal_age) %>%
  unique() %>%
  filter(!is.na(ABCD_pubertal_age))
tempo_zscore_maker <- ABCD_Exclude %>%
  select(participant_id, ABCD_pubertal_tempo) %>%
  unique() %>%
  filter(!is.na(ABCD_pubertal_tempo))

# calculate z-scores
age_zscore_maker$ABCD_pubertal_age_zscore <- as.numeric(scale(age_zscore_maker$ABCD_pubertal_age))
tempo_zscore_maker$ABCD_pubertal_tempo_zscore <- as.numeric(scale(tempo_zscore_maker$ABCD_pubertal_tempo))

# remove superfluous column before merging back in
age_zscore_maker <- age_zscore_maker %>% select(-ABCD_pubertal_age)
tempo_zscore_maker <- tempo_zscore_maker %>% select(-ABCD_pubertal_tempo)

# merge in
ABCD_Exclude <- ABCD_Exclude %>%
  left_join(age_zscore_maker, by = 'participant_id') %>%
  left_join(tempo_zscore_maker, by = 'participant_id')

# Reorganize for export
ABCD_Exclude <- ABCD_Exclude %>%
  select(participant_id, session_id, sex, interview_age_yrs, ABCD_pubertal_age, ABCD_pubertal_age_zscore, ABCD_pubertal_tempo, ABCD_pubertal_tempo_zscore, everything())

#################
# IN THE BELOW CODE BLOCK
# Export to CSV
#################

write_csv(General_Exclude, paste0(derivative_data, "Manual_Puberty_6.0_Age_Tempo_2_23_26.csv"))
write_csv(ABCD_Exclude, paste0(derivative_data, "ABCD_Puberty_6.0_Age_Tempo_2_23_26.csv"))
