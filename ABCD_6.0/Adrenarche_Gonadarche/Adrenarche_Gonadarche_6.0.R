################
# Adrenarche_Gonadarche_6.0.R
# Robert Toms, robert.toms@utdallas.edu
# 10/27/2025
################

# The purpose of this code is to calculate age at Adrenarche, Gonadarche, and general pubertal onset in females.

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

# Set 999s and 777s to NA, per Herting et al., 2021 best practices
pds_sex <- pds_sex %>%
  mutate(across(c(growth_spurt, 
                  body_hair, 
                  skin_pimples, 
                  thelarche, 
                  menarche, 
                  voice_deep, 
                  facial_hair
  ), ~case_when(
    . == 999 ~ NA,
    . == 777 ~ NA,
    TRUE ~ .)))

# Make female- specific dataframe
female_pds <- pds_sex %>%
  filter(sex == "F") %>%
  select(-voice_deep, -facial_hair, -male_PDS)
male_pds <- pds_sex %>%
  filter(sex == "M") %>%
  select(-thelarche, -menarche, -ABCD_PDS) %>%
  rename(ABCD_PDS = male_PDS)

#################
# IN THE BELOW CODE BLOCK
# Manually Calculate PDS Values
#################

# Set premenarche to 1, postmenarche to 4 for PDS calculation
female_pds <- female_pds %>%
  mutate(menarche = case_when(
    menarche == 1 ~ 4,
    menarche == 0 ~ 1)) 

# revert n/a's to NA and set as numeric type
female_pds <- female_pds %>%
  mutate(thelarche = if_else(thelarche == "n/a", NA, as.numeric(thelarche))) %>%
  mutate(ABCD_PDS = if_else(ABCD_PDS == "n/a", NA, as.numeric(ABCD_PDS)))
male_pds <- male_pds %>%
  mutate(voice_deep = if_else(voice_deep == "n/a", NA, as.numeric(voice_deep))) %>%
  mutate(facial_hair = if_else(facial_hair == "n/a", NA, as.numeric(facial_hair))) %>%
  mutate(ABCD_PDS = if_else(ABCD_PDS == "n/a", NA, as.numeric(ABCD_PDS)))

# calculate PDS values
female_pds <- female_pds %>%
  rowwise() %>%
  # General PDS
  mutate(manual_pds = sum(growth_spurt, body_hair, skin_pimples, thelarche, menarche, na.rm = TRUE)) %>%
  mutate(num_items = sum(!is.na(growth_spurt),
                         !is.na(body_hair),
                         !is.na(skin_pimples),
                         !is.na(thelarche),
                         !is.na(menarche))) %>%
  mutate(manual_pds = manual_pds / num_items) %>%
  # Adrenarche PDS
  mutate(adrenarche_pds = sum(body_hair, skin_pimples)) %>%
  mutate(adrenarche_pds = adrenarche_pds / 2) %>%
  # Gonadarche PDS
  mutate(gonadarche_pds = sum(thelarche, menarche)) %>%
  mutate(gonadarche_pds = gonadarche_pds / 2)
male_pds <- male_pds %>%
  rowwise() %>%
  # General PDS
  mutate(manual_pds = sum(growth_spurt, body_hair, skin_pimples, voice_deep, facial_hair, na.rm = TRUE)) %>%
  mutate(num_items = sum(!is.na(growth_spurt),
                         !is.na(body_hair),
                         !is.na(skin_pimples),
                         !is.na(voice_deep),
                         !is.na(facial_hair))) %>%
  mutate(manual_pds = manual_pds / num_items) %>%
  # Adrenarche PDS
  mutate(adrenarche_pds = sum(body_hair, skin_pimples)) %>%
  mutate(adrenarche_pds = adrenarche_pds / 2) %>%
  # Gonadarche PDS
  mutate(gonadarche_pds = sum(voice_deep, facial_hair)) %>%
  mutate(gonadarche_pds = gonadarche_pds / 2)

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
m_decreasing <- male_pds %>%
  select(participant_id, session_id, growth_spurt, body_hair, skin_pimples, voice_deep, facial_hair)

# male list values for is_inconsistent to iterate thru
m_decreasing <- m_decreasing %>%
  group_by(participant_id) %>%
  mutate(growth_values = list(na.omit(growth_spurt)),
         body_values = list(na.omit(body_hair)),
         skin_values = list(na.omit(skin_pimples)),
         voice_values = list(na.omit(voice_deep)),
         facial_values = list(na.omit(facial_hair)))

# apply determine_decreasing function
m_decreasing <- m_decreasing %>%
  rowwise() %>%
  mutate(growth_decreasing_Y1N0 = determine_decreasing(growth_values),
         body_decreasing_Y1N0 = determine_decreasing(body_values),
         skin_decreasing_Y1N0 = determine_decreasing(skin_values),
         voice_decreasing_Y1N0 = determine_decreasing(voice_values),
         facial_decreasing_Y1N0 = determine_decreasing(facial_values))

# isolate female pds reports
f_decreasing <- female_pds %>%
  select(participant_id, session_id, growth_spurt, body_hair, skin_pimples, thelarche, menarche)

# male list values for is_inconsistent to iterate thru
f_decreasing <- f_decreasing %>%
  group_by(participant_id) %>%
  mutate(growth_values = list(na.omit(growth_spurt)),
         body_values = list(na.omit(body_hair)),
         skin_values = list(na.omit(skin_pimples)),
         thelarche_values = list(na.omit(thelarche)),
         menarche_values = list(na.omit(menarche)))

# apply determine_decreasing function
f_decreasing <- f_decreasing %>%
  rowwise() %>%
  mutate(growth_decreasing_Y1N0 = determine_decreasing(growth_values),
         body_decreasing_Y1N0 = determine_decreasing(body_values),
         skin_decreasing_Y1N0 = determine_decreasing(skin_values),
         thelarche_decreasing_Y1N0 = determine_decreasing(thelarche_values),
         menarche_decreasing_Y1N0 = determine_decreasing(menarche_values))


# count inconsistencies in pubertal categories
m_decreasing <- m_decreasing %>%
  mutate(num_gonad_decreasing = sum(voice_decreasing_Y1N0, facial_decreasing_Y1N0, na.rm = TRUE),
         num_adren_decreasing = sum(body_decreasing_Y1N0, skin_decreasing_Y1N0, na.rm = TRUE),
         num_general_decreasing = sum(growth_decreasing_Y1N0, body_decreasing_Y1N0, skin_decreasing_Y1N0, voice_decreasing_Y1N0, facial_decreasing_Y1N0, na.rm = TRUE))

f_decreasing <- f_decreasing %>%
  mutate(num_gonad_decreasing = sum(thelarche_decreasing_Y1N0, menarche_decreasing_Y1N0, na.rm = TRUE),
         num_adren_decreasing = sum(body_decreasing_Y1N0, skin_decreasing_Y1N0, na.rm = TRUE),
         num_general_decreasing = sum(growth_decreasing_Y1N0, body_decreasing_Y1N0, skin_decreasing_Y1N0, thelarche_decreasing_Y1N0, menarche_decreasing_Y1N0, na.rm = TRUE))

# keep only useful columns
m_decreasing <- m_decreasing %>%
  select(participant_id, num_gonad_decreasing, num_adren_decreasing, num_general_decreasing) %>%
  unique()
f_decreasing <- f_decreasing %>%
  select(participant_id, num_gonad_decreasing, num_adren_decreasing, num_general_decreasing) %>%
  unique()

#################
# IN THE BELOW CODE BLOCK
# Calculate Age at Adrenarche, Gonadarche, General Pubertal onset
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
fem_arche_slopes <- female_pds %>%
  group_by(participant_id) %>%
  group_modify(~ {
    tibble(
      ABCD_slope = find_slope(.x, "ABCD_PDS"),
      ABCD_intercept = find_intercept(.x, "ABCD_PDS"),
      manual_slope = find_slope(.x, "manual_pds"),
      manual_intercept = find_intercept(.x, "manual_pds"),
      adrenarche_slope = find_slope(.x, "adrenarche_pds"),
      adrenarche_intercept = find_intercept(.x, "adrenarche_pds"),
      gonadarche_slope = find_slope(.x, "gonadarche_pds"),
      gonadarche_intercept = find_intercept(.x, "gonadarche_pds")
    )
  })

male_arche_slopes <- male_pds %>%
  group_by(participant_id) %>%
  group_modify(~ {
    tibble(
      ABCD_slope = find_slope(.x, "ABCD_PDS"),
      ABCD_intercept = find_intercept(.x, "ABCD_PDS"),
      manual_slope = find_slope(.x, "manual_pds"),
      manual_intercept = find_intercept(.x, "manual_pds"),
      adrenarche_slope = find_slope(.x, "adrenarche_pds"),
      adrenarche_intercept = find_intercept(.x, "adrenarche_pds"),
      gonadarche_slope = find_slope(.x, "gonadarche_pds"),
      gonadarche_intercept = find_intercept(.x, "gonadarche_pds")
    )
  })

# Calculate Onset Ages: Age where PDS = 2.5
fem_arche_slopes <- fem_arche_slopes %>%
  mutate(ABCD_pubertal_age = (2.5 - ABCD_intercept) / ABCD_slope) %>%
  mutate(manual_pubertal_age = (2.5 - manual_intercept) / manual_slope) %>%
  mutate(adrenarche_age = (2.5 - adrenarche_intercept) / adrenarche_slope) %>%
  mutate(gonadarche_age = (2.5 - gonadarche_intercept) / gonadarche_slope)
male_arche_slopes <- male_arche_slopes %>%
  mutate(ABCD_pubertal_age = (2.5 - ABCD_intercept) / ABCD_slope) %>%
  mutate(manual_pubertal_age = (2.5 - manual_intercept) / manual_slope) %>%
  mutate(adrenarche_age = (2.5 - adrenarche_intercept) / adrenarche_slope) %>%
  mutate(gonadarche_age = (2.5 - gonadarche_intercept) / gonadarche_slope)

#################
# IN THE BELOW CODE BLOCK
# Remove Ages and Slopes based on faulty self-report data
#################

# isolate ages and rename tempos for each pubertal category
female_age_tempo <- fem_arche_slopes %>%
  select(participant_id, 
         adrenarche_age, adrenarche_slope,
         gonadarche_age, gonadarche_slope,
         manual_pubertal_age, manual_slope,
         ABCD_pubertal_age, ABCD_slope) %>%
  rename(adrenarche_tempo = adrenarche_slope,
         gonadarche_tempo = gonadarche_slope,
         manual_pubertal_tempo = manual_slope,
         ABCD_pubertal_tempo = ABCD_slope)
male_age_tempo <- male_arche_slopes %>%
  select(participant_id, 
         adrenarche_age, adrenarche_slope,
         gonadarche_age, gonadarche_slope,
         manual_pubertal_age, manual_slope,
         ABCD_pubertal_age, ABCD_slope) %>%
  rename(adrenarche_tempo = adrenarche_slope,
         gonadarche_tempo = gonadarche_slope,
         manual_pubertal_tempo = manual_slope,
         ABCD_pubertal_tempo = ABCD_slope)

# merge in decreasing counts
female_age_tempo <- female_age_tempo %>%
  left_join(f_decreasing, by = 'participant_id')
male_age_tempo <- male_age_tempo %>%
  left_join(m_decreasing, by = 'participant_id')

# flag if any relevant self reports indicate reverse puberty
female_age_tempo <- female_age_tempo %>%
  mutate(Exclude_Adren_Y1N0 = if_else(num_adren_decreasing > 0, 1, 0)) %>%
  mutate(Exclude_Gonad_Y1N0 = if_else(num_gonad_decreasing > 0, 1, 0)) %>%
  mutate(Exclude_General_Y1N0 = if_else(num_general_decreasing > 0, 1, 0))
male_age_tempo <- male_age_tempo %>%
  mutate(Exclude_Adren_Y1N0 = if_else(num_adren_decreasing > 0, 1, 0)) %>%
  mutate(Exclude_Gonad_Y1N0 = if_else(num_gonad_decreasing > 0, 1, 0)) %>%
  mutate(Exclude_General_Y1N0 = if_else(num_general_decreasing > 0, 1, 0))

## COUNT EXCLUSIONS
# total: 5678
table(female_age_tempo$Exclude_Adren_Y1N0)    # 3201 subjects excluded, 2477 usable
table(female_age_tempo$Exclude_Gonad_Y1N0)    # 1732 subjects excluded, 3946 usable
table(female_age_tempo$Exclude_General_Y1N0)  # 4369 subjects excluded, 1309 usable

# total: 6188
table(male_age_tempo$Exclude_Adren_Y1N0)   # 4271 subjects excluded, 1917 usable
table(male_age_tempo$Exclude_Gonad_Y1N0)   # 3966 subjects excluded, 2222 usable
table(male_age_tempo$Exclude_General_Y1N0) # 5517 subjects excluded, 671 usable

# total: 11866
# Adrenarche, 7472 subjects excluded, 4394 usable
# Gonadarche, 5698 subjects excluded, 6168 usable
# General, 9886 subjects excluded, 1980 usable

## Set relevant exclusions to NA
female_age_tempo <- female_age_tempo %>%
  rowwise() %>%
  mutate(adrenarche_age = if_else(Exclude_Adren_Y1N0 == 1, NA, adrenarche_age),
         adrenarche_tempo = if_else(Exclude_Adren_Y1N0 == 1, NA, adrenarche_tempo),
         gonadarche_age = if_else(Exclude_Gonad_Y1N0 == 1, NA, gonadarche_age),
         gonadarche_tempo = if_else(Exclude_Gonad_Y1N0 == 1, NA, gonadarche_tempo),
         manual_pubertal_age = if_else(Exclude_General_Y1N0 == 1, NA, manual_pubertal_age),
         manual_pubertal_tempo = if_else(Exclude_General_Y1N0 == 1, NA, manual_pubertal_tempo),
         ABCD_pubertal_age = if_else(Exclude_General_Y1N0 == 1, NA, ABCD_pubertal_age),
         ABCD_pubertal_tempo = if_else(Exclude_General_Y1N0 == 1, NA, ABCD_pubertal_tempo))
male_age_tempo <- male_age_tempo %>%
  rowwise() %>%
  mutate(adrenarche_age = if_else(Exclude_Adren_Y1N0 == 1, NA, adrenarche_age),
         adrenarche_tempo = if_else(Exclude_Adren_Y1N0 == 1, NA, adrenarche_tempo),
         gonadarche_age = if_else(Exclude_Gonad_Y1N0 == 1, NA, gonadarche_age),
         gonadarche_tempo = if_else(Exclude_Gonad_Y1N0 == 1, NA, gonadarche_tempo),
         manual_pubertal_age = if_else(Exclude_General_Y1N0 == 1, NA, manual_pubertal_age),
         manual_pubertal_tempo = if_else(Exclude_General_Y1N0 == 1, NA, manual_pubertal_tempo),
         ABCD_pubertal_age = if_else(Exclude_General_Y1N0 == 1, NA, ABCD_pubertal_age),
         ABCD_pubertal_tempo = if_else(Exclude_General_Y1N0 == 1, NA, ABCD_pubertal_tempo))

#################
# IN THE BELOW CODE BLOCK
# Clean and Organize
#################

# isolate pds items and counts
fem_pubertal_items <- female_pds %>%
  select(participant_id, session_id, sex, interview_age_yrs,
         growth_spurt, body_hair, skin_pimples, thelarche, menarche,
         adrenarche_pds, gonadarche_pds, manual_pds, ABCD_PDS, num_items)
male_pubertal_items <- male_pds %>%
  select(participant_id, session_id, sex, interview_age_yrs,
         growth_spurt, body_hair, skin_pimples, voice_deep, facial_hair,
         adrenarche_pds, gonadarche_pds, manual_pds, ABCD_PDS, num_items)

# merge items and ages
fem_pubertal_timing <- fem_pubertal_items %>%
  left_join(female_age_tempo, by = "participant_id")
male_pubertal_timing <- male_pubertal_items %>%
  left_join(male_age_tempo, by = "participant_id")

# reorder for clarity and readability
fem_pubertal_timing <- fem_pubertal_timing %>%
  select(participant_id, session_id, sex, interview_age_yrs,
         adrenarche_age, adrenarche_tempo,
         gonadarche_age, gonadarche_tempo,
         manual_pubertal_age, manual_pubertal_tempo,
         ABCD_pubertal_age, ABCD_pubertal_tempo,
         everything())
male_pubertal_timing <- male_pubertal_timing %>%
  select(participant_id, session_id, sex, interview_age_yrs,
         adrenarche_age, adrenarche_tempo,
         gonadarche_age, gonadarche_tempo,
         manual_pubertal_age, manual_pubertal_tempo,
         ABCD_pubertal_age, ABCD_pubertal_tempo,
         everything())

#################
# IN THE BELOW CODE BLOCK
# Validity-Check Data
#################

### AGES

# set Inf's to NA
fem_pubertal_timing <- fem_pubertal_timing %>%
  mutate(adrenarche_age = if_else(is.infinite(adrenarche_age), NA, adrenarche_age),
         gonadarche_age = if_else(is.infinite(gonadarche_age), NA, gonadarche_age))
male_pubertal_timing <- male_pubertal_timing %>%
  mutate(adrenarche_age = if_else(is.infinite(adrenarche_age), NA, adrenarche_age),
         gonadarche_age = if_else(is.infinite(gonadarche_age), NA, gonadarche_age))

# check age means
mean(fem_pubertal_timing$adrenarche_age, na.rm = TRUE)
mean(fem_pubertal_timing$gonadarche_age, na.rm = TRUE)
mean(fem_pubertal_timing$manual_pubertal_age, na.rm = TRUE)
mean(fem_pubertal_timing$ABCD_pubertal_age, na.rm = TRUE)
mean(male_pubertal_timing$adrenarche_age, na.rm = TRUE)
mean(male_pubertal_timing$gonadarche_age, na.rm = TRUE)
mean(male_pubertal_timing$manual_pubertal_age, na.rm = TRUE)
mean(male_pubertal_timing$ABCD_pubertal_age, na.rm = TRUE)

# count before resets/exclusions
fem_pubertal_timing %>%  filter(!is.na(adrenarche_age)) %>%  distinct(participant_id) %>%  nrow()       # 2236
fem_pubertal_timing %>%  filter(!is.na(gonadarche_age)) %>%  distinct(participant_id) %>%  nrow()       # 3633
fem_pubertal_timing %>%  filter(!is.na(manual_pubertal_age)) %>%  distinct(participant_id) %>%  nrow()  # 1129
fem_pubertal_timing %>%  filter(!is.na(ABCD_pubertal_age)) %>%  distinct(participant_id) %>%  nrow()    # 1076
male_pubertal_timing %>%  filter(!is.na(adrenarche_age)) %>%  distinct(participant_id) %>%  nrow()       # 1713
male_pubertal_timing %>%  filter(!is.na(gonadarche_age)) %>%  distinct(participant_id) %>%  nrow()       # 2010
male_pubertal_timing %>%  filter(!is.na(manual_pubertal_age)) %>%  distinct(participant_id) %>%  nrow()  # 520
male_pubertal_timing %>%  filter(!is.na(ABCD_pubertal_age)) %>%  distinct(participant_id) %>%  nrow()    # 494

# Reset Age at Adrenarche, Gonadarche, and General Pubertal Onsets to NA if ages are negative or have not yet occurred.
# In an effort to remove impossible and unconfirmed pubertal onset ages, those occurring before birth or in the future (in relation to inteview age) are reset to NA.
fem_pubertal_timing <- fem_pubertal_timing %>%
  mutate(adrenarche_age = if_else(adrenarche_age < 0 | adrenarche_age > interview_age_yrs, NA, adrenarche_age),
         gonadarche_age = if_else(gonadarche_age < 0 | gonadarche_age > interview_age_yrs, NA, gonadarche_age),
         manual_pubertal_age = if_else(manual_pubertal_age < 0 | manual_pubertal_age > interview_age_yrs, NA, manual_pubertal_age),
         ABCD_pubertal_age = if_else(ABCD_pubertal_age < 0 | ABCD_pubertal_age > interview_age_yrs, NA, ABCD_pubertal_age))
male_pubertal_timing <- male_pubertal_timing %>%
  mutate(adrenarche_age = if_else(adrenarche_age < 0 | adrenarche_age > interview_age_yrs, NA, adrenarche_age),
         gonadarche_age = if_else(gonadarche_age < 0 | gonadarche_age > interview_age_yrs, NA, gonadarche_age),
         manual_pubertal_age = if_else(manual_pubertal_age < 0 | manual_pubertal_age > interview_age_yrs, NA, manual_pubertal_age),
         ABCD_pubertal_age = if_else(ABCD_pubertal_age < 0 | ABCD_pubertal_age > interview_age_yrs, NA, ABCD_pubertal_age))

# count after resets/exclusions
fem_pubertal_timing %>%  filter(!is.na(adrenarche_age)) %>%  distinct(participant_id) %>%  nrow()       # 2236 -> 2065
fem_pubertal_timing %>%  filter(!is.na(gonadarche_age)) %>%  distinct(participant_id) %>%  nrow()       # 3633 -> 3309
fem_pubertal_timing %>%  filter(!is.na(manual_pubertal_age)) %>%  distinct(participant_id) %>%  nrow()  # 1129 -> 1027
fem_pubertal_timing %>%  filter(!is.na(ABCD_pubertal_age)) %>%  distinct(participant_id) %>%  nrow()    # 1076 -> 990
male_pubertal_timing %>%  filter(!is.na(adrenarche_age)) %>%  distinct(participant_id) %>%  nrow()       # 1713 -> 1417
male_pubertal_timing %>%  filter(!is.na(gonadarche_age)) %>%  distinct(participant_id) %>%  nrow()       # 2010 -> 1291
male_pubertal_timing %>%  filter(!is.na(manual_pubertal_age)) %>%  distinct(participant_id) %>%  nrow()  # 520 -> 382
male_pubertal_timing %>%  filter(!is.na(ABCD_pubertal_age)) %>%  distinct(participant_id) %>%  nrow()    # 494 -> 381

# check age means
mean(fem_pubertal_timing$adrenarche_age, na.rm = TRUE)
mean(fem_pubertal_timing$gonadarche_age, na.rm = TRUE)
mean(fem_pubertal_timing$manual_pubertal_age, na.rm = TRUE)
mean(fem_pubertal_timing$ABCD_pubertal_age, na.rm = TRUE)
mean(male_pubertal_timing$adrenarche_age, na.rm = TRUE)
mean(male_pubertal_timing$gonadarche_age, na.rm = TRUE)
mean(male_pubertal_timing$manual_pubertal_age, na.rm = TRUE)
mean(male_pubertal_timing$ABCD_pubertal_age, na.rm = TRUE)

### TEMPOS

# count before resets/exclusions
fem_pubertal_timing %>%  filter(!is.na(adrenarche_tempo)) %>%  distinct(participant_id) %>%  nrow()       # 2247
fem_pubertal_timing %>%  filter(!is.na(gonadarche_tempo)) %>%  distinct(participant_id) %>%  nrow()       # 3659
fem_pubertal_timing %>%  filter(!is.na(manual_pubertal_tempo)) %>%  distinct(participant_id) %>%  nrow()  # 1129
fem_pubertal_timing %>%  filter(!is.na(ABCD_pubertal_tempo)) %>%  distinct(participant_id) %>%  nrow()    # 1076
male_pubertal_timing %>%  filter(!is.na(adrenarche_tempo)) %>%  distinct(participant_id) %>%  nrow()       # 1724
male_pubertal_timing %>%  filter(!is.na(gonadarche_tempo)) %>%  distinct(participant_id) %>%  nrow()       # 2037
male_pubertal_timing %>%  filter(!is.na(manual_pubertal_tempo)) %>%  distinct(participant_id) %>%  nrow()  # 520
male_pubertal_timing %>%  filter(!is.na(ABCD_pubertal_tempo)) %>%  distinct(participant_id) %>%  nrow()    # 494

# Reset Tempos to NA if they are negative, which would indicate reverse puberty
fem_pubertal_timing <- fem_pubertal_timing %>%
  mutate(adrenarche_tempo = if_else(adrenarche_tempo < 0, NA, adrenarche_tempo),
         gonadarche_tempo = if_else(gonadarche_tempo < 0, NA, gonadarche_tempo),
         manual_pubertal_tempo = if_else(manual_pubertal_tempo < 0, NA, manual_pubertal_tempo),
         ABCD_pubertal_tempo = if_else(ABCD_pubertal_tempo < 0, NA, ABCD_pubertal_tempo))
male_pubertal_timing <- male_pubertal_timing %>%
  mutate(adrenarche_tempo = if_else(adrenarche_tempo < 0, NA, adrenarche_tempo),
         gonadarche_tempo = if_else(gonadarche_tempo < 0, NA, gonadarche_tempo),
         manual_pubertal_tempo = if_else(manual_pubertal_tempo < 0, NA, manual_pubertal_tempo),
         ABCD_pubertal_tempo = if_else(ABCD_pubertal_tempo < 0, NA, ABCD_pubertal_tempo))

# count after resets/exclusions
fem_pubertal_timing %>%  filter(!is.na(adrenarche_tempo)) %>%  distinct(participant_id) %>%  nrow()       # 2247 -> 2234
fem_pubertal_timing %>%  filter(!is.na(gonadarche_tempo)) %>%  distinct(participant_id) %>%  nrow()       # 3659 -> 3613
fem_pubertal_timing %>%  filter(!is.na(manual_pubertal_tempo)) %>%  distinct(participant_id) %>%  nrow()  # 1129 -> 1122
fem_pubertal_timing %>%  filter(!is.na(ABCD_pubertal_tempo)) %>%  distinct(participant_id) %>%  nrow()    # 1076 -> 1071
male_pubertal_timing %>%  filter(!is.na(adrenarche_tempo)) %>%  distinct(participant_id) %>%  nrow()       # 1724 -> 1709
male_pubertal_timing %>%  filter(!is.na(gonadarche_tempo)) %>%  distinct(participant_id) %>%  nrow()       # 2037 -> 1987
male_pubertal_timing %>%  filter(!is.na(manual_pubertal_tempo)) %>%  distinct(participant_id) %>%  nrow()  # 520 -> 517
male_pubertal_timing %>%  filter(!is.na(ABCD_pubertal_tempo)) %>%  distinct(participant_id) %>%  nrow()    # 494 -> 493

#################
# IN THE BELOW CODE BLOCK
# Make Flags for Post-Arche at baseline and pre-arche at last report
#################

## Arche at First Report
# If first report had a PDS score of 2.5 or greater, flag as the appropriate Arche at first report
fem_pubertal_timing <- fem_pubertal_timing %>%
  group_by(participant_id) %>%
  # Adrenarche
  mutate(first_adren_pds = na.omit(adrenarche_pds)[1]) %>%
  mutate(PostAdrenarche_Baseline_Y1N0 = if_else(first_adren_pds >= 2.5, 1, 0)) %>%
  # Gonadarche
  mutate(first_gonad_pds = na.omit(gonadarche_pds)[1]) %>%
  mutate(PostGonadarche_Baseline_Y1N0 = if_else(first_gonad_pds >= 2.5, 1, 0)) %>%
  # General
  mutate(first_manual_pds = na.omit(manual_pds)[1]) %>%
  mutate(PostPubertal_Baseline_Y1N0 = if_else(first_manual_pds >= 2.5, 1, 0)) %>%
  # ABCD Precalculated General
  mutate(first_abcd_pds = na.omit(ABCD_PDS)[1]) %>%
  mutate(ABCD_PostPubertal_Baseline_Y1N0 = if_else(first_abcd_pds >= 2.5, 1, 0))

male_pubertal_timing <- male_pubertal_timing %>%
  group_by(participant_id) %>%
  # Adrenarche
  mutate(first_adren_pds = na.omit(adrenarche_pds)[1]) %>%
  mutate(PostAdrenarche_Baseline_Y1N0 = if_else(first_adren_pds >= 2.5, 1, 0)) %>%
  # Gonadarche
  mutate(first_gonad_pds = na.omit(gonadarche_pds)[1]) %>%
  mutate(PostGonadarche_Baseline_Y1N0 = if_else(first_gonad_pds >= 2.5, 1, 0)) %>%
  # General
  mutate(first_manual_pds = na.omit(manual_pds)[1]) %>%
  mutate(PostPubertal_Baseline_Y1N0 = if_else(first_manual_pds >= 2.5, 1, 0)) %>%
  # ABCD Precalculated General
  mutate(first_abcd_pds = na.omit(ABCD_PDS)[1]) %>%
  mutate(ABCD_PostPubertal_Baseline_Y1N0 = if_else(first_abcd_pds >= 2.5, 1, 0))

# PrePubertal at Last Report
fem_pubertal_timing <- fem_pubertal_timing %>%
  group_by(participant_id) %>%
  mutate(last_adren_pds = adrenarche_pds[max(which(!is.na(adrenarche_pds)))]) %>%
  mutate(PreAdrenarche_Y1N0 = if_else(last_adren_pds < 2.5, 1, 0)) %>%
  # Gonadarche
  mutate(last_gonad_pds = gonadarche_pds[max(which(!is.na(gonadarche_pds)))]) %>%
  mutate(PreGonadarche_Y1N0 = if_else(last_gonad_pds < 2.5, 1, 0)) %>%
  # General
  mutate(last_manual_pds = manual_pds[max(which(!is.na(manual_pds)))]) %>%
  mutate(PrePubertal_Y1N0 = if_else(last_manual_pds < 2.5, 1, 0)) %>%
  # ABCD Precalculated General
  mutate(last_abcd_pds = manual_pds[max(which(!is.na(ABCD_PDS)))]) %>%
  mutate(ABCD_PrePubertal_Y1N0 = if_else(last_abcd_pds < 2.5, 1, 0))

male_pubertal_timing <- male_pubertal_timing %>%
  group_by(participant_id) %>%
  mutate(last_adren_pds = adrenarche_pds[max(which(!is.na(adrenarche_pds)))]) %>%
  mutate(PreAdrenarche_Y1N0 = if_else(last_adren_pds < 2.5, 1, 0)) %>%
  # Gonadarche
  mutate(last_gonad_pds = gonadarche_pds[max(which(!is.na(gonadarche_pds)))]) %>%
  mutate(PreGonadarche_Y1N0 = if_else(last_gonad_pds < 2.5, 1, 0)) %>%
  # General
  mutate(last_manual_pds = manual_pds[max(which(!is.na(manual_pds)))]) %>%
  mutate(PrePubertal_Y1N0 = if_else(last_manual_pds < 2.5, 1, 0)) %>%
  # ABCD Precalculated General
  mutate(last_abcd_pds = manual_pds[max(which(!is.na(ABCD_PDS)))]) %>%
  mutate(ABCD_PrePubertal_Y1N0 = if_else(last_abcd_pds < 2.5, 1, 0))

#################
# IN THE BELOW CODE BLOCK
# Make joint-sex Adrenarche, Gonadarche, and General Dataframes
#################

# Adrenarche
fem_adren <- fem_pubertal_timing %>%
  select(participant_id, session_id, sex, interview_age_yrs, adrenarche_age, adrenarche_tempo, 
         adrenarche_pds, body_hair, skin_pimples, num_adren_decreasing, Exclude_Adren_Y1N0, PostAdrenarche_Baseline_Y1N0, PreAdrenarche_Y1N0)
male_adren <- male_pubertal_timing %>%
  select(participant_id, session_id, sex, interview_age_yrs, adrenarche_age, adrenarche_tempo, 
         adrenarche_pds, body_hair, skin_pimples, num_adren_decreasing, Exclude_Adren_Y1N0, PostAdrenarche_Baseline_Y1N0, PreAdrenarche_Y1N0)
Adrenarche <- fem_adren %>%
  rbind(male_adren)

# Gonadarche
fem_gonad <- fem_pubertal_timing %>%
  mutate(voice_deep = NA,
         facial_hair = NA) %>%
  select(participant_id, session_id, sex, interview_age_yrs, gonadarche_age, gonadarche_tempo, 
         gonadarche_pds, thelarche, menarche, voice_deep, facial_hair, num_gonad_decreasing, Exclude_Gonad_Y1N0, PostGonadarche_Baseline_Y1N0, PreGonadarche_Y1N0)
male_gonad <- male_pubertal_timing %>%
  mutate(thelarche = NA,
         menarche = NA) %>%
  select(participant_id, session_id, sex, interview_age_yrs, gonadarche_age, gonadarche_tempo, 
         gonadarche_pds, thelarche, menarche, voice_deep, facial_hair, num_gonad_decreasing, Exclude_Gonad_Y1N0, PostGonadarche_Baseline_Y1N0, PreGonadarche_Y1N0)
Gonadarche <- fem_gonad %>%
  rbind(male_gonad)

# General Pubertal Onset
fem_general <- fem_pubertal_timing %>%
  mutate(voice_deep = NA,
         facial_hair = NA) %>%
  select(participant_id, session_id, sex, interview_age_yrs, manual_pds, manual_pubertal_age, manual_pubertal_tempo, ABCD_PDS, ABCD_pubertal_age, ABCD_pubertal_tempo,
         growth_spurt, body_hair, skin_pimples, thelarche, menarche, voice_deep, facial_hair, num_general_decreasing,
         Exclude_General_Y1N0, PostPubertal_Baseline_Y1N0, PrePubertal_Y1N0, ABCD_PostPubertal_Baseline_Y1N0, ABCD_PrePubertal_Y1N0)
male_general <- male_pubertal_timing %>%
  mutate(thelarche = NA,
         menarche = NA) %>%
  select(participant_id, session_id, sex, interview_age_yrs, manual_pds, manual_pubertal_age, manual_pubertal_tempo, ABCD_PDS, ABCD_pubertal_age, ABCD_pubertal_tempo,
         growth_spurt, body_hair, skin_pimples, thelarche, menarche, voice_deep, facial_hair, num_general_decreasing,
         Exclude_General_Y1N0, PostPubertal_Baseline_Y1N0, PrePubertal_Y1N0, ABCD_PostPubertal_Baseline_Y1N0, ABCD_PrePubertal_Y1N0)
General_Puberty <- fem_general %>%
  rbind(male_general)

# All 3 combined
adren_abridged <- Adrenarche %>%
  select(participant_id, session_id, adrenarche_age, adrenarche_tempo, adrenarche_pds, num_adren_decreasing, Exclude_Adren_Y1N0, PostAdrenarche_Baseline_Y1N0, PreAdrenarche_Y1N0)

gonad_abridged <- Gonadarche %>%
  select(participant_id, session_id, gonadarche_age, gonadarche_tempo, gonadarche_pds, num_gonad_decreasing, Exclude_Gonad_Y1N0, PostGonadarche_Baseline_Y1N0, PreGonadarche_Y1N0)

all_arches <- General_Puberty %>%
  full_join(adren_abridged, by = c("participant_id", "session_id")) %>%
  full_join(gonad_abridged, by = c("participant_id", "session_id")) %>%
  # organize
  select(participant_id, session_id, sex, interview_age_yrs, growth_spurt, body_hair, skin_pimples, thelarche, menarche, voice_deep, facial_hair, everything())

#################
# IN THE BELOW CODE BLOCK
# Make Dataframes that remove exclusions
#################

Adrenarche_Exclude <- Adrenarche %>%
  # before: 11688
  filter(Exclude_Adren_Y1N0 == 0) %>%             # has a relevant decreasing value
  # after: 4394
  filter(PostAdrenarche_Baseline_Y1N0 == 0) %>%   # post at baseline
  # after: 3819
  filter(PreAdrenarche_Y1N0 == 0)                 # pre at last report
  # after: 3291

Gonadarche_Exclude <- Gonadarche %>%
  # before: 11688
  filter(Exclude_Gonad_Y1N0 == 0) %>%
  # after: 6168
  filter(PostGonadarche_Baseline_Y1N0 == 0) %>%
  # after: 5800
  filter(PreGonadarche_Y1N0 == 0)
  # after: 4768

General_Puberty_Exclude <- General_Puberty %>%
  # before: 11688
  filter(Exclude_General_Y1N0 == 0) %>%
  # after: 1980
  filter(PostPubertal_Baseline_Y1N0 == 0) %>%
  # after: 1894
  filter(PrePubertal_Y1N0 == 0)
  # after: 1383

ABCD_Puberty_Exclude <- General_Puberty %>%
  # before: 11688
  filter(Exclude_General_Y1N0 == 0) %>%
  # after: 1980
  filter(ABCD_PostPubertal_Baseline_Y1N0 == 0) %>%
  # after: 1783
  filter(ABCD_PrePubertal_Y1N0 == 0)
  # after: 1305

# run after each stage to get exclusion counts
num_adren <- Adrenarche_Exclude %>% # 3291
  select(participant_id) %>%
  unique()

num_gonad <- Gonadarche_Exclude %>% # 4768
  select(participant_id) %>%
  unique()

num_gen <- General_Puberty_Exclude %>% # 1383
  select(participant_id) %>%
  unique()

num_abcd <- ABCD_Puberty_Exclude %>% # 1305
  select(participant_id) %>%
  unique()

#################
# IN THE BELOW CODE BLOCK
# Export to CSV
#################

write_csv(all_arches, paste0(derivative_data, "All_6.0_Arches_No_Exclude_10_27_25.csv"))
write_csv(Adrenarche_Exclude, paste0(derivative_data, "Adrenarche_6.0_Age_Tempos_10_27_25.csv"))
write_csv(Gonadarche_Exclude, paste0(derivative_data, "Gonadarche_6.0_Age_Tempos_10_27_25.csv"))
write_csv(General_Puberty_Exclude, paste0(derivative_data, "Puberty_6.0_Age_Tempos_10_27_25.csv"))
write_csv(ABCD_Puberty_Exclude, paste0(derivative_data, "ABCDPDS_Puberty_6.0_Age_Tempos_10_27_25.csv"))

