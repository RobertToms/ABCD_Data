################
# Adrenarche_Gonadarche_5.0.R
# Robert Toms, robert.toms@utdallas.edu
# 10/28/2025
################

# The purpose of this code is to calculate age at Adrenarche, Gonadarche, and general pubertal onset in females.

library(tidyverse)
library(dplyr)
library(tidyverse)
library(dplyr)

datapath <- "/path/to/ABCD_5.0/core"
derivative_data <- "/path/to/derivative_data/5.0/"

pds <- read_csv(paste0(datapath, "/physical-health/ph_y_pds.csv"))
sex <- read_csv(paste0(datapath, "/abcd-general/abcd_p_demo.csv"))
age_date <- read_csv(paste0(datapath, "/abcd-general/abcd_y_lt.csv"))

#################
# IN THE BELOW CODE BLOCK
# Clean and Prepare Data
#################

# remove irrelevant columns
sex <- sex %>%
  select(src_subject_id, demo_sex_v2) %>% # Biological Sex
  rename(sex = demo_sex_v2) %>%
  # remove NA rows
  filter(!is.na(sex))

age_date <- age_date %>%
  select(src_subject_id, eventname, interview_date, interview_age) %>%
  filter(eventname == 'baseline_year_1_arm_1' |
           eventname == '1_year_follow_up_y_arm_1' |
           eventname == '2_year_follow_up_y_arm_1' |
           eventname == '3_year_follow_up_y_arm_1' |
           eventname == '4_year_follow_up_y_arm_1')

# convert interview age to years
age_date <- age_date %>%
  mutate(interview_age = interview_age / 12)

# make human readable
sex <- sex %>%
  mutate(sex = case_when(
    sex == 1 ~ "M",
    sex == 2 ~ "F"))

# remove irrelevant columns
pds <- pds %>%
  select(src_subject_id,   # subject ID
         eventname,       # visit
         pds_ht2_y,     # Growth in Height: Has not yet begun to spurt (spurt means grow faster than usual) = 1; Has barely started = 2; Is definitely underway (has definitely started) = 3; Seems complete = 4; I don't know = 999; Decline to answer = 777
         pds_bdyhair_y,     # Body Hair Growth: Has not yet begun to grow = 1; Has barely started = 2; Is definitely underway (has definitely started) = 3; Seems complete = 4; I don't know = 999; Decline to answer = 777
         pds_skin2_y,     # Skin changes/Pimples: skin has not yet started changing = 1; skin has barely started changing = 2; skin changes are definitely underway (has definitely started) = 3; skin changes seem complete = 4; I don't know = 999; Decline to answer = 777
         pds_f4_2_y,  # (Female) Breasts begun to grow? : have not yet started growing = 1; have barely started growing = 2; breast growth is definitely underway (has definitely started) = 3; breast growth seems complete = 4; I don't know = 999; Decline to answer = 777
         pds_f5_y,  # (Female) Begun to Menstruate? : Yes = 1; No = 0; I don't know = 999; Refuse to answer = 777
         pds_m4_y, # (Male) Have you noticed a deepening of your voice? : voice has not yet started changing = 1; voice has barely started changing = 2; voice changes are definitely underway = 3; voice changes seem complete = 4; I don't know = 999         
         pds_m5_y) # (Male) Have you begun to grow hair on your face? : facial hair has not yet started growing = 1; facial hair has barely started growing = 2; facial hair has definitely started = 3; facial growth seems complete = 4; I don't know = 999


# make human readable
pds <- pds %>%
  rename(growth_spurt = pds_ht2_y,
         body_hair = pds_bdyhair_y,
         skin_pimples = pds_skin2_y,
         thelarche = pds_f4_2_y,
         menarche = pds_f5_y,
         voice_deep = pds_m4_y,
         facial_hair = pds_m5_y)

# merge in age, date, and sex data
pds_sex <- pds %>%
  left_join(age_date, by = c("src_subject_id", "eventname")) %>%
  left_join(sex, by = "src_subject_id")

# Set 999s and 777s to NA, per Herting et al., 2021 best practices
pds_sex <- pds_sex %>%
  mutate(across(c(growth_spurt, 
                  body_hair, 
                  skin_pimples, 
                  thelarche, 
                  menarche,
                  voice_deep,
                  facial_hair), ~case_when(
                    . == 999 ~ NA,
                    . == 777 ~ NA,
                    TRUE ~ .)))

# Make male and female specific dataframes
female_pds <- pds_sex %>%
  filter(sex == "F")
male_pds <- pds_sex %>%
  filter(sex == "M")

#################
# IN THE BELOW CODE BLOCK
# Manually Calculate PDS Values
#################

# Calculate General PDS Value: mean of growth_spurt, body_hair, skin_pimples, thelarche & menarche
female_pds <- female_pds %>%
  rowwise() %>%
  mutate(manual_pds = mean(c(growth_spurt, body_hair, skin_pimples, thelarche, menarche), na.rm = TRUE)) %>%
  # set NaN's to NA
  mutate(manual_pds = if_else(is.nan(manual_pds), NA, manual_pds))

# Calculate General PDS Value: mean of growth_spurt, body_hair, skin_pimples, voice deepening & facial hair
male_pds <- male_pds %>%
  rowwise() %>%
  mutate(manual_pds = mean(c(growth_spurt, body_hair, skin_pimples, voice_deep, facial_hair), na.rm = TRUE)) %>%
  # set NaN's to NA
  mutate(manual_pds = if_else(is.nan(manual_pds), NA, manual_pds))

# calculate Adrenarche and Gonadarche PDS values
female_pds <- female_pds %>%
  rowwise() %>%
  # Adrenarche PDS
  mutate(adrenarche_pds = sum(body_hair, skin_pimples)) %>%
  mutate(adrenarche_pds = adrenarche_pds / 2) %>%
  # Gonadarche PDS
  mutate(gonadarche_pds = sum(thelarche, menarche)) %>%
  mutate(gonadarche_pds = gonadarche_pds / 2)
# calculate Adrenarche and Gonadarche PDS values
male_pds <- male_pds %>%
  rowwise() %>%
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
  select(src_subject_id, eventname, growth_spurt, body_hair, skin_pimples, voice_deep, facial_hair)

# male list values for is_inconsistent to iterate thru
m_decreasing <- m_decreasing %>%
  group_by(src_subject_id) %>%
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
  select(src_subject_id, eventname, growth_spurt, body_hair, skin_pimples, thelarche, menarche)

# male list values for is_inconsistent to iterate thru
f_decreasing <- f_decreasing %>%
  group_by(src_subject_id) %>%
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
  select(src_subject_id, num_gonad_decreasing, num_adren_decreasing, num_general_decreasing) %>%
  unique()
f_decreasing <- f_decreasing %>%
  select(src_subject_id, num_gonad_decreasing, num_adren_decreasing, num_general_decreasing) %>%
  unique()

#################
# IN THE BELOW CODE BLOCK
# Calculate Age at Adrenarche, Gonadarche, General Pubertal onset
#################

# define function to get slopes
find_slope <- function(data, y) {                   # takes in dataframe and specific y column
  model_data <- data %>%                                                         
    select(interview_age, all_of(y)) %>%        # keep x and y
    drop_na()                                       # drop NAs
  if (nrow(model_data) < 2) return(NA_real_)        # return NA in the column if insufficent data to calculate slopes
  model <- lm(reformulate("interview_age", response = y), data = model_data)   # calculate linear model
  coef(model)[["interview_age"]]                # pull slope value from linear model
}

# define function to get intercepts
find_intercept <- function(data, y) {                   # takes in dataframe and specific y column
  model_data <- data %>%                                                         
    select(interview_age, all_of(y)) %>%        # keep x and y
    drop_na()                                       # drop NAs
  if (nrow(model_data) < 2) return(NA_real_)        # return NA in the column if insufficent data to calculate a linear model
  model <- lm(reformulate("interview_age", response = y), data = model_data)   # calculate linear model
  coef(model)[[1]]                # pull intercept value from linear model
}

# Calculate slopes and intercepts for onsets of interest
fem_arche_slopes <- female_pds %>%
  group_by(src_subject_id) %>%
  group_modify(~ {
    tibble(
      manual_slope = find_slope(.x, "manual_pds"),
      manual_intercept = find_intercept(.x, "manual_pds"),
      adrenarche_slope = find_slope(.x, "adrenarche_pds"),
      adrenarche_intercept = find_intercept(.x, "adrenarche_pds"),
      gonadarche_slope = find_slope(.x, "gonadarche_pds"),
      gonadarche_intercept = find_intercept(.x, "gonadarche_pds")
    )
  })

male_arche_slopes <- male_pds %>%
  group_by(src_subject_id) %>%
  group_modify(~ {
    tibble(
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
  mutate(manual_pubertal_age = (2.5 - manual_intercept) / manual_slope) %>%
  mutate(adrenarche_age = (2.5 - adrenarche_intercept) / adrenarche_slope) %>%
  mutate(gonadarche_age = (2.5 - gonadarche_intercept) / gonadarche_slope)
male_arche_slopes <- male_arche_slopes %>%
  mutate(manual_pubertal_age = (2.5 - manual_intercept) / manual_slope) %>%
  mutate(adrenarche_age = (2.5 - adrenarche_intercept) / adrenarche_slope) %>%
  mutate(gonadarche_age = (2.5 - gonadarche_intercept) / gonadarche_slope)

#################
# IN THE BELOW CODE BLOCK
# Remove Ages and Slopes based on faulty self-report data
#################

# isolate ages and rename tempos for each pubertal category
female_age_tempo <- fem_arche_slopes %>%
  select(src_subject_id, 
         adrenarche_age, adrenarche_slope,
         gonadarche_age, gonadarche_slope,
         manual_pubertal_age, manual_slope) %>%
  rename(adrenarche_tempo = adrenarche_slope,
         gonadarche_tempo = gonadarche_slope,
         manual_pubertal_tempo = manual_slope)
male_age_tempo <- male_arche_slopes %>%
  select(src_subject_id, 
         adrenarche_age, adrenarche_slope,
         gonadarche_age, gonadarche_slope,
         manual_pubertal_age, manual_slope) %>%
  rename(adrenarche_tempo = adrenarche_slope,
         gonadarche_tempo = gonadarche_slope,
         manual_pubertal_tempo = manual_slope)

# merge in decreasing counts
female_age_tempo <- female_age_tempo %>%
  left_join(f_decreasing, by = 'src_subject_id')
male_age_tempo <- male_age_tempo %>%
  left_join(m_decreasing, by = 'src_subject_id')

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
# total: 5677
table(female_age_tempo$Exclude_Adren_Y1N0)    # 2578 subjects excluded, 3099 usable
table(female_age_tempo$Exclude_Gonad_Y1N0)    # 1315 subjects excluded, 4362 usable
table(female_age_tempo$Exclude_General_Y1N0)  # 3896 subjects excluded, 1781 usable

# total: 6188
table(male_age_tempo$Exclude_Adren_Y1N0)   # 3752 subjects excluded, 2436 usable
table(male_age_tempo$Exclude_Gonad_Y1N0)   # 3398 subjects excluded, 2790 usable
table(male_age_tempo$Exclude_General_Y1N0) # 5219 subjects excluded, 969 usable

# total: 11865
# Adrenarche, 6330 subjects excluded, 5535 usable
# Gonadarche, 4713 subjects excluded, 7152 usable
# General, 9115 subjects excluded, 2750 usable

## Set relevant exclusions to NA
female_age_tempo <- female_age_tempo %>%
  rowwise() %>%
  mutate(adrenarche_age = if_else(Exclude_Adren_Y1N0 == 1, NA, adrenarche_age),
         adrenarche_tempo = if_else(Exclude_Adren_Y1N0 == 1, NA, adrenarche_tempo),
         gonadarche_age = if_else(Exclude_Gonad_Y1N0 == 1, NA, gonadarche_age),
         gonadarche_tempo = if_else(Exclude_Gonad_Y1N0 == 1, NA, gonadarche_tempo),
         manual_pubertal_age = if_else(Exclude_General_Y1N0 == 1, NA, manual_pubertal_age),
         manual_pubertal_tempo = if_else(Exclude_General_Y1N0 == 1, NA, manual_pubertal_tempo))
male_age_tempo <- male_age_tempo %>%
  rowwise() %>%
  mutate(adrenarche_age = if_else(Exclude_Adren_Y1N0 == 1, NA, adrenarche_age),
         adrenarche_tempo = if_else(Exclude_Adren_Y1N0 == 1, NA, adrenarche_tempo),
         gonadarche_age = if_else(Exclude_Gonad_Y1N0 == 1, NA, gonadarche_age),
         gonadarche_tempo = if_else(Exclude_Gonad_Y1N0 == 1, NA, gonadarche_tempo),
         manual_pubertal_age = if_else(Exclude_General_Y1N0 == 1, NA, manual_pubertal_age),
         manual_pubertal_tempo = if_else(Exclude_General_Y1N0 == 1, NA, manual_pubertal_tempo))

#################
# IN THE BELOW CODE BLOCK
# Clean and Organize
#################

# isolate pds items and counts
fem_pubertal_items <- female_pds %>%
  select(src_subject_id, eventname, sex, interview_age,
         growth_spurt, body_hair, skin_pimples, thelarche, menarche,
         adrenarche_pds, gonadarche_pds, manual_pds)
male_pubertal_items <- male_pds %>%
  select(src_subject_id, eventname, sex, interview_age,
         growth_spurt, body_hair, skin_pimples, voice_deep, facial_hair,
         adrenarche_pds, gonadarche_pds, manual_pds)

# merge items and ages
fem_pubertal_timing <- fem_pubertal_items %>%
  left_join(female_age_tempo, by = "src_subject_id")
male_pubertal_timing <- male_pubertal_items %>%
  left_join(male_age_tempo, by = "src_subject_id")

# reorder for clarity and readability
fem_pubertal_timing <- fem_pubertal_timing %>%
  select(src_subject_id, eventname, sex, interview_age,
         adrenarche_age, adrenarche_tempo,
         gonadarche_age, gonadarche_tempo,
         manual_pubertal_age, manual_pubertal_tempo,
         everything())
male_pubertal_timing <- male_pubertal_timing %>%
  select(src_subject_id, eventname, sex, interview_age,
         adrenarche_age, adrenarche_tempo,
         gonadarche_age, gonadarche_tempo,
         manual_pubertal_age, manual_pubertal_tempo,
         everything())

#################
# IN THE BELOW CODE BLOCK
# Validity-Check Data
#################

### AGES

# set Inf's to NA
fem_pubertal_timing <- fem_pubertal_timing %>%
  mutate(adrenarche_age = if_else(is.infinite(adrenarche_age), NA, adrenarche_age),
         gonadarche_age = if_else(is.infinite(gonadarche_age), NA, gonadarche_age),
         manual_pubertal_age = if_else(is.infinite(manual_pubertal_age), NA, manual_pubertal_age))
male_pubertal_timing <- male_pubertal_timing %>%
  mutate(adrenarche_age = if_else(is.infinite(adrenarche_age), NA, adrenarche_age),
         gonadarche_age = if_else(is.infinite(gonadarche_age), NA, gonadarche_age),
         manual_pubertal_age = if_else(is.infinite(manual_pubertal_age), NA, manual_pubertal_age))

# check age means
mean(fem_pubertal_timing$adrenarche_age, na.rm = TRUE)
mean(fem_pubertal_timing$gonadarche_age, na.rm = TRUE)
mean(fem_pubertal_timing$manual_pubertal_age, na.rm = TRUE)
mean(male_pubertal_timing$adrenarche_age, na.rm = TRUE)
mean(male_pubertal_timing$gonadarche_age, na.rm = TRUE)
mean(male_pubertal_timing$manual_pubertal_age, na.rm = TRUE)

# count before resets/exclusions
fem_pubertal_timing %>%  filter(!is.na(adrenarche_age)) %>%  distinct(src_subject_id) %>%  nrow()       # 2777
fem_pubertal_timing %>%  filter(!is.na(gonadarche_age)) %>%  distinct(src_subject_id) %>%  nrow()       # 3898
fem_pubertal_timing %>%  filter(!is.na(manual_pubertal_age)) %>%  distinct(src_subject_id) %>%  nrow()  # 1589
male_pubertal_timing %>%  filter(!is.na(adrenarche_age)) %>%  distinct(src_subject_id) %>%  nrow()       # 2129
male_pubertal_timing %>%  filter(!is.na(gonadarche_age)) %>%  distinct(src_subject_id) %>%  nrow()       # 2366
male_pubertal_timing %>%  filter(!is.na(manual_pubertal_age)) %>%  distinct(src_subject_id) %>%  nrow()  # 804

# Reset Age at Adrenarche, Gonadarche, and General Pubertal Onsets to NA if ages are negative or have not yet occurred.
# In an effort to remove impossible and unconfirmed pubertal onset ages, those occurring before birth or in the future (in relation to inteview age) are reset to NA.
fem_pubertal_timing <- fem_pubertal_timing %>%
  mutate(adrenarche_age = if_else(adrenarche_age < 0 | adrenarche_age > interview_age, NA, adrenarche_age),
         gonadarche_age = if_else(gonadarche_age < 0 | gonadarche_age > interview_age, NA, gonadarche_age),
         manual_pubertal_age = if_else(manual_pubertal_age < 0 | manual_pubertal_age > interview_age, NA, manual_pubertal_age))
male_pubertal_timing <- male_pubertal_timing %>%
  mutate(adrenarche_age = if_else(adrenarche_age < 0 | adrenarche_age > interview_age, NA, adrenarche_age),
         gonadarche_age = if_else(gonadarche_age < 0 | gonadarche_age > interview_age, NA, gonadarche_age),
         manual_pubertal_age = if_else(manual_pubertal_age < 0 | manual_pubertal_age > interview_age, NA, manual_pubertal_age))

# count after resets/exclusions
fem_pubertal_timing %>%  filter(!is.na(adrenarche_age)) %>%  distinct(src_subject_id) %>%  nrow()       # 2777 -> 2232
fem_pubertal_timing %>%  filter(!is.na(gonadarche_age)) %>%  distinct(src_subject_id) %>%  nrow()       # 3898 -> 2875
fem_pubertal_timing %>%  filter(!is.na(manual_pubertal_age)) %>%  distinct(src_subject_id) %>%  nrow()  # 1589 -> 1300
male_pubertal_timing %>%  filter(!is.na(adrenarche_age)) %>%  distinct(src_subject_id) %>%  nrow()       # 2129 -> 1303
male_pubertal_timing %>%  filter(!is.na(gonadarche_age)) %>%  distinct(src_subject_id) %>%  nrow()       # 2366 -> 793
male_pubertal_timing %>%  filter(!is.na(manual_pubertal_age)) %>%  distinct(src_subject_id) %>%  nrow()  # 804 -> 425

# check age means
mean(fem_pubertal_timing$adrenarche_age, na.rm = TRUE)
mean(fem_pubertal_timing$gonadarche_age, na.rm = TRUE)
mean(fem_pubertal_timing$manual_pubertal_age, na.rm = TRUE)
mean(male_pubertal_timing$adrenarche_age, na.rm = TRUE)
mean(male_pubertal_timing$gonadarche_age, na.rm = TRUE)
mean(male_pubertal_timing$manual_pubertal_age, na.rm = TRUE)


### TEMPOS

# count before resets/exclusions
fem_pubertal_timing %>%  filter(!is.na(adrenarche_tempo)) %>%  distinct(src_subject_id) %>%  nrow()       # 2821
fem_pubertal_timing %>%  filter(!is.na(gonadarche_tempo)) %>%  distinct(src_subject_id) %>%  nrow()       # 3968
fem_pubertal_timing %>%  filter(!is.na(manual_pubertal_tempo)) %>%  distinct(src_subject_id) %>%  nrow()  # 1589
male_pubertal_timing %>%  filter(!is.na(adrenarche_tempo)) %>%  distinct(src_subject_id) %>%  nrow()       # 2202
male_pubertal_timing %>%  filter(!is.na(gonadarche_tempo)) %>%  distinct(src_subject_id) %>%  nrow()       # 2583
male_pubertal_timing %>%  filter(!is.na(manual_pubertal_tempo)) %>%  distinct(src_subject_id) %>%  nrow()  # 809

# Reset Tempos to NA if they are negative, which would indicate reverse puberty
fem_pubertal_timing <- fem_pubertal_timing %>%
  mutate(adrenarche_tempo = if_else(adrenarche_tempo < 0, NA, adrenarche_tempo),
         gonadarche_tempo = if_else(gonadarche_tempo < 0, NA, gonadarche_tempo),
         manual_pubertal_tempo = if_else(manual_pubertal_tempo < 0, NA, manual_pubertal_tempo))
male_pubertal_timing <- male_pubertal_timing %>%
  mutate(adrenarche_tempo = if_else(adrenarche_tempo < 0, NA, adrenarche_tempo),
         gonadarche_tempo = if_else(gonadarche_tempo < 0, NA, gonadarche_tempo),
         manual_pubertal_tempo = if_else(manual_pubertal_tempo < 0, NA, manual_pubertal_tempo))

# count after resets/exclusions
fem_pubertal_timing %>%  filter(!is.na(adrenarche_tempo)) %>%  distinct(src_subject_id) %>%  nrow()       # 2821 -> 2771
fem_pubertal_timing %>%  filter(!is.na(gonadarche_tempo)) %>%  distinct(src_subject_id) %>%  nrow()       # 3968 -> 3845
fem_pubertal_timing %>%  filter(!is.na(manual_pubertal_tempo)) %>%  distinct(src_subject_id) %>%  nrow()  # 1589 -> 1576
male_pubertal_timing %>%  filter(!is.na(adrenarche_tempo)) %>%  distinct(src_subject_id) %>%  nrow()       # 2202 -> 2147
male_pubertal_timing %>%  filter(!is.na(gonadarche_tempo)) %>%  distinct(src_subject_id) %>%  nrow()       # 2583 -> 2440
male_pubertal_timing %>%  filter(!is.na(manual_pubertal_tempo)) %>%  distinct(src_subject_id) %>%  nrow()  # 809 -> 799

#################
# IN THE BELOW CODE BLOCK
# Make Flags for Post-Arche at baseline and pre-arche at last report
#################

## Arche at First Report
# If first report had a PDS score of 2.5 or greater, flag as the appropriate Arche at first report
fem_pubertal_timing <- fem_pubertal_timing %>%
  group_by(src_subject_id) %>%
  # Adrenarche
  mutate(first_adren_pds = na.omit(adrenarche_pds)[1]) %>%
  mutate(PostAdrenarche_Baseline_Y1N0 = if_else(first_adren_pds >= 2.5, 1, 0)) %>%
  # Gonadarche
  mutate(first_gonad_pds = na.omit(gonadarche_pds)[1]) %>%
  mutate(PostGonadarche_Baseline_Y1N0 = if_else(first_gonad_pds >= 2.5, 1, 0)) %>%
  # General
  mutate(first_manual_pds = na.omit(manual_pds)[1]) %>%
  mutate(PostPubertal_Baseline_Y1N0 = if_else(first_manual_pds >= 2.5, 1, 0))

male_pubertal_timing <- male_pubertal_timing %>%
  group_by(src_subject_id) %>%
  # Adrenarche
  mutate(first_adren_pds = na.omit(adrenarche_pds)[1]) %>%
  mutate(PostAdrenarche_Baseline_Y1N0 = if_else(first_adren_pds >= 2.5, 1, 0)) %>%
  # Gonadarche
  mutate(first_gonad_pds = na.omit(gonadarche_pds)[1]) %>%
  mutate(PostGonadarche_Baseline_Y1N0 = if_else(first_gonad_pds >= 2.5, 1, 0)) %>%
  # General
  mutate(first_manual_pds = na.omit(manual_pds)[1]) %>%
  mutate(PostPubertal_Baseline_Y1N0 = if_else(first_manual_pds >= 2.5, 1, 0))

# PrePubertal at Last Report
fem_pubertal_timing <- fem_pubertal_timing %>%
  group_by(src_subject_id) %>%
  mutate(last_adren_pds = adrenarche_pds[max(which(!is.na(adrenarche_pds)))]) %>%
  mutate(PreAdrenarche_Y1N0 = if_else(last_adren_pds < 2.5, 1, 0)) %>%
  # Gonadarche
  mutate(last_gonad_pds = gonadarche_pds[max(which(!is.na(gonadarche_pds)))]) %>%
  mutate(PreGonadarche_Y1N0 = if_else(last_gonad_pds < 2.5, 1, 0)) %>%
  # General
  mutate(last_manual_pds = manual_pds[max(which(!is.na(manual_pds)))]) %>%
  mutate(PrePubertal_Y1N0 = if_else(last_manual_pds < 2.5, 1, 0))

male_pubertal_timing <- male_pubertal_timing %>%
  group_by(src_subject_id) %>%
  mutate(last_adren_pds = adrenarche_pds[max(which(!is.na(adrenarche_pds)))]) %>%
  mutate(PreAdrenarche_Y1N0 = if_else(last_adren_pds < 2.5, 1, 0)) %>%
  # Gonadarche
  mutate(last_gonad_pds = gonadarche_pds[max(which(!is.na(gonadarche_pds)))]) %>%
  mutate(PreGonadarche_Y1N0 = if_else(last_gonad_pds < 2.5, 1, 0)) %>%
  # General
  mutate(last_manual_pds = manual_pds[max(which(!is.na(manual_pds)))]) %>%
  mutate(PrePubertal_Y1N0 = if_else(last_manual_pds < 2.5, 1, 0))

#################
# IN THE BELOW CODE BLOCK
# Make joint-sex Adrenarche, Gonadarche, and General Dataframes
#################

# Adrenarche
fem_adren <- fem_pubertal_timing %>%
  select(src_subject_id, eventname, sex, interview_age, adrenarche_age, adrenarche_tempo, 
         adrenarche_pds, body_hair, skin_pimples, num_adren_decreasing, Exclude_Adren_Y1N0, PostAdrenarche_Baseline_Y1N0, PreAdrenarche_Y1N0)
male_adren <- male_pubertal_timing %>%
  select(src_subject_id, eventname, sex, interview_age, adrenarche_age, adrenarche_tempo, 
         adrenarche_pds, body_hair, skin_pimples, num_adren_decreasing, Exclude_Adren_Y1N0, PostAdrenarche_Baseline_Y1N0, PreAdrenarche_Y1N0)
Adrenarche <- fem_adren %>%
  rbind(male_adren)

# Gonadarche
fem_gonad <- fem_pubertal_timing %>%
  mutate(voice_deep = NA,
         facial_hair = NA) %>%
  select(src_subject_id, eventname, sex, interview_age, gonadarche_age, gonadarche_tempo, 
         gonadarche_pds, thelarche, menarche, voice_deep, facial_hair, num_gonad_decreasing, Exclude_Gonad_Y1N0, PostGonadarche_Baseline_Y1N0, PreGonadarche_Y1N0)
male_gonad <- male_pubertal_timing %>%
  mutate(thelarche = NA,
         menarche = NA) %>%
  select(src_subject_id, eventname, sex, interview_age, gonadarche_age, gonadarche_tempo, 
         gonadarche_pds, thelarche, menarche, voice_deep, facial_hair, num_gonad_decreasing, Exclude_Gonad_Y1N0, PostGonadarche_Baseline_Y1N0, PreGonadarche_Y1N0)
Gonadarche <- fem_gonad %>%
  rbind(male_gonad)

# General Pubertal Onset
fem_general <- fem_pubertal_timing %>%
  mutate(voice_deep = NA,
         facial_hair = NA) %>%
  select(src_subject_id, eventname, sex, interview_age, manual_pds, manual_pubertal_age, manual_pubertal_tempo, 
         growth_spurt, body_hair, skin_pimples, thelarche, menarche, voice_deep, facial_hair, num_general_decreasing,
         Exclude_General_Y1N0, PostPubertal_Baseline_Y1N0, PrePubertal_Y1N0)
male_general <- male_pubertal_timing %>%
  mutate(thelarche = NA,
         menarche = NA) %>%
  select(src_subject_id, eventname, sex, interview_age, manual_pds, manual_pubertal_age, manual_pubertal_tempo, 
         growth_spurt, body_hair, skin_pimples, thelarche, menarche, voice_deep, facial_hair, num_general_decreasing,
         Exclude_General_Y1N0, PostPubertal_Baseline_Y1N0, PrePubertal_Y1N0)
General_Puberty <- fem_general %>%
  rbind(male_general)

# All 3 combined
adren_abridged <- Adrenarche %>%
  select(src_subject_id, eventname, adrenarche_age, adrenarche_tempo, adrenarche_pds, num_adren_decreasing, Exclude_Adren_Y1N0, PostAdrenarche_Baseline_Y1N0, PreAdrenarche_Y1N0)

gonad_abridged <- Gonadarche %>%
  select(src_subject_id, eventname, gonadarche_age, gonadarche_tempo, gonadarche_pds, num_gonad_decreasing, Exclude_Gonad_Y1N0, PostGonadarche_Baseline_Y1N0, PreGonadarche_Y1N0)

all_arches <- General_Puberty %>%
  full_join(adren_abridged, by = c("src_subject_id", "eventname")) %>%
  full_join(gonad_abridged, by = c("src_subject_id", "eventname")) %>%
  # organize
  select(src_subject_id, eventname, sex, interview_age, growth_spurt, body_hair, skin_pimples, thelarche, menarche, voice_deep, facial_hair, everything())

#################
# IN THE BELOW CODE BLOCK
# Make Dataframes that remove exclusions
#################

Adrenarche_Exclude <- Adrenarche %>%
  # before: 11685
  filter(Exclude_Adren_Y1N0 == 0) %>%             # has a relevant decreasing value
  # after: 5535
  filter(PostAdrenarche_Baseline_Y1N0 == 0) %>%   # post at baseline
  # after: 4817
  filter(PreAdrenarche_Y1N0 == 0)                 # pre at last report
  # after: 3589

Gonadarche_Exclude <- Gonadarche %>%
  # before: 11685
  filter(Exclude_Gonad_Y1N0 == 0) %>%             # has a relevant decreasing value
  # after: 7152
  filter(PostGonadarche_Baseline_Y1N0 == 0) %>%   # post at baseline
  # after: 6720
  filter(PreGonadarche_Y1N0 == 0)                 # pre at last report
  # after: 3994

General_Puberty_Exclude <- General_Puberty %>%
  # before: 11685
  filter(Exclude_General_Y1N0 == 0) %>%           # has a relevant decreasing value
  # after: 2750
  filter(PostPubertal_Baseline_Y1N0 == 0) %>%     # post at baseline
  # after: 2639
  filter(PrePubertal_Y1N0 == 0)                   # pre at last report
  # after: 1735


# run after each stage to get exclusion counts
num_adren <- Adrenarche_Exclude %>% # 3589
  select(src_subject_id) %>%
  unique()

num_gonad <- Gonadarche_Exclude %>% # 3994
  select(src_subject_id) %>%
  unique()

num_gen <- General_Puberty_Exclude %>% # 1735
  select(src_subject_id) %>%
  unique()

#################
# IN THE BELOW CODE BLOCK
# Export to CSV
#################

write_csv(all_arches, paste0(derivative_data, "All_5.0_Arches_No_Exclude_10_28_25.csv"))
write_csv(Adrenarche_Exclude, paste0(derivative_data, "Adrenarche_5.0_Age_Tempos_10_28_25.csv"))
write_csv(Gonadarche_Exclude, paste0(derivative_data, "Gonadarche_5.0_Age_Tempos_10_28_25.csv"))
write_csv(General_Puberty_Exclude, paste0(derivative_data, "Puberty_5.0_Age_Tempos_10_28_25.csv"))

