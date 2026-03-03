################
# Pubertal_Onsets_Tempos_5.0.R
# Robert Toms, robert.toms@utdallas.edu
# 2/27/2026
################

# The purpose of this code is to calculate age at General Pubertal Onset and Pubertal Tempo in males and females.

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

# make human readable
sex <- sex %>%
  mutate(sex = case_when(
    sex == 1 ~ "M",
    sex == 2 ~ "F",
    sex == 3 ~ "IM"))

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

# count 999s and 777s
table(pds_sex$growth_spurt == 999, pds_sex$sex) #5577; 2985 F / 2592 M
table(pds_sex$body_hair == 999, pds_sex$sex) #859; 460 F / 399 M
table(pds_sex$skin_pimples == 999, pds_sex$sex) #1997; 885 F / 1111 M / 1 IM
table(pds_sex$thelarche == 999, pds_sex$sex) # 1131; 1131 F
table(pds_sex$menarche == 999, pds_sex$sex) #680; 680 F
table(pds_sex$voice_deep == 999, pds_sex$sex) #506; 506 M
table(pds_sex$facial_hair == 999, pds_sex$sex) #402; 401 M / 1 IM
# 5577+859+1997+1131+680+506+402 = 11152

pds_999s <- pds_sex %>% filter(pds_sex$growth_spurt == 999 | pds_sex$body_hair == 999 | pds_sex$skin_pimples == 999 | pds_sex$thelarche == 999 | pds_sex$menarche == 999 | pds_sex$voice_deep == 999 | pds_sex$facial_hair == 999)
table(pds_999s$sex) # 3875 F / 3408 M / 1 IM
subs_999 <- pds_999s %>% distinct(src_subject_id) %>% pull(src_subject_id) # 7284 timepoints, 6303 subjects affected
sex_999 <- pds_999s %>% select(src_subject_id, sex) %>% unique()
table(sex_999$sex) # 3312 F / 2990 M / 1 IM

table(pds_sex$growth_spurt == 777, pds_sex$sex) #488; 239 F / 249 M
table(pds_sex$body_hair == 777, pds_sex$sex) #793; 488 F / 305 M
table(pds_sex$skin_pimples == 777, pds_sex$sex) #398; 164 F / 234 M
table(pds_sex$thelarche == 777, pds_sex$sex) #726; 725 F / 1 IM
table(pds_sex$menarche == 777, pds_sex$sex) #787; 787 F
table(pds_sex$voice_deep == 777, pds_sex$sex) #134; 134 M
table(pds_sex$facial_hair == 777, pds_sex$sex) #191; 191 M
# 488+793+398+726+787+134+191 = 3517

pds_777s <- pds_sex %>% filter(pds_sex$growth_spurt == 777 | pds_sex$body_hair == 777 | pds_sex$skin_pimples == 777 | pds_sex$thelarche == 777 | pds_sex$menarche == 777 | pds_sex$voice_deep == 777 | pds_sex$facial_hair == 777)
table(pds_777s$sex) # 1480 F / 675 M; 2155 total
subs_777 <- pds_777s %>% distinct(src_subject_id) %>% pull(src_subject_id) # 2155 timepoints, 1627 subjects affected
sex_777 <- pds_777s %>% select(src_subject_id, sex) %>% unique()
table(sex_777$sex) # 1071 F / 556 M ; 1627 total

# Count subjects
subjects_counter <- pds_sex %>% distinct(src_subject_id) %>% pull(src_subject_id) # 11868

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
    . == 'n/a' ~ NA,
    TRUE ~ as.numeric(.))))

# Count remaining subjects
subjects_counter <- pds_sex %>% distinct(src_subject_id) %>% pull(src_subject_id)

# 2 females have values in male PDS categories (voice_deep and facial_hair), & 1 male has values in female PDS categories (thelarche and menarche), set these to NA
pds_sex <- pds_sex %>%
  mutate(thelarche = if_else(sex == 'M', NA, thelarche),
         menarche = if_else(sex == 'M', NA, menarche),
         voice_deep = if_else(sex == 'F', NA, voice_deep),
         facial_hair = if_else(sex == 'F', NA, facial_hair))

#################
# IN THE BELOW CODE BLOCK
# Manually Calculate PDS Values
#################

# calculate PDS values
pds_sex <- pds_sex %>%
  rowwise() %>%
  # General PDS
  mutate(manual_pds = case_when(
    sex == 'F' ~ sum(growth_spurt, body_hair, skin_pimples, thelarche, menarche, na.rm = TRUE),
    sex == 'M' ~ sum(growth_spurt, body_hair, skin_pimples, voice_deep, facial_hair, na.rm = TRUE),
    sex == 'IM' ~ sum(growth_spurt, body_hair, skin_pimples, voice_deep, facial_hair, na.rm = TRUE))) %>%
  mutate(num_items = sum(!is.na(growth_spurt),
                         !is.na(body_hair),
                         !is.na(skin_pimples),
                         !is.na(thelarche),
                         !is.na(menarche),
                         !is.na(voice_deep),
                         !is.na(facial_hair))) %>%
  mutate(manual_pds = manual_pds / num_items) %>%
  mutate(manual_pds = if_else(is.nan(manual_pds), NA, manual_pds))

#################
# IN THE BELOW CODE BLOCK
# Count subjects with no/insufficient data
#################

# Manually Calculated PDS
pds_counts <- pds_sex %>% group_by(src_subject_id) %>% mutate(num_timepoints = sum(!is.na(manual_pds))) %>% ungroup() %>% select(src_subject_id, num_timepoints, sex) %>% unique()
table(pds_counts$num_timepoints, pds_counts$sex) # 1 F / 3 M with no timepoints, 191 F / 157 M with 1 timepoint, 348 total

subject_counts <- pds_counts %>% filter(num_timepoints > 1)
table(subject_counts$sex) # 5485 F / 6028 M / 3 IM; 11516 total remain

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
  select(src_subject_id, eventname, growth_spurt, body_hair, skin_pimples, thelarche, menarche, voice_deep, facial_hair)

# make list values for is_inconsistent to iterate thru
pds_decreasing <- pds_decreasing %>%
  group_by(src_subject_id) %>%
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
  select(src_subject_id, num_general_decreasing) %>%
  unique()

#################
# IN THE BELOW CODE BLOCK
# Calculate Age at General Pubertal onset
#################

# define function to get slopes
find_slope <- function(data, y) {                   # takes in dataframe and specific y column
  model_data <- data %>%                                                         
    select(interview_age, all_of(y)) %>%        # keep x and y
    drop_na()                                       # drop NAs
  if (nrow(model_data) < 2) return(NA_real_)        # return NA in the column if insufficent data to calculate slopes
  model <- lm(reformulate("interview_age", response = y), data = model_data)   # calculate linear model
  coef(model)[[2]]                # pull slope value from linear model
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
arche_slopes <- pds_sex %>%
  group_by(src_subject_id) %>%
  group_modify(~ {
    tibble(
      manual_slope = find_slope(.x, "manual_pds"),
      manual_intercept = find_intercept(.x, "manual_pds")
    )
  })

# Calculate Onset Ages: Age where PDS = 2.0
arche_slopes <- arche_slopes %>%
  mutate(manual_pubertal_age = (2.0 - manual_intercept) / manual_slope)

#################
# IN THE BELOW CODE BLOCK
# Remove Ages and Slopes based on faulty self-report data
#################

# isolate ages and rename tempos for each pubertal category
age_tempo <- arche_slopes %>%
  select(src_subject_id,
         manual_pubertal_age, manual_slope) %>%
  rename(manual_pubertal_tempo = manual_slope)

# merge in decreasing counts
age_tempo <- age_tempo %>%
  left_join(pds_decreasing, by = 'src_subject_id')

# flag if any relevant self reports indicate reverse puberty
age_tempo <- age_tempo %>%
  mutate(Has_Decrease_Y1N0 = if_else(num_general_decreasing > 0, 1, 0))

## COUNT EXCLUSIONS
age_tempo <- age_tempo %>% 
  mutate(has_manual = if_else(!is.na(manual_pubertal_age), 'has manual', 'no manual score'))

age_tempo <- age_tempo %>% left_join(sex, by = 'src_subject_id')

######### COUNT AND HAND-CHECK ANOMALIES ######
# NDAR_INVV7NEVHLK has NaN age, reset to NA
age_tempo <- age_tempo %>% mutate(manual_pubertal_age = if_else(is.nan(manual_pubertal_age), NA, manual_pubertal_age))

counter_age_tempo <- age_tempo %>% select(-sex) %>% left_join(pds_counts, by = 'src_subject_id') %>% filter(num_timepoints > 1)
table(counter_age_tempo$Has_Decrease_Y1N0, counter_age_tempo$sex, counter_age_tempo$has_manual)
#     F    M  IM  total: 11516 -> 11514
# 0 1588 809   1  - 2398 subjects with valid manual pubertal data
# 1 3896 5218  2 - 9116 subjects excluded for inconsistent reporting/reported pubertal regression

# 11514 total, 2 (1M/1F) had 2+ timepoints of data. 
# NDAR_INV749XW1TD (M) was missing interview ages (linear model invalid), 
# NDAR_INVV7NEVHLK (F) had an NaN age and 0 tempo, reset NaN age to NA above, 0 tempo will be set to NA later with others

sum(age_tempo$Has_Decrease_Y1N0 == 1)
sum(age_tempo$has_manual == 'has manual')

with_2 <- subject_counts %>% distinct(src_subject_id) %>% pull(src_subject_id)
without_2 <- age_tempo %>% filter(has_manual == 'has manual') %>% distinct(src_subject_id) %>% pull(src_subject_id)
missing <- setdiff(with_2, without_2)
# NDAR_INV749XW1TD has 4tps, reverse puberty w/ no age/tempo, NDAR_INVV7NEVHLK has 2tps, NaN age w/ 0 tempo, will get reset to NA later
#########################################

## Set relevant exclusions to NA
age_tempo <- age_tempo %>%
  rowwise() %>%
  mutate(manual_pubertal_age = if_else(Has_Decrease_Y1N0 == 1, NA, manual_pubertal_age),
         manual_pubertal_tempo = if_else(Has_Decrease_Y1N0 == 1, NA, manual_pubertal_tempo))

sum(!is.na(age_tempo$manual_pubertal_age)) # 2399
sum(!is.na(age_tempo$manual_pubertal_tempo)) # 2399

#################
# IN THE BELOW CODE BLOCK
# Clean and Organize
#################

# isolate pds items and counts
pubertal_items <- pds_sex %>%
  select(src_subject_id, eventname, interview_age,
         growth_spurt, body_hair, skin_pimples, thelarche, menarche, voice_deep, facial_hair,
         manual_pds, num_items)

# merge items and ages
pubertal_timing <- pubertal_items %>%
  left_join(age_tempo, by = "src_subject_id")

# reorder for clarity and readability
pubertal_timing <- pubertal_timing %>%
  select(src_subject_id, eventname, sex, interview_age,
         manual_pubertal_age, manual_pubertal_tempo,
         everything())

#################
# IN THE BELOW CODE BLOCK
# Validity-Check Data
#################

# count Inf's - Ages would be calculated as "infinite" if there is no pubertal progress, i.e. a score of 2 at every report
sum(is.infinite(pubertal_timing$manual_pubertal_age)) # 20 rows
sum(pubertal_timing$manual_pubertal_tempo == 0, na.rm = TRUE) # 20 rows
inf_count <- pubertal_timing %>% select(src_subject_id, manual_pubertal_age, sex) %>% filter(is.infinite(manual_pubertal_age)) %>% unique() 
inf_count %>% distinct(src_subject_id) %>% nrow() # 5 subjects have "infinite" pubertal ages, indicating no pubertal progress
table(inf_count$sex) # 5 M with Inf pubertal ages
inf_count <- pubertal_timing %>% select(src_subject_id, manual_pubertal_tempo, sex) %>% filter(manual_pubertal_tempo == 0) %>% unique() 
inf_count %>% distinct(src_subject_id) %>% nrow() # 5 subjects have flat pubertal tempos, indicating no pubertal progress
table(inf_count$sex) # 5 M reporting no pubertal progress

# set Inf's to NA for Age, and 0s to NA for slope (these mathematically co-occur for subjects, when there is no pubertal progression at all)
pubertal_timing <- pubertal_timing %>%
  mutate(manual_pubertal_age = if_else(is.infinite(manual_pubertal_age) | is.nan(manual_pubertal_age), NA, manual_pubertal_age)) %>%
  mutate(manual_pubertal_tempo = if_else(manual_pubertal_tempo == 0, NA, manual_pubertal_tempo)) 
# just as puberty does not go in reverse, it does not stagnate


### AGES
# check age means
mean(pubertal_timing$manual_pubertal_age, na.rm = TRUE)

# count before resets/exclusions
pubertal_timing %>%  filter(!is.na(manual_pubertal_age)) %>%  distinct(src_subject_id) %>%  nrow()  # 11535 -> 2394 from excluding inconsistent reporters

# count sex
subject_counter <- pubertal_timing %>% filter(!is.na(manual_pubertal_age)) %>% select(src_subject_id, manual_pubertal_age, sex) %>% unique()
table(subject_counter$sex) # 1589 F / 804 M / 1 IM; 2394 total subjects remain

# Reset Age at General Pubertal Onset to NA if ages are negative or have not yet occurred.
# In an effort to remove impossible and unconfirmed pubertal onset ages, those occurring before birth or in the future (in relation to inteview age) are reset to NA.
# NEGATIVE AGES
pubertal_timing <- pubertal_timing %>%
  mutate(manual_pubertal_age = if_else(manual_pubertal_age < 0, NA, manual_pubertal_age))

# count after resets/exclusions
pubertal_timing %>%  filter(!is.na(manual_pubertal_age)) %>%  distinct(src_subject_id) %>%  nrow()  # 2394 -> 2377; 17 removed

# count sex
subject_counter <- pubertal_timing %>% filter(!is.na(manual_pubertal_age)) %>% select(src_subject_id, manual_pubertal_age, sex) %>% unique()
table(subject_counter$sex) # 1578 F / 798 M / 1 IM ; 11 F / 6 M removed, 2377 total subjects remain

# check age means
mean(pubertal_timing$manual_pubertal_age, na.rm = TRUE)

# AGES IMPUTED TO BE AFTER LAST INTERVIEW
pubertal_timing <- pubertal_timing %>%
  mutate(manual_pubertal_age = if_else(manual_pubertal_age > interview_age, NA, manual_pubertal_age))

# count after resets/exclusions
pubertal_timing %>%  filter(!is.na(manual_pubertal_age)) %>%  distinct(src_subject_id) %>%  nrow()  # 2377 -> 2076; 301 removed

# count sex
subject_counter <- pubertal_timing %>% filter(!is.na(manual_pubertal_age)) %>% select(src_subject_id, manual_pubertal_age, sex) %>% unique()
table(subject_counter$sex) # 1460 F / 615 M / 1 IM; 118 F / 183 M removed, 2076 total subjects remain

# check age means
mean(pubertal_timing$manual_pubertal_age, na.rm = TRUE)


### TEMPOS

# count before resets/exclusions
pubertal_timing %>%  filter(!is.na(manual_pubertal_tempo)) %>%  distinct(src_subject_id) %>%  nrow()  # 2394

# count sex
subject_counter <- pubertal_timing %>% filter(!is.na(manual_pubertal_tempo)) %>% select(src_subject_id, manual_pubertal_tempo, sex) %>% unique()
table(subject_counter$sex) # 1589 F / 804 M / 1 IM ; 2394 total subjects remain

# Reset Tempos to NA if they are negative, which would indicate reverse puberty
pubertal_timing <- pubertal_timing %>%
  mutate(manual_pubertal_tempo = if_else(manual_pubertal_tempo < 0, NA, manual_pubertal_tempo))

# count after resets/exclusions
pubertal_timing %>%  filter(!is.na(manual_pubertal_tempo)) %>%  distinct(src_subject_id) %>%  nrow()  # 2394 -> 2371, -23

# count sex
subject_counter <- pubertal_timing %>% filter(!is.na(manual_pubertal_tempo)) %>% select(src_subject_id, manual_pubertal_tempo, sex) %>% unique()
table(subject_counter$sex) # 1576 F / 794 M / 1 IM; 13 F / 10 M removed, 2371 total subjects remain

#################
# IN THE BELOW CODE BLOCK
# Make Flags for Post-Puberty at baseline and pre-puberty at last report
#################

## Arche at First Report
# If first report had a PDS score of 2.0 or greater, flag as the appropriate Arche at first report
pubertal_timing <- pubertal_timing %>%
  group_by(src_subject_id) %>%
  # General
  mutate(first_manual_pds = na.omit(manual_pds)[1]) %>%
  mutate(PostPubertal_FirstReport_Y1N0 = if_else(first_manual_pds >= 2.0, 1, 0))

# PrePubertal at Last Report
pubertal_timing <- pubertal_timing %>%
  group_by(src_subject_id) %>%
  # General
  mutate(last_manual_pds = manual_pds[max(which(!is.na(manual_pds)))]) %>%
  mutate(PrePubertal_Y1N0 = if_else(last_manual_pds < 2.0, 1, 0))

#################
# IN THE BELOW CODE BLOCK
# Clean & Organize
#################

# keep useful columns
general <- pubertal_timing %>%
  select(src_subject_id, eventname, sex, interview_age, manual_pds, manual_pubertal_age, manual_pubertal_tempo,
         growth_spurt, body_hair, skin_pimples, thelarche, menarche, voice_deep, facial_hair, num_general_decreasing,
         Has_Decrease_Y1N0, PostPubertal_FirstReport_Y1N0, PrePubertal_Y1N0)

#################
# IN THE BELOW CODE BLOCK
# Make Dataframe that removes exclusions and get counts
#################

### MANUAL PUBERTAL SCORES
# count subjects
counter <- general %>% filter(!is.na(manual_pubertal_age)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 1460 F / 615 M / 1 IM; 2076 ages remain
counter <- general %>% filter(!is.na(manual_pubertal_tempo)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 1576 F / 794 M / 1 IM; 2370 tempos remain

# has a relevant decreasing value/inconsistently reported (already reset to NA, shouldn't change counts)
General_Exclude <- general %>% 
  mutate(manual_pubertal_age = if_else(Has_Decrease_Y1N0 == 1, NA, manual_pubertal_age)) %>% 
  mutate(manual_pubertal_tempo = if_else(Has_Decrease_Y1N0 == 1, NA, manual_pubertal_tempo))

# count subjects
counter <- General_Exclude %>% filter(!is.na(manual_pubertal_age)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 1460 F / 615 M / 1 IM; 2076 ages remain
counter <- General_Exclude %>% filter(!is.na(manual_pubertal_tempo)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 1576 F / 794 M / 1 IM; 2370 tempos remain

### TEMPOS will not be excluded for being Post-Pubertal at First Report or Pre-Pubertal at Last Report.
###        Even if they do not experience a Pubertal transition during the course of data collection, the rate of pubertal change is still valid.
### AGES will be excluded for these cases -- imputed ages are not valid if the subject has not yet experienced the event to which they refer
###                                       -- imputed ages are not valid if we cannot determine the age just prior to the occurrence of the event.

# Post Pubertal at first report
General_Exclude <- General_Exclude %>% 
  mutate(manual_pubertal_age = if_else(PostPubertal_FirstReport_Y1N0 == 1, NA, manual_pubertal_age))

# count subjects
counter <- General_Exclude %>% filter(!is.na(manual_pubertal_age)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 1159 F / 557 M / 1 IM; 359 (301F/58M) removed, 1717 ages remain

# pre at last report
General_Exclude <- General_Exclude %>% 
  mutate(manual_pubertal_age = if_else(PrePubertal_Y1N0 == 1, NA, manual_pubertal_age))

# count subjects
counter <- General_Exclude %>% filter(!is.na(manual_pubertal_age)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 1155 F / 554 M / 1 IM; 7 (4F/3M) removed, 1710 ages remain

#################
# IN THE BELOW CODE BLOCK
# Make Z-Scores of Age and Tempo, merge in
#################

### MANUAL
# must be split off and have 1 row per subject, so distributions are accurate when calculating Z-scores
age_zscore_maker <- General_Exclude %>%
  select(src_subject_id, manual_pubertal_age) %>%
  unique() %>%
  filter(!is.na(manual_pubertal_age))
tempo_zscore_maker <- General_Exclude %>%
  select(src_subject_id, manual_pubertal_tempo) %>%
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
  left_join(age_zscore_maker, by = 'src_subject_id') %>%
  left_join(tempo_zscore_maker, by = 'src_subject_id')

# Reorganize for export
General_Exclude <- General_Exclude %>%
  select(src_subject_id, eventname, sex, interview_age, manual_pubertal_age, manual_pubertal_age_zscore, manual_pubertal_tempo, manual_pubertal_tempo_zscore, everything())

#################
# IN THE BELOW CODE BLOCK
# Export to CSV
#################

write_csv(General_Exclude, paste0(derivative_data, "General_Puberty_5.0_Age_Tempo_2_27_26.csv"))
