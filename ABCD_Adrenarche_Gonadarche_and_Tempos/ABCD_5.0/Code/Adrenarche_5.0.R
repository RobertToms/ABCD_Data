################
# Adrenarche_5.0.R
# Robert Toms, robert.toms@utdallas.edu
# 1/30/2026 --> Faith Crighton (02/27/26)
################

# The purpose of this code is to calculate age at Adrenarche and Adrenarche Tempo in males and females.

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

table(sex$sex)
# F      IM    M 
# 5677    3 6188 

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

# count 999s (Don't Know) and 777s (Refuse to Answer)
table(pds_sex$body_hair == 999, pds_sex$sex) # 859 total Body Hair "Don't Know" reports; 460 F / 399 M / 0 IM
table(pds_sex$skin_pimples == 999, pds_sex$sex) # 1997 total Skin Change "Don't Know" reports; 885 F / 1111 M / 1 IM
# 859 + 1997 = 2856 total "Don't Know reports; 1345 F / 1510 M / 1 IM

pds_999s <- pds_sex %>% filter(pds_sex$body_hair == 999 | pds_sex$skin_pimples == 999)
table(pds_999s$sex) # 1184 F / 1363 M / 1 IM; 2548 total timepoints affected by "Don't Know" reports
subs_999 <- pds_999s %>% distinct(src_subject_id) %>% pull(src_subject_id) # 2548 timepoints, 2405 subjects affected
sex_999 <- pds_999s %>% select(src_subject_id, sex) %>% unique()
table(sex_999$sex) # 2404 subjects affected by "Don't Know" reports; 1124 F / 1280 M / 1 IM

table(pds_sex$body_hair == 777, pds_sex$sex) # 793 total Body Hair "Refuse to Answer" reports; 488 F / 305 M
table(pds_sex$skin_pimples == 777, pds_sex$sex) # 398 total Skin Changes "Refuse to Answer" reports; 164 F / 234 M
# 793 + 398 = 1191 total "Refuse to Answer" reports; 652 F / 539 M

pds_777s <- pds_sex %>% filter(pds_sex$body_hair == 777 | pds_sex$skin_pimples == 777)
table(pds_777s$sex) # 559 F / 437 M, 996 total timepoints affected by "Refuse to Answer" reports
subs_777 <- pds_777s %>% distinct(src_subject_id) %>% pull(src_subject_id) # 996 timepoints, 817 subjects affected
sex_777 <- pds_777s %>% select(src_subject_id, sex) %>% unique()
table(sex_777$sex) # 817 subjects affected by "Refuse to Answer" reports; 442 F / 375 M

# Count subjects
subjects_counter <- pds_sex %>% distinct(src_subject_id) %>% pull(src_subject_id) # 11868


# Set 999s and 777s to NA, per Herting et al., 2021 best practices
pds_sex <- pds_sex %>%
  mutate(across(c(body_hair, 
                  skin_pimples
  ), ~case_when(
    . == 999 ~ NA,
    . == 777 ~ NA,
    TRUE ~ .)))

# Count remaining subjects
# subjects_counter <- pds_sex %>% distinct(src_subject_id) %>% pull(src_subject_id)

#################
# IN THE BELOW CODE BLOCK
# Manually Calculate PDS Values
#################

# Adrenarche PDS: Average of "Body Hair" and "Skin Changes" PDS scores, must have both to have a valid Adrenarche PDS score.

# calculate PDS values
pds_sex <- pds_sex %>%
  rowwise() %>%
  # Adrenarche PDS
  mutate(adrenarche_pds = sum(body_hair, skin_pimples)) %>% # if either is NA, the sum will be NA
  mutate(adrenarche_pds = adrenarche_pds / 2)

#################
# IN THE BELOW CODE BLOCK
# Count subjects with no data or insufficient data to impute ages/tempos (< 2 timepoints of data)
#################

# Adrenarche
pds_counts <- pds_sex %>% group_by(src_subject_id) %>% mutate(num_timepoints = sum(!is.na(adrenarche_pds))) %>% ungroup() %>% select(src_subject_id, num_timepoints, sex) %>% unique()
table(pds_counts$num_timepoints, pds_counts$sex) # 94 total, 51F/43M subjects with no adrenarche data; 455 total, 250F/205M with 1 timepoint
pds_counts <- pds_counts %>% filter(num_timepoints > 1)
table(pds_counts$sex) # 5376 F / 5940 M / 3 IM; 11319 total subjects remain with 2+ timepoints of adrenarche data

# incorporate exclusions
pds_sex <- pds_sex %>% group_by(src_subject_id) %>% mutate(num_timepoints = sum(!is.na(adrenarche_pds))) %>% ungroup() %>% filter(num_timepoints > 1)
subs_count <- pds_sex %>% distinct(src_subject_id) %>% pull(src_subject_id) # 11319 subjects remain, sanity checked

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
  select(src_subject_id, eventname, body_hair, skin_pimples)

# make list values for is_inconsistent to iterate thru
pds_decreasing <- pds_decreasing %>%
  group_by(src_subject_id) %>%
  mutate(body_values = list(na.omit(body_hair)),
         skin_values = list(na.omit(skin_pimples)))

# apply determine_decreasing function
pds_decreasing <- pds_decreasing %>%
  rowwise() %>%
  mutate(body_decreasing_Y1N0 = determine_decreasing(body_values),
         skin_decreasing_Y1N0 = determine_decreasing(skin_values))

# count inconsistencies in each pubertal category
pds_decreasing <- pds_decreasing %>%
  mutate(num_adren_decreasing = sum(body_decreasing_Y1N0, skin_decreasing_Y1N0, na.rm = TRUE))

# keep only useful columns
pds_decreasing <- pds_decreasing %>%
  select(src_subject_id, num_adren_decreasing) %>%
  unique()

#################
# IN THE BELOW CODE BLOCK
# Calculate Age at Adrenarche
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
      adrenarche_slope = find_slope(.x, "adrenarche_pds"),
      adrenarche_intercept = find_intercept(.x, "adrenarche_pds")
    )
  })

# Calculate Onset Ages: Age where PDS = 2.0
arche_slopes <- arche_slopes %>%
  mutate(adrenarche_age = (2.0 - adrenarche_intercept) / adrenarche_slope)

#################
# IN THE BELOW CODE BLOCK
# Remove Ages and Slopes based on faulty self-report data
#################

# isolate adrenarche ages/tempos and rename adrenarche tempos
age_tempo <- arche_slopes %>%
  select(src_subject_id, 
         adrenarche_age, adrenarche_slope) %>%
  rename(adrenarche_tempo = adrenarche_slope)

# merge in decreasing counts
age_tempo <- age_tempo %>%
  left_join(pds_decreasing, by = 'src_subject_id')

# flag if any relevant self reports indicate reverse puberty
age_tempo <- age_tempo %>%
  mutate(Has_Decrease_Y1N0 = if_else(num_adren_decreasing > 0, 1, 0))

## COUNT EXCLUSIONS
age_tempo <- age_tempo %>% left_join(sex, by = 'src_subject_id')

table(age_tempo$Has_Decrease_Y1N0, age_tempo$sex)
#      F   IM    M  total: 11319
# 0 2821    1 2202 - 5024 subjects with valid Adrenarche data
# 1 2555    2 3738 - 6295 subjects excluded for inconsistent reporting/reported pubertal regression


## Set relevant exclusions to NA
age_tempo <- age_tempo %>%
  rowwise() %>%
  mutate(adrenarche_age = if_else(Has_Decrease_Y1N0 == 1, NA, adrenarche_age),
         adrenarche_tempo = if_else(Has_Decrease_Y1N0 == 1, NA, adrenarche_tempo))

sum(!is.na(age_tempo$adrenarche_age)) # 5019
sum(!is.na(age_tempo$adrenarche_tempo)) # 5024
# 5 NaN Adrenarche Ages, reset to Inf to get counted and removed a little later
age_tempo <- age_tempo %>%
  mutate(adrenarche_age = if_else(is.nan(adrenarche_age), Inf, adrenarche_age))

sum(!is.na(age_tempo$adrenarche_age)) # 5024
sum(!is.na(age_tempo$adrenarche_tempo)) # 5024


#################
# IN THE BELOW CODE BLOCK
# Clean and Organize
#################

# isolate pds items and counts
pubertal_items <- pds_sex %>%
  select(src_subject_id, eventname, sex, interview_age,
         body_hair, skin_pimples, adrenarche_pds)

# merge items and ages
pubertal_timing <- pubertal_items %>%
  select(-sex) %>%
  left_join(age_tempo, by = "src_subject_id")

# reorder for clarity and readability
pubertal_timing <- pubertal_timing %>%
  select(src_subject_id, eventname, sex, interview_age,
         adrenarche_age, adrenarche_tempo,
         everything())

#################
# IN THE BELOW CODE BLOCK
# Validity-Check Data
#################

### AGES

# count Inf's - Ages would be calculated as "infinite" if there is no pubertal progress, i.e. a score of 2 at every report
sum(is.infinite(pubertal_timing$adrenarche_age)) # 482 rows
inf_count <- pubertal_timing %>% select(src_subject_id, adrenarche_age, sex) %>% filter(is.infinite(adrenarche_age)) %>% unique() 
inf_count %>% distinct(src_subject_id) %>% nrow() # 117 subjects have "infinite" adrenarche ages, indicating no pubertal progress
table(inf_count$sex) # 44 F / 73 M reporting no pubertal progress

# set Inf's to NA for Age, and 0s to NA for slope (these mathematically co-occur for subjects, when there is no pubertal progression at all)
pubertal_timing <- pubertal_timing %>%
  mutate(adrenarche_age = if_else(is.infinite(adrenarche_age), NA, adrenarche_age)) %>%
  mutate(adrenarche_tempo = if_else(adrenarche_tempo == 0, NA, adrenarche_tempo)) 
# just as puberty does not go in reverse, it does not stagnate

# check age mean
mean(pubertal_timing$adrenarche_age, na.rm = TRUE)

# count before resets/exclusions
pubertal_timing %>%  filter(!is.na(adrenarche_age)) %>%  distinct(src_subject_id) %>%  nrow()    # 5024 -> 4907, 117 subjects dropped for Inf
pubertal_timing %>%  filter(!is.na(adrenarche_tempo)) %>%  distinct(src_subject_id) %>%  nrow()  # 5024 -> 4907, 117 subjects dropped for Inf

# count sex
subject_counter <- pubertal_timing %>% filter(!is.na(adrenarche_age)) %>% select(src_subject_id, adrenarche_age, sex) %>% unique()
table(subject_counter$sex) # 2777 F, 2129 M / 1 IM ; 4907 total subjects remain


### Reset Age at Adrenarche, Gonadarche, and General Pubertal Onsets to NA if ages are negative or have not yet occurred.
# In an effort to remove impossible and unconfirmed pubertal onset ages, 
# those occurring before birth or in the future (in relation to inteview age) are reset to NA.

# NEGATIVE AGES
# count negative and post-interview ages
subject_counter <- pubertal_timing %>% filter(adrenarche_age < 0) %>% select(src_subject_id, adrenarche_age, sex) %>% unique()
table(subject_counter$sex) # 30 F, 49 M; 79 total subjects have negative calculated Adrenarche ages

# reset to NA
pubertal_timing <- pubertal_timing %>%
  mutate(adrenarche_age = if_else(adrenarche_age < 0, NA, adrenarche_age))

# check age means
mean(pubertal_timing$adrenarche_age, na.rm = TRUE)

# count after resets/exclusions
pubertal_timing %>% filter(!is.na(adrenarche_age)) %>% distinct(src_subject_id) %>% nrow() # 4907 -> 4828; 79 removed

# AGES IMPUTED TO BE AFTER LAST INTERVIEW
subject_counter <- pubertal_timing %>% filter(adrenarche_age > interview_age) %>% select(src_subject_id, sex) %>% unique()
table(subject_counter$sex) # 2188 F, 1896 M, 1 IM subjects affected (not all will be excluded, some may have a later report after their calculated adrenarche age)

# count M/F before
subject_counter <- pubertal_timing %>% filter(!is.na(adrenarche_age)) %>% select(src_subject_id, sex) %>% unique()
table(subject_counter$sex) # 2747 F, 2080 M, 1 IM; 4828 total

# reset to NA
pubertal_timing <- pubertal_timing %>%
  mutate(adrenarche_age = if_else(adrenarche_age > interview_age, NA, adrenarche_age))

# count M/F after
subject_counter <- pubertal_timing %>% filter(!is.na(adrenarche_age)) %>% select(src_subject_id, sex) %>% unique()
table(subject_counter$sex) # 2747 -> 2516 F, 2080 -> 1688 M; 231F / 392M (623) subjects removed for their imputed age of Adrenarche being after their last age at interview

# count subjects remaining
pubertal_timing %>% filter(!is.na(adrenarche_age)) %>% distinct(src_subject_id) %>% nrow()       # 4828 -> 4205; 623 total subjects removed
# 231 + 392= 623

### TEMPOS
# Negative Tempos
# count before resets/exclusionss
pubertal_timing %>% filter(!is.na(adrenarche_tempo)) %>%  distinct(src_subject_id) %>% nrow() # 4907 total subjects with tempos remaining

# count negative tempos
pubertal_timing %>% filter(adrenarche_tempo < 0) %>% distinct(src_subject_id) %>% nrow() # 115 subjects with negative tempos

# count M/F before
subject_counter <- pubertal_timing %>% filter(!is.na(adrenarche_tempo)) %>% select(src_subject_id, sex) %>% unique()
table(subject_counter$sex) # 2777 F, 2129 M, 1 IM; 4907 total

# Reset Tempos to NA if they are negative, which would indicate reverse puberty
pubertal_timing <- pubertal_timing %>%
  mutate(adrenarche_tempo = if_else(adrenarche_tempo < 0, NA, adrenarche_tempo))

# count M/F after
subject_counter <- pubertal_timing %>% filter(!is.na(adrenarche_tempo)) %>% select(src_subject_id, sex) %>% unique()
table(subject_counter$sex) # 2721 F, 2070 M, 1 IM; removed for negative tempos indicating pubertal regression: 56 F, 59 M; 115 total

# count after resets/exclusions
pubertal_timing %>% filter(!is.na(adrenarche_tempo)) %>%  distinct(src_subject_id) %>% nrow() # 4907 -> 4792; 115 removed

#################
# IN THE BELOW CODE BLOCK
# Make Flags for Post-Adrenarche at first report and Pre-Adrenarche at last report
#################

## Adrenarche at First Report
# If first report had a PDS score of 2.0 or greater, flag as Adrenarche at first report
pubertal_timing <- pubertal_timing %>%
  group_by(src_subject_id) %>%
  mutate(first_adren_pds = na.omit(adrenarche_pds)[1]) %>%
  mutate(PostAdrenarche_FirstReport_Y1N0 = if_else(first_adren_pds >= 2.0, 1, 0))

# PrePubertal at Last Report
pubertal_timing <- pubertal_timing %>%
  group_by(src_subject_id) %>%
  mutate(last_adren_pds = adrenarche_pds[max(which(!is.na(adrenarche_pds)))]) %>%
  mutate(PreAdrenarche_Y1N0 = if_else(last_adren_pds < 2.0, 1, 0))

# Sanity check -- is anyone both?
table(pubertal_timing$PostAdrenarche_FirstReport_Y1N0, pubertal_timing$PreAdrenarche_Y1N0)
# yes
pubertal_timing %>% filter(PostAdrenarche_FirstReport_Y1N0 == 1 & PreAdrenarche_Y1N0 == 1) %>% filter(!is.na(adrenarche_age) | !is.na(adrenarche_tempo)) %>% nrow()
# none of them have valid tempos or ages remaining

#################
# IN THE BELOW CODE BLOCK
# Clean & Organize
#################

# keep only useful columns
adrenarche <- pubertal_timing %>%
  select(src_subject_id, eventname, sex, interview_age, adrenarche_age, adrenarche_tempo, 
         adrenarche_pds, body_hair, skin_pimples, num_adren_decreasing, Has_Decrease_Y1N0, PostAdrenarche_FirstReport_Y1N0, PreAdrenarche_Y1N0)

#################
# IN THE BELOW CODE BLOCK
# Make Dataframe that removes exclusions and get counts
#################

# count subjects
counter <- adrenarche %>% filter(!is.na(adrenarche_age)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 2516 F / 1688 M / 1 IM; 4205 ages remain
counter <- adrenarche %>% filter(!is.na(adrenarche_tempo)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 2721 F / 2070 M / 1 IM; 4792 tempos remain

# has a relevant decreasing value/inconsistently reported (already reset to NA, shouldn't change counts)
Adrenarche_Exclude <- adrenarche %>%
  mutate(adrenarche_age = if_else(Has_Decrease_Y1N0 == 1, NA, adrenarche_age)) %>%
  mutate(adrenarche_tempo = if_else(Has_Decrease_Y1N0 == 1, NA, adrenarche_tempo))

# count subjects
counter <- Adrenarche_Exclude %>% filter(!is.na(adrenarche_age)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 2516 F / 1688 M / 1 IM; 4205 ages remain
counter <- Adrenarche_Exclude %>% filter(!is.na(adrenarche_tempo)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 2721 F / 2070 M / 1 IM; 4792 tempos remain

### TEMPOS will not be excluded for being Post-Adrenarche at First Report or Pre-Adrenarche at Last Report.
###        Even if they do not experience an Adrenarche transition during the course of data collection, the rate of pubertal change is still valid.
### AGES will be excluded for these cases -- imputed ages are not valid if the subject has not yet experienced the event to which they refer
###                                       -- imputed ages are not valid if we cannot determine the age just prior to the occurrence of the event.

# post at first report
Adrenarche_Exclude <- Adrenarche_Exclude %>% 
  mutate(adrenarche_age = if_else(PostAdrenarche_FirstReport_Y1N0 == 1, NA, adrenarche_age))

# count subjects
counter <- Adrenarche_Exclude %>% filter(!is.na(adrenarche_age)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 1704 F / 1365 M / 1 IM; 4205 -> 3070 ages remain; 812 F / 323 M, 1135 total

# pre at last report
Adrenarche_Exclude <- Adrenarche_Exclude %>%
  mutate(adrenarche_age = if_else(PreAdrenarche_Y1N0 == 1, NA, adrenarche_age))

# count subjects
counter <- Adrenarche_Exclude %>% filter(!is.na(adrenarche_age)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 1701 F / 1363 M / 1 IM; 3070 -> 3065 ages remain, 3 F / 2 M removed

#################
# IN THE BELOW CODE BLOCK
# Make Z-Scores of Age and Tempo, merge in
#################

# must be split off and have 1 row per subject, so distributions are accurate when calculating Z-scores
age_zscore_maker <- Adrenarche_Exclude %>%
  select(src_subject_id, adrenarche_age) %>%
  unique() %>%
  filter(!is.na(adrenarche_age))
tempo_zscore_maker <- Adrenarche_Exclude %>%
  select(src_subject_id, adrenarche_tempo) %>%
  unique() %>%
  filter(!is.na(adrenarche_tempo))

# calculate z-scores
age_zscore_maker$adrenarche_age_zscore <- as.numeric(scale(age_zscore_maker$adrenarche_age))
tempo_zscore_maker$adrenarche_tempo_zscore <- as.numeric(scale(tempo_zscore_maker$adrenarche_tempo))

# remove superfluous column before merging back in
age_zscore_maker <- age_zscore_maker %>% select(-adrenarche_age)
tempo_zscore_maker <- tempo_zscore_maker %>% select(-adrenarche_tempo)

# merge in
Adrenarche_Exclude <- Adrenarche_Exclude %>%
  left_join(age_zscore_maker, by = 'src_subject_id') %>%
  left_join(tempo_zscore_maker, by = 'src_subject_id')

# Reorganize for export
Adrenarche_Exclude <- Adrenarche_Exclude %>%
  select(src_subject_id, eventname, sex, interview_age, adrenarche_age, adrenarche_age_zscore, adrenarche_tempo, adrenarche_tempo_zscore, everything())

#################
# IN THE BELOW CODE BLOCK
# Export to CSV
#################

write_csv(Adrenarche_Exclude, paste0(derivative_data, "Adrenarche_5.0_Age_Tempo_2_27_26.csv"))

