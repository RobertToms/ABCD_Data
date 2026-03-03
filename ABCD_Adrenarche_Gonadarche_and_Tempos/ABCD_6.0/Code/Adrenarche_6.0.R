################
# Adrenarche_6.0.R
# Robert Toms, robert.toms@utdallas.edu
# 2/20/2026
################

# The purpose of this code is to calculate age at Adrenarche and Adrenarche Tempo in males and females.

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
table(pds_sex$body_hair == 999, pds_sex$sex) # 859 total Body Hair "Don't Know" reports; 460 F / 399 M
table(pds_sex$skin_pimples == 999, pds_sex$sex) # 1996 total Skin Change "Don't Know" reports; 885 F / 1111 M
# 859 + 1996 = 2855 total "Don't Know reports; 1345 F / 1510 M

pds_999s <- pds_sex %>% filter(pds_sex$body_hair == 999 | pds_sex$skin_pimples == 999)
table(pds_999s$sex) # 1184 F / 1363 M; 2547 total timepoints affected by "Don't Know" reports
subs_999 <- pds_999s %>% distinct(participant_id) %>% pull(participant_id) # 2547 timepoints, 2404 subjects affected
sex_999 <- pds_999s %>% select(participant_id, sex) %>% unique()
table(sex_999$sex) # 2404 subjects affected by "Don't Know" reports; 1124 F / 1280 M

table(pds_sex$body_hair == 777, pds_sex$sex) # 985 total Body Hair "Refuse to Answer" reports; 610 F / 375 M
table(pds_sex$skin_pimples == 777, pds_sex$sex) # 507 total Skin Changes "Refuse to Answer" reports; 214 F / 293 M
# 985 + 507 = 1492 total "Refuse to Answer" reports; 824 F / 668 M

pds_777s <- pds_sex %>% filter(pds_sex$body_hair == 777 | pds_sex$skin_pimples == 777)
table(pds_777s$sex) # 700 F / 542 M, 1242 total timepoints affected by "Refuse to Answer" reports
subs_777 <- pds_777s %>% distinct(participant_id) %>% pull(participant_id) # 1242 timepoints, 949 subjects affected
sex_777 <- pds_777s %>% select(participant_id, sex) %>% unique()
table(sex_777$sex) # 949 subjects affected by "Refuse to Answer" reports; 515 F / 434 M

# Count subjects
subjects_counter <- pds_sex %>% distinct(participant_id) %>% pull(participant_id) # 11866

# 2 subjects missing, who and why?
all_subjects <- sex %>% distinct(participant_id) %>% pull(participant_id) 
missing <- setdiff(all_subjects, subjects_counter)
# sub-C2ZW147D -- male, no parent or youth pds data, only baseline data in ab_g_dyn
# sub-V4AXRCZ6 -- male, no youth pds data, only baseline parent pds and ab_g_dyn data

# Set 999s and 777s to NA, per Herting et al., 2021 best practices
pds_sex <- pds_sex %>%
  mutate(across(c(body_hair, 
                  skin_pimples
  ), ~case_when(
    . == 999 ~ NA,
    . == 777 ~ NA,
    TRUE ~ .)))

# Count remaining subjects
subjects_counter <- pds_sex %>% distinct(participant_id) %>% pull(participant_id)

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

#### Females
# Adrenarche
pds_counts <- pds_sex %>% group_by(participant_id) %>% mutate(num_timepoints = sum(!is.na(adrenarche_pds))) %>% ungroup() %>% select(participant_id, num_timepoints, sex) %>% unique()
table(pds_counts$num_timepoints, pds_counts$sex) # 78 total, 41F/37M subjects with no adrenarche data; 365 total, 201F/164M with 1 timepoint
pds_counts <- pds_counts %>% filter(num_timepoints > 1)
table(pds_counts$sex) # 5436 F / 5987 M; 11423 total subjects remain with 2+ timepoints of adrenarche data

# incorporate exclusions
pds_sex <- pds_sex %>% group_by(participant_id) %>% mutate(num_timepoints = sum(!is.na(adrenarche_pds))) %>% ungroup() %>% filter(num_timepoints > 1)
subs_count <- pds_sex %>% distinct(participant_id) %>% pull(participant_id) # 11423 subjects remain, sanity checked

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
  select(participant_id, session_id, body_hair, skin_pimples)

# male list values for is_inconsistent to iterate thru
pds_decreasing <- pds_decreasing %>%
  group_by(participant_id) %>%
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
  select(participant_id, num_adren_decreasing) %>%
  unique()

#################
# IN THE BELOW CODE BLOCK
# Calculate Age at Adrenarche
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
  select(participant_id, 
         adrenarche_age, adrenarche_slope) %>%
  rename(adrenarche_tempo = adrenarche_slope)

# merge in decreasing counts
age_tempo <- age_tempo %>%
  left_join(pds_decreasing, by = 'participant_id')

# flag if any relevant self reports indicate reverse puberty
age_tempo <- age_tempo %>%
  mutate(Has_Decrease_Y1N0 = if_else(num_adren_decreasing > 0, 1, 0))

## COUNT EXCLUSIONS
age_tempo <- age_tempo %>% left_join(sex, by = 'participant_id')

table(age_tempo$Has_Decrease_Y1N0, age_tempo$sex)
#     F    M     total: 11423
# 0 2247 1724  - 3971 subjects with valid Adrenarche data
# 1 3189 4263  - 7452 subjects excluded for inconsistent reporting/reported pubertal regression

## Set relevant exclusions to NA
age_tempo <- age_tempo %>%
  rowwise() %>%
  mutate(adrenarche_age = if_else(Has_Decrease_Y1N0 == 1, NA, adrenarche_age),
         adrenarche_tempo = if_else(Has_Decrease_Y1N0 == 1, NA, adrenarche_tempo))

#################
# IN THE BELOW CODE BLOCK
# Clean and Organize
#################

# isolate pds items and counts
pubertal_items <- pds_sex %>%
  select(participant_id, session_id, sex, interview_age_yrs,
         body_hair, skin_pimples, adrenarche_pds)

# merge items and ages
pubertal_timing <- pubertal_items %>%
  select(-sex) %>%
  left_join(age_tempo, by = "participant_id")

# reorder for clarity and readability
pubertal_timing <- pubertal_timing %>%
  select(participant_id, session_id, sex, interview_age_yrs,
         adrenarche_age, adrenarche_tempo,
         everything())

#################
# IN THE BELOW CODE BLOCK
# Validity-Check Data
#################

### AGES

# count Inf's - Ages would be calculated as "infinite" if there is no pubertal progress, i.e. a score of 2 at every report
sum(is.infinite(pubertal_timing$adrenarche_age)) # 106 rows
inf_count <- pubertal_timing %>% select(participant_id, adrenarche_age, sex) %>% filter(is.infinite(adrenarche_age)) %>% unique() 
inf_count %>% distinct(participant_id) %>% nrow() # 22 subjects have "infinite" adrenarche ages, indicating no pubertal progress
table(inf_count$sex) # 11 F / 11 M reporting no pubertal progress

# set Inf's to NA for Age, and 0s to NA for slope (these mathematically co-occur for subjects, when there is no pubertal progression at all)
pubertal_timing <- pubertal_timing %>%
  mutate(adrenarche_age = if_else(is.infinite(adrenarche_age), NA, adrenarche_age)) %>%
  mutate(adrenarche_tempo = if_else(adrenarche_tempo == 0, NA, adrenarche_tempo)) 
# just as puberty does not go in reverse, it does not stagnate

# check age mean
mean(pubertal_timing$adrenarche_age, na.rm = TRUE)

# count before resets/exclusions
pubertal_timing %>%  filter(!is.na(adrenarche_age)) %>%  distinct(participant_id) %>%  nrow()    # 3971 -> 3949, 22 subjects dropped
pubertal_timing %>%  filter(!is.na(adrenarche_tempo)) %>%  distinct(participant_id) %>%  nrow()  # 3971 -> 3949, 22 subjects dropped

# count sex
subject_counter <- pubertal_timing %>% filter(!is.na(adrenarche_age)) %>% select(participant_id, adrenarche_age, sex) %>% unique()
table(subject_counter$sex) # 2236 F, 1713 M ; 3949 total subjects remain


### Reset Age at Adrenarche, Gonadarche, and General Pubertal Onsets to NA if ages are negative or have not yet occurred.
# In an effort to remove impossible and unconfirmed pubertal onset ages, 
# those occurring before birth or in the future (in relation to inteview age) are reset to NA.

# NEGATIVE AGES
# count negative and post-interview ages
subject_counter <- pubertal_timing %>% filter(adrenarche_age < 0) %>% select(participant_id, adrenarche_age, sex) %>% unique()
table(subject_counter$sex) # 33 F, 17 M; 50 total subjects have negative calculated Adrenarche ages

# reset to NA
pubertal_timing <- pubertal_timing %>%
  mutate(adrenarche_age = if_else(adrenarche_age < 0, NA, adrenarche_age))

# check age means
mean(pubertal_timing$adrenarche_age, na.rm = TRUE)

# count after resets/exclusions
pubertal_timing %>% filter(!is.na(adrenarche_age)) %>% distinct(participant_id) %>% nrow() # 3949 -> 3899; 50 removed

# AGES IMPUTED TO BE AFTER LAST INTERVIEW
subject_counter <- pubertal_timing %>% filter(adrenarche_age > interview_age_yrs) %>% select(participant_id, sex) %>% unique()
table(subject_counter$sex) # 1624 F, 1507 M subjects affected (not all will be excluded, some may have a later report after their calculated adrenarche age)

# count M/F before
subject_counter <- pubertal_timing %>% filter(!is.na(adrenarche_age)) %>% select(participant_id, sex) %>% unique()
table(subject_counter$sex) # 2203 F, 1696 M

# reset to NA
pubertal_timing <- pubertal_timing %>%
  mutate(adrenarche_age = if_else(adrenarche_age > interview_age_yrs, NA, adrenarche_age))

# count M/F after
subject_counter <- pubertal_timing %>% filter(!is.na(adrenarche_age)) %>% select(participant_id, sex) %>% unique()
table(subject_counter$sex) # 2203 -> 2125 F, 1696 -> 1569 M; 78F / 147M subjects removed for their imputed age of Adrenarche being after their last age at interview

# count subjects remaining
pubertal_timing %>% filter(!is.na(adrenarche_age)) %>% distinct(participant_id) %>% nrow()       # 3899 -> 3694; 205 total subjects removed
# 78 + 147 = 205

### TEMPOS
# Negative Tempos
# count before resets/exclusionss
pubertal_timing %>%  filter(!is.na(adrenarche_tempo)) %>%  distinct(participant_id) %>% nrow() # 3949 total subjects with tempos remaining

# count negative tempos
pubertal_timing %>% filter(adrenarche_tempo < 0) %>% distinct(participant_id) %>% nrow() # 40 subjects with negative tempos

# count M/F before
subject_counter <- pubertal_timing %>% filter(!is.na(adrenarche_tempo)) %>% select(participant_id, sex) %>% unique()
table(subject_counter$sex) # 2236 F, 1713 M

# Reset Tempos to NA if they are negative, which would indicate reverse puberty
pubertal_timing <- pubertal_timing %>%
  mutate(adrenarche_tempo = if_else(adrenarche_tempo < 0, NA, adrenarche_tempo))

# count M/F after
subject_counter <- pubertal_timing %>% filter(!is.na(adrenarche_tempo)) %>% select(participant_id, sex) %>% unique()
table(subject_counter$sex) # 2217 F, 1692 M; removed for negative tempos indicating pubertal regression: 19 F, 21 M

# count after resets/exclusions
pubertal_timing %>% filter(!is.na(adrenarche_tempo)) %>%  distinct(participant_id) %>% nrow() # 3949 -> 3909; 40 removed

#################
# IN THE BELOW CODE BLOCK
# Make Flags for Post-Adrenarche at first report and Pre-Adrenarche at last report
#################

## Adrenarche at First Report
# If first report had a PDS score of 2.0 or greater, flag as the appropriate Arche at first report
pubertal_timing <- pubertal_timing %>%
  group_by(participant_id) %>%
  mutate(first_adren_pds = na.omit(adrenarche_pds)[1]) %>%
  mutate(PostAdrenarche_FirstReport_Y1N0 = if_else(first_adren_pds >= 2.0, 1, 0))

# PrePubertal at Last Report
pubertal_timing <- pubertal_timing %>%
  group_by(participant_id) %>%
  mutate(last_adren_pds = adrenarche_pds[max(which(!is.na(adrenarche_pds)))]) %>%
  mutate(PreAdrenarche_Y1N0 = if_else(last_adren_pds < 2.0, 1, 0))

# Sanity check -- is anyone both?
table(pubertal_timing$PostAdrenarche_FirstReport_Y1N0, pubertal_timing$PreAdrenarche_Y1N0)
# yes -- but none of them have valid tempos or ages remaining

#################
# IN THE BELOW CODE BLOCK
# Clean & Organize
#################

# keep only useful columns
adrenarche <- pubertal_timing %>%
  select(participant_id, session_id, sex, interview_age_yrs, adrenarche_age, adrenarche_tempo, 
         adrenarche_pds, body_hair, skin_pimples, num_adren_decreasing, Has_Decrease_Y1N0, PostAdrenarche_FirstReport_Y1N0, PreAdrenarche_Y1N0)

#################
# IN THE BELOW CODE BLOCK
# Make Dataframe that removes exclusions and get counts
#################

# count subjects
counter <- adrenarche %>% filter(!is.na(adrenarche_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 2125 F / 1569 M; 3694 ages remain
counter <- adrenarche %>% filter(!is.na(adrenarche_tempo)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 2217 F / 1692 M; 3909 tempos remain

# has a relevant decreasing value/inconsistently reported (already reset to NA, shouldn't change counts)
Adrenarche_Exclude <- adrenarche %>%
  mutate(adrenarche_age = if_else(Has_Decrease_Y1N0 == 1, NA, adrenarche_age))
#%>%  mutate(adrenarche_tempo = if_else(Has_Decrease_Y1N0 == 1, NA, adrenarche_tempo))

# count subjects
counter <- Adrenarche_Exclude %>% filter(!is.na(adrenarche_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 2125 F / 1569 M; 3694 ages remain
counter <- Adrenarche_Exclude %>% filter(!is.na(adrenarche_tempo)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 2217 F / 1692 M; 3909 tempos remain

# post at first report
Adrenarche_Exclude <- Adrenarche_Exclude %>% 
  mutate(adrenarche_age = if_else(PostAdrenarche_FirstReport_Y1N0 == 1, NA, adrenarche_age))
#%>%  mutate(adrenarche_tempo = if_else(PostAdrenarche_FirstReport_Y1N0 == 1, NA, adrenarche_tempo))

### TEMPOS will not be excluded for being Post-Adrenarche at First Report or Pre-Adrenarche at Last Report.
###        Even if they do not experience an Adrenarche transition during the course of data collection, the rate of pubertal change is still valid.
### AGES will be excluded for these cases -- imputed ages are not valid if the subject has not yet experienced the event to which they refer
###                                       -- imputed ages are not valid if we cannot determine the age just prior to the occurrence of the event.

# count subjects
counter <- Adrenarche_Exclude %>% filter(!is.na(adrenarche_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 1452 F / 1314 M; 3694 -> 2766 ages remain; 928 removed, 673 F / 255M

# pre at last report
Adrenarche_Exclude <- Adrenarche_Exclude %>% 
  mutate(adrenarche_age = if_else(PreAdrenarche_Y1N0 == 1, NA, adrenarche_age))

# count subjects
counter <- Adrenarche_Exclude %>% filter(!is.na(adrenarche_age)) %>% select(participant_id, sex) %>% unique()
table(counter$sex) # 1452 F / 1314 M; 0 removed

#################
# IN THE BELOW CODE BLOCK
# Make Z-Scores of Age and Tempo, merge in
#################

# must be split off and have 1 row per subject, so distributions are accurate when calculating Z-scores
age_zscore_maker <- Adrenarche_Exclude %>%
  select(participant_id, adrenarche_age) %>%
  unique() %>%
  filter(!is.na(adrenarche_age))
tempo_zscore_maker <- Adrenarche_Exclude %>%
  select(participant_id, adrenarche_tempo) %>%
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
  left_join(age_zscore_maker, by = 'participant_id') %>%
  left_join(tempo_zscore_maker, by = 'participant_id')

# Reorganize for export
Adrenarche_Exclude <- Adrenarche_Exclude %>%
  select(participant_id, session_id, sex, interview_age_yrs, adrenarche_age, adrenarche_age_zscore, adrenarche_tempo, adrenarche_tempo_zscore, everything())

#################
# IN THE BELOW CODE BLOCK
# Export to CSV
#################

write_csv(Adrenarche_Exclude, paste0(derivative_data, "Adrenarche_6.0_Age_Tempo_2_20_26.csv"))
