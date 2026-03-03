################
# Gonadarche_5.0.R
# Robert Toms, robert.toms@utdallas.edu
# 2/27/2026
################

# The purpose of this code is to calculate age at Gonadarche and Gonadarche Tempo in males and females.

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


# count 999s (Don't Know) and 777s (Refuse to Answer)
table(pds_sex$thelarche == 999, pds_sex$sex) # 1131 total Thelarche "Don't Know" reports; 1131 F
table(pds_sex$menarche == 999, pds_sex$sex) # 680 total Menarche "Don't Know" reports; 680 F
table(pds_sex$voice_deep == 999, pds_sex$sex) # 506 total Deepening Voice "Don't Know" reports; 506 M
table(pds_sex$facial_hair == 999, pds_sex$sex) # 402 total Facial Hair Growth "Don't Know" reports; 401 M / 1 IM
# 1131 + 680 = 1811 total Female "Don't Know" reports
# 506 + 401 = 907 total Male "Don't Know" reports, 1 Intersex Male idk report
# 1811 + 907 + 1 = 2719 total "Don't Know" reports

pds_999s <- pds_sex %>% filter((pds_sex$thelarche == 999 | pds_sex$menarche == 999) | 
                                 (pds_sex$voice_deep == 999 | pds_sex$facial_hair == 999))
table(pds_999s$sex) # 1578 F / 762 M / 1 IM; 2341 total timepoints affected by "Don't Know" reports
subs_999 <- pds_999s %>% distinct(src_subject_id) %>% pull(src_subject_id) # 2341 timepoints, 2251 subjects affected
sex_999 <- pds_999s %>% select(src_subject_id, sex) %>% unique()
table(sex_999$sex) # 2251 subjects affected by "Don't Know" reports; 1488 F / 762 M / 1 IM

table(pds_sex$thelarche == 777, pds_sex$sex) # 726 total Thelarche "Refuse to Answer" reports; 725 F / 1 M (what? it'll get NA'd out.)
table(pds_sex$menarche == 777, pds_sex$sex) # 787 total Menarche "Refuse to Answer" reports; 787 F
table(pds_sex$voice_deep == 777, pds_sex$sex) # 134 total Deepening Voice "Refuse to Answer" reports; 134 M
table(pds_sex$facial_hair == 777, pds_sex$sex) # 191 total Facial Hair "Refuse to Answer" reports; 191 M
# 725 + 787 = 1512 total Female "Refuse to Answer" reports
# 1 + 134 + 191 = 326 total Male "Refuse to Answer" reports
# 1512 + 326 = 1838 total "Refuse to Answer" reports

pds_777s <- pds_sex %>% filter((pds_sex$thelarche == 777 | pds_sex$menarche == 777) | 
                                 (pds_sex$voice_deep == 777 | pds_sex$facial_hair == 777))
table(pds_777s$sex) # 1220 F / 251 M, 1471 total timepoints affected by "Refuse to Answer" reports
subs_777 <- pds_777s %>% distinct(src_subject_id) %>% pull(src_subject_id) # 1471 timepoints, 1134 subjects affected
sex_777 <- pds_777s %>% select(src_subject_id, sex) %>% unique()
table(sex_777$sex) # 1134 subjects affected by "Refuse to Answer" reports; 908 F / 226 M

# Count subjects
subjects_counter <- pds_sex %>% distinct(src_subject_id) %>% pull(src_subject_id) # 11868

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
subjects_counter <- pds_sex %>% distinct(src_subject_id) %>% pull(src_subject_id) # still 11868

#################
# IN THE BELOW CODE BLOCK
# Manually Calculate PDS Values
#################

# Gonadarche PDS: 
# Males: Average of "Voice Deepening" and "Facial Hair Growth" PDS scores, must have both to have a valid Gonadarche PDS score.
# Females: Average of "Thelarche" and "Menarche" PDS scores, must have both to have a valid Gonadarche PDS score.

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
pds_counts <- pds_sex %>% group_by(src_subject_id) %>% mutate(num_timepoints = sum(!is.na(gonadarche_pds))) %>% ungroup() %>% select(src_subject_id, num_timepoints, sex) %>% unique()
table(pds_counts$num_timepoints, pds_counts$sex) # 100 total, 83F/14M/3IM subjects with no gonadarche data; 534 total, 335F/199M with 1 timepoint; 634 total
pds_counts <- pds_counts %>% filter(num_timepoints > 1)
table(pds_counts$sex) # 5259 F / 5975 M; 11234 total subjects remain with 2+ timepoints of gonadarche data

# incorporate exclusions
pds_sex <- pds_sex %>% group_by(src_subject_id) %>% mutate(num_timepoints = sum(!is.na(gonadarche_pds))) %>% ungroup() %>% filter(num_timepoints > 1)
subs_count <- pds_sex %>% distinct(src_subject_id) %>% pull(src_subject_id) # 11234 subjects remain, sanity checked

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
  select(src_subject_id, eventname, thelarche, menarche, voice_deep, facial_hair)

# male list values for is_inconsistent to iterate thru
pds_decreasing <- pds_decreasing %>%
  group_by(src_subject_id) %>%
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
  select(src_subject_id, num_gonad_decreasing) %>%
  unique()

#################
# IN THE BELOW CODE BLOCK
# Calculate Age at Gonadarche
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
  select(src_subject_id, 
         gonadarche_age, gonadarche_slope) %>%
  rename(gonadarche_tempo = gonadarche_slope)

# merge in decreasing counts
age_tempo <- age_tempo %>%
  left_join(pds_decreasing, by = 'src_subject_id')

# flag if any relevant self reports indicate reverse puberty
age_tempo <- age_tempo %>%
  mutate(Has_Decrease_Y1N0 = if_else(num_gonad_decreasing > 0, 1, 0))

## COUNT EXCLUSIONS
age_tempo <- age_tempo %>% left_join(sex, by = 'src_subject_id')

table(age_tempo$Has_Decrease_Y1N0, age_tempo$sex)
#     F    M     total: 11234
# 0 3968 2583  - 6551 subjects with valid Gonadarche data
# 1 1291 3392  - 4683 subjects excluded for inconsistent reporting/reported pubertal regression

## Set relevant exclusions to NA
age_tempo <- age_tempo %>%
  rowwise() %>%
  mutate(gonadarche_age = if_else(Has_Decrease_Y1N0 == 1, NA, gonadarche_age),
         gonadarche_tempo = if_else(Has_Decrease_Y1N0 == 1, NA, gonadarche_tempo))

# count
age_tempo %>%  filter(!is.na(gonadarche_age)) %>%  distinct(src_subject_id) %>%  nrow()    # 11234 -> 6543
age_tempo %>%  filter(!is.na(gonadarche_tempo)) %>%  distinct(src_subject_id) %>%  nrow()  # 11234 -> 6551

# 8 NaN Adrenarche Ages, reset to Inf to get counted and removed a little later
weird <- age_tempo %>%
  filter(is.na(gonadarche_age) & !is.na(gonadarche_tempo))
age_tempo <- age_tempo %>%
  mutate(gonadarche_age = if_else(is.nan(gonadarche_age), Inf, gonadarche_age))

sum(!is.na(age_tempo$gonadarche_age)) # 6551
sum(!is.na(age_tempo$gonadarche_tempo)) # 6551
# aligned again

#################
# IN THE BELOW CODE BLOCK
# Clean and Organize
#################

# isolate pds items and counts
pubertal_items <- pds_sex %>%
  select(src_subject_id, eventname, sex, interview_age,
         thelarche, menarche, voice_deep, facial_hair, gonadarche_pds)

# merge items and ages
pubertal_timing <- pubertal_items %>%
  select(-sex) %>%
  left_join(age_tempo, by = "src_subject_id")

# reorder for clarity and readability
pubertal_timing <- pubertal_timing %>%
  select(src_subject_id, eventname, sex, interview_age,
         gonadarche_age, gonadarche_tempo,
         everything())

#################
# IN THE BELOW CODE BLOCK
# Validity-Check Data
#################

### AGES

# count Inf's - Ages would be calculated as "infinite" if there is no pubertal progress, i.e. a score of 2 at every report
sum(is.infinite(pubertal_timing$gonadarche_age)) # 1168 rows
sum(pubertal_timing$gonadarche_tempo == 0, na.rm = TRUE) # 1168 rows
inf_count <- pubertal_timing %>% select(src_subject_id, gonadarche_age, sex) %>% filter(is.infinite(gonadarche_age)) %>% unique() 
inf_count %>% distinct(src_subject_id) %>% nrow() # 287 subjects have "infinite" gonadarche ages, indicating no pubertal progress
inf_count <- pubertal_timing %>% select(src_subject_id, gonadarche_tempo, sex) %>% filter(gonadarche_tempo == 0) %>% unique() 
inf_count %>% distinct(src_subject_id) %>% nrow() # 287 subjects have flat gonadarche tempos, indicating no pubertal progress
table(inf_count$sex) # 70 F / 217 M reporting no pubertal progress

# set Inf's to NA for Age, and 0s to NA for slope (these mathematically co-occur for subjects, when there is no pubertal progression at all)
pubertal_timing <- pubertal_timing %>%
  mutate(gonadarche_age = if_else(is.infinite(gonadarche_age) | is.nan(gonadarche_age), NA, gonadarche_age)) %>%
  mutate(gonadarche_tempo = if_else(gonadarche_tempo == 0, NA, gonadarche_tempo)) 
# just as puberty does not go in reverse, it does not stagnate

# check age mean
mean(pubertal_timing$gonadarche_age, na.rm = TRUE)

# count before resets/exclusions
pubertal_timing %>%  filter(!is.na(gonadarche_age)) %>%  distinct(src_subject_id) %>%  nrow()    # 6551 -> 6264, 287 subjects dropped
pubertal_timing %>%  filter(!is.na(gonadarche_tempo)) %>%  distinct(src_subject_id) %>%  nrow()  # 6551 -> 6264, 287 subjects dropped

# count sex
subject_counter <- pubertal_timing %>% filter(!is.na(gonadarche_age)) %>% select(src_subject_id, gonadarche_age, sex) %>% unique()
table(subject_counter$sex) # 3898 F, 2366 M ; 6264 total subjects remain


### Reset Age at Gonadarche, Gonadarche, and General Pubertal Onsets to NA if ages are negative or have not yet occurred.
# In an effort to remove impossible and unconfirmed pubertal onset ages, 
# those occurring before birth or in the future (in relation to inteview age) are reset to NA.

# NEGATIVE AGES
# count negative and post-interview ages
subject_counter <- pubertal_timing %>% filter(gonadarche_age < 0) %>% select(src_subject_id, gonadarche_age, sex) %>% unique()
table(subject_counter$sex) # 129 F, 144 M; 273 total subjects have negative calculated Gonadarche ages

# reset to NA
pubertal_timing <- pubertal_timing %>%
  mutate(gonadarche_age = if_else(gonadarche_age < 0, NA, gonadarche_age))

# check age means
mean(pubertal_timing$gonadarche_age, na.rm = TRUE)

# count after resets/exclusions
pubertal_timing %>% filter(!is.na(gonadarche_age)) %>% distinct(src_subject_id) %>% nrow() # 6264 -> 5991; 273 removed

# AGES IMPUTED TO BE AFTER LAST INTERVIEW
subject_counter <- pubertal_timing %>% filter(gonadarche_age > interview_age) %>% select(src_subject_id, sex) %>% unique()
table(subject_counter$sex) # 3408 F, 2157 M subjects affected (not all will be excluded, some may have a later report after their calculated gonadarche age)

# count M/F before
subject_counter <- pubertal_timing %>% filter(!is.na(gonadarche_age)) %>% select(src_subject_id, sex) %>% unique()
table(subject_counter$sex) # 3769 F, 2222 M

# reset to NA
pubertal_timing <- pubertal_timing %>%
  mutate(gonadarche_age = if_else(gonadarche_age > interview_age, NA, gonadarche_age))

# count M/F after
subject_counter <- pubertal_timing %>% filter(!is.na(gonadarche_age)) %>% select(src_subject_id, sex) %>% unique()
table(subject_counter$sex) # 3769 -> 3127 F, 2222 -> 1350 M; 642F / 872M subjects removed for their imputed age of Gonadarche being after their last age at interview

# count subjects remaining
pubertal_timing %>% filter(!is.na(gonadarche_age)) %>% distinct(src_subject_id) %>% nrow() # 5991 -> 4477; 1514 total subjects removed
# 642 + 872 = 1514

### TEMPOS
# Negative Tempos
# count before resets/exclusionss
pubertal_timing %>%  filter(!is.na(gonadarche_tempo)) %>%  distinct(src_subject_id) %>% nrow() # 6264 total subjects with tempos remaining

# count negative tempos
pubertal_timing %>% filter(gonadarche_tempo < 0) %>% distinct(src_subject_id) %>% nrow() # 273 subjects with negative tempos

# count M/F before
subject_counter <- pubertal_timing %>% filter(!is.na(gonadarche_tempo)) %>% select(src_subject_id, sex) %>% unique()
table(subject_counter$sex) # 3898 F, 2366 M

# Reset Tempos to NA if they are negative, which would indicate reverse puberty
pubertal_timing <- pubertal_timing %>%
  mutate(gonadarche_tempo = if_else(gonadarche_tempo < 0, NA, gonadarche_tempo))

# count M/F after
subject_counter <- pubertal_timing %>% filter(!is.na(gonadarche_tempo)) %>% select(src_subject_id, sex) %>% unique()
table(subject_counter$sex) # 3774 F, 2217 M; removed for negative tempos indicating pubertal regression: -124 F, -149 M; -273

# count after resets/exclusions
pubertal_timing %>% filter(!is.na(gonadarche_tempo)) %>%  distinct(src_subject_id) %>% nrow() # 6264 -> 5991; 273 removed

#################
# IN THE BELOW CODE BLOCK
# Make Flags for Post-Gonadarche at first report and Pre-Gonadarche at last report
#################

## Gonadarche at First Report
# If first report had a PDS score of 2.0 or greater, flag as Gonadarche at first report
pubertal_timing <- pubertal_timing %>%
  group_by(src_subject_id) %>%
  mutate(first_gonad_pds = na.omit(gonadarche_pds)[1]) %>%
  mutate(PostGonadarche_FirstReport_Y1N0 = if_else(first_gonad_pds >= 2.0, 1, 0))

# PrePubertal at Last Report
pubertal_timing <- pubertal_timing %>%
  group_by(src_subject_id) %>%
  mutate(last_gonad_pds = gonadarche_pds[max(which(!is.na(gonadarche_pds)))]) %>%
  mutate(PreGonadarche_Y1N0 = if_else(last_gonad_pds < 2.0, 1, 0))

# Sanity check -- is anyone both?
table(pubertal_timing$PostGonadarche_FirstReport_Y1N0, pubertal_timing$PreGonadarche_Y1N0, pubertal_timing$sex)
pubertal_timing %>% filter(PostGonadarche_FirstReport_Y1N0 == 1 & PreGonadarche_Y1N0 == 1) %>% filter(!is.na(gonadarche_age) | !is.na(gonadarche_tempo)) %>% nrow()
# yes -- but none of them have valid tempos or ages remaining

#################
# IN THE BELOW CODE BLOCK
# Clean & Organize
#################

# keep only useful columns
gonadarche <- pubertal_timing %>%
  select(src_subject_id, eventname, sex, interview_age, gonadarche_age, gonadarche_tempo, 
         gonadarche_pds, thelarche, menarche, voice_deep, facial_hair, num_gonad_decreasing, Has_Decrease_Y1N0, PostGonadarche_FirstReport_Y1N0, PreGonadarche_Y1N0)

#################
# IN THE BELOW CODE BLOCK
# Make Dataframe that removes exclusions and get counts
#################

# count subjects
counter <- gonadarche %>% filter(!is.na(gonadarche_age)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 3127 F / 1350 M; 4477 ages remain
counter <- gonadarche %>% filter(!is.na(gonadarche_tempo)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 3774 F / 2217 M; 5991 tempos remain

# has a relevant decreasing value/inconsistently reported (already reset to NA, shouldn't change counts)
Gonadarche_Exclude <- gonadarche %>%
  mutate(gonadarche_age = if_else(Has_Decrease_Y1N0 == 1, NA, gonadarche_age)) %>% 
  mutate(gonadarche_tempo = if_else(Has_Decrease_Y1N0 == 1, NA, gonadarche_tempo))

# count subjects
counter <- Gonadarche_Exclude %>% filter(!is.na(gonadarche_age)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 3127 F / 1350 M; 4477 ages remain
counter <- Gonadarche_Exclude %>% filter(!is.na(gonadarche_tempo)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 3774 F / 2217 M; 5991 ages remain

### TEMPOS will not be excluded for being Post-Gonadarche at First Report or Pre-Gonadarche at Last Report.
###        Even if they do not experience a Gonadarche transition during the course of data collection, the rate of pubertal change is still valid.
### AGES will be excluded for these cases -- imputed ages are not valid if the subject has not yet experienced the event to which they refer
###                                       -- imputed ages are not valid if we cannot determine the age just prior to the occurrence of the event.

# post at first report
Gonadarche_Exclude <- Gonadarche_Exclude %>%
  mutate(gonadarche_age = if_else(PostGonadarche_FirstReport_Y1N0 == 1, NA, gonadarche_age)) 

# count subjects
counter <- Gonadarche_Exclude %>% filter(!is.na(gonadarche_age)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 2525 F / 1233 M; 4477 -> 3758 ages remain; 602 F / 117 M removed; 719 total

# pre at last report
Gonadarche_Exclude <- Gonadarche_Exclude %>%
  mutate(gonadarche_age = if_else(PreGonadarche_Y1N0 == 1, NA, gonadarche_age))

# count subjects
counter <- Gonadarche_Exclude %>% filter(!is.na(gonadarche_age)) %>% select(src_subject_id, sex) %>% unique()
table(counter$sex) # 2518 F / 1233 M; 3758 -> 3751 ages remain; 7 F removed, -7 total

#################
# IN THE BELOW CODE BLOCK
# Make Z-Scores of Age and Tempo, merge in
#################

# must be split off and have 1 row per subject, so distributions are accurate when calculating Z-scores
age_zscore_maker <- Gonadarche_Exclude %>%
  select(src_subject_id, gonadarche_age) %>%
  unique() %>%
  filter(!is.na(gonadarche_age))
tempo_zscore_maker <- Gonadarche_Exclude %>%
  select(src_subject_id, gonadarche_tempo) %>%
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
  left_join(age_zscore_maker, by = 'src_subject_id') %>%
  left_join(tempo_zscore_maker, by = 'src_subject_id')

# Reorganize for export
Gonadarche_Exclude <- Gonadarche_Exclude %>%
  select(src_subject_id, eventname, sex, interview_age, gonadarche_age, gonadarche_age_zscore, gonadarche_tempo, gonadarche_tempo_zscore, everything())

#################
# IN THE BELOW CODE BLOCK
# Export to CSV
#################

write_csv(Gonadarche_Exclude, paste0(derivative_data, "Gonadarche_5.0_Age_Tempo_2_27_26.csv"))
