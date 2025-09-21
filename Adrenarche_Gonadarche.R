################
# Adrenarche_Gonadarche.R
# Robert Toms, robert.toms@utdallas.edu
# 9/10/2025
################

# The purpose of this code is to calculate age at Adrenarche, Gonadarche, and general pubertal onset in females.

library(tidyverse)
library(dplyr)
library(tidyverse)
library(dplyr)

datapath <- "/path/to/ABCD_6.0/Data"

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
         #ph_y_pds__m_001,  # (Male) Deepening Voice? : voice has not yet started changing = 1; voice has barely started changing = 2; voice changes are definitely underway (has definitely started) = 3; voice changes seem complete = 4; I don't know = 999; Decline to answer = 777
         #ph_y_pds__m_002,  # (Male) Facial Hair Growth? : Facial hair not yet started growing = 1; Facial hair has barely started growing = 2; Facial hair has definitely started = 3; Facial hair growth seems complete = 4; I don't know = 999; Decline to answer = 777
         ph_y_pds__f_mean) #, # (Female) PDS value : mean of ph_y_pds 001, 002, 003 and f_001 & f_002. f_002 Yes = 4, No = 1
         #ph_y_pds__m_mean) # (Male) PDS Value

# make human readable
pds <- pds %>%
  rename(interview_datetime = ph_y_pds_dtt,
         interview_age_yrs = ph_y_pds_age,
         growth_spurt = ph_y_pds_001,
         body_hair = ph_y_pds_002,
         skin_pimples = ph_y_pds_003,
         thelarche = ph_y_pds__f_001,
         menarche = ph_y_pds__f_002,
         #voice_deep = ph_y_pds__m_001,
         #facial_hair = ph_y_pds__m_002,
         ABCD_PDS = ph_y_pds__f_mean)
         #male_PDS = ph_y_pds__m_mean)

# merge in sex data
pds_sex <- pds %>%
  left_join(sex, by = "participant_id")

# Set 999s and 777s to NA, per Herting et al., 2021 best practices
pds_sex <- pds_sex %>%
  mutate(across(c(growth_spurt, 
                  body_hair, 
                  skin_pimples, 
                  thelarche, 
                  menarche#, 
                  #voice_deep, 
                  #facial_hair
                  ), ~case_when(
    . == 999 ~ NA,
    . == 777 ~ NA,
    TRUE ~ .)))

# Make female- specific dataframe
female_pds <- pds_sex %>%
  filter(sex == "F") #%>%
  #select(-voice_deep, -facial_hair, -male_PDS)

#################
# IN THE BELOW CODE BLOCK
# Manually Calculate PDS Values
#################

# Set premenarche to 1, postmenarche to 4 for PDS calculation
female_pds <- female_pds %>%
  mutate(menarche = case_when(
    menarche == 1 ~ 4,
    menarche == 0 ~ 1)) %>%
  mutate(thelarche = if_else(thelarche == "n/a", NA, as.numeric(thelarche))) %>%
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

#################
# IN THE BELOW CODE BLOCK
# Calculate Slopes of each pubertal category
#################

# According to the best practices of (Larson, Chaku, & Moussa-Tooks, 2025), 
# only adolescents with at least 2 increasing assessments of pubertal development are to be included in analysis.

# define function to get slopes
find_slope <- function(data, y) {                   # takes in dataframe and specific y column
  model_data <- data %>%                                                         
    select(interview_age_yrs, all_of(y)) %>%        # keep x and y
    drop_na()                                       # drop NAs
  if (nrow(model_data) < 2) return(NA_real_)        # return NA in the column if insufficent data to calculate slopes
  model <- lm(reformulate("interview_age_yrs", response = y), data = model_data)   # calculate linear model
  coef(model)[["interview_age_yrs"]]                # pull slope value from linear model
}

# calculate individual item slopes
female_pds_slopes <- female_pds %>%
  group_by(participant_id) %>%
  group_modify(~ {
    tibble(
      growth_spurt_slope = find_slope(.x, "growth_spurt"),
      body_hair_slope = find_slope(.x, "body_hair"),
      skin_pimples_slope = find_slope(.x, "skin_pimples"),
      thelarche_slope = find_slope(.x, "thelarche"),
      menarche_slope = find_slope(.x, "menarche")
    )
  })

# count number of increasing items
female_pds_slopes <- female_pds_slopes %>%
  # all categories
  mutate(num_general_increasing = sum(growth_spurt_slope > 0,
                              body_hair_slope > 0,
                              skin_pimples_slope > 0,
                              thelarche_slope > 0,
                              menarche_slope > 0, na.rm = TRUE)) %>%
  # adrenarche
  mutate(num_adrenarche_increasing = sum(body_hair_slope > 0,
                                         skin_pimples_slope > 0, na.rm = TRUE)) %>%
  # gonadarche
  mutate(num_gonadarche_increasing = sum(thelarche_slope > 0,
                                         menarche_slope > 0, na.rm = TRUE))

# Count number of participants with less than 2 valid pds datapoints
table(female_pds_slopes$num_general_increasing)
#   0    1    2    3    4    5 
#  187  59   148  382 1106 3796 
# Total: 5678
# 187 + 59 = 246
# 246/5678 = 4.3%

table(female_pds_slopes$num_adrenarche_increasing)
#   0    1    2 
#  288  618 4772 
# Total: 5678
# 288 + 618 = 906
# 906/5678 = 16%

table(female_pds_slopes$num_gonadarche_increasing)
#   0    1    2 
#  280  690 4708 
# Total: 5678
# 280 + 690 = 970
# 970/5678 = 17.1%

# Exclude those with less than 2 pubertal development metrics in either category
#female_pds_slopes <- female_pds_slopes %>%
  # 5678 total
  #filter(num_adrenarche_increasing == 2) %>%
  # 4772 remaining  -- 906 excluded
  #filter(num_gonadarche_increasing == 2)
  # 4271 remaining  -- 501 excluded

num_increasing <- female_pds_slopes %>%
  select(participant_id, num_general_increasing, num_adrenarche_increasing, num_gonadarche_increasing)

female_pds <- female_pds %>%
  left_join(num_increasing, by = "participant_id")

#################
# IN THE BELOW CODE BLOCK
# Calculate Age at Adrenarche, Gonadarche, General Pubertal onset
#################

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
arche_slopes <- female_pds %>%
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
arche_slopes <- arche_slopes %>%
  mutate(ABCD_pubertal_age = (2.5 - ABCD_intercept) / ABCD_slope) %>%
  mutate(manual_pubertal_age = (2.5 - manual_intercept) / manual_slope) %>%
  mutate(adrenarche_age = (2.5 - adrenarche_intercept) / adrenarche_slope) %>%
  mutate(gonadarche_age = (2.5 - gonadarche_intercept) / gonadarche_slope)

#################
# IN THE BELOW CODE BLOCK
# Clean and organize for export
#################

# isolate age at pubertal milestones
pubertal_ages <- arche_slopes %>%
  select(participant_id, 
        adrenarche_age, adrenarche_slope,
        gonadarche_age, gonadarche_slope,
        manual_pubertal_age, manual_slope,
        ABCD_pubertal_age, ABCD_slope) %>%
  rename(adrenarche_tempo = adrenarche_slope,
         gonadarche_tempo = gonadarche_slope,
         manual_pubertal_tempo = manual_slope,
         ABCD_pubertal_tempo = ABCD_slope)

# isolate pds items and counts
pubertal_items <- female_pds %>%
  select(participant_id, session_id, sex, interview_age_yrs,
         growth_spurt, body_hair, skin_pimples, thelarche, menarche,
         adrenarche_pds, gonadarche_pds, manual_pds, ABCD_PDS, num_items, num_adrenarche_increasing, num_gonadarche_increasing, num_general_increasing)

# merge items and ages
pubertal_timing <- pubertal_items %>%
  left_join(pubertal_ages, by = "participant_id")

# reorder for clarity and readability
pubertal_timing <- pubertal_timing %>%
  select(participant_id, session_id, sex, interview_age_yrs,
         adrenarche_age, adrenarche_tempo,
         gonadarche_age, gonadarche_tempo,
         manual_pubertal_age, manual_pubertal_tempo,
         ABCD_pubertal_age, ABCD_pubertal_tempo,
         everything())

# Flag exclusions
pubertal_timing <- pubertal_timing %>%
  mutate(ABCD_Manual_Flag_Y1N0 = if_else(!is.na(manual_pds) & is.na(ABCD_PDS), 1, 0)) %>%
  mutate(Exclude_Adren_Y1N0 = if_else(num_adrenarche_increasing < 2, 1, 0)) %>%
  mutate(Exclude_Gonad_Y1N0 = if_else(num_gonadarche_increasing < 2, 1, 0)) %>%
  mutate(Exclude_General_Y1N0 = if_else(num_general_increasing < 2, 1, 0))

#################
# IN THE BELOW CODE BLOCK
# Export to CSV
#################

csvpath <- "/path/to/derivative_data/"

write_csv(pubertal_timing, paste0(csvpath, "Pubertal_Onsets_9_10_25.csv"))


#################
# IN THE BELOW CODE BLOCK
# Extra stats
#################

# implement initial outlier exclusions, then you can use these:

mean(is.finite(pubertal_ages$adrenarche_age))
mean(is.finite(pubertal_ages$gonadarche_age))
mean(is.finite(pubertal_ages$manual_pubertal_age))
mean(is.finite(pubertal_ages$ABCD_pubertal_age))

sd(is.finite(pubertal_ages$adrenarche_age))
sd(is.finite(pubertal_ages$gonadarche_age))
sd(is.finite(pubertal_ages$manual_pubertal_age))
sd(is.finite(pubertal_ages$ABCD_pubertal_age))
