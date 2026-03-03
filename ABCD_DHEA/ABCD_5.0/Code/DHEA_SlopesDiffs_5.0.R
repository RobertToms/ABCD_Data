# ---
  #title: "DHEA_SlopesDiffs_5.0.R"
  #author: "Aaron Clouse & Robert Toms"
  #date: "1/30/2026"
#  ---
  ##Load Packages
  
library(tidyverse)

##Read in the data

#______________________________________________________________________________

# REQUIRED: paths to your raw and derivative data

sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"

#______________________________________________________________________________

#REQUIRED: this should be a csv of interview ages provided by ABCD.
#note that this script makes the assumption that ages are in MONTHS. Three columns are required:

# 1. interview_age, which is the age of the participant at that eventname in MONTHS 
# 2. src_subject_id
# 3. eventname

interview_age <- read_csv(paste0(sourcepath, "abcd-general/abcd_y_lt.csv"))

#______________________________________________________________________________

#REQUIRED: Where do you want this file written to and how would you like it named?
# write_file: the general slope/difference data for both males and females
# parent_file: premenarche slope/difference data for parent reports
# youth_file: premenarche slope/differerence data for youth reports

write_file <- paste0(derivative_data, "abcd_5.0_DHEA_slopes_1_30_26.csv")
parent_file <- paste0(derivative_data, "parent_5.0_premenarche_DHEA_slopes_1_30_26.csv")
youth_file <- paste0(derivative_data, "youth_5.0_premenarche_DHEA_slopes_1_30_26.csv")
#______________________________________________________________________________

#OPTIONAL: this should be a demographics csv provided by ABCD. 
# this is only used to include a sex variable on the output csv. it needs three columns: 

# 1. demo_sex_v2 
# 2. src_subject_id
# 3. eventname

#NOTE: this is an optional input. FEEL FREE TO SKIP OVER THE "DEMOGRAPHICS" section below, 
#it has no effect on the calculation of slopes or differences.

demo <- read_csv(paste0(sourcepath, "abcd-general/abcd_p_demo.csv"))

#______________________________________________________________________________

#OPTIONAL: These are csv files (5.0_Youth_PrePost_Menarche_12_2_25.csv, 5.0_Parent_PrePost_Menarche_12_2_25.csv) that identifies when participants report menarche.
# It was created using the PrePost_Menarche_5.0.R file. 

# they have four required columns: 
# 1. first_post_event: identifies the visit at which individuals are postmenarche (Ex. postmenarche year 2, postmenarche year 3, etc.)
# 2. src_subject_id
# 3. PostMenarche_at_Baseline_Y1N0: Yes = 1, No = 0 flag, indicating if the subject was postmenarche at first report. 
# 4. Inconsistent_Reporting_Y1N0: Yes = 1, No = 0 flag, indicating if subject reported being premenarche after reporting being postmenarche

#with some tweaking to the column names and values in the "POSTMENARCHE DATA" section of this script,
#this could be a way to filter out unwanted timepoints depending on the nature of your variable of interest. 
#NOTE: this is an optional input. FEEL FREE TO SKIP OVER THE "POSTMENARCHE DATA" section below.
youth <- read_csv(paste0(derivative_data, "5.0_Youth_PrePost_Menarche_12_2_25.csv"))
parent <- read_csv(paste0(derivative_data, "5.0_Parent_PrePost_Menarche_12_2_25.csv"))
#______________________________________________________________________________

# REQUIRED: This is a csv file containing validated saliva samples and exclusions based on the quality of the saliva sample. Four columns are required: 

# 1. Excluded? which contains a "0" for included and a "1" for excluded
# 2. SubjectID: which contains the same information as src_subject_id 
# 3. EventName: which contains the same information as eventname
# 4. DHEAmean: Validated Salivary DHEA measurement (pg/mL)

Exclusions <- read_csv(paste0(derivative_data, "DHEA_5.0_Exclusions_10_7_25.csv"))
#______________________________________________________________________________


## EXCLUSIONS

# Exclude DHEA samples with a 1 in the Exclusion column, remove subjects with no valid DHEA samples
Exclusions <- Exclusions %>%
  mutate(DHEAmean = ifelse(`Excluded?`== 1, NA, DHEAmean)) %>%
  filter(!is.na(DHEAmean))

# Format MasterSex to match legacy code
Exclusions <- Exclusions %>%
  mutate(sex = case_when(
    MasterSex == 1 ~ 'F',  # Typically in ABCD data M = 1, F = 2, but in DHEA_Exclusions_5.0.R this was reversed to M=2,F=1, to match with Salivary Sex. 
    MasterSex == 2 ~ 'M'))

dhea_subs <- Exclusions %>% select(src_subject_id, sex) %>% unique()
table(dhea_subs$sex) # 11627 subjects with valid DHEA data; 5586 F / 6041 M

# Only select relevant columns
Exclusions <- Exclusions %>%
  select(src_subject_id , eventname, sex, DHEAmean)

dhea_f <- Exclusions %>% filter(sex == 'F') %>% select(-sex)
Exclusions <- Exclusions %>% select(-sex)

# sanity check that there are 5618 females
f_dhea_subs <- dhea_f %>% distinct(src_subject_id) %>% pull(src_subject_id) # 5586

## PREP POSTMENARCHE DATA

# keep useful columns
youth_menarche <- youth %>%
  select(src_subject_id, last_pre_event, first_post_event, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)
parent_menarche <- parent %>%
  select(src_subject_id, last_pre_event, first_post_event, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)

# remove subjects with no validated pre/post menarche transition
youth_menarche <- youth_menarche %>% filter(!is.na(first_post_event) & !is.na(last_pre_event))
parent_menarche <- parent_menarche %>% filter(!is.na(first_post_event) & !is.na(last_pre_event))

# Count subjects in each and overlaps
subs_y_menarche <- youth_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 3833
subs_p_menarche <- parent_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 4011
subs_dhea <- dhea_f %>% distinct(src_subject_id) %>% pull(src_subject_id) # 5586

y_has_dhea_and_menarche <- intersect(subs_y_menarche, subs_dhea) # 3806
y_has_dhea_no_menarche <- setdiff(subs_dhea, subs_y_menarche) # 1780
y_has_menarche_no_dhea <- setdiff(subs_y_menarche, subs_dhea) # 27

p_has_dhea_and_menarche <- intersect(subs_p_menarche, subs_dhea) # 3979
p_has_dhea_no_menarche <- setdiff(subs_dhea, subs_p_menarche) # 1607
p_has_menarche_no_dhea <- setdiff(subs_p_menarche, subs_dhea) # 32

# merge into DHEA data
youth_DHEA_menarche <- youth_menarche %>%
  left_join(Exclusions, by = "src_subject_id")
parent_DHEA_menarche <- parent_menarche %>%
  left_join(Exclusions, by = "src_subject_id")

# Count subjects
dhea_y_subjects <- youth_DHEA_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 3833
dhea_p_subjects <- parent_DHEA_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 4011

# remove timepoints without valid dhea data
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  filter(!is.na(DHEAmean))
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  filter(!is.na(DHEAmean))

# Count subjects
dhea_y_subjects <- youth_DHEA_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 3806
dhea_p_subjects <- parent_DHEA_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 3979

# make numeric versions of eventname, to compare against first postmenarche, so we can keep only premenarche
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  mutate(eventname_num = case_when(
    eventname == "baseline_year_1_arm_1" ~ 0,
    eventname == "1_year_follow_up_y_arm_1" ~ 1,
    eventname == "2_year_follow_up_y_arm_1" ~ 2,
    eventname == "3_year_follow_up_y_arm_1" ~ 3,
    eventname == "4_year_follow_up_y_arm_1" ~ 4))
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  mutate(eventname_num = case_when(
    eventname == "baseline_year_1_arm_1" ~ 0,
    eventname == "1_year_follow_up_y_arm_1" ~ 1,
    eventname == "2_year_follow_up_y_arm_1" ~ 2,
    eventname == "3_year_follow_up_y_arm_1" ~ 3,
    eventname == "4_year_follow_up_y_arm_1" ~ 4))


#this code ensures that only premenarche data points are included in the calculation of slopes and raw differences. 
# remove any data point at or after menarche
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  filter(eventname_num < first_post_event)
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  filter(eventname_num < first_post_event)

# Count subjects
dhea_y_subjects <- youth_DHEA_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 3806 -> 3759; -47
dhea_p_subjects <- parent_DHEA_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 3979 -> 3920; -59


##MAKE DATA WIDE AND AVAILABILITY COUNTS

#Format wide ABCD data according to subjectID and event
data <- Exclusions %>%
  pivot_wider(names_from = eventname, values_from = c(DHEAmean))

#sum the number of available timepoints for each participant 
data$available_timepoints <- rowSums(!is.na(data[, c("baseline_year_1_arm_1",
                                                     "1_year_follow_up_y_arm_1",
                                                     "2_year_follow_up_y_arm_1",
                                                     "3_year_follow_up_y_arm_1",
                                                     "4_year_follow_up_y_arm_1")]))

##DEMOGRAPHICS

#add sex to the data frame by matching subject IDs 
data <- data %>%
  left_join(demo %>% select(src_subject_id, demo_sex_v2), by = "src_subject_id") %>%
  filter(!is.na(demo_sex_v2))

#display timepoint totals
table(data$available_timepoints[data$demo_sex_v2 == 1])
# MALES:
#  1    2    3    4    5   
#  683 1982 2042 1177  157  -- 683 + 1982 + 3376 = 6041 M

table(data$available_timepoints[data$demo_sex_v2 == 2])
# FEMALES:
#  1    2    3    4    5    
# 583 1786 1975 1103  139 -- 583 + 1786 + 3217 = 5586 F

# 683+583=1266 with insufficient data to calculate change 
# 11627-1266=10361 with sufficient data
# 1982+1786=3768 differences calculated
# 3376+3217=6593 slopes calculated

######### MALES AND FEMALES ##########
##SIMPLE RAW DIFFERENCE SCORE

#if values are present for baseline and year 1, but no other timepoints, calculate the raw difference score. 
data <- data %>%
  mutate(
    raw_difference_score = ifelse(!is.na(baseline_year_1_arm_1) &
                                    !is.na(`1_year_follow_up_y_arm_1`) &
                                    is.na(`2_year_follow_up_y_arm_1`) &
                                    is.na(`3_year_follow_up_y_arm_1`) &
                                    is.na(`4_year_follow_up_y_arm_1`),
                                  `1_year_follow_up_y_arm_1` - `baseline_year_1_arm_1`, 
                                  NA))




##PREP FOR SLOPE CALCULATION
#select only interview age, subject ID and event name 
interview_age <- interview_age %>%
  select(src_subject_id,
         eventname,
         interview_age)

#join interview age to the long data in a new table
data_lm <- Exclusions %>%
  left_join(interview_age, by = c("src_subject_id", "eventname"))

#remove subs that have a raw score (2 timepoints, one of them at baseline)
already_calculated_raw <- data %>% # 2822 subjects with already calculated raw scores
  filter(!is.na(raw_difference_score) & raw_difference_score != "") %>%
  distinct(src_subject_id) %>%
  pull(src_subject_id)

data_lm <- data_lm %>% # 31453 -> 25809, 5644 timepoints removed
  filter(!src_subject_id %in% already_calculated_raw)

#remove other datapoints with less than 3 timepoints- if they have 2 timepoints, a raw difference score is calculated. 
less_than_three <- data_lm %>% # 2212 subjects with less than 3 timepoints
  group_by(src_subject_id) %>%
  filter(n() < 3) %>%
  distinct(src_subject_id) %>%
  pull(src_subject_id)

data_lm <- data_lm %>% # 25809 -> 22651, 3158 timepoints removed
  group_by(src_subject_id) %>%
  filter(n() >= 3) %>%
  ungroup()


##CALCULATE SLOPE

# one subject is missing interivew ages -- substitute in appropriate interview ages (in months) for slope calculation
data_lm <- data_lm %>%
  mutate(interview_age = if_else(is.na(interview_age),
                                 case_when(
                                   eventname == "baseline_year_1_arm_1" ~ 0,
                                   eventname == "1_year_follow_up_y_arm_1" ~ 12,
                                   eventname == "2_year_follow_up_y_arm_1" ~ 24,
                                   eventname == "3_year_follow_up_y_arm_1" ~ 36,
                                   eventname == "4_year_follow_up_y_arm_1" ~ 48), interview_age))

#extract the slope of the linear model for each subject 
data_slopes <- data_lm %>%
  group_by(src_subject_id) %>%
  summarise(slope_DHEAmean = lm(DHEAmean ~ interview_age)$coef[2])

#attach the slopes back to the original data frame by participant
data <- data %>%
  left_join(data_slopes %>% select(src_subject_id, slope_DHEAmean), by = "src_subject_id")

#scale slopes
data$slope_DHEAmean_scale <- as.numeric(scale(data$slope_DHEAmean))


##COMPLEX RAW DIFFERENCE SCORE

#Complex raw difference score and scale (follow up greater than 1yr apart)
#creates a new data frame for participants that do not have a calculated slope or raw difference score 
complex_raw <- data %>%
  filter(is.na(raw_difference_score) & is.na(slope_DHEAmean))

#keep only participants that have 2 timepoints of data 
complex_raw <- complex_raw %>%
  filter(available_timepoints == 2)

#identify columns that contain dhea data
timepoint_cols <- c("baseline_year_1_arm_1",
                    "1_year_follow_up_y_arm_1",
                    "2_year_follow_up_y_arm_1",
                    "3_year_follow_up_y_arm_1",
                    "4_year_follow_up_y_arm_1")

#function determines the position of dhea columns that contain values, and returns the difference (the amount of time in years) between those two timepoints 
time_between_visits <- function(visit, timepoints) {
  has_value <- which(!is.na(visit[timepoints]))
  if (length(has_value) == 2) {
    return(diff(has_value))
  } else {
    return(NA)
  }
}

#apply the time between visits function to each row of the data frame 
complex_raw$yrs_between_visits <- apply(complex_raw, 1, 
                                        time_between_visits, 
                                        timepoints = timepoint_cols)

#function isolates the two values present for each participant, finds the difference between the two 
calculate_raw_diff <- function(visit, timepoints) {
  values <- visit[timepoints]
  values <- as.numeric(values[!is.na(values)])
  if (length(values) == 2) {
    return(values[2] - values[1])
  } else {
    return(NA)
  }
}

#apply the calculate raw diff function rowwise to determine raw difference score in participants with timepoints greater than yr apart 
complex_raw$raw_difference_score <- apply(complex_raw, 1, 
                                          calculate_raw_diff, 
                                          timepoints = timepoint_cols)

#note that the participants raw difference score was greater that 1yr apart or not at baseline
complex_raw <- complex_raw %>%
  mutate(
    notes = ifelse(yrs_between_visits == 1,
                   paste("two timepoints",
                         complex_raw$yrs_between_visits,
                         "years apart, not at baseline"),
                   paste("two timepoints",
                         complex_raw$yrs_between_visits,
                         "years apart")))

#isolate raw scores and notes to be included in the greater data table 
complex_raw <- complex_raw %>%
  select(src_subject_id, raw_difference_score, notes)

#add the complex raw scores back into the table by subject ID 
data <- data %>% 
  left_join(complex_raw %>% 
              select(src_subject_id, 
                     raw_difference_score), 
            by = "src_subject_id") %>%
  mutate(raw_difference_score = coalesce(raw_difference_score.y, raw_difference_score.x)) %>%
  select(-raw_difference_score.y, -raw_difference_score.x)

#add the notes column to the data frame 
data <- data %>%
  left_join(complex_raw %>% 
              select(src_subject_id, 
                     notes), 
            by = "src_subject_id")

##SCALE RAW DIFFERENCE SCORE
#scale raw difference scores 
data$raw_difference_scale <- as.numeric(scale(data$raw_difference_score))


############ PREMENARCHE FEMALES ###############
##PREP FOR PREMENARCHE SLOPE CALCULATION
# merge in ages
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  left_join(interview_age, by = c("src_subject_id", "eventname"))
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  left_join(interview_age, by = c("src_subject_id", "eventname"))

# no missing ages

# count number of valid datapoints per participant
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  group_by(src_subject_id) %>%
  mutate(premenarche_timepoints = n())
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  group_by(src_subject_id) %>%
  mutate(premenarche_timepoints = n())

y_tps <- youth_DHEA_menarche %>% select(src_subject_id, premenarche_timepoints) %>% unique()
table(y_tps$premenarche_timepoints) # 820 with only 1, 1705 with 2, 1234 with 3+ (3759 total)
p_tps <- parent_DHEA_menarche %>% select(src_subject_id, premenarche_timepoints) %>% unique()
table(p_tps$premenarche_timepoints) # 903 with only 1, 1814 with 2, 1203 with 3+ (3920 total)

# remove those with only 1 timepoint
youth_DHEA_menarche <- youth_DHEA_menarche %>% # 8040 -> 7220, 820 subjects removed
  filter(premenarche_timepoints > 1)
parent_DHEA_menarche <- parent_DHEA_menarche %>% # 8243 -> 7340, 903 subjects removed
  filter(premenarche_timepoints > 1)

# Count subjects
dhea_y_subjects <- youth_DHEA_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 3759 -> 2939; 820 removed
dhea_p_subjects <- parent_DHEA_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 3920 -> 3017; 903 removed


##CALCULATE PREMENARCHE DIFFERENCES AND SLOPES
# calculate raw differences
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  group_by(src_subject_id) %>%
  mutate(scores = list(na.omit(DHEAmean))) %>%
  rowwise() %>%
  mutate(raw_difference_premenarche = if_else(premenarche_timepoints == 2, 
                                              last(scores) - scores[1], NA)) %>%
  select(-scores)
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  group_by(src_subject_id) %>%
  mutate(scores = list(na.omit(DHEAmean))) %>%
  rowwise() %>%
  mutate(raw_difference_premenarche = if_else(premenarche_timepoints == 2, 
                                              last(scores) - scores[1], NA)) %>%
  select(-scores)

#scale differences
youth_DHEA_menarche$raw_difference_premenarche_scale <- as.numeric(scale(youth_DHEA_menarche$raw_difference_premenarche))
parent_DHEA_menarche$raw_difference_premenarche_scale <- as.numeric(scale(parent_DHEA_menarche$raw_difference_premenarche))


# add in interview age for the one person without them
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  mutate(interview_age = if_else(is.na(interview_age),
                                 case_when(
                                   eventname == "baseline_year_1_arm_1" ~ 0,
                                   eventname == "1_year_follow_up_y_arm_1" ~ 12,
                                   eventname == "2_year_follow_up_y_arm_1" ~ 24,
                                   eventname == "3_year_follow_up_y_arm_1" ~ 36,
                                   eventname == "4_year_follow_up_y_arm_1" ~ 48), interview_age))
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  mutate(interview_age = if_else(is.na(interview_age),
                                 case_when(
                                   eventname == "baseline_year_1_arm_1" ~ 0,
                                   eventname == "1_year_follow_up_y_arm_1" ~ 12,
                                   eventname == "2_year_follow_up_y_arm_1" ~ 24,
                                   eventname == "3_year_follow_up_y_arm_1" ~ 36,
                                   eventname == "4_year_follow_up_y_arm_1" ~ 48), interview_age))


# calculate slopes
youth_slopes <- youth_DHEA_menarche %>%
  group_by(src_subject_id) %>%
  summarise(slope_DHEAmean_premenarche = lm(DHEAmean ~ interview_age)$coef[2])
parent_slopes <- parent_DHEA_menarche %>%
  group_by(src_subject_id) %>%
  summarise(slope_DHEAmean_premenarche = lm(DHEAmean ~ interview_age)$coef[2])

# merge slopes with menarche data
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  left_join(youth_slopes %>% select(src_subject_id, slope_DHEAmean_premenarche), by = "src_subject_id")
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  left_join(parent_slopes %>% select(src_subject_id, slope_DHEAmean_premenarche), by = "src_subject_id")

# reset 2 timepoint slopes to NA
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  mutate(slope_DHEAmean_premenarche = if_else(premenarche_timepoints == 2, NA, slope_DHEAmean_premenarche))

#scale slopes
youth_DHEA_menarche$slope_DHEAmean_premenarche_scale <- as.numeric(scale(youth_DHEA_menarche$slope_DHEAmean_premenarche))
parent_DHEA_menarche$slope_DHEAmean_premenarche_scale <- as.numeric(scale(parent_DHEA_menarche$slope_DHEAmean_premenarche))

##ADD NOTES TO RAW DIFFERENCES

# find difference of visits for those with 2 timepoints
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  group_by(src_subject_id) %>%
  mutate(visits = list(na.omit(eventname_num))) %>%
  rowwise() %>%
  mutate(yrs_between_visits = if_else(premenarche_timepoints == 2, 
                                      last(visits) - visits[1], NA)) %>%
  select(-visits)
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  group_by(src_subject_id) %>%
  mutate(visits = list(na.omit(eventname_num))) %>%
  rowwise() %>%
  mutate(yrs_between_visits = if_else(premenarche_timepoints == 2, 
                                      last(visits) - visits[1], NA)) %>%
  select(-visits)

#note if the participants raw difference score was greater that 1yr apart
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  rowwise() %>%
  mutate(premenarche_notes = ifelse(yrs_between_visits == 1,
                                    NA,
                                    paste("two timepoints", yrs_between_visits, "years apart"))) %>%
  select(-yrs_between_visits)
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  rowwise() %>%
  mutate(premenarche_notes = ifelse(yrs_between_visits == 1,
                                    NA,
                                    paste("two timepoints", yrs_between_visits, "years apart"))) %>%
  select(-yrs_between_visits)

##FORMAT DATAFRAMES FOR EXPORT

# premenarche
premenarche_youth <- data %>%
  left_join(youth_DHEA_menarche %>% select(src_subject_id, premenarche_timepoints, raw_difference_premenarche, raw_difference_premenarche_scale, slope_DHEAmean_premenarche, slope_DHEAmean_premenarche_scale, premenarche_notes), by = "src_subject_id") %>%
  left_join(youth %>% select(src_subject_id, last_pre_event, first_post_event, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0), by = 'src_subject_id') %>%
  unique()

premenarche_parent <- data %>%
  left_join(parent_DHEA_menarche %>% select(src_subject_id, premenarche_timepoints, raw_difference_premenarche, raw_difference_premenarche_scale, slope_DHEAmean_premenarche, slope_DHEAmean_premenarche_scale, premenarche_notes), by = "src_subject_id") %>%
  left_join(parent %>% select(src_subject_id, last_pre_event, first_post_event, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0), by = 'src_subject_id') %>%
  unique()


# reorganize
premenarche_youth <- premenarche_youth %>%
  select(src_subject_id, demo_sex_v2, last_pre_event, first_post_event, premenarche_timepoints,
         slope_DHEAmean_premenarche, slope_DHEAmean_premenarche_scale, raw_difference_premenarche, raw_difference_premenarche_scale,
         premenarche_notes, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0,
         baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`) %>%
  filter(demo_sex_v2 == 2)

premenarche_parent <- premenarche_parent %>%
  select(src_subject_id, demo_sex_v2, last_pre_event, first_post_event, premenarche_timepoints,
         slope_DHEAmean_premenarche, slope_DHEAmean_premenarche_scale, raw_difference_premenarche, raw_difference_premenarche_scale, 
         premenarche_notes, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0,
         baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`) %>%
  filter(demo_sex_v2 == 2)

data <- data %>%
  select(src_subject_id, demo_sex_v2, available_timepoints, slope_DHEAmean, slope_DHEAmean_scale, raw_difference_score, raw_difference_scale,
         notes, baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)

##WRITE CSV

#write the completed data frame to a csv 
write_csv(data, write_file)
write_csv(premenarche_youth, youth_file)
write_csv(premenarche_parent, parent_file)

#############
# Sanity Check Counts

premen_y <- premenarche_youth %>% mutate(has_change = (!is.na(slope_DHEAmean_premenarche) | !is.na(raw_difference_premenarche)))
table(premen_y$has_change) # 2939
premen_p <- premenarche_parent %>% mutate(has_change = (!is.na(slope_DHEAmean_premenarche) | !is.na(raw_difference_premenarche)))
table(premen_p$has_change) # 3017
dhea_all <- data %>% mutate(has_change = (!is.na(slope_DHEAmean) | !is.na(raw_difference_score)))
table(dhea_all$has_change) # 10361
table(dhea_all$has_change, dhea_all$demo_sex_v2) # 5003 F / 5358 M

