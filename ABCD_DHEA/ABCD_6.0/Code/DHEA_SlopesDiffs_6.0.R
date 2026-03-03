---
#title: "DHEA_SlopesDiffs_6.0.R"
#author: "Aaron Clouse & Robert Toms"
#date: "12/3/2025"
---
##Load Packages

library(tidyverse)

##Read in the data

#______________________________________________________________________________

# REQUIRED: paths to your raw and derivative data
sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/6.0/"

#______________________________________________________________________________

#REQUIRED: this should be a csv of interview ages provided by ABCD.
# Three columns are required:

# 1. ab_g_dyn__visit_age, which is the age of the participant at that session_id 
# 2. participant_id
# 3. session_id

interview_age <- read_tsv(paste0(sourcepath, "abcd_general/ab_g_dyn.tsv"))
#______________________________________________________________________________

#REQUIRED: Where do you want this file written to and how would you like it named?
# write_file: the general slope/difference data for both males and females
# parent_file: premenarche slope/difference data for parent reports
# youth_file: premenarche slope/differerence data for youth reports

write_file <- paste0(derivative_data, "abcd_6.0_DHEA_slopes_12_3_25.csv")
parent_file <- paste0(derivative_data, "parent_6.0_premenarche_DHEA_slopes_12_3_25.csv")
youth_file <- paste0(derivative_data, "youth_6.0_premenarche_DHEA_slopes_12_3_25.csv")
#______________________________________________________________________________

#OPTIONAL: this should be a demographics csv provided by ABCD. 
# this is only used to include a sex variable on the output csv. it needs three columns: 

# 1. ab_g_stc__cohort_sex 
# 2. participant_id
# 3. session_id

#NOTE: this is an optional input. FEEL FREE TO SKIP OVER THE "DEMOGRAPHICS" section below, 
#it has no effect on the calculation of slopes or differences.

demo <- read_tsv(paste0(sourcepath, "abcd_general/ab_g_stc.tsv"))
#______________________________________________________________________________

#REQUIRED: These are csv files (Youth_PrePost_Menarche_11_21_25.csv, Parent_PrePost_Menarche_11_21_25.csv) that identifies when participants report menarche.
# They were created using the PrePost_Menarche_6.0.R file. 

# they have six key columns: 
# 1. first_post_session: identifies the first visit at which individuals are postmenarche (Ex. postmenarche year 2, postmenarche year 3, etc.)
# 2. last_pre_session: identifies the last visit at which individuals are premenarche
# 3. participant_id
# 4. PostMenarche_at_Baseline_Y1N0: Yes = 1, No = 0 flag, indicating if the subject was postmenarche at baseline. 
# 5. Inconsistent_Reporting_Y1N0: Yes = 1, No = 0 flag, indicating if subject reported being premenarche after reporting being postmenarche
# 6. NoBaselineReport_Y1N0: Yes = 1, No = 0 flag, indicating if the subject had no usable menarche report data at baseline.

youth <- read_csv(paste0(derivative_data, "Youth_PrePost_Menarche_11_21_25.csv"))
parent <- read_csv(paste0(derivative_data, "Parent_PrePost_Menarche_11_21_25.csv"))
#______________________________________________________________________________

# REQUIRED: This is a csv file containing validated saliva samples and exclusions based on the quality of the saliva sample. Four columns are required: 

# 1. Excluded? which contains a "0" for included and a "1" for excluded
# 2. SubjectID: which contains the same information as participant_id 
# 3. session_id: which contains the same information as session_id
# 4. DHEAmean: Validated Salivary DHEA measurement (pg/mL)

Exclusions <- read_csv(paste0(derivative_data, "DHEA_Exclusions_11_21_25.csv"))
#______________________________________________________________________________


## EXCLUSIONS

# Exclude DHEA samples with a 1 in the Exclusion column, remove subjects with no valid DHEA samples
Exclusions <- Exclusions %>%
  mutate(DHEAmean = ifelse(`Excluded?`== 1, NA, DHEAmean)) %>%
  filter(!is.na(DHEAmean))

dhea_subs <- Exclusions %>% select(participant_id, sex) %>% unique()
table(dhea_subs$sex) # 11718 subjects with valid DHEA data; 5618 F / 6100 M

# Only select relevant columns
Exclusions <- Exclusions %>%
  select(participant_id , session_id, sex, DHEAmean)

dhea_f <- Exclusions %>% filter(sex == 2) %>% select(-sex)
Exclusions <- Exclusions %>% select(-sex)

# sanity check that there are 5618 females
f_dhea_subs <- dhea_f %>% distinct(participant_id) %>% pull(participant_id) # 5618

## PREP POSTMENARCHE DATA

# keep useful columns
youth_menarche <- youth %>%
  select(participant_id, last_pre_session, first_post_session, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)
parent_menarche <- parent %>%
  select(participant_id, last_pre_session, first_post_session, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)

# remove subjects with no validated pre/post menarche transition
youth_menarche <- youth_menarche %>% filter(!is.na(first_post_session) & !is.na(last_pre_session))
parent_menarche <- parent_menarche %>% filter(!is.na(first_post_session) & !is.na(last_pre_session))

# Count subjects in each and overlaps
subs_y_menarche <- youth_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4822
subs_p_menarche <- parent_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4923
subs_dhea <- dhea_f %>% distinct(participant_id) %>% pull(participant_id) # 5618

y_has_dhea_and_menarche <- intersect(subs_y_menarche, subs_dhea) # 4794
y_has_dhea_no_menarche <- setdiff(subs_dhea, subs_y_menarche) # 824
y_has_menarche_no_dhea <- setdiff(subs_y_menarche, subs_dhea) # 28

p_has_dhea_and_menarche <- intersect(subs_p_menarche, subs_dhea) # 4892
p_has_dhea_no_menarche <- setdiff(subs_dhea, subs_p_menarche) # 726
p_has_menarche_no_dhea <- setdiff(subs_p_menarche, subs_dhea) # 31

# merge into DHEA data
youth_DHEA_menarche <- youth_menarche %>%
  left_join(Exclusions, by = "participant_id")
parent_DHEA_menarche <- parent_menarche %>%
  left_join(Exclusions, by = "participant_id")

# Count subjects
dhea_y_subjects <- youth_DHEA_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4822
dhea_p_subjects <- parent_DHEA_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4923

# remove timepoints without valid dhea data
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  filter(!is.na(DHEAmean))
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  filter(!is.na(DHEAmean))

# Count subjects
dhea_y_subjects <- youth_DHEA_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4794
dhea_p_subjects <- parent_DHEA_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4892

# make numeric versions of session_id, to compare against first postmenarche, so we can keep only premenarche
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  mutate(session_id_num = case_when(
    session_id == "ses-00A" ~ 0,
    session_id == "ses-01A" ~ 1,
    session_id == "ses-02A" ~ 2,
    session_id == "ses-03A" ~ 3,
    session_id == "ses-04A" ~ 4,
    session_id == "ses-05A" ~ 5,
    session_id == "ses-06A" ~ 6))
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  mutate(session_id_num = case_when(
    session_id == "ses-00A" ~ 0,
    session_id == "ses-01A" ~ 1,
    session_id == "ses-02A" ~ 2,
    session_id == "ses-03A" ~ 3,
    session_id == "ses-04A" ~ 4,
    session_id == "ses-05A" ~ 5,
    session_id == "ses-06A" ~ 6))

#this code ensures that only premenarche data points are included in the calculation of slopes and raw differences. 
# remove any data point at or after menarche
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  filter(session_id_num < first_post_session)
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  filter(session_id_num < first_post_session)

# Count subjects
dhea_y_subjects <- youth_DHEA_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4794 -> 4727; 67
dhea_p_subjects <- parent_DHEA_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4892 -> 4811; 81


##MAKE DATA WIDE AND AVAILABILITY COUNTS

#Format wide ABCD data according to subjectID and session
data <- Exclusions %>%
  pivot_wider(names_from = session_id, values_from = c(DHEAmean))

#sum the number of available timepoints for each participant 
data$available_timepoints <- rowSums(!is.na(data[, c("ses-00A",
                                                     "ses-01A",
                                                     "ses-02A",
                                                     "ses-03A",
                                                     "ses-04A",
                                                     "ses-05A",
                                                     "ses-06A")]))


##DEMOGRAPHICS

#add sex to the data frame by matching subject IDs 
data <- data %>%
  left_join(demo %>% select(participant_id, ab_g_stc__cohort_sex), by = "participant_id")

#display timepoint totals
table(data$available_timepoints[data$ab_g_stc__cohort_sex == 1])
# MALES:
#  1    2    3    4    5    6   7 
# 460 1099 1613 1520 1034  335  39 -- 460 + 1099 + 4541 = 6100 M

table(data$available_timepoints[data$ab_g_stc__cohort_sex == 2])
# FEMALES:
#  1    2    3    4    5    6    7 
# 429 1064 1601 1302  909  269   44 -- 429 + 1064 + 4125 = 5618 F

# 460+429=889 with insufficient data to calculate change 
# 11718-889=10829 with sufficient data
# 1099+1064=2163 differences calculated
# 4541+4125=8666 slopes calculated

######### MALES AND FEMALES ##########
##SIMPLE RAW DIFFERENCE SCORE

#if values are present for baseline and year 1, but no other timepoints, calculate the raw difference score. 
data <- data %>%
  mutate(
    raw_difference_score = ifelse(!is.na(`ses-00A`) &
                                    !is.na(`ses-01A`) &
                                    is.na(`ses-02A`) &
                                    is.na(`ses-03A`) &
                                    is.na(`ses-04A`) &
                                    is.na(`ses-05A`) &
                                    is.na(`ses-06A`),
                                  `ses-01A` - `ses-00A`, 
                                  NA))



##PREP FOR SLOPE CALCULATION
#select only interview age, subject ID and session name 
interview_age <- interview_age %>%
  select(participant_id,
         session_id,
         ab_g_dyn__visit_age)

#join interview age to the long data in a new table
data_lm <- Exclusions %>%
  left_join(interview_age, by = c("participant_id", "session_id"))

#remove subs that have a raw score (2 timepoints, one of them at baseline)
already_calculated_raw <- data %>% # 1386 subjects with already calculated raw scores
  filter(!is.na(raw_difference_score) & raw_difference_score != "") %>%
  pull(participant_id)

data_lm <- data_lm %>% # 40065 -> 37293, 2772 timepoints removed
  filter(!participant_id %in% already_calculated_raw)

#remove other datapoints with less than 3 timepoints- if they have 2 timepoints, a raw difference score is calculated. 
less_than_three <- data_lm %>% # 1666 subjects with less than 3 timepoints
  group_by(participant_id) %>%
  filter(n() < 3) %>%
  distinct(participant_id) %>%
  pull(participant_id)

data_lm <- data_lm %>% # 37293 -> 34850, 2443 timepoints removed
  group_by(participant_id) %>%
  filter(n() >= 3) %>%
  ungroup()


##CALCULATE SLOPE

#extract the slope of the linear model for each subject 
data_slopes <- data_lm %>%
  group_by(participant_id) %>%
  summarise(slope_DHEAmean = lm(DHEAmean ~ ab_g_dyn__visit_age)$coef[2])

#attach the slopes back to the original data frame by participant
data <- data %>%
  left_join(data_slopes %>% select(participant_id, slope_DHEAmean), by = "participant_id")

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
timepoint_cols <- c("ses-00A",
                    "ses-01A",
                    "ses-02A",
                    "ses-03A",
                    "ses-04A",
                    "ses-05A",
                    "ses-06A")

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
  select(participant_id, raw_difference_score, notes)

#add the complex raw scores back into the table by subject ID 
data <- data %>% 
  left_join(complex_raw %>% 
              select(participant_id, 
                     raw_difference_score), 
            by = "participant_id") %>%
  mutate(raw_difference_score = coalesce(raw_difference_score.y, raw_difference_score.x)) %>%
  select(-raw_difference_score.y, -raw_difference_score.x)

#add the notes column to the data frame 
data <- data %>%
  left_join(complex_raw %>% 
              select(participant_id, 
                     notes), 
            by = "participant_id")

##SCALE RAW DIFFERENCE SCORE
#scale raw difference scores 
data$raw_difference_scale <- as.numeric(scale(data$raw_difference_score))


############ PREMENARCHE FEMALES ###############
##PREP FOR PREMENARCHE SLOPE CALCULATION
# merge in ages
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  left_join(interview_age, by = c("participant_id", "session_id"))
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  left_join(interview_age, by = c("participant_id", "session_id"))

# no missing ages

# count number of valid datapoints per participant
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  group_by(participant_id) %>%
  mutate(premenarche_timepoints = n())
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  group_by(participant_id) %>%
  mutate(premenarche_timepoints = n())

y_tps <- youth_DHEA_menarche %>% select(participant_id, premenarche_timepoints) %>% unique()
table(y_tps$premenarche_timepoints) # 925 with only 1, 2059 with 2, 1743 with 3+ (4727 total)
p_tps <- parent_DHEA_menarche %>% select(participant_id, premenarche_timepoints) %>% unique()
table(p_tps$premenarche_timepoints) # 994 with only 1, 2137 with 2, 1680 with 3+ (4811 total)

# remove those with only 1 timepoint
youth_DHEA_menarche <- youth_DHEA_menarche %>% # 10533 -> 9608, 925 subjects removed
  filter(premenarche_timepoints > 1)
parent_DHEA_menarche <- parent_DHEA_menarche %>% # 10536 -> 9542, 994 subjects removed
  filter(premenarche_timepoints > 1)

# Count subjects
dhea_y_subjects <- youth_DHEA_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4727 -> 3802; 925 removed
dhea_p_subjects <- parent_DHEA_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4811 -> 3817; 994 removed


##CALCULATE PREMENARCHE DIFFERENCES AND SLOPES
# calculate raw differences
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  group_by(participant_id) %>%
  mutate(scores = list(na.omit(DHEAmean))) %>%
  rowwise() %>%
  mutate(raw_difference_premenarche = if_else(premenarche_timepoints == 2, 
                                              last(scores) - scores[1], NA)) %>%
  select(-scores)
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  group_by(participant_id) %>%
  mutate(scores = list(na.omit(DHEAmean))) %>%
  rowwise() %>%
  mutate(raw_difference_premenarche = if_else(premenarche_timepoints == 2, 
                                              last(scores) - scores[1], NA)) %>%
  select(-scores)

#scale differences
youth_DHEA_menarche$raw_difference_premenarche_scale <- as.numeric(scale(youth_DHEA_menarche$raw_difference_premenarche))
parent_DHEA_menarche$raw_difference_premenarche_scale <- as.numeric(scale(parent_DHEA_menarche$raw_difference_premenarche))

# calculate slopes
youth_slopes <- youth_DHEA_menarche %>%
  group_by(participant_id) %>%
  summarise(slope_DHEAmean_premenarche = lm(DHEAmean ~ ab_g_dyn__visit_age)$coef[2])
parent_slopes <- parent_DHEA_menarche %>%
  group_by(participant_id) %>%
  summarise(slope_DHEAmean_premenarche = lm(DHEAmean ~ ab_g_dyn__visit_age)$coef[2])

# merge slopes with menarche data
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  left_join(youth_slopes %>% select(participant_id, slope_DHEAmean_premenarche), by = "participant_id")
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  left_join(parent_slopes %>% select(participant_id, slope_DHEAmean_premenarche), by = "participant_id")

# reset 2 timepoint slopes to NA
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  mutate(slope_DHEAmean_premenarche = if_else(premenarche_timepoints == 2, NA, slope_DHEAmean_premenarche))

#scale slopes
youth_DHEA_menarche$slope_DHEAmean_premenarche_scale <- as.numeric(scale(youth_DHEA_menarche$slope_DHEAmean_premenarche))
parent_DHEA_menarche$slope_DHEAmean_premenarche_scale <- as.numeric(scale(parent_DHEA_menarche$slope_DHEAmean_premenarche))

##ADD NOTES TO RAW DIFFERENCES

# find difference of visits for those with 2 timepoints
youth_DHEA_menarche <- youth_DHEA_menarche %>%
  group_by(participant_id) %>%
  mutate(visits = list(na.omit(session_id_num))) %>%
  rowwise() %>%
  mutate(yrs_between_visits = if_else(premenarche_timepoints == 2, 
                                      last(visits) - visits[1], NA)) %>%
  select(-visits)
parent_DHEA_menarche <- parent_DHEA_menarche %>%
  group_by(participant_id) %>%
  mutate(visits = list(na.omit(session_id_num))) %>%
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
  left_join(youth_DHEA_menarche %>% select(participant_id, premenarche_timepoints, raw_difference_premenarche, raw_difference_premenarche_scale, slope_DHEAmean_premenarche, slope_DHEAmean_premenarche_scale, premenarche_notes), by = "participant_id") %>%
  left_join(youth %>% select(participant_id, last_pre_session, first_post_session, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0), by = 'participant_id') %>%
  unique()

premenarche_parent <- data %>%
  left_join(parent_DHEA_menarche %>% select(participant_id, premenarche_timepoints, raw_difference_premenarche, raw_difference_premenarche_scale, slope_DHEAmean_premenarche, slope_DHEAmean_premenarche_scale, premenarche_notes), by = "participant_id") %>%
  left_join(parent %>% select(participant_id, last_pre_session, first_post_session, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0), by = 'participant_id') %>%
  unique()


# reorganize
premenarche_youth <- premenarche_youth %>%
  select(participant_id, ab_g_stc__cohort_sex, last_pre_session, first_post_session, premenarche_timepoints,
         slope_DHEAmean_premenarche, slope_DHEAmean_premenarche_scale, raw_difference_premenarche, raw_difference_premenarche_scale,
         premenarche_notes, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0,
         `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`) %>%
  filter(ab_g_stc__cohort_sex == 2)

premenarche_parent <- premenarche_parent %>%
  select(participant_id, ab_g_stc__cohort_sex, last_pre_session, first_post_session, premenarche_timepoints,
         slope_DHEAmean_premenarche, slope_DHEAmean_premenarche_scale, raw_difference_premenarche, raw_difference_premenarche_scale, 
         premenarche_notes, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0,
         `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`) %>%
  filter(ab_g_stc__cohort_sex == 2)

data <- data %>%
  select(participant_id, ab_g_stc__cohort_sex, available_timepoints, slope_DHEAmean, slope_DHEAmean_scale, raw_difference_score, raw_difference_scale,
         notes, `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)

##WRITE CSV

#write the completed data frame to a csv 
write_csv(data, write_file)
write_csv(premenarche_youth, youth_file)
write_csv(premenarche_parent, parent_file)

#############
# Sanity Check Counts

premen_y <- premenarche_youth %>% mutate(has_change = (!is.na(slope_DHEAmean_premenarche) | !is.na(raw_difference_premenarche)))
table(premen_y$has_change) # 3802
premen_p <- premenarche_parent %>% mutate(has_change = (!is.na(slope_DHEAmean_premenarche) | !is.na(raw_difference_premenarche)))
table(premen_p$has_change) # 3817
dhea_all <- data %>% mutate(has_change = (!is.na(slope_DHEAmean) | !is.na(raw_difference_score)))
table(dhea_all$has_change) # 10829
table(dhea_all$has_change, dhea_all$ab_g_stc__cohort_sex) # 5189 F / 5640 M

