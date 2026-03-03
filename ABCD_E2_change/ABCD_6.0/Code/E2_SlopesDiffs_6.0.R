---
#title: "E2_SlopesDiffs_6.0.R"
#author: "Aaron Clouse & Robert Toms"
#date: "12/2/2025"
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
# parent_file: premenarche slope/difference data for parent reports
# youth_file: premenarche slope/differerence data for youth reports

parent_file <- paste0(derivative_data, "parent_6.0_premenarche_E2_slopes_12_2_25.csv")
youth_file <- paste0(derivative_data, "youth_6.0_premenarche_E2_slopes_12_2_25.csv")
#______________________________________________________________________________

#OPTIONAL: this should be a demographics csv provided by ABCD. 
# this is only used to include a sex variable on the output csv. it needs two columns: 

# 1. ab_g_stc__cohort_sex 
# 2. participant_id

#NOTE: this is an optional input. it has no effect on the calculation of slopes or differences.

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
# 4. E2mean: Validated Salivary E2 measurement (pg/mL)

Exclusions <- read_csv(paste0(derivative_data, "E2_Exclusions_11_20_25.csv"))
#______________________________________________________________________________


## EXCLUSIONS

# Only select relevant columns for exclusion
Exclusions <- Exclusions %>%
  select(participant_id , session_id, E2mean, `Excluded?`)

# Exclude E2 samples with a 1 in the Exclusion column, remove subjects with no valid E2 samples
Exclusions <- Exclusions %>%
  mutate(E2mean = ifelse(`Excluded?`== 1, NA, E2mean)) %>%
  filter(!is.na(E2mean))

# count exclusions
length(table(Exclusions$participant_id)) # 5600 subjects with valid estradiol data

# Remove Excluded? column
Exclusions <- Exclusions %>%
  select(-`Excluded?`)


## POSTMENARCHE DATA

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
subs_e2 <- Exclusions %>% distinct(participant_id) %>% pull(participant_id) # 5600

y_has_e2_and_menarche <- intersect(subs_y_menarche, subs_e2) # 4786
y_has_e2_no_menarche <- setdiff(subs_e2, subs_y_menarche) # 814
y_has_menarche_no_e2 <- setdiff(subs_y_menarche, subs_e2) # 36

p_has_e2_and_menarche <- intersect(subs_p_menarche, subs_e2) # 4883
p_has_e2_no_menarche <- setdiff(subs_e2, subs_p_menarche) # 717
p_has_menarche_no_e2 <- setdiff(subs_p_menarche, subs_e2) # 40

# merge menarche and E2 data
youth_E2_menarche <- youth_menarche %>%
  left_join(Exclusions, by = "participant_id")
parent_E2_menarche <- parent_menarche %>%
  left_join(Exclusions, by = "participant_id")

# Count subjects
e2_y_subjects <- youth_E2_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4822
e2_p_subjects <- parent_E2_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4923

# remove timepoints without valid e2 data
youth_E2_menarche <- youth_E2_menarche %>%
  filter(!is.na(E2mean))
parent_E2_menarche <- parent_E2_menarche %>%
  filter(!is.na(E2mean))

# Count subjects
e2_y_subjects <- youth_E2_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4786
e2_p_subjects <- parent_E2_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4883

# make numeric versions of session_id, to compare against first postmenarche, so we can keep only premenarche
youth_E2_menarche <- youth_E2_menarche %>%
  mutate(session_id_num = case_when(
    session_id == "ses-00A" ~ 0,
    session_id == "ses-01A" ~ 1,
    session_id == "ses-02A" ~ 2,
    session_id == "ses-03A" ~ 3,
    session_id == "ses-04A" ~ 4,
    session_id == "ses-05A" ~ 5,
    session_id == "ses-06A" ~ 6))
parent_E2_menarche <- parent_E2_menarche %>%
  mutate(session_id_num = case_when(
    session_id == "ses-00A" ~ 0,
    session_id == "ses-01A" ~ 1,
    session_id == "ses-02A" ~ 2,
    session_id == "ses-03A" ~ 3,
    session_id == "ses-04A" ~ 4,
    session_id == "ses-05A" ~ 5,
    session_id == "ses-06A" ~ 6))


#### this code ensures that only premenarche data points are included in the calculation of slopes and raw differences. 
# remove any data point at or after menarche
youth_E2_menarche <- youth_E2_menarche %>%
  filter(session_id_num < first_post_session)
parent_E2_menarche <- parent_E2_menarche %>%
  filter(session_id_num < first_post_session)

# Count subjects
e2_y_subjects <- youth_E2_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4786 -> 4704
e2_p_subjects <- parent_E2_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4883 -> 4789


##PREP FOR PREMENARCHE SLOPE CALCULATION

# isolate useful age columns
interview_age <- interview_age %>%
  select(participant_id,
         session_id,
         ab_g_dyn__visit_age) %>%
  rename(interview_age = ab_g_dyn__visit_age)

# merge in ages
youth_E2_menarche <- youth_E2_menarche %>%
  left_join(interview_age, by = c("participant_id", "session_id"))
parent_E2_menarche <- parent_E2_menarche %>%
  left_join(interview_age, by = c("participant_id", "session_id"))

# no missing ages

# count number of valid datapoints per participant
youth_E2_menarche <- youth_E2_menarche %>%
  group_by(participant_id) %>%
  mutate(premenarche_timepoints = n())
parent_E2_menarche <- parent_E2_menarche %>%
  group_by(participant_id) %>%
  mutate(premenarche_timepoints = n())

y_tps <- youth_E2_menarche %>% select(participant_id, premenarche_timepoints) %>% unique()
table(y_tps$premenarche_timepoints) # 1018 with only 1, 2063 with 2, 1623 with 3+ (4704 total)
p_tps <- parent_E2_menarche %>% select(participant_id, premenarche_timepoints) %>% unique()
table(p_tps$premenarche_timepoints) # 1102 with only 1, 2132 with 2, 1555 with 3+ (4789 total)

# remove those with only 1 timepoint
youth_E2_menarche <- youth_E2_menarche %>% # 10254 -> 9236, 1018 subjects removed
  filter(premenarche_timepoints > 1)
parent_E2_menarche <- parent_E2_menarche %>% # 10241 -> 9139, 1102 subjects removed
  filter(premenarche_timepoints > 1)

# Count subjects
e2_y_subjects <- youth_E2_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4704 -> 3686 ; 1018
e2_p_subjects <- parent_E2_menarche %>% distinct(participant_id) %>% pull(participant_id) # 4789 -> 3687 ; 1102

##CALCULATE PREMENARCHE DIFFERENCES AND SLOPES

# calculate raw differences
youth_E2_menarche <- youth_E2_menarche %>%
  group_by(participant_id) %>%
  mutate(scores = list(na.omit(E2mean))) %>%
  rowwise() %>%
  mutate(raw_difference_premenarche = if_else(premenarche_timepoints == 2, 
                                              last(scores) - scores[1], NA)) %>%
  select(-scores)
parent_E2_menarche <- parent_E2_menarche %>%
  group_by(participant_id) %>%
  mutate(scores = list(na.omit(E2mean))) %>%
  rowwise() %>%
  mutate(raw_difference_premenarche = if_else(premenarche_timepoints == 2, 
                                              last(scores) - scores[1], NA)) %>%
  select(-scores)

#scale differences
youth_E2_menarche$raw_difference_premenarche_scale <- as.numeric(scale(youth_E2_menarche$raw_difference_premenarche))
parent_E2_menarche$raw_difference_premenarche_scale <- as.numeric(scale(parent_E2_menarche$raw_difference_premenarche))

# calculate slopes
youth_slopes <- youth_E2_menarche %>%
  group_by(participant_id) %>%
  summarise(slope_E2mean_premenarche = lm(E2mean ~ interview_age)$coef[2])
parent_slopes <- parent_E2_menarche %>%
  group_by(participant_id) %>%
  summarise(slope_E2mean_premenarche = lm(E2mean ~ interview_age)$coef[2])

# merge slopes with menarche data
youth_E2_menarche <- youth_E2_menarche %>%
  left_join(youth_slopes %>% select(participant_id, slope_E2mean_premenarche), by = "participant_id")
parent_E2_menarche <- parent_E2_menarche %>%
  left_join(parent_slopes %>% select(participant_id, slope_E2mean_premenarche), by = "participant_id")

# reset 2 timepoint slopes to NA
youth_E2_menarche <- youth_E2_menarche %>%
  mutate(slope_E2mean_premenarche = if_else(premenarche_timepoints == 2, NA, slope_E2mean_premenarche))

#scale slopes
youth_E2_menarche$slope_E2mean_premenarche_scale <- as.numeric(scale(youth_E2_menarche$slope_E2mean_premenarche))
parent_E2_menarche$slope_E2mean_premenarche_scale <- as.numeric(scale(parent_E2_menarche$slope_E2mean_premenarche))


##ADD NOTES TO RAW DIFFERENCES


# find difference of visits for those with 2 timepoints
youth_E2_menarche <- youth_E2_menarche %>%
  group_by(participant_id) %>%
  mutate(visits = list(na.omit(session_id_num))) %>%
  rowwise() %>%
  mutate(yrs_between_visits = if_else(premenarche_timepoints == 2, 
                                      last(visits) - visits[1], NA)) %>%
  select(-visits)
parent_E2_menarche <- parent_E2_menarche %>%
  group_by(participant_id) %>%
  mutate(visits = list(na.omit(session_id_num))) %>%
  rowwise() %>%
  mutate(yrs_between_visits = if_else(premenarche_timepoints == 2, 
                                      last(visits) - visits[1], NA)) %>%
  select(-visits)

#note if the participants raw difference score was greater that 1yr apart
youth_E2_menarche <- youth_E2_menarche %>%
  rowwise() %>%
  mutate(premenarche_notes = ifelse(yrs_between_visits == 1,
                                    NA,
                                    paste("two timepoints", yrs_between_visits, "years apart"))) %>%
  select(-yrs_between_visits)
parent_E2_menarche <- parent_E2_menarche %>%
  rowwise() %>%
  mutate(premenarche_notes = ifelse(yrs_between_visits == 1,
                                    NA,
                                    paste("two timepoints", yrs_between_visits, "years apart"))) %>%
  select(-yrs_between_visits)


##FORMAT DATAFRAMES FOR EXPORT


#Format wide ABCD data according to subjectID and session
youth_flags <- youth %>%
  select(participant_id, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)
parent_flags <- parent %>%
  select(participant_id, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)

data <- Exclusions %>%
  pivot_wider(names_from = session_id, values_from = c(E2mean))

# join E2 data from each visit for reference
premenarche_youth <- data %>%
  left_join(youth_E2_menarche %>% select(participant_id, first_post_session, premenarche_timepoints, raw_difference_premenarche, raw_difference_premenarche_scale, slope_E2mean_premenarche, slope_E2mean_premenarche_scale, premenarche_notes), by = "participant_id") %>%
  unique() %>%
  left_join(youth_flags, by = "participant_id")
premenarche_parent <- data %>%
  left_join(parent_E2_menarche %>% select(participant_id, first_post_session, premenarche_timepoints, raw_difference_premenarche, raw_difference_premenarche_scale, slope_E2mean_premenarche, slope_E2mean_premenarche_scale, premenarche_notes), by = "participant_id") %>%
  unique() %>%
  left_join(parent_flags, by = "participant_id")

# reorganize
premenarche_youth <- premenarche_youth %>%
  select(participant_id, first_post_session, premenarche_timepoints,
         slope_E2mean_premenarche, slope_E2mean_premenarche_scale, raw_difference_premenarche, raw_difference_premenarche_scale,
         premenarche_notes, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0,
         `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)
premenarche_parent <- premenarche_parent %>%
  select(participant_id, first_post_session, premenarche_timepoints,
         slope_E2mean_premenarche, slope_E2mean_premenarche_scale, raw_difference_premenarche, raw_difference_premenarche_scale, 
         premenarche_notes, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0,
         `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)


##WRITE CSV

#write the completed data frame to a csv 
write_csv(premenarche_youth, youth_file)
write_csv(premenarche_parent, parent_file)

#############
# Sanity Check Counts

premen_y <- premenarche_youth %>% mutate(has_change = (!is.na(slope_E2mean_premenarche) | !is.na(raw_difference_premenarche)))
table(premen_y$has_change)
premen_p <- premenarche_parent %>% mutate(has_change = (!is.na(slope_E2mean_premenarche) | !is.na(raw_difference_premenarche)))
table(premen_p$has_change)
