#---
  #title: "E2_SlopesDiffs_5.0.R"
  #author: "Aaron Clouse & Robert Toms"
  #date: "2/6/2026"
#---
  
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
# parent_file: premenarche slope/difference data for parent reports
# youth_file: premenarche slope/differerence data for youth reports

parent_file <- paste0(derivative_data, "parent_6.0_premenarche_E2_slopes_2_6_26.csv")
youth_file <- paste0(derivative_data, "youth_6.0_premenarche_E2_slopes_2_6_26.csv")
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
# 4. E2mean: Validated Salivary DHEA measurement (pg/mL)

Exclusions <- read_csv(paste0(derivative_data, "E2_Exclusions_5.0_12_5_25.csv")) %>% rename(src_subject_id = SubjectID, eventname = EventName)
#______________________________________________________________________________


## EXCLUSIONS

# Only select relevant columns for exclusion
Exclusions <- Exclusions %>%
  select(src_subject_id, eventname, E2mean, `Excluded?`)

# Exclude E2 samples with a 1 in the Exclusion column, remove subjects with no valid E2 samples
Exclusions <- Exclusions %>%
  mutate(E2mean = ifelse(`Excluded?`== 1, NA, E2mean)) %>%
  filter(!is.na(E2mean))

# count exclusions
length(table(Exclusions$src_subject_id)) # 5574 subjects with valid estradiol data

# Remove Excluded? column, all remaining passed sample exclusion
Exclusions <- Exclusions %>%
  select(-`Excluded?`)


## POSTMENARCHE DATA

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
subs_e2 <- Exclusions %>% distinct(src_subject_id) %>% pull(src_subject_id) # 5574

y_has_e2_and_menarche <- intersect(subs_y_menarche, subs_e2) # 3791
y_has_e2_no_menarche <- setdiff(subs_e2, subs_y_menarche) # 1783
y_has_menarche_no_e2 <- setdiff(subs_y_menarche, subs_e2) # 42

p_has_e2_and_menarche <- intersect(subs_p_menarche, subs_e2) # 3967
p_has_e2_no_menarche <- setdiff(subs_e2, subs_p_menarche) # 1607
p_has_menarche_no_e2 <- setdiff(subs_p_menarche, subs_e2) # 44

# merge menarche and E2 data
youth_E2_menarche <- youth_menarche %>%
  left_join(Exclusions, by = "src_subject_id")
parent_E2_menarche <- parent_menarche %>%
  left_join(Exclusions, by = "src_subject_id")

# Count subjects
e2_y_subjects <- youth_E2_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 3833
e2_p_subjects <- parent_E2_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 4011

# remove timepoints without valid e2 data
youth_E2_menarche <- youth_E2_menarche %>%
  filter(!is.na(E2mean))
parent_E2_menarche <- parent_E2_menarche %>%
  filter(!is.na(E2mean))

# Count subjects
e2_y_subjects <- youth_E2_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 3833 -> 3791, -42
e2_p_subjects <- parent_E2_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 4011 -> 3967, -44

# make numeric versions of eventname, to compare against first postmenarche, so we can keep only premenarche
youth_E2_menarche <- youth_E2_menarche %>%
  mutate(eventname_num = case_when(
    eventname == "baseline_year_1_arm_1" ~ 0,
    eventname == "1_year_follow_up_y_arm_1" ~ 1,
    eventname == "2_year_follow_up_y_arm_1" ~ 2,
    eventname == "3_year_follow_up_y_arm_1" ~ 3,
    eventname == "4_year_follow_up_y_arm_1" ~ 4))
parent_E2_menarche <- parent_E2_menarche %>%
  mutate(eventname_num = case_when(
    eventname == "baseline_year_1_arm_1" ~ 0,
    eventname == "1_year_follow_up_y_arm_1" ~ 1,
    eventname == "2_year_follow_up_y_arm_1" ~ 2,
    eventname == "3_year_follow_up_y_arm_1" ~ 3,
    eventname == "4_year_follow_up_y_arm_1" ~ 4))


#### this code ensures that only premenarche data points are included in the calculation of slopes and raw differences. 
# remove any data point at or after menarche
youth_E2_menarche <- youth_E2_menarche %>%
  filter(eventname_num < first_post_event)
parent_E2_menarche <- parent_E2_menarche %>%
  filter(eventname_num < first_post_event)

# Count subjects
e2_y_subjects <- youth_E2_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 3791 -> 3729, -62
e2_p_subjects <- parent_E2_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 3967 -> 3893, -74


##PREP FOR PREMENARCHE SLOPE CALCULATION

# merge in ages
youth_E2_menarche <- youth_E2_menarche %>%
  left_join(interview_age, by = c("src_subject_id", "eventname"))
parent_E2_menarche <- parent_E2_menarche %>%
  left_join(interview_age, by = c("src_subject_id", "eventname"))

# no missing ages

# count number of valid datapoints per participant
youth_E2_menarche <- youth_E2_menarche %>%
  group_by(src_subject_id) %>%
  mutate(premenarche_timepoints = n())
parent_E2_menarche <- parent_E2_menarche %>%
  group_by(src_subject_id) %>%
  mutate(premenarche_timepoints = n())

y_tps <- youth_E2_menarche %>% select(src_subject_id, premenarche_timepoints) %>% unique()
table(y_tps$premenarche_timepoints) # 903 with only 1, 1677 with 2, 1149 with 3+ (3729 total)
p_tps <- parent_E2_menarche %>% select(src_subject_id, premenarche_timepoints) %>% unique()
table(p_tps$premenarche_timepoints) # 1000 with only 1, 1771 with 2, 1122 with 3+ (4789 total)

# remove those with only 1 timepoint
youth_E2_menarche <- youth_E2_menarche %>% # 7801 -> 6898, 903 subjects removed
  filter(premenarche_timepoints > 1)
parent_E2_menarche <- parent_E2_menarche %>% # 7999 -> 6999, 1000 subjects removed
  filter(premenarche_timepoints > 1)

# Count subjects
e2_y_subjects <- youth_E2_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 3729 -> 2826 ; 903
e2_p_subjects <- parent_E2_menarche %>% distinct(src_subject_id) %>% pull(src_subject_id) # 3893 -> 2893 ; 1000


##CALCULATE PREMENARCHE DIFFERENCES AND SLOPES

# calculate raw differences
youth_E2_menarche <- youth_E2_menarche %>%
  group_by(src_subject_id) %>%
  mutate(scores = list(na.omit(E2mean))) %>%
  rowwise() %>%
  mutate(raw_difference_premenarche = if_else(premenarche_timepoints == 2, 
                                              last(scores) - scores[1], NA)) %>%
  select(-scores)
parent_E2_menarche <- parent_E2_menarche %>%
  group_by(src_subject_id) %>%
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
  group_by(src_subject_id) %>%
  summarise(slope_E2mean_premenarche = lm(E2mean ~ interview_age)$coef[2])
parent_slopes <- parent_E2_menarche %>%
  group_by(src_subject_id) %>%
  summarise(slope_E2mean_premenarche = lm(E2mean ~ interview_age)$coef[2])

# merge slopes with menarche data
youth_E2_menarche <- youth_E2_menarche %>%
  left_join(youth_slopes %>% select(src_subject_id, slope_E2mean_premenarche), by = "src_subject_id")
parent_E2_menarche <- parent_E2_menarche %>%
  left_join(parent_slopes %>% select(src_subject_id, slope_E2mean_premenarche), by = "src_subject_id")

# reset 2 timepoint slopes to NA
youth_E2_menarche <- youth_E2_menarche %>%
  mutate(slope_E2mean_premenarche = if_else(premenarche_timepoints == 2, NA, slope_E2mean_premenarche))

#scale slopes
youth_E2_menarche$slope_E2mean_premenarche_scale <- as.numeric(scale(youth_E2_menarche$slope_E2mean_premenarche))
parent_E2_menarche$slope_E2mean_premenarche_scale <- as.numeric(scale(parent_E2_menarche$slope_E2mean_premenarche))


##ADD NOTES TO RAW DIFFERENCES


# find difference of visits for those with 2 timepoints
youth_E2_menarche <- youth_E2_menarche %>%
  group_by(src_subject_id) %>%
  mutate(visits = list(na.omit(eventname_num))) %>%
  rowwise() %>%
  mutate(yrs_between_visits = if_else(premenarche_timepoints == 2, 
                                      last(visits) - visits[1], NA)) %>%
  select(-visits)
parent_E2_menarche <- parent_E2_menarche %>%
  group_by(src_subject_id) %>%
  mutate(visits = list(na.omit(eventname_num))) %>%
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


#Format wide ABCD data according to subjectID and event
youth_flags <- youth %>%
  select(src_subject_id, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)
parent_flags <- parent %>%
  select(src_subject_id, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)

data <- Exclusions %>%
  pivot_wider(names_from = eventname, values_from = c(E2mean))

# join E2 data from each visit for reference
premenarche_youth <- data %>%
  left_join(youth_E2_menarche %>% select(src_subject_id, first_post_event, premenarche_timepoints, raw_difference_premenarche, raw_difference_premenarche_scale, slope_E2mean_premenarche, slope_E2mean_premenarche_scale, premenarche_notes), by = "src_subject_id") %>%
  unique() %>%
  left_join(youth_flags, by = "src_subject_id")
premenarche_parent <- data %>%
  left_join(parent_E2_menarche %>% select(src_subject_id, first_post_event, premenarche_timepoints, raw_difference_premenarche, raw_difference_premenarche_scale, slope_E2mean_premenarche, slope_E2mean_premenarche_scale, premenarche_notes), by = "src_subject_id") %>%
  unique() %>%
  left_join(parent_flags, by = "src_subject_id")

# reorganize
premenarche_youth <- premenarche_youth %>%
  select(src_subject_id, first_post_event, premenarche_timepoints,
         slope_E2mean_premenarche, slope_E2mean_premenarche_scale, raw_difference_premenarche, raw_difference_premenarche_scale,
         premenarche_notes, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0,
         baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)
premenarche_parent <- premenarche_parent %>%
  select(src_subject_id, first_post_event, premenarche_timepoints,
         slope_E2mean_premenarche, slope_E2mean_premenarche_scale, raw_difference_premenarche, raw_difference_premenarche_scale, 
         premenarche_notes, PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0,
         baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`, `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)

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
