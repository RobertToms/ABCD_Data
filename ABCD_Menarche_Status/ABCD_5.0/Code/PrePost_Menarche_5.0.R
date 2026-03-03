############################
# PrePost_Menarche_5.0.R
# Author: Robert Toms
# Date: 12/2/2025
############################

# Import everything
library(tidyverse)
library(lubridate)

sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"

youth_pds <- read_csv(paste0(sourcepath, '/physical-health/ph_y_pds.csv'))
parent_pds <- read_csv(paste0(sourcepath, '/physical-health/ph_p_pds.csv'))
sex <- read_csv(paste0(sourcepath, '/abcd-general/abcd_p_demo.csv'))

#################
# IN THE BELOW CODE BLOCK
# Clean and Prepare Data
#################

# keep only necessary columns
sex <- sex %>%
  select(src_subject_id, demo_sex_v2) %>%
  filter(!is.na(demo_sex_v2))

youth <- youth_pds %>%
  select(src_subject_id, eventname, 
         pds_f5_y) # Have you begun to menstruate (started to have your period)? No = 1; Yes = 4; Decline to Answer = 777, I don't know = 999
parent <- parent_pds %>%
  select(src_subject_id, eventname, 
         pds_f5b_p) # Has your child begun to menstruate? No = 1; Yes = 4; I don't know = 999

# merge
youth <- youth %>%
  left_join(sex, by = 'src_subject_id')
parent <- parent %>%
  left_join(sex, by = 'src_subject_id')

# keep only females, as only females had menarche data collected
youth <- youth %>%
  filter(demo_sex_v2 == 2) # 49151 -> 23417, 25734 tps dropped
parent <- parent %>%
  filter(demo_sex_v2 == 2) # 49151 -> 23417, 25734 tps dropped
sex <- sex %>%
  filter(demo_sex_v2 == 2) # 11868 -> 5677, 6191 subjects dropped (6188 Male, 3 Intersex Male)

# remove sex column
youth <- youth %>%
  select(-demo_sex_v2)
parent <- parent %>%
  select(-demo_sex_v2)

## Count timepoints and subjects with 999s or 777s
youth_999 <- youth %>%            
  filter(pds_f5_y == 999) # 680 tps
youth_777 <- youth %>%            
  filter(pds_f5_y == 777) # 787 tps
parent_999 <- parent %>%
  filter(pds_f5b_p == 999) # 362 tps
y_999_subjects <- length(table(youth_999$src_subject_id)) # 680 subjects affected
y_777_subjects <- length(table(youth_777$src_subject_id)) # 628 subjects affected
p_999_subjects <- length(table(parent_999$src_subject_id)) # 298 subjects affected

# set 999s and 777s to NA, per best practices of (Herting et al., 2021)
youth <- youth %>%
  mutate(pds_f5_y = if_else(pds_f5_y == '999' | pds_f5_y == '777', NA, pds_f5_y))
parent <- parent %>%
  mutate(pds_f5b_p = if_else(pds_f5b_p == '999' | pds_f5b_p == '777', NA, pds_f5b_p))

# pivot wide
youth <- youth %>%
  pivot_wider(names_from = eventname, values_from = pds_f5_y)
parent <- parent %>%
  pivot_wider(names_from = eventname, values_from = pds_f5b_p)

# make list vector of menarche reports
youth <- youth %>%
  group_by(src_subject_id) %>%
  mutate(menarche_reports = list(c(baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`,
                                   `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)))
parent <- parent %>%
  group_by(src_subject_id) %>%
  mutate(menarche_reports = list(c(baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`,
                                   `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`)))

# count subjects with no data
youth_na_subs <- youth %>%
  rowwise() %>%
  filter(is.na(baseline_year_1_arm_1) & is.na(`1_year_follow_up_y_arm_1`) &
           is.na(`2_year_follow_up_y_arm_1`) & is.na(`3_year_follow_up_y_arm_1`) & is.na(`4_year_follow_up_y_arm_1`)) # 33 subjects with no usable data
parent_na_subs <- parent %>%
  rowwise() %>%
  filter(is.na(baseline_year_1_arm_1) & is.na(`1_year_follow_up_y_arm_1`) &
           is.na(`2_year_follow_up_y_arm_1`) & is.na(`3_year_follow_up_y_arm_1`) & is.na(`4_year_follow_up_y_arm_1`)) # 17 subjects with no usable data

# Youth: 5677 females - 33 = 5644 females with any usable data
# Parent: 5677 females - 17 = 5660 females with any usable data

# remove subjects with no data
youth <- youth %>%
  rowwise() %>%
  filter(!is.na(baseline_year_1_arm_1) | !is.na(`1_year_follow_up_y_arm_1`) |
           !is.na(`2_year_follow_up_y_arm_1`) | !is.na(`3_year_follow_up_y_arm_1`) | !is.na(`4_year_follow_up_y_arm_1`)) # 5644 subjects remaining
parent <- parent %>%
  rowwise() %>%
  filter(!is.na(baseline_year_1_arm_1) | !is.na(`1_year_follow_up_y_arm_1`) |
           !is.na(`2_year_follow_up_y_arm_1`) | !is.na(`3_year_follow_up_y_arm_1`) | !is.na(`4_year_follow_up_y_arm_1`)) # 5660 subjects remaining

#################
# IN THE BELOW CODE BLOCK
# Find Last PreMenarche / First PostMenarche Timepoints
#################

## Find Last Premenarche Timepoint
# Youth
youth$last_pre_index <- sapply(youth$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(1))  # Find pre-menarche "1" occurrences
  if (length(post_men_indices) > 0) {
    index <- tail(post_men_indices, 1)  # Get last occurrence
    return(index)
  } else {
    return(NA)  # No pre-menarche occurrence
  }
})

# Parent
parent$last_pre_index <- sapply(parent$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(1))  # Find pre-menarche "1" occurrences
  if (length(post_men_indices) > 0) {
    index <- tail(post_men_indices, 1)  # Get last occurrence
    return(index)
  } else {
    return(NA)  # No pre-menarche occurrence
  }
})

## Find First Postmenarche timepoint
# Parent
youth$first_post_index <- sapply(youth$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(4))  # Find post-menarche "4" occurrences
  if (length(post_men_indices) > 0) {
    index <- post_men_indices[1]  # Get first occurrence
    return(index)
  } else {
    return(NA)  # No post-menarche occurrence
  }
})
# Youth
parent$first_post_index <- sapply(parent$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(4))  # Find post-menarche "4" occurrences
  if (length(post_men_indices) > 0) {
    index <- post_men_indices[1]  # Get first occurrence
    return(index)
  } else {
    return(NA)  # No post-menarche occurrence
  }
})

# Convert indexes to timepoints by subtracting 1
youth$last_pre_event <- youth$last_pre_index - 1
parent$last_pre_event <- parent$last_pre_index - 1
youth$first_post_event <- youth$first_post_index - 1
parent$first_post_event <- parent$first_post_index - 1

#################
# IN THE BELOW CODE BLOCK
# Flag for PreMenarche at Last report, Postmenarche at first report, and Inconsistent reporting (premenarche after postmenarche)
#################

# POSTMENARCHE AT BASELINE: Flagged if baseline report is Postmenarche ("4")
# NO BASELINE REPORT: Flagged if baseline report is NA
# PREMENARCHE AT LAST REPORT: If no postmenarche reports, premenarche at all reports (including last) are assumed
# INCONSISTENT REPORTING: If premenarche status is reported after a postmenarche report (1 after a 4), this is considered Inconsistent reporting

youth <- youth %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(baseline_year_1_arm_1 == "4", 1, 0)) %>%
  mutate(NoBaselineReport_Y1N0 = if_else(is.na(baseline_year_1_arm_1), 1, 0)) %>%
  mutate(PreMenarche_at_LastReport_Y1N0 = if_else(is.na(first_post_event), 1, 0)) %>%
  mutate(Inconsistent_Reporting_Y1N0 = if_else(!is.na(first_post_event) && !is.na(last_pre_event) && first_post_event < last_pre_event, 1, 0))

parent <- parent %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(baseline_year_1_arm_1 == "4", 1, 0)) %>%
  mutate(NoBaselineReport_Y1N0 = if_else(is.na(baseline_year_1_arm_1), 1, 0)) %>%
  mutate(PreMenarche_at_LastReport_Y1N0 = if_else(is.na(first_post_event), 1, 0)) %>%
  mutate(Inconsistent_Reporting_Y1N0 = if_else(!is.na(first_post_event) && !is.na(last_pre_event) && first_post_event < last_pre_event, 1, 0))


# count flags
table(youth$PostMenarche_at_Baseline_Y1N0)  # 165 PostMenarche at Baseline
table(youth$NoBaselineReport_Y1N0)          # 610 Missing Baseline Report
table(youth$PreMenarche_at_LastReport_Y1N0) # 1553 PreMenarche at last report
table(youth$Inconsistent_Reporting_Y1N0)    # 89 Reported Inconsistently
table(youth$Inconsistent_Reporting_Y1N0, youth$PostMenarche_at_Baseline_Y1N0) # 35 Inconsistently reported and were Postmenarche at baseline
table(youth$Inconsistent_Reporting_Y1N0, youth$NoBaselineReport_Y1N0) # 7 Inconsistently reported and are missing their Baseline report
table(youth$NoBaselineReport_Y1N0, youth$PreMenarche_at_LastReport_Y1N0) # 225 Missing Baseline and Premenarche at last report

table(parent$PostMenarche_at_Baseline_Y1N0)  # 155 PostMenarche at Baseline
table(parent$NoBaselineReport_Y1N0)          # 53 Missing Baseline Report
table(parent$PreMenarche_at_LastReport_Y1N0) # 1477 PreMenarche at last report
table(parent$Inconsistent_Reporting_Y1N0)    # 56 Reported Inconsistently
table(parent$Inconsistent_Reporting_Y1N0, parent$PostMenarche_at_Baseline_Y1N0) # 11 Inconsistently reported and were Postmenarche at baseline
table(parent$Inconsistent_Reporting_Y1N0, parent$NoBaselineReport_Y1N0) # 0 Inconsistently reported and are missing their Baseline report
table(parent$NoBaselineReport_Y1N0, parent$PreMenarche_at_LastReport_Y1N0) # 17 Missing Baseline and Premenarche at last report

### The following are all the possible reporting cases, and if we can determine a valid pre/post menarche transition from them ###
#
#   Consistent Reporters (no premenarche reports after first postmenarche report)
#
#       Normal/No Flags - Will have valid pre/post timepoints
#       Premenarche at Last Report - has no postmenarche timepoints, so will not have a valid pre/post transition
#       Postmenarche at Baseline - has no clear premenarche report, cannot determine valid pre/post transition due to limits of self-report data
#       No Baseline Report
#               Normal/No other flags - Consistently reported premenarche then postmenarche timepoints, so will have a valid pre/post transition
#               Premenarche at Last Report - has no postmenarche timepoints, so will not have pre/post transition
#               Postmenarche at First Report - has no clear premenarche timepoint, so cannot determine pre/post transition due to limits of self report data
# 
#   Inconsistent Reporters (premenarche report(s) after first postmenarche report)
#
#       Inconsistent with No other flags - Will have their reports corrected, see the next section below. Will have potentially valid pre/post transition (see NOTE in next section below).
#       Postmenarche at Baseline - Likely an error of reporting or data entry, as menarche before age 9 is highly unlikely. Cannot determine valid pre/post transition due to unclear/errored data.
#       No Baseline Report - baseline not confirmable as pre or post and following timepoints were reported inconsistently, so cannot pinpoint valid pre/post transition 
#               Premenarche at Last Report - see above, cannot find a valid pre/post transition
#               Postmenarche at First Report - see above, cannot find a valid pre/post transition
#


#################
# IN THE BELOW CODE BLOCK
# Find adjusted premenarche timepoints for Inconsistent reporters
#################

# Subjects who reported inconsistently may have their reported pre/post menarche transition time corrected. The reasoning for this is as follows:
# In these cases, some young women may experience irregular cycling during menarche onset, where bleeding occurs sporadically, 
# leading to confusion as to whether menarche has truly occurred. It may be the case that the onset of this irregular cycling 
# is the result of the cascading biological effects that lead to menarche onset, thus-—though irregular-—these may potentially be 
# treated as menarche events. In such cases it is likely that the large organizational effects of rising estradiol would have 
# still occurred with the first menarche, even if the acute changes related to menstrual cycle fluctuations do not occur. 
# If so, then we would expect no differences in the organizational effects of estradiol from the consistent versus inconsistent reporters. 

# NOTE: Analyses should be run with and without the individuals flagged with a "1" in the Inconsistent_Reporting_Y1N0 column, to check
# for any differences in effects.

# These corrections take this form:
# Where an individual's last premenarche timepoint is reported to be after their first postmenarche timepoint,
# the last premenarche timepoint PRIOR TO the first postmenarche timepoint is substituted.
# Example:
# c( 1 , 1 , 4 , 1 , 4 ) , where 1 = Pre, 4 = Post
#        ^   ^   ^        
#        a   b   c          "a" gets substituted for "c" as the last premenarche timepoint
#


## Define function that finds most recent premenarche timepoint with menarche data prior to first postmenarche report
find_premenarche <- function(menarche_reports) {
  first_postmenarche <- which(menarche_reports == 4)[1]  # Find the first occurrence of 4 (postmenarche)
  if (is.na(first_postmenarche)) return(NA) # No 4 found
  preceding_1s <- which(menarche_reports[1:(first_postmenarche - 1)] == 1) # Find all 1s before that
  if (length(preceding_1s) == 0) return(NA) # No 1s before the 4
  return(tail(preceding_1s, 1)) # Return the last 1 before the 4
}

# apply function
youth <- youth %>%
  rowwise() %>%
  mutate(Inconsistent_Pre = find_premenarche(menarche_reports)) %>%
  # Convert index to timepoint by subtracting 1
  mutate(Inconsistent_Pre = Inconsistent_Pre - 1)
parent <- parent %>%
  rowwise() %>%
  mutate(Inconsistent_Pre = find_premenarche(menarche_reports)) %>%
  # Convert index to timepoint by subtracting 1
  mutate(Inconsistent_Pre = Inconsistent_Pre - 1)

# count changes
table(youth$Inconsistent_Reporting_Y1N0 & youth$NoBaselineReport_Y1N0 == 0, youth$Inconsistent_Pre) # 28 + 15 + 4 = 47 to be changed/corrected
table(parent$Inconsistent_Reporting_Y1N0 & parent$NoBaselineReport_Y1N0 == 0, parent$Inconsistent_Pre) # 23 + 19 + 3 = 45 to be changed/corrected

# There are also inconsistent reporters with a Postmenarche at Baseline Flag. By not including this flag in the following 2 calls, 
# we allow these to replace the incorrect premenarche timepoint with an accurate NA - accurate because we cannot verify the correct premenarche timepoint.

# set Inconsistent reporter's pre sessions to corrected versions -- if also no baseline report, set last_pre_event to NA
youth <- youth %>%
  mutate(last_pre_event = if_else(Inconsistent_Reporting_Y1N0 == 1 & NoBaselineReport_Y1N0 == 0, Inconsistent_Pre, last_pre_event, missing = last_pre_event)) %>%
  mutate(last_pre_event = if_else(Inconsistent_Reporting_Y1N0 == 1 & NoBaselineReport_Y1N0 == 1, NA, last_pre_event, missing = last_pre_event))
parent <- parent %>%
  mutate(last_pre_event = if_else(Inconsistent_Reporting_Y1N0 == 1 & NoBaselineReport_Y1N0 == 0, Inconsistent_Pre, last_pre_event, missing = last_pre_event)) %>%
  mutate(last_pre_event = if_else(Inconsistent_Reporting_Y1N0 == 1 & NoBaselineReport_Y1N0 == 1, NA, last_pre_event, missing = last_pre_event))

# count subjects affected by missing premenarche and missing postmenarche events
y_no_pre <- youth %>% filter(is.na(last_pre_event))
y_no_pre_num <- length(table(y_no_pre$src_subject_id)) # 258 subjects affected
y_no_post <- youth %>% filter(is.na(first_post_event))
y_no_post_num <- length(table(y_no_post$src_subject_id)) # 1553 subjects affected

p_no_pre <- parent %>% filter(is.na(last_pre_event))
p_no_pre_num <- length(table(p_no_pre$src_subject_id)) # 172 subjects affected
p_no_post <- parent %>% filter(is.na(first_post_event))
p_no_post_num <- length(table(p_no_post$src_subject_id)) # 1477 subjects affected


#################
# IN THE BELOW CODE BLOCK
# Clean for Export, Flag Counts, & Perform Sanity Checks
#################

# keep and order useful columns
youth <- youth %>%
  select(src_subject_id, last_pre_event, first_post_event, menarche_reports,
         baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`,
         `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`,
         PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)
parent <- parent %>%
  select(src_subject_id, last_pre_event, first_post_event, menarche_reports,
         baseline_year_1_arm_1, `1_year_follow_up_y_arm_1`, `2_year_follow_up_y_arm_1`,
         `3_year_follow_up_y_arm_1`, `4_year_follow_up_y_arm_1`,
         PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)

####### Clean Flag Counts for Reporting #######
# For clarity, each subject will only be flagged with 1 primary flag.

##### No Baseline Report and Inconsistent Reporting
# reset Inconsistent Reporting to 0 if No Baseline Report - since they are missing data and inconsistent, we can't know their true pre/post transition
sum(youth$NoBaselineReport_Y1N0 == 1 & youth$Inconsistent_Reporting_Y1N0 == 1) # 7
sum(parent$NoBaselineReport_Y1N0 == 1 & parent$Inconsistent_Reporting_Y1N0 == 1) # 0
youth <- youth %>%
  mutate(Inconsistent_Reporting_Y1N0 = if_else(NoBaselineReport_Y1N0 == 1, 0, Inconsistent_Reporting_Y1N0))
parent <- parent %>%
  mutate(Inconsistent_Reporting_Y1N0 = if_else(NoBaselineReport_Y1N0 == 1, 0, Inconsistent_Reporting_Y1N0))
sum(youth$NoBaselineReport_Y1N0 == 1 & youth$Inconsistent_Reporting_Y1N0) # 0
sum(parent$NoBaselineReport_Y1N0 == 1 & parent$Inconsistent_Reporting_Y1N0 == 1) # 0

##### Postmenarche at Baseline and Inconsistent Reporting
# Reset Postmenarche at Baseline to 0 if also inconsistent - likely an error of either reporting or data entry due to unlikely menarche before age 9, cannot determine a validated pre/post transition due to unclear/errored data.
sum(youth$PostMenarche_at_Baseline_Y1N0 == 1 & youth$Inconsistent_Reporting_Y1N0 == 1) # 35
sum(parent$PostMenarche_at_Baseline_Y1N0 == 1 & parent$Inconsistent_Reporting_Y1N0 == 1) # 11
youth <- youth %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(PostMenarche_at_Baseline_Y1N0 == 1 & Inconsistent_Reporting_Y1N0 == 1, 0, PostMenarche_at_Baseline_Y1N0))
parent <- parent %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(PostMenarche_at_Baseline_Y1N0 == 1 & Inconsistent_Reporting_Y1N0 == 1, 0, PostMenarche_at_Baseline_Y1N0))
sum(youth$PostMenarche_at_Baseline_Y1N0 == 1 & youth$Inconsistent_Reporting_Y1N0 == 1) # 0
sum(parent$PostMenarche_at_Baseline_Y1N0 == 1 & parent$Inconsistent_Reporting_Y1N0 == 1) # 0

##### No Baseline Report and Premenarche at Last Report
# Reset no baseline at last report flag to 0, leave premenarche at last report flag as 1-- consistent reporting means we could have determined a pre/post menarche transition, if there had been one.
sum(youth$NoBaselineReport_Y1N0 == 1 & youth$PreMenarche_at_LastReport_Y1N0 == 1) # 225
sum(parent$NoBaselineReport_Y1N0 == 1 & parent$PreMenarche_at_LastReport_Y1N0 == 1) # 17
youth <- youth %>%
  mutate(NoBaselineReport_Y1N0 = if_else(PreMenarche_at_LastReport_Y1N0 == 1 & NoBaselineReport_Y1N0 == 1, 0, NoBaselineReport_Y1N0))
parent <- parent %>%
  mutate(NoBaselineReport_Y1N0 = if_else(PreMenarche_at_LastReport_Y1N0 == 1 & NoBaselineReport_Y1N0 == 1, 0, NoBaselineReport_Y1N0))
sum(youth$NoBaselineReport_Y1N0 == 1 & youth$PreMenarche_at_LastReport_Y1N0 == 1) # 0
sum(parent$NoBaselineReport_Y1N0 == 1 & parent$PreMenarche_at_LastReport_Y1N0 == 1) # 0

######## SANITY CHECKS AND COUNTS #########

# sanity check: is anyone flagged for multiple things and why
youth_flags <- youth %>% rowwise() %>% mutate(num_flags = sum(PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0, na.rm = TRUE))
table(youth_flags$num_flags) # no subjects with more than 1 flag - 2150 flagged (will be more than total excluded, as some inconsistent and no baseline flags had usable data)
parent_flags <- parent %>% rowwise() %>% mutate(num_flags = sum(PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0, na.rm = TRUE))
table(parent_flags$num_flags) # no subjects with more than 1 flag - 1713 flagged (will be more than total excluded, as some inconsistent and no baseline flags had usable data)

# count final pre/post people
# no flags, no baseline and no other flags, and inconsistent with no other flags should have pre/post timepoints
counter_p <- parent %>%
  mutate(num_flags = sum(PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0, na.rm = TRUE)) %>%
  mutate(has_both = !is.na(last_pre_event) & !is.na(first_post_event)) %>%
  mutate(valid = if_else(!is.na(last_pre_event) & !is.na(first_post_event) & # Valid if it has both pre and post, no flags, or just missing baseline, or just inconsistent
                           (num_flags == 0 |
                              (NoBaselineReport_Y1N0 == 1 & num_flags == 1) |
                              (Inconsistent_Reporting_Y1N0 == 1 & num_flags == 1)), 1, 0))
table(counter_p$valid) # 4011 valid, 1649 invalid
table(counter_p$has_both) # 4011 have both pre/post, 1649 do not
table(counter_p$valid, counter_p$PreMenarche_at_LastReport_Y1N0) # 1477 not valid because premenarche at last report
table(counter_p$valid, counter_p$PostMenarche_at_Baseline_Y1N0) # 144 not valid because postmenarche at baseline
table(counter_p$valid, counter_p$Inconsistent_Reporting_Y1N0) # Inconsistent Reporters: 11 not valid, 45 valid/corrected
# manually sanity checked 11 not valid inconsistent reporters -- all reported being postmenarche at baseline
table(counter_p$valid, counter_p$NoBaselineReport_Y1N0) # 19 valid missing Baseline, 17 invalid missing baseline
# manually sanity checked 17 invalid missing baseline -- all reported being postmenarche at first report and throughout, all 19 valid people reported consistently
# 1477+144+11+17 = 1649, all invalids accounted for
# 1713 = 1649 + 45 + 19, all flags accounted for

counter_y <- youth %>%
  mutate(num_flags = sum(PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0, na.rm = TRUE)) %>%
  mutate(has_both = !is.na(last_pre_event) & !is.na(first_post_event)) %>%
  mutate(valid = if_else(!is.na(last_pre_event) & !is.na(first_post_event) & # Valid if it has both pre and post, no flags, or just missing baseline, or just inconsistent
                           (num_flags == 0 |
                              (NoBaselineReport_Y1N0 == 1 & num_flags == 1) |
                              (Inconsistent_Reporting_Y1N0 == 1 & num_flags == 1)), 1, 0))
table(counter_y$valid) # 3833 valid
table(counter_y$has_both) # 3833 have both pre/post, 1811 do not
table(counter_y$valid, counter_y$PreMenarche_at_LastReport_Y1N0) # 1553 not valid because premenarche at last report
table(counter_y$valid, counter_y$PostMenarche_at_Baseline_Y1N0) # 130 not valid because postmenarche at baseline
table(counter_y$valid, counter_y$Inconsistent_Reporting_Y1N0) # Inconsistent Reporters: 35 not valid, 47 valid
# manually sanity checked that 35 not valid inconsistents are invalid because they reported being postmenarche at baseline, 47 valids reported consistently
table(counter_y$valid, counter_y$NoBaselineReport_Y1N0) # 292 valid missing Baseline, 93 invalid missing baseline
# For the 93 invalid No Baselines, 7 were inconsistent and 86 were postmenarche throughout (see sanity check below)
# 1553+130+35+93 = 1811, all invalids are accounted for
# 2150 = 1811 + 47 + 292, all flags accounted for

### SANITY CHECKS ###
## Youth No Baseline Reports - how many of 93 were inconsistent vs. postmenarche at first report?
# isolate the 103 invalid no baseline reports
no_base_invalid_y <- counter_y %>% filter(NoBaselineReport_Y1N0 == 1 & valid == 0)
# find last pre and first post again
no_base_invalid_y$last_pre <- sapply(no_base_invalid_y$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(1))  # Find pre-menarche occurrences
  if (length(post_men_indices) > 0) {
    index <- tail(post_men_indices, 1)  # Get last occurrence
    return(index)
  } else {
    return(NA)  # No pre-menarche occurrence
  }
})
no_base_invalid_y$first_post <- sapply(no_base_invalid_y$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(4))  # Find post-menarche occurrences
  if (length(post_men_indices) > 0) {
    index <- post_men_indices[1]  # Get first occurrence
    return(index)
  } else {
    return(NA)  # No post-menarche occurrence
  }
})
# check for inconsistency and postmenarche throughout
no_base_invalid_y <- no_base_invalid_y %>%
  mutate(inconsistent = if_else(!is.na(last_pre) & last_pre > first_post, TRUE, FALSE),
         post_throughout = !is.na(last_pre) & !is.na(first_post))
table(no_base_invalid_y$inconsistent, no_base_invalid_y$post_throughout)
# 7 reported inconsistently, 86 were postmenarche throughout

# -- are all last_premenarche_session's prior to their first_postmenarche_session?
SanityCheck_y <- counter_y %>% mutate(pre_then_post = if_else(last_pre_event < first_post_event, "pre_then_post", "post_then_pre"))
table(SanityCheck_y$pre_then_post, SanityCheck_y$valid) # all valid subjects have valid timepoints
SanityCheck_p <- counter_p %>% mutate(pre_then_post = if_else(last_pre_event < first_post_event, "pre_then_post", "post_then_pre"))
table(SanityCheck_p$pre_then_post, SanityCheck_p$valid) # all valid subjects have valid timepoints

# convert menarche_reports to string so it can be written into a csv
youth <- youth %>%
  mutate(menarche_reports = paste(menarche_reports, collapse = ", "))
parent <- parent %>%
  mutate(menarche_reports = paste(menarche_reports, collapse = ", "))

#################
# IN THE BELOW CODE BLOCK
# Write to CSV
#################

write_csv(youth, paste0(derivative_data, '5.0_Youth_PrePost_Menarche_12_2_25.csv'))
write_csv(parent, paste0(derivative_data, '5.0_Parent_PrePost_Menarche_12_2_25.csv'))
