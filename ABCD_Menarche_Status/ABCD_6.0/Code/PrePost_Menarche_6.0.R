############################
# PrePost_Menarche_6.0.R
# Author: Robert Toms
# Date: 12/1/2025
############################

# Import everything
library(tidyverse)
library(lubridate)

sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/6.0/"

youth_pds <- read_tsv(paste0(sourcepath, '/Physical_Health/ph_y_pds.tsv'))
parent_pds <- read_tsv(paste0(sourcepath, '/Physical_Health/ph_p_pds.tsv'))
sex <- read_tsv(paste0(sourcepath, '/abcd_general/ab_g_stc.tsv'))

#################
# IN THE BELOW CODE BLOCK
# Clean and Prepare Data
#################

sex <- sex %>%
  select(participant_id, ab_g_stc__cohort_sex)

# keep only necessary columns
Fyouth <- youth_pds %>%
  select(participant_id, session_id, 
         ph_y_pds__f_002) # Have you begun to menstruate (started to have your period)? No = 0; Yes = 1; Decline to Answer = 777, I don't know = 999
parent <- parent_pds %>%
  select(participant_id, session_id, 
         ph_p_pds__f_002) # Has your child begun to menstruate? No = 0; Yes = 1; Decline to Answer = 777, I don't know = 999

# merge
youth <- youth %>%
  left_join(sex, by = 'participant_id')
parent <- parent %>%
  left_join(sex, by = 'participant_id')

# keep only females, as only females had menarche data collected
youth <- youth %>%
  filter(ab_g_stc__cohort_sex == 2) # 67908 -> 32365, 35543 tps dropped
parent <- parent %>%
  filter(ab_g_stc__cohort_sex == 2) # 67223 -> 31978, 35245 tps dropped
sex <- sex %>%
  filter(ab_g_stc__cohort_sex == 2) # 11868 -> 5678, 6190 subjects dropped

# remove sex column
youth <- youth %>%
  select(-ab_g_stc__cohort_sex)
parent <- parent %>%
  select(-ab_g_stc__cohort_sex)

## Count subjects with 999s or 777s
youth_999 <- youth %>%            
  filter(ph_y_pds__f_002 == 999) # 680 tps to be set to NA
youth_777 <- youth %>%            
  filter(ph_y_pds__f_002 == 777) # 1049 tps to be set to NA
parent_999 <- parent %>%
  filter(ph_p_pds__f_002 == 999) # 511 tps to be set to NA
y_999_subjects <- length(table(youth_999$participant_id)) # 680 subjects affected
y_777_subjects <- length(table(youth_777$participant_id)) # 735 subjects affected
p_999_subjects <- length(table(parent_999$participant_id)) # 386 subjects affected

# set 999s and 777s to NA (Herting et al., 2021), set 'n/a' to NA
youth <- youth %>%
  mutate(ph_y_pds__f_002 = if_else(ph_y_pds__f_002 == '999' | ph_y_pds__f_002 == '777' | ph_y_pds__f_002 == 'n/a', NA, ph_y_pds__f_002))
parent <- parent %>%
  mutate(ph_p_pds__f_002 = if_else(ph_p_pds__f_002 == '999' | ph_p_pds__f_002 == '777'| ph_p_pds__f_002 == 'n/a', NA, ph_p_pds__f_002))

# pivot wide
youth <- youth %>%
  pivot_wider(names_from = session_id, values_from = ph_y_pds__f_002)
parent <- parent %>%
  pivot_wider(names_from = session_id, values_from = ph_p_pds__f_002)

# Youth: 5678 females
# Parent: 5677 females

# who's missing and why?
parent_subs <- parent %>% pull(participant_id)
youth_subs <- youth %>% pull(participant_id)
missing_from_parent <- youth %>% filter(!(participant_id %in% parent_subs))
# sub-FZWKRBW7, reason unclear after manual inspection. only has a baseline report in youth, not enough data to determine pre/post menarche transition.

# make searchable list vector of menarche reports
youth <- youth %>%
  group_by(participant_id) %>%
  mutate(menarche_reports = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)))
parent <- parent %>%
  group_by(participant_id) %>%
  mutate(menarche_reports = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`)))

# count subjects with no data
youth_na_subs <- youth %>%
  rowwise() %>%
  filter(is.na(`ses-00A`) & is.na(`ses-01A`) &
           is.na(`ses-02A`) & is.na(`ses-03A`) & is.na(`ses-04A`) & is.na(`ses-05A`) & is.na(`ses-06A`)) # 27 subjects with no usable data
parent_na_subs <- parent %>%
  rowwise() %>%
  filter(is.na(`ses-00A`) & is.na(`ses-01A`) &
           is.na(`ses-02A`) & is.na(`ses-03A`) & is.na(`ses-04A`) & is.na(`ses-05A`) & is.na(`ses-06A`)) # 14 subjects with no usable data

# Youth: 5678 females - 27 = 5651 females with any usable data
# Parent: 5677 females - 14 = 5663 females with any usable data

# remove subjects with no data
youth <- youth %>%
  rowwise() %>%
  filter(!is.na(`ses-00A`) | !is.na(`ses-01A`) |!is.na(`ses-02A`) | !is.na(`ses-03A`) | !is.na(`ses-04A`) | !is.na(`ses-05A`) | !is.na(`ses-06A`)) # 5651 subjects remaining
parent <- parent %>%
  rowwise() %>%
  filter(!is.na(`ses-00A`) | !is.na(`ses-01A`) |!is.na(`ses-02A`) | !is.na(`ses-03A`) | !is.na(`ses-04A`) | !is.na(`ses-05A`) | !is.na(`ses-06A`)) # 5663 subjects remaining

#################
# IN THE BELOW CODE BLOCK
# Find Last PreMenarche / First PostMenarche Timepoints
#################

## Find Last Premenarche Timepoint
# Youth
youth$last_pre_index <- sapply(youth$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(0))  # Find pre-menarche occurrences
  if (length(post_men_indices) > 0) {
    index <- tail(post_men_indices, 1)  # Get last occurrence
    return(index)
  } else {
    return(NA)  # No pre-menarche occurrence
  }
})

# Parent
parent$last_pre_index <- sapply(parent$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(0))  # Find pre-menarche occurrences
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
  post_men_indices <- which(i %in% c(1))  # Find post-menarche occurrences
  if (length(post_men_indices) > 0) {
    index <- post_men_indices[1]  # Get first occurrence
    return(index)
  } else {
    return(NA)  # No post-menarche occurrence
  }
})
# Youth
parent$first_post_index <- sapply(parent$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(1))  # Find post-menarche occurrences
  if (length(post_men_indices) > 0) {
    index <- post_men_indices[1]  # Get first occurrence
    return(index)
  } else {
    return(NA)  # No post-menarche occurrence
  }
})

# Convert indexes to timepoints by subtracting 1
youth$last_pre_session <- youth$last_pre_index - 1
parent$last_pre_session <- parent$last_pre_index - 1
youth$first_post_session <- youth$first_post_index - 1
parent$first_post_session <- parent$first_post_index - 1

#################
# IN THE BELOW CODE BLOCK
# Flag for PreMenarche at last report, Postmenarche at Baseline, Inconsistent reporting (premenarche after postmenarche), and No Baseline Report
#################

# POSTMENARCHE AT BASELINE: Flagged if baseline report is Postmenarche ("1")
# NO BASELINE REPORT: Flagged if baseline report is NA
# PREMENARCHE AT LAST REPORT: If no postmenarche reports, premenarche at all reports (including last) are assumed
# INCONSISTENT REPORTING: If premenarche status is reported after a postmenarche report (0 after a 1), this is considered Inconsistent reporting

youth <- youth %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(`ses-00A` == "1", 1, 0)) %>%
  mutate(NoBaselineReport_Y1N0 = if_else(is.na(`ses-00A`), 1, 0)) %>%
  mutate(PreMenarche_at_LastReport_Y1N0 = if_else(is.na(first_post_session), 1, 0)) %>%
  mutate(Inconsistent_Reporting_Y1N0 = if_else(!is.na(first_post_session) && !is.na(last_pre_session) && first_post_session < last_pre_session, 1, 0))

parent <- parent %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(`ses-00A` == "1", 1, 0)) %>%
  mutate(NoBaselineReport_Y1N0 = if_else(is.na(`ses-00A`), 1, 0)) %>%
  mutate(PreMenarche_at_LastReport_Y1N0 = if_else(is.na(first_post_session), 1, 0)) %>%
  mutate(Inconsistent_Reporting_Y1N0 = if_else(!is.na(first_post_session) && !is.na(last_pre_session) && first_post_session < last_pre_session, 1, 0))

# count flags
table(youth$PostMenarche_at_Baseline_Y1N0)  # 165 Postmenarche at Baseline
table(youth$NoBaselineReport_Y1N0)          # 616 Missing Baseline Report
table(youth$PreMenarche_at_LastReport_Y1N0) # 561 Premenarche at Last report
table(youth$Inconsistent_Reporting_Y1N0)    # 120 Reported Inconsistently
table(youth$Inconsistent_Reporting_Y1N0, youth$PostMenarche_at_Baseline_Y1N0) # 35 Inconsistently reported and were Postmenarche at baseline
table(youth$Inconsistent_Reporting_Y1N0, youth$NoBaselineReport_Y1N0) # 12 Inconsistently reported and are missing their Baseline report
table(youth$NoBaselineReport_Y1N0, youth$PreMenarche_at_LastReport_Y1N0) # 61 Missing Baseline and Premenarche at last report

table(parent$PostMenarche_at_Baseline_Y1N0)  # 155 Postmenarche at Baseline
table(parent$NoBaselineReport_Y1N0)          # 55 Missing Baseline Report
table(parent$PreMenarche_at_LastReport_Y1N0) # 565 Premenarche at Last Report
table(parent$Inconsistent_Reporting_Y1N0)    # 86 Reported Inconsistently
table(parent$Inconsistent_Reporting_Y1N0, parent$PostMenarche_at_Baseline_Y1N0) # 13 Inconsistently reported and were Postmenarche at baseline
table(parent$Inconsistent_Reporting_Y1N0, parent$NoBaselineReport_Y1N0) # 1 Inconsistently reported and are missing their Baseline report
table(parent$NoBaselineReport_Y1N0, parent$PreMenarche_at_LastReport_Y1N0) # 6 Missing Baseline and Premenarche at last report

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
#       Postmenarche at Baseline - Likely an error of reporting or data entry, as menarche before age 9 is highly unlikely (). Cannot determine valid pre/post transition due to unclear/errored data.
#       No Baseline Report - baseline not confirmable as pre or post and following timepoints were reported inconsistently, so cannot pinpoint valid pre/post transition 
#               Premenarche at Last Report - see above, cannot find a valid pre/post transition
#               Postmenarche at First Report - see above, cannot find a valid pre/post transition
#

#################
# IN THE BELOW CODE BLOCK
# Find adjusted premenarche timepoints for Inconsistent reporters, where possible
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
# c( 0 , 0 , 1 , 0 , 1 , 1 , 1 ) , where 0 = Pre, 1 = Post
#        ^   ^   ^        
#        a   b   c          "a" gets substituted for "c" as the last premenarche timepoint
#

## Define function that finds most recent premenarche timepoint with menarche data prior to first postmenarche report
find_premenarche <- function(menarche_reports) {
  first_postmenarche <- which(menarche_reports == 1)[1]  # Find the first occurrence of 1
  if (is.na(first_postmenarche)) return(NA) # No 1 found
  preceding_0s <- which(menarche_reports[1:(first_postmenarche - 1)] == 0) # Find all 0s before that
  if (length(preceding_0s) == 0) return(NA) # No 0s before the 1
  return(tail(preceding_0s, 1)) # Return the last one before the 1
}

# apply function to find corrected timepoints
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


# count number of those that can be (potentially) validly corrected 
table(youth$Inconsistent_Reporting_Y1N0 & youth$NoBaselineReport_Y1N0 == 0, youth$Inconsistent_Pre) # 34+21+11+5+2 = 73 to be changed/corrected
table(parent$Inconsistent_Reporting_Y1N0 & parent$NoBaselineReport_Y1N0 == 0, parent$Inconsistent_Pre) # 28+29+12+3 = 72 to be changed/corrected

# There are also inconsistent reporters with a Postmenarche at Baseline Flag. By not including this flag in the following 2 calls, 
# we allow these to replace the incorrect premenarche timepoint with an accurate NA - accurate because we cannot verify the correct premenarche timepoint.

# set Inconsistent reporter's pre sessions to corrected versions -- if also no baseline report, set last_pre_session to NA
youth <- youth %>%
  mutate(last_pre_session = if_else(Inconsistent_Reporting_Y1N0 == 1 & NoBaselineReport_Y1N0 == 0, Inconsistent_Pre, last_pre_session, missing = last_pre_session)) %>%
  mutate(last_pre_session = if_else(Inconsistent_Reporting_Y1N0 == 1 & NoBaselineReport_Y1N0 == 1, NA, last_pre_session, missing = last_pre_session))
parent <- parent %>%
  mutate(last_pre_session = if_else(Inconsistent_Reporting_Y1N0 == 1 & NoBaselineReport_Y1N0 == 0, Inconsistent_Pre, last_pre_session, missing = last_pre_session)) %>%
  mutate(last_pre_session = if_else(Inconsistent_Reporting_Y1N0 == 1 & NoBaselineReport_Y1N0 == 1, NA, last_pre_session, missing = last_pre_session))



# count subjects affected by missing premenarche and missing postmenarche events
y_no_pre <- youth %>% filter(is.na(last_pre_session))
y_no_pre_num <- length(table(y_no_pre$participant_id)) 
y_no_pre_num # 268 subjects affected
y_no_post <- youth %>% filter(is.na(first_post_session))
y_no_post_num <- length(table(y_no_post$participant_id)) 
y_no_post_num # 561 subjects affected

p_no_pre <- parent %>% filter(is.na(last_pre_session))
p_no_pre_num <- length(table(p_no_pre$participant_id)) 
p_no_pre_num # 175 subjects affected
p_no_post <- parent %>% filter(is.na(first_post_session))
p_no_post_num <- length(table(p_no_post$participant_id)) 
p_no_post_num # 565 subjects affected

#################
# IN THE BELOW CODE BLOCK
# Clean for Export, Flag Counts, & Perform Sanity Checks
#################

# keep and order useful columns
youth <- youth %>%
  select(participant_id, last_pre_session, first_post_session, 
         `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`,
         PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)
parent <- parent %>%
  select(participant_id, last_pre_session, first_post_session, 
         `ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`, `ses-06A`,
         PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0)


####### Clean Flag Counts for Reporting #######
# For clarity, each subject will only be flagged with 1 primary flag.

##### No Baseline Report and Inconsistent Reporting
# reset Inconsistent Reporting to 0 if No Baseline Report - since they are missing data and inconsistent, we can't know their true pre/post transition
sum(youth$NoBaselineReport_Y1N0 == 1 & youth$Inconsistent_Reporting_Y1N0 == 1) # 12
sum(parent$NoBaselineReport_Y1N0 == 1 & parent$Inconsistent_Reporting_Y1N0 == 1) # 1
youth <- youth %>%
  mutate(Inconsistent_Reporting_Y1N0 = if_else(NoBaselineReport_Y1N0 == 1, 0, Inconsistent_Reporting_Y1N0))
parent <- parent %>%
  mutate(Inconsistent_Reporting_Y1N0 = if_else(NoBaselineReport_Y1N0 == 1, 0, Inconsistent_Reporting_Y1N0))
sum(youth$NoBaselineReport_Y1N0 == 1 & youth$Inconsistent_Reporting_Y1N0) # 0
sum(parent$NoBaselineReport_Y1N0 == 1 & parent$Inconsistent_Reporting_Y1N0 == 1) # 0

##### Postmenarche at Baseline and Inconsistent Reporting
# Reset Postmenarche at Baseline to 0 if also inconsistent - likely an error of either reporting or data entry due to unlikely menarche before age 9, cannot determine a validated pre/post transition due to unclear/errored data.
sum(youth$PostMenarche_at_Baseline_Y1N0 == 1 & youth$Inconsistent_Reporting_Y1N0 == 1) # 35
sum(parent$PostMenarche_at_Baseline_Y1N0 == 1 & parent$Inconsistent_Reporting_Y1N0 == 1) # 13
youth <- youth %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(PostMenarche_at_Baseline_Y1N0 == 1 & Inconsistent_Reporting_Y1N0 == 1, 0, PostMenarche_at_Baseline_Y1N0))
parent <- parent %>%
  mutate(PostMenarche_at_Baseline_Y1N0 = if_else(PostMenarche_at_Baseline_Y1N0 == 1 & Inconsistent_Reporting_Y1N0 == 1, 0, PostMenarche_at_Baseline_Y1N0))
sum(youth$PostMenarche_at_Baseline_Y1N0 == 1 & youth$Inconsistent_Reporting_Y1N0 == 1) # 0
sum(parent$PostMenarche_at_Baseline_Y1N0 == 1 & parent$Inconsistent_Reporting_Y1N0 == 1) # 0

##### No Baseline Report and Premenarche at Last Report
# Reset no baseline at last report flag to 0, leave premenarche at last report flag as 1-- consistent reporting means we could have determined a pre/post menarche transition, if there had been one.
sum(youth$NoBaselineReport_Y1N0 == 1 & youth$PreMenarche_at_LastReport_Y1N0 == 1) # 61
sum(parent$NoBaselineReport_Y1N0 == 1 & parent$PreMenarche_at_LastReport_Y1N0 == 1) # 6
youth <- youth %>%
  mutate(NoBaselineReport_Y1N0 = if_else(PreMenarche_at_LastReport_Y1N0 == 1 & NoBaselineReport_Y1N0 == 1, 0, NoBaselineReport_Y1N0))
parent <- parent %>%
  mutate(NoBaselineReport_Y1N0 = if_else(PreMenarche_at_LastReport_Y1N0 == 1 & NoBaselineReport_Y1N0 == 1, 0, NoBaselineReport_Y1N0))
sum(youth$NoBaselineReport_Y1N0 == 1 & youth$PreMenarche_at_LastReport_Y1N0 == 1) # 0
sum(parent$NoBaselineReport_Y1N0 == 1 & parent$PreMenarche_at_LastReport_Y1N0 == 1) # 0

######## SANITY CHECKS AND COUNTS #########

# sanity check: is anyone flagged for multiple things and why
youth_flags <- youth %>% rowwise() %>% mutate(num_flags = sum(PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0, na.rm = TRUE))
table(youth_flags$num_flags) # no subjects with more than 1 flag - 1354 flagged (will be more than total excluded, as some inconsistent and no baseline flags had usable data)
parent_flags <- parent %>% rowwise() %>% mutate(num_flags = sum(PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0, na.rm = TRUE))
table(parent_flags$num_flags) # no subjects with more than 1 flag - 841 flagged (will be more than total excluded, as some inconsistent and no baseline flags had usable data)

### count final valid pre/post people
# no flags, no baseline and no other flags, and inconsistent with no other flags should have pre/post timepoints
counter_p <- parent %>%
  mutate(num_flags = sum(PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, Inconsistent_Reporting_Y1N0, na.rm = TRUE)) %>%
  mutate(has_both = !is.na(last_pre_session) & !is.na(first_post_session)) %>%
  mutate(valid = if_else(!is.na(last_pre_session) & !is.na(first_post_session) & # Valid if it has both pre and post, no flags, or just missing baseline, or just inconsistent
                           (num_flags == 0 |
                              (NoBaselineReport_Y1N0 == 1 & num_flags == 1) |
                              (Inconsistent_Reporting_Y1N0 == 1 & num_flags == 1)), 1, 0))
table(counter_p$valid) # 4923 valid, 740 invalid, 5663 total
table(counter_p$has_both) # 4923 have both pre/post, 740 do not
table(counter_p$valid, counter_p$PostMenarche_at_Baseline_Y1N0) # 142 not valid because postmenarche at baseline
table(counter_p$valid, counter_p$PreMenarche_at_LastReport_Y1N0) # 565 not valid because premenarche at last report
table(counter_p$valid, counter_p$Inconsistent_Reporting_Y1N0) # Inconsistent Reporters: 13 not valid, 72 valid/corrected
table(counter_p$valid, counter_p$NoBaselineReport_Y1N0) # 20 not valid because they lack baseline data and had another issue, 29 valid - manually sanity checked these. 
# For the 20 invalid No Baselines, 1 was inconsistent, 19 were postmenarche throughout
# 565+142+20+13 = 740, all invalids accounted for
# 841 = 740 + 72 + 29, all flags accounted for

counter_y <- youth %>%
  mutate(num_flags = sum(PostMenarche_at_Baseline_Y1N0, NoBaselineReport_Y1N0, PreMenarche_at_LastReport_Y1N0, na.rm = TRUE)) %>%
  mutate(has_both = !is.na(last_pre_session) & !is.na(first_post_session)) %>%
  mutate(valid = if_else(!is.na(last_pre_session) & !is.na(first_post_session) & # Valid if it has both pre and post, no flags, or just missing baseline, or just inconsistent
                           (num_flags == 0 |
                              (NoBaselineReport_Y1N0 == 1 & num_flags == 1) |
                              (Inconsistent_Reporting_Y1N0 == 1 & num_flags == 1)), 1, 0))
table(counter_y$valid) # 4822 valid, 829 invalid, 5651 total
table(counter_y$has_both) # 4822 have both pre/post, 829 do not
table(counter_y$valid, counter_y$PostMenarche_at_Baseline_Y1N0) # 130 not valid because postmenarche at baseline
table(counter_y$valid, counter_y$PreMenarche_at_LastReport_Y1N0) # 561 not valid because premenarche at last report
table(counter_y$valid, counter_y$Inconsistent_Reporting_Y1N0) # Inconsistent Reporters: 35 not valid, 73 valid/corrected
table(counter_y$valid, counter_y$NoBaselineReport_Y1N0) # 103 not valid because they lack baseline data and had another issue, 452 valid
# For the 103 invalid No Baselines, 12 were inconsistent and 91 were postmenarche throughout (see sanity check below)
# 561+130+35+103 = 829, all invalids are accounted for
# 1354 = 829 + 73 + 452, all flags accounted for

### SANITY CHECKS ###
## Youth No Baseline Reports - how many of 103 were inconsistent vs. postmenarche at first report?
# isolate the 103 invalid no baseline reports
no_base_invalid_y <- counter_y %>% filter(NoBaselineReport_Y1N0 == 1 & valid == 0)
# make menarche list vector
no_base_invalid_y <- no_base_invalid_y %>% mutate(menarche_reports = list(c(`ses-00A`, `ses-01A`, `ses-02A`, `ses-03A`, `ses-04A`, `ses-05A`,`ses-06A`)))
# find last pre and first post again
no_base_invalid_y$last_pre <- sapply(no_base_invalid_y$menarche_reports, function(i) {
    post_men_indices <- which(i %in% c(0))  # Find pre-menarche occurrences
    if (length(post_men_indices) > 0) {
      index <- tail(post_men_indices, 1)  # Get last occurrence
      return(index)
    } else {
      return(NA)  # No pre-menarche occurrence
    }
  })
no_base_invalid_y$first_post <- sapply(no_base_invalid_y$menarche_reports, function(i) {
  post_men_indices <- which(i %in% c(1))  # Find post-menarche occurrences
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
# 12 were inconsistent, 91 were postmenarche throughout

# -- how many of the baseline reports were 777s or 999s?
baseline_reports_y <- youth_pds %>% select(participant_id, session_id, ph_y_pds__f_002) %>% pivot_wider(names_from = session_id, values_from =  ph_y_pds__f_002) %>% mutate(`ses-00A` = if_else(is.na(`ses-00A`), 'n/a', `ses-00A`))
nobaseline_y <- counter_y %>% select(participant_id, NoBaselineReport_Y1N0, valid) %>% left_join(baseline_reports_y , by = 'participant_id') %>% filter(NoBaselineReport_Y1N0 == 1)
table(nobaseline_y$`ses-00A`) # of the 555, 46 were 777s, 468 were 999s, 41 were n/a

baseline_reports_p <- parent_pds %>% select(participant_id, session_id, ph_p_pds__f_002) %>% pivot_wider(names_from = session_id, values_from =  ph_p_pds__f_002) %>% mutate(`ses-00A` = if_else(is.na(`ses-00A`), 'n/a', `ses-00A`))
nobaseline_p <- counter_p %>% select(participant_id, NoBaselineReport_Y1N0, valid) %>% left_join(baseline_reports_p , by = 'participant_id') %>% filter(NoBaselineReport_Y1N0 == 1)
table(nobaseline_p$`ses-00A`) # of the 49, 36 were 999s, 13 were n/a

# -- are all last_premenarche_session's prior to their first_postmenarche_session?
SanityCheck_y <- counter_y %>% mutate(pre_then_post = if_else(last_pre_session < first_post_session, "pre_then_post", "post_then_pre"))
table(SanityCheck_y$pre_then_post, SanityCheck_y$valid) # all valid subjects have valid timepoints
SanityCheck_p <- counter_p %>% mutate(pre_then_post = if_else(last_pre_session < first_post_session, "pre_then_post", "post_then_pre"))
table(SanityCheck_p$pre_then_post, SanityCheck_p$valid) # all valid subjects have valid timepoints


#################
# IN THE BELOW CODE BLOCK
# Write to CSV
#################

write_csv(youth, paste0(derivative_data, 'Youth_PrePost_Menarche_11_21_25.csv'))
write_csv(parent, paste0(derivative_data, 'Parent_PrePost_Menarche_11_21_25.csv'))
