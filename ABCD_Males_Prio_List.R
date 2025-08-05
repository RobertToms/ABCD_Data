###################
# ABCD_Males_Prio_List.R
# Author: Robert Toms
# Date: July 23, 2025
###################

# The purpose of this script is to build a working list of male subjects in the ABCD Study, organized in priority by PLEs, with usable .tgz scans.
# IT SHOULD BE UPDATED WHEN YEAR 3 & 4 QC DATA IS AVAILABLE

# import libraries
library(tidyverse)

# import data
all_tgzs <- read_csv("/path/to/all_tgz.txt")
qc_data <- read_csv("/path/to/mri_y_qc_incl.csv")
ples <- read_csv("/path/to/mh_y_pps.csv")
demo <- read_csv("/path/to/abcd_p_demo.csv")


#########################
# IN THE BELOW CODE BLOCK
# Use Demographics data to build list of Male Subjects
#########################

demo_males <- demo %>%
  # keep only relevant columns- ID, visit, sex
  select(src_subject_id, eventname, demo_sex_v2) %>%
  # Fill empty sex columns for each subject
  group_by(src_subject_id) %>%
  fill(demo_sex_v2, .direction = "down") %>%
  # remove females
  filter(demo_sex_v2 != 2)


#########################
# IN THE BELOW CODE BLOCK
# Add in PLEs to demographic data
# Sum each subjects PLE scores for priority ranking
# Add processing order column
#########################

# keep only relevent columns - ID, visit, PLE score
ples <- ples %>%
  select(src_subject_id, eventname, pps_y_ss_severity_score)

# Merge PLEs into demographic data
demo_ples <- demo_males %>%
  left_join(ples, by = c("src_subject_id", "eventname"))

# calculate PLE sums for priority ranking
demo_ples <- demo_ples %>%
  group_by(src_subject_id) %>%
  mutate(psychosis_sum = sum(pps_y_ss_severity_score)) %>% 
  ungroup()

# keep only useful columns, sort into priority order
prio <- demo_ples %>%
  select(src_subject_id, psychosis_sum) %>%
  unique() %>%
  arrange(-psychosis_sum)

# Add column for priority number
prio <- prio %>%
  mutate(processing_order = row_number())


#########################
# IN THE BELOW CODE BLOCK
# Use QC data and TGZ list to find which timepoints have usable scans for each subject
#########################

# keep only necessary columns for QC data, format ID and eventname for TGZ comparison
qc_data <- qc_data %>%
  select(src_subject_id, eventname, imgincl_t1w_include) %>%
  mutate(src_subject_id = str_replace(src_subject_id, "_", "")) %>%
  mutate(eventname = case_when(
    eventname == "baseline_year_1_arm_1" ~ "baselineYear1Arm1",
    eventname == "2_year_follow_up_y_arm_1" ~ "2YearFollowUpYArm1"))

# extract IDs and eventnames from TGZs
all_tgzs <- all_tgzs %>%
  separate(Tgz, into = c("src_subject_id", "eventname"), sep = "_", extra = "merge") %>%
  separate(eventname, into = c("eventname", "tgz_string"), sep = "_", extra = "merge")

# merge qc data into tgz list
qc_tgz <- all_tgzs %>%
  left_join(qc_data, by = c("src_subject_id", "eventname"))
# ^^^^^^ those with a 1 QC value will be sorted later by NORM status, then latest timepoint, per ABCD guidance
# https://docs.abcdstudy.org/latest/documentation/imaging/

# make list of NA QC's to determine
NA_qcs <- qc_tgz %>%
  filter(is.na(imgincl_t1w_include))

#########################
# IN THE BELOW CODE BLOCK
# Prepare individual scan QC data for tgz comparison
#########################
# Specific QC guidance:
# https://docs.abcdstudy.org/latest/documentation/imaging/type_qc.html

# import scan-specific QC data
specific_qc <- read_csv("/path/to/mri_y_qc_raw_smr_t1.csv")

# keep only necessary columns
specific_qc <- specific_qc %>%
  select(src_subject_id, eventname,
         iqc_t1_1_studydate, iqc_t1_1_seriestime, iqc_t1_1_qc_score, iqc_t1_1_pc_score,
         iqc_t1_2_studydate, iqc_t1_2_seriestime, iqc_t1_2_qc_score, iqc_t1_2_pc_score,
         iqc_t1_3_studydate, iqc_t1_3_seriestime, iqc_t1_3_qc_score, iqc_t1_3_pc_score,
         iqc_t1_total_ser,   iqc_t1_total_passqc, iqc_t1_good_ser,   iqc_t1_ok_ser,     iqc_t1_ser_qcs, iqc_t1_other_qc)

# format IDs and eventnames for tgz comparison
specific_qc <- specific_qc %>%
  mutate(src_subject_id = str_replace(src_subject_id, "_", "")) %>%
  mutate(eventname = case_when(
    eventname == "baseline_year_1_arm_1" ~ "baselineYear1Arm1",
    eventname == "2_year_follow_up_y_arm_1" ~ "2YearFollowUpYArm1"))

# format and combine studydate and seriestime for tgz comparison
specific_qc <- specific_qc %>%
  # format columns
  mutate(across(c(iqc_t1_1_studydate, iqc_t1_2_studydate, iqc_t1_3_studydate, 
                  iqc_t1_1_seriestime, iqc_t1_2_seriestime, iqc_t1_3_seriestime),
                as.character)) %>%
  mutate(iqc_t1_1_studydate = str_replace_all(iqc_t1_1_studydate, "-", "")) %>%
  mutate(iqc_t1_2_studydate = str_replace_all(iqc_t1_2_studydate, "-", "")) %>%
  mutate(iqc_t1_3_studydate = str_replace_all(iqc_t1_3_studydate, "-", "")) %>%
  # combine into timestamp
  mutate(timepoint1 = paste(iqc_t1_1_studydate, iqc_t1_1_seriestime, sep = "")) %>%
  mutate(timepoint2 = paste(iqc_t1_2_studydate, iqc_t1_2_seriestime, sep = "")) %>%
  mutate(timepoint3 = paste(iqc_t1_3_studydate, iqc_t1_3_seriestime, sep = ""))

# organize for clarity
specific_qc <- specific_qc %>%
  select(src_subject_id, eventname, timepoint1,
         iqc_t1_1_qc_score, # Quality Check Score: 1 is pass, 0 is fail
         iqc_t1_1_pc_score, # Processing Compliance Score: 1 is pass, 0 is fail
         timepoint2, iqc_t1_2_qc_score, iqc_t1_2_pc_score,
         timepoint3, iqc_t1_3_qc_score, iqc_t1_3_pc_score,
         iqc_t1_total_ser, # Total number received
         iqc_t1_total_passqc, # Total passed visual QC
         iqc_t1_good_ser, # Total Passed QC and PC
         iqc_t1_ok_ser, # Total Passed QC (PC ignored)
         iqc_t1_ser_qcs, # Number with QC issues
         iqc_t1_other_qc) # Number with other QC issues


#########################
# IN THE BELOW CODE BLOCK
# Match QC data to NA QCs
#########################

# Expand NA QC TGZs
NA_qcs <- NA_qcs %>%
  separate(tgz_string, into = c("scantype", "extension"), sep = "_", extra = "merge") %>%
  #isolate the NORM status
  extract(scantype, into = c("ABCD", "Norm?"), 
          regex = "^(.+?)(-NORM)?$", remove = FALSE) %>%
  #isolate the .tgz
  separate(extension, into = c("timepoint", "tgz"), sep = "\\.") %>%
  # remove scantype
  select(-scantype)

# Combine in QC data
NA_merged <- NA_qcs %>%
  left_join(specific_qc, by = c("src_subject_id", "eventname"))

# compute timestamp differences
NA_merged <- NA_merged %>%
  mutate(across(c(timepoint, timepoint1, timepoint2, timepoint3),
                as.numeric)) %>%
  mutate(tp1 = (timepoint - timepoint1),
         tp2 = (timepoint - timepoint2),
         tp3 = (timepoint - timepoint3)) %>%
  mutate(across(c(timepoint, timepoint1, timepoint2, timepoint3),
                as.character))

# set qc column to 1 if applicable
NA_merged <- NA_merged %>%
  mutate(imgincl_t1w_include = if_else(tp1 >= -2, iqc_t1_1_qc_score, NA))

# remove any baseline or year 2 without QC data-- year 3 and 4 will be treated as usable for now.
# Aaron and Dr. Damme previously assumed Year3 and Year4 scans were usable when computing usable scans.
NA_merged <- NA_merged %>%
  filter((eventname == "baselineYear1Arm1" & imgincl_t1w_include == 1) |
         (eventname == "2YearFollowUpYArm1" & imgincl_t1w_include == 1) |
         (eventname == "3YearFollowUpYArm1" | eventname == "4YearFollowUpYArm1" | eventname == "6YearFollowUpYArm1"))
           
# Set year 3, 4, 6 to "2" as a placeholder until QC data is available
NA_merged <- NA_merged %>%
  mutate(imgincl_t1w_include = if_else((eventname == "3YearFollowUpYArm1" | eventname == "4YearFollowUpYArm1" | eventname == "6YearFollowUpYArm1"),
                                        2, imgincl_t1w_include))

#########################
# IN THE BELOW CODE BLOCK
# Combine NA_merged and qc_tgz into all tgz scans with qc data
#########################

# expand out qc_tgz
qc_tgz <- qc_tgz %>%
  separate(tgz_string, into = c("scantype", "extension"), sep = "_", extra = "merge") %>%
  #isolate the NORM status
  extract(scantype, into = c("ABCD", "Norm?"), 
          regex = "^(.+?)(-NORM)?$", remove = FALSE) %>%
  #isolate the .tgz
  separate(extension, into = c("timepoint", "tgz"), sep = "\\.") %>%
  # remove scantype
  select(-scantype)

# keep only matching columns in NA_merged
NA_merged <- NA_merged %>%
  select(src_subject_id, eventname, ABCD, `Norm?`, timepoint, tgz, imgincl_t1w_include)

# remove NAs from qc_tgz, merge in filled in NAs
all_qc <- qc_tgz %>%
  filter(!is.na(imgincl_t1w_include)) %>%
  rbind(NA_merged) %>%
  arrange(src_subject_id)

#########################
# IN THE BELOW CODE BLOCK
# Sort/filter by NORM status, then latest timepoint, per ABCD guidance
# https://docs.abcdstudy.org/latest/documentation/imaging/
#########################

all_qc <- all_qc %>%
  # remove unusable scans
  filter(imgincl_t1w_include != 0) %>%
  group_by(src_subject_id, eventname) %>%
  # count scans and norms per subject/timepoint
  mutate(scan_count = sum(ABCD == "ABCD-T1")) %>%
  mutate(norm_count = sum(`Norm?` == "-NORM"))

####### ALL REMAINING SCANS ARE THEORETICALLY USABLE ######
######## BELOW IS A TABLE OF HOW MANY SCANS EXIST PER SUBJECT/EVENTNAME
# table(all_qc$scan_count)
#  1     2       3     4     5     6     7     8    10    11 
# 7134 24104   429  3896   445   318    14   104    20    11 

######## INCLUDE ALL "1" SCANS #########
to_use <- all_qc %>%
  filter(scan_count == 1)
all_qc <- all_qc %>%
  filter(scan_count != 1)

### Keep only latest timepoint for non-NORM and NORM scans, for each subject/eventname
# sort
nonNorms <- all_qc %>%
  filter(`Norm?` != "-NORM")
norms <- all_qc %>%
  filter(`Norm?` == "-NORM")

# find latest timepoint
nonNorms <- nonNorms %>%
  group_by(src_subject_id, eventname) %>%
  mutate(latest_tp = max(timepoint))
norms <- norms %>%
  group_by(src_subject_id, eventname) %>%
  mutate(latest_tp = max(timepoint))

# keep only latest timepoint
nonNorms <- nonNorms %>%
  filter(timepoint == latest_tp)
norms <- norms %>%
  filter(timepoint == latest_tp)

# recombine
two_plus_scans <- nonNorms %>%
  rbind(norms) %>%
  arrange(src_subject_id)

# Recount total scans and NORMs
two_plus_scans <- two_plus_scans %>%
  unique() %>%
  group_by(src_subject_id, eventname) %>%
  mutate(scan_count = sum(ABCD == "ABCD-T1")) %>%
  mutate(norm_count = sum(`Norm?` == "-NORM"))

# SANITY CHECK - assess norm/non norm situation
two_plus_scans <- two_plus_scans %>%
  mutate(total_vs_norms = paste0(as.character(scan_count), "_", as.character(norm_count)))
table(two_plus_scans$total_vs_norms)

#   1_0   1_1   2_1 
#   592    6   25462 

### Remove non-NORM scans from 2_1 group, to prioritize only the NORM, per ABCD guidance
# split off
two_one <- two_plus_scans %>%
  filter(total_vs_norms == "2_1")

# remove from two_plus_scans, clean, and add to to_use
two_plus_scans <- two_plus_scans %>%
  filter(total_vs_norms != "2_1") %>%
  select(-latest_tp, -total_vs_norms)

to_use <- to_use %>%
  rbind(two_plus_scans)

# remove non-NORMs from two_one (2_1) pairs, add to to_use
two_one <- two_one %>%
  filter(`Norm?` == "-NORM") %>%
  select(-latest_tp, -total_vs_norms)

to_use <- to_use %>%
  rbind(two_one)

# to_use NOW CONTAINS ALL TGZs THAT SHOULD BE USED FOR PROCESSING

#########################
# IN THE BELOW CODE BLOCK
# Combine prio list and usable TGZ list 
#########################

# remove _ from prio subject ids
prio <- prio %>%
  mutate(src_subject_id = str_replace(src_subject_id, "_", "")) %>%
  filter(!is.na(psychosis_sum)) %>%
  mutate(Who_Processed = "",
         Baseline = "",
         Year2 = "",
         Year3 = "",
         Year4 = "",
         Year6 = "") 

# recombine tgz strings
to_use <- to_use %>%
  unite("Norm_status", ABCD, 'Norm?', sep = "", remove = FALSE) %>%
  unite("filename", src_subject_id, eventname, Norm_status, timepoint, sep = "_", remove = FALSE) %>%
  unite("Full_Filename", filename, tgz, sep = ".", remove = FALSE) %>%
  select(src_subject_id,
         eventname,
         Full_Filename)

# merge
prio <- prio %>%
  left_join(to_use, by = "src_subject_id")

# count scan number
prio <- prio %>%
  group_by(src_subject_id) %>%
  mutate(number_of_scans = sum(Who_Processed == ""))

# write tgz filenames into appropriate columns
prio <- prio %>%
  mutate(Baseline = if_else(eventname == "baselineYear1Arm1", Full_Filename, NA)) %>%
  mutate(Year2 = if_else(eventname == "2YearFollowUpYArm1", Full_Filename, NA)) %>%
  mutate(Year3 = if_else(eventname == "3YearFollowUpYArm1", Full_Filename, NA)) %>%
  mutate(Year4 = if_else(eventname == "4YearFollowUpYArm1", Full_Filename, NA)) %>%
  mutate(Year6 = if_else(eventname == "6YearFollowUpYArm1", Full_Filename, NA))

# fill empty scan columns as appropriate, keep only one row per subject
prio <- prio %>%
  group_by(src_subject_id) %>%
  fill(Baseline, .direction = "downup") %>%
  fill(Year2, .direction = "downup" ) %>%
  fill(Year3, .direction = "downup" ) %>%
  fill(Year4, .direction = "downup" ) %>%
  fill(Year6, .direction = "downup" ) %>%
  select(-eventname, -Full_Filename) %>%
  unique()

# Clean up for CSV conversion
prio <- prio %>%
  select(src_subject_id, psychosis_sum, processing_order, number_of_scans, Who_Processed, everything()) %>%
  mutate(across(c(Baseline, Year2, Year3, Year4, Year6), ~ if_else(is.na(.), "", as.character(.))))

# Split into longitudinal and 1 timepoint
long <- prio %>%
  filter(number_of_scans >= 2)

single <- prio %>%
  filter(number_of_scans == 1)

prio <- prio %>%
  select(-Who_Processed)

#########################
# IN THE BELOW CODE BLOCK
# Write to CSV
#########################

# for OneDrive
write_csv(prio, "/path/to/ABCD_Males_Full_Priority_List.csv")
write_csv(long, "/path/to/ABCD_Males_Long_Prio_List.csv")
write_csv(single, "/path/to/ABCD_Males_Single_Prio_List.csv")
