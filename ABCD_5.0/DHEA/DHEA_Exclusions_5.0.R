###############################
# DHEA_Exclusions_5.0.R
# Author: Robert Toms
# Date: 10/7/2025
###############################

# import libraries
library(dplyr)
library(readr)
library(hms)

# define paths
sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"

# read in csv files
hormones <- read_csv(paste0(sourcepath, 'physical-health/ph_y_sal_horm.csv'))
birth_control <- read_csv(paste0(sourcepath, 'physical-health/ph_y_pds.csv')) 
sex <- read_csv(paste0(sourcepath, '/abcd-general/abcd_p_demo.csv'))


###############
# IN THE BELOW CODE BLOCK
# Merge Hormone and Demographic Data
###############

# keep only relevant columns in Demographics, AllSalivaHormones, and BirthControl
sex <- sex %>%
  select(src_subject_id, demo_sex_v2) %>%
  filter(!is.na(demo_sex_v2))

hormones <- hormones %>%
  select(src_subject_id, 
         eventname, 
         hormone_sal_sex, 		# Salivary Sex; 1=Pink/Purple (female) ; 2=Blue (male) ; 3=Participant unable to complete ; 4=Participant/Parent refused ; 6=Institution not authorized to collect samples due to COVID ; 7=Staff discomfort due to COVID ; 8=Exceed 15min of < 6ft distance from youth (COVID) ; 9=Not approved by ABCD for collection ; 5=Not collected (other)
         hormone_sal_wake_y, 		# What time did you wake up today?
         hormone_sal_caff_y, 		# Have you had any caffeine in the last 12 hours?
         hormone_sal_caff_mg_y,		# If yes, how much caffeine (in mg)
         hormone_sal_active,		# In the last 12 hours, did you exercise vigorously (sweating, breathing hard) for at least 20 minutes?
         hormone_sal_active_minutes_y,	# For how long did you exercise?
         hormone_sal_start_y,		# Time saliva sample collection started
         hormone_sal_end_y,		# Time saliva sample colleciton finished
         hormone_sal_freezer_y,		# Time saliva sample was moved to the freezer
         hormon_sal_notes_y___1,	# No concerns about sample
         hormon_sal_notes_y___2,	# Contaminated sample?
         hormon_sal_notes_y___3,	# Discolored sample?
         hormon_sal_notes_y___4,	# Excessive bubbles in sample?
         hormon_sal_notes_y___5,	# Very little saliva provided?
         hormon_sal_notes_y___6,	# Other sample concern is present
         hormone_scr_dhea_mean,		# Salimetrics hormone test DHEA mean of measures (pg/mL)
         hormone_scr_dhea_rep1,		# Salimetrics hormone test DHEA repetition 1 (pg/mL)
         hormone_scr_dhea_rep1_ll,	# Salimetrics hormone test DHEA below lower limit of sensitivity (repetition 1)
         hormone_scr_dhea_rep1_qns,	# Salimetrics hormone test DHEA quantity not sufficient (repetition 1)
         hormone_scr_dhea_rep1_nd,	# Salimetrics hormone test DHEA none detected (repetition 1)
         hormone_scr_dhea_rep2,		# Salimetrics hormone test DHEA repetition 2 (pg/mL)
         hormone_scr_dhea_rep2_ll,	# Salimetrics hormone test DHEA below lower limit of sensitivity (repetition 2)
         hormone_scr_dhea_rep2_qns,	# Salimetrics hormone test DHEA quantity not sufficient (repetition 2)
         hormone_scr_dhea_rep2_nd)	# Salimetrics hormone test DHEA none detected (repetition 2)

# Keep only relevant birth control information
birth_control <- birth_control %>%
  select(src_subject_id, 
         eventname,
         menstrualcycle4_y) # Are you currently using hormonal birth control (eg. the pill, hormone patch, hormone injection)?

# rename remaining columns for readability, for both Demographics and AllSalivaHormones
sex <- sex %>%
  rename(MasterSex = demo_sex_v2)

hormones <- hormones %>%
  rename(SalivarySex = hormone_sal_sex,
         WakeTime = hormone_sal_wake_y,
         "HadCaffeine?" = hormone_sal_caff_y,
         "CaffAmount(mg)" = hormone_sal_caff_mg_y,
         "Exercised?" = hormone_sal_active,
         "ExerciseLength(min)" = hormone_sal_active_minutes_y,
         StartTime = hormone_sal_start_y,
         EndTime = hormone_sal_end_y,
         FreezeTime = hormone_sal_freezer_y,
         "No_Sal_Problems?" = hormon_sal_notes_y___1,
         "Contaminated_Sal?" = hormon_sal_notes_y___2,
         "Discolored_Sal?" = hormon_sal_notes_y___3,
         "Bubbles_Sal?" = hormon_sal_notes_y___4,
         "NotEnough_Sal?" = hormon_sal_notes_y___5,
         "OtherComplication_Sal" = hormon_sal_notes_y___6,
         DHEAmean = hormone_scr_dhea_mean,
         DHEA_Rep1 = hormone_scr_dhea_rep1,
         DHEA_Rep1_LL = hormone_scr_dhea_rep1_ll,
         DHEA_Rep1_QNS = hormone_scr_dhea_rep1_qns,
         DHEA_Rep1_ND = hormone_scr_dhea_rep1_nd,
         DHEA_Rep2 = hormone_scr_dhea_rep2,
         DHEA_Rep2_LL = hormone_scr_dhea_rep2_ll,
         DHEA_Rep2_QNS = hormone_scr_dhea_rep2_qns,
         DHEA_Rep2_ND = hormone_scr_dhea_rep2_nd)

birth_control <- birth_control %>%
  rename(BirthControlStatus = menstrualcycle4_y)

## Add Master Sex from Demographics to AllSalivaHormones
# Join the dataframes
hormones <- hormones %>%
  left_join(sex, by = "src_subject_id")

# Merge BirthControl into AllSalivaHormones
hormones <- hormones %>%
  left_join(birth_control, by = c("src_subject_id", "eventname"))

# Recode 777s and 999s in BirthControlStatus to 0s (Chaku & Barry, 2023)
# Are you currently using hormonal birth control? 1 = Yes; 0 = No; 999 = Don't know; 777 = Refuse to answer
hormones <- hormones %>%
  mutate(BirthControlStatus = as.numeric(BirthControlStatus)) %>%
  mutate(BirthControlStatus = if_else(BirthControlStatus == 777 | BirthControlStatus == 999, 0, BirthControlStatus))

# Calculate number of timepoints with at least one DHEA sample
NumTimepointsWithData <- hormones %>%
  group_by(src_subject_id) %>%
  summarise(TimepointsWithDHEAData = sum(!is.na(DHEAmean)))

# merge in column listing how many Timepoints with Data a given subject has
hormones <- hormones %>%
  left_join(NumTimepointsWithData, by = "src_subject_id")

# Add column with number of samples (reps) per visit (should be 0, 1, or 2)
hormones <- hormones %>%
  mutate(SamplesPerVisit = rowSums(!is.na(cbind(DHEA_Rep1, DHEA_Rep2))))

############################
# IN THE BELOW CODE BLOCK
# Make new columns with Time Differentials-- key to exclusion criteria from (Uban et al., 2018)
# Derive Absolute Values of Time Differentials (PI Suggestion)
# Reorder Columns for clarity and readability
############################

# Convert times to hms
hormones <- hormones %>%
  mutate(WakeTime = as_hms(WakeTime),
         StartTime = as_hms(StartTime),
         EndTime = as_hms(EndTime),
         FreezeTime = as_hms(FreezeTime))

# Make new columns for WakeTime -> StartTime, StartTime -> EndTime, EndTime -> FreezeTime
# In the current data type, these times are represented in the GUI as HH:MM:SS, but are stored in seconds counting up from midnight (1am = 3600, etc.). These will be stored as differences of seconds.
hormones <- hormones %>%
  mutate(WakeToStart = StartTime - WakeTime) %>%
  mutate(StartToEnd = EndTime - StartTime) %>%
  mutate(EndToFreeze = FreezeTime - EndTime)

#Make Absolute Value columns for WakeToStart, StartToEnd, EndToFreeze
hormones <- hormones %>%
  mutate(Abs_WakeToStart = abs(WakeToStart)) %>%
  mutate(Abs_StartToEnd = abs(StartToEnd)) %>%
  mutate(Abs_EndToFreeze = abs(EndToFreeze))

# Reorder Columns for readability and inter-relevance
hormones <- hormones %>%
  select(src_subject_id, eventname,
         TimepointsWithDHEAData, MasterSex, SalivarySex,
         BirthControlStatus,
         "HadCaffeine?",
         "CaffAmount(mg)",
         "Exercised?",
         "ExerciseLength(min)",
         WakeTime,
         StartTime,
         EndTime,
         FreezeTime,
         WakeToStart,
         StartToEnd,
         EndToFreeze,
         Abs_WakeToStart,
         Abs_StartToEnd,
         Abs_EndToFreeze,
         "No_Sal_Problems?",
         "Contaminated_Sal?",
         "Discolored_Sal?",
         "Bubbles_Sal?",
         "NotEnough_Sal?",
         "OtherComplication_Sal",
         DHEAmean,
         SamplesPerVisit,
         DHEA_Rep1,
         DHEA_Rep1_LL,
         DHEA_Rep1_QNS,
         DHEA_Rep1_ND,
         DHEA_Rep2,
         DHEA_Rep2_LL,
         DHEA_Rep2_QNS,
         DHEA_Rep2_ND)

############################
# IN THE BELOW CODE BLOCK
# Creating Individual Columns for Caution/Exclusion Flags
############################

# Create columns for Exclusion/Caution Flags
hormones <- hormones %>%
  mutate("DHEA1" = 0) %>%              # (DHEA1) = DHEA_Rep1 out of range/no data (Herting et al., 2021)
  mutate("DHEA2" = 0) %>%              # (DHEA2) = DHEA_Rep2 out of range/no data (Herting et al., 2021)
  mutate("Mas!=Sal" = 0) %>%        # (Mas!=Sal) = Master Sex does not match Salivary Sex (Herting et al., 2021)
  mutate("QualCon" = 0) %>%         # (QualCon) = Quality Concerns(No_Sal_Problems != 1) & ((DHEA1) | (DHEA2)) Chaku & Barry, 2023) 
  mutate("NoDHEA_Data" = 0) %>%      # (NoDHEA_Data) = No data available (Chaku & Barry, 2023) 
  mutate("BirthConNA" = 0) %>%      # (BirthConNA) = missing Birth Control data (Chaku & Barry, 2023) 
  mutate("CaffNA" = 0) %>%          # (CaffNA) = missing Caffeine data (Chaku & Barry, 2023) 
  mutate("ExerciseNA" = 0) %>%      # (ExerciseNA) = missing Exercise data (Chaku & Barry, 2023) 
  mutate("S_b4_W" = 0) %>%          # (S_b4_W) = Start Time reported to be before Wake Time (Chaku & Barry, 2023) 
  mutate("E_b4_S" = 0) %>%          # (E_b4_S) = End Time reported to be before Start Time (Chaku & Barry, 2023) 
  mutate("F_b4_E" = 0) %>%          # (F_b4_E) = Freeze Time reported to be before End Time (Chaku & Barry, 2023) 
  mutate(S2Egr30 = 0) %>%           # (S2Egr30) = StartToEnd > 30 minutes (Uban et al., 2018)
  mutate(WakeAfter3 = 0) %>%        # (WakeAfter3) = WakeTime after 3pm (Uban et al., 2018)
  mutate(FrozeTooSlow = 0)          # (FrozeTooSlow) = EndToFreeze > 30 minutes (Salimetrics)


##################
# IN THE BELOW CODE BLOCK
# Quality checking and Generating a single hormone metric (Herting et al., 2021)
#   (Chaku & Barry, 2023) note in their paper that they followed the best practices 
#   of (Herting et al., 2021) for the first part of inclusion/exclusion—quality 
#   checking and generating a single hormone metric per participant—so Herting et al.’s 
#   original methodology was followed for that section.
# link to Herting et al. paper: https://www.frontiersin.org/journals/endocrinology/articles/10.3389/fendo.2020.549928/full
# Steps 1-6 referred to below from Figure 1
##################

# QUALITY CHECK AND GENERATE A SINGLE HORMONE METRIC
# STEP 1: Check that Master Sex matches Salivary Sex
# Convert MasterSex (1=M, 2=F) to match SalivarySex (1=F, 2=M)
hormones <- hormones %>%
  mutate(MasterSex = case_when(
    MasterSex == 1 ~ 2,
    MasterSex == 2 ~ 1,
    TRUE ~ MasterSex
  ))
# Add Exclusion Flag (Mas!=Sal) -- (Herting et al., 2021)
hormones$`Mas!=Sal` <- if_else(hormones$MasterSex == hormones$SalivarySex, 0, 1)

# STEP 2: Was the Hormone sample collected? (Flag if value is NA)
# STEP 3: Has the sample been processed for Hormone levels? (if there were values, it was assumed that the sample has been processed)
hormones <- hormones %>%
  mutate(DHEA1 = if_else(!is.na(DHEA_Rep1), 0, 1),
         DHEA2 = if_else(!is.na(DHEA_Rep2), 0, 1))
# count number flagged
table(hormones$DHEA1) # 3733
table(hormones$DHEA2) # 3853
hormones <- hormones %>%
  mutate(step_2_3 = if_else(DHEA1 == 1 | DHEA2 == 1, 1, 0))
table(hormones$step_2_3) # 3903

# STEP 4: Do replicate values fall above higher limits of each hormone assay? (Samples > 1000 pg/mL were flagged)
# STEP 5: Is the replicate too low for detection? (Samples < 10.2 pg/mL were flagged)
# Add Flag for 10.2pg/mL < DHEA < 1000 pg/mL | DHEA == NA samples -- (Herting et al., 2021)

# count number of outliers
hormones <- hormones %>%
  mutate(DHEA_Rep1 = as.numeric(DHEA_Rep1),
         DHEA_Rep2 = as.numeric(DHEA_Rep2)) %>%
  mutate(below_detect = if_else(DHEA_Rep1 < 10.2 | DHEA_Rep2 < 10.2, 1, 0)) %>%
  mutate(above_detect = if_else(DHEA_Rep1 > 1000 | DHEA_Rep2 > 1000, 1, 0))
table(hormones$below_detect) # 1423
table(hormones$above_detect) # 7
hormones$step_4_5 <- if_else(hormones$below_detect == 1 | hormones$above_detect == 1, 1, 0)
table(hormones$step_4_5) # 1430

# STEPS 4 & 5 (continued): If outside Salimetrics test sensitivity limits, set to 0 (Herting et al., 2021) or NA (Chaku & Barry, 2023)
hormones$DHEA1 <- if_else(hormones$DHEA_Rep1 < 10.2 | 
                            hormones$DHEA_Rep1 > 1000 | 
                            is.na(hormones$DHEA_Rep1), 
                          hormones$DHEA1 <- 1, 0)
hormones$DHEA2 <- if_else(hormones$DHEA_Rep2 < 10.2 | 
                            hormones$DHEA_Rep2 > 1000 | 
                            is.na(hormones$DHEA_Rep2), 
                          hormones$DHEA2 <- 1, 0)


# STEP 6: Obtain a single value for each hormone type.
# Timepoints with 2 values were averaged, timepoints with 1 value used the single value.
# If DHEA is Outside of lower/upper limits, set to NA (Chaku & Barry, 2023)
hormones$DHEA_Rep1 <- if_else(hormones$DHEA1 == 1, NA, hormones$DHEA_Rep1)
hormones$DHEA_Rep2 <- if_else(hormones$DHEA2 == 1, NA, hormones$DHEA_Rep2)

# Recompute SamplesPerVisit, (PI Suggestion)
hormones <- hormones %>%
  mutate(SamplesPerVisit = rowSums(!is.na(cbind(DHEA_Rep1, DHEA_Rep2))))

# Recompute DHEAmeans for Single Hormone Value
hormones <- hormones %>%
  mutate(DHEAmean = rowMeans(cbind(DHEA_Rep1, DHEA_Rep2), na.rm = TRUE),
         DHEAmean = coalesce(DHEAmean, DHEA_Rep1, DHEA_Rep2))


#######################
# IN THE BELOW CODE BLOCK
# In following the best practices of Chaku & Barry, further exclusions were made beyond the checks made by Herting et al.
# link to Chaku & Barry, 2023: https://onlinelibrary.wiley.com/doi/full/10.1002/icd.2415
# Figure 1, page 9
########################

# Exclude Mismatches MasterSex and Salivary Sex
hormones$`Excluded?` <- if_else(hormones$`Mas!=Sal` == 1, 1, 0)
table(hormones$`Excluded?`) # n = 2350 excluded

### Further Exclusion/Caution Flags (Chaku & Barry, 2023)
## Quality Concerns (QualCon) 
# If sample quality was in doubt (bubbles, discoloration, etc., No Saliva Problems column != 1) \
#     AND there was at least 1 missing or unusable sample (DHEA1 or DHEA2 Flag), QualCon flag was changed from 0 to 1
hormones$QualCon <- if_else((hormones$`No_Sal_Problems?` != 1 & hormones$DHEA1 == 1) | (hormones$`No_Sal_Problems?` != 1 & hormones$DHEA2 == 1), 1, 0)
hormones$QualCon <- if_else(is.na(hormones$QualCon), 0, hormones$QualCon) # set NA values to 0, to maintain counts
hormones$`Excluded?` <- if_else(hormones$QualCon == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 2822, 472 excluded

# *** Upon PI Suggestion, keeping all valid-test DHEA outliers ***

## No DHEA Data (NoDHEA_Data)
# If the DHEAmean was NA (meaning there were zero samples in a timepoint), the NoDHEA_Data flag was changed from 0 to 1. 
hormones$NoDHEA_Data <- if_else(is.na(hormones$DHEAmean), 1, 0)
hormones$`Excluded?` <- if_else(hormones$NoDHEA_Data == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 4800, 1978 excluded

## Missing Birth Control data (females) (BirthConNA) 
####### NOTE: When using BirthConNA, only including those with birth control data and endorsing not using birth control, 
# our sample size dropped from thousands to a few dozen. We created this variable flag in the lines below, but in 
# flagging for exclusion due to birth control use (line 401) we chose to assume that no birth control data meant 
# birth control was not being used, and only excluded those who had birth control data and affirmed birth control use. 
# Only keeping timepoint with birth control data would exclude all pre-menarche timepoints, for young women who have 
# no reason to consider birth control yet. ##########
# If missing Birth Control data (BirthControlStatus=NA), the BirthConNA flag was changed from 0 to 1
hormones$BirthConNA <- if_else(is.na(hormones$BirthControlStatus), 1, 0)
hormones$BirthControlStatus <- if_else(is.na(hormones$BirthControlStatus), 0, hormones$BirthControlStatus)# set NA Birth Control values to 0 
hormones$`Excluded?` <- if_else(hormones$BirthControlStatus == 1, 1, hormones$`Excluded?`) # Chaku & Barry used BirthConNA, not BirthControlStatus. See note above.
table(hormones$`Excluded?`) # n = 4903, 103 excluded

## Missing Caffeine data (CaffNA)
# If `HadCaffeine?` was NA, or was positive and Caffeine amounts were unreported, CaffNA was changed from 0 to 1
hormones$CaffNA <- if_else((is.na(hormones$`HadCaffeine?`) | (hormones$`HadCaffeine?` == 1 & is.na(hormones$`CaffAmount(mg)`))), 1, 0)
hormones$`Excluded?` <- if_else(hormones$CaffNA == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 4964, 61 excluded

## Missing Exercise Data (ExerciseNA)
# If `Exercised?` was NA, or was positive and Exercise Length was unreported, ExerciseNA was changed from 0 to 1
hormones$ExerciseNA <- if_else((is.na(hormones$`Exercised?`) | (hormones$`Exercised?` == 1 & is.na(hormones$`ExerciseLength(min)`))), 1, 0)
hormones$`Excluded?` <- if_else(hormones$ExerciseNA == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 4978, 14 excluded

# Any missing Times
hormones <- hormones %>%
  mutate(no_wake = if_else(is.na(WakeTime), 1, 0),
         no_start = if_else(is.na(StartTime), 1, 0),
         no_end = if_else(is.na(EndTime), 1, 0),
         no_freeze = if_else(is.na(FreezeTime), 1, 0))

# count numbers of missing times
table(hormones$no_wake) # 2371
table(hormones$no_start) # 2327
table(hormones$no_end) # 2373
table(hormones$no_freeze) # 2382

# Flag for exclusion and count
hormones$`Excluded?` <- if_else(hormones$no_wake == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 5026, 48 excluded
hormones$`Excluded?` <- if_else(hormones$no_start == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 5032, 6 excluded
hormones$`Excluded?` <- if_else(hormones$no_end == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 5070, 38 excluded
hormones$`Excluded?` <- if_else(hormones$no_freeze == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 5095, 25 excluded

## StartTime prior to WakeTime (S_b4_W)
# If Start Time was reported as before Wake Time, S_b4_W was changed from 0 to 1
hormones$S_b4_W <- if_else(hormones$StartTime < hormones$WakeTime, 1, 0)
hormones$S_b4_W <- if_else(is.na(hormones$S_b4_W), 0, hormones$S_b4_W) # set NA Start before Wake values to 0 (missing start or wake time, accounted for above)
hormones$`Excluded?` <- if_else(hormones$S_b4_W == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 5158, 63 excluded

## EndTime prior to StartTime (E_b4_S)
# If End Time was reported as before Start Time, E_b4_S was changed from 0 to 1
hormones$E_b4_S <- if_else(hormones$EndTime < hormones$StartTime, hormones$E_b4_S <- 1, 0)
hormones$E_b4_S <- if_else(is.na(hormones$E_b4_S), 0, hormones$E_b4_S) # set NA End before Start values to 0 (missing start or end time, accounted for above)
hormones$`Excluded?` <- if_else(hormones$E_b4_S == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 5245, 87 excluded

## FreezeTime prior to EndTime (F_b4_E)
# If Freeze Time was reported as before End Time, F_b4_E was changed from 0 to 1
hormones$F_b4_E <- if_else(hormones$FreezeTime < hormones$EndTime, hormones$F_b4_E <- 1, 0)
hormones$F_b4_E <- if_else(is.na(hormones$F_b4_E), 0, hormones$F_b4_E) # set NA Freeze before End values to 0 (missing end or freeze time, accounted for above)
hormones$`Excluded?` <- if_else(hormones$F_b4_E == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 5544, 299 excluded



#################
# IN THE BELOW CODE BLOCK
# Further specific clean exclusions were performed by Uban et al,. 2018, and one was derived from Salimetrics use book
# link to Uban paper: https://pmc.ncbi.nlm.nih.gov/articles/PMC6487488/
# link to Salimetrics use book: https://salimetrics.com/wp-content/uploads/2018/03/dhea-saliva-elisa-kit.pdf
#################

## FURTHER SPECIFIC CLEAR EXCLUSIONS

## Sample Collection Start to Finish > 30 minutes (S2Egr30) (Figure 2, Uban et al., 2018)
# If Samples took longer than 30 minutes to collect, S2Egr30 was changed from 0 to 1 
hormones$S2Egr30 <- if_else(hormones$StartToEnd > 1800, 1, hormones$S2Egr30)
hormones$S2Egr30 <- if_else(is.na(hormones$S2Egr30), 0, hormones$S2Egr30) # set NA [Start to End > 30 minutes] values to 0 (missing start or end time, accounted for above)
hormones$`Excluded?` <- if_else(hormones$S2Egr30 == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 5697, 154 excluded

## Wake Time after 3pm (WakeAfter3) (Section 3.2.2, Uban et al., 2018)
# If Wake Time was reported as after 3pm, WakeAfter3 was changed from 0 to 1 
hormones$WakeAfter3 <- if_else(hormones$WakeTime > 54000, 1, hormones$WakeAfter3)
hormones$WakeAfter3 <- if_else(is.na(hormones$WakeAfter3), 0, hormones$WakeAfter3) # set NA [Wake Time after 3pm] values to 0 (Missing wake or start times, accounted for above)
hormones$`Excluded?` <- if_else(hormones$WakeAfter3 == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 5737, 40 excluded

# Sample took too long between End Time and Freeze Time. (FrozeTooSlow) (Salimetrics, page 6)
# "Refrigerate sample within 30 minutes, and freeze at or below -20ºC within 4 hours of collection."
#     Sample took too long between End Time and Freeze Time. 
#     If the difference between EndTime and FreezeTime was > 30 minutes, FrozeTooSlow was changed from 0 to 1
hormones$FrozeTooSlow <- if_else(hormones$EndToFreeze > 1800, 1, hormones$FrozeTooSlow)
hormones$FrozeTooSlow <- if_else(is.na(hormones$FrozeTooSlow), 0, hormones$FrozeTooSlow) # set NA [End time to Freeze Time > 30 minutes] values to 0 (Missing end or freeze times, accounted for above)
hormones$`Excluded?` <- if_else(hormones$FrozeTooSlow == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 6133, 396 excluded

####################
# IN THE BELOW CODE BLOCK
# SANITY CHECK:
# Re-Flag positives of all aftermentioned Exclusion Flags as Excluded
# Make sure Exclusion counts line up
####################

## Reset General Exclusion Column
hormones <- hormones %>%
  mutate(`Excluded?` = 0)

# Flag all Exclusions as Excluded
hormones$`Excluded?` <- if_else(hormones$`Mas!=Sal` == 1 |
  hormones$QualCon == 1 |
    hormones$NoDHEA_Data == 1 |
    hormones$BirthControlStatus == 1 | # Chaku & Barry used BirthConNA, not BirthControlStatus. See note in line 339.
    hormones$CaffNA == 1 |
    hormones$ExerciseNA == 1 |
    hormones$no_wake == 1 |
    hormones$no_start == 1 |
    hormones$no_end == 1 |
    hormones$no_freeze == 1 |
    hormones$S_b4_W == 1 |
    hormones$E_b4_S == 1 |
    hormones$F_b4_E == 1 |
    hormones$S2Egr30 == 1 |
    hormones$WakeAfter3 == 1 |
    hormones$FrozeTooSlow == 1, 1, 0)

# Aim: 6133
table(hormones$`Excluded?`)
# Total: 6133 excluded, 31453 usable

# Sanity check for single exclusions
hormones <- hormones %>%
  mutate(NumExclusions = rowSums(across(c(QualCon, NoDHEA_Data, BirthControlStatus, 
                                          CaffNA, ExerciseNA, no_wake, no_start, no_end, no_freeze, S_b4_W, E_b4_S, F_b4_E, 
                                          S2Egr30, WakeAfter3, FrozeTooSlow), ~ . == 1), na.rm = TRUE))

SingleExclusions <- hormones %>%
  filter(NumExclusions == 1)
# 3148 of 6133 timepoints excluded for a single reason

#######################
# IN THE BELOW CODE BLOCK
# Write Exclusion Dataframe to csv
########################

# Write to .csv
write_csv(hormones, paste0(derivative_data, "DHEA_5.0_Exclusions_10_7_25.csv"))
