###############################
# DHEA_Exclusions_6.0.R
# Author: Robert Toms
# Date: 9/30/2025
###############################

# import libraries
library(dplyr)
library(readr)
library(hms)

# define paths
sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/6.0/"

# read in csv files
hormones <- read_tsv(paste0(sourcepath, 'Physical_Health/ph_y_phs.tsv'))
birth_control <- read_tsv(paste0(sourcepath, 'Physical_Health/ph_y_pds.tsv')) 
sex <- read_tsv(paste0(sourcepath, '/abcd_general/ab_g_stc.tsv'))

###############
# IN THE BELOW CODE BLOCK
# Merge Hormone and Demographic Data
###############

# keep only relevant columns in Demographics, AllSalivaHormones, and BirthControl
sex <- sex %>%
  select(participant_id, ab_g_stc__cohort_sex)

hormones <- hormones %>%
  select(participant_id, 
         session_id, 
         ph_y_phs_001, 		# What time did you wake up today?
         ph_y_phs_003, 		# Have you had any caffeine in the last 12 hours?
         ph_y_phs_003__01,		# If yes, how much caffeine (in mg)
         ph_y_phs_004,		# In the last 12 hours, did you exercise vigorously (sweating, breathing hard) for at least 20 minutes?
         ph_y_phs_004__01,	# For how long did you exercise?
         ph_y_phs__coll_t,		# Time saliva sample collection started
         ph_y_phs__coll__end_t,		# Time saliva sample collection finished
         ph_y_phs__freeze_t,		# Time saliva sample was moved to the freezer
         ph_y_phs_005___1,	# No concerns about sample
         ph_y_phs_005___2,	# Contaminated sample?
         ph_y_phs_005___3,	# Discolored sample?
         ph_y_phs_005___4,	# Excessive bubbles in sample?
         ph_y_phs_005___5,	# Very little saliva provided?
         ph_y_phs_005___6,	# Other sample concern is present
         ph_y_phs__dhea_mean,		# Salimetrics hormone test DHEA mean of measures (pg/mL)
         ph_y_phs__dhea__r01_qnt,		# Salimetrics hormone test DHEA repetition 1 (pg/mL)
         ph_y_phs__dhea__r01_low,	# Salimetrics hormone test DHEA below lower limit of sensitivity (repetition 1)
         ph_y_phs__dhea__r01_insuff,	# Salimetrics hormone test DHEA quantity not sufficient (repetition 1)
         ph_y_phs__dhea__r01_null,	# Salimetrics hormone test DHEA none detected (repetition 1)
         ph_y_phs__dhea__r02_qnt,		# Salimetrics hormone test DHEA repetition 2 (pg/mL)
         ph_y_phs__dhea__r02_low,	# Salimetrics hormone test DHEA below lower limit of sensitivity (repetition 2)
         ph_y_phs__dhea__r02_insuff,	# Salimetrics hormone test DHEA quantity not sufficient (repetition 2)
         ph_y_phs__dhea__r02_null)	# Salimetrics hormone test DHEA none detected (repetition 2)

# Keep only relevant birth control information
birth_control <- birth_control %>%
  select(participant_id, 
         session_id,
         ph_y_pds__f_002__05) # Are you currently using hormonal birth control (eg. the pill, hormone patch, hormone injection)?

# rename remaining columns for readability, for both Demographics and AllSalivaHormones
sex <- sex %>%
  rename(sex = ab_g_stc__cohort_sex)

hormones <- hormones %>%
  rename(WakeTime = ph_y_phs_001,
         "HadCaffeine?" = ph_y_phs_003,
         "CaffAmount(mg)" = ph_y_phs_003__01,
         "Exercised?" = ph_y_phs_004,
         "ExerciseLength(min)" = ph_y_phs_004__01,
         StartTime = ph_y_phs__coll_t,
         EndTime = ph_y_phs__coll__end_t,
         FreezeTime = ph_y_phs__freeze_t,
         "No_Sal_Problems?" = ph_y_phs_005___1,
         "Contaminated_Sal?" = ph_y_phs_005___2,
         "Discolored_Sal?" = ph_y_phs_005___3,
         "Bubbles_Sal?" = ph_y_phs_005___4,
         "NotEnough_Sal?" = ph_y_phs_005___5,
         "OtherComplication_Sal" = ph_y_phs_005___6,
         DHEAmean = ph_y_phs__dhea_mean,
         DHEA_Rep1 = ph_y_phs__dhea__r01_qnt,
         DHEA_Rep1_LL = ph_y_phs__dhea__r01_low,
         DHEA_Rep1_QNS = ph_y_phs__dhea__r01_insuff,
         DHEA_Rep1_ND = ph_y_phs__dhea__r01_null,
         DHEA_Rep2 = ph_y_phs__dhea__r02_qnt,
         DHEA_Rep2_LL = ph_y_phs__dhea__r02_low,
         DHEA_Rep2_QNS = ph_y_phs__dhea__r02_insuff,
         DHEA_Rep2_ND = ph_y_phs__dhea__r02_null)

birth_control <- birth_control %>%
  rename(BirthControlStatus = ph_y_pds__f_002__05)

## Add Master Sex from Demographics to AllSalivaHormones
# Join the dataframes
hormones <- hormones %>%
  left_join(sex, by = "participant_id")

# Merge BirthControl into AllSalivaHormones
hormones <- hormones %>%
  left_join(birth_control, by = c("participant_id", "session_id"))

# Recode all 'n/a' values to NA
hormones <- hormones %>%
  mutate(across(c(WakeTime, `HadCaffeine?`, `CaffAmount(mg)`, `Exercised?`, `ExerciseLength(min)`, 
                  StartTime, EndTime, FreezeTime, `No_Sal_Problems?`, `Contaminated_Sal?`, `Discolored_Sal?`, `Bubbles_Sal?`,
                  `NotEnough_Sal?`, OtherComplication_Sal, DHEAmean, DHEA_Rep1, DHEA_Rep1_LL, DHEA_Rep1_QNS, DHEA_Rep1_ND,
                  DHEA_Rep2, DHEA_Rep2_LL, DHEA_Rep2_QNS, DHEA_Rep2_ND, BirthControlStatus), ~na_if(., 'n/a')))

# Recode 777s and 999s in BirthControlStatus to 0s (Chaku & Barry, 2023)
# Are you currently using hormonal birth control? 1 = Yes; 0 = No; 999 = Don't know; 777 = Refuse to answer
hormones <- hormones %>%
  mutate(BirthControlStatus = as.numeric(BirthControlStatus)) %>%
  mutate(BirthControlStatus = if_else(BirthControlStatus == 777 | BirthControlStatus == 999, 0, BirthControlStatus))

# Calculate number of timepoints with at least one DHEA sample
NumTimepointsWithData <- hormones %>%
  group_by(participant_id) %>%
  summarise(TimepointsWithDHEAData = sum(!is.na(DHEAmean)))

# merge in column listing how many Timepoints with Data a given subject has
hormones <- hormones %>%
  left_join(NumTimepointsWithData, by = "participant_id")

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
  select(participant_id, session_id,
         TimepointsWithDHEAData, sex,
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
  #mutate("Mas!=Sal" = 0) %>%        # (Mas!=Sal) = Master Sex does not match Salivary Sex (Herting et al., 2021)
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
# STEP 1: Salivary Sex matches Master Sex.
# ABCD 6.0 has removed the Salivary Sex item, precluding the use of this criterion.

# STEP 2: Was the Hormone sample collected? (Flag if value is NA)
# STEP 3: Has the sample been processed for Hormone levels? (if there were values, it was assumed that the sample has been processed)
hormones <- hormones %>%
  mutate(DHEA1 = if_else(!is.na(DHEA_Rep1), 0, 1),
         DHEA2 = if_else(!is.na(DHEA_Rep2), 0, 1))
# count number flagged
table(hormones$DHEA1) # 4886
table(hormones$DHEA2) # 5056
hormones <- hormones %>%
  mutate(step_2_3 = if_else(DHEA1 == 1 | DHEA2 == 1, 1, 0))
table(hormones$step_2_3) # 5107

# STEP 4: Do replicate values fall above higher limits of each hormone assay? (Samples > 1000 pg/mL were flagged)
# STEP 5: Is the replicate too low for detection? (Samples < 10.2 pg/mL were flagged)
# Add Flag for 10.2pg/mL < DHEA < 1000 pg/mL | DHEA == NA samples -- (Herting et al., 2021)

# count number of outliers
hormones <- hormones %>%
  mutate(DHEA_Rep1 = as.numeric(DHEA_Rep1),
         DHEA_Rep2 = as.numeric(DHEA_Rep2)) %>%
  mutate(below_detect = if_else(DHEA_Rep1 < 10.2 | DHEA_Rep2 < 10.2, 1, 0)) %>%
  mutate(above_detect = if_else(DHEA_Rep1 > 1000 | DHEA_Rep2 > 1000, 1, 0))
table(hormones$below_detect) # 1438
table(hormones$above_detect) # 12
hormones$step_4_5 <- if_else(hormones$below_detect == 1 | hormones$above_detect == 1, 1, 0)
table(hormones$step_4_5) # 1450

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

### Further Exclusion/Caution Flags (Chaku & Barry, 2023)
## Quality Concerns (QualCon) 
# If sample quality was in doubt (bubbles, discoloration, etc., No Saliva Problems column != 1) \
#     AND there was at least 1 missing or unusable sample (DHEA1 or DHEA2 Flag), QualCon flag was changed from 0 to 1
hormones$QualCon <- if_else((hormones$`No_Sal_Problems?` != 1 & hormones$DHEA1 == 1) | (hormones$`No_Sal_Problems?` != 1 & hormones$DHEA2 == 1), 1, 0)
hormones$`Excluded?` <- if_else(hormones$QualCon == 1, 1, 0)
table(hormones$`Excluded?`) # n = 1187

# *** Upon PI Suggestion, keeping all valid-test DHEA outliers ***

## No DHEA Data (NoDHEA_Data)
# If the DHEAmean was NA (meaning there were zero samples in a timepoint), the NoDHEA_Data flag was changed from 0 to 1. 
hormones$NoDHEA_Data <- if_else(is.na(hormones$DHEAmean), 1, 0)
hormones$`Excluded?` <- if_else(hormones$NoDHEA_Data == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 5931, 4744 excluded

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
table(hormones$`Excluded?`) # n = 6599, 668 excluded

## Missing Caffeine data (CaffNA)
# If `HadCaffeine?` was NA, or was positive and Caffeine amounts were unreported, CaffNA was changed from 0 to 1
hormones$CaffNA <- if_else((is.na(hormones$`HadCaffeine?`) | (hormones$`HadCaffeine?` == 1 & is.na(hormones$`CaffAmount(mg)`))), 1, 0)
hormones$`Excluded?` <- if_else(hormones$CaffNA == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 6676, 77 excluded

## Missing Exercise Data (ExerciseNA)
# If `Exercised?` was NA, or was positive and Exercise Length was unreported, ExerciseNA was changed from 0 to 1
hormones$ExerciseNA <- if_else((is.na(hormones$`Exercised?`) | (hormones$`Exercised?` == 1 & is.na(hormones$`ExerciseLength(min)`))), 1, 0)
hormones$`Excluded?` <- if_else(hormones$ExerciseNA == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 6697, 21 excluded

# Any missing Times
hormones <- hormones %>%
  mutate(no_wake = if_else(is.na(WakeTime), 1, 0),
         no_start = if_else(is.na(StartTime), 1, 0),
         no_end = if_else(is.na(EndTime), 1, 0),
         no_freeze = if_else(is.na(FreezeTime), 1, 0))

# count numbers of missing times
table(hormones$no_wake) # 2572
table(hormones$no_start) # 52
table(hormones$no_end) # 125
table(hormones$no_freeze) # 129

# Flag for exclusion and count
hormones$`Excluded?` <- if_else(hormones$no_wake == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 8693, 1996 excluded
hormones$`Excluded?` <- if_else(hormones$no_start == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 8702, 9 excluded
hormones$`Excluded?` <- if_else(hormones$no_end == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 8757, 55 excluded
hormones$`Excluded?` <- if_else(hormones$no_freeze == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 8794, 37 excluded

## StartTime prior to WakeTime (S_b4_W)
# If Start Time was reported as before Wake Time, S_b4_W was changed from 0 to 1
hormones$S_b4_W <- if_else(hormones$StartTime < hormones$WakeTime, 1, 0)
hormones$S_b4_W <- if_else(is.na(hormones$S_b4_W), 0, hormones$S_b4_W) # set NA Start before Wake values to 0 (missing start or wake time, accounted for above)
hormones$`Excluded?` <- if_else(hormones$S_b4_W == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 8886, 92 excluded

## EndTime prior to StartTime (E_b4_S)
# If End Time was reported as before Start Time, E_b4_S was changed from 0 to 1
hormones$E_b4_S <- if_else(hormones$EndTime < hormones$StartTime, hormones$E_b4_S <- 1, 0)
hormones$E_b4_S <- if_else(is.na(hormones$E_b4_S), 0, hormones$E_b4_S) # set NA End before Start values to 0 (missing start or end time, accounted for above)
hormones$`Excluded?` <- if_else(hormones$E_b4_S == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 9002, 116 excluded

## FreezeTime prior to EndTime (F_b4_E)
# If Freeze Time was reported as before End Time, F_b4_E was changed from 0 to 1
hormones$F_b4_E <- if_else(hormones$FreezeTime < hormones$EndTime, hormones$F_b4_E <- 1, 0)
hormones$F_b4_E <- if_else(is.na(hormones$F_b4_E), 0, hormones$F_b4_E) # set NA Freeze before End values to 0 (missing end or freeze time, accounted for above)
hormones$`Excluded?` <- if_else(hormones$F_b4_E == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 9413, 411 excluded



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
table(hormones$`Excluded?`) # n = 9572, 159 excluded

## Wake Time after 3pm (WakeAfter3) (Section 3.2.2, Uban et al., 2018)
# If Wake Time was reported as after 3pm, WakeAfter3 was changed from 0 to 1 
hormones$WakeAfter3 <- if_else(hormones$WakeTime > 54000, 1, hormones$WakeAfter3)
hormones$WakeAfter3 <- if_else(is.na(hormones$WakeAfter3), 0, hormones$WakeAfter3) # set NA [Wake Time after 3pm] values to 0 (Missing wake or start times, accounted for above)
hormones$`Excluded?` <- if_else(hormones$WakeAfter3 == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 9628, 56 excluded

# Sample took too long between End Time and Freeze Time. (FrozeTooSlow) (Salimetrics, page 6)
# "Refrigerate sample within 30 minutes, and freeze at or below -20ºC within 4 hours of collection."
#     Sample took too long between End Time and Freeze Time. 
#     If the difference between EndTime and FreezeTime was > 30 minutes, FrozeTooSlow was changed from 0 to 1
hormones$FrozeTooSlow <- if_else(hormones$EndToFreeze > 1800, 1, hormones$FrozeTooSlow)
hormones$FrozeTooSlow <- if_else(is.na(hormones$FrozeTooSlow), 0, hormones$FrozeTooSlow) # set NA [End time to Freeze Time > 30 minutes] values to 0 (Missing end or freeze times, accounted for above)
hormones$`Excluded?` <- if_else(hormones$FrozeTooSlow == 1, 1, hormones$`Excluded?`)
table(hormones$`Excluded?`) # n = 10090, 462 excluded

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
hormones$`Excluded?` <- if_else(#hormones$`Mas!=Sal` == 1 |
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

# Aim: 10090
table(hormones$`Excluded?`)
# Total: 10090 excluded, 40065 usable

# Sanity check for single exclusions
hormones <- hormones %>%
  mutate(NumExclusions = rowSums(across(c(QualCon, NoDHEA_Data, BirthControlStatus, 
                                          CaffNA, ExerciseNA, no_wake, no_start, no_end, no_freeze, S_b4_W, E_b4_S, F_b4_E, 
                                          S2Egr30, WakeAfter3, FrozeTooSlow), ~ . == 1), na.rm = TRUE))

SingleExclusions <- hormones %>%
  filter(NumExclusions == 1)
# 8009 of 10090 timepoints excluded for a single reason

#######################
# IN THE BELOW CODE BLOCK
# Write Exclusion Dataframe to csv
########################

# Write to .csv
write_csv(hormones, paste0(derivative_data, "DHEA_Exclusions_9_30_25.csv"))
