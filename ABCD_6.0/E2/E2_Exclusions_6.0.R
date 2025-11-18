###############################
# E2_Exclusions_6.0.R
# Author: Robert Toms
# 5.0 -> 6.0 Update: Dylan Seay
# Date: 9/30/2025
###############################

# import libraries
library(dplyr)
library(readr)
library(hms)
library(lubridate)

###############
# IN THE BELOW CODE BLOCK
# Import and Merge Hormone and Demographic Data
###############
# define paths
sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/6.0/"

# read in csv files
AllSalivaHormones <- read_tsv(paste0(sourcepath, "Physical_Health/ph_y_phs.tsv"))
Demographics <- read_tsv(paste0(sourcepath, "abcd_general/ab_g_stc.tsv"))
BirthControl <- read_tsv(paste0(sourcepath, "Physical_Health/ph_y_pds.tsv"))

# keep only relevant columns in Demographics, AllSalivaHormones, and BirthControl
Demographics <- Demographics %>%
  select(participant_id, ab_g_stc__cohort_sex)

AllSalivaHormones <- AllSalivaHormones %>%
  select(participant_id, 
         session_id, 
         ph_y_phs_001, 		# What time did you wake up today?
         ph_y_phs_003, 		# Have you had any caffeine in the last 12 hours?
         ph_y_phs_003__01,		# If yes, how much caffeine (in mg)
         ph_y_phs_004,		# In the last 12 hours, did you exercise vigorously (sweating, breathing hard) for at least 20 minutes?
         ph_y_phs_004__01,	# For how long did you exercise?
         ph_y_phs__coll_t,		# Time saliva sample collection started
         ph_y_phs__coll__end_t,		# Time saliva sample colleciton finished
         ph_y_phs__freeze_t,		# Time saliva sample was moved to the freezer
         ph_y_phs_005___1,	# No concerns about sample
         ph_y_phs_005___2,	# Contaminated sample?
         ph_y_phs_005___3,	# Discolored sample?
         ph_y_phs_005___4,	# Excessive bubbles in sample?
         ph_y_phs_005___5,	# Very little saliva provided?
         ph_y_phs_005___6,	# Other sample concern is present
         ph_y_phs__hse_mean,		# Salimetrics hormone test estradiol (HSE) mean of measures (pg/mL)
         ph_y_phs__hse__r01_qnt,		# Salimetrics hormone test estradiol (HSE) repetition 1 (pg/mL)
         ph_y_phs__hse__r01_low,	# Salimetrics hormone test estradiol (HSE) below lower limit of sensitivity (repetition 1)
         ph_y_phs__hse__r01_insuff,	# Salimetrics hormone test estradiol (HSE)  quantity not sufficient (repetition 1)
         ph_y_phs__hse__r01_null,	# Salimetrics hormone test estradiol (HSE) none detected (repetition 1)
         ph_y_phs__hse__r02_qnt,		# Salimetrics hormone test estradiol (HSE) repetition 2 (pg/mL)
         ph_y_phs__hse__r02_low,	# Salimetrics hormone test estradiol (HSE)  below lower limit of sensitivity (repetition 2)
         ph_y_phs__hse__r02_insuff,	# Salimetrics hormone test estradiol (HSE)  quantity not sufficient (repetition 2)
         ph_y_phs__hse__r02_null)	# Salimetrics hormone test estradiol (HSE)  none detected (repetition 2)

# Keep only relevant birth control information
BirthControl <- BirthControl %>%
  select(participant_id, 
         session_id,
         ph_y_pds__f_002__05) # Are you currently using hormonal birth control (eg. the pill, hormone patch, hormone injection)?

# rename remaining columns for readability, for both Demographics and AllSalivaHormones
Demographics <- Demographics %>%
  rename(sex = ab_g_stc__cohort_sex)

AllSalivaHormones <- AllSalivaHormones %>%
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
         E2mean = ph_y_phs__hse_mean,
         E2_Rep1 = ph_y_phs__hse__r01_qnt,
         E2_Rep1_LL = ph_y_phs__hse__r01_low,
         E2_Rep1_QNS = ph_y_phs__hse__r01_insuff,
         E2_Rep1_ND = ph_y_phs__hse__r01_null,
         E2_Rep2 = ph_y_phs__hse__r02_qnt,
         E2_Rep2_LL = ph_y_phs__hse__r02_low,
         E2_Rep2_QNS = ph_y_phs__hse__r02_insuff,
         E2_Rep2_ND = ph_y_phs__hse__r02_null)

BirthControl <- BirthControl %>%
  rename(BirthControlStatus = ph_y_pds__f_002__05)

## Add Master Sex from Demographics to AllSalivaHormones
AllSalivaHormones <- AllSalivaHormones %>%
  left_join(Demographics, by = "participant_id")

# Remove Males
AllSalivaHormones <- AllSalivaHormones %>%
  # before: 50155 
  filter(sex == 2)
  # after: 23961, 26194 removed

# Merge BirthControl into AllSalivaHormones
AllSalivaHormones <- AllSalivaHormones %>%
  left_join(BirthControl, by = c("participant_id", "session_id"))

#BirthControlStatus is character. Make numeric and convert n/a -> 0
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(BirthControlStatus = as.numeric(BirthControlStatus))

# Recode all 'n/a' values to NA
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(across(c(WakeTime, `HadCaffeine?`, `CaffAmount(mg)`, `Exercised?`, `ExerciseLength(min)`, 
                  StartTime, EndTime, FreezeTime, `No_Sal_Problems?`, `Contaminated_Sal?`, `Discolored_Sal?`, `Bubbles_Sal?`,
                  `NotEnough_Sal?`, OtherComplication_Sal, E2mean, E2_Rep1, E2_Rep1_LL, E2_Rep1_QNS, E2_Rep1_ND,
                  E2_Rep2, E2_Rep2_LL, E2_Rep2_QNS, E2_Rep2_ND), ~na_if(., 'n/a')))

# Recode 999s and 777s in BirthControlStatus to 0s (Chaku & Barry, 2023)
# 5.0 Are you currently using hormonal birth control? 1 = Yes; 0 = No; 2 = Don't know; 3 = Refuse to answer
# 6.0 Are you currently using hormonal birth control? 1 = Yes, 0 = No, 999 = Don't know, 777 = Decline to answer
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(BirthControlStatus = if_else(BirthControlStatus == 999 | BirthControlStatus == 777, 0, BirthControlStatus))

# Calculate number of timepoints with at least one E2 sample
NumTimepointsWithData <- AllSalivaHormones %>%
  group_by(participant_id) %>%
  summarise(TimepointsWithE2Data = sum(!is.na(E2mean)))

# merge in column listing how many Timepoints with Data a given subject has
AllSalivaHormones <- AllSalivaHormones %>%
  left_join(NumTimepointsWithData, by = "participant_id")

# Add column with number of samples (reps) per visit (should be 0, 1, or 2)
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(SamplesPerVisit = rowSums(!is.na(cbind(E2_Rep1, E2_Rep2))))


############################
# IN THE BELOW CODE BLOCK
# Make new columns with Time Differentials-- key to exclusion criteria from (Uban et al., 2018)
# Derive Absolute Values of Time Differentials (PI Suggestion)
# Reorder Columns for clarity and readability
############################

# convert 'n/a' values to NA
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(WakeTime = na_if(WakeTime, "n/a"),
         StartTime = na_if(StartTime, "n/a"),
         EndTime = na_if(EndTime, "n/a"),
         FreezeTime = na_if(FreezeTime, "n/a"))

# convert times to hms
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(WakeTime = as_hms(WakeTime),
         StartTime = as_hms(StartTime),
         EndTime = as_hms(EndTime),
         FreezeTime = as_hms(FreezeTime))

# Make new columns for WakeTime -> StartTime, StartTime -> EndTime, EndTime -> FreezeTime
# In the current data type, these times are represented in the GUI as HH:MM:SS, but are stored in seconds counting up from midnight (1am = 3600, etc.). These will be stored as differences of seconds.
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(WakeToStart = StartTime - WakeTime) %>%
  mutate(StartToEnd = EndTime - StartTime) %>%
  mutate(EndToFreeze = FreezeTime - EndTime)

#Make Absolute Value columns for WakeToStart, StartToEnd, EndToFreeze
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(Abs_WakeToStart = abs(WakeToStart)) %>%
  mutate(Abs_StartToEnd = abs(StartToEnd)) %>%
  mutate(Abs_EndToFreeze = abs(EndToFreeze))

# Reorder Columns for readability and inter-relevance
AllSalivaHormones <- AllSalivaHormones %>%
  select(participant_id,
         session_id,
         TimepointsWithE2Data,
         sex,
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
         E2mean,
         SamplesPerVisit,
         E2_Rep1,
         E2_Rep1_LL,
         E2_Rep1_QNS,
         E2_Rep1_ND,
         E2_Rep2,
         E2_Rep2_LL,
         E2_Rep2_QNS,
         E2_Rep2_ND)

############################
# IN THE BELOW CODE BLOCK
# Creating Individual Columns for Caution/Exclusion Flags
############################

# Create columns for Exclusion/Caution Flags
AllSalivaHormones <- AllSalivaHormones %>%
  mutate("E1" = 0) %>%              # (E1) = E2Rep1 out of range/no data (Herting et al., 2021)
  mutate("E2" = 0) %>%              # (E2) = E2Rep2 out of range/no data (Herting et al., 2021)
  #mutate("Mas!=Sal" = 0) %>%        # (Mas!=Sal) = Master Sex does not match Salivary Sex (Herting et al., 2021) # not in 6.0
  mutate("QualCon" = 0) %>%         # (QualCon) = Quality Concerns(No_Sal_Problems != 1) & ((E1) | (E2)) Chaku & Barry, 2023) 
  mutate("NoEst_Data" = 0) %>%      # (NoEst_Data) = No data available (Chaku & Barry, 2023) 
  mutate("BirthConNA" = 0) %>%      # (BirthConNA) = missing Birth Control data (Chaku & Barry, 2023) 
  mutate("CaffNA" = 0) %>%          # (CaffNA) = missing Caffeine data (Chaku & Barry, 2023) 
  mutate("ExerciseNA" = 0) %>%      # (ExerciseNA) = missing Exercise data (Chaku & Barry, 2023) 
  mutate("S_b4_W" = 0) %>%          # (S_b4_W) = Start Time reported to be before Wake Time (Chaku & Barry, 2023) 
  mutate("E_b4_S" = 0) %>%          # (E_b4_S) = End Time reported to be before Start Time (Chaku & Barry, 2023) 
  mutate("F_b4_E" = 0) %>%          # (F_b4_E) = Freeze Time reported to be before End Time (Chaku & Barry, 2023) 
  mutate(S2Egr30 = 0) %>%           # (S2Egr30) = StartToEnd > 30 minutes (Uban et al., 2018)
  mutate(WakeAfter3 = 0) %>%        # (WakeAfter3) = WakeTime after 3pm (Uban et al., 2018)
  mutate(FrozeTooSlow = 0)       # (FrozeTooSlow) = EndToFreeze > 30 minutes (Salimetrics)

##################
# IN THE BELOW CODE BLOCK
# Quality checking and Generating a single hormone metric (Herting et al., 2021)
#   (Chaku & Barry, 2023) note in their paper that they followed the best practices 
#   of (Herting et al., 2021) for the first part of inclusion/exclusion—quality 
#   checking and generating a single hormone metric per participant—so Herting et al.’s 
#   original methodology was followed for that section. Apart from Lower Limit 
#   difference of 0.01 vs. 0.1 (confirmed to be 0.1 by Salimetrics), Chaku and 
#   Herting’s methods are identical (with one exception, noted below re: lines 301-302)
# link to Herting et al. paper: https://www.frontiersin.org/journals/endocrinology/articles/10.3389/fendo.2020.549928/full
# Steps 1-6 referred to below from Figure 1
##################

# QUALITY CHECK AND GENERATE A SINGLE HORMONE METRIC (Herting et al., 2021)
# STEP 1: Salivary Sex matches Master Sex.
# ABCD 6.0 has removed the Salivary Sex item, precluding the use of this criterion.

# STEP 2: Was the Hormone sample collected? (Flagged if value was NA)
# STEP 3: Has the sample been processed for Hormone levels? (if there were values, it was assumed that the sample has been processed)
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(E1 = if_else(!is.na(E2_Rep1), 0, 1),
         E2 = if_else(!is.na(E2_Rep2), 0, 1))

#count number flagged
table(AllSalivaHormones$E1) # 3038
table(AllSalivaHormones$E2) # 3244
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(step_2_3 = if_else(E1 == 1 | E2 == 1, 1, 0))
table(AllSalivaHormones$step_2_3) # 3309

# STEP 4: Do replicate values fall above higher limits of each hormone assay? (Samples > 32 pg/mL were flagged)
# STEP 5: Is the replicate too low for detection? (Samples < 0.1 pg/mL were flagged)
# Add Flag for 0.1 < Est < 32 | Est == NA samples -- (Herting et al., 2021)
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(E2mean = as.numeric(E2mean),
         E2_Rep1 = as.numeric(E2_Rep1),
         E2_Rep2 = as.numeric(E2_Rep2)) %>%
  mutate(below_detect = if_else(E2_Rep1 < 0.1 | E2_Rep2 < 0.1, 1, 0)) %>%
  mutate(above_detect = if_else(E2_Rep1 > 32 | E2_Rep2 > 32, 1, 0))
table(AllSalivaHormones$below_detect) # 204
table(AllSalivaHormones$above_detect) # 1
AllSalivaHormones$step_4_5 <- if_else(AllSalivaHormones$below_detect == 1 | AllSalivaHormones$above_detect == 1, 1, 0)
table(AllSalivaHormones$step_4_5) # 205

# STEPS 4 & 5 (continued): If outside Salimetrics test sensitivity limits, set to 0 (Herting et al., 2021) or NA (Chaku & Barry, 2023)
# Flag 
AllSalivaHormones$E1 <- if_else(AllSalivaHormones$E2_Rep1 < 0.1 | 
                                  AllSalivaHormones$E2_Rep1 > 32 | 
                                  is.na(AllSalivaHormones$E2_Rep1), 
                                AllSalivaHormones$E1 <- 1, 0)
AllSalivaHormones$E2 <- if_else(AllSalivaHormones$E2_Rep2 < 0.1 | 
                                  AllSalivaHormones$E2_Rep2 > 32 | 
                                  is.na(AllSalivaHormones$E2_Rep2), 
                                AllSalivaHormones$E2 <- 1, 0)

# If E2 is Outside of lower/upper limits, set to NA (Chaku & Barry, 2023)
AllSalivaHormones$E2_Rep1 <- if_else(AllSalivaHormones$E1 == 1, NA, AllSalivaHormones$E2_Rep1)
AllSalivaHormones$E2_Rep2 <- if_else(AllSalivaHormones$E2 == 1, NA, AllSalivaHormones$E2_Rep2)

# Recompute SamplesPerVisit, (PI Suggestion)
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(SamplesPerVisit = rowSums(!is.na(cbind(E2_Rep1, E2_Rep2))))

# STEP 6: Obtain a single value for each hormone type.
# Timepoints with 2 values were averaged, timepoints with 1 value used the single value.
# Recompute E2means for Single Hormone Value
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(E2mean = rowMeans(cbind(E2_Rep1, E2_Rep2), na.rm = TRUE),
         E2mean = coalesce(E2mean, E2_Rep1, E2_Rep2))

#######################
# IN THE BELOW CODE BLOCK
# Add Caution/Exclusion Flags
# In following the best practices of Chaku & Barry, further exclusions were made beyond the checks made by Herting et al.
# link to Chaku & Barry, 2023: https://onlinelibrary.wiley.com/doi/full/10.1002/icd.2415
# Figure 1, page 9
########################

# set NA No_Sal_Problems to 0, a lack of quality check data invalidates the data
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(`No_Sal_Problems?` = as.numeric(`No_Sal_Problems?`)) %>%
  mutate(`No_Sal_Problems?` = if_else(is.na(`No_Sal_Problems?`), 0, `No_Sal_Problems?`))

### Further Exclusion/Caution Flags (Chaku & Barry, 2023)
## Quality Concerns (QualCon) 
# If sample quality was in doubt (bubbles, discoloration, etc., No Saliva Problems column != 1) 
#     AND there was at least 1 missing or unusable sample (E1 or E2 Flag), QualCon flag was changed from 0 to 1
AllSalivaHormones$QualCon <- if_else((AllSalivaHormones$`No_Sal_Problems?` != 1 & AllSalivaHormones$E1 == 1) | 
                                       (AllSalivaHormones$`No_Sal_Problems?` != 1 & AllSalivaHormones$E2 == 1), 1, 0)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$QualCon == 1, 1, 0)
table(AllSalivaHormones$`Excluded?`) # n = 776

# *** Upon PI Suggestion, keeping all valid-test estradiol outliers ***

## No Estradiol Data (NoEst_Data)
# If the E2mean was NA (meaning there were zero samples in a timepoint), the NoEst_Data flag was changed from 0 to 1. 
AllSalivaHormones$NoEst_Data <- if_else(is.na(AllSalivaHormones$E2mean), 1, 0)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$NoEst_Data == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 3195, 2419 excluded

## Missing Birth Control data (females) (BirthConNA) 
####### NOTE: When using BirthConNA, only including those with birth control data and endorsing not using birth control, 
# our sample size dropped from thousands to a few dozen. We created this variable flag in the lines below, but in flagging 
# for exclusion due to birth control use, we chose to assume that no birth control data meant birth control was 
# not being used, and only excluded those who had birth control data and affirmed birth control use. Only keeping timepoints 
# with birth control data would exclude all pre-menarche timepoints, for young women who have no reason to consider birth control yet. ##########
# If missing Birth Control data (BirthControlStatus=NA), the BirthConNA flag was changed from 0 to 1
AllSalivaHormones$BirthConNA <- if_else(is.na(AllSalivaHormones$BirthControlStatus), 1, 0)
AllSalivaHormones$BirthControlStatus <- if_else(is.na(AllSalivaHormones$BirthControlStatus), 0, AllSalivaHormones$BirthControlStatus)# set NA Birth Control values to 0 
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$BirthControlStatus == 1, 1, AllSalivaHormones$`Excluded?`) # Chaku & Barry used BirthConNA, not BirthControlStatus. See note above.
table(AllSalivaHormones$`Excluded?`) # n = 3851, 656 excluded

## Missing Caffeine data (CaffNA) 
# If `HadCaffeine?` was NA, or was positive and Caffeine amounts were unreported, CaffNA was changed from 0 to 1
AllSalivaHormones$CaffNA <- if_else((is.na(AllSalivaHormones$`HadCaffeine?`) | (AllSalivaHormones$`HadCaffeine?` == 1 & is.na(AllSalivaHormones$`CaffAmount(mg)`))), 1, 0)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$CaffNA == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 3890, 39 excluded

## Missing Exercise Data (ExerciseNA)
# If `Exercised?` was NA, or was positive and Exercise Length was unreported, ExerciseNA was changed from 0 to 1
AllSalivaHormones$ExerciseNA <- if_else((is.na(AllSalivaHormones$`Exercised?`) | (AllSalivaHormones$`Exercised?` == 1 & is.na(AllSalivaHormones$`ExerciseLength(min)`))), 1, 0)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$ExerciseNA == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 3898, 8 excluded

# Any missing Times
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(no_wake = if_else(is.na(WakeTime), 1, 0),
         no_start = if_else(is.na(StartTime), 1, 0),
         no_end = if_else(is.na(EndTime), 1, 0),
         no_freeze = if_else(is.na(FreezeTime), 1, 0))

# count numbers of missing times
table(AllSalivaHormones$no_wake) # 1232
table(AllSalivaHormones$no_start) # 27
table(AllSalivaHormones$no_end) # 58
table(AllSalivaHormones$no_freeze) # 64

# Flag for exclusion and count
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$no_wake == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 4706, 808 excluded
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$no_start == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 4708, 2 excluded
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$no_end == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 4727, 19 excluded
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$no_freeze == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 4744, 17 excluded

## StartTime prior to WakeTime (S_b4_W)
# If Start Time was reported as before Wake Time, S_b4_W was changed from 0 to 1
AllSalivaHormones$S_b4_W <- if_else(AllSalivaHormones$StartTime < AllSalivaHormones$WakeTime, AllSalivaHormones$S_b4_W <- 1, 0)
AllSalivaHormones$S_b4_W <- if_else(is.na(AllSalivaHormones$S_b4_W), 0, AllSalivaHormones$S_b4_W) # set NA Start before Wake values to 0 (missing start or wake time, accounted for above)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$S_b4_W == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 4788, 44 excluded

## EndTime prior to StartTime (E_b4_S)
# If End Time was reported as before Start Time, E_b4_S was changed from 0 to 1
AllSalivaHormones$E_b4_S <- if_else(AllSalivaHormones$EndTime < AllSalivaHormones$StartTime, AllSalivaHormones$E_b4_S <- 1, 0)
AllSalivaHormones$E_b4_S <- if_else(is.na(AllSalivaHormones$E_b4_S), 0, AllSalivaHormones$E_b4_S) # set NA End before Start values to 0 (missing start or end time, accounted for above)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$E_b4_S == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 4838, 50 excluded

## FreezeTime prior to EndTime (F_b4_E)
# If Freeze Time was reported as before End Time, F_b4_E was changed from 0 to 1
AllSalivaHormones$F_b4_E <- if_else(AllSalivaHormones$FreezeTime < AllSalivaHormones$EndTime, AllSalivaHormones$F_b4_E <- 1, 0)
AllSalivaHormones$F_b4_E <- if_else(is.na(AllSalivaHormones$F_b4_E), 0, AllSalivaHormones$F_b4_E) # set NA Freeze before End values to 0 (missing end or freeze time, accounted for above)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$F_b4_E == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 5029, 191 excluded


#################
# IN THE BELOW CODE BLOCK
# Additional Caution/Exclusion Flags
# Further specific clean exclusions were performed by Uban et al,. 2018, and one was derived from Salimetrics use book
# link to Uban paper: https://pmc.ncbi.nlm.nih.gov/articles/PMC6487488/
# link to Salimetrics use book: https://salimetrics.com/wp-content/uploads/2018/03/17beta-estradiol-saliva-elisa-kit.pdf
#################

## FURTHER SPECIFIC CLEAR EXCLUSIONS

## Sample Collection Start to Finish > 30 minutes (S2Egr30) (Figure 2, Uban et al., 2018)
# If Samples took longer than 30 minutes to collect, S2Egr30 was changed from 0 to 1 
AllSalivaHormones$S2Egr30 <- if_else(AllSalivaHormones$StartToEnd > 1800, 1, AllSalivaHormones$S2Egr30)
AllSalivaHormones$S2Egr30 <- if_else(is.na(AllSalivaHormones$S2Egr30), 0, AllSalivaHormones$S2Egr30) # set NA [Start to End > 30 minutes] values to 0 (missing start or end time, accounted for above)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$S2Egr30 == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 5104, 75 excluded

## Wake Time after 3pm (WakeAfter3) (Section 3.2.2, Uban et al., 2018)
# If Wake Time was reported as after 3pm, WakeAfter3 was changed from 0 to 1 
AllSalivaHormones$WakeAfter3 <- if_else(AllSalivaHormones$WakeTime > 54000, 1, AllSalivaHormones$WakeAfter3)
AllSalivaHormones$WakeAfter3 <- if_else(is.na(AllSalivaHormones$WakeAfter3), 0, AllSalivaHormones$WakeAfter3) # set NA [Wake Time after 3pm] values to 0 (Missing wake or start times, accounted for above)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$WakeAfter3 == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 5129, 25 excluded

# Sample took too long between End Time and Freeze Time. (FrozeTooSlow) (Salimetrics, page 6)
# "Refrigerate sample within 30 minutes, and freeze at or below -20ºC within 4 hours of collection."
#     Sample took too long between End Time and Freeze Time. 
#     If the difference between EndTime and FreezeTime was > 30 minutes, FrozeTooSlow was changed from 0 to 1
AllSalivaHormones$FrozeTooSlow <- if_else(AllSalivaHormones$EndToFreeze > 1800, 1, AllSalivaHormones$FrozeTooSlow)
AllSalivaHormones$FrozeTooSlow <- if_else(is.na(AllSalivaHormones$FrozeTooSlow), 0, AllSalivaHormones$FrozeTooSlow) # set NA [End time to Freeze Time > 30 minutes] values to 0 (Missing end or freeze times, accounted for above)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$FrozeTooSlow == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 5357, 228 excluded

####################
# IN THE BELOW CODE BLOCK
# SANITY CHECK:
# Re-Flag positives of all aftermentioned Exclusion Flags as Excluded
# Make sure Exclusion counts line up
####################

## Reset General Exclusion Column
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(`Excluded?` = 0)

# Flag all Exclusions as Excluded
AllSalivaHormones$`Excluded?` <- if_else(
  AllSalivaHormones$QualCon == 1 |
    AllSalivaHormones$NoEst_Data == 1 |
    AllSalivaHormones$BirthControlStatus == 1 | # Chaku & Barry used BirthConNA, not BirthControlStatus. See note in line 339.
    AllSalivaHormones$CaffNA == 1 |
    AllSalivaHormones$ExerciseNA == 1 |
    AllSalivaHormones$no_wake == 1 |
    AllSalivaHormones$no_start == 1 |
    AllSalivaHormones$no_end == 1 |
    AllSalivaHormones$no_freeze == 1 |
    AllSalivaHormones$S_b4_W == 1 |
    AllSalivaHormones$E_b4_S == 1 |
    AllSalivaHormones$F_b4_E == 1 |
    AllSalivaHormones$S2Egr30 == 1 |
    AllSalivaHormones$WakeAfter3 == 1 |
    AllSalivaHormones$FrozeTooSlow == 1, 1, 0)

# Aim: 5357
table(AllSalivaHormones$`Excluded?`)
# Total: 5357 excluded, 18604 usable, 23961 total

# Sanity check for single exclusions
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(NumExclusions = rowSums(across(c(QualCon, NoEst_Data, BirthControlStatus, 
                                          CaffNA, ExerciseNA, no_wake, no_start, no_end, no_freeze, S_b4_W, E_b4_S, F_b4_E, 
                                          S2Egr30, WakeAfter3, FrozeTooSlow), ~ . == 1), na.rm = TRUE))

SingleExclusions <- AllSalivaHormones %>%
  filter(NumExclusions == 1)
# 4050 of 5357 timepoints excluded for a single reason

#######################
# IN THE BELOW CODE BLOCK
# Clean data for export
########################

# order useful columns
AllSalivaHormones <- AllSalivaHormones %>%
  select(participant_id, session_id, sex, TimepointsWithE2Data,
         BirthControlStatus, `HadCaffeine?`, `CaffAmount(mg)`, `Exercised?`, `ExerciseLength(min)`, WakeTime, StartTime, EndTime, FreezeTime,
         WakeToStart, StartToEnd, EndToFreeze, Abs_WakeToStart, Abs_StartToEnd, Abs_EndToFreeze,
         `No_Sal_Problems?`, `Contaminated_Sal?`, `Discolored_Sal?`, `Bubbles_Sal?`, `NotEnough_Sal?`, OtherComplication_Sal,
         E2mean, SamplesPerVisit, E2_Rep1, E2_Rep1_LL, E2_Rep1_QNS, E2_Rep1_ND, E2_Rep2, E2_Rep2_LL, E2_Rep2_QNS, E2_Rep2_ND,
         E1, E2, step_2_3, below_detect, above_detect, step_4_5, 
         QualCon, NoEst_Data, BirthConNA, CaffNA, ExerciseNA, no_wake, no_start, no_end, no_freeze, S_b4_W, E_b4_S, F_b4_E,
         S2Egr30, WakeAfter3, FrozeTooSlow, `Excluded?`, NumExclusions)

#######################
# IN THE BELOW CODE BLOCK
# Write Exclusion Dataframe to csv
########################

# Write to .csv
write_csv(AllSalivaHormones, paste0(derivative_data, "E2_Exclusions_6.0_10_1_25.csv"))
