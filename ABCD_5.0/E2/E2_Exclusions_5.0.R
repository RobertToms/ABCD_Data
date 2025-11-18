###############################
# E2_Exclusions_5.0.R
# Author: Robert Toms
# Date: 10/6/2025
###############################

# import libraries
library(dplyr)
library(readr)
library(hms)

###############
# IN THE BELOW CODE BLOCK
# Import and Merge Hormone and Demographic Data
###############

sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"

# read in csv files
AllSalivaHormones <- read_csv(paste0(sourcepath, "physical-health/ph_y_sal_horm.csv"))
Demographics <- read_csv(paste0(sourcepath, "abcd-general/abcd_p_demo.csv"))
BirthControl <- read_csv(paste0(sourcepath, "physical-health/ph_y_pds.csv"))

# keep only relevant columns in Demographics, AllSalivaHormones, and BirthControl
Demographics <- Demographics %>%
  select(src_subject_id, demo_sex_v2)

AllSalivaHormones <- AllSalivaHormones %>%
  select(src_subject_id, 
         eventname, 
         hormone_sal_sex, 		# Salivary Sex; Female = 1, Male = 2
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
         hormone_scr_hse_mean,		# Salimetrics hormone test estradiol (HSE) mean of measures (pg/mL)
         hormone_scr_hse_rep1,		# Salimetrics hormone test estradiol (HSE) repetition 1 (pg/mL)
         hormone_scr_hse_rep1_ll,	# Salimetrics hormone test estradiol (HSE) below lower limit of sensitivity (repetition 1)
         hormone_scr_hse_rep1_qns,	# Salimetrics hormone test estradiol (HSE)  quantity not sufficient (repetition 1)
         hormone_scr_hse_rep1_nd,	# Salimetrics hormone test estradiol (HSE) none detected (repetition 1)
         hormone_scr_hse_rep2,		# Salimetrics hormone test estradiol (HSE) repetition 2 (pg/mL)
         hormone_scr_hse_rep2_ll,	# Salimetrics hormone test estradiol (HSE)  below lower limit of sensitivity (repetition 2)
         hormone_scr_hse_rep2_qns,	# Salimetrics hormone test estradiol (HSE)  quantity not sufficient (repetition 2)
         hormone_scr_hse_rep2_nd)	# Salimetrics hormone test estradiol (HSE)  none detected (repetition 2)

# Keep only relevant birth control information
BirthControl <- BirthControl %>%
  select(src_subject_id, 
         eventname,
         menstrualcycle4_y) # Are you currently using hormonal birth control (eg. the pill, hormone patch, hormone injection)?

# rename remaining columns for readability, for both Demographics and AllSalivaHormones
Demographics <- Demographics %>%
  rename(SubjectID = src_subject_id,
         MasterSex = demo_sex_v2)

AllSalivaHormones <- AllSalivaHormones %>%
  rename(SubjectID = src_subject_id,
         EventName = eventname,
         SalivarySex = hormone_sal_sex,
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
         E2mean = hormone_scr_hse_mean,
         E2_Rep1 = hormone_scr_hse_rep1,
         E2_Rep1_LL = hormone_scr_hse_rep1_ll,
         E2_Rep1_QNS = hormone_scr_hse_rep1_qns,
         E2_Rep1_ND = hormone_scr_hse_rep1_nd,
         E2_Rep2 = hormone_scr_hse_rep2,
         E2_Rep2_LL = hormone_scr_hse_rep2_ll,
         E2_Rep2_QNS = hormone_scr_hse_rep2_qns,
         E2_Rep2_ND = hormone_scr_hse_rep2_nd)

BirthControl <- BirthControl %>%
  rename(SubjectID = src_subject_id, 
         EventName = eventname,
         BirthControlStatus = menstrualcycle4_y)

## Add Master Sex from Demographics to AllSalivaHormones
# Remove redundant SubjectID rows with NA values, and join the dataframes
Demographics <- na.omit(Demographics)

AllSalivaHormones <- AllSalivaHormones %>%
  left_join(Demographics, by = "SubjectID")

# Merge BirthControl into AllSalivaHormones
AllSalivaHormones <- AllSalivaHormones %>%
  left_join(BirthControl, by = c("SubjectID", "EventName"))

# Recode 2s and 3s in BirthControlStatus to 0s (Chaku & Barry, 2023)
# Are you currently using hormonal birth control? 1 = Yes; 0 = No; 2 = Don't know; 3 = Refuse to answer
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(BirthControlStatus = if_else(BirthControlStatus == 2 | BirthControlStatus == 3, 0, BirthControlStatus))

#############################
# IN THE BELOW CODE BLOCK
# Expand Master and Salivary Sex into binary covariates for later comparison/exclusion
# (Exclusion if MasterSex != SalivarySex recommended by Chaku & Barry and Herting et al., 2021)
#############################

## Expand Master and Salivary Sex into binary Covariates
# Instantiate new Columns
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(MasSex_F = 0) %>% # MasterSex = Female
  mutate(MasSex_M = 0) %>% # MasterSex = Male
  mutate(SalSex_F = 0) %>% # SalivarySex = Female
  mutate(SalSex_M = 0) %>% # SalivarySex = Male
  mutate(SalSex_U2C = 0) %>% # SalivarySex = Particpant Unable to Complete
  mutate(SalSex_Ref = 0) %>% # SalivarySex = Participant/Parent Refused
  mutate(SalSex_NCO = 0) # SalivarySex = Not Collected/Other

## Populate new Columns
AllSalivaHormones$MasSex_F <- if_else(AllSalivaHormones$MasterSex == 2, AllSalivaHormones$MasSex_F <- 1, 0)
AllSalivaHormones$MasSex_M <- if_else(AllSalivaHormones$MasterSex == 1, AllSalivaHormones$MasSex_M <- 1, 0)
AllSalivaHormones$SalSex_F <- if_else(AllSalivaHormones$SalivarySex == 1, AllSalivaHormones$SalSex_F <- 1, 0)
AllSalivaHormones$SalSex_M <- if_else(AllSalivaHormones$SalivarySex == 2, AllSalivaHormones$SalSex_M <- 1, 0)
AllSalivaHormones$SalSex_U2C <- if_else(AllSalivaHormones$SalivarySex == 3, AllSalivaHormones$SalSex_U2C <- 1, 0)
AllSalivaHormones$SalSex_Ref <- if_else(AllSalivaHormones$SalivarySex == 4, AllSalivaHormones$SalSex_Ref <- 1, 0)
AllSalivaHormones$SalSex_NCO <- if_else(AllSalivaHormones$SalivarySex == 5, AllSalivaHormones$SalSex_NCO <- 1, 0)

######################
# IN THE BELOW CODE BLOCK
# Adjust Mean of E2 samples to include timepoints with just 1 sample rather than just 2 (Herting et al., 2021)
# Derive each subject’s number of timepoints with at least 1 E2 sample (PI Suggestion)
# Count number of samples per timepoint (PI Suggestion)
######################

### E2 SAMPLES
# Recompute E2means to average Timepoints with 2 values. If timepoints only have 1 value, use that.
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(E2mean = rowMeans(cbind(E2_Rep1, E2_Rep2), na.rm = TRUE),
         E2mean = coalesce(E2mean, E2_Rep1, E2_Rep2))

# Calculate number of timepoints with at least one E2 sample
NumTimepointsWithData <- AllSalivaHormones %>%
  group_by(SubjectID) %>%
  summarise(TimepointsWithE2Data = sum(!is.na(E2mean)))

# merge in column listing how many Timepoints with Data a given subject has
AllSalivaHormones <- AllSalivaHormones %>%
  left_join(NumTimepointsWithData, by = "SubjectID")

# Add column with number of samples (reps) per visit (should be 0, 1, or 2)
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(SamplesPerVisit = rowSums(!is.na(cbind(E2_Rep1, E2_Rep2))))


############################
# IN THE BELOW CODE BLOCK
# Make new columns with Time Differentials-- key to exclusion criteria from (Uban et al., 2018)
# Derive Absolute Values of Time Differentials (PI Suggestion)
# Reorder Columns for clarity and readability
############################

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
  select(SubjectID,
         EventName,
         TimepointsWithE2Data,
         MasterSex,
         MasSex_F,
         MasSex_M,
         BirthControlStatus,
         SalivarySex,
         SalSex_F,
         SalSex_M,
         SalSex_U2C,
         SalSex_Ref,
         SalSex_NCO,
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
  mutate("Mas!=Sal" = 0) %>%        # (Mas!=Sal) = Master Sex does not match Salivary Sex (Herting et al., 2021)
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

# Keep only Females
AllSalivaHormones <- AllSalivaHormones %>%
  # before: 37586 timepoints
  filter(MasterSex == 2)
# after: 17924, 19662 removed

## Quality Check and Get Single Hormone Values: (Herting et al., 2021)

# Quality Check
# STEP 1: Check that Master Sex matches Salivary Sex
# Convert MasterSex (1=M, 2=F) to match SalivarySex (1=F, 2=M)
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(MasterSex = case_when(
    MasterSex == 1 ~ 2,
    MasterSex == 2 ~ 1,
    TRUE ~ MasterSex
  ))
# Add Exclusion Flag (Mas!=Sal) -- (Herting et al., 2021)
AllSalivaHormones$`Mas!=Sal` <- if_else(AllSalivaHormones$MasterSex == AllSalivaHormones$SalivarySex, 0, 1)

# STEP 2: Was the Hormone sample collected? (Flagged if value was NA)
# STEP 3: Has the sample been processed for Hormone levels? (if there were values, it was assumed that the sample has been processed)
# STEP 4: Do replicate values fall above higher limits of each hormone assay? (Samples > 32 pg/mL were flagged)
# STEP 5: Is the replicate too low for detection? (Samples < 0.1 pg/mL were flagged)
# Add Flag for 0.1 < Est < 32 | Est == NA samples -- (Herting et al., 2021)
AllSalivaHormones$E1 <- if_else(AllSalivaHormones$E2_Rep1 < 0.1 | 
                                  AllSalivaHormones$E2_Rep1 > 32 | 
                                  is.na(AllSalivaHormones$E2_Rep1), 
                                AllSalivaHormones$E1 <- 1, 0)
AllSalivaHormones$E2 <- if_else(AllSalivaHormones$E2_Rep2 < 0.1 | 
                                  AllSalivaHormones$E2_Rep2 > 32 | 
                                  is.na(AllSalivaHormones$E2_Rep2), 
                                AllSalivaHormones$E2 <- 1, 0)

# count number to be reset to NA
AllSalivaHormones$settoNA <- if_else(AllSalivaHormones$E1 == 1 | AllSalivaHormones$E2 == 1, 1, 0)
table(AllSalivaHormones$settoNA)
AllSalivaHormones <- AllSalivaHormones %>%
  select(-settoNA)

# STEPS 4 & 5: If outside Salimetrics test sensitivity limits, set to 0 (Herting et al., 2021) or NA (Chaku & Barry, 2023)
# If E2 is Outside of lower/upper limits, set to NA (Chaku & Barry, 2023)
AllSalivaHormones$E2_Rep1 <- if_else(AllSalivaHormones$E1 == 1, NA, AllSalivaHormones$E2_Rep1)
AllSalivaHormones$E2_Rep2 <- if_else(AllSalivaHormones$E2 == 1, NA, AllSalivaHormones$E2_Rep2)

# Get Single Hormone Values
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
# In following the best practices of Chaku & Barry, further exclusions were made beyond the checks made by Herting et al.
# link to Chaku & Barry, 2023: https://onlinelibrary.wiley.com/doi/full/10.1002/icd.2415
# Figure 1, page 9
########################

# Exclude Mismatches MasterSex and Salivary Sex
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$`Mas!=Sal` == 1, 1, 0)
table(AllSalivaHormones$`Excluded?`) # n = 1090 excluded

### Further Exclusion/Caution Flags (Chaku & Barry, 2023)
## Quality Concerns (QualCon) 
# If sample quality was in doubt (bubbles, discoloration, etc., No Saliva Problems column != 1) 
#     AND there was at least 1 missing or unusable sample (E1 or E2 Flag), QualCon flag was changed from 0 to 1
AllSalivaHormones$QualCon <- if_else((AllSalivaHormones$`No_Sal_Problems?` != 1 & AllSalivaHormones$E1 == 1) | (AllSalivaHormones$`No_Sal_Problems?` != 1 & AllSalivaHormones$E2 == 1), 1, AllSalivaHormones$QualCon)
AllSalivaHormones$QualCon <- if_else(is.na(AllSalivaHormones$QualCon), 0, AllSalivaHormones$QualCon) # set NA values to 0, to maintain counts
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$QualCon == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 1405, 315 excluded

# *** Upon PI Suggestion, keeping all valid-test estradiol outliers ***

## No Estradiol Data (NoEst_Data)
# If the E2mean was NA (meaning there were zero samples in a timepoint), the NoEst_Data flag was changed from 0 to 1. 
AllSalivaHormones$NoEst_Data <- if_else(is.na(AllSalivaHormones$E2mean), 1, 0)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$NoEst_Data == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 2428, 1023 excluded. 

## Missing Birth Control data (females) (BirthConNA)
####### NOTE: When using BirthConNA, only including those with birth control data and endorsing not using birth control, our sample size dropped from thousands to a few dozen. We created this variable flag in the lines below, but in flagging for exclusion due to birth control use (line 401) we chose to assume that no birth control data meant birth control was not being used, and only excluded those who had birth control data and affirmed birth control use. Only keeping timepoint with birth control data would exclude all pre-menarche timepoints, for young women who have no reason to consider birth control yet. ##########
# If Females (by MasterSex) were missing Birth Control data (BirthControlStatus=NA), the BirthConNA flag was changed from 0 to 1
AllSalivaHormones$BirthConNA <- if_else(AllSalivaHormones$MasSex_F == 1 & is.na(AllSalivaHormones$BirthControlStatus), 1, 0)
AllSalivaHormones$BirthControlStatus <- if_else(is.na(AllSalivaHormones$BirthControlStatus), 0, AllSalivaHormones$BirthControlStatus)# set NA Birth Control values to 0 
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$BirthControlStatus == 1, 1, AllSalivaHormones$`Excluded?`) # Chaku & Barry used BirthConNA, not BirthControlStatus. See note above.
table(AllSalivaHormones$`Excluded?`) # n = 2531, 103 excluded

## Missing Caffeine data (CaffNA) 
# If `HadCaffeine?` was NA, or was positive and Caffeine amounts were unreported, CaffNA was changed from 0 to 1
AllSalivaHormones$CaffNA <- if_else((is.na(AllSalivaHormones$`HadCaffeine?`) | (AllSalivaHormones$`HadCaffeine?` == 1 & is.na(AllSalivaHormones$`CaffAmount(mg)`))), 1, 0)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$CaffNA == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 2562, 31 excluded

## Missing Exercise Data (ExerciseNA)
# If `Exercised?` was NA, or was positive and Exercise Length was unreported, ExerciseNA was changed from 0 to 1
AllSalivaHormones$ExerciseNA <- if_else((is.na(AllSalivaHormones$`Exercised?`) | (AllSalivaHormones$`Exercised?` == 1 & is.na(AllSalivaHormones$`ExerciseLength(min)`))), 1, 0)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$ExerciseNA == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 2569, 7 excluded

# Any missing Times
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(no_wake = if_else(is.na(WakeTime), 1, 0),
         no_start = if_else(is.na(StartTime), 1, 0),
         no_end = if_else(is.na(EndTime), 1, 0),
         no_freeze = if_else(is.na(FreezeTime), 1, 0))

# count numbers of missing times
table(AllSalivaHormones$no_wake) # 1114
table(AllSalivaHormones$no_start) # 1087
table(AllSalivaHormones$no_end) # 1105
table(AllSalivaHormones$no_freeze) # 1112

# Flag for exclusion and count
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$no_wake == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 2594, 25 excluded
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$no_start == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 2595, 1 excluded
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$no_end == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 2607, 12 excluded
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$no_freeze == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 2618, 11 excluded

## StartTime prior to WakeTime (S_b4_W)
# If Start Time was reported as before Wake Time, S_b4_W was changed from 0 to 1
AllSalivaHormones$S_b4_W <- if_else(AllSalivaHormones$StartTime < AllSalivaHormones$WakeTime, AllSalivaHormones$S_b4_W <- 1, 0)
AllSalivaHormones$S_b4_W <- if_else(is.na(AllSalivaHormones$S_b4_W), 0, AllSalivaHormones$S_b4_W) # set NA Start before Wake values to 0 (missing start or wake time, accounted for above)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$S_b4_W == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 2650, 32 excluded

## EndTime prior to StartTime (E_b4_S)
# If End Time was reported as before Start Time, E_b4_S was changed from 0 to 1
AllSalivaHormones$E_b4_S <- if_else(AllSalivaHormones$EndTime < AllSalivaHormones$StartTime, AllSalivaHormones$E_b4_S <- 1, 0)
AllSalivaHormones$E_b4_S <- if_else(is.na(AllSalivaHormones$E_b4_S), 0, AllSalivaHormones$E_b4_S) # set NA End before Start values to 0 (missing start or end time, accounted for above)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$E_b4_S == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 2686, 36 excluded

## FreezeTime prior to EndTime (F_b4_E)
# If Freeze Time was reported as before End Time, F_b4_E was changed from 0 to 1
AllSalivaHormones$F_b4_E <- if_else(AllSalivaHormones$FreezeTime < AllSalivaHormones$EndTime, AllSalivaHormones$F_b4_E <- 1, 0)
AllSalivaHormones$F_b4_E <- if_else(is.na(AllSalivaHormones$F_b4_E), 0, AllSalivaHormones$F_b4_E) # set NA Freeze before End values to 0 (missing end or freeze time, accounted for above)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$F_b4_E == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 2830, 144 excluded

#################
# IN THE BELOW CODE BLOCK
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
table(AllSalivaHormones$`Excluded?`) # n = 2902, 72 excluded

## Wake Time after 3pm (WakeAfter3) (Section 3.2.2, Uban et al., 2018)
# If Wake Time was reported as after 3pm, WakeAfter3 was changed from 0 to 1 
AllSalivaHormones$WakeAfter3 <- if_else(AllSalivaHormones$WakeTime > 54000, 1, AllSalivaHormones$WakeAfter3)
AllSalivaHormones$WakeAfter3 <- if_else(is.na(AllSalivaHormones$WakeAfter3), 0, AllSalivaHormones$WakeAfter3) # set NA [Wake Time after 3pm] values to 0 (Missing wake or start times, accounted for above)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$WakeAfter3 == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 2921, 19 excluded

# Sample took too long between End Time and Freeze Time. (FrozeTooSlow) (Salimetrics, page 6)
# "Refrigerate sample within 30 minutes, and freeze at or below -20ºC within 4 hours of collection."
#     Sample took too long between End Time and Freeze Time. 
#     If the difference between EndTime and FreezeTime was > 30 minutes, FrozeTooSlow was changed from 0 to 1
AllSalivaHormones$FrozeTooSlow <- if_else(AllSalivaHormones$EndToFreeze > 1800, 1, AllSalivaHormones$FrozeTooSlow)
AllSalivaHormones$FrozeTooSlow <- if_else(is.na(AllSalivaHormones$FrozeTooSlow), 0, AllSalivaHormones$FrozeTooSlow) # set NA [End time to Freeze Time > 30 minutes] values to 0 (Missing end or freeze times, accounted for above)
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$FrozeTooSlow == 1, 1, AllSalivaHormones$`Excluded?`)
table(AllSalivaHormones$`Excluded?`) # n = 3117, 196 excluded

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
AllSalivaHormones$`Excluded?` <- if_else(AllSalivaHormones$`Mas!=Sal` == 1 |
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

# Aim: 3117
table(AllSalivaHormones$`Excluded?`)
# Total: 3117 excluded, 14807 usable, 17924 total

# Sanity check for single exclusions
AllSalivaHormones <- AllSalivaHormones %>%
  mutate(NumExclusions = rowSums(across(c(`Mas!=Sal`, QualCon, NoEst_Data, BirthControlStatus, 
                                          CaffNA, ExerciseNA, S_b4_W, E_b4_S, F_b4_E, 
                                          S2Egr30, WakeAfter3, FrozeTooSlow), ~ . == 1), na.rm = TRUE))

SingleExclusions <- AllSalivaHormones %>%
  filter(NumExclusions == 1)

#######################
# IN THE BELOW CODE BLOCK
# Write Exclusion Dataframe to csv
########################

# Write to .csv
write_csv(AllSalivaHormones, paste0(derivative_data, "E2_Exclusions_5.0_10_6_25.csv"))
