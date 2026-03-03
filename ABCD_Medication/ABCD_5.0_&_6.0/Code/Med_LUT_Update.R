#########################
# Med_LUT_Update.R
# Author: Robert Toms
# Date: 5/9/2025
#########################

# import libraries and data
library(tidyverse)

LUT <- read_csv('/path/to/CSVs/ABCD_rx_LUT.csv')
Meds <- read_csv('/path/to/CSVs/ph_p_meds.csv')

#######################
# IN THE BELOW CODE BLOCK
# Make Relevant Columns
#######################

# set all O/H's to 0
LUT <- LUT %>%
  mutate(OralContraception_Y1N0 = 0) %>%
  mutate(HormonalSuspension_Y1N0 = 0)

####################################
# IN BELOW CODE BLOCK
# Get Consolidated list of unique medications from Meds
####################################

# Kept Medication name columns, absent columns had no data
MedsSimple <- Meds %>%
  select(src_subject_id,
         eventname,
         med1_rxnorm_p,
         med2_rxnorm_p,
         med3_rxnorm_p,
         med4_rxnorm_p,
         med5_rxnorm_p,
         med1_rxnorm_1yr_p,
         med2_rxnorm_1yr_p,
         med3_rxnorm_1yr_p,
         med4_rxnorm_1yr_p,
         med5_rxnorm_1yr_p,
         med6_rxnorm_1yr_p,
         med7_rxnorm_1yr_p,
         med_otc_1_rxnorm_p,
         med_otc_2_rxnorm_p,
         med_otc_3_rxnorm_p,
         med_otc_4_rxnorm_p,
         med_otc_5_rxnorm_p,
         med_otc_6_rxnorm_p,
         med_otc_7_rxnorm_p,
         med_otc_8_rxnorm_p,
         med_otc_9_rxnorm_p,
         med_otc_1_rxnorm_1yr_p,
         med_otc_2_rxnorm_1yr_p,
         med_otc_3_rxnorm_1yr_p,
         med_otc_4_rxnorm_1yr_p,
         med_otc_5_rxnorm_1yr_p,
         med_otc_6_rxnorm_1yr_p,
         med_otc_7_rxnorm_1yr_p)
#####

# Collapse Medication columns into 1 column

MedsList <- MedsSimple %>%
  pivot_longer(cols = starts_with("med"),
               names_to = "Medications")

MedsList <- MedsList %>%
  filter(!is.na(value))

JustMeds <- MedsList %>%
  select(value) %>%
  unique()

# JustMeds is a list of all the medications in ph_p_meds

##########################
# IN THE BELOW CODE BLOCK
# Add new medications list to LUT
# Isolate medications to manually categorize
##########################

JustMeds <- JustMeds %>%
  rename("Medication Code and Name" = 'value')
  

FullMeds <- LUT %>%
  full_join(JustMeds, by = 'Medication Code and Name')

# Remove medications positive in other columns
ToCheck <- FullMeds %>%
  mutate(across(where(is.numeric), ~ if_else(is.na(.x), 0, .x)),
         across(where(is.character), ~ if_else(is.na(.x), "0", .x))) %>%
  filter(Antiepilepctic_Y1N0 == 0,
         Mood_Stabilizer_Y1N0 == 0,
         Antipsychotics_Y1N0 == 0,
         Stimulants_Y1NN0 == 0,
         AntiDepressants_Y1N0 == 0,
         SSRI_Y1N0 == 0,
         MAOI_Y1N0 == 0,
         Sedative_Y1N0 == 0,
         Melatonin_Y1N0 == 0,
         Allergy_Y1N0 == 0,
         PainKiller_Y1N0 == 0,
         ColdFluMedication_Y1N0 == 0,
         Inhaler_Y1N0 ==0,
         Steriod_Y1N0 ==0)

ToCheck <- ToCheck %>%
  select(`Medication Code and Name`,
         `Other Explanation`,
         OralContraception_Y1N0,
         HormonalSuspension_Y1N0,
         OtherConcerningRx_Y1N0) %>%
  mutate(`Other Explanation` = if_else(`Other Explanation` == "0", "", `Other Explanation`)) %>%
  arrange(`Medication Code and Name`) %>%
  arrange(`Other Explanation`)

# write csv to fill in manually
# write_csv(ToCheck, '/path/to/CSVs/MedsToCheck.csv')

##########################
# IN THE BELOW CODE BLOCK
# Med uses have been filled in manually
# Isolate remaining medications for manual categorization
##########################

# import filled in csv
FilledIn <- read_csv('/path/to/MedsFilledIn.csv')

# separate medication code in MedsList
MedsSplit <- MedsList %>%
  separate(`value`, into = c('Med Code', 
                             'Med Name1', 'Med Name2', 'Med Name3', 
                             'Med Name4', 'Med Name5', 'Med Name6', 
                             'Med Name7', 'Med Name8', 'Med Name9', 
                             'Med Name10', 'Med Name11', 'Med Name12', 
                             'Med Name13', 'Med Name14', 'Med Name15', 
                             'Med Name16', 'Med Name17', 'Med Name18', 
                             'Med Name19', 'Med Name20', 'Med Name21',
                             'Med Name22', 'Med Name23', 'Med Name24', 
                             'Med Name25', 'Med Name26', 'Med Name27', 
                             'Med Name28', 'Med Name29', 'Med Name30',
                             'Med Name31', 'Med Name32', 'Med Name33', 
                             'Med Name34', 'Med Name35', 'Med Name36', 
                             'Med Name37', 'Med Name38', 'Med Name39',
                             'Med Name40', 'Med Name41', 'Med Name42', 
                             'Med Name43', 'Med Name44', 'Med Name45', 
                             'Med Name46', 'Med Name47', 'Med Name48', 
                             'Med Name49', 'Med Name50', 'Med Name51',
                             'Med Name52', 'Med Name53', 'Med Name54', 
                             'Med Name55', 'Med Name56', 'Med Name57', 
                             'Med Name58', 'Med Name59', 'Med Name60'), sep = " ")

MedsMerged <- MedsSplit %>%
  unite('Med Name',
        c('Med Name1', 'Med Name2', 'Med Name3', 
          'Med Name4', 'Med Name5', 'Med Name6', 
          'Med Name7', 'Med Name8', 'Med Name9', 
          'Med Name10', 'Med Name11', 'Med Name12', 
          'Med Name13', 'Med Name14', 'Med Name15', 
          'Med Name16', 'Med Name17', 'Med Name18', 
          'Med Name19', 'Med Name20', 'Med Name21',
          'Med Name22', 'Med Name23', 'Med Name24', 
          'Med Name25', 'Med Name26', 'Med Name27', 
          'Med Name28', 'Med Name29', 'Med Name30',
          'Med Name31', 'Med Name32', 'Med Name33', 
          'Med Name34', 'Med Name35', 'Med Name36', 
          'Med Name37', 'Med Name38', 'Med Name39',
          'Med Name40', 'Med Name41', 'Med Name42', 
          'Med Name43', 'Med Name44', 'Med Name45', 
          'Med Name46', 'Med Name47', 'Med Name48', 
          'Med Name49', 'Med Name50', 'Med Name51',
          'Med Name52', 'Med Name53', 'Med Name54', 
          'Med Name55', 'Med Name56', 'Med Name57', 
          'Med Name58', 'Med Name59', 'Med Name60'), sep = " ", na.rm = TRUE)


MedsInfo <- MedsMerged %>%
  full_join(FilledIn, by = 'Med Code')

Stragglers <- MedsInfo %>%
  select(`Med Code`, `Med Name.x`, `Other Explanation`, OralContraception_Y1N0, HormonalSuspension_Y1N0, Antipsychotics_Y1N0, OtherConcerningRx_Y1N0)

Stragglers <- Stragglers %>%
  filter(is.na(`Other Explanation`)) %>%
  unique() %>%
  filter(`Med Code` != `Med Name.x`) %>%
  arrange(`Med Name.x`)

# Still 761 unlabelled medications, write to csv to fill in manually
#write_csv(Stragglers, '/path/to/Stragglers.csv')

# read in filled in Stragglers
StragglersFilled <- read_csv('/path/to/StragglersFilled.csv')

###########################
# IN THE BELOW CODE BLOCK
# Integrate Stragglers
# Join Med information/flags to the Subject/timepoint info
###########################

# Prep/Clean data and merge
MedsInfo <- MedsInfo %>%
  select(-`Med Name.y`) %>%
  rename('Med Name' = 'Med Name.x')

MedsInfo <- MedsInfo %>%
  select(-'...4')

StragglersFilled <- StragglersFilled %>%
  rename('Med Name' = 'Med Name.x')

MedsInfoFilled <- MedsInfo %>%
  left_join(StragglersFilled, by = c('Med Name'))

MedsInfoFilled <- MedsInfoFilled %>%
  unite('Other Explanation', c(`Other Explanation.x`, `Other Explanation.y`)) %>%
  unite('OralContraception_Y1N0', c(OralContraception_Y1N0.x, OralContraception_Y1N0.y)) %>%
  unite('HormonalSuspension_Y1N0', c(HormonalSuspension_Y1N0.x, HormonalSuspension_Y1N0.y)) %>%
  unite('Antipsychotics_Y1N0', c(Antipsychotics_Y1N0.x, Antipsychotics_Y1N0.y)) %>%
  unite('OtherConcerningRx_Y1N0', c(OtherConcerningRx_Y1N0.x, OtherConcerningRx_Y1N0.y))

# Clean post-merge
MedsInfoFilled <- MedsInfoFilled %>%
  filter(`Other Explanation` != 'NA_NA')
  
MedsInfoFilled <- MedsInfoFilled %>%
  mutate(across(c(OralContraception_Y1N0, HormonalSuspension_Y1N0, Antipsychotics_Y1N0, OtherConcerningRx_Y1N0), 
                ~ str_replace_all(as.character(.), 'NA_NA', ""))) %>%
  mutate(across(c(OralContraception_Y1N0, HormonalSuspension_Y1N0, Antipsychotics_Y1N0, OtherConcerningRx_Y1N0, `Other Explanation`), 
                ~ str_replace_all(as.character(.), 'NA_', ""))) %>%
  mutate(across(c(OralContraception_Y1N0, HormonalSuspension_Y1N0, Antipsychotics_Y1N0, OtherConcerningRx_Y1N0, `Other Explanation`), 
                ~ str_replace_all(as.character(.), '_NA', "")))

MedsInfoFilled <- MedsInfoFilled %>%
  mutate(across(c(OralContraception_Y1N0, HormonalSuspension_Y1N0, Antipsychotics_Y1N0, OtherConcerningRx_Y1N0), 
                ~ if_else((.) != '1', '0', .)))

MedsInfoFilled <- MedsInfoFilled %>%
  select(-Medications, -`Med Code.y`) %>%
  rename('Med Code' = 'Med Code.x')

###########################
# IN THE BELOW CODE BLOCK
# Merge into LUT format
###########################

# separate medication code in MedsList
LUT <- LUT %>%
  separate(`Medication Code and Name`, into = c('Med Code', 
                             'Med Name1', 'Med Name2', 'Med Name3', 
                             'Med Name4', 'Med Name5', 'Med Name6', 
                             'Med Name7', 'Med Name8', 'Med Name9', 
                             'Med Name10', 'Med Name11', 'Med Name12', 
                             'Med Name13', 'Med Name14', 'Med Name15', 
                             'Med Name16', 'Med Name17', 'Med Name18', 
                             'Med Name19', 'Med Name20', 'Med Name21',
                             'Med Name22', 'Med Name23', 'Med Name24', 
                             'Med Name25', 'Med Name26', 'Med Name27', 
                             'Med Name28', 'Med Name29', 'Med Name30',
                             'Med Name31', 'Med Name32', 'Med Name33', 
                             'Med Name34', 'Med Name35', 'Med Name36', 
                             'Med Name37', 'Med Name38', 'Med Name39',
                             'Med Name40', 'Med Name41', 'Med Name42', 
                             'Med Name43', 'Med Name44', 'Med Name45', 
                             'Med Name46', 'Med Name47', 'Med Name48', 
                             'Med Name49', 'Med Name50', 'Med Name51',
                             'Med Name52', 'Med Name53', 'Med Name54', 
                             'Med Name55', 'Med Name56', 'Med Name57', 
                             'Med Name58', 'Med Name59', 'Med Name60'), sep = " ")

LUT <- LUT %>%
  unite('Med Name',
        c('Med Name1', 'Med Name2', 'Med Name3', 
          'Med Name4', 'Med Name5', 'Med Name6', 
          'Med Name7', 'Med Name8', 'Med Name9', 
          'Med Name10', 'Med Name11', 'Med Name12', 
          'Med Name13', 'Med Name14', 'Med Name15', 
          'Med Name16', 'Med Name17', 'Med Name18', 
          'Med Name19', 'Med Name20', 'Med Name21',
          'Med Name22', 'Med Name23', 'Med Name24', 
          'Med Name25', 'Med Name26', 'Med Name27', 
          'Med Name28', 'Med Name29', 'Med Name30',
          'Med Name31', 'Med Name32', 'Med Name33', 
          'Med Name34', 'Med Name35', 'Med Name36', 
          'Med Name37', 'Med Name38', 'Med Name39',
          'Med Name40', 'Med Name41', 'Med Name42', 
          'Med Name43', 'Med Name44', 'Med Name45', 
          'Med Name46', 'Med Name47', 'Med Name48', 
          'Med Name49', 'Med Name50', 'Med Name51',
          'Med Name52', 'Med Name53', 'Med Name54', 
          'Med Name55', 'Med Name56', 'Med Name57', 
          'Med Name58', 'Med Name59', 'Med Name60'), sep = " ", na.rm = TRUE)

LUT <- LUT %>%
  select(-HormonalSuspension_Y1N0,
         -OralContraception_Y1N0, 
         -OtherConcerningRx_Y1N0,
         -'...20',
         -'...21',
         -'...22')

Full_LUT <- MedsInfoFilled %>%
  left_join(LUT, by = c('Med Code', 'Med Name'))

Full_LUT <- Full_LUT %>%
  select(-`Other Explanation.y`) %>%
  rename('Other Explanation' = 'Other Explanation.x')

Full_LUT <- Full_LUT %>%
  rename('Antipsychotics_Y1N0' = Antipsychotics_Y1N0.x) %>%
  rename('Antiepileptic_Y1N0' = Antiepilepctic_Y1N0)

# remove Antipsychotics.y
Full_LUT <- Full_LUT %>%
  select(-Antipsychotics_Y1N0.y)

##############################
# IN THE BELOW CODE BLOCK
# Update Category flags using key words
##############################


Full_LUT <- Full_LUT %>%
  # contraceptive
  mutate(OralContraception_Y1N0 = if_else(str_detect(`Other Explanation`, "ontraceptive"), 1, OralContraception_Y1N0)) %>%
  # HormonalSuspension
  mutate(HormonalSuspension_Y1N0 = if_else(str_detect(`Other Explanation`, "Cyproterone") |
                                             str_detect(`Other Explanation`, "androgen")  , 1, HormonalSuspension_Y1N0)) %>%
  # antiepileptic
  mutate(Antiepileptic_Y1N0 = if_else(str_detect(`Other Explanation`, "ntiepileptic"), 1, Antiepileptic_Y1N0)) %>%
  # mood stabilizer
  mutate(Mood_Stabilizer_Y1N0 = if_else(str_detect(`Other Explanation`, "ood-stabilizer") |
                                          str_detect(`Other Explanation`, "ood stabilizer") |
                                          str_detect(`Other Explanation`, "SNRI"), 1, Mood_Stabilizer_Y1N0)) %>%
  # stimulants
  mutate(Stimulants_Y1NN0 = if_else(str_detect(`Other Explanation`, "timulant"), 1, Stimulants_Y1NN0)) %>%
  # antidepressants
  mutate(AntiDepressants_Y1N0 = if_else(str_detect(`Other Explanation`, "ntidepressant") |
                                          str_detect(`Other Explanation`, "SSRI") |
                                          str_detect(`Other Explanation`, "SNRI"), 1, AntiDepressants_Y1N0)) %>%
  # SSRI
  mutate(SSRI_Y1N0 = if_else(str_detect(`Other Explanation`, "SSRI"), 1, SSRI_Y1N0)) %>%
  # sedative
  mutate(Sedative_Y1N0 = if_else(str_detect(`Other Explanation`, "sedative") |
                                          str_detect(`Other Explanation`, "SARI") |
                                          str_detect(`Other Explanation`, "opioid"), 1, Sedative_Y1N0)) %>%
  # Melatonin
  mutate(Melatonin_Y1N0 = if_else(str_detect(`Other Explanation`, "elatonin"), 1, Melatonin_Y1N0)) %>%
  # allergy
  mutate(Allergy_Y1N0 = if_else(str_detect(`Other Explanation`, "llergy") |
                                  str_detect(`Other Explanation`, "ntihistamine")|
                                  str_detect(`Other Explanation`, "expectorant") |
                                  str_detect(`Other Explanation`, "decongestant"), 1, Allergy_Y1N0)) %>%
  # painkiller
  mutate(PainKiller_Y1N0 = if_else(str_detect(`Other Explanation`, "nalgesic") |
                                   str_detect(`Other Explanation`, "NSAID") |
                                   str_detect(`Other Explanation`, "opioid"), 1, PainKiller_Y1N0)) %>%
  # ColdFlu
  mutate(ColdFluMedication_Y1N0 = if_else(str_detect(`Other Explanation`, "ColdFlu") |
                                  str_detect(`Other Explanation`, "expectorant") |
                                  str_detect(`Other Explanation`, "decongestant") |
                                  str_detect(`Other Explanation`, "antitussive") |
                                  str_detect(`Other Explanation`, "cough suppressant"), 1, ColdFluMedication_Y1N0)) %>%
  # Inhaler
  mutate(Inhaler_Y1N0 = if_else(str_detect(`Other Explanation`, "inhaler") |
                                  str_detect(`Other Explanation`, "bronchodilator"), 1, Inhaler_Y1N0)) %>%
  # Steroid
  mutate(Steriod_Y1N0 = if_else(str_detect(`Other Explanation`, "steroid"), 1, Steriod_Y1N0))

# Clean
# Set all NA's to 0
Full_LUT <- Full_LUT %>%
  mutate(across(c(OralContraception_Y1N0, HormonalSuspension_Y1N0, Antipsychotics_Y1N0, OtherConcerningRx_Y1N0,
                  Antiepileptic_Y1N0, Mood_Stabilizer_Y1N0, Stimulants_Y1NN0, AntiDepressants_Y1N0,
                  SSRI_Y1N0, MAOI_Y1N0, Sedative_Y1N0, Melatonin_Y1N0, Allergy_Y1N0,
                  PainKiller_Y1N0, ColdFluMedication_Y1N0, Inhaler_Y1N0, Steriod_Y1N0), 
                ~ if_else(is.na(.), 0, as.numeric(.))))

Full_LUT <- Full_LUT %>%
  rename('Contraception_Y1N0' = OralContraception_Y1N0)

# Full_LUT has Subject/Timepoint specific medications and medication types.

##########################
# IN THE BELOW CODE BLOCK
# Summarize for Subject/Timepoint and medication type
##########################

# Count relevant medications per timepoint
SummarizedLUT <- Full_LUT %>%
  group_by(src_subject_id, eventname) %>%
  summarise('BC/HormSus/AntiPsych Count' = sum(c(Contraception_Y1N0, HormonalSuspension_Y1N0, Antipsychotics_Y1N0)))


# write to csv
write_csv(Full_LUT, '/path/to/Full_Meds_LUT.csv')
write_csv(SummarizedLUT, '/path/to/Number_Relevant_Meds_LUT.csv')
