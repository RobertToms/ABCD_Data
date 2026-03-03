#########################
# Obstetric_Complications_5.0.R
# Author: Robert Toms
# Date: 10/9/2025
#########################

# The purpose of this code is to flag each participant with psychosis-associated Obstetric Complications,
# based on the categories from this article: https://academic.oup.com/schizophreniabulletin/article/34/6/1083/1939509#29826173.

library(tidyverse)

sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"

# import data
OCs <- read_csv(paste0(datapath, 'physical-health/ph_p_dhx.csv'))
Meds <- read_csv(paste0(derivative_data, 'Full_Meds_LUT.csv'))

########################
# IN THE BELOW CODE BLOCK
# Isolate relevant variables
########################

## Isolate relevant variables
# OCs
OCs <- OCs %>%
  select(src_subject_id, eventname,
         # Associated with Hypoxia
         devhx_13_3_p,   # Was your child born by Caesarian section?
         devhx_10b3_p,   # Bleeding during pregnancy
         devhx_10c3_p,   # Pre-eclampsia, eclampsia, or toxemia?
         devhx_10c3_a_p, # Pre-Eclampsia
         devhx_14a3_p,   # Blue at Birth
         devhx_14c3_p,   # Required Resuscitation/Did not breathe at first? 
         devhx_14f3_p,   # Neonatal Apnea/Required Oxygen?
         # Maternal Stress Exposure
         devhx_6_p,      # Unwanted Pregnancy/Was your pregnancy with this child a planned pregnancy?
         # Prenatal Maternal Infection
         devhx_10h3_p, # Genital Tract Infection/UTI?
         devhx_10f3_p, # Rubella?
         # Upper Respiratory Infections, Influenza, Toxoplasmosis, Herpes type 2
         devhx_10m3_p, # Any other conditions requiring medical care? 
         # Medications during pregnancy
         devhx_8_rxnorm_med1, # Before knowing about pregnancy
         devhx_8_rxnorm_med2,
         devhx_8_rxnorm_med3,
         devhx_9_med1_rxnorm, # After Parent(s) found out about pregnancy
         devhx_9_med2_rxnorm,
         devhx_9_med3_rxnorm)

# Meds
Meds <- Meds %>%
  select(-src_subject_id, -eventname)

##########################
# IN THE BELOW CODE BLOCK
# Get full list of Parental Medications
##########################

## Separate Med Codes from Med Names
# Med 1
OCs <- OCs %>%
  separate(`devhx_8_rxnorm_med1`, into = c('Med_Code1', 
                                           'Med Name1', 'Med Name2', 'Med Name3', 'Med Name4', 'Med Name5', 
                                           'Med Name6', 'Med Name7', 'Med Name8', 'Med Name9', 'Med Name10', 
                                           'Med Name11', 'Med Name12', 'Med Name13', 'Med Name14', 'Med Name15', 
                                           'Med Name16', 'Med Name17', 'Med Name18', 'Med Name19', 'Med Name20', 
                                           'Med Name21', 'Med Name22', 'Med Name23', 'Med Name24', 'Med Name25', 
                                           'Med Name26', 'Med Name27', 'Med Name28', 'Med Name29', 'Med Name30',
                                           'Med Name31', 'Med Name32', 'Med Name33', 'Med Name34', 'Med Name35', 
                                           'Med Name36', 'Med Name37', 'Med Name38', 'Med Name39', 'Med Name40', 
                                           'Med Name41', 'Med Name42', 'Med Name43', 'Med Name44', 'Med Name45', 
                                           'Med Name46', 'Med Name47', 'Med Name48', 'Med Name49', 'Med Name50', 
                                           'Med Name51', 'Med Name52', 'Med Name53', 'Med Name54', 'Med Name55', 
                                           'Med Name56', 'Med Name57', 'Med Name58', 'Med Name59', 'Med Name60'), sep = " ")
OCs <- OCs %>%
  unite('Med_Name1',
        c('Med Name1', 'Med Name2', 'Med Name3', 'Med Name4', 'Med Name5', 
          'Med Name6', 'Med Name7', 'Med Name8', 'Med Name9', 'Med Name10', 
          'Med Name11', 'Med Name12', 'Med Name13', 'Med Name14', 'Med Name15', 
          'Med Name16', 'Med Name17', 'Med Name18', 'Med Name19', 'Med Name20', 
          'Med Name21', 'Med Name22', 'Med Name23', 'Med Name24', 'Med Name25', 
          'Med Name26', 'Med Name27', 'Med Name28', 'Med Name29', 'Med Name30',
          'Med Name31', 'Med Name32', 'Med Name33', 'Med Name34', 'Med Name35', 
          'Med Name36', 'Med Name37', 'Med Name38', 'Med Name39', 'Med Name40', 
          'Med Name41', 'Med Name42', 'Med Name43', 'Med Name44', 'Med Name45', 
          'Med Name46', 'Med Name47', 'Med Name48', 'Med Name49', 'Med Name50', 
          'Med Name51', 'Med Name52', 'Med Name53', 'Med Name54', 'Med Name55', 
          'Med Name56', 'Med Name57', 'Med Name58', 'Med Name59', 'Med Name60'), sep = " ", na.rm = TRUE)
# Med 2
OCs <- OCs %>%
  separate(`devhx_8_rxnorm_med2`, into = c('Med_Code2', 
                                           'Med Name1', 'Med Name2', 'Med Name3', 'Med Name4', 'Med Name5', 
                                           'Med Name6', 'Med Name7', 'Med Name8', 'Med Name9', 'Med Name10', 
                                           'Med Name11', 'Med Name12', 'Med Name13', 'Med Name14', 'Med Name15', 
                                           'Med Name16', 'Med Name17', 'Med Name18', 'Med Name19', 'Med Name20', 
                                           'Med Name21', 'Med Name22', 'Med Name23', 'Med Name24', 'Med Name25', 
                                           'Med Name26', 'Med Name27', 'Med Name28', 'Med Name29', 'Med Name30',
                                           'Med Name31', 'Med Name32', 'Med Name33', 'Med Name34', 'Med Name35', 
                                           'Med Name36', 'Med Name37', 'Med Name38', 'Med Name39', 'Med Name40', 
                                           'Med Name41', 'Med Name42', 'Med Name43', 'Med Name44', 'Med Name45', 
                                           'Med Name46', 'Med Name47', 'Med Name48', 'Med Name49', 'Med Name50', 
                                           'Med Name51', 'Med Name52', 'Med Name53', 'Med Name54', 'Med Name55', 
                                           'Med Name56', 'Med Name57', 'Med Name58', 'Med Name59', 'Med Name60'), sep = " ")
OCs <- OCs %>%
  unite('Med_Name2',
        c('Med Name1', 'Med Name2', 'Med Name3', 'Med Name4', 'Med Name5', 
          'Med Name6', 'Med Name7', 'Med Name8', 'Med Name9', 'Med Name10', 
          'Med Name11', 'Med Name12', 'Med Name13', 'Med Name14', 'Med Name15', 
          'Med Name16', 'Med Name17', 'Med Name18', 'Med Name19', 'Med Name20', 
          'Med Name21', 'Med Name22', 'Med Name23', 'Med Name24', 'Med Name25', 
          'Med Name26', 'Med Name27', 'Med Name28', 'Med Name29', 'Med Name30',
          'Med Name31', 'Med Name32', 'Med Name33', 'Med Name34', 'Med Name35', 
          'Med Name36', 'Med Name37', 'Med Name38', 'Med Name39', 'Med Name40', 
          'Med Name41', 'Med Name42', 'Med Name43', 'Med Name44', 'Med Name45', 
          'Med Name46', 'Med Name47', 'Med Name48', 'Med Name49', 'Med Name50', 
          'Med Name51', 'Med Name52', 'Med Name53', 'Med Name54', 'Med Name55', 
          'Med Name56', 'Med Name57', 'Med Name58', 'Med Name59', 'Med Name60'), sep = " ", na.rm = TRUE)
# Med 3
OCs <- OCs %>%
  separate(`devhx_8_rxnorm_med3`, into = c('Med_Code3', 
                                           'Med Name1', 'Med Name2', 'Med Name3', 'Med Name4', 'Med Name5', 
                                           'Med Name6', 'Med Name7', 'Med Name8', 'Med Name9', 'Med Name10', 
                                           'Med Name11', 'Med Name12', 'Med Name13', 'Med Name14', 'Med Name15', 
                                           'Med Name16', 'Med Name17', 'Med Name18', 'Med Name19', 'Med Name20', 
                                           'Med Name21', 'Med Name22', 'Med Name23', 'Med Name24', 'Med Name25', 
                                           'Med Name26', 'Med Name27', 'Med Name28', 'Med Name29', 'Med Name30',
                                           'Med Name31', 'Med Name32', 'Med Name33', 'Med Name34', 'Med Name35', 
                                           'Med Name36', 'Med Name37', 'Med Name38', 'Med Name39', 'Med Name40', 
                                           'Med Name41', 'Med Name42', 'Med Name43', 'Med Name44', 'Med Name45', 
                                           'Med Name46', 'Med Name47', 'Med Name48', 'Med Name49', 'Med Name50', 
                                           'Med Name51', 'Med Name52', 'Med Name53', 'Med Name54', 'Med Name55', 
                                           'Med Name56', 'Med Name57', 'Med Name58', 'Med Name59', 'Med Name60'), sep = " ")
OCs <- OCs %>%
  unite('Med_Name3',
        c('Med Name1', 'Med Name2', 'Med Name3', 'Med Name4', 'Med Name5', 
          'Med Name6', 'Med Name7', 'Med Name8', 'Med Name9', 'Med Name10', 
          'Med Name11', 'Med Name12', 'Med Name13', 'Med Name14', 'Med Name15', 
          'Med Name16', 'Med Name17', 'Med Name18', 'Med Name19', 'Med Name20', 
          'Med Name21', 'Med Name22', 'Med Name23', 'Med Name24', 'Med Name25', 
          'Med Name26', 'Med Name27', 'Med Name28', 'Med Name29', 'Med Name30',
          'Med Name31', 'Med Name32', 'Med Name33', 'Med Name34', 'Med Name35', 
          'Med Name36', 'Med Name37', 'Med Name38', 'Med Name39', 'Med Name40', 
          'Med Name41', 'Med Name42', 'Med Name43', 'Med Name44', 'Med Name45', 
          'Med Name46', 'Med Name47', 'Med Name48', 'Med Name49', 'Med Name50', 
          'Med Name51', 'Med Name52', 'Med Name53', 'Med Name54', 'Med Name55', 
          'Med Name56', 'Med Name57', 'Med Name58', 'Med Name59', 'Med Name60'), sep = " ", na.rm = TRUE)

# Med 4
OCs <- OCs %>%
  separate(`devhx_9_med1_rxnorm`, into = c('Med_Code4', 
                                           'Med Name1', 'Med Name2', 'Med Name3', 'Med Name4', 'Med Name5', 
                                           'Med Name6', 'Med Name7', 'Med Name8', 'Med Name9', 'Med Name10', 
                                           'Med Name11', 'Med Name12', 'Med Name13', 'Med Name14', 'Med Name15', 
                                           'Med Name16', 'Med Name17', 'Med Name18', 'Med Name19', 'Med Name20', 
                                           'Med Name21', 'Med Name22', 'Med Name23', 'Med Name24', 'Med Name25', 
                                           'Med Name26', 'Med Name27', 'Med Name28', 'Med Name29', 'Med Name30',
                                           'Med Name31', 'Med Name32', 'Med Name33', 'Med Name34', 'Med Name35', 
                                           'Med Name36', 'Med Name37', 'Med Name38', 'Med Name39', 'Med Name40', 
                                           'Med Name41', 'Med Name42', 'Med Name43', 'Med Name44', 'Med Name45', 
                                           'Med Name46', 'Med Name47', 'Med Name48', 'Med Name49', 'Med Name50', 
                                           'Med Name51', 'Med Name52', 'Med Name53', 'Med Name54', 'Med Name55', 
                                           'Med Name56', 'Med Name57', 'Med Name58', 'Med Name59', 'Med Name60'), sep = " ")
OCs <- OCs %>%
  unite('Med_Name4',
        c('Med Name1', 'Med Name2', 'Med Name3', 'Med Name4', 'Med Name5', 
          'Med Name6', 'Med Name7', 'Med Name8', 'Med Name9', 'Med Name10', 
          'Med Name11', 'Med Name12', 'Med Name13', 'Med Name14', 'Med Name15', 
          'Med Name16', 'Med Name17', 'Med Name18', 'Med Name19', 'Med Name20', 
          'Med Name21', 'Med Name22', 'Med Name23', 'Med Name24', 'Med Name25', 
          'Med Name26', 'Med Name27', 'Med Name28', 'Med Name29', 'Med Name30',
          'Med Name31', 'Med Name32', 'Med Name33', 'Med Name34', 'Med Name35', 
          'Med Name36', 'Med Name37', 'Med Name38', 'Med Name39', 'Med Name40', 
          'Med Name41', 'Med Name42', 'Med Name43', 'Med Name44', 'Med Name45', 
          'Med Name46', 'Med Name47', 'Med Name48', 'Med Name49', 'Med Name50', 
          'Med Name51', 'Med Name52', 'Med Name53', 'Med Name54', 'Med Name55', 
          'Med Name56', 'Med Name57', 'Med Name58', 'Med Name59', 'Med Name60'), sep = " ", na.rm = TRUE)
# Med 5
OCs <- OCs %>%
  separate(`devhx_9_med2_rxnorm`, into = c('Med_Code5', 
                                           'Med Name1', 'Med Name2', 'Med Name3', 'Med Name4', 'Med Name5', 
                                           'Med Name6', 'Med Name7', 'Med Name8', 'Med Name9', 'Med Name10', 
                                           'Med Name11', 'Med Name12', 'Med Name13', 'Med Name14', 'Med Name15', 
                                           'Med Name16', 'Med Name17', 'Med Name18', 'Med Name19', 'Med Name20', 
                                           'Med Name21', 'Med Name22', 'Med Name23', 'Med Name24', 'Med Name25', 
                                           'Med Name26', 'Med Name27', 'Med Name28', 'Med Name29', 'Med Name30',
                                           'Med Name31', 'Med Name32', 'Med Name33', 'Med Name34', 'Med Name35', 
                                           'Med Name36', 'Med Name37', 'Med Name38', 'Med Name39', 'Med Name40', 
                                           'Med Name41', 'Med Name42', 'Med Name43', 'Med Name44', 'Med Name45', 
                                           'Med Name46', 'Med Name47', 'Med Name48', 'Med Name49', 'Med Name50', 
                                           'Med Name51', 'Med Name52', 'Med Name53', 'Med Name54', 'Med Name55', 
                                           'Med Name56', 'Med Name57', 'Med Name58', 'Med Name59', 'Med Name60'), sep = " ")
OCs <- OCs %>%
  unite('Med_Name5',
        c('Med Name1', 'Med Name2', 'Med Name3', 'Med Name4', 'Med Name5', 
          'Med Name6', 'Med Name7', 'Med Name8', 'Med Name9', 'Med Name10', 
          'Med Name11', 'Med Name12', 'Med Name13', 'Med Name14', 'Med Name15', 
          'Med Name16', 'Med Name17', 'Med Name18', 'Med Name19', 'Med Name20', 
          'Med Name21', 'Med Name22', 'Med Name23', 'Med Name24', 'Med Name25', 
          'Med Name26', 'Med Name27', 'Med Name28', 'Med Name29', 'Med Name30',
          'Med Name31', 'Med Name32', 'Med Name33', 'Med Name34', 'Med Name35', 
          'Med Name36', 'Med Name37', 'Med Name38', 'Med Name39', 'Med Name40', 
          'Med Name41', 'Med Name42', 'Med Name43', 'Med Name44', 'Med Name45', 
          'Med Name46', 'Med Name47', 'Med Name48', 'Med Name49', 'Med Name50', 
          'Med Name51', 'Med Name52', 'Med Name53', 'Med Name54', 'Med Name55', 
          'Med Name56', 'Med Name57', 'Med Name58', 'Med Name59', 'Med Name60'), sep = " ", na.rm = TRUE)
# Med 6
OCs <- OCs %>%
  separate(`devhx_9_med3_rxnorm`, into = c('Med_Code6', 
                                           'Med Name1', 'Med Name2', 'Med Name3', 'Med Name4', 'Med Name5', 
                                           'Med Name6', 'Med Name7', 'Med Name8', 'Med Name9', 'Med Name10', 
                                           'Med Name11', 'Med Name12', 'Med Name13', 'Med Name14', 'Med Name15', 
                                           'Med Name16', 'Med Name17', 'Med Name18', 'Med Name19', 'Med Name20', 
                                           'Med Name21', 'Med Name22', 'Med Name23', 'Med Name24', 'Med Name25', 
                                           'Med Name26', 'Med Name27', 'Med Name28', 'Med Name29', 'Med Name30',
                                           'Med Name31', 'Med Name32', 'Med Name33', 'Med Name34', 'Med Name35', 
                                           'Med Name36', 'Med Name37', 'Med Name38', 'Med Name39', 'Med Name40', 
                                           'Med Name41', 'Med Name42', 'Med Name43', 'Med Name44', 'Med Name45', 
                                           'Med Name46', 'Med Name47', 'Med Name48', 'Med Name49', 'Med Name50', 
                                           'Med Name51', 'Med Name52', 'Med Name53', 'Med Name54', 'Med Name55', 
                                           'Med Name56', 'Med Name57', 'Med Name58', 'Med Name59', 'Med Name60'), sep = " ")
OCs <- OCs %>%
  unite('Med_Name6',
        c('Med Name1', 'Med Name2', 'Med Name3', 'Med Name4', 'Med Name5', 
          'Med Name6', 'Med Name7', 'Med Name8', 'Med Name9', 'Med Name10', 
          'Med Name11', 'Med Name12', 'Med Name13', 'Med Name14', 'Med Name15', 
          'Med Name16', 'Med Name17', 'Med Name18', 'Med Name19', 'Med Name20', 
          'Med Name21', 'Med Name22', 'Med Name23', 'Med Name24', 'Med Name25', 
          'Med Name26', 'Med Name27', 'Med Name28', 'Med Name29', 'Med Name30',
          'Med Name31', 'Med Name32', 'Med Name33', 'Med Name34', 'Med Name35', 
          'Med Name36', 'Med Name37', 'Med Name38', 'Med Name39', 'Med Name40', 
          'Med Name41', 'Med Name42', 'Med Name43', 'Med Name44', 'Med Name45', 
          'Med Name46', 'Med Name47', 'Med Name48', 'Med Name49', 'Med Name50', 
          'Med Name51', 'Med Name52', 'Med Name53', 'Med Name54', 'Med Name55', 
          'Med Name56', 'Med Name57', 'Med Name58', 'Med Name59', 'Med Name60'), sep = " ", na.rm = TRUE)

## Collapse Med list into two columns -- Med Code and Med Name
# Isolate Med Columns
JustMeds <- OCs %>%
  select(src_subject_id, Med_Code1, Med_Name1,
         Med_Code2, Med_Name2,
         Med_Code3, Med_Name3,
         Med_Code4, Med_Name4,
         Med_Code5, Med_Name5,
         Med_Code6, Med_Name6)

Long_Meds <- JustMeds %>%
  select(src_subject_id, Med_Code1, Med_Name1) %>%
  rename(Med_Code = Med_Code1,
         Med_Name = Med_Name1)
Meds2 <- JustMeds %>%
  select(src_subject_id, Med_Code2, Med_Name2) %>%
  rename(Med_Code = Med_Code2,
         Med_Name = Med_Name2)
Meds3 <- JustMeds %>%
  select(src_subject_id, Med_Code3, Med_Name3) %>%
  rename(Med_Code = Med_Code3,
         Med_Name = Med_Name3)
Meds4 <- JustMeds %>%
  select(src_subject_id, Med_Code4, Med_Name4) %>%
  rename(Med_Code = Med_Code4,
         Med_Name = Med_Name4)
Meds5 <- JustMeds %>%
  select(src_subject_id, Med_Code5, Med_Name5) %>%
  rename(Med_Code = Med_Code5,
         Med_Name = Med_Name5)
Meds6 <- JustMeds %>%
  select(src_subject_id, Med_Code6, Med_Name6) %>%
  rename(Med_Code = Med_Code6,
         Med_Name = Med_Name6)
# Unite All meds
Long_Meds <- Long_Meds %>%
  rbind(Meds2) %>%
  rbind(Meds3) %>%
  rbind(Meds4) %>%
  rbind(Meds5) %>%
  rbind(Meds6) %>%
  filter(!is.na(Med_Code))

Meds <- Meds %>%
  rename(Med_Code = `Med Code`) %>%
  unique()

##########################
# IN THE BELOW CODE BLOCK
# Merge medication List with Medication LookUp Table
##########################

Long_Meds_ID <- Long_Meds %>%
  left_join(Meds, by = 'Med_Code')

# systematically add descriptors for common medications
Long_Meds_ID <- Long_Meds_ID %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'rogesterone'), "fertility hormone", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'lomiphene'), "fertility hormone", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'Levothyroxine Sodium'), "hypothyroidism", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'nsulin'), "insulin", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'ellbutrin'), "NDRI (antidepressant, ADHD)", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'Ambien'), "sedative", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'Zofran'), "antiemetic", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'Day Pack'), "contraceptive", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'Acetaminophen'), "painkiller", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'Lovenox'), "anticoagulant", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'Effexor'), "SNRI", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'Celexa'), "SSRI", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'Labetalol'), "beta-blocker", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'Lisinopril'), "ACE-inhibitor", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'Fluoxetine') | 
                                         str_detect(Med_Name, 'Sertraline') |
                                         str_detect(Med_Name, 'Paroxetine'), "SSRI", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'Heparin') | str_detect(Med_Name, 'heparin'), "anticoagulant", `Other Explanation`)) %>%
  mutate(`Other Explanation` = if_else(str_detect(Med_Name, 'Metformin'), "diabetes medication", `Other Explanation`))


##########################
# IN THE BELOW CODE BLOCK
# Separate and Export uncategorized Meds list for manual classification
##########################

To_Fill_In <- Long_Meds_ID %>%
  filter(is.na(`Other Explanation`)) %>%
  filter(Med_Code != Med_Name) %>%
  arrange(Med_Name)

Already_Classified <- Long_Meds_ID %>%
  filter(!is.na(`Other Explanation`)) %>%
  filter(Med_Code != Med_Name)

# write csv for manual fill-in
# write_csv(To_Fill_In, '/path/to/FillIn_Meds.csv')

# read in filled in csv
Filled_In <- read_csv(paste0(derivative_data, 'OC_FilledIn_Meds.csv'))

# Merge Medication classification lists back together
Full_Meds <- Already_Classified %>%
  rbind(Filled_In) %>%
  select(Med_Code, Med_Name, `Other Explanation`) %>%
  unique()
# tie to subjects
Explained_Meds <- Long_Meds %>%
  left_join(Full_Meds, by = c('Med_Code', 'Med_Name')) 

##########################
# IN THE BELOW CODE BLOCK
# Flag Medications if related to: Upper Respiratory Infections, Influenza, Toxoplasmosis, Herpes type 2
##########################

# instantiate column for concerning medication flag
Explained_Meds <- Explained_Meds %>%
  mutate(Antibiotic = 0) %>%
  mutate(Not_Antibiotic = 0) %>%
  mutate(Relevant_Med_Y1N0 = 0)

# Add flags for medications treating bacterial infections, herpes, or specifically the influenza virus
Explained_Meds <- Explained_Meds %>%
  # Just antibiotics
  mutate(Antibiotic = if_else(str_detect(`Other Explanation`, 'ntibiotic'), 1, 0)) %>%
  # herpes or specific antiviral
  mutate(Not_Antibiotic = if_else(str_detect(`Med_Name`, 'Tamiflu') | str_detect(`Med_Name`, 'Acyclovir') | str_detect(`Other Explanation`, 'herpes'), 1, 0)) %>%
  # antibiotics or herpes medications
  mutate(Relevant_Med_Y1N0 = if_else(str_detect(`Other Explanation`, 'ntibiotic') | str_detect(`Other Explanation`, 'herpes'), 1, 0)) %>%
  # specific influenza antiviral and unflagged herpes antiviral
  mutate(Relevant_Med_Y1N0 = if_else(str_detect(`Med_Name`, 'Tamiflu') | str_detect(`Med_Name`, 'Acyclovir'), 1, Relevant_Med_Y1N0))


##########################
# IN THE BELOW CODE BLOCK
# Clean/Rename Columns for clarity/readability
# Cross-check/combine flag data from Year 4 into baseline
##########################

# rename
OCs <- OCs %>%
  rename(         
    # Associated with Hypoxia
    C_Section_Y1N0 = devhx_13_3_p,   # Was your child born by Caesarian section?
    Bleeding_Preg_Y1N0 = devhx_10b3_p,   # Bleeding during pregnancy
    Pe_E_T_Y1N0 = devhx_10c3_p,   # Pre-eclampsia, eclampsia, or toxemia?
    Pre_Eclampsia_Y1N0 = devhx_10c3_a_p, # Pre-Eclampsia
    Blue_at_Birth_Y1N0 = devhx_14a3_p,   # Blue at Birth
    Required_Resusc_Y1N0 = devhx_14c3_p,   # Required Resuscitation/Did not breathe at first? 
    Neonatal_Apnea_Y1N0 = devhx_14f3_p,   # Neonatal Apnea/Required Oxygen?
    # Maternal Stress Exposure
    Planned_Preg_Y1N0 = devhx_6_p,      # Unwanted Pregnancy/Was your pregnancy with this child a planned pregnancy?
    # Prenatal Maternal Infection
    UTI_Y1N0 = devhx_10h3_p, # Genital Tract Infection/UTI?
    Rubella_Y1N0 = devhx_10f3_p, # Rubella?
    # Upper Respiratory Infections, Influenza, Toxoplasmosis, Herpes type 2
    OtherConditions_Y1N0 = devhx_10m3_p) # Any other conditions requiring medical care?

## If flag is endorsed in Year 4, set to positive in Baseline, or else leave it alone.
# C-Section
OCs <- OCs %>%
  group_by(src_subject_id) %>%
  mutate(C_Section_Y1N0 = if_else(any(eventname == "4_year_follow_up_y_arm_1" & C_Section_Y1N0 == 1) & eventname == "baseline_year_1_arm_1", 
                                  1, 
                                  C_Section_Y1N0, missing = C_Section_Y1N0))
#Bleeding Pregnancy
OCs <- OCs %>%
  group_by(src_subject_id) %>%
  mutate(Bleeding_Preg_Y1N0 = if_else(any(eventname == "4_year_follow_up_y_arm_1" & Bleeding_Preg_Y1N0 == 1) & eventname == "baseline_year_1_arm_1", 
                                      1, 
                                      Bleeding_Preg_Y1N0, missing = Bleeding_Preg_Y1N0))
# Pre-Eclampsia, Eclampsia, Toxemia
OCs <- OCs %>%
  group_by(src_subject_id) %>%
  mutate(Pe_E_T_Y1N0 = if_else(any(eventname == "4_year_follow_up_y_arm_1" & Pe_E_T_Y1N0 == 1) & eventname == "baseline_year_1_arm_1", 
                               1, 
                               Pe_E_T_Y1N0, missing = Pe_E_T_Y1N0))
# Pre-Eclampsia
OCs <- OCs %>%
  group_by(src_subject_id) %>%
  mutate(Pre_Eclampsia_Y1N0 = if_else(any(eventname == "4_year_follow_up_y_arm_1" & Pre_Eclampsia_Y1N0 == 1) & eventname == "baseline_year_1_arm_1", 
                                      1, 
                                      Pre_Eclampsia_Y1N0, missing = Pre_Eclampsia_Y1N0))
# Blue at Birth
OCs <- OCs %>%
  group_by(src_subject_id) %>%
  mutate(Blue_at_Birth_Y1N0 = if_else(any(eventname == "4_year_follow_up_y_arm_1" & Blue_at_Birth_Y1N0 == 1) & eventname == "baseline_year_1_arm_1", 
                                      1, 
                                      Blue_at_Birth_Y1N0, missing = Blue_at_Birth_Y1N0))
# Required Resuscitation
OCs <- OCs %>%
  group_by(src_subject_id) %>%
  mutate(Required_Resusc_Y1N0 = if_else(any(eventname == "4_year_follow_up_y_arm_1" & Required_Resusc_Y1N0 == 1) & eventname == "baseline_year_1_arm_1", 
                                        1, 
                                        Required_Resusc_Y1N0, missing = Required_Resusc_Y1N0))
# NeoNatal Apnea
OCs <- OCs %>%
  group_by(src_subject_id) %>%
  mutate(Neonatal_Apnea_Y1N0 = if_else(any(eventname == "4_year_follow_up_y_arm_1" & Neonatal_Apnea_Y1N0 == 1) & eventname == "baseline_year_1_arm_1", 
                                       1, 
                                       Neonatal_Apnea_Y1N0, missing = Neonatal_Apnea_Y1N0))
# Planned Pregnancy
OCs <- OCs %>%
  group_by(src_subject_id) %>%
  mutate(Planned_Preg_Y1N0 = if_else(any(eventname == "4_year_follow_up_y_arm_1" & Planned_Preg_Y1N0 == 1) & eventname == "baseline_year_1_arm_1", 
                                     1, 
                                     Planned_Preg_Y1N0, missing = Planned_Preg_Y1N0)) %>%
  mutate(Planned_Preg_Y1N0 = if_else(any(eventname == "4_year_follow_up_y_arm_1" & Planned_Preg_Y1N0 == 0) & eventname == "baseline_year_1_arm_1", 
                                     0, 
                                     Planned_Preg_Y1N0, missing = Planned_Preg_Y1N0))
# UTI
OCs <- OCs %>%
  group_by(src_subject_id) %>%
  mutate(UTI_Y1N0 = if_else(any(eventname == "4_year_follow_up_y_arm_1" & UTI_Y1N0 == 1) & eventname == "baseline_year_1_arm_1", 
                            1, 
                            UTI_Y1N0, missing = UTI_Y1N0))
# Rubella
OCs <- OCs %>%
  group_by(src_subject_id) %>%
  mutate(Rubella_Y1N0 = if_else(any(eventname == "4_year_follow_up_y_arm_1" & Rubella_Y1N0 == 1) & eventname == "baseline_year_1_arm_1", 
                                1, 
                                Rubella_Y1N0, missing = Rubella_Y1N0))
# Other Conditions requiring medical attention
OCs <- OCs %>%
  group_by(src_subject_id) %>%
  mutate(OtherConditions_Y1N0 = if_else(any(eventname == "4_year_follow_up_y_arm_1" & OtherConditions_Y1N0 == 1) & eventname == "baseline_year_1_arm_1", 
                                        1, 
                                        OtherConditions_Y1N0, missing = OtherConditions_Y1N0))

# Year4 is now redundant, remove it, while sanity checking that they have a baseline counterpart. Then remove eventname.
OCs <- OCs %>%
  group_by(src_subject_id) %>%
  filter(!(eventname == "4_year_follow_up_y_arm_1" & any(eventname == "baseline_year_1_arm_1"))) %>%
  select(-eventname)


##########################
# IN THE BELOW CODE BLOCK
# Merge Medication info/Flags into Full Obstetric Complications data frame
##########################

# merge
All_OCs <- OCs %>%
  left_join(Explained_Meds, by = c("src_subject_id" = "src_subject_id", "Med_Code1" = "Med_Code", "Med_Name1" = "Med_Name")) %>%
  rename(MedDescriptor1 = `Other Explanation`, Relevant_Med_1_Y1N0 = Relevant_Med_Y1N0, Antibiotic1 = Antibiotic, Not_Antibiotic1 = Not_Antibiotic) %>%
  unique() %>%
  left_join(Explained_Meds, by = c("src_subject_id" = "src_subject_id", "Med_Code2" = "Med_Code", "Med_Name2" = "Med_Name")) %>%
  rename(MedDescriptor2 = `Other Explanation`, Relevant_Med_2_Y1N0 = Relevant_Med_Y1N0, Antibiotic2 = Antibiotic, Not_Antibiotic2 = Not_Antibiotic) %>%
  unique() %>%
  left_join(Explained_Meds, by = c("src_subject_id" = "src_subject_id", "Med_Code3" = "Med_Code", "Med_Name3" = "Med_Name")) %>%
  rename(MedDescriptor3 = `Other Explanation`, Relevant_Med_3_Y1N0 = Relevant_Med_Y1N0, Antibiotic3 = Antibiotic, Not_Antibiotic3 = Not_Antibiotic) %>%
  unique() %>%
  left_join(Explained_Meds, by = c("src_subject_id" = "src_subject_id", "Med_Code4" = "Med_Code", "Med_Name4" = "Med_Name")) %>%
  rename(MedDescriptor4 = `Other Explanation`, Relevant_Med_4_Y1N0 = Relevant_Med_Y1N0, Antibiotic4 = Antibiotic, Not_Antibiotic4 = Not_Antibiotic) %>%
  unique() %>%
  left_join(Explained_Meds, by = c("src_subject_id" = "src_subject_id", "Med_Code5" = "Med_Code", "Med_Name5" = "Med_Name")) %>%
  rename(MedDescriptor5 = `Other Explanation`, Relevant_Med_5_Y1N0 = Relevant_Med_Y1N0, Antibiotic5 = Antibiotic, Not_Antibiotic5 = Not_Antibiotic) %>%
  unique() %>%
  left_join(Explained_Meds, by = c("src_subject_id" = "src_subject_id", "Med_Code6" = "Med_Code", "Med_Name6" = "Med_Name")) %>%
  rename(MedDescriptor6 = `Other Explanation`, Relevant_Med_6_Y1N0 = Relevant_Med_Y1N0, Antibiotic6 = Antibiotic, Not_Antibiotic6 = Not_Antibiotic) %>%
  unique()

# reorder meds columns for clarity
All_OCs <- All_OCs %>%
  select(src_subject_id, C_Section_Y1N0, Bleeding_Preg_Y1N0, Pe_E_T_Y1N0, Pre_Eclampsia_Y1N0, Blue_at_Birth_Y1N0,
         Required_Resusc_Y1N0, Neonatal_Apnea_Y1N0, Planned_Preg_Y1N0, UTI_Y1N0, Rubella_Y1N0, OtherConditions_Y1N0,
         Med_Code1, Med_Name1, MedDescriptor1, Antibiotic1, Not_Antibiotic1, Relevant_Med_1_Y1N0,
         Med_Code2, Med_Name2, MedDescriptor2, Antibiotic2, Not_Antibiotic2, Relevant_Med_2_Y1N0,
         Med_Code3, Med_Name3, MedDescriptor3, Antibiotic3, Not_Antibiotic3, Relevant_Med_3_Y1N0,
         Med_Code4, Med_Name4, MedDescriptor4, Antibiotic4, Not_Antibiotic4, Relevant_Med_4_Y1N0,
         Med_Code5, Med_Name5, MedDescriptor5, Antibiotic5, Not_Antibiotic5, Relevant_Med_5_Y1N0,
         Med_Code6, Med_Name6, MedDescriptor6, Antibiotic6, Not_Antibiotic6, Relevant_Med_6_Y1N0)

##########################
# IN THE BELOW CODE BLOCK
# Flag each row/subject for Hypoxia, Prenatal Maternal Infection, Maternal Stress Exposure
##########################

# Count relevant meds and instantiate categorical flag columns
All_OCs <- All_OCs %>%
  mutate(Antibiotic_Count = sum(Antibiotic1, Antibiotic2, Antibiotic3, Antibiotic4, Antibiotic5, Antibiotic6, na.rm = TRUE)) %>%
  mutate(Not_Antibiotic_Count = sum(Not_Antibiotic1, Not_Antibiotic2, Not_Antibiotic3, Not_Antibiotic4, Not_Antibiotic5, Not_Antibiotic6, na.rm = TRUE)) %>%
  mutate(All_Relevant_Med_Count = sum(Relevant_Med_1_Y1N0, Relevant_Med_2_Y1N0, Relevant_Med_3_Y1N0, Relevant_Med_4_Y1N0, Relevant_Med_5_Y1N0, Relevant_Med_6_Y1N0, na.rm = TRUE)) %>%
  mutate(Hypoxia_Flag = 0) %>%
  mutate(Infection_Flag = 0) %>%
  mutate(Stress_Flag = 0)

# Hypoxia
All_OCs <- All_OCs %>%
  mutate(Hypoxia_Flag = if_else(C_Section_Y1N0 == 1, Hypoxia_Flag + 1, Hypoxia_Flag, missing = 0)) %>%       # C-Section
  mutate(Hypoxia_Flag = if_else(Bleeding_Preg_Y1N0 == 1, Hypoxia_Flag + 1, Hypoxia_Flag, missing = 0)) %>%   # Bleeding during pregnancy
  mutate(Hypoxia_Flag = if_else(Pe_E_T_Y1N0 == 1 & Pre_Eclampsia_Y1N0 == 1, Hypoxia_Flag + 1, Hypoxia_Flag, missing = 0)) %>%   # Pre-Eclampsia
  mutate(Hypoxia_Flag = if_else(Blue_at_Birth_Y1N0 == 1, Hypoxia_Flag + 1, Hypoxia_Flag, missing = 0)) %>%   # Blue at Birth
  mutate(Hypoxia_Flag = if_else(Required_Resusc_Y1N0 == 1, Hypoxia_Flag + 1, Hypoxia_Flag, missing = 0)) %>% # Required Resuscitation/Did not breathe at first? 
  mutate(Hypoxia_Flag = if_else(Neonatal_Apnea_Y1N0 == 1, Hypoxia_Flag + 1, Hypoxia_Flag, missing = 0))      # Neonatal Apnea/Required Oxygen?

# Prenatal Maternal Infection
All_OCs <- All_OCs %>%
  mutate(Infection_Flag = if_else(UTI_Y1N0 == 1, Infection_Flag + 1, Infection_Flag, missing = 0)) %>%       # UTI
  mutate(Infection_Flag = if_else(Rubella_Y1N0 == 1, Infection_Flag + 1, Infection_Flag, missing = 0)) %>%   # Rubella
  mutate(Infection_Flag = if_else(All_Relevant_Med_Count >= 1, Infection_Flag + 1, Infection_Flag, missing = 0))  # Antibiotics and/or herpes medication; Upper Respiratory Infections, Influenza, Toxoplasmosis, Herpes type 2

# Maternal Stress Exposure
All_OCs <- All_OCs %>%
  mutate(Stress_Flag = if_else(Planned_Preg_Y1N0 == 0, Stress_Flag + 1, Stress_Flag, missing = 0))         # Unplanned (unwanted?) Pregnancy

##########################
# IN THE BELOW CODE BLOCK
# Count Number of Categories Present
##########################

# Instantiate column
All_OCs <- All_OCs %>%
  mutate(FlaggedCategories = 0)

# Populate column by counting nonzero flags, rowwise
All_OCs <- All_OCs %>%
  # Hypoxia
  mutate(FlaggedCategories = if_else(Hypoxia_Flag >= 1, FlaggedCategories + 1, FlaggedCategories)) %>% 
  # Prenatal Maternal Infection
  mutate(FlaggedCategories = if_else(Infection_Flag >= 1, FlaggedCategories + 1, FlaggedCategories)) %>% 
  # Maternal Stress Exposure
  mutate(FlaggedCategories = if_else(Stress_Flag >= 1, FlaggedCategories + 1, FlaggedCategories))

# remove unused column
All_OCs <- All_OCs %>%
  select(-OtherConditions_Y1N0)

# organize for usability
All_OCs <- All_OCs %>%
  select(src_subject_id, FlaggedCategories, Hypoxia_Flag, Infection_Flag, Stress_Flag, 
         C_Section_Y1N0, Bleeding_Preg_Y1N0, Pe_E_T_Y1N0, Pre_Eclampsia_Y1N0, Blue_at_Birth_Y1N0, 
         Required_Resusc_Y1N0, Neonatal_Apnea_Y1N0, Planned_Preg_Y1N0, UTI_Y1N0, Rubella_Y1N0,
         All_Relevant_Med_Count, Antibiotic_Count, Not_Antibiotic_Count, everything())

##########################
# IN THE BELOW CODE BLOCK
# Write to CSV
##########################

write_csv(All_OCs, paste0(derivative_data, '/5.0/ABCD_5.0_Obstetric_Complications_10_9_25.csv'))

