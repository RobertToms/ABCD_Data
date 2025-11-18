#########################
# Obstetric_Complications.R
# Author: Robert Toms
# Date: 9/12/2025
#########################

# The purpose of this code is to flag each participant with psychosis-associated Obstetric Complications,
# based on the categories from this article: https://academic.oup.com/schizophreniabulletin/article/34/6/1083/1939509#29826173.

library(tidyverse)

sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/6.0/"

# import data
OCs <- read_tsv(paste0(sourcepath, 'Physical_Health/ph_p_dhx.tsv'))
Meds <- read_csv(paste0(derivative_data, 'Full_Meds_LUT.csv'))
More_Meds <- read_csv(paste0(derivative_data, 'OC_FilledIn_Meds.csv'))

########################
# IN THE BELOW CODE BLOCK
# Isolate relevant variables
########################

## Isolate relevant variables
# OCs
OCs <- OCs %>%
  select(participant_id, session_id,
         # Associated with Hypoxia
         ph_p_dhx__birth_002,   # Was your child born by Caesarian section?
         ph_p_dhx__med_002,   # Heavy bleeding [during pregnancy] requiring bed rest or special treatment?
         ph_p_dhx__med_003,   # Pre-eclampsia, eclampsia, or toxemia?
         ph_p_dhx__med_014, # Pre-Eclampsia
         ph_p_dhx__birth_003,   # Blue at Birth
         ph_p_dhx__birth_005,   # Required Resuscitation/Did not breathe at first? 
         ph_p_dhx__birth_008,   # Neonatal Apnea/Required Oxygen?
         # Maternal Stress Exposure
         ph_p_dhx_006,      # Unwanted Pregnancy/Was your pregnancy with this child a planned pregnancy?
         # Prenatal Maternal Infection
         ph_p_dhx__med_008, # Genital Tract Infection/UTI?
         ph_p_dhx__med_006, # Rubella? (first 3 months of pregnancy)
         # Upper Respiratory Infections, Influenza, Toxoplasmosis, Herpes type 2
         ph_p_dhx__med_013, # Any other conditions requiring medical care? 
         # Prescription Medications during pregnancy
         ph_p_dhx__rx__label_001a, # Before knowing about pregnancy
         ph_p_dhx__rx__label_002a,
         ph_p_dhx__rx__label_003a,
         ph_p_dhx__rx__label_004a,
         ph_p_dhx__rx__label_001b, # After Parent(s) found out about pregnancy
         ph_p_dhx__rx__label_002b,
         ph_p_dhx__rx__label_003b,
         ph_p_dhx__rx__label_004b)

# Meds
Meds <- Meds %>%
  select(-src_subject_id, -eventname)

Meds <- Meds %>%
  rename(Med_Code = `Med Code`) %>%
  rename(Med_Name = `Med Name`) %>%
  pivot_longer(c(Med_Code, Med_Name), names_to = 'nametype', values_to = 'Med_Name') %>%
  select(-nametype) %>%
  unique()

# More_Meds
More_Meds <- More_Meds %>%
  select(-src_subject_id, -`Med Name`)

More_Meds <- More_Meds %>%
  mutate(Med_Code = as.character(Med_Code)) %>%
  pivot_longer(c(Med_Code, Med_Name), names_to = 'nametype', values_to = 'Med_Name') %>%
  select(-nametype)
  
# Merge Meds and More_Meds
Meds <- Meds %>%
  rbind(More_Meds)

Meds <- Meds %>%
  select(Med_Name, everything()) %>%
  unique()
  
##########################
# IN THE BELOW CODE BLOCK
# Isolate list of Parental Medications
##########################

# Isolate 
JustMeds <- OCs %>%
  select(participant_id, 
       ph_p_dhx__rx__label_001a, # Before knowing about pregnancy
       ph_p_dhx__rx__label_002a,
       ph_p_dhx__rx__label_003a,
       ph_p_dhx__rx__label_004a,
       ph_p_dhx__rx__label_001b, # After Parent(s) found out about pregnancy
       ph_p_dhx__rx__label_002b,
       ph_p_dhx__rx__label_003b,
       ph_p_dhx__rx__label_004b) %>%
  rename(Med1 = ph_p_dhx__rx__label_001a, # Before knowing about pregnancy
         Med2 = ph_p_dhx__rx__label_002a,
         Med3 = ph_p_dhx__rx__label_003a,
         Med4 = ph_p_dhx__rx__label_004a,
         Med5 = ph_p_dhx__rx__label_001b, # After Parent(s) found out about pregnancy
         Med6 = ph_p_dhx__rx__label_002b,
         Med7 = ph_p_dhx__rx__label_003b,
         Med8 = ph_p_dhx__rx__label_004b)

Long_Meds <- JustMeds %>%
  pivot_longer(c(Med1, Med2, Med3, Med4, Med5, Med6, Med6, Med7, Med8), names_to = "MedNum", values_to = "Med") %>%
  select(-MedNum)

Long_Meds_ID <- Long_Meds %>%
  rename(Med_Name = Med)

Long_Meds_ID <- Long_Meds_ID %>%
  left_join(Meds, by = 'Med_Name')

# Remove rows with no medications
Long_Meds_ID <- Long_Meds_ID %>%
  filter(Med_Name != 'n/a')

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

########################
# IN THE BELOW CODE BLOCK
# Fill in remaining medications
########################

# Split off those that need to be filled in (n = 38)
FillIn <- Long_Meds_ID %>%
  filter(is.na(`Other Explanation`))
FilledIn <- Long_Meds_ID %>%
  filter(!is.na(`Other Explanation`))

# Write to csv
#write_csv(FillIn, paste0(derivative_data, 'FillInMeds.csv'))

# Manually filled in Medication descriptions

# Read back in
lastmeds <- read_csv(paste0(derivative_data, 'FillInMeds.csv'))

# merge back in
Explained_Meds <- FilledIn %>%
  rbind(lastmeds)



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
  mutate(Relevant_Med_Y1N0 = if_else(str_detect(`Med_Name`, 'Tamiflu') | str_detect(`Med_Name`, 'Acyclovir'), 1, Relevant_Med_Y1N0)) %>%
  unique()



##########################
# IN THE BELOW CODE BLOCK
# Clean/Rename Columns for clarity/readability
# Simplify patient data to one row per subject
##########################

# rename
OCs <- OCs %>%
  rename(         
    # Associated with Hypoxia
    C_Section_Y1N0 = ph_p_dhx__birth_002,   # Was your child born by Caesarian section?
    Bleeding_Preg_Y1N0 = ph_p_dhx__med_002,   # Bleeding during pregnancy
    Pe_E_T_Y1N0 = ph_p_dhx__med_003,   # Pre-eclampsia, eclampsia, or toxemia?
    Pre_Eclampsia_Y1N0 = ph_p_dhx__med_014, # Pre-Eclampsia
    Blue_at_Birth_Y1N0 = ph_p_dhx__birth_003,   # Blue at Birth
    Required_Resusc_Y1N0 = ph_p_dhx__birth_005,   # Required Resuscitation/Did not breathe at first? 
    Neonatal_Apnea_Y1N0 = ph_p_dhx__birth_008,   # Neonatal Apnea/Required Oxygen?
    # Maternal Stress Exposure
    Planned_Preg_Y1N0 = ph_p_dhx_006,      # Unwanted Pregnancy/Was your pregnancy with this child a planned pregnancy?
    # Prenatal Maternal Infection
    UTI_Y1N0 = ph_p_dhx__med_008, # Genital Tract Infection/UTI?
    Rubella_Y1N0 = ph_p_dhx__med_006, # Rubella?
    # Upper Respiratory Infections, Influenza, Toxoplasmosis, Herpes type 2
    OtherConditions_Y1N0 = ph_p_dhx__med_013) # Any other conditions requiring medical care?

# set all "n/a" values to NA
OCs <- OCs %>%
  mutate(across(c(C_Section_Y1N0, Bleeding_Preg_Y1N0, Pe_E_T_Y1N0, 
      Pre_Eclampsia_Y1N0, Blue_at_Birth_Y1N0, Required_Resusc_Y1N0, Neonatal_Apnea_Y1N0,
      Planned_Preg_Y1N0, UTI_Y1N0, Rubella_Y1N0, OtherConditions_Y1N0),
    ~ as.numeric(if_else(. == "n/a", NA_character_, .))))

table(OCs$session_id)
## Only medication data for ses-00A and ses-04A
## If flag is endorsed in Year 4, set to positive in Baseline, or else leave it alone.
OCs <- OCs %>%
  group_by(participant_id) %>%
  # C-Section
  mutate(C_Section_Y1N0 = if_else(any(session_id == "ses-04A" & C_Section_Y1N0 == 1) & session_id == "ses-00A", 
                                  1, 
                                  C_Section_Y1N0, missing = C_Section_Y1N0)) %>%
#Bleeding Pregnancy
  mutate(Bleeding_Preg_Y1N0 = if_else(any(session_id == "ses-04A" & Bleeding_Preg_Y1N0 == 1) & session_id == "ses-00A", 
                                      1, 
                                      Bleeding_Preg_Y1N0, missing = Bleeding_Preg_Y1N0)) %>%
# Pre-Eclampsia, Eclampsia, Toxemia
  mutate(Pe_E_T_Y1N0 = if_else(any(session_id == "ses-04A" & Pe_E_T_Y1N0 == 1) & session_id == "ses-00A", 
                               1, 
                               Pe_E_T_Y1N0, missing = Pe_E_T_Y1N0)) %>%
# Pre-Eclampsia
  mutate(Pre_Eclampsia_Y1N0 = if_else(any(session_id == "ses-04A" & Pre_Eclampsia_Y1N0 == 1) & session_id == "ses-00A", 
                                      1, 
                                      Pre_Eclampsia_Y1N0, missing = Pre_Eclampsia_Y1N0)) %>%
# Blue at Birth
  mutate(Blue_at_Birth_Y1N0 = if_else(any(session_id == "ses-04A" & Blue_at_Birth_Y1N0 == 1) & session_id == "ses-00A", 
                                      1, 
                                      Blue_at_Birth_Y1N0, missing = Blue_at_Birth_Y1N0)) %>%
# Required Resuscitation
  mutate(Required_Resusc_Y1N0 = if_else(any(session_id == "ses-04A" & Required_Resusc_Y1N0 == 1) & session_id == "ses-00A", 
                                        1, 
                                        Required_Resusc_Y1N0, missing = Required_Resusc_Y1N0)) %>%
# NeoNatal Apnea
  mutate(Neonatal_Apnea_Y1N0 = if_else(any(session_id == "ses-04A" & Neonatal_Apnea_Y1N0 == 1) & session_id == "ses-00A", 
                                       1, 
                                       Neonatal_Apnea_Y1N0, missing = Neonatal_Apnea_Y1N0)) %>%
# Planned Pregnancy
  mutate(Planned_Preg_Y1N0 = if_else(any(session_id == "ses-04A" & Planned_Preg_Y1N0 == 1) & session_id == "ses-00A", 
                                     1, 
                                     Planned_Preg_Y1N0, missing = Planned_Preg_Y1N0)) %>%
  mutate(Planned_Preg_Y1N0 = if_else(any(session_id == "ses-04A" & Planned_Preg_Y1N0 == 0) & session_id == "ses-00A", 
                                     0, 
                                     Planned_Preg_Y1N0, missing = Planned_Preg_Y1N0)) %>%
# UTI
  mutate(UTI_Y1N0 = if_else(any(session_id == "ses-04A" & UTI_Y1N0 == 1) & session_id == "ses-00A", 
                            1, 
                            UTI_Y1N0, missing = UTI_Y1N0)) %>%
# Rubella
  mutate(Rubella_Y1N0 = if_else(any(session_id == "ses-04A" & Rubella_Y1N0 == 1) & session_id == "ses-00A", 
                                1, 
                                Rubella_Y1N0, missing = Rubella_Y1N0)) %>%
# Other Conditions requiring medical attention
  mutate(OtherConditions_Y1N0 = if_else(any(session_id == "ses-04A" & OtherConditions_Y1N0 == 1) & session_id == "ses-00A", 
                                        1, 
                                        OtherConditions_Y1N0, missing = OtherConditions_Y1N0))

# Year4 is now redundant, remove it, while sanity checking that they have a baseline counterpart. Then remove session_id.
OCs <- OCs %>%
  group_by(participant_id) %>%
  filter(!(session_id == "ses-04A" & any(session_id == "ses-00A"))) %>%
  select(-session_id)

# remove medication columns, detailed ones will be merged in after
OCs <- OCs %>%
  select(-ph_p_dhx__rx__label_001a, 
         -ph_p_dhx__rx__label_002a,
         -ph_p_dhx__rx__label_003a, 
         -ph_p_dhx__rx__label_004a,
         -ph_p_dhx__rx__label_001b, 
         -ph_p_dhx__rx__label_002b,
         -ph_p_dhx__rx__label_003b, 
         -ph_p_dhx__rx__label_004b)

##########################
# IN THE BELOW CODE BLOCK
# Merge Medication info/Flags into Full Obstetric Complications data frame
##########################

# Count number of medications per participant
Explained_Meds <- Explained_Meds %>%
  group_by(participant_id) %>%
  mutate(nummeds = sum(!is.na(Med_Name)))
table(Explained_Meds$nummeds) # up to 8 meds for 1 person

# make tally to convert to wide
Explained_Meds <- Explained_Meds %>%
  group_by(participant_id) %>%
  mutate(mednum = row_number())

# Make New Medication columns
Explained_Meds <- Explained_Meds %>%
  mutate(relemed = paste0("Relevant_Med_", mednum, "_Y1N0")) %>%  # create numbered medication columns -- "Relevant_Med_1_Y1N0", etc.
  mutate(expl = paste0("Explanation_", mednum)) %>%  # create numbered drug explanation columns
  mutate(antibio = paste0("Antibiotic", mednum, "_Y1N0")) %>%  # create numbered medication columns -- "Antibiotic1", etc.
  mutate(notantibio = paste0("Not_Antibiotic", mednum, "_Y1N0")) %>%  # create numbered medication columns -- "Not_Antibiotic1", etc.
  mutate(mednum = paste0("Med", mednum)) %>%  # create numbered medication columns -- "Med1", "Med2", etc.
  group_by(participant_id) %>%
  pivot_wider(
    names_from = mednum,
    values_from = Med_Name) %>%
  pivot_wider(
    names_from = expl,
    values_from = `Other Explanation`) %>%
  pivot_wider(
    names_from = antibio,
    values_from = Antibiotic) %>%
  pivot_wider(
    names_from = notantibio,
    values_from = Not_Antibiotic) %>%
  pivot_wider(
    names_from = relemed,
    values_from = Relevant_Med_Y1N0)

# Fill and collapse into a single line
Explained_Meds_clean <- Explained_Meds %>%
  fill(c(Med1, Med2, Med3, Med4, Med5, Med6, Med7, Med8,
         Antibiotic1_Y1N0, Antibiotic2_Y1N0, Antibiotic3_Y1N0, Antibiotic4_Y1N0, Antibiotic5_Y1N0, Antibiotic6_Y1N0, Antibiotic7_Y1N0, Antibiotic8_Y1N0,
         Not_Antibiotic1_Y1N0, Not_Antibiotic2_Y1N0, Not_Antibiotic3_Y1N0, Not_Antibiotic4_Y1N0, Not_Antibiotic5_Y1N0, Not_Antibiotic6_Y1N0, Not_Antibiotic7_Y1N0, Not_Antibiotic8_Y1N0,
         Relevant_Med_1_Y1N0, Relevant_Med_2_Y1N0, Relevant_Med_3_Y1N0, Relevant_Med_4_Y1N0, Relevant_Med_5_Y1N0, Relevant_Med_6_Y1N0, Relevant_Med_7_Y1N0, Relevant_Med_8_Y1N0,
         Explanation_1, Explanation_2, Explanation_3, Explanation_4, Explanation_5, Explanation_6, Explanation_7, Explanation_8), 
         .direction = "downup") %>%
  # Remove flags that won't get used
  select(-OralContraception_Y1N0, -HormonalSuspension_Y1N0, -Antipsychotics_Y1N0, -OtherConcerningRx_Y1N0, 
         -Antiepileptic_Y1N0, -Mood_Stabilizer_Y1N0, -Stimulants_Y1NN0, -AntiDepressants_Y1N0,
         -SSRI_Y1N0, -MAOI_Y1N0, -Sedative_Y1N0, -Melatonin_Y1N0, -Allergy_Y1N0, -PainKiller_Y1N0, 
         -ColdFluMedication_Y1N0, -Inhaler_Y1N0, -Steriod_Y1N0) %>%
  unique()

# merge and reorder columns for clarity
All_OCs <- OCs %>%
  left_join(Explained_Meds_clean, by = 'participant_id') %>%
  select(participant_id, C_Section_Y1N0, Bleeding_Preg_Y1N0, Pe_E_T_Y1N0, Pre_Eclampsia_Y1N0, Blue_at_Birth_Y1N0, Required_Resusc_Y1N0, Neonatal_Apnea_Y1N0,
         Planned_Preg_Y1N0, UTI_Y1N0, Rubella_Y1N0, OtherConditions_Y1N0, nummeds,
         Med1, Explanation_1, Antibiotic1_Y1N0, Not_Antibiotic1_Y1N0, Relevant_Med_1_Y1N0,
         Med2, Explanation_2, Antibiotic2_Y1N0, Not_Antibiotic2_Y1N0, Relevant_Med_2_Y1N0,
         Med3, Explanation_3, Antibiotic3_Y1N0, Not_Antibiotic3_Y1N0, Relevant_Med_3_Y1N0,
         Med4, Explanation_4, Antibiotic4_Y1N0, Not_Antibiotic4_Y1N0, Relevant_Med_4_Y1N0,
         Med5, Explanation_5, Antibiotic5_Y1N0, Not_Antibiotic5_Y1N0, Relevant_Med_5_Y1N0,
         Med6, Explanation_6, Antibiotic6_Y1N0, Not_Antibiotic6_Y1N0, Relevant_Med_6_Y1N0,
         Med7, Explanation_7, Antibiotic7_Y1N0, Not_Antibiotic7_Y1N0, Relevant_Med_7_Y1N0,
         Med8, Explanation_8, Antibiotic8_Y1N0, Not_Antibiotic8_Y1N0, Relevant_Med_8_Y1N0)



##########################
# IN THE BELOW CODE BLOCK
# Flag each row/subject for Hypoxia, Prenatal Maternal Infection, Maternal Stress Exposure
##########################

# Count relevant meds and instantiate categorical flag columns
All_OCs <- All_OCs %>%
  mutate(Antibiotic_Count = sum(Antibiotic1_Y1N0, Antibiotic2_Y1N0, Antibiotic3_Y1N0, Antibiotic4_Y1N0, Antibiotic5_Y1N0, Antibiotic6_Y1N0, Antibiotic7_Y1N0, Antibiotic8_Y1N0, na.rm = TRUE)) %>%
  mutate(Not_Antibiotic_Count = sum(Not_Antibiotic1_Y1N0, Not_Antibiotic2_Y1N0, Not_Antibiotic3_Y1N0, Not_Antibiotic4_Y1N0, Not_Antibiotic5_Y1N0, Not_Antibiotic6_Y1N0, Not_Antibiotic7_Y1N0, Not_Antibiotic8_Y1N0, na.rm = TRUE)) %>%
  mutate(All_Relevant_Med_Count = sum(Relevant_Med_1_Y1N0, Relevant_Med_2_Y1N0, Relevant_Med_3_Y1N0, Relevant_Med_4_Y1N0, Relevant_Med_5_Y1N0, Relevant_Med_6_Y1N0, Relevant_Med_7_Y1N0, Relevant_Med_8_Y1N0, na.rm = TRUE)) %>%
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


##########################
# IN THE BELOW CODE BLOCK
# Clean and Organize for Export
##########################

All_OCs <- All_OCs %>%
  select(participant_id, C_Section_Y1N0, Bleeding_Preg_Y1N0, Pe_E_T_Y1N0, Pre_Eclampsia_Y1N0, Blue_at_Birth_Y1N0, 
         Required_Resusc_Y1N0, Neonatal_Apnea_Y1N0, Planned_Preg_Y1N0, UTI_Y1N0, Rubella_Y1N0, 
         Hypoxia_Flag, Stress_Flag, Infection_Flag, Antibiotic_Count, Not_Antibiotic_Count, All_Relevant_Med_Count, FlaggedCategories, 
         nummeds,
         Med1, Explanation_1, Antibiotic1_Y1N0, Not_Antibiotic1_Y1N0, Relevant_Med_1_Y1N0,
         Med2, Explanation_2, Antibiotic2_Y1N0, Not_Antibiotic2_Y1N0, Relevant_Med_2_Y1N0,
         Med3, Explanation_3, Antibiotic3_Y1N0, Not_Antibiotic3_Y1N0, Relevant_Med_3_Y1N0,
         Med4, Explanation_4, Antibiotic4_Y1N0, Not_Antibiotic4_Y1N0, Relevant_Med_4_Y1N0,
         Med5, Explanation_5, Antibiotic5_Y1N0, Not_Antibiotic5_Y1N0, Relevant_Med_5_Y1N0,
         Med6, Explanation_6, Antibiotic6_Y1N0, Not_Antibiotic6_Y1N0, Relevant_Med_6_Y1N0,
         Med7, Explanation_7, Antibiotic7_Y1N0, Not_Antibiotic7_Y1N0, Relevant_Med_7_Y1N0,
         Med8, Explanation_8, Antibiotic8_Y1N0, Not_Antibiotic8_Y1N0, Relevant_Med_8_Y1N0)

# Clean NAs out
All_OCs <- All_OCs %>%
  mutate(across(c(Med1, Explanation_1, Med2, Explanation_2, Med3, Explanation_3, Med4, Explanation_4,
                  Med5, Explanation_5, Med6, Explanation_6, Med7, Explanation_7, Med8, Explanation_8), ~replace_na(., ""))) %>%
  mutate(across(c(Pre_Eclampsia_Y1N0, nummeds,
                  Relevant_Med_1_Y1N0, Relevant_Med_2_Y1N0, Relevant_Med_3_Y1N0, Relevant_Med_4_Y1N0,
                  Relevant_Med_5_Y1N0, Relevant_Med_6_Y1N0, Relevant_Med_7_Y1N0, Relevant_Med_8_Y1N0,
                  Antibiotic1_Y1N0, Antibiotic2_Y1N0, Antibiotic3_Y1N0, Antibiotic4_Y1N0, Antibiotic5_Y1N0, Antibiotic6_Y1N0, Antibiotic7_Y1N0, Antibiotic8_Y1N0,
                  Not_Antibiotic1_Y1N0, Not_Antibiotic2_Y1N0, Not_Antibiotic3_Y1N0, Not_Antibiotic4_Y1N0, Not_Antibiotic5_Y1N0, Not_Antibiotic6_Y1N0, Not_Antibiotic7_Y1N0, Not_Antibiotic8_Y1N0), 
               ~replace_na(., 0)))


##########################
# IN THE BELOW CODE BLOCK
# Write to CSV
##########################

write_csv(All_OCs, paste0(derivative_data, 'Obstetric_Complications_9_12_25.csv'))
