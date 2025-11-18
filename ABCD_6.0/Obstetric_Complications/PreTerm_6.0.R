##################
# PreTerm_6.0.R
# Author: Robert Toms
# Date: 10/22/2025
##################

library(tidyverse)

sourcepath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/6.0/"

parent <- read_tsv(paste0(sourcepath, "Physical_Health/ph_p_dhx.tsv")) #Parent Report ABCD Data
twins <- read_tsv(paste0(sourcepath, "abcd_general/ab_g_stc.tsv")) #Parent Report ABCD Data
mri <- read_csv(paste0(derivative_data, "6.0_Usable_Scan_List.csv")) #Parent Report ABCD Data

##############
# IN THE BELOW CODE BLOCK
# Prepare Pre-Term Status Data
##############

# keep and rename useful columns
premature <- parent %>%
  select(participant_id, session_id, ph_p_dhx__birth_001, ph_p_dhx__birth_001__01) %>%
  rename(premature = ph_p_dhx__birth_001,
         how_premature = ph_p_dhx__birth_001__01)

# format NAs and as numeric so they can be compared on a numeric continuum
premature <- premature %>%
  mutate(premature = if_else(premature == 'n/a', NA, as.numeric(premature)),
         how_premature = if_else(how_premature == 'n/a', NA, as.numeric(how_premature)))

##############
# IN THE BELOW CODE BLOCK
# Check that only Baseline reports are necessary
##############

# check yes/no pre-term reports
pivoted <- premature %>%
  select(-how_premature) %>%
  pivot_wider(names_from = session_id, values_from = premature)

table(pivoted$`ses-04A`) # 3 marked as premature in year 4, of those, only 1 marked as not premature in baseline, and that 1 is only 1 week premature. Treated as inconsistent, excluded.
table(pivoted$`ses-00A`)

# check reported number of weeks pre-term
pivoted_how <- premature %>%
  select(-premature) %>%
  pivot_wider(names_from = session_id, values_from = how_premature) # 3 with data, only one that doesn't match is discussed above -- 1 week premature, inconsistently reported, excluded.

# keep only baseline
premature <- premature %>%
  filter(session_id == 'ses-00A')

# get counts of:
table(premature$premature)     # Pre-Term or Not Pre-Term
table(premature$how_premature) # How Many Weeks Pre-Term

##############
# IN THE BELOW CODE BLOCK
# Find how many are Twins
# Find how many T1 MRI scans each subject has
##############

# keep useful columns for twin data
twins <- twins %>%
  select(participant_id, ab_g_stc__cohort_sex, ab_g_stc__design_famrel) # 0 = only child, 1 = sibling, 2 = twin, 3 = triplet

# count the number of scans per participant
mri <- mri %>%
  group_by(participant_id) %>%
  mutate(number_of_scans = sum(usable))

# keep useful columns
mri <- mri %>%
  select(participant_id, number_of_scans)

# make a dataframe for very premature births (30 weeks or earlier)
very_preterm <- premature %>%
  filter(how_premature > 9, how_premature != 999)

# merge in twin and MRI count data
very_preterm <- very_preterm %>%
  left_join(twins, by = 'participant_id') %>%
  left_join(mri, by = 'participant_id')

# keep and rename useful columns
very_preterm <- very_preterm %>%
  select(participant_id, ab_g_stc__cohort_sex, how_premature, ab_g_stc__design_famrel, number_of_scans) %>%
  rename(sex = ab_g_stc__cohort_sex,
         twin_status = ab_g_stc__design_famrel) %>%
  unique()

# make dataframe for all premature births
general_preterm <- premature %>%
  select(participant_id, premature, how_premature) %>%
  filter(premature == 1)

# merge in twin and MRI count data, keep and rename useful columns
general_preterm <- general_preterm %>%
  left_join(twins, by = 'participant_id') %>%
  left_join(mri, by = 'participant_id') %>%
  rename(sex = ab_g_stc__cohort_sex,
         twin_status = ab_g_stc__design_famrel) %>%
  unique()

##############
# IN THE BELOW CODE BLOCK
# Format for Export & Export to CSV
##############

# make biological sex readable
general_preterm <- general_preterm %>%
  mutate(sex = case_when(
    sex == 1 ~ "M",
    sex == 2 ~ "F"))
very_preterm <- very_preterm %>%
  mutate(sex = case_when(
    sex == 1 ~ "M",
    sex == 2 ~ "F"))

# reorder columns
general_preterm <- general_preterm %>%
  select(participant_id, sex, everything())

# Write to CSV
write_csv(general_preterm, paste0(derivative_data, "ABCD_6.0_PreTerm.csv"))
write_csv(very_preterm, paste0(derivative_data, "ABCD_6.0_Very_PreTerm.csv"))
