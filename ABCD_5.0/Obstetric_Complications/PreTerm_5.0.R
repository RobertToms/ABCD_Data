##################
# PreTerm_5.0.R
# Author: Robert Toms
# Date: 10/28/2025
##################

# Import Libraries and Data
library(tidyverse)

sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"

parent <- read_csv(paste0(sourcepath, "physical-health/ph_p_dhx.csv")) # Parent Report ABCD Birth/Pregnancy data
demo <- read_csv(paste0(sourcepath, "abcd-general/abcd_p_demo.csv")) # Contains twin status
twins <- read_csv(paste0(sourcepath, "genetics/gen_y_pihat.csv")) # Contains twin status
mri <- read_csv(paste0(derivative_data, "mri_availability_4_7_25.csv")) # List of number of usable T1 mri scans in ABCD 5.0

##############
# IN THE BELOW CODE BLOCK
# Prepare Pre-Term Status Data
##############

# keep and rename useful columns
premature <- parent %>%
  select(src_subject_id, eventname, devhx_12a_p, devhx_12_p) %>%
  rename(premature = devhx_12a_p,
         how_premature = devhx_12_p)

demo <- demo %>%
  select(src_subject_id, demo_sex_v2) %>%
  filter(!is.na(demo_sex_v2))

##############
# IN THE BELOW CODE BLOCK
# Check that only Baseline reports are necessary
##############

# check yes/no pre-term reports
pivoted <- premature %>%
  select(-how_premature) %>%
  pivot_wider(names_from = eventname, values_from = premature)

table(pivoted$`4_year_follow_up_y_arm_1`) # 3 marked as premature in year 4, of those, only 1 marked as not premature in baseline, and that 1 is only 1 week premature. Treated as inconsistent, excluded.
table(pivoted$baseline_year_1_arm_1)

# check reported number of weeks pre-term
pivoted_how <- premature %>%
  select(-premature) %>%
  pivot_wider(names_from = eventname, values_from = how_premature) # 3 with data, only one that doesn't match is discussed above -- 1 week premature, inconsistently reported, excluded.

# keep only baseline
premature <- premature %>%
  filter(eventname == 'baseline_year_1_arm_1')

# get counts of:
table(premature$premature)     # Pre-Term or Not Pre-Term
table(premature$how_premature) # How Many Weeks Pre-Term

##############
# IN THE BELOW CODE BLOCK
# Find how many are Twins
# Find how many T1 MRI scans each subject has
##############

# keep useful columns
mri <- mri %>%
  select(src_subject_id, number_mris_usable)
twins <- twins %>%
  select(src_subject_id, rel_relationship) # 0 = only child, 1 = sibling, 2 = twin, 3 = triplet

# make a dataframe for very premature births (30 weeks or earlier)
very_preterm <- premature %>%
  filter(how_premature > 9, how_premature != 999)

# merge in twin and MRI count data
very_preterm <- very_preterm %>%
  left_join(twins, by = 'src_subject_id') %>%
  left_join(demo, by = 'src_subject_id') %>%
  left_join(mri, by = 'src_subject_id')

# keep and rename useful columns
very_preterm <- very_preterm %>%
  select(src_subject_id, demo_sex_v2, how_premature, rel_relationship, number_mris_usable) %>%
  rename(sex = demo_sex_v2,
         twin_status = rel_relationship) %>%
  unique()

# make dataframe for all premature births
general_preterm <- premature %>%
  select(src_subject_id, premature, how_premature) %>%
  filter(premature == 1)

# merge in twin and MRI count data, keep and rename useful columns
general_preterm <- general_preterm %>%
  left_join(demo, by = 'src_subject_id') %>%
  left_join(twins, by = 'src_subject_id') %>%
  left_join(mri, by = 'src_subject_id') %>%
  rename(sex = demo_sex_v2,
         twin_status = rel_relationship) %>%
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
  select(src_subject_id, sex, everything())

# Write to CSV
write_csv(general_preterm, paste0(derivative_data, "ABCD_5.0_PreTerm.csv"))
write_csv(very_preterm, paste0(derivative_data, "ABCD_5.0_Very_PreTerm.csv"))
