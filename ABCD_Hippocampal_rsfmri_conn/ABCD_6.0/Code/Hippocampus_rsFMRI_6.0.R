#########################
# Hippocampus_rsFMRI_6.0.R
# Author: Dylan Seay & Robert Toms
# Date: 11/17/2025
#########################

library(tidyverse)

sourcepath <- "/Users/dal247378/Desktop/ABCD_6.0/Data/"
derivative_data <- "/Users/dal247378/Desktop/derivative_data/6.0/"

## Import data ##
rsfMRI_data <- read_tsv(paste0(sourcepath,'/Imaging/RestingState_fMRI/mr_y_rsfmri__corr__gpnet__aseg.tsv'))
QC_Data <- read_tsv(paste0(sourcepath,'/Imaging/QC/mr_y_qc__incl.tsv'))

## Pull Hippocampus-Relevant Rows ##
Hippocampus <- rsfMRI_data %>%
  select(participant_id, session_id,
        mr_y_rsfmri__corr__gpnet__aseg__aud__hc__lh_mean, #Average correlation between Gordon network: auditory & Subcortical ROI: hippocampus (left hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__cio__hc__lh_mean, #Average correlation between Gordon network: cingulo-opercular & Subcortical ROI: hippocampus (left hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__cip__hc__lh_mean, #Average correlation between Gordon network: cingulo-parietal & Subcortical ROI: hippocampus (left hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__def__hc__lh_mean, #Average correlation between Gordon network: default & Subcortical ROI: hippocampus (left hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__doa__hc__lh_mean, #Average correlation between Gordon network: dorsal attention & Subcortical ROI: hippocampus (left hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__frp__hc__lh_mean, #Average correlation between Gordon network: fronto-parietal & Subcortical ROI: hippocampus (left hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__non__hc__lh_mean, #Average correlation between Gordon network: none & Subcortical ROI: hippocampus (left hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__ret__hc__lh_mean, #Average correlation between Gordon network: retrosplenial temporal & Subcortical ROI: hippocampus (left hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__sal__hc__lh_mean, #Average correlation between Gordon network: salience & Subcortical ROI: hippocampus (left hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__smh__hc__lh_mean, #Average correlation between Gordon network: sensorimotor hand & Subcortical ROI: hippocampus (left hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__smm__hc__lh_mean, #Average correlation between Gordon network: sensorimotor mouth & Subcortical ROI: hippocampus (left hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__vea__hc__lh_mean, #Average correlation between Gordon network: ventral attention & Subcortical ROI: hippocampus (left hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__vis__hc__lh_mean, #Average correlation between Gordon network: visual & Subcortical ROI: hippocampus (left hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__aud__hc__rh_mean, #Average correlation between Gordon network: auditory & Subcortical ROI: hippocampus (right hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__cio__hc__rh_mean, #Average correlation between Gordon network: cingulo-opercular & Subcortical ROI: hippocampus (right hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__cip__hc__rh_mean, #Average correlation between Gordon network: cingulo-parietal & Subcortical ROI: hippocampus (right hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__def__hc__rh_mean, #Average correlation between Gordon network: default & Subcortical ROI: hippocampus (right hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__doa__hc__rh_mean, #Average correlation between Gordon network: dorsal attention & Subcortical ROI: hippocampus (right hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__frp__hc__rh_mean, #Average correlation between Gordon network: fronto-parietal & Subcortical ROI: hippocampus (right hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__non__hc__rh_mean, #Average correlation between Gordon network: none & Subcortical ROI: hippocampus (right hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__ret__hc__rh_mean, #Average correlation between Gordon network: retrosplenial temporal & Subcortical ROI: hippocampus (right hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__sal__hc__rh_mean, #Average correlation between Gordon network: salience & Subcortical ROI: hippocampus (right hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__smh__hc__rh_mean, #Average correlation between Gordon network: sensorimotor hand & Subcortical ROI: hippocampus (right hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__smm__hc__rh_mean, #Average correlation between Gordon network: sensorimotor mouth & Subcortical ROI: hippocampus (right hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__vea__hc__rh_mean, #Average correlation between Gordon network: ventral attention & Subcortical ROI: hippocampus (right hemisphere)
        mr_y_rsfmri__corr__gpnet__aseg__vis__hc__rh_mean) #Average correlation between Gordon network: visual & Subcortical ROI: hippocampus (right hemisphere)

## Remove n/a's and Convert to numeric ##
Hippocampus <- Hippocampus %>%
  mutate(across(contains("rsfmri"), ~if_else(.x == 'n/a', NA, as.numeric(.x))))

# Make Means of interhemispheric connectivity
Hippocampus <- Hippocampus %>%
  mutate(Auditory_HC_mean = (mr_y_rsfmri__corr__gpnet__aseg__aud__hc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__aud__hc__rh_mean) / 2,
         CinguloOpercular_HC_mean = (mr_y_rsfmri__corr__gpnet__aseg__cio__hc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__cio__hc__rh_mean) / 2,
         CinguloParietal_HC_mean = (mr_y_rsfmri__corr__gpnet__aseg__cip__hc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__cip__hc__rh_mean) / 2,
         Default_HC_mean = (mr_y_rsfmri__corr__gpnet__aseg__def__hc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__def__hc__rh_mean) / 2,
         DorsalAttn_HC_mean = (mr_y_rsfmri__corr__gpnet__aseg__doa__hc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__doa__hc__rh_mean) / 2,
         FrontoParietal_HC_mean = (mr_y_rsfmri__corr__gpnet__aseg__frp__hc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__frp__hc__rh_mean) / 2,
         None_HC_mean = (mr_y_rsfmri__corr__gpnet__aseg__non__hc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__non__hc__rh_mean) / 2,
         RetrosplenialTemporal_HC_mean = (mr_y_rsfmri__corr__gpnet__aseg__ret__hc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__ret__hc__rh_mean) / 2,
         Salience_HC_mean = (mr_y_rsfmri__corr__gpnet__aseg__sal__hc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__sal__hc__rh_mean) / 2,
         SensorimotorHand_HC_mean = (mr_y_rsfmri__corr__gpnet__aseg__smh__hc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__smh__hc__rh_mean) / 2,
         SensorimotorMouth_HC_mean = (mr_y_rsfmri__corr__gpnet__aseg__smm__hc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__smm__hc__rh_mean) / 2,
         VentralAttn_HC_mean = (mr_y_rsfmri__corr__gpnet__aseg__vea__hc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__vea__hc__rh_mean) / 2,
         Visual_HC_mean = (mr_y_rsfmri__corr__gpnet__aseg__vis__hc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__vis__hc__rh_mean) / 2)

## Rename for readability ##
Hippocampus <- Hippocampus %>%
  rename('Auditory <-> lhHC' = mr_y_rsfmri__corr__gpnet__aseg__aud__hc__lh_mean, #Average correlation between Gordon network: auditory & Subcortical ROI: hippocampus (left hemisphere)
         'CinguloOpercular <-> lhHC' = mr_y_rsfmri__corr__gpnet__aseg__cio__hc__lh_mean, #Average correlation between Gordon network: cingulo-opercular & Subcortical ROI: hippocampus (left hemisphere)
         'CinguloParietal <-> lhHC' = mr_y_rsfmri__corr__gpnet__aseg__cip__hc__lh_mean, #Average correlation between Gordon network: cingulo-parietal & Subcortical ROI: hippocampus (left hemisphere)
         'DMN <-> lhHC' = mr_y_rsfmri__corr__gpnet__aseg__def__hc__lh_mean, #Average correlation between Gordon network: default & Subcortical ROI: hippocampus (left hemisphere)
         'DorsalAttention <-> lhHC' = mr_y_rsfmri__corr__gpnet__aseg__doa__hc__lh_mean, #Average correlation between Gordon network: dorsal attention & Subcortical ROI: hippocampus (left hemisphere)
         'FrontoParietal <-> lhHC' = mr_y_rsfmri__corr__gpnet__aseg__frp__hc__lh_mean, #Average correlation between Gordon network: fronto-parietal & Subcortical ROI: hippocampus (left hemisphere)
         'None <-> lhHC' = mr_y_rsfmri__corr__gpnet__aseg__non__hc__lh_mean, #Average correlation between Gordon network: none & Subcortical ROI: hippocampus (left hemisphere)
         'RetrosplenialTemporal <-> lhHC' = mr_y_rsfmri__corr__gpnet__aseg__ret__hc__lh_mean, #Average correlation between Gordon network: retrosplenial temporal & Subcortical ROI: hippocampus (left hemisphere)
         'Salience <-> lhHC' = mr_y_rsfmri__corr__gpnet__aseg__sal__hc__lh_mean, #Average correlation between Gordon network: salience & Subcortical ROI: hippocampus (left hemisphere)
         'SensorimotorHand <-> lhHC' = mr_y_rsfmri__corr__gpnet__aseg__smh__hc__lh_mean, #Average correlation between Gordon network: sensorimotor hand & Subcortical ROI: hippocampus (left hemisphere)
         'SensorimotorMouth <-> lhHC' = mr_y_rsfmri__corr__gpnet__aseg__smm__hc__lh_mean, #Average correlation between Gordon network: sensorimotor mouth & Subcortical ROI: hippocampus (left hemisphere)
         'VentralAttention <-> lhHC' = mr_y_rsfmri__corr__gpnet__aseg__vea__hc__lh_mean, #Average correlation between Gordon network: ventral attention & Subcortical ROI: hippocampus (left hemisphere)
         'Visual <-> lhHC' = mr_y_rsfmri__corr__gpnet__aseg__vis__hc__lh_mean, #Average correlation between Gordon network: visual & Subcortical ROI: hippocampus (left hemisphere)
         'Auditory <-> rhHC' = mr_y_rsfmri__corr__gpnet__aseg__aud__hc__rh_mean, #Average correlation between Gordon network: auditory & Subcortical ROI: hippocampus (right hemisphere)
         'CinguloOpercular <-> rhHC' = mr_y_rsfmri__corr__gpnet__aseg__cio__hc__rh_mean, #Average correlation between Gordon network: cingulo-opercular & Subcortical ROI: hippocampus (right hemisphere)
         'CinguloParietal <-> rhHC' = mr_y_rsfmri__corr__gpnet__aseg__cip__hc__rh_mean, #Average correlation between Gordon network: cingulo-parietal & Subcortical ROI: hippocampus (right hemisphere)
         'DMN <-> rhHC' = mr_y_rsfmri__corr__gpnet__aseg__def__hc__rh_mean, #Average correlation between Gordon network: default & Subcortical ROI: hippocampus (right hemisphere)
         'DorsalAttention <-> rhHC' = mr_y_rsfmri__corr__gpnet__aseg__doa__hc__rh_mean, #Average correlation between Gordon network: dorsal attention & Subcortical ROI: hippocampus (right hemisphere)
         'FrontoParietal <-> rhHC' = mr_y_rsfmri__corr__gpnet__aseg__frp__hc__rh_mean, #Average correlation between Gordon network: fronto-parietal & Subcortical ROI: hippocampus (right hemisphere)
         'None <-> rhHC' = mr_y_rsfmri__corr__gpnet__aseg__non__hc__rh_mean, #Average correlation between Gordon network: none & Subcortical ROI: hippocampus (right hemisphere)
         'RestroplenialTemporal <-> rhHC' = mr_y_rsfmri__corr__gpnet__aseg__ret__hc__rh_mean, #Average correlation between Gordon network: retrosplenial temporal & Subcortical ROI: hippocampus (right hemisphere)
         'Salience <-> rhHC' = mr_y_rsfmri__corr__gpnet__aseg__sal__hc__rh_mean, #Average correlation between Gordon network: salience & Subcortical ROI: hippocampus (right hemisphere)
         'SensorimotorHand <-> rhHC' = mr_y_rsfmri__corr__gpnet__aseg__smh__hc__rh_mean, #Average correlation between Gordon network: sensorimotor hand & Subcortical ROI: hippocampus (right hemisphere)
         'SensorimotorMouth <-> rhHC' = mr_y_rsfmri__corr__gpnet__aseg__smm__hc__rh_mean, #Average correlation between Gordon network: sensorimotor mouth & Subcortical ROI: hippocampus (right hemisphere)
         'VentralAttention <-> rhHC' = mr_y_rsfmri__corr__gpnet__aseg__vea__hc__rh_mean, #Average correlation between Gordon network: ventral attention & Subcortical ROI: hippocampus (right hemisphere)
         'Visual <-> rhHC' = mr_y_rsfmri__corr__gpnet__aseg__vis__hc__rh_mean) #Average correlation between Gordon network: visual & Subcortical ROI: hippocampus (right hemisphere))

## Keep relevant QC Data rows ##
QC_Data <- QC_Data %>%
  select(participant_id, session_id, mr_y_qc__incl__rsfmri_indicator)

## Merge data ##
Hippocampus_QC <- Hippocampus %>%
  left_join(QC_Data, by = c('participant_id', 'session_id'))

# count subjects
Hippo_num <- Hippocampus_QC %>% distinct(participant_id) %>% pull(participant_id) # 11697 subjects, 171 with no data

# sanity check -- do these 171 have no data?
all_subs <- read_tsv(paste0(sourcepath, 'abcd_general/ab_g_stc.tsv')) %>% pull(participant_id) # 11868 total subjects
allegedly_missing <- setdiff(all_subs, Hippo_num)
has_rsfmri <- rsfMRI_data %>% distinct(participant_id) %>% pull(participant_id) # 11697 distinct subjects
actual_hippo_missing <- setdiff(allegedly_missing, has_rsfmri) # 171 confirmed, no data
# redownloaded rsfmri data, confirmed not a download error.

## Filter out unusable data ##
Hippocampus_QC <- Hippocampus_QC %>%
  filter(mr_y_qc__incl__rsfmri_indicator == 1 | is.na(mr_y_qc__incl__rsfmri_indicator))

# count subjects
Hippo_num_ex <- Hippocampus_QC %>% distinct(participant_id) %>% pull(participant_id) # 11127 subjects, 570 removed

# sanity check that the 570 had no usable data
removed_hippo_subs <- setdiff(has_rsfmri, Hippo_num_ex)
hippo_check <- QC_Data %>% filter(participant_id %in% removed_hippo_subs)
# confirmed, no usable data

# reorder to put means first
Hippocampus_QC <- Hippocampus_QC %>%
  select(participant_id, session_id, contains("mean"), everything())

## Export data ##
#write_csv(Hippocampus_QC, paste0(derivative_data, 'rsFMRI_Hippocampus_6.0.csv'))

####### get M/F counts
sex <- read_tsv(paste0(sourcepath, 'abcd_general/ab_g_stc.tsv')) %>% select(participant_id, ab_g_stc__cohort_sex)
hippo_sex_count <- Hippocampus_QC %>%
  left_join(sex, by = 'participant_id') %>%
  select(participant_id, ab_g_stc__cohort_sex) %>%
  unique()

table(hippo_sex_count$ab_g_stc__cohort_sex) # 5753 M, 5374 F

missing_hippo <- sex %>% filter(participant_id %in% actual_hippo_missing)
table(missing_hippo$ab_g_stc__cohort_sex) # 87 M 84 F

removed_hippo <- sex %>% filter(participant_id %in% removed_hippo_subs)
table(removed_hippo$ab_g_stc__cohort_sex) # 350 M 220 F
