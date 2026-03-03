################################
# Cbm-Thal_rsFMRI_6.0.R 
# Author: Robert Toms
# Date: 11/19/2025
################################

library(tidyverse)

datapath <- "/path/to/ABCD_6.0/Data/"
derivative_data <- "/path/to/derivative_data/6.0/"

rsfMRI_data <- read_tsv(paste0(datapath, 'Imaging/RestingState_fMRI/mr_y_rsfmri__corr__gpnet__aseg.tsv'))

# Select Relevant Rows
Cerebellar <- rsfMRI_data %>%
  select(participant_id, session_id,
         mr_y_rsfmri__corr__gpnet__aseg__aud__cbc__lh_mean,	  # Average correlation between auditory network and ASEG ROI left-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__aud__cbc__rh_mean,    # Average correlation between auditory network and ASEG ROI right-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__cio__cbc__lh_mean,	# Average correlation between cingulo-opercular network and ASEG ROI left-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__cio__cbc__rh_mean,	# Average correlation between cingulo-opercular network and ASEG ROI right-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__cip__cbc__lh_mean,	# Average correlation between cingulo-parietal network and ASEG ROI left-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__cip__cbc__rh_mean,	# Average correlation between cingulo-parietal network and ASEG ROI right-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__def__cbc__lh_mean,  	# Average correlation between default network and ASEG ROI left-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__def__cbc__rh_mean,	  # Average correlation between default network and ASEG ROI right-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__doa__cbc__lh_mean,	  # Average correlation between dorsal attention network and ASEG ROI left-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__doa__cbc__rh_mean,	  # Average correlation between dorsal attention network and ASEG ROI right-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__frp__cbc__lh_mean,	# Average correlation between fronto-parietal network and ASEG ROI left-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__frp__cbc__rh_mean,	# Average correlation between fronto-parietal network and ASEG ROI right-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__non__cbc__lh_mean,	# Average correlation between none network and ASEG ROI left-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__non__cbc__rh_mean,	# Average correlation between none network and ASEG ROI right-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__ret__cbc__lh_mean,	  # Average correlation between retrosplenial temporal network and ASEG ROI left-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__ret__cbc__rh_mean,	  # Average correlation between retrosplenial temporal network and ASEG ROI right-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__sal__cbc__lh_mean,	  # Average correlation between salience network and ASEG ROI left-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__sal__cbc__rh_mean,	  # Average correlation between salience network and ASEG ROI right-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__smh__cbc__lh_mean,	  # Average correlation between sensorimotor hand network and ASEG ROI left-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__smh__cbc__rh_mean,	  # Average correlation between sensorimotor hand network and ASEG ROI right-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__smm__cbc__lh_mean,	  # Average correlation between sensorimotor mouth network and ASEG ROI left-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__smm__cbc__rh_mean,	  # Average correlation between sensorimotor mouth network and ASEG ROI right-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__vea__cbc__lh_mean,	  # Average correlation between ventral attention network and ASEG ROI left-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__vea__cbc__rh_mean,	  # Average correlation between ventral attention network and ASEG ROI right-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__vis__cbc__lh_mean,	  # Average correlation between visual network and ASEG ROI left-cerebellum-cortex
         mr_y_rsfmri__corr__gpnet__aseg__vis__cbc__rh_mean)	  # Average correlation between visual network and ASEG ROI right-cerebellum-cortex


Thalamic <- rsfMRI_data %>%
  select(participant_id, session_id,
         mr_y_rsfmri__corr__gpnet__aseg__aud__th__lh_mean,	  # Average correlation between auditory network and ASEG ROI left-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__aud__th__rh_mean,	  # Average correlation between auditory network and ASEG ROI right-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__cio__th__lh_mean,	# Average correlation between cingulo-opercular network and ASEG ROI left-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__cio__th__rh_mean,	# Average correlation between cingulo-opercular network and ASEG ROI right-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__cip__th__lh_mean,	# Average correlation between cingulo-parietal network and ASEG ROI left-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__cip__th__rh_mean,	# Average correlation between cingulo-parietal network and ASEG ROI right-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__def__th__lh_mean,	  # Average correlation between default network and ASEG ROI left-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__def__th__rh_mean,	  # Average correlation between default network and ASEG ROI right-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__doa__th__lh_mean,	# Average correlation between dorsal attention network and ASEG ROI left-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__doa__th__rh_mean,	# Average correlation between dorsal attention network and ASEG ROI right-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__frp__th__lh_mean,	# Average correlation between fronto-parietal network and ASEG ROI left-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__frp__th__rh_mean,	# Average correlation between fronto-parietal network and ASEG ROI right-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__non__th__lh_mean,	# Average correlation between none network and ASEG ROI left-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__non__th__rh_mean,	# Average correlation between none network and ASEG ROI right-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__ret__th__lh_mean,	# Average correlation between retrosplenial temporal network and ASEG ROI left-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__ret__th__rh_mean,	# Average correlation between retrosplenial temporal network and ASEG ROI right-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__sal__th__lh_mean,	  # Average correlation between salience network and ASEG ROI left-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__sal__th__rh_mean,	  # Average correlation between salience network and ASEG ROI right-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__smh__th__lh_mean,	# Average correlation between sensorimotor hand network and ASEG ROI left-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__smh__th__rh_mean,	# Average correlation between sensorimotor hand network and ASEG ROI right-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__smm__th__lh_mean,	# Average correlation between sensorimotor mouth network and ASEG ROI left-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__smm__th__rh_mean,	# Average correlation between sensorimotor mouth network and ASEG ROI right-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__vea__th__lh_mean,	# Average correlation between ventral attention network and ASEG ROI left-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__vea__th__rh_mean,	# Average correlation between ventral attention network and ASEG ROI right-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__vis__th__lh_mean,	  # Average correlation between visual network and ASEG ROI left-thalamus-proper
         mr_y_rsfmri__corr__gpnet__aseg__vis__th__rh_mean)	  # Average correlation between visual network and ASEG ROI right-thalamus-proper

# Remove n/a's and Convert to numeric
Cerebellar <- Cerebellar %>%
  mutate(across(contains("rsfmri"), ~if_else(.x == 'n/a', NA, as.numeric(.x))))
Thalamic <- Thalamic %>%
  mutate(across(contains("rsfmri"), ~if_else(.x == 'n/a', NA, as.numeric(.x))))

# Make Means of interhemispheric connectivity
Cerebellar <- Cerebellar %>%
  mutate(Auditory_Cbm_mean = (mr_y_rsfmri__corr__gpnet__aseg__aud__cbc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__aud__cbc__rh_mean) / 2,
         CinguloOpercular_Cbm_mean = (mr_y_rsfmri__corr__gpnet__aseg__cio__cbc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__cio__cbc__rh_mean) / 2,
         CinguloParietal_Cbm_mean = (mr_y_rsfmri__corr__gpnet__aseg__cip__cbc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__cip__cbc__rh_mean) / 2,
         Default_Cbm_mean = (mr_y_rsfmri__corr__gpnet__aseg__def__cbc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__def__cbc__rh_mean) / 2,
         DorsalAttn_Cbm_mean = (mr_y_rsfmri__corr__gpnet__aseg__doa__cbc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__doa__cbc__rh_mean) / 2,
         FrontoParietal_Cbm_mean = (mr_y_rsfmri__corr__gpnet__aseg__frp__cbc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__frp__cbc__rh_mean) / 2,
         None_Cbm_mean = (mr_y_rsfmri__corr__gpnet__aseg__non__cbc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__non__cbc__rh_mean) / 2,
         RetrosplenialTemporal_Cbm_mean = (mr_y_rsfmri__corr__gpnet__aseg__ret__cbc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__ret__cbc__rh_mean) / 2,
         Salience_Cbm_mean = (mr_y_rsfmri__corr__gpnet__aseg__sal__cbc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__sal__cbc__rh_mean) / 2,
         SensorimotorHand_Cbm_mean = (mr_y_rsfmri__corr__gpnet__aseg__smh__cbc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__smh__cbc__rh_mean) / 2,
         SensorimotorMouth_Cbm_mean = (mr_y_rsfmri__corr__gpnet__aseg__smm__cbc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__smm__cbc__rh_mean) / 2,
         VentralAttn_Cbm_mean = (mr_y_rsfmri__corr__gpnet__aseg__vea__cbc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__vea__cbc__rh_mean) / 2,
         Visual_Cbm_mean = (mr_y_rsfmri__corr__gpnet__aseg__vis__cbc__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__vis__cbc__rh_mean) / 2)

Thalamic <- Thalamic %>%
  mutate(Auditory_Thal_mean = (mr_y_rsfmri__corr__gpnet__aseg__aud__th__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__aud__th__rh_mean) / 2,
         CinguloOpercular_Thal_mean = (mr_y_rsfmri__corr__gpnet__aseg__cio__th__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__cio__th__rh_mean) / 2,
         CinguloParietal_Thal_mean = (mr_y_rsfmri__corr__gpnet__aseg__cip__th__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__cip__th__rh_mean) / 2,
         Default_Thal_mean = (mr_y_rsfmri__corr__gpnet__aseg__def__th__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__def__th__rh_mean) / 2,
         DorsalAttn_Thal_mean = (mr_y_rsfmri__corr__gpnet__aseg__doa__th__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__doa__th__rh_mean) / 2,
         FrontoParietal_Thal_mean = (mr_y_rsfmri__corr__gpnet__aseg__frp__th__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__frp__th__rh_mean) / 2,
         None_Thal_mean = (mr_y_rsfmri__corr__gpnet__aseg__non__th__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__non__th__rh_mean) / 2,
         RetrosplenialTemporal_Thal_mean = (mr_y_rsfmri__corr__gpnet__aseg__ret__th__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__ret__th__rh_mean) / 2,
         Salience_Thal_mean = (mr_y_rsfmri__corr__gpnet__aseg__sal__th__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__sal__th__rh_mean) / 2,
         SensorimotorHand_Thal_mean = (mr_y_rsfmri__corr__gpnet__aseg__smh__th__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__smh__th__rh_mean) / 2,
         SensorimotorMouth_Thal_mean = (mr_y_rsfmri__corr__gpnet__aseg__smm__th__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__smm__th__rh_mean) / 2,
         VentralAttn_Thal_mean = (mr_y_rsfmri__corr__gpnet__aseg__vea__th__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__vea__th__rh_mean) / 2,
         Visual_Thal_mean = (mr_y_rsfmri__corr__gpnet__aseg__vis__th__lh_mean + mr_y_rsfmri__corr__gpnet__aseg__vis__th__rh_mean) / 2)

# Rename for readability
Cerebellar <- Cerebellar %>%
  rename('Auditory <-> LhCbm' = mr_y_rsfmri__corr__gpnet__aseg__aud__cbc__lh_mean,	  # Average correlation between auditory network and ASEG ROI left-cerebellum-cortex
         'Auditory <-> RhCbm' = mr_y_rsfmri__corr__gpnet__aseg__aud__cbc__rh_mean,    # Average correlation between auditory network and ASEG ROI right-cerebellum-cortex
         'Cingulo-Opercular <-> LhCbm' = mr_y_rsfmri__corr__gpnet__aseg__cio__cbc__lh_mean,	# Average correlation between cingulo-opercular network and ASEG ROI left-cerebellum-cortex
         'Cingulo-Opercular <-> RhCbm' = mr_y_rsfmri__corr__gpnet__aseg__cio__cbc__rh_mean,	# Average correlation between cingulo-opercular network and ASEG ROI right-cerebellum-cortex
         'Cingulo-Parietal <-> LhCbm' = mr_y_rsfmri__corr__gpnet__aseg__cip__cbc__lh_mean,	# Average correlation between cingulo-parietal network and ASEG ROI left-cerebellum-cortex
         'Cingulo-Parietal <-> RhCbm' = mr_y_rsfmri__corr__gpnet__aseg__cip__cbc__rh_mean,	# Average correlation between cingulo-parietal network and ASEG ROI right-cerebellum-cortex
         'DefaultMode <-> LhCbm' = mr_y_rsfmri__corr__gpnet__aseg__def__cbc__lh_mean,  	# Average correlation between default network and ASEG ROI left-cerebellum-cortex
         'DefaultMode <-> RhCbm' = mr_y_rsfmri__corr__gpnet__aseg__def__cbc__rh_mean,	  # Average correlation between default network and ASEG ROI right-cerebellum-cortex
         'DorsalAttn <-> LhCbm' = mr_y_rsfmri__corr__gpnet__aseg__doa__cbc__lh_mean,	  # Average correlation between dorsal attention network and ASEG ROI left-cerebellum-cortex
         'DorsalAttn <-> RhCbm' = mr_y_rsfmri__corr__gpnet__aseg__doa__cbc__rh_mean,	  # Average correlation between dorsal attention network and ASEG ROI right-cerebellum-cortex
         'Fronto-Parietal <-> LhCbm' = mr_y_rsfmri__corr__gpnet__aseg__frp__cbc__lh_mean,	# Average correlation between fronto-parietal network and ASEG ROI left-cerebellum-cortex
         'Fronto-Parietal <-> RhCbm' = mr_y_rsfmri__corr__gpnet__aseg__frp__cbc__rh_mean,	# Average correlation between fronto-parietal network and ASEG ROI right-cerebellum-cortex
         'None <-> LhCbm' = mr_y_rsfmri__corr__gpnet__aseg__non__cbc__lh_mean,	# Average correlation between none network and ASEG ROI left-cerebellum-cortex
         'None <-> RhCbm' = mr_y_rsfmri__corr__gpnet__aseg__non__cbc__rh_mean,	# Average correlation between none network and ASEG ROI right-cerebellum-cortex
         'RetrosplenialTemporal <-> LhCbm' = mr_y_rsfmri__corr__gpnet__aseg__ret__cbc__lh_mean,	  # Average correlation between retrosplenial temporal network and ASEG ROI left-cerebellum-cortex
         'RetrosplenialTemporal <-> RhCbm' = mr_y_rsfmri__corr__gpnet__aseg__ret__cbc__rh_mean,	  # Average correlation between retrosplenial temporal network and ASEG ROI right-cerebellum-cortex
         'Salience <-> LhCbm' = mr_y_rsfmri__corr__gpnet__aseg__sal__cbc__lh_mean,	  # Average correlation between salience network and ASEG ROI left-cerebellum-cortex
         'Salience <-> RhCbm' = mr_y_rsfmri__corr__gpnet__aseg__sal__cbc__rh_mean,	  # Average correlation between salience network and ASEG ROI right-cerebellum-cortex
         'SensoriMotorHand <-> LhCbm' = mr_y_rsfmri__corr__gpnet__aseg__smh__cbc__lh_mean,	  # Average correlation between sensorimotor hand network and ASEG ROI left-cerebellum-cortex
         'SensoriMotorHand <-> RhCbm' =  mr_y_rsfmri__corr__gpnet__aseg__smh__cbc__rh_mean,	  # Average correlation between sensorimotor hand network and ASEG ROI right-cerebellum-cortex
         'SensoriMotorMouth <-> LhCbm' = mr_y_rsfmri__corr__gpnet__aseg__smm__cbc__lh_mean,	  # Average correlation between sensorimotor mouth network and ASEG ROI left-cerebellum-cortex
         'SensoriMotorMouth <-> RhCbm' = mr_y_rsfmri__corr__gpnet__aseg__smm__cbc__rh_mean,	  # Average correlation between sensorimotor mouth network and ASEG ROI right-cerebellum-cortex
         'VentralAttn <-> LhCbm' = mr_y_rsfmri__corr__gpnet__aseg__vea__cbc__lh_mean,	  # Average correlation between ventral attention network and ASEG ROI left-cerebellum-cortex
         'VentralAttn <-> RhCbm' = mr_y_rsfmri__corr__gpnet__aseg__vea__cbc__rh_mean,	  # Average correlation between ventral attention network and ASEG ROI right-cerebellum-cortex
         'Visual <-> LhCbm' = mr_y_rsfmri__corr__gpnet__aseg__vis__cbc__lh_mean,	  # Average correlation between visual network and ASEG ROI left-cerebellum-cortex
         'Visual <-> RhCbm' = mr_y_rsfmri__corr__gpnet__aseg__vis__cbc__rh_mean)	  # Average correlation between visual network and ASEG ROI right-cerebellum-cortex

Thalamic <- Thalamic %>%
  rename('Auditory <-> LhThal' = mr_y_rsfmri__corr__gpnet__aseg__aud__th__lh_mean,	  # Average correlation between auditory network and ASEG ROI left-thalamus-proper
         'Auditory <-> RhThal' = mr_y_rsfmri__corr__gpnet__aseg__aud__th__rh_mean,	  # Average correlation between auditory network and ASEG ROI right-thalamus-proper
         'Cingulo-Opercular <-> LhThal' = mr_y_rsfmri__corr__gpnet__aseg__cio__th__lh_mean,	# Average correlation between cingulo-opercular network and ASEG ROI left-thalamus-proper
         'Cingulo-Opercular <-> RhThal' = mr_y_rsfmri__corr__gpnet__aseg__cio__th__rh_mean,	# Average correlation between cingulo-opercular network and ASEG ROI right-thalamus-proper
         'Cingulo-Parietal <-> LhThal' = mr_y_rsfmri__corr__gpnet__aseg__cip__th__lh_mean,	# Average correlation between cingulo-parietal network and ASEG ROI left-thalamus-proper
         'Cingulo-Parietal <-> RhThal' = mr_y_rsfmri__corr__gpnet__aseg__cip__th__rh_mean,	# Average correlation between cingulo-parietal network and ASEG ROI right-thalamus-proper
         'DefaultMode <-> LhThal' = mr_y_rsfmri__corr__gpnet__aseg__def__th__lh_mean,	  # Average correlation between default network and ASEG ROI left-thalamus-proper
         'DefaultMode <-> RhThal' = mr_y_rsfmri__corr__gpnet__aseg__def__th__rh_mean,	  # Average correlation between default network and ASEG ROI right-thalamus-proper
         'DorsalAttn <-> LhThal' = mr_y_rsfmri__corr__gpnet__aseg__doa__th__lh_mean,	# Average correlation between dorsal attention network and ASEG ROI left-thalamus-proper
         'DorsalAttn <-> RhThal' = mr_y_rsfmri__corr__gpnet__aseg__doa__th__rh_mean,	# Average correlation between dorsal attention network and ASEG ROI right-thalamus-proper
         'Fronto-Parietal <-> LhThal' = mr_y_rsfmri__corr__gpnet__aseg__frp__th__lh_mean,	# Average correlation between fronto-parietal network and ASEG ROI left-thalamus-proper
         'Fronto-Parietal <-> RhThal' = mr_y_rsfmri__corr__gpnet__aseg__frp__th__rh_mean,	# Average correlation between fronto-parietal network and ASEG ROI right-thalamus-proper
         'None <-> LhThal' = mr_y_rsfmri__corr__gpnet__aseg__non__th__lh_mean,	# Average correlation between none network and ASEG ROI left-thalamus-proper
         'None <-> RhThal' = mr_y_rsfmri__corr__gpnet__aseg__non__th__rh_mean,	# Average correlation between none network and ASEG ROI right-thalamus-proper
         'RetrosplenialTemporal <-> LhThal' = mr_y_rsfmri__corr__gpnet__aseg__ret__th__lh_mean,	# Average correlation between retrosplenial temporal network and ASEG ROI left-thalamus-proper
         'RetrosplenialTemporal <-> RhThal' = mr_y_rsfmri__corr__gpnet__aseg__ret__th__rh_mean,	# Average correlation between retrosplenial temporal network and ASEG ROI right-thalamus-proper
         'Salience <-> LhThal' = mr_y_rsfmri__corr__gpnet__aseg__sal__th__lh_mean,	  # Average correlation between salience network and ASEG ROI left-thalamus-proper
         'Salience <-> RhThal' = mr_y_rsfmri__corr__gpnet__aseg__sal__th__rh_mean,	  # Average correlation between salience network and ASEG ROI right-thalamus-proper
         'SensoriMotorHand <-> LhThal' =  mr_y_rsfmri__corr__gpnet__aseg__smh__th__lh_mean,	# Average correlation between sensorimotor hand network and ASEG ROI left-thalamus-proper
         'SensoriMotorHand <-> RhThal' =  mr_y_rsfmri__corr__gpnet__aseg__smh__th__rh_mean,	# Average correlation between sensorimotor hand network and ASEG ROI right-thalamus-proper
         'SensoriMotorMouth <-> LhThal' =  mr_y_rsfmri__corr__gpnet__aseg__smm__th__lh_mean,	# Average correlation between sensorimotor mouth network and ASEG ROI left-thalamus-proper
         'SensoriMotorMouth <-> RhThal' =  mr_y_rsfmri__corr__gpnet__aseg__smm__th__rh_mean,	# Average correlation between sensorimotor mouth network and ASEG ROI right-thalamus-proper
         'VentralAttn <-> LhThal' = mr_y_rsfmri__corr__gpnet__aseg__vea__th__lh_mean,	# Average correlation between ventral attention network and ASEG ROI left-thalamus-proper
         'VentralAttn <-> RhThal' = mr_y_rsfmri__corr__gpnet__aseg__vea__th__rh_mean,	# Average correlation between ventral attention network and ASEG ROI right-thalamus-proper
         'Visual <-> LhThal' = mr_y_rsfmri__corr__gpnet__aseg__vis__th__lh_mean,	  # Average correlation between visual network and ASEG ROI left-thalamus-proper
         'Visual <-> RhThal' = mr_y_rsfmri__corr__gpnet__aseg__vis__th__rh_mean)	  # Average correlation between visual network and ASEG ROI right-thalamus-proper



# read in rs-qc data
rs_qc_data <- read_tsv(paste0(datapath, 'Imaging/QC/mr_y_qc__incl.tsv'))

# keep only useful columns
rs_qc_data <- rs_qc_data %>%
  select(participant_id, session_id, mr_y_qc__incl__rsfmri_indicator)


# integrate qc data
Thalamic_QC <- Thalamic %>%
  left_join(rs_qc_data, by = c('participant_id', 'session_id'))

Cerebellar_QC <- Cerebellar %>%
  left_join(rs_qc_data, by = c('participant_id', 'session_id'))

# count subjects
Thal_num <- Thalamic_QC %>% distinct(participant_id) %>% pull(participant_id) # 11697 subjects, 171 with no data
Cbm_num <- Cerebellar_QC %>% distinct(participant_id) %>% pull(participant_id) # 11697 subjects, 171 with no data

# sanity check -- do these 171 have no data?
all_subs <- read_tsv(paste0(datapath, 'abcd_general/ab_g_stc.tsv')) %>% pull(participant_id) # 11868 total subjects
allegedly_missing_thal <- setdiff(all_subs, Thal_num)
allegedly_missing_cbm <- setdiff(all_subs, Cbm_num)
has_rsfmri <- rsfMRI_data %>% distinct(participant_id) %>% pull(participant_id) # 11697 distinct subjects
actual_thal_missing <- setdiff(allegedly_missing_thal, has_rsfmri) # 171 confirmed, no data
actual_cbm_missing <- setdiff(allegedly_missing_cbm, has_rsfmri) # 171 confirmed, no data
# redownloaded rsfmri data, confirmed not a download error.

# remove unusable data
Thalamic_QC <- Thalamic_QC %>%
  filter(mr_y_qc__incl__rsfmri_indicator == 1 | is.na(mr_y_qc__incl__rsfmri_indicator))

Cerebellar_QC <- Cerebellar_QC %>%
  filter(mr_y_qc__incl__rsfmri_indicator == 1 | is.na(mr_y_qc__incl__rsfmri_indicator))

# count subjects
Thal_num_ex <- Thalamic_QC %>% distinct(participant_id) %>% pull(participant_id) # 11127 subjects, 570 removed
Cbm_num_ex <- Cerebellar_QC %>% distinct(participant_id) %>% pull(participant_id) # 11127 subjects, 570 removed

# sanity check that the 570 had no usable data
removed_thal_subs <- setdiff(has_rsfmri, Thal_num_ex)
removed_cbm_subs <- setdiff(has_rsfmri, Cbm_num_ex)
Thal_check <- rs_qc_data %>% filter(participant_id %in% removed_thal_subs)
Cbm_check <- rs_qc_data %>% filter(participant_id %in% removed_cbm_subs)
# confirmed, no usable data

# reorder to put means first
Cerebellar_QC <- Cerebellar_QC %>%
  select(participant_id, session_id, contains("mean"), everything())
Thalamic_QC <- Thalamic_QC %>%
  select(participant_id, session_id, contains("mean"), everything())

# write to csv's
write_csv(Cerebellar_QC, paste0(derivative_data, 'rsFMRI_Cerebellum_6.0.csv'))
write_csv(Thalamic_QC, paste0(derivative_data, 'rsFMRI_Thalamus_6.0.csv'))

####### get M/F counts
sex <- read_tsv(paste0(datapath, 'abcd_general/ab_g_stc.tsv')) %>% select(participant_id, ab_g_stc__cohort_sex)
cbm_sex_count <- Cerebellar_QC %>%
  left_join(sex, by = 'participant_id') %>%
  select(participant_id, ab_g_stc__cohort_sex) %>%
  unique()
thal_sex_count <- Thalamic_QC %>%
  left_join(sex, by = 'participant_id') %>%
  select(participant_id, ab_g_stc__cohort_sex) %>%
  unique()

table(cbm_sex_count$ab_g_stc__cohort_sex) # 5753 M, 5374 F
table(thal_sex_count$ab_g_stc__cohort_sex)# 5753 M, 5374 F

missing_thal <- sex %>% filter(participant_id %in% actual_thal_missing)
table(missing_thal$ab_g_stc__cohort_sex) # 87 M 84 F
missing_cbm <- sex %>% filter(participant_id %in% actual_cbm_missing)
table(missing_cbm$ab_g_stc__cohort_sex) # 87 M 84 F

removed_cbm <- sex %>% filter(participant_id %in% removed_cbm_subs)
table(removed_cbm$ab_g_stc__cohort_sex) # 350 M 220 F
removed_thal <- sex %>% filter(participant_id %in% removed_thal_subs)
table(removed_thal$ab_g_stc__cohort_sex) # 350 M 220 F