################################
# Cbm-Thal_rsFMRI.R 
# Author: Robert Toms
# Date: 9/11/2025
################################

library(tidyverse)

sourcepath <- "/path/to/ABCD_6.0/Data/"
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

# remove unusable data
Thalamic_QC <- Thalamic_QC %>%
  filter(mr_y_qc__incl__rsfmri_indicator == 1 | is.na(mr_y_qc__incl__rsfmri_indicator))

Cerebellar_QC <- Cerebellar_QC %>%
  filter(mr_y_qc__incl__rsfmri_indicator == 1 | is.na(mr_y_qc__incl__rsfmri_indicator))


# write to csv's

write_csv(Cerebellar_QC, paste0(derivative_data, 'rsFMRI_Cerebellum.csv'))
write_csv(Thalamic_QC, paste0(derivative_data, 'rsFMRI_Thalamus.csv'))

          