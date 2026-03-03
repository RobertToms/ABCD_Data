###########################
# Cbm-Thal_rsFMRI_5.0.R
# Author: Robert Toms
# Date: 10/9/2025
###########################

library(tidyverse)

sourcepath <- "/Users/dal247378/Desktop/ABCD_5.0/"
derivative_data <- "/Users/dal247378/Desktop/derivative_data/5.0/"

corrections <- read_csv(paste0(sourcepath, "/img_correction/rsfmri_gpnet_aseg_correction.csv"))
imaging_data <- read_csv(paste0(sourcepath, "/core/imaging/mri_y_rsfmr_cor_gp_aseg.csv"))
rs_qc_data <- read_csv(paste0(sourcepath, "/core/imaging/mri_y_qc_incl.csv"))

#####################
# IN THE BELOW CODE BLOCK
# Get list of correct column names
#####################

##### Massive variable naming error in the ABCD dataset thru 5.1. Correcting variable names is essential.
# You are receiving this email because you were a first or last author on a paper that used ABCD rsfMRI data 
# and may be affected by the issue described below.
# Please see https://docs.abcdstudy.org/latest/documentation/release_notes/6_0.html#column-misnaming-in-rsfmri-network-to-subcortical-roi-correlation-tabulated-data 
# for information regarding a mislabeling that occurred to some of the files in ABCD releases 1.0 to 5.1. 
# Note that this error only applies to the variables "rsfMRI network to subcortical ROI correlation". 
# All other rsfMRI metrics are unaffected. The error has been corrected in ABCD release 6.0"

cols_of_interest <- corrections %>%
  mutate(cbm_or_thal = if_else(str_detect(`label (correct)`, "cerebellum"), "C", NA),
         cbm_or_thal = if_else(str_detect(`label (correct)`, "thalamus"), "T", cbm_or_thal)) %>%
  filter(!is.na(cbm_or_thal)) %>%
  arrange(cbm_or_thal)

cols_of_interest <- cols_of_interest %>%
  select(-name, -cbm_or_thal)

#####################
# IN THE BELOW CODE BLOCK
# Get misnamed columns with correct data
#####################

# SANITY CHECK: find how many columns to expect
cbm <- imaging_data %>%
  select(src_subject_id, eventname, contains('crcx'))
# 26 data columns, 2 id columns


# Get a list of the columns with correct data, to be pasted into a select() call
columns <- ""

for (column in cols_of_interest$`name_nda (previously used)`) {
  columns <- columns %>%
    paste0(column, sep = ", ")
}

print(columns)

# isolate those columns
data <- imaging_data %>%
  select(src_subject_id, eventname,
         rsfmri_cor_ngd_au_scs_crcxlh, rsfmri_cor_ngd_au_scs_thplh, rsfmri_cor_ngd_au_scs_cdelh, rsfmri_cor_ngd_au_scs_ptlh, rsfmri_cor_ngd_au_scs_pllh, rsfmri_cor_ngd_au_scs_bs, rsfmri_cor_ngd_au_scs_hplh, rsfmri_cor_ngd_au_scs_aglh, rsfmri_cor_ngd_au_scs_crcxrh, rsfmri_cor_ngd_au_scs_aalh, rsfmri_cor_ngd_au_scs_vtdclh, rsfmri_cor_ngd_au_scs_thprh, rsfmri_cor_ngd_au_scs_cderh, rsfmri_cor_ngd_none_scs_agrh, rsfmri_cor_ngd_none_scs_aarh, rsfmri_cor_ngd_none_scs_vtdcrh, rsfmri_cor_ngd_rst_scs_crcxlh, rsfmri_cor_ngd_rst_scs_thplh, rsfmri_cor_ngd_rst_scs_cdelh, rsfmri_cor_ngd_rst_scs_ptlh, rsfmri_cor_ngd_rst_scs_pllh, rsfmri_cor_ngd_rst_scs_aglh, rsfmri_cor_ngd_rst_scs_bs, rsfmri_cor_ngd_rst_scs_hplh, rsfmri_cor_ngd_rst_scs_aalh, rsfmri_cor_ngd_rst_scs_vtdclh, rsfmri_cor_ngd_au_scs_ptrh, rsfmri_cor_ngd_au_scs_plrh, rsfmri_cor_ngd_au_scs_hprh, rsfmri_cor_ngd_au_scs_agrh, rsfmri_cor_ngd_au_scs_aarh, rsfmri_cor_ngd_au_scs_vtdcrh, rsfmri_cor_ngd_cerc_scs_crcxlh, rsfmri_cor_ngd_cerc_scs_thplh, rsfmri_cor_ngd_cerc_scs_pllh, rsfmri_cor_ngd_cerc_scs_cdelh, rsfmri_cor_ngd_cerc_scs_ptlh, rsfmri_cor_ngd_cerc_scs_bs, rsfmri_cor_ngd_cerc_scs_hplh, rsfmri_cor_ngd_rst_scs_crcxrh, rsfmri_cor_ngd_rst_scs_thprh, rsfmri_cor_ngd_rst_scs_cderh, rsfmri_cor_ngd_rst_scs_ptrh, rsfmri_cor_ngd_rst_scs_plrh, rsfmri_cor_ngd_rst_scs_hprh, rsfmri_cor_ngd_rst_scs_agrh, rsfmri_cor_ngd_rst_scs_aarh, rsfmri_cor_ngd_smh_scs_thplh, rsfmri_cor_ngd_rst_scs_vtdcrh, rsfmri_cor_ngd_smh_scs_crcxlh, rsfmri_cor_ngd_smh_scs_cdelh, rsfmri_cor_ngd_smh_scs_ptlh)

#####################
# IN THE BELOW CODE BLOCK
# Rename columns with correct names
#####################

# Make list of new column names
newcolumns <- ""

for (column in cols_of_interest$`name_nda (correct)`) {
  newcolumns <- newcolumns %>%
    paste0(column, sep = "', '")
}
print(newcolumns)

# rename columns
colnames(data) <- c('src_subject_id', 'eventname', 'rsfmri_cor_ngd_au_scs_crcxlh', 'rsfmri_cor_ngd_cerc_scs_crcxlh', 'rsfmri_cor_ngd_copa_scs_crcxlh', 'rsfmri_cor_ngd_df_scs_crcxlh', 'rsfmri_cor_ngd_dsa_scs_crcxlh', 'rsfmri_cor_ngd_fopa_scs_crcxlh', 'rsfmri_cor_ngd_none_scs_crcxlh', 'rsfmri_cor_ngd_rst_scs_crcxlh', 'rsfmri_cor_ngd_sa_scs_crcxlh', 'rsfmri_cor_ngd_smh_scs_crcxlh', 'rsfmri_cor_ngd_smm_scs_crcxlh', 'rsfmri_cor_ngd_vta_scs_crcxlh', 'rsfmri_cor_ngd_vs_scs_crcxlh', 'rsfmri_cor_ngd_au_scs_crcxrh', 'rsfmri_cor_ngd_cerc_scs_crcxrh', 'rsfmri_cor_ngd_copa_scs_crcxrh', 'rsfmri_cor_ngd_df_scs_crcxrh', 'rsfmri_cor_ngd_dsa_scs_crcxrh', 'rsfmri_cor_ngd_fopa_scs_crcxrh', 'rsfmri_cor_ngd_none_scs_crcxrh', 'rsfmri_cor_ngd_rst_scs_crcxrh', 'rsfmri_cor_ngd_sa_scs_crcxrh', 'rsfmri_cor_ngd_smh_scs_crcxrh', 'rsfmri_cor_ngd_smm_scs_crcxrh', 'rsfmri_cor_ngd_vta_scs_crcxrh', 'rsfmri_cor_ngd_vs_scs_crcxrh', 'rsfmri_cor_ngd_au_scs_thplh', 'rsfmri_cor_ngd_cerc_scs_thplh', 'rsfmri_cor_ngd_copa_scs_thplh', 'rsfmri_cor_ngd_df_scs_thplh', 'rsfmri_cor_ngd_dsa_scs_thplh', 'rsfmri_cor_ngd_fopa_scs_thplh', 'rsfmri_cor_ngd_none_scs_thplh', 'rsfmri_cor_ngd_rst_scs_thplh', 'rsfmri_cor_ngd_sa_scs_thplh', 'rsfmri_cor_ngd_smh_scs_thplh', 'rsfmri_cor_ngd_smm_scs_thplh', 'rsfmri_cor_ngd_vta_scs_thplh', 'rsfmri_cor_ngd_vs_scs_thplh', 'rsfmri_cor_ngd_au_scs_thprh', 'rsfmri_cor_ngd_cerc_scs_thprh', 'rsfmri_cor_ngd_copa_scs_thprh', 'rsfmri_cor_ngd_df_scs_thprh', 'rsfmri_cor_ngd_dsa_scs_thprh', 'rsfmri_cor_ngd_fopa_scs_thprh', 'rsfmri_cor_ngd_none_scs_thprh', 'rsfmri_cor_ngd_rst_scs_thprh', 'rsfmri_cor_ngd_sa_scs_thprh', 'rsfmri_cor_ngd_smh_scs_thprh', 'rsfmri_cor_ngd_smm_scs_thprh', 'rsfmri_cor_ngd_vta_scs_thprh', 'rsfmri_cor_ngd_vs_scs_thprh')


#####################
# IN THE BELOW CODE BLOCK
# Rename for readability
#####################

Cerebellar <- data %>%
  select(src_subject_id, eventname, contains('crcx'))
# 26 data columns, 2 id columns

Thalamic <- data %>%
  select(src_subject_id, eventname, contains('thp'))

# Calculate inter-hemispheric means
Cerebellar <- Cerebellar %>%
  mutate(Auditory_Cbm_mean = (rsfmri_cor_ngd_au_scs_crcxlh + rsfmri_cor_ngd_au_scs_crcxrh) / 2,
         CinguloOpercular_Cbm_mean = (rsfmri_cor_ngd_cerc_scs_crcxlh + rsfmri_cor_ngd_cerc_scs_crcxrh) / 2,
         CinguloParietal_Cbm_mean = (rsfmri_cor_ngd_copa_scs_crcxlh + rsfmri_cor_ngd_copa_scs_crcxrh) / 2,
         Default_Cbm_mean = (rsfmri_cor_ngd_df_scs_crcxlh + rsfmri_cor_ngd_df_scs_crcxrh) / 2,
         DorsalAttn_Cbm_mean = (rsfmri_cor_ngd_dsa_scs_crcxlh + rsfmri_cor_ngd_dsa_scs_crcxrh) / 2,
         FrontoParietal_Cbm_mean = (rsfmri_cor_ngd_fopa_scs_crcxlh + rsfmri_cor_ngd_fopa_scs_crcxrh) / 2,
         None_Cbm_mean = (rsfmri_cor_ngd_none_scs_crcxlh + rsfmri_cor_ngd_none_scs_crcxrh) / 2,
         RetrosplenialTemporal_Cbm_mean = (rsfmri_cor_ngd_rst_scs_crcxlh + rsfmri_cor_ngd_rst_scs_crcxrh) / 2,
         Salience_Cbm_mean = (rsfmri_cor_ngd_sa_scs_crcxlh + rsfmri_cor_ngd_sa_scs_crcxrh) / 2,
         SensorimotorHand_Cbm_mean = (rsfmri_cor_ngd_smh_scs_crcxlh + rsfmri_cor_ngd_smh_scs_crcxrh) / 2,
         SensorimotorMouth_Cbm_mean = (rsfmri_cor_ngd_smm_scs_crcxlh + rsfmri_cor_ngd_smm_scs_crcxrh) / 2,
         VentralAttn_Cbm_mean = (rsfmri_cor_ngd_vta_scs_crcxlh + rsfmri_cor_ngd_vta_scs_crcxrh) / 2,
         Visual_Cbm_mean = (rsfmri_cor_ngd_vs_scs_crcxlh + rsfmri_cor_ngd_vs_scs_crcxrh) / 2)

Thalamic <- Thalamic %>%
  mutate(Auditory_Thal_mean = (rsfmri_cor_ngd_au_scs_thplh + rsfmri_cor_ngd_au_scs_thprh) / 2,
         CinguloOpercular_Thal_mean = (rsfmri_cor_ngd_cerc_scs_thplh + rsfmri_cor_ngd_cerc_scs_thprh) / 2,
         CinguloParietal_Thal_mean = (rsfmri_cor_ngd_copa_scs_thplh + rsfmri_cor_ngd_copa_scs_thprh) / 2,
         Default_Thal_mean = (rsfmri_cor_ngd_df_scs_thplh + rsfmri_cor_ngd_df_scs_thprh) / 2,
         DorsalAttn_Thal_mean = (rsfmri_cor_ngd_dsa_scs_thplh + rsfmri_cor_ngd_dsa_scs_thprh) / 2,
         FrontoParietal_Thal_mean = (rsfmri_cor_ngd_fopa_scs_thplh + rsfmri_cor_ngd_fopa_scs_thprh) / 2,
         None_Thal_mean = (rsfmri_cor_ngd_none_scs_thplh + rsfmri_cor_ngd_none_scs_thprh) / 2,
         RetrosplenialTemporal_Thal_mean = (rsfmri_cor_ngd_rst_scs_thplh + rsfmri_cor_ngd_rst_scs_thprh) / 2,
         Salience_Thal_mean = (rsfmri_cor_ngd_sa_scs_thplh + rsfmri_cor_ngd_sa_scs_thprh) / 2,
         SensorimotorHand_Thal_mean = (rsfmri_cor_ngd_smh_scs_thplh + rsfmri_cor_ngd_smh_scs_thprh) / 2,
         SensorimotorMouth_Thal_mean = (rsfmri_cor_ngd_smm_scs_thplh + rsfmri_cor_ngd_smm_scs_thprh) / 2,
         VentralAttn_Thal_mean = (rsfmri_cor_ngd_vta_scs_thplh + rsfmri_cor_ngd_vta_scs_thprh) / 2,
         Visual_Thal_mean = (rsfmri_cor_ngd_vs_scs_thplh + rsfmri_cor_ngd_vs_scs_thprh) / 2)

# Rename for readability
Cerebellar <- Cerebellar %>%
  rename('Auditory <-> LhCbm' = rsfmri_cor_ngd_au_scs_crcxlh,	  # Average correlation between auditory network and ASEG ROI left-cerebellum-cortex
         'Auditory <-> RhCbm' = rsfmri_cor_ngd_au_scs_crcxrh,    # Average correlation between auditory network and ASEG ROI right-cerebellum-cortex
         'Cingulo-Opercular <-> LhCbm' = rsfmri_cor_ngd_cerc_scs_crcxlh,	# Average correlation between cingulo-opercular network and ASEG ROI left-cerebellum-cortex
         'Cingulo-Opercular <-> RhCbm' = rsfmri_cor_ngd_cerc_scs_crcxrh,	# Average correlation between cingulo-opercular network and ASEG ROI right-cerebellum-cortex
         'Cingulo-Parietal <-> LhCbm' = rsfmri_cor_ngd_copa_scs_crcxlh,	# Average correlation between cingulo-parietal network and ASEG ROI left-cerebellum-cortex
         'Cingulo-Parietal <-> RhCbm' = rsfmri_cor_ngd_copa_scs_crcxrh,	# Average correlation between cingulo-parietal network and ASEG ROI right-cerebellum-cortex
         'DefaultMode <-> LhCbm' = rsfmri_cor_ngd_df_scs_crcxlh,  	# Average correlation between default network and ASEG ROI left-cerebellum-cortex
         'DefaultMode <-> RhCbm' = rsfmri_cor_ngd_df_scs_crcxrh,	  # Average correlation between default network and ASEG ROI right-cerebellum-cortex
         'DorsalAttn <-> LhCbm' = rsfmri_cor_ngd_dsa_scs_crcxlh,	  # Average correlation between dorsal attention network and ASEG ROI left-cerebellum-cortex
         'DorsalAttn <-> RhCbm' = rsfmri_cor_ngd_dsa_scs_crcxrh,	  # Average correlation between dorsal attention network and ASEG ROI right-cerebellum-cortex
         'Fronto-Parietal <-> LhCbm' = rsfmri_cor_ngd_fopa_scs_crcxlh,	# Average correlation between fronto-parietal network and ASEG ROI left-cerebellum-cortex
         'Fronto-Parietal <-> RhCbm' = rsfmri_cor_ngd_fopa_scs_crcxrh,	# Average correlation between fronto-parietal network and ASEG ROI right-cerebellum-cortex
         'None <-> LhCbm' = rsfmri_cor_ngd_none_scs_crcxlh,	# Average correlation between none network and ASEG ROI left-cerebellum-cortex
         'None <-> RhCbm' = rsfmri_cor_ngd_none_scs_crcxrh,	# Average correlation between none network and ASEG ROI right-cerebellum-cortex
         'RetrosplenialTemporal <-> LhCbm' = rsfmri_cor_ngd_rst_scs_crcxlh,	  # Average correlation between retrosplenial temporal network and ASEG ROI left-cerebellum-cortex
         'RetrosplenialTemporal <-> RhCbm' = rsfmri_cor_ngd_rst_scs_crcxrh,	  # Average correlation between retrosplenial temporal network and ASEG ROI right-cerebellum-cortex
         'Salience <-> LhCbm' = rsfmri_cor_ngd_sa_scs_crcxlh,	  # Average correlation between salience network and ASEG ROI left-cerebellum-cortex
         'Salience <-> RhCbm' = rsfmri_cor_ngd_sa_scs_crcxrh,	  # Average correlation between salience network and ASEG ROI right-cerebellum-cortex
         'SensoriMotorHand <-> LhCbm' = rsfmri_cor_ngd_smh_scs_crcxlh,	  # Average correlation between sensorimotor hand network and ASEG ROI left-cerebellum-cortex
         'SensoriMotorHand <-> RhCbm' =  rsfmri_cor_ngd_smh_scs_crcxrh,	  # Average correlation between sensorimotor hand network and ASEG ROI right-cerebellum-cortex
         'SensoriMotorMouth <-> LhCbm' = rsfmri_cor_ngd_smm_scs_crcxlh,	  # Average correlation between sensorimotor mouth network and ASEG ROI left-cerebellum-cortex
         'SensoriMotorMouth <-> RhCbm' = rsfmri_cor_ngd_smm_scs_crcxrh,	  # Average correlation between sensorimotor mouth network and ASEG ROI right-cerebellum-cortex
         'VentralAttn <-> LhCbm' = rsfmri_cor_ngd_vta_scs_crcxlh,	  # Average correlation between ventral attention network and ASEG ROI left-cerebellum-cortex
         'VentralAttn <-> RhCbm' = rsfmri_cor_ngd_vta_scs_crcxrh,	  # Average correlation between ventral attention network and ASEG ROI right-cerebellum-cortex
         'Visual <-> LhCbm' = rsfmri_cor_ngd_vs_scs_crcxlh,	  # Average correlation between visual network and ASEG ROI left-cerebellum-cortex
         'Visual <-> RhCbm' = rsfmri_cor_ngd_vs_scs_crcxrh)	  # Average correlation between visual network and ASEG ROI right-cerebellum-cortex

Thalamic <- Thalamic %>%
  rename('Auditory <-> LhThal' = rsfmri_cor_ngd_au_scs_thplh,	  # Average correlation between auditory network and ASEG ROI left-thalamus-proper
         'Auditory <-> RhThal' = rsfmri_cor_ngd_au_scs_thprh,	  # Average correlation between auditory network and ASEG ROI right-thalamus-proper
         'Cingulo-Opercular <-> LhThal' = rsfmri_cor_ngd_cerc_scs_thplh,	# Average correlation between cingulo-opercular network and ASEG ROI left-thalamus-proper
         'Cingulo-Opercular <-> RhThal' = rsfmri_cor_ngd_cerc_scs_thprh,	# Average correlation between cingulo-opercular network and ASEG ROI right-thalamus-proper
         'Cingulo-Parietal <-> LhThal' = rsfmri_cor_ngd_copa_scs_thplh,	# Average correlation between cingulo-parietal network and ASEG ROI left-thalamus-proper
         'Cingulo-Parietal <-> RhThal' = rsfmri_cor_ngd_copa_scs_thprh,	# Average correlation between cingulo-parietal network and ASEG ROI right-thalamus-proper
         'DefaultMode <-> LhThal' = rsfmri_cor_ngd_df_scs_thplh,	  # Average correlation between default network and ASEG ROI left-thalamus-proper
         'DefaultMode <-> RhThal' = rsfmri_cor_ngd_df_scs_thprh,	  # Average correlation between default network and ASEG ROI right-thalamus-proper
         'DorsalAttn <-> LhThal' = rsfmri_cor_ngd_dsa_scs_thplh,	# Average correlation between dorsal attention network and ASEG ROI left-thalamus-proper
         'DorsalAttn <-> RhThal' = rsfmri_cor_ngd_dsa_scs_thprh,	# Average correlation between dorsal attention network and ASEG ROI right-thalamus-proper
         'Fronto-Parietal <-> LhThal' = rsfmri_cor_ngd_fopa_scs_thplh,	# Average correlation between fronto-parietal network and ASEG ROI left-thalamus-proper
         'Fronto-Parietal <-> RhThal' = rsfmri_cor_ngd_fopa_scs_thprh,	# Average correlation between fronto-parietal network and ASEG ROI right-thalamus-proper
         'None <-> LhThal' = rsfmri_cor_ngd_none_scs_thplh,	# Average correlation between none network and ASEG ROI left-thalamus-proper
         'None <-> RhThal' = rsfmri_cor_ngd_none_scs_thprh,	# Average correlation between none network and ASEG ROI right-thalamus-proper
         'RetrosplenialTemporal <-> LhThal' = rsfmri_cor_ngd_rst_scs_thplh,	# Average correlation between retrosplenial temporal network and ASEG ROI left-thalamus-proper
         'RetrosplenialTemporal <-> RhThal' = rsfmri_cor_ngd_rst_scs_thprh,	# Average correlation between retrosplenial temporal network and ASEG ROI right-thalamus-proper
         'Salience <-> LhThal' = rsfmri_cor_ngd_sa_scs_thplh,	  # Average correlation between salience network and ASEG ROI left-thalamus-proper
         'Salience <-> RhThal' = rsfmri_cor_ngd_sa_scs_thprh,	  # Average correlation between salience network and ASEG ROI right-thalamus-proper
         'SensoriMotorHand <-> LhThal' =  rsfmri_cor_ngd_smh_scs_thplh,	# Average correlation between sensorimotor hand network and ASEG ROI left-thalamus-proper
         'SensoriMotorHand <-> RhThal' =  rsfmri_cor_ngd_smh_scs_thprh,	# Average correlation between sensorimotor hand network and ASEG ROI right-thalamus-proper
         'SensoriMotorMouth <-> LhThal' =  rsfmri_cor_ngd_smm_scs_thplh,	# Average correlation between sensorimotor mouth network and ASEG ROI left-thalamus-proper
         'SensoriMotorMouth <-> RhThal' =  rsfmri_cor_ngd_smm_scs_thprh,	# Average correlation between sensorimotor mouth network and ASEG ROI right-thalamus-proper
         'VentralAttn <-> LhThal' = rsfmri_cor_ngd_vta_scs_thplh,	# Average correlation between ventral attention network and ASEG ROI left-thalamus-proper
         'VentralAttn <-> RhThal' = rsfmri_cor_ngd_vta_scs_thprh,	# Average correlation between ventral attention network and ASEG ROI right-thalamus-proper
         'Visual <-> LhThal' = rsfmri_cor_ngd_vs_scs_thplh,	  # Average correlation between visual network and ASEG ROI left-thalamus-proper
         'Visual <-> RhThal' = rsfmri_cor_ngd_vs_scs_thprh)	  # Average correlation between visual network and ASEG ROI right-thalamus-proper

#####################
# IN THE BELOW CODE BLOCK
# QC these data
#####################

# Keep useful columns
rs_qc_data <- rs_qc_data %>%
  select(src_subject_id, eventname, imgincl_rsfmri_include) %>%
  unique()

# integrate qc data
Thalamic_QC <- Thalamic %>%
  left_join(rs_qc_data, by = c('src_subject_id', 'eventname'))

Cerebellar_QC <- Cerebellar %>%
  left_join(rs_qc_data, by = c('src_subject_id', 'eventname'))

# count subjects
Thal_num <- Thalamic_QC %>% distinct(src_subject_id) %>% pull(src_subject_id) # 11616 subjects, 252 with no data
Cbm_num <- Cerebellar_QC %>% distinct(src_subject_id) %>% pull(src_subject_id) # 11616 subjects, 252 with no data

# sanity check -- do these 252 have no data?
all_subs <- read_csv(paste0(sourcepath, 'core/abcd-general/abcd_p_demo.csv')) %>% distinct(src_subject_id) %>% pull(src_subject_id) # 11868 total subjects
allegedly_missing_thal <- setdiff(all_subs, Thal_num)
allegedly_missing_cbm <- setdiff(all_subs, Cbm_num)
has_rsfmri <- imaging_data %>% distinct(src_subject_id) %>% pull(src_subject_id) # 11616 distinct subjects
actual_thal_missing <- setdiff(allegedly_missing_thal, has_rsfmri) # 252 confirmed, no data
actual_cbm_missing <- setdiff(allegedly_missing_cbm, has_rsfmri) # 252 confirmed, no data
# redownloaded rsfmri data, confirmed not a download error.

### remove unusable data
# before: 22130
Thalamic_QC <- Thalamic_QC %>%
  filter(imgincl_rsfmri_include == 1 | is.na(imgincl_rsfmri_include))
# after: 19764

# before: 22130
Cerebellar_QC <- Cerebellar_QC %>%
  filter(imgincl_rsfmri_include == 1 | is.na(imgincl_rsfmri_include))
# after: 19764

# count subjects
Thal_num_ex <- Thalamic_QC %>% distinct(src_subject_id) %>% pull(src_subject_id) # 10876 subjects, 740 removed
Cbm_num_ex <- Cerebellar_QC %>% distinct(src_subject_id) %>% pull(src_subject_id) # 10876 subjects, 740 removed

# sanity check that the 740 had no usable data
removed_thal_subs <- setdiff(has_rsfmri, Thal_num_ex)
removed_cbm_subs <- setdiff(has_rsfmri, Cbm_num_ex)
Thal_check <- rs_qc_data %>% filter(src_subject_id %in% removed_thal_subs)
Cbm_check <- rs_qc_data %>% filter(src_subject_id %in% removed_cbm_subs)
# All had no usable data, except 3. 
# These 3 subjects: NDAR_INV2K3JH38W, NDAR_INVAYCZM73P, NDAR_INVPA1L60YE had 2 timepoints of qc data, 1 usable 1 unusable, with no resting-state connectivity data for the usable scans.
# rsfmri data were redownloaded, confirmed not a download error.

# reorder to put means first
Cerebellar_QC <- Cerebellar_QC %>%
  select(src_subject_id, eventname, contains("mean"), everything())
Thalamic_QC <- Thalamic_QC %>%
  select(src_subject_id, eventname, contains("mean"), everything())

#####################
# IN THE BELOW CODE BLOCK
# Write to CSV
#####################

write_csv(Cerebellar_QC, paste0(derivative_data, 'rsFMRI_5.0_Cerebellum.csv'))
write_csv(Thalamic_QC, paste0(derivative_data, 'rsFMRI_5.0_Thalamus.csv'))


####### get Male/Female counts
sex <- read_csv(paste0(sourcepath, 'core/abcd-general/abcd_p_demo.csv')) %>% select(src_subject_id, demo_sex_v2) %>% filter(!is.na(demo_sex_v2)) %>% unique()
cbm_sex_count <- Cerebellar_QC %>%
  left_join(sex, by = 'src_subject_id') %>%
  select(src_subject_id, demo_sex_v2) %>%
  unique()
thal_sex_count <- Thalamic_QC %>%
  left_join(sex, by = 'src_subject_id') %>%
  select(src_subject_id, demo_sex_v2) %>%
  unique()

table(cbm_sex_count$demo_sex_v2) # 5590 M, 5283 F, 3 IM
table(thal_sex_count$demo_sex_v2)# 5590 M, 5283 F, 3 IM

missing_thal <- sex %>% filter(src_subject_id %in% actual_thal_missing)
table(missing_thal$demo_sex_v2) # 133 M 119 F
missing_cbm <- sex %>% filter(src_subject_id %in% actual_cbm_missing)
table(missing_cbm$demo_sex_v2) # 133 M 119 F

removed_cbm <- sex %>% filter(src_subject_id %in% removed_cbm_subs)
table(removed_cbm$demo_sex_v2) # 465 M 275 F
removed_thal <- sex %>% filter(src_subject_id %in% removed_thal_subs)
table(removed_thal$demo_sex_v2) # 465 M 275 F
