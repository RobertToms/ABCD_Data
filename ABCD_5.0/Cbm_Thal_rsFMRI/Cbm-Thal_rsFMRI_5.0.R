###########################
# Cbm-Thal_rsFMRI_5.0.R
# Author: Robert Toms
# Date: 10/9/2025
###########################

library(tidyverse)

sourcepath <- "/path/to/ABCD_5.0/core/"
derivative_data <- "/path/to/derivative_data/5.0/"

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
  select(src_subject_id, eventname, imgincl_rsfmri_include)

# integrate qc data
Thalamic_QC <- Thalamic %>%
  left_join(rs_qc_data, by = c('src_subject_id', 'eventname'))

Cerebellar_QC <- Cerebellar %>%
  left_join(rs_qc_data, by = c('src_subject_id', 'eventname'))

### remove unusable data
# before: 22134
Thalamic_QC <- Thalamic_QC %>%
  filter(imgincl_rsfmri_include == 1 | is.na(imgincl_rsfmri_include))
# after: 19766

# before: 22134
Cerebellar_QC <- Cerebellar_QC %>%
  filter(imgincl_rsfmri_include == 1 | is.na(imgincl_rsfmri_include))
# after: 19766

#####################
# IN THE BELOW CODE BLOCK
# Write to CSV
#####################

write_csv(Cerebellar_QC, paste0(derivative_data, 'rsFMRI_5.0_Cerebellum.csv'))
write_csv(Thalamic_QC, paste0(derivative_data, 'rsFMRI_5.0_Thalamus.csv'))
