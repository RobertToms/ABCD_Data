##########################################################################
# Female_6.0_Volumes.sh
# Author: Robert Toms
# Date: 9/9/2025
# Path: /path/to/Post_Processing/Female_6.0_Volumes.sh
##########################################################################

SUBJECTS_DIR=/path/to/ABCD_6.0/mri_data/derivative/freesurfer/freesurfer8.0.0
cd $SUBJECTS_DIR

NiiList=()
VolumesFile=/path/to/Post_Processing/Female_6.0_Volumes.csv
FullListFile=/path/to/FreeSurfer_Scripts/LargeNiftiList.txt
BatchReport=/path/to/FreeSurfer_Scripts/Batch_Report.txt

######################################################
# Read each line from the txt file, get list of scans
######################################################
echo ""

while IFS= read -r file; do
	
	# Isolate subject ID and visit
	sub=$(echo "$file" | cut -d'_' -f1)
	visit=$(echo "$file" | cut -d'_' -f2)
	# Add to TargetBatch Array
	NiiList+=("${sub}"^"${visit}"$'\n')
	echo "Retrieving $sub's Longitudinal Hippo subvolumes for $visit."
	echo ""
done < $FullListFile
#done < /path/to/Post_Processing/Processed_Niftis.txt

#echo ${NiiList[@]}

###############################################
# Locate Volumes Files, Compile Error Messages
###############################################

# For Error messages, take note of which subject IDs get returned at the end. Some may have usable hippocampal subvolumes, while others will not. We ran into a bug in line 2136/2137 of SegmentSubfieldsT1Longitudinal.m, where labelling of the hippocampal molecular layer was throwing a critical error after subvolumes had been determined and written into a temporary location-- this code finds and uses them, if they exist, and lets you know if they do not.

ErrorMessages=""
tpcount=0
tpvar="null"

# If you want a spacer in the Volumes csv to delineate the batch that is getting written in from the pre-existing volumes
#echo "" >> $VolumesFile

for timepoint in ${NiiList[@]}; do
	sub=$(echo "$timepoint" | cut -d'^' -f1)
	visit=$(echo "${timepoint}" | cut -d'^' -f2)

	# if it's the same subject as before
	if [ $sub == $tpvar ]; then
		tpcount=$((tpcount + 1))
	else 
		tpcount=1
		tpvar="$sub"
	fi
	
	# Check for Left Hemisphere Volumes File
  	lh_VolumesFile="${SUBJECTS_DIR}/${sub}/${sub}_${visit}.long.${sub}_long/mri/lh.hippoSfVolumes-T1.long.v22.txt"
  	if [ ! -f $lh_VolumesFile ]; then
  		ErrorMessages+="$sub $visit Left Hemisphere main file missing, incomplete volumes. Skipping..."$'\n'
  		continue
  	fi
  	
  	# Check for Right Hemisphere Volumes File
  	rh_VolumesFile="${SUBJECTS_DIR}/${sub}/${sub}_${visit}.long.${sub}_long/mri/rh.hippoSfVolumes-T1.long.v22.txt"
  	if [ ! -f $rh_VolumesFile ]; then
  		ErrorMessages+="Right Hemisphere Missing: ${sub}, ${visit}. Attempting second location."$'\n'
  		# Check if finished but not moved
  		rh_VolumesFile_backup="${SUBJECTS_DIR}/${sub}/${sub}_long/tmp/hippoSF_T1_long.v22_right/volumesHippo_tp_${tpcount}.txt"
  		# If Right hemisphere errored and doesn't have volumes
  		if [ ! -f $rh_VolumesFile_backup ]; then
	  		ErrorMessages+="      $sub's RH Hippo subvolumes for $visit DO NOT EXIST."$'\n'
	  		echo ""
	  		continue
	  	# If right hemisphere errored but has volumes
	  	else rh_VolumesFile=$rh_VolumesFile_backup
	  		ErrorMessages+="      Backup Located -- $sub $visit"$'\n'
	  		echo "$rh_VolumesFile"
	  		echo "$rh_VolumesFile_backup"
	  	fi
	  	ErrorMessages+=" "$'\n'
	fi
	
	####################################
  	# Isolate Numbers from VolumesFiles
  	####################################
  	
  	# Left Hemisphere
  	Clean_lh_volumes=""
  	echo ""
  	echo ""
  	echo "###### $sub $visit | TPCount:$tpcount ######"
  	echo "Left Hemisphere"
  	while IFS= read -r file; do
		# Isolate subregion label and volume measurement
		label=$(echo "$file" | cut -d' ' -f1)
		num=$(echo "$file" | cut -d' ' -f2)
		# Add to TargetBatch Array
		Clean_lh_volumes+=",${num}"
		echo "$sub $label's lh_Subvolume: $num."
	done < $lh_VolumesFile
  	echo " "
  	
  	# Right Hemisphere
  	echo "Right Hemisphere"
  	Clean_rh_volumes=""
  	while IFS= read -r file; do
		# Isolate subregion label and volume measurement
		label=$(echo "$file" | cut -d' ' -f1)
		num=$(echo "$file" | cut -d' ' -f2)
		# Add to TargetBatch Array
		Clean_rh_volumes+=",${num}"
		echo "$sub $label's rh_Subvolume: $num."
	done < $rh_VolumesFile
  	
  	#####################################
  	# Write Volumes to Csv
  	#####################################
  	
	# Combine Volumes into 1 string
	row="$sub,$visit"
	row+="$Clean_lh_volumes"
	row+="$Clean_rh_volumes"
	# Paste into csv
	printf '%s\n' "$row" >> $VolumesFile

	######################
	# Reset Volumes Files
	######################
	lh_VolumesFile=""
	rh_VolumesFile=""
	
done

###################################
# Print Error Messages to Terminal
###################################

echo ""
echo "###### ERROR MESSAGES ######"
echo ""
printf "%s" "$ErrorMessages"
printf "%s" "$ErrorMessages" >> $BatchReport
