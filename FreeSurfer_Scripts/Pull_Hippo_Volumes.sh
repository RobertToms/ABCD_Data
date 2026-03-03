##########################################################################
# Pull_Hippo_Volumes.sh
# Author: Robert Toms
# Date: 1/12/2025
# Path: ~/dammi/Sandbox/Pull_Hippo_Volumes.sh
##########################################################################


Volumes_File=~/dammi/Sandbox/Saturn_Volumes.csv

# Set Subjects Directory to where all derivative data is located
#SUBJECTS_DIR=~/dammi/ABCD_6.0/mri_data/derivative/freesurfer/freesurfer8.0.0
SUBJECTS_DIR=~/dammi/Sandbox/Saturn_derivative

cd $SUBJECTS_DIR
echo ""
echo "SUBJECTS_DIR:"
pwd
echo ""
echo "Building List of Subjects..."
echo ""

#########################################################
############# Build List of sub-... Dirs ################
#########################################################

# get list of all directories in the current directory (SUBJECTS_DIR) that are for each individual subject
subjects_list=$(find . -maxdepth 1 -type d -name "sub*")
total_subjects=$(find . -maxdepth 1 -type d -name "sub*" | wc -l)

# shave off "./" from each one
for sub in $subjects_list; do
	ID=$(echo $sub | cut -d'/' -f2)
	subjects_list_clean+="$ID"$'\n'
done

echo "SUBJECTS TO SEARCH:"
#echo $subjects_list_clean
printf "%s" "$subjects_list_clean"
echo ""
echo "$total_subjects Total Subjects"
echo ""

##############################################################
############# Search Through and Pull Volumes ################
##############################################################

Pre_Seg_Errors=""
Seg_Errors=""
Completed=""
subject_num=0
seg_error_num=0
pre_seg_error_num=0
total_tps=0

echo "#########################################"
echo "###### FINDING AND PULLING VOLUMES ######"
echo "#########################################"
echo ""

echo "participant_id,session_id,left_hippocampal_tail,left_subiculum_body,left_ca1_body,left_subiculum_head,left_hippocampal_fissure,left_presubiculum_head,left_ca1_head,left_presubiculum_body,left_parasubiculum,left_molecular_layer_hp_head,left_molecular_layer_hp_body,left_gc_ml_dg_head,left_ca3_body,left_gc_ml_dg_body,left_ca4_head,left_ca4_body,left_fimbria,left_ca3_head,left_hata,left_whole_hippocampal_body,left_whole_hippocampal_head,left_whole_hippocampus,right_hippocampal_tail,right_subiculum_body,right_ca1_body,right_subiculum_head,right_hippocampal_fissure,right_presubiculum_head,right_ca1_head,right_presubiculum_body,right_parasubiculum,right_molecular_layer_hp_head,right_molecular_layer_hp_body,right_gc_ml_dg_head,right_ca3_body,right_gc_ml_dg_body,right_ca4_head,right_ca4_body,right_fimbria,right_ca3_head,right_hata,right_whole_hippocampal_body,right_whole_hippocampal_head,right_whole_hippocampus" > $Volumes_File

for sub in $subjects_list_clean; do
	
	((subject_num++))
	
	# enter subject's derivative directory
	cd $SUBJECTS_DIR/$sub
	#pwd
	
	#################################
	#### GET SESSION INFORMATION ####
	
	# make list of sessions for this subject, in session order
	directories=$(find . -maxdepth 1 -type d -name "sub*A" | sort -t- -k2,2n)
	
	for ses in $directories; do
		session=$(echo $ses | cut -d'_' -f2)
		sessions+="$session"$'\n'
	done
	
	# count number of session for this subject
	num_sessions=$(find . -maxdepth 1 -type d -name "sub*A" | wc -l)
	
	echo ""
	echo ""
	echo "####################################"
	echo "# Pulling $subject_num of $total_subjects"
	echo "# $sub: $num_sessions timepoints found"
	echo "#" $sessions
	
	total_tps=$((total_tps + num_sessions))
	
	#########################################
	#### Search Through and Pull Volumes ####
	
	tp_num=1
	
	# iterate through each session
	for session in $sessions; do
				
		# set volumes file locations
		lh_VolumesFile="${SUBJECTS_DIR}/${sub}/${sub}_${session}.long.${sub}_long/mri/lh.hippoSfVolumes-T1.long.v22.txt"
		rh_VolumesFile="${SUBJECTS_DIR}/${sub}/${sub}_${session}.long.${sub}_long/mri/rh.hippoSfVolumes-T1.long.v22.txt"
	
		#########################
		# CHECK FOR ERRORS, FIND CORRECT VOLUMES FILE
		# Left Hemisphere File
		if [ ! -f $lh_VolumesFile ]; then
  			Pre_Seg_Errors+="$sub"$'\n'
  			((pre_seg_error_num++))
  			sessions=""
			continue
		fi
  	
	  	# Right Hemisphere File
  		if [ ! -f $rh_VolumesFile ]; then
  			echo "Right Hemisphere Missing: ${sub}, ${visit}. Attempting second location."
  			
  			# Check alternative location -- if due to the MATLAB bug on line 2136/2137 of SegmentSubfieldsT1Longitudinal.m, may have usable volumes located here
  			rh_VolumesFile="${SUBJECTS_DIR}/${sub}/${sub}_long/tmp/hippoSF_T1_long.v22_right/volumesHippo_tp_${tp_num}.txt"
  			if [ ! -f $rh_VolumesFile ]; then
		 	 	echo "      True Segmentation Error: $sub $visit."
		 	 	Seg_Errors+="$sub $session"$'\n'
		 	 	((seg_error_num++))
		 	 	continue
		 	else
		 		echo "	File Found."
	  		fi
  		fi
  		
  		####################
  		# PULL VOLUMES
  		
  		# Left Hemisphere
  		Clean_lh_volumes=""
	  	echo ""
  		echo "###### $sub $session | TPCount:$tp_num ######"
  		echo "Left Hemisphere"
  		while IFS= read -r file; do
			# Isolate subregion label and volume measurement
			label=$(echo "$file" | cut -d' ' -f1)
			num=$(echo "$file" | cut -d' ' -f2)
			# Add to TargetBatch Array
			Clean_lh_volumes+=",${num}"
			echo "$sub $label's lh_Subvolume: $num."
		done < $lh_VolumesFile
		
		# Right Hemisphere
  		echo " "
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
  		
  		######################
  		# WRITE INTO CSV
  		
		# Combine Volumes into 1 string
		row="$sub,$session"
		row+="$Clean_lh_volumes"
		row+="$Clean_rh_volumes"
		# Paste into csv
		printf '%s\n' "$row" >> $Volumes_File
  		
		Completed+="$sub"$'\n'
  		######################
  		# Set Up for Next Session
  		
  		# increment tp_num
		((tp_num++))
		
		# reset volumes files
		lh_VolumesFile=""
		rh_VolumesFile=""
		
		# reset sessions list
		sessions=""
		
	done
done

# Make Pre-Segmentation Errors List only repeat subject IDs once
Pre_Seg_Clean=$(printf "%s\n" "$Pre_Seg_Errors" | sort -u)

pre_seg_sub_num=$(printf "%s\n" "$Pre_Seg_Clean" | cut -d' ' -f1 | sort -u | wc -l)
seg_sub_num=$(printf "%s\n" "$Seg_Errors" | cut -d' ' -f1 | sort -u | wc -l)
complete_num=$(printf "%s\n" "$Completed" | cut -d' ' -f1 | sort -u | wc -l)

# PRINT ERROR MESSAGES	
echo ""
echo "##########################################"
echo "############ ERROR MESSAGES ##############"
echo ""
echo "PRE-SEGMENTATION ERRORS:"
printf "%s\n" "$Pre_Seg_Clean"
echo ""
echo ""
echo "SEGMENTATION ERRORS:"
echo ""
printf "%s" "$Seg_Errors"
echo ""
echo "Total Number of Longitudinal Timepoints: $total_tps"
echo "Segmentation Error Subjects: $seg_sub_num"
echo "Pre-Segmentation Error Subjects: $pre_seg_sub_num"
echo "Total Error Subjects: $((seg_sub_num + pre_seg_sub_num))"
echo ""
echo "Completed TEST: $complete_num"
echo ""
echo "All Done!"

