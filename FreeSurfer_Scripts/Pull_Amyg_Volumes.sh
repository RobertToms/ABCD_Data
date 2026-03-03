##########################################################################
# Pull_Amyg_Volumes.sh
# Author: Robert Toms
# Date: 12/18/2025
# Path: ~/dammi/Sandbox/Pull_Amyg_Volumes.sh
##########################################################################


Volumes_File=~/dammi/Sandbox/6.0_Amyg_Subvolumes.csv

# Set Subjects Directory to where all derivative data is located
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
subjects_list=$(find . -maxdepth 1 -type d -name "sub*" | sort -u)
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

echo "participant_id,session_id,lh_Lateral-nucleus,lh_Basal-nucleus,lh_Accessory-Basal-nucleus,lh_Anterior-amygdaloid-area-AAA,lh_Central-nucleus,lh_Medial-nucleus,lh_Cortical-nucleus,lh_Corticoamygdaloid-transition,lh_Paralaminar-nucleus,lh_Whole_amygdala,rh_Lateral-nucleus,rh_Basal-nucleus,rh_Accessory-Basal-nucleus,rh_Anterior-amygdaloid-area-AAA,rh_Central-nucleus,rh_Medial-nucleus,rh_Cortical-nucleus,rh_Corticoamygdaloid-transition,rh_Paralaminar-nucleus,rh_Whole_amygdala" > $Volumes_File


Pre_Seg_Errors=""
Seg_Errors=""
subject_num=0

seg_error_num=0
pre_seg_error_num=0
total_tps=0

echo "#########################################"
echo "###### FINDING AND PULLING VOLUMES ######"
echo "#########################################"
echo ""

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
		lh_VolumesFile="${SUBJECTS_DIR}/${sub}/${sub}_${session}.long.${sub}_long/mri/lh.amygNucVolumes-T1.long.v22.txt"
		rh_VolumesFile="${SUBJECTS_DIR}/${sub}/${sub}_${session}.long.${sub}_long/mri/rh.amygNucVolumes-T1.long.v22.txt"
	
		#########################
		# CHECK FOR ERRORS, FIND CORRECT VOLUMES FILE
		# Left Hemisphere File
		if [ ! -f $lh_VolumesFile ]; then
  			Pre_Seg_Errors+="$sub"$'\n'
  			((pre_seg_error_num++))
  			if [ $tp_num -eq $num_sessions ]; then
  				sessions=""
  				echo ""
	  			echo "BASE Log Tail:"
  				tail -n 9 ${SUBJECTS_DIR}/${sub}/${sub}_long/scripts/recon-all.log
  			else ((tp_num++))
  			fi
			continue
		fi
  	
	  	# Right Hemisphere File
  		if [ ! -f $rh_VolumesFile ]; then
  			echo "Right Hemisphere Missing: ${sub}, ${visit}. Attempting second location."
  			
  			# Check alternative location -- if due to the MATLAB bug on line 2136/2137 of SegmentSubfieldsT1Longitudinal.m, may have usable volumes located here
  			rh_VolumesFile="${SUBJECTS_DIR}/${sub}/${sub}_long/tmp/hippoSF_T1_long.v22_right/volumesAmygdala_tp_${tp_num}.txt"
  			if [ ! -f $rh_VolumesFile ]; then
		 	 	echo "      True Segmentation Error: $sub $visit."
		 	 	Seg_Errors+="$sub $session"$'\n'
		 	 	((seg_error_num++))
		 		if [ $tp_num -eq $num_sessions ]; then
  					sessions=""
  				else ((tp_num++))
  				fi
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

# PRINT ERROR MESSAGES	
#echo ""
#echo "##########################################"
#echo "############ ERROR MESSAGES ##############"
#echo ""
#echo "PRE-SEGMENTATION ERRORS:"
#printf "%s\n" "$Pre_Seg_Clean"
#echo ""
#echo ""
#echo "SEGMENTATION ERRORS:"
#echo ""
#printf "%s" "$Seg_Errors"
#echo ""
#echo "All Done!"




echo ""
echo "##########################################"
echo "############### MESSAGES #################"
echo ""
echo "Total Subs: $total_subjects"
echo ""
echo "PRE-SEGMENTATION / LEFT HEMISPHERE ERRORS: $pre_seg_sub_num subjects, $pre_seg_error_num total"
printf "%s\n" "$Pre_Seg_Clean"
echo ""
echo ""
echo "SEGMENTATION ERRORS: $seg_sub_num subjects, $seg_error_num total"
echo ""
printf "%s" "$Seg_Errors"

echo ""
echo "Total Subjects: $total_subjects"
echo "Total Number of Longitudinal Timepoints: $total_tps"
echo "Segmentation Error Subjects: $seg_sub_num"
echo "Pre-Segmentation Error Subjects: $pre_seg_sub_num"
echo "Total Error Subjects: $((seg_sub_num + pre_seg_sub_num))"
echo ""
date
echo ""
echo "All Done!"
echo ""
