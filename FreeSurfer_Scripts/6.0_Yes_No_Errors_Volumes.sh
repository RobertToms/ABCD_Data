##########################################################################
# 6.0_Yes_No_Errors_Volumes.sh
# Author: Robert Toms
# Date: 1/8/2025
# Path: ~/dammi/Sandbox/6.0_Yes_No_Errors_Volumes.sh
##########################################################################

## This Script searches the target SUBJECTS_DIR to build you a csv 
# of subjects that details who is missing hippocampal subvolume data, 
# from what timepoints, and the stage at which they likely errored out.


####### HERE IS R CODE TO ANALYZE THE OUTPUT FILE ######
#errs_vols <- read_csv('~/dammi/Sandbox/6.0_Errors_or_Volumes.csv')

#colnames(errs_vols)
#table(errs_vols$error_type)
#table(errs_vols$error_stage)
#table(errs_vols$error_stage, errs_vols$problem_tp)
#table(errs_vols$error_type)

#errs_vols <- errs_vols %>%
#  mutate(missing_vols = (sessions_run != sessions_with_vols))

#table(errs_vols$missing_vols)






Output_File=~/dammi/Sandbox/6.0_Errors_or_Volumes.csv
Error_Messages=~/dammi/Sandbox/error_messages.txt

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

#########################################################################
############# Search Through and Check For Volumes Files ################
#########################################################################

# Initialize Output File
row="participant_id,expected_sessions,sessions_run,sessions_with_vols,ses-00A,ses-02A,ses-03A,ses-04A,ses-06A,error_type,problem_tp,error_stage"
# Paste into csv
printf '%s\n' "$row" >> $Output_File


Pre_Seg_Errors=""
Seg_Errors=""
subject_num=0

echo "###################################"
echo "###### FINDING VOLUMES FILES ######"
echo "###################################"
echo ""

for sub in $subjects_list_clean; do
	
	ses0=' '
	ses2=' '
	ses3=' '
	ses4=' '
	ses6=' '
	problem_tp=''
	error_stage=' '
	
	((subject_num++))
	
	# Find number of expected sessions
	cd /mnt/md0/ScratchPad_/ABCD/ABCD_6.0/mri_data/$sub
	raw_sessions=$(find . -maxdepth 1 -type d -name "ses-*" | wc -l)

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
	echo "# $subject_num of $total_subjects"
	echo "# $sub: $num_sessions of $raw_sessions timepoints found"
	echo "#" $sessions
	echo ""
	
	#########################################
	#### Search Through and Pull Volumes ####
	
	tp_num=1
	has_vols_num=0
	
	# iterate through each session
	for session in $sessions; do
		
		error_type=" "
		
		# set volumes file locations
		lh_VolumesFile="${SUBJECTS_DIR}/${sub}/${sub}_${session}.long.${sub}_long/mri/lh.hippoSfVolumes-T1.long.v22.txt"
		rh_VolumesFile="${SUBJECTS_DIR}/${sub}/${sub}_${session}.long.${sub}_long/mri/rh.hippoSfVolumes-T1.long.v22.txt"
	
		#########################
		# CHECK FOR ERRORS, FIND CORRECT VOLUMES FILE
		# Left Hemisphere File
		if [ ! -f $lh_VolumesFile ]; then
  			Pre_Seg_Errors+="$sub"$'\n'
  			error_type='Pre-Seg'
  			
  			echo ""
  			echo "SESSION $session"
  			tail -n 3 ${SUBJECTS_DIR}/${sub}/${sub}_${session}/scripts/recon-all.log
  			
  			# check if this is the problematic timepoint, and at which stage
  			cross_log=$(tail -n 3 ${SUBJECTS_DIR}/${sub}/${sub}_${session}/scripts/recon-all.log)
  			base_log=$(tail -n 3 ${SUBJECTS_DIR}/${sub}/${sub}_long/scripts/recon-all.log)
  			long_log=$(tail -n 3 ${SUBJECTS_DIR}/${sub}/${sub}_${session}.long.${sub}_long/scripts/recon-all.log)
			if echo "$cross_log" | grep -q "ERRORS"; then
				problem_tp="$session"
				error_stage='cross'

	 			echo "" >> $Error_Messages
	  			echo "SESSION $session" >> $Error_Messages
	  			echo "$cross_log" >> $Error_Messages
			fi
			if [ "$error_stage" == " " ]; then
				if echo "$base_log" | grep -q "ERRORS"; then
					error_stage="base"

	 				echo "" >> $Error_Messages
		  			echo "SESSION $session" >> $Error_Messages
		  			echo "$base_log" >> $Error_Messages
				fi
			fi
			if [ "$error_stage" == " " ]; then
				if echo "$long_log" | grep -q "ERRORS"; then
					problem_tp="$session"
					error_stage="long"

		 			echo "" >> $Error_Messages
		  			echo "SESSION $session" >> $Error_Messages
		  			echo "$long_log" >> $Error_Messages
				fi
			fi
				
  			if [ $tp_num -eq $num_sessions ]; then
  				sessions=""
  				
  				echo "" >> $Error_Messages
	  			echo "BASE Log Tail:" >> $Error_Messages
  				tail -n 7 ${SUBJECTS_DIR}/${sub}/${sub}_long/scripts/recon-all.log >> $Error_Messages
  				  				
  				echo ""
	  			echo "BASE Log Tail:"
  				tail -n 7 ${SUBJECTS_DIR}/${sub}/${sub}_long/scripts/recon-all.log
  				
  			else ((tp_num++))
  			fi
  			if [ $session == 'ses-00A' ]; then
				ses0='0'
			elif [ $session == 'ses-02A' ]; then
  				ses2='0'
			elif [ $session == 'ses-03A' ]; then
  				ses3='0'
			elif [ $session == 'ses-04A' ]; then
				ses4='0'
			elif [ $session == 'ses-06A' ]; then  		
  				ses6='0'
  			fi
  			
			continue
		fi
  	
	  	# Right Hemisphere File
  		if [ ! -f $rh_VolumesFile ]; then
  			echo "Right Hemisphere Missing: ${sub}, ${session}. Attempting second location."
  			
  			# Check alternative location -- if due to the MATLAB bug on line 2136/2137 of SegmentSubfieldsT1Longitudinal.m, may have usable volumes located here
  			rh_VolumesFile="${SUBJECTS_DIR}/${sub}/${sub}_long/tmp/hippoSF_T1_long.v22_right/volumesHippo_tp_${tp_num}.txt"
  			if [ ! -f $rh_VolumesFile ]; then
		 	 	echo "      True Segmentation Error: $sub $session."
 	  			error_type='Seg'
		 	 	Seg_Errors+="$sub $session"$'\n'
		 	 	  	if [ $session == 'ses-00A' ]; then
			  			ses0='0'
			  		elif [ $session == 'ses-02A' ]; then
  						ses2='0'
			  		elif [ $session == 'ses-03A' ]; then
  						ses3='0'
			  		elif [ $session == 'ses-04A' ]; then
						ses4='0'
			  		elif [ $session == 'ses-06A' ]; then  		
  						ses6='0'
  					fi
		 		if [ $tp_num -eq $num_sessions ]; then
  					sessions=""
  				else ((tp_num++))
  				fi
		 	 	continue
		 	else
		 		echo "	File Found."
	  		fi
  		fi
  		
  		######################
  		# SIGNAL VALID VOLUMES
  		
  		echo "$session LH volumes found"
  		echo "$session RH volumes found" 
  		((has_vols_num++))		
  		
  		######################
  		# SIGNAL IF SPECIFIC SESSIONS HAVE VOLUMES
  		
  		if [ $session == 'ses-00A' ]; then
  			ses0='1'
  		elif [ $session == 'ses-02A' ]; then
  			ses2='1'
  		elif [ $session == 'ses-03A' ]; then
  			ses3='1'
  		elif [ $session == 'ses-04A' ]; then
			ses4='1'
  		elif [ $session == 'ses-06A' ]; then  		
  			ses6='1'
  		fi
  					
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
	
  	######################
  	# WRITE INTO CSV
  row="participant_id,expected_sessions,sessions_run,sessions_with_vols,ses-00A,ses-02A,ses-03A,ses-04A,ses-06A,error_type,problem_tp,error_stage"
	# Combine Volumes into 1 string
	echo ""
	echo "$row"
	row="$sub,$raw_sessions,$num_sessions,$has_vols_num,$ses0,$ses2,$ses3,$ses4,$ses6,$error_type,$problem_tp,$error_stage"
	echo "$row"
	# Paste into csv
	printf '%s\n' "$row" >> $Output_File
done

# Make Pre-Segmentation Errors List only repeat subject IDs once
Pre_Seg_Clean=$(printf "%s\n" "$Pre_Seg_Errors" | sort -u)
Seg_Clean=$(printf "%s\n" "$Seg_Errors" | sort -u)

pre_seg_sub_num=$(printf "%s\n" "$Pre_Seg_Clean" | cut -d' ' -f1 | sort -u | wc -l)
seg_sub_num=$(printf "%s\n" "$Seg_Errors" | cut -d' ' -f1 | sort -u | wc -l)

((pre_seg_sub_num--))
((seg_sub_num--))

# PRINT ERROR MESSAGES	
echo ""
echo "##########################################"
echo "############ ERROR MESSAGES ##############"
echo ""
echo "PRE-SEGMENTATION ERRORS: $pre_seg_sub_num subjects"
printf "%s\n" "$Pre_Seg_Clean"
echo ""
echo ""
echo "SEGMENTATION ERRORS: $seg_sub_num subjects"
echo ""
printf "%s" "$Seg_Clean"

echo ""
echo "All Done!"


