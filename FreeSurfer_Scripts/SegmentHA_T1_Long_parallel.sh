##########################################################################
# SegmentHA_T1_Long_parallel.sh
# Author: Robert Toms, robert.toms@utdallas.edu
# Date: 9/4/2025
# Path: /path/to/FreeSurfer_Scripts/SegmentHA_T1_Long_parallel.sh
# NOTE: Optimized for the ABCD 6.0 Release, if used for another release, changes may need to be made to ensure compliance with BIDS naming conventions
##########################################################################

CurrentBatchFile=/path/to/FreeSurfer_Scripts/NiftiList.txt
BatchReport=/path/to/FreeSurfer_Scripts/Batch_Report.txt

TargetBatch=()
JobsNum=0

#######################
# Count Number of Subjects to be run
# Build Array of Subject IDs
#######################

## build associative array of current batch
mapfile -t Batch < $CurrentBatchFile

# Get list of unique Subject IDs from nii scan list
declare -A subjects
for nii in ${Batch[@]}; do
	just_id=$(echo $nii | cut -d'_' -f1)
	sessionID=$(echo $nii | cut -d'_' -f2)
	subjects["$just_id"]+="$sessionID "
done

JobsNum=${#subjects[@]}
subjectIDs=("${!subjects[@]}")

echo " "
echo "Number of Scans = ${#Batch[@]}"
echo "Number of segmentHA_T1_long Jobs = $JobsNum"
echo " "

########################
### Run segmentHA_T1_long.sh in parallel
########################
# relies on GNU Parallel for parallelization: https://www.gnu.org/software/parallel/

echo ${subjectIDs[@]} | tr ' ' '\n' | parallel 'IFS="^" read -ra parts <<< {} && echo "Queueing ${parts[0]}_long"'
echo ${subjectIDs[@]} | tr ' ' '\n' | parallel -j $JobsNum 'IFS="^" read -ra parts <<< {} && segmentHA_T1_long.sh ${parts[0]}_long $SUBJECTS_DIR'

##############################################
# Print finish status of each individual segmentHA_T1.sh
##############################################
for subject in ${subjectIDs[@]}; do
#	sub=$(echo "$subject" | cut -d'^' -f1)
   	LogFile="${SUBJECTS_DIR}/${subject}_long/scripts/long-hippocampal-subfields-T1.log"
  	echo " "  >> $BatchReport
   	echo "Longitudinal Segment_HA Parallel Report for ${subject}_long:" >> $BatchReport
   	tail -n 4 "$LogFile" >> $BatchReport
  	echo " " >> $BatchReport
done
