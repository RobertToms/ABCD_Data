##########################################################################
# BIDS_Compliant_Move.sh
# Author: Robert Toms, robert.toms@utdallas.edu
# Date: 9/4/2025
# Path: /path/to/FreeSurfer_Scripts/BIDS_Compliant_Move.sh
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


######################
# Create target directory
######################

cd $SUBJECTS_DIR
pwd

for subject in ${subjectIDs[@]}; do
	mkdir ${subject}
    	if [ -d "./${subject}" ]; then
    		echo "Made ${subject} in " $PWD
		echo "Made ${subject} in " $PWD >> $BatchReport
    	else
    		echo "Error creating $subject subdirectory"
    		echo "Error creating $subject subdirectory" >> $BatchReport
	fi
done

echo ""
#####################################
# Move Files to BIDS_Compliant target directory
#####################################

for sub in ${subjectIDs[@]}; do
    echo " "  >> $BatchReport
    echo "${sub}" >> $BatchReport

    echo "Moving ${sub}'s data"

    mv ${sub}_* ${sub}
    	if [ -z "$(ls -A "./${sub}")" ]; then
  		echo "ERROR moving ${sub}'s data!" >> $BatchReport
  		echo "ERROR moving ${sub}'s data!"
  	else echo "Data moved to ${sub} successfully" >> $BatchReport
  		echo "Data moved to ${sub} successfully"
	fi

done
