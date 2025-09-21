##########################################################################
# BASE_recon-all_parallel.sh
# Author: Robert Toms, robert.toms@utdallas.edu
# Date: 9/4/2025
# Path: /path/to/FreeSurfer_Scripts/BASE_recon-all_parallel.sh
# NOTE: Optimized for the ABCD 6.0 Release, if used for another release, changes may need to be made to ensure compliance with BIDS naming conventions
##########################################################################

CurrentBatchFile=/path/to/FreeSurfer_Scripts/NiftiList.txt
BatchReport=/path/to/FreeSurfer_Scripts/Batch_Report.txt

TargetBatch=()
JobsNum=0

#######################
# Count Number of Subjects to be run
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
echo "Number of BASE Jobs = $JobsNum"
echo " "

#######################
# Construct BASE Recon-All Commands
#######################

### In future releases, where longitudinal scans have more than 5 timepoints, add additional "if" clauses as needed.

BaseCommands=/path/to/FreeSurfer_Scripts/temp/base_commands.txt

for id in "${!subjects[@]}"; do
	read -a scans <<< "${subjects[$id]}"	# get their list of scans
	number_of_scans=${#scans[@]}		# count how many scans they have
	echo "$id: $number_of_scans scans"

	if [[ $number_of_scans -eq 2 ]]; then
		echo "recon-all -base ${id}_long -tp ${id}_${scans[0]} -tp ${id}_${scans[1]} -all" >> $BaseCommands
	fi
	
	if [[ $number_of_scans -eq 3 ]]; then
		echo "recon-all -base ${id}_long -tp ${id}_${scans[0]} -tp ${id}_${scans[1]} -tp ${id}_${scans[2]} -all" >> $BaseCommands
	fi

	if [[ $number_of_scans -eq 4 ]]; then
		echo "recon-all -base ${id}_long -tp ${id}_${scans[0]} -tp ${id}_${scans[1]} -tp ${id}_${scans[2]} -tp ${id}_${scans[3]} -all" >> $BaseCommands
	fi
	
	if [[ $number_of_scans -eq 5 ]]; then
		echo "recon-all -base ${id}_long -tp ${id}_${scans[0]} -tp ${id}_${scans[1]} -tp ${id}_${scans[2]} -tp ${id}_${scans[3]} -tp ${id}_${scans[4]} -all" >> $BaseCommands
	fi
 done

echo ""
#########################
# Run BASE Recon-all in parallel
#########################
# relies on GNU Parallel for parallelization: https://www.gnu.org/software/parallel/

echo "RUNNING RECON-ALL BASE:"
cat $BaseCommands

cat $BaseCommands | parallel -j $JobsNum


#########################
# Delete Base Commands so the next batch can make a new one
#########################

rm /path/to/FreeSurfer_Scripts/temp/base_commands.txt

# prints finish status of each individual BASE recon-all
for id in "${!subjects[@]}"; do
	sub=$(echo "$id" | cut -d'^' -f1)
  	LogFile="${SUBJECTS_DIR}/${sub}_long/scripts/recon-all.log"
  	echo " "  >> $BatchReport
   	echo "BASE Recon-All Parallel Report for ${sub}_long:" >> $BatchReport
   	tail -n 4 "$LogFile" >> $BatchReport
  	echo " " >> $BatchReport
done





