##########################################################################
# LONG_recon-all_parallel.sh
# Author: Robert Toms, robert.toms@utdallas.edu
# Date: 9/4/2025
# Path: /path/to/FreeSurfer_Scripts/LONG_recon-all_parallel.sh
# NOTE: Optimized for the ABCD 6.0 Release, if used for another release, changes may need to be made to ensure compliance with BIDS naming conventions
##########################################################################

CurrentBatchFile=/path/to/FreeSurfer_Scripts/NiftiList.txt
BatchReport=/path/to/FreeSurfer_Scripts/Batch_Report.txt

TargetBatch=()
JobsNum=0

# Read each line from the txt file, get list of scans
while IFS= read -r file; do
	
	# Isolate subject ID and visit
	sub=$(echo "$file" | cut -d'_' -f1)
	visit=$(echo "$file" | cut -d'_' -f2)
	# Add to TargetBatch Array
	TargetBatch+=("${sub}"^"${visit}"$'\n')
	(( JobsNum++ ))
	echo "Preparing $sub's $visit for LONG."
done < $CurrentBatchFile

### Print number of Long Jobs
echo " "
echo "Number of LONG Jobs = $JobsNum"
echo " "

#######################
# Construct LONG Recon-All Commands
# Write into a temporary .txt file
#######################

LongCommands=/path/to/FreeSurfer_Scripts/temp/long_commands.txt

echo "RUNNING RECON-ALL CROSS:"
for subject in ${TargetBatch[@]}; do
	sub=$(echo "$subject" | cut -d'^' -f1)
	visit=$(echo "$subject" | cut -d'^' -f2)
	
	echo "recon-all -long ${sub}_${visit} ${sub}_long -all"
	echo "recon-all -long ${sub}_${visit} ${sub}_long -all" >> $LongCommands
done

#########################
# Run LONG Recon-all in parallel
#########################
# relies on GNU Parallel for parallelization: https://www.gnu.org/software/parallel/

cat $LongCommands | parallel -j $JobsNum

#########################
# Delete long_commands.txt so the next batch can make a new one
#########################

rm /path/to/FreeSurfer_Scripts/temp/long_commands.txt

#########################
# Write results into BatchReport
#########################

# prints finish status of each individual LONG recon-all
for subject in ${TargetBatch[@]}; do
	sub=$(echo "$subject" | cut -d'^' -f1)
	visit=$(echo "$subject" | cut -d'^' -f2)
  	LogFile="${SUBJECTS_DIR}/${sub}_${visit}.long.${sub}_long/scripts/recon-all.log"
  	echo " "  >> $BatchReport
   	echo "LONG Recon-All Parallel Report for ${sub}_${visit}.long.${sub}_long:" >> $BatchReport
   	tail -n 4 "$LogFile" >> $BatchReport
  	echo " " >> $BatchReport
done
