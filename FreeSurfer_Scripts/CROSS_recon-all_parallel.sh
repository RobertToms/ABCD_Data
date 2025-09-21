##########################################################################
# CROSS_recon-all_parallel.sh
# Author: Robert Toms, robert.toms@utdallas.edu
# Date: 9/4/2025
# Path: /path/to/FreeSurfer_Scripts/CROSS_recon-all_parallel.sh
# NOTE: Optimized for the ABCD 6.0 Release, if used for another release, changes may need to be made to ensure compliance with BIDS naming conventions
##########################################################################

CurrentBatchFile=/path/to/FreeSurfer_Scripts/NiftiList.txt
BatchReport=/path/to/FreeSurfer_Scripts/Batch_Report.txt

TargetBatch=()
JobsNum=0
echo ""

# Read each line from the txt file
while IFS= read -r file; do
	
	# Isolate subject ID and visit
	sub=$(echo "$file" | cut -d'_' -f1)
	visit=$(echo "$file" | cut -d'_' -f2)
	nifti=$(echo "$file")
	# Add to TargetBatch Array
	TargetBatch+=("${sub}"^"${visit}"^"${nifti}"$'\n')
	(( JobsNum++ ))
	echo "Preparing $sub's $visit for CROSS."
done < $CurrentBatchFile

echo ""
#######################
# Construct CROSS Recon-All Commands
# Write into a temporary .txt file
#######################

CrossCommands=/path/to/FreeSurfer_Scripts/temp/cross_commands.txt

echo "RUNNING RECON-ALL CROSS:"
for subject in ${TargetBatch[@]}; do
	sub=$(echo "$subject" | cut -d'^' -f1)
	visit=$(echo "$subject" | cut -d'^' -f2)
	nifti=$(echo "$subject" | cut -d'^' -f3)
	NiftiPath=($(ls /path/to/ABCD_6.0/mri_data/${sub}/${visit}/anat/${nifti}))
	
	echo "recon-all -subjid ${sub}_${visit} -i ${NiftiPath} -all"
	echo "recon-all -subjid ${sub}_${visit} -i ${NiftiPath} -all" >> $CrossCommands

done

#########################
# Run CROSS Recon-all in parallel
#########################
# relies on GNU Parallel for parallelization: https://www.gnu.org/software/parallel/

cat $CrossCommands | parallel -j $JobsNum

#########################
# Delete cross_commands.txt so the next batch can make a new one
#########################

rm /path/to/FreeSurfer_Scripts/temp/cross_commands.txt

#########################
# Write results into BatchReport
#########################

# prints finish status of each individual CROSS recon-all
for subject in ${TargetBatch[@]}; do
	sub=$(echo "$subject" | cut -d'^' -f1)
	visit=$(echo "$subject" | cut -d'^' -f2)
  	LogFile="${SUBJECTS_DIR}/${sub}_${visit}/scripts/recon-all.log"
  	echo " "  >> $BatchReport
   	echo "CROSS Recon-All Parallel Report for ${sub}_$visit:" >> $BatchReport
   	tail -n 4 "$LogFile" >> $BatchReport
  	echo " " >> $BatchReport
done
