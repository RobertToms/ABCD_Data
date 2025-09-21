##########################################################################
# Batcher.sh
# Author: Robert Toms, robert.toms@utdallas.edu
# Date: 9/4/2025
# Path: /path/to/FreeSurfer_Scripts/Batcher.sh
# NOTE: Optimized for the ABCD 6.0 Release, if used for another release, changes may need to be made to ensure compliance with BIDS naming conventions
##########################################################################

# set up FreeSurfer Environment, if not already set up
source $FREESURFER_HOME/SetUpFreeSurfer.sh

###########################
####### Read in and arrange data
###########################

FullBatch=()
JobsNum=0

FullListFile=/path/to/FreeSurfer_Scripts/LargeNiftiList.txt
CurrentBatchFile=/path/to/FreeSurfer_Scripts/NiftiList.txt
BatchReport=/path/to/FreeSurfer_Scripts/Batch_Report.txt

# Get populate array with FULL list of nii scans
mapfile -t FullBatch < $FullListFile
JobsNum=${#FullBatch[@]}

# Get list of unique Subject IDs from nii scan list
declare -A subjects
for nii in ${FullBatch[@]}; do
	just_id=$(echo $nii | cut -d'_' -f1)
	subjects["$just_id"]+="$nii "
done

subjectIDs=("${!subjects[@]}")

echo " "
echo "### SCANS TO RUN ###"
echo " "
for id in "${!subjects[@]}"; do
  echo "$id: ${subjects[$id]}"
done

echo " "
echo "Total unique subjects: ${#subjects[@]}"
echo "Total number of scans: $JobsNum"


###########################
####### Run Full Longitudinal Pipeline in batches of BatchSize
###########################

### SET BATCH SIZE ### -- Maximum number of SCANS to process at a time, not number of subjects
# This should be based on how much RAM you have at your disposal. For FreeSurfer 8.0.0, it is reasonable to assume up to 24GB per scan.
BatchSize=21

# Isolate Current Batch and Run in succession
current_batch_size=0

for id in "${!subjects[@]}"; do 		# for each subject
	read -a scans <<< "${subjects[$id]}"	# get their list of scans
	number_of_scans=${#scans[@]}		# count how many scans they have
	echo "$id: $number_of_scans scans"
	
	# if adding their scans will exceed the Batch Size, run batch
	if (( current_batch_size + number_of_scans > BatchSize )); then	
		final_batch_size=$current_batch_size
				
		# Delineate between current batch and next batch
		(( JobsNum -= final_batch_size ))
		echo "$id exceeds Batch Size, saved for next batch"
		echo "$final_batch_size for this batch"
		echo "" >> $BatchReport
		echo "$final_batch_size for this batch" >> $BatchReport
		echo "$JobsNum scans remaining"
				
		# Run Longitudinal FreeSurfer Loop
			/path/to/FreeSurfer_Scripts/Longitudinal_Pipeline.sh
		# wipe NiftiList.txt
		> $CurrentBatchFile
		
		# reset batch size trackers
		current_batch_size=0
		final_batch_size=0
	fi

	# if they won't exceed Batch Size, add them to the current batch
	for scan in "${scans[@]}"; do
		echo "$scan" >> $CurrentBatchFile # write the niftis into NiftiList.txt
	done
	(( current_batch_size += number_of_scans ))
	
	# final run
	if [ $current_batch_size -eq $JobsNum ]; then
		echo "$current_batch_size for this batch"
		echo "" >> $BatchReport
		echo "Final Run: $current_batch_size for this batch"  >> $BatchReport
		# Run Longitudinal FreeSurfer Loop
			/path/to/FreeSurfer_Scripts/Longitudinal_Pipeline.sh
		# Wipe NiftiList.txt
		> $CurrentBatchFile
		JobsNum=0
	fi
done

###########################
####### Pull Volumes
###########################

FinishedList=/path/to/Post_Processing/Finished_Niftis.txt

# Pull Volumes
/path/to/Post_Processing/Female_6.0_Volumes.sh

# make spacer in finished list
echo " " >> $FinishedList

# Write processed/pulled niftis into Finished List
for nii in $(cat $FullListFile); do
	echo "$nii" >> $FinishedList
done

# Wipe Nifti List to fill with new batch
> $FullListFile





