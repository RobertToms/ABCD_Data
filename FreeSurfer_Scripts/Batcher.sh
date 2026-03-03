##########################################################################
# Batcher.sh
# Author: Robert Toms, robert.toms@utdallas.edu
# Date: 9/4/2025
# Path: /home/kate/Desktop/6.0_Update/FreeSurfer_Scripts/Batcher.sh
##########################################################################

# set up FreeSurfer Environment, if not already set up
source $FREESURFER_HOME/SetUpFreeSurfer.sh

###########################
####### Read in and arrange data
###########################

FullBatch=()
JobsNum=0
Batch_Num=1

### SET BATCH SIZE ### -- Maximum number of SCANS to process at a time, not number of subjects
BatchSize=20

FullListFile=/home/kate/Desktop/6.0_Update/FreeSurfer_Scripts/LargeNiftiList.txt
CurrentBatchFile=/home/kate/Desktop/6.0_Update/FreeSurfer_Scripts/NiftiList.txt
BatchReport=/home/kate/Desktop/6.0_Update/FreeSurfer_Scripts/Batch_Report.txt

# count number of batches
total_scans=$(cat $FullListFile | wc -l)
total_batches=$(( (total_scans + 19) / $BatchSize ))

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
echo "Total Batches: $total_batches"

###########################
####### Run Full Longitudinal Pipeline in batches of BatchSize
###########################

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
		echo "$JobsNum scans remaining"	 >> $BatchReport
		echo "$JobsNum scans remaining"	
		echo "Running Batch $Batch_Num / $total_batches" >> $BatchReport
		echo "Running Batch $Batch_Num / $total_batches"
		
		# Run Longitudinal FreeSurfer Loop
		batch_start_time=$(date)
			/home/kate/Desktop/6.0_Update/FreeSurfer_Scripts/Longitudinal_Pipeline.sh
		batch_end_time=$(date)
		
		# print estimated time remaining: Length of Pipeline * number of batches left
		Time1=$(date -d "$batch_start_time" +%s)
		Time2=$(date -d "$batch_end_time" +%s)
		time_elapsed=$((Time2-Time1))
		batches_left=$((total_batches-Batch_Num-1))
		remaining_time=$((time_elapsed*batches_left))
		
		SECS=$(($remaining_time % 60))
		MINS=$(($remaining_time / 60 % 60))
		HOURS=$(($remaining_time / 3600 % 24))
		DAYS=$(($remaining_time / 86400))
		echo "Estimated Time Remaining: ${DAYS} days ${HOURS} hrs ${MINS} mins ${SECS} secs"
		echo "Estimated Time Remaining: ${DAYS} days ${HOURS} hrs ${MINS} mins ${SECS} secs" >> $BatchReport
		echo "" >> $BatchReport
		# wipe NiftiList.txt
		> $CurrentBatchFile
		
		# reset batch size trackers, increment batch_num
		current_batch_size=0
		final_batch_size=0
		((Batch_Num++))
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
		echo "Running Batch $Batch_Num / $total_batches" >> $BatchReport
		echo "Running Batch $Batch_Num / $total_batches"
		# Run Longitudinal FreeSurfer Loop
			/home/kate/Desktop/6.0_Update/FreeSurfer_Scripts/Longitudinal_Pipeline.sh
		# Wipe NiftiList.txt
		> $CurrentBatchFile
		JobsNum=0
	fi
done


