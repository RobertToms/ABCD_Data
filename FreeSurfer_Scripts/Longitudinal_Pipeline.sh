##########################################################################
# Longitudinal_Pipeline.sh
# Author: Robert Toms
# Date: 9/4/2025
# Path: /home/kate/Desktop/6.0_Update/FreeSurfer_Scripts/Longitudinal_Pipeline.sh
##########################################################################

BatchReport=/home/kate/Desktop/6.0_Update/FreeSurfer_Scripts/Batch_Report.txt

start_time=$(date)

# Uses ABCD 6.0 Raw data, already in .nii.gz form

echo ""
echo "# # # # # # # # # # # # # # # # # # # # # # #"
echo "# Starting new Batch @" $(date +"%Y-%m-%d %H:%M:%S")
echo "# # # # # # # # # # # # # # # # # # # # # # #"
echo ""


echo "# Starting CROSS @" $(date +"%Y-%m-%d %H:%M:%S")  >> $BatchReport
echo "" >> $BatchReport

################
#echo "Moving back up for re-segmentation"
#/home/kate/Desktop/6.0_Update/Error_Checking/Move_Back_Up.sh
################

# CROSS Recon-All
/home/kate/Desktop/6.0_Update/FreeSurfer_Scripts/CROSS_recon-all_parallel.sh
	echo "" >> $BatchReport
	echo "##########################"  >> $BatchReport
  	echo "# CROSS Completed"  >> $BatchReport
  	echo "#" $(date +"%Y-%m-%d %H:%M:%S")  >> $BatchReport
	echo "##########################"  >> $BatchReport
	echo "" >> $BatchReport

# BASE Recon-All	
/home/kate/Desktop/6.0_Update/FreeSurfer_Scripts/BASE_recon-all_parallel.sh
	echo "" >> $BatchReport
	echo "##########################"  >> $BatchReport
  	echo "# BASE Completed"  >> $BatchReport
  	echo "#" $(date +"%Y-%m-%d %H:%M:%S")  >> $BatchReport
	echo "##########################"  >> $BatchReport
	echo "" >> $BatchReport
	
# LONG Recon-All	
/home/kate/Desktop/6.0_Update/FreeSurfer_Scripts/LONG_recon-all_parallel.sh
	echo "" >> $BatchReport
	echo "##########################"  >> $BatchReport
  	echo "# LONG Completed"  >> $BatchReport
  	echo "#" $(date +"%Y-%m-%d %H:%M:%S")  >> $BatchReport
	echo "##########################"  >> $BatchReport
	echo "" >> $BatchReport
	
# SegmentHA Longitudinally	
/home/kate/Desktop/6.0_Update/FreeSurfer_Scripts/SegmentHA_T1_Long_parallel.sh
	echo "" >> $BatchReport
	echo "##########################"  >> $BatchReport
  	echo "# Segmentation Completed"  >> $BatchReport
  	echo "#" $(date +"%Y-%m-%d %H:%M:%S")  >> $BatchReport
	echo "##########################"  >> $BatchReport
	echo "" >> $BatchReport
	
# Move to BIDS Compliant Subdirectory
/home/kate/Desktop/6.0_Update/FreeSurfer_Scripts/BIDS_Compliant_Move.sh
	echo "" >> $BatchReport
	echo "#################################"  >> $BatchReport
  	echo "# BIDS Reorganization Completed"  >> $BatchReport
  	echo "#" $(date +"%Y-%m-%d %H:%M:%S")  >> $BatchReport
	echo "#################################"  >> $BatchReport
	echo "" >> $BatchReport

end_time=$(date)
T1=$(date -d "$start_time" +%s)
T2=$(date -d "$end_time" +%s)
time_elapsed_secs=$((T2-T1))
SECS=$(($time_elapsed_secs % 60))
MINS=$(($time_elapsed_secs / 60 % 60))
HOURS=$(($time_elapsed_secs / 3600 % 24))
DAYS=$(($time_elapsed_secs / 86400))

echo ""
echo "# # # # # # # # # # # # # # # # # # # # # # #"  >> $BatchReport
echo "# Longitudinal Batch finished." >> $BatchReport
echo "#" $(date +"%Y-%m-%d %H:%M:%S")  >> $BatchReport
echo "# Pipeline took ${DAYS} days ${HOURS} hrs ${MINS} mins ${SECS} secs" >> $BatchReport
echo "# # # # # # # # # # # # # # # # # # # # # # #"  >> $BatchReport
echo ""
