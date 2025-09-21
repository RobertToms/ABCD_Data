##########################################################################
# Longitudinal_Pipeline.sh
# Author: Robert Toms
# Date: 9/4/2025
# Path: /path/to/FreeSurfer_Scripts/Longitudinal_Pipeline.s
# NOTE: Optimized for the ABCD 6.0 Release, if used for another release, changes may need to be made to ensure compliance with BIDS naming conventions
##########################################################################

BatchReport=/path/to/FreeSurfer_Scripts/Batch_Report.txt

# Uses ABCD 6.0 Raw data, already in .nii.gz form

echo ""
echo "# # # # # # # # # # # # # # # # # # # # # # #"
echo "# Starting new Batch @" $(date +"%Y-%m-%d %H:%M:%S")
echo "# # # # # # # # # # # # # # # # # # # # # # #"
echo ""


echo "# Starting CROSS @" $(date +"%Y-%m-%d %H:%M:%S")  >> $BatchReport
echo "" >> $BatchReport

# CROSS Recon-All
/path/to/FreeSurfer_Scripts/CROSS_recon-all_parallel.sh
	echo "" >> $BatchReport
	echo "##########################"  >> $BatchReport
  	echo "# CROSS Completed"  >> $BatchReport
  	echo "#" $(date +"%Y-%m-%d %H:%M:%S")  >> $BatchReport
	echo "##########################"  >> $BatchReport
	echo "" >> $BatchReport

# BASE Recon-All	
/path/to/FreeSurfer_Scripts/BASE_recon-all_parallel.sh
	echo "" >> $BatchReport
	echo "##########################"  >> $BatchReport
  	echo "# BASE Completed"  >> $BatchReport
  	echo "#" $(date +"%Y-%m-%d %H:%M:%S")  >> $BatchReport
	echo "##########################"  >> $BatchReport
	echo "" >> $BatchReport
	
# LONG Recon-All	
/path/to/FreeSurfer_Scripts/LONG_recon-all_parallel.sh
	echo "" >> $BatchReport
	echo "##########################"  >> $BatchReport
  	echo "# LONG Completed"  >> $BatchReport
  	echo "#" $(date +"%Y-%m-%d %H:%M:%S")  >> $BatchReport
	echo "##########################"  >> $BatchReport
	echo "" >> $BatchReport
	
# SegmentHA Longitudinally	
/path/to/FreeSurfer_Scripts/SegmentHA_T1_Long_parallel.sh
	echo "" >> $BatchReport
	echo "##########################"  >> $BatchReport
  	echo "# Segmentation Completed"  >> $BatchReport
  	echo "#" $(date +"%Y-%m-%d %H:%M:%S")  >> $BatchReport
	echo "##########################"  >> $BatchReport
	echo "" >> $BatchReport
	
# Move to BIDS Compliant Subdirectory
/path/to/FreeSurfer_Scripts/BIDS_Compliant_Move.sh
	echo "" >> $BatchReport
	echo "#################################"  >> $BatchReport
  	echo "# BIDS Reorganization Completed"  >> $BatchReport
  	echo "#" $(date +"%Y-%m-%d %H:%M:%S")  >> $BatchReport
	echo "#################################"  >> $BatchReport
	echo "" >> $BatchReport

echo ""
echo "# # # # # # # # # # # # # # # # # # # # # # #"  >> $BatchReport
echo "# Longitudinal Batch finished." >> $BatchReport
echo "#" $(date +"%Y-%m-%d %H:%M:%S")  >> $BatchReport
echo "# # # # # # # # # # # # # # # # # # # # # # #"  >> $BatchReport
echo ""
