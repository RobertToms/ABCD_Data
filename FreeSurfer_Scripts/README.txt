##########################################
# README -- CONTENTS OF CURRENT DIRECTORY: ~/RobertToms/ABCD_Data/FreeSurfer_Scripts
# Author: Robert Toms
##########################################

This Folder contains all scripts and files necessary to locally run ABCD_6.0 data in parallel through the longitudinal FreeSurfer 8.0.0 pipeline, plus Hippocampal/Amygdala segmentation.

For more info: https://surfer.nmr.mgh.harvard.edu/fswiki/LongitudinalProcessing
For those who care: Parallelization uses GNU Parallel.

NOTE: Throughout the pipeline, elements of nifti filenames are parsed to name output directories and construct files and paths for finding data. Make sure they are named in a BIDS compatible manner: subjectid_visitid_thispartdoesntmatterasmuch_T1w.nii or .nii.gz

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To run subjects through the pipeline, do the following:

1) Add names of all nifti files you would like to run into LargeNiftiList.txt, then save the file.
	- Line 42 of CROSS_recon-all_parallel.sh tells the program where to find these nifti files. Update this as needed.
	- File names should be grouped by subject, each have their own line, and be in chronological order
		ex: 	sub-YU8GX8G7_ses-00A_rec-NORM_run-01_T1w.nii.gz
			sub-YU8GX8G7_ses-02A_rec-NORM_run-01_T1w.nii.gz
			sub-YU8GX8G7_ses-04A_rec-NORM_run-01_T1w.nii.gz
			sub-YU8GX8G7_ses-06A_rec-NORM_run-01_T1w.nii.gz
			sub-4F29HZH4_ses-00A_rec-NORM_run-01_T1w.nii.gz
			sub-4F29HZH4_ses-02A_rec-NORM_run-01_T1w.nii.gz
			sub-4F29HZH4_ses-04A_rec-NORM_run-01_T1w.nii.gz

2) Check a few things to avoid issues down the line
	- Make sure Batch_Report.txt has been emptied, so when you check it you will only see information pertaining to the batch you are currently processing. 
	- Make sure that NiftiList.txt is empty--Batcher will write and erase filenames into this document, and if it's not empty you'll be running way more in your first batch than you'd expect
	- If you aren't rerunning any subjects, disregard this one. If you are re-running any subjects, make sure that your $SUBJECTS_DIR doesn't have directories in it that will conflict with the names of directories you are about to create by running these scripts. Sticking all the relevant pre-existing subject data in their own folder does the trick for that, and lets you keep the old data until you've confirmed the new data is better.

3) Set the BatchSize variable in Batcher.sh (Line 20) to the maximum number of scans you want to run at a time
	- this is determined by the amount of RAM available to you; this Linux Machine has ~500GB RAM, be sure to leave a reasonable buffer. If it exceeds this, everything crashes.
	- assume 24GB RAM per scan for FreeSurfer 8.0.0

4) Open a terminal, and run Batcher.sh

The last 4 lines of each scan's recon-all log are automatically printed into Batch_Report.txt after each step is completed. You can manually inspect these to see if any have errored out and need to be examined or fixed.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Pipeline Order (for 3 batches as an example)

Batcher.sh
	Longitudinal_Pipeline.sh
		CROSS_recon-all_parallel.sh
		BASE_recon-all_parallel.sh
		LONG_recon-all_parallel.sh
		SegmentHA_T1_Long_parallel.sh
		BIDS_Compliant_Move.sh
	Longitudinal_Pipeline.sh
		CROSS_recon-all_parallel.sh
		BASE_recon-all_parallel.sh
		LONG_recon-all_parallel.sh
		SegmentHA_T1_Long_parallel.sh
		BIDS_Compliant_Move.sh
	Longitudinal_Pipeline.sh
		CROSS_recon-all_parallel.sh
		BASE_recon-all_parallel.sh
		LONG_recon-all_parallel.sh
		SegmentHA_T1_Long_parallel.sh
		BIDS_Compliant_Move.sh
		
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CONTENTS:

temp/
- leave this folder be. The various stages of recon-all commands get automatically written into temporary text documents in this folder, so they can be run in parallel.

BASE_recon-all_parallel.sh
- Reads the current batch of subjects/niftis from NiftiList.txt
- Runs the Base step of the FreeSurfer Longitudinal pipeline on these subjects' data, in parallel
- Creates a within-subjects normalized template from an individual subject's CROSS scans

Batcher.sh
- Reads in the full list of subjects/scans to be run from LargeNiftiList.txt
- Iteratively and progressively writes batches up to BATCH_SIZE into NiftiList.txt
	- Runs through list (nonlinearly, I don't know why it chooses the order it chooses) and if including the next subject will exceed BATCH_SIZE, they get added to the next batch and Longitudinal_Pipeline.sh is run
	
Batch_Report.txt
- Outputs for each scan at each step get written into this text file as the pipeline progresses.
- Last 4 lines of each recon-all log or long-hippocampal-subfields.txt, just enough for you to know if they errored out or not
- Also contains timestamps for start and finish of each step

BIDS_Compliant_Move.sh
- Organizes fully processed and segmented data into a BIDS-Compliant format.
- Each step is stored individually in $SUBJECTS_DIR, they need to all be kept in a directory named as the subject ID
	ex: sub-87T95RHP	<--- this directory is made and the below directories are moved into it
		sub-87T95RHP_long
		sub-87T95RHP_ses-00A
		sub-87T95RHP_ses-00A.long.sub-87T95RHP_long
		sub-87T95RHP_ses-02A
		sub-87T95RHP_ses-02A.long.sub-87T95RHP_long
		sub-87T95RHP_ses-04A
		sub-87T95RHP_ses-04A.long.sub-87T95RHP_long
		
CROSS_recon-all_parallel.sh
- Reads the current batch of subjects/niftis from NiftiList.txt
- Runs the Cross step of the FreeSurfer Longitudinal pipeline on these subjects' data, in parallel
- Step 1 of pipeline, basic recon-all processing

LargeNiftiList.txt
- Contains the FULL list of all nifti filenames that you want to run, across all batches
- File names should each have their own line, and be in chronological order
	ex: 	sub-YU8GX8G7_ses-00A_rec-NORM_run-01_T1w.nii.gz
		sub-YU8GX8G7_ses-02A_rec-NORM_run-01_T1w.nii.gz
		sub-YU8GX8G7_ses-04A_rec-NORM_run-01_T1w.nii.gz
		sub-YU8GX8G7_ses-06A_rec-NORM_run-01_T1w.nii.gz
		sub-4F29HZH4_ses-00A_rec-NORM_run-01_T1w.nii.gz
		sub-4F29HZH4_ses-02A_rec-NORM_run-01_T1w.nii.gz
		sub-4F29HZH4_ses-04A_rec-NORM_run-01_T1w.nii.gz
		
Longitudinal_Pipeline.sh
- Runs every step of the pipeline, in order
	- CROSS_recon-all_parallel.sh
	- BASE_recon-all_parallel.sh
	- LONG_recon-all_parallel.sh
	- SegmentHA_T1_Long_parallel.sh
	- BIDS_Compliant_Move.sh
- Prints timestamps of starts/finshes into Batch_Report.txt

LONG_recon-all_parallel.sh
- Reads the current batch of subjects/niftis from NiftiList.txt
- Runs the Long step of the FreeSurfer Longitudinal pipeline on these subjects' data, in parallel
- Registers each subject's CROSS scans to their BASE template

NiftiList.txt
- Current batch of scans gets written into this txt file by Batcher.sh
- Gets wiped at the end of each batch
- When pipeline is not running, this txt document should be empty

SegmentHA_T1_Long_parallel.sh
- Reads the current batch of subjects/niftis from NiftiList.txt
- Runs the segmentHA_T1_long.sh (FreeSurfer script) step of the FreeSurfer Longitudinal pipeline on these subjects' data, in parallel
- Computes Hippocampal and Amygdalar subfield volumes

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

