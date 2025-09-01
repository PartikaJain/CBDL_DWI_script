#! /bin/bash

## ("#!" is an operator called shebang which directs the script to the interpreter location). 
## To run this file, open terminal and type "chmod +x mydtipipeline.sh" to make this file executable and run this file , type ./mydtipipeline.sh##

for j in sub-CC* ;do 
echo $j
cd $j
i=`expr substr ${j} 5 11`

d=*dwi.nii.gz*
bvc=*dwi.bvec*
bvl=*dwi.bval*
jsn=*dwi.json*
t=*T1w.nii.gz*

#### Step 1 - Preprocessing of DTI data ######

### Step 1.1 - Conversion of DTI file formats into mif (MRtrix Image Format)
time mrconvert $d sub${i}.mif -fslgrad $bvc $bvl -json_import $jsn

### Step 1.2 - Denoising the data
time dwidenoise sub${i}.mif sub${i}_den.mif -noise noise.mif

### Step 1.3 - Unringing the data
time mrdegibbs sub${i}_den.mif sub${i}_den_unr.mif

### Step 1.4 - Motion and distortion correction
time dwifslpreproc -rpe_none -pe_dir j- sub${i}_den_unr.mif sub${i}_preproc.mif -eddy_options " --slm=linear --data_is_shelled"

### Step 1.5 - Bias Field correction
time dwibiascorrect ants sub${i}_preproc.mif sub${i}_unbiased.mif -fslgrad $bvc $bvl

### Step 1.6 - Brain mask estimation 
time mrconvert sub${i}_unbiased.mif sub${i}_unbiased.nii -export_grad_fsl bvecs_new.bvecs bvals_new.bvals

time bet2 sub${i}_unbiased.nii sub${i}_masked.nii -m -f 0.3
mrconvert  sub${i}_masked.nii.gz mask.mif

#### Step 2- Fiber Orientation Distribution (FOD)

### Step 2.1 - Response function estimation
time dwi2response dhollander -shell 0,1000,2000 sub${i}_unbiased.mif wm.txt gm.txt csf.txt -mask mask.mif -voxels voxels.mif -force

### Step 2.2 - Estimation of Fiber Orientation Distribution (FOD)
time dwi2fod msmt_csd -shell 0,1000,2000 sub${i}_unbiased.mif -mask mask.mif wm.txt wmfod.mif gm.txt gmfod.mif csf.txt csffod.mif

### Step 2.3 - Intensity Normalization 
time mtnormalise wmfod.mif wmfod_norm.mif gmfod.mif gmfod_norm.mif csffod.mif csffod_norm.mif -mask mask.mif

#### Step 3- Creating Whole-brain tractogram

### Step 3.1 - Preparing Anatomical constrained tractography (ACT)

## Step 3.1.1 - Preparing Mask for streamline termination
time mrconvert $t T1w.mif 

time 5ttgen fsl T1w.mif 5tt.mif   
time dwiextract sub${i}_unbiased.mif - -bzero | mrmath - mean mean_b0.mif -axis 3

mrconvert 5tt.mif 5tt.nii.gz
mrconvert mean_b0.mif mean_b0.nii.gz

fslroi 5tt.nii.gz 5tt_gm.nii.gz 0 1

flirt -in mean_b0.nii.gz -ref 5tt_gm.nii.gz -interp nearestneighbour -dof 6 -omat diff2struct_fsl.mat  

transformconvert diff2struct_fsl.mat mean_b0.nii.gz 5tt.nii.gz flirt_import diff2struct_mrtrix.txt

mrtransform 5tt.mif -linear diff2struct_mrtrix.txt -inverse 5tt_coreg.mif

### Step 3.1.2 - Preparing a mask for streamline seeding
time 5tt2gmwmi 5tt_coreg.mif gmwmSeed_coreg.mif

### Step 3.2 - Creating streamlines
time tckgen -act 5tt_coreg.mif -backtrack -seed_gmwmi gmwmSeed_coreg.mif  -maxlength 250 -cutoff 0.06 -select 10000000 wmfod_norm.mif tracks_10M.tck

### Step 3.3 - optimising the number of streamlines
time tcksift2 -act 5tt_coreg.mif -out_mu sift_mu.txt -out_coeffs sift_coeffs.txt -nthreads 8 tracks_10M.tck wmfod_norm.mif sift_10M.txt -force

### Step 4.1 - Using recon-all to process T1 image
export FREESURFER_HOME=/usr/local/freesurfer
source $FREESURFER_HOME/SetUpFreeSurfer.sh

time ls *.nii.gz | parallel --jobs 8 recon-all -s {} -i {} -all -qcache

### Step 4.2 - Converting anatomical labels to parcellated images
time labelconvert /media/cbdl-pratika/CBDL/Pratika/reconall_309/sub${i}_T1w.nii.gz/mri/aparc+aseg.mgz /home/cbdl-pratika/freesurfer/FreeSurferColorLUT.txt /home/cbdl-pratika/mrtrix3/share/mrtrix3/labelconvert/fs_default_modi_DK.txt sub${i}_parcellated.mif -force
 
### Step 4.3 - Generating structural connectome matrix using tck2connectome
time tck2connectome -symmetric -zero_diagonal -scale_invnodevol -tck_weights_in sift_10M.txt tracks_10M.tck sub${i}_parcellated.mif CC${k}_parcel_SC.csv -out_assignment assignments_SC.csv -force

cd ..
done










