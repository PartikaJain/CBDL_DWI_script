\# CBDL\_DWI\_script

A custom shell pipeline for processing \*\*Diffusion-Weighted Imaging (DWI)\*\* data using tools from \*\*MRtrix3\*\*, \*\*FSL\*\*, and \*\*FreeSurfer\*\*.   
This script is designed to be run in a subject-wise loop, automating DWI preprocessing, fiber orientation estimation, tractography, and structural connectome generation.

\---

\#\# 🧠 Pipeline Overview

\#\#\# \*\*Step 1 – DWI Preprocessing\*\*  
\- Conversion of DWI files to \`.mif\` format  
\- Denoising and unringing  
\- Motion and distortion correction using \`dwifslpreproc\`  
\- Bias field correction and brain mask creation

\#\#\# \*\*Step 2 – Fiber Orientation Distribution (FOD) Estimation\*\*  
\- Response function estimation using the \`dhollander\` algorithm  
\- Multi-shell multi-tissue constrained spherical deconvolution (\`msmt\_csd\`)  
\- Intensity normalization across tissues

\#\#\# \*\*Step 3 – Whole-brain Tractography\*\*  
\- Generation of 5-tissue-type (5TT) segmentation from T1w image  
\- Coregistration of diffusion and structural space  
\- Streamline generation using anatomically-constrained tractography (ACT)  
\- SIFT2 filtering for streamline weighting

\#\#\# \*\*Step 4 – Structural Connectome Construction\*\*  
\- Cortical parcellation with FreeSurfer (\`recon-all\`)  
\- Label conversion and parcellation alignment  
\- Generation of subject-specific structural connectome matrix using \`tck2connectome\`

\---

\#\# 📁 Inputs

\- Diffusion-weighted image (\`\*\_dwi.nii.gz\`) with \`.bvec\`, \`.bval\`, and \`.json\` sidecars  
\- T1-weighted structural image (\`\*\_T1w.nii.gz\`)  
\- FreeSurfer outputs (\`aparc+aseg.mgz\`)  
\- Custom LUT and parcellation template for label conversion

\---

\#\# 💻 Dependencies

Ensure the following tools are installed and properly configured:  
\- \[MRtrix3\](https://www.mrtrix.org/)  
\- \[FSL\](https://fsl.fmrib.ox.ac.uk/fsl/fslwiki/)  
\- \[FreeSurfer\](https://surfer.nmr.mgh.harvard.edu/)  
\- \[GNU Parallel\](https://www.gnu.org/software/parallel/)  
\- \[ANTs\](https://stnava.github.io/ANTs/), if using ANTs-based bias correction

\---  
📄 Citation  
If you use this script in your research, please cite the GitHub repository as:

CBDL DWI Processing Script. GitHub. https://github.com/Pratikasiwatch/CBDL\_DWI\_script

📬 Contact  
For feedback, questions, or contributions, please contact:

Pratika Jain  
PhD Researcher, CBDL Lab, NBRC, Gurgaon, India  
✉️ partika.mhg.bhu16@gmail.com  
🌐 https://github.com/Pratikasiwatch

