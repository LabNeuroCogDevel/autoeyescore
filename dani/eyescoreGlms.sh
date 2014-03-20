#!/bin/bash
set -e # exit on error
set -x # print output

path=$1
pathAnalysis=$2
id=$3
date=$4

# fmri section
# - check the right number of runs
# - motion censoring
inputs=$( ls -d $path/$id/$date/MultimodalWM*/nfswkmtd_functional.nii.gz )
censor=$( ls $path/$id/$date/MultimodalWM*/mcplots_censor.1D )
for c in $censor; do cat $c >> $pathAnalysis/$id/$date/mcplots_censor_cat.1D; done
pathModel=$pathAnalysis/$id/$date/glm_fixed_load_acc
mkdir -p $pathModel
cd $pathModel

# behavior section
# - check the right number of runs
# - concatenate timing files
for t in $( ls .../... ); do cat $t >> ...; done

# command is run in background with output suppressed so more jobs can run
3dDeconvolve -input $inputs -mask $mask -nfirst 0 -polort 3 -allzero_OK -GOFORIT 20 \
  -censor $pathModel/../mcplots_censor_cat.1D \
  -num_stimts 16 -basis_normall 1 \
  -stim_times 1 $path/$id/$date/timings/cue_corr_low 'BLOCK(1.5,1)' -stim_label 1 cueL_corr \
  -stim_times 2 $path/$id/$date/timings/cue_incorr_low 'BLOCK(1.5,1)' -stim_label 2 cueL_incorr \
  -stim_times 3 $path/$id/$date/timings/cue_catch_low 'BLOCK(1.5,1)' -stim_label 3 cueL_catch \
  -stim_times 4 $path/$id/$date/timings/cue_corr_high 'BLOCK(1.5,1)' -stim_label 4 cueH_corr \
  -stim_times 5 $path/$id/$date/timings/cue_incorr_high 'BLOCK(1.5,1)' -stim_label 5 cueH_incorr \
  -stim_times 6 $path/$id/$date/timings/cue_catch_high 'BLOCK(1.5,1)' -stim_label 6 cueH_catch \
  -stim_times_AM1 7 $path/$id/$date/timings/delay_corr_low 'dmBLOCK' -stim_label 7 delayL_corr \
  -stim_times_AM1 8 $path/$id/$date/timings/delay_incorr_low 'dmBLOCK' -stim_label 8 delayL_incorr \
  -stim_times_AM1 9 $path/$id/$date/timings/delay_catch_low 'dmBLOCK' -stim_label 9 delayL_catch \
  -stim_times_AM1 10 $path/$id/$date/timings/delay_corr_high 'dmBLOCK' -stim_label 10 delayH_corr \
  -stim_times_AM1 11 $path/$id/$date/timings/delay_incorr_high 'dmBLOCK' -stim_label 11 delayH_incorr \
  -stim_times_AM1 12 $path/$id/$date/timings/delay_catch_high 'dmBLOCK' -stim_label 12 delayH_catch \
  -stim_times_AM1 13 $path/$id/$date/timings/target_corr_low 'dmBLOCK' -stim_label 13 targetL_corr \
  -stim_times_AM1 14 $path/$id/$date/timings/target_incorr_low 'dmBLOCK' -stim_label 14 targetL_incorr \
  -stim_times_AM1 15 $path/$id/$date/timings/target_corr_high 'dmBLOCK' -stim_label 15 targetH_corr \
  -stim_times_AM1 16 $path/$id/$date/timings/target_incorr_high 'dmBLOCK' -stim_label 16 targetH_incorr \
  -num_glt 6 \
  -gltsym 'SYM: +0.5*cueL_corr +0.5*cueH_corr' -glt_label 1 cue \
  -gltsym 'SYM: -cueL_corr +cueH_corr' -glt_label 2 cue_HvsL \
  -gltsym 'SYM: +0.5*delayL_corr +0.5*delayH_corr' -glt_label 3 delay \
  -gltsym 'SYM: -delayL_corr +delayH_corr' -glt_label 4 delay_HvsL \
  -gltsym 'SYM: +0.5*targetL_corr +0.5*targetH_corr' -glt_label 5 target \
  -gltsym 'SYM: -targetL_corr +targetH_corr' -glt_label 6 target_HvsL \
  -xjpeg $pathModel/glm_matrix.jpg -tout -fout -bucket $pathModel/glm_out
#sh glm_out.REML_cmd ## runs 3dREMLfit which includes ARMA(1,1) autocorrelation

