#!/bin/bash
#set -e # exit on error
#set -x # print output

# read arguments
# no error control for now, will just use set -e and hope
path=$1
task=$2
id=$3
date=$4
model=$5
mask=$6

# set up paths
pathPreproc=$path/$id/$date
pathGlm=$path/GLM/$task/$id/$date
pathTimings=$pathGlm/timings
pathModel=$pathGlm/$model
mkdir -p $pathModel
cd $pathModel

# get fmri run and motion info
case $task in "MGSEncode") runID="MS" ;; "AntiState") runID="AS" ;; esac
inputs=$( ls $pathPreproc/${runID}*/nfswkmtd_functional.nii.gz )
censor=$( ls $pathPreproc/${runID}*/mcplots_censor.1D )
motionCat=$pathGlm/mcplots_censor_cat.1D
if [ ! -e $motionCat ]; then for c in $censor; do cat $c >> $motionCat; done; fi

# write command to variable (will write to file)
deconvolveScript=$pathModel/glm_out.cmd
echo "3dDeconvolve -input $( echo $inputs ) -mask $mask -nfirst 0 -polort 3 -allzero_OK -GOFORIT 99 -censor $pathGlm/mcplots_censor_cat.1D \\" > $deconvolveScript

# add line for each folder in timings file
timings=( $( ls $pathTimings ) )
num_stimts=${#timings[@]}
echo "  -num_stimts $num_stimts -basis_normall 1 \\" >> $deconvolveScript
for t in $( seq $num_stimts ); do
  timing=${timings[$((t-1))]}
  stim_times="-stim_times"; deconvolveFun="GAM" # defaults
  case $timing in # need to get durations for blocks; also, blinks/unscored saccades will be gamma
    # "blinks"|"unscored") ;;
    as_*|ps_*)
      deconvolveFun="BLOCK(4.5,1)" ;;
    vgs*cue_short*|delay*delay_short|mgs*)
      deconvolveFun="BLOCK(1.5,1)" ;;
    vgs*cue_long*)
      deconvolveFun="BLOCK(3,1)" ;;
    delay*delay_long)
      deconvolveFun="BLOCK(9,1)" ;;
  esac
  if [ $model == "beta_series" ] & [ $( echo $timing | cut -d_ -f2 ) == correct ]; then stim_times="-stim_times_IM"; fi
  echo "  $stim_times $t $pathTimings/$timing '${deconvolveFun}' -stim_label $t $timing \\" >> $deconvolveScript
done

# add contrasts for MGSEncode task (to combine across different cue/delay lengths)
if [ $task == "MGSEncode" ] && [ $model == "glm" ]; then
  echo "  -num_glt 8 \\" >> $deconvolveScript
  echo "  -gltsym 'SYM: +0.5*vgs_correct_cue_short +0.5*vgs_correct_cue_long' -glt_label 1 cue \\" >> $deconvolveScript
  echo "  -gltsym 'SYM: +vgs_correct_cue_short -vgs_correct_cue_long' -glt_label 2 cue_short-long \\" >> $deconvolveScript
  echo "  -gltsym 'SYM: +0.25*delay_correct_cue_short_delay_short +0.25*delay_correct_cue_long_delay_short +0.25*delay_correct_cue_short_delay_long +0.25*delay_correct_cue_long_delay_long' -glt_label 3 delay \\" >> $deconvolveScript
  echo "  -gltsym 'SYM: +0.5*delay_correct_cue_short_delay_short -0.5*delay_correct_cue_long_delay_short +0.5*delay_correct_cue_short_delay_long -0.5*delay_correct_cue_long_delay_long' -glt_label 4 delay_cueShort-cueLong \\" >> $deconvolveScript
  echo "  -gltsym 'SYM: +0.5*delay_correct_cue_short_delay_short +0.5*delay_correct_cue_long_delay_short -0.5*delay_correct_cue_short_delay_long -0.5*delay_correct_cue_long_delay_long' -glt_label 5 delay_delayShort-delayLong \\" >> $deconvolveScript
  echo "  -gltsym 'SYM: +0.25*mgs_correct_cue_short_delay_short +0.25*mgs_correct_cue_long_delay_short +0.25*mgs_correct_cue_short_delay_long +0.25*mgs_correct_cue_long_delay_long' -glt_label 6 target \\" >> $deconvolveScript
  echo "  -gltsym 'SYM: +0.5*mgs_correct_cue_short_delay_short -0.5*mgs_correct_cue_long_delay_short +0.5*mgs_correct_cue_short_delay_long -0.5*mgs_correct_cue_long_delay_long' -glt_label 7 target_cueShort-cueLong \\" >> $deconvolveScript
  echo "  -gltsym 'SYM: +0.5*mgs_correct_cue_short_delay_short +0.5*mgs_correct_cue_long_delay_short -0.5*mgs_correct_cue_short_delay_long -0.5*mgs_correct_cue_long_delay_long' -glt_label 8 target_delayShort-delayLong \\" >> $deconvolveScript
fi

# output data info
echo "  -quiet -jobs 1 -xjpeg $pathModel/glm_matrix.jpg -tout -fout -bucket $pathModel/glm_out" >> $deconvolveScript

