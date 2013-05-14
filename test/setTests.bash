#!/usr/bin/env bash

######
# 
# Generate files to test AGAINST in R
#  i.e. generate ground truth
#
#####

taskdir=/mnt/B/bea_res/Data/Tasks/

# list of trials to explore
listfn='task_subj_date_run_trial.list';
[ ! -r $listfn ] &&  echo "need $listfn" && exit 1

# remake test directories
for dir in expected input; do
   [ -d $dir ] && rm -r $dir
   mkdir -p $dir
done

# generate test data
sed 1d "$listfn" | while read task subj date run trial reason ; do
 tsv=$taskdir/$task/Basic/$subj/$date/Raw/EyeData/txt/$subj.$date.$run.tsv
 [ ! -r $tsv ] && echo cannot read $tsv && continue
 cp "$tsv" input/

 #TODO: Run R to get scored saccades
 #save in expected as $(basename $tsv .tsv).rout
done 
