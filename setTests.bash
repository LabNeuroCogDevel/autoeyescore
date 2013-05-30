#!/usr/bin/env bash

######
# 
#  get parsed raw data needed by trials.txt in test directory
#
#####

taskdir=/mnt/B/bea_res/Data/Tasks/

# list of trials to explore
listfn='trials.txt';
[ ! -r $listfn ] &&  echo "need $listfn" && exit 1

# remake test directories
for dir in expected input; do
   [ -d $dir ] && rm -r $dir
   mkdir -p $dir
done

# generate test data
sed 1d "$listfn" | while read task subj date run trial reason ; do
 tsv=$taskdir/$task/Basic/$subj/$date/Raw/EyeData/txt/$subj.$date.$run.data.tsv
 [ ! -r $tsv ] && echo cannot read $tsv && continue
 cp "$tsv" input/

 #TODO: Run R to get scored saccades
 #save in expected as $(basename $tsv .tsv).rout
done 
