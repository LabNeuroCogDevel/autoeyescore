#!/usr/bin/env bash

#
# find all eyd files
# convert to tsv if needed
#

#
# because every paradigmn does things slightly different, there is a nameTSV.bash script holding a function
# that should be in the cwd
[ ! -f nameTSV.bash ] && echo "not in a task score directory! cannot find $(pwd)/nameTSV.bash" && exit 1
source nameTSV.bash

for f in $( find $basicdir -maxdepth 5 -mindepth 5 -iname \*.eyd ); do
# eyds are <subj>/<date>/Raw/EyeData/*eyd in from basicdir

 outdir="$(dirname "$f")/txt"
 tsv="$outdir/$(nameTSV $f)"

 #[ -r $tsv ] && echo "skipping $tsv" && continue
 [ ! -d $outdir ] && mkdir -p $outdir

 if [ ! -r $tsv ]; then
  echo $f \> $tsv
  ../dataFromAnyEyd.pl $f > $tsv
  # remove if cannot understand it's format
  [ "$(cat $tsv|wc -l)" -lt 10 ] && echo "Removed $tsv!" && rm $tsv
 fi


 ## parse eprime if we can
 # extract rr from sss.dddd.r.data.csv
 rawdir=$(dirname $(dirname $f))
 run=$(basename $tsv .data.tsv)
 run=${run##*.} 
 eplog=$( find $rawdir/E[pP]rime/ -iname "* $run-$subject*txt" 2>/dev/null |tail -n1) # take the highest number
 eptxt="${tsv/data.tsv/eplog}.txt" #append .txt to make sure we don't overwrite anything

 # skip if we already have eptxt or if there is no eplog
 [ -r $eptxt -o -z "$eplog" ] && continue
 # proceed if we can read eplog
 [ -r "$eplog" ] && ../parseEP.pl "$eplog" > "$eptxt" && echo "$eplog > $eptxt"
 [ "$(cat $eptxt|wc -l)" -lt 10 ] && echo "Removed $eptxt (bad $eplog?)!" && rm $eptxt

done

## OLD: sort by age
#for f in $(
#         find /mnt/B/bea_res/Data/Tasks/BarsScan/Basic/ -maxdepth 5 -mindepth 5 -iname \*.eyd | 
#          sort -t/ -k10nr # do newest first
#         ); do
