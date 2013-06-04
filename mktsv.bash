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
 [ -r $tsv ] && continue

 [ ! -d $outdir ] && mkdir -p $outdir

 echo $f \> $tsv
 ../../eyds/dataFromAnyEyd.pl $f > $tsv
done

## OLD: sort by age
#for f in $(
#         find /mnt/B/bea_res/Data/Tasks/BarsScan/Basic/ -maxdepth 5 -mindepth 5 -iname \*.eyd | 
#          sort -t/ -k10nr # do newest first
#         ); do
