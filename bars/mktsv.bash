#!/usr/bin/env bash

#
# find all eyd files
# convert to tsv if needed
#
for f in $(
         find /Users/lncd/rcn/bea_res/Data/Tasks/BarsBehavioral/Basic/ -maxdepth 5 -mindepth 5 -iname \*.eyd | 
          sort -t/ -k10nr # do newest first
         ); do

 outdir="$(dirname "$f")/txt"
 [ ! -d $outdir ] && mkdir -p $outdir

 tsv="$outdir/$(echo $f | perl -lne 'print "$+{id}.$+{date}.$+{run}.tsv" if m:/(?<date>\d{8})/.*/(?<id>\d{5})[^/]*bars(?<run>\d):')"

 [ -r $tsv ] && echo "skipping $tsv" && continue

 echo $f \> $tsv
 ../../eyds/dataFromAnyEyd.pl $f > $tsv
 #break
done
