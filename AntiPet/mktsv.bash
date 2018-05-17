#!/usr/bin/env bash

#
# find all eyd files
# convert to tsv if needed
#
tsvListFile="tsvs.ls"
echo -n > $tsvListFile
for f in $(
         find /mnt/B/bea_res/Data/Tasks/AntiPet//Basic/ -maxdepth 5 -mindepth 5 -iname \*.eyd -newermt 2013-03-01| 
          sort -t/ -k10nr # do newest first
         ); do

 echo $f
 outdir="$(dirname "$f")/txt"
 [ ! -d $outdir ] && mkdir -p $outdir

 tsv="$outdir/$(echo $f | perl -lne 'print "$+{id}.$+{date}.anti.1.tsv" if m:/(?<id>\d{5})/(?<date>\d{8})/Raw:')"
 echo "$tsv" >> $tsvListFile

 [ -r $tsv ] && echo "skipping $tsv" && continue

 set -x
 ../../eyds/dataFromAnyEyd.pl $f > $tsv
 set +x
done
