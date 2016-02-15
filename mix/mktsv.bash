#!/usr/bin/env bash

#
# find all eyd files
# convert to tsv if needed
#

# where is base dir?
task=Mix
basedir="/Users/lncd/rcn/bea_res/Data/Tasks/$task/Basic/"
[ ! -r $dir ] && basdir="/mnt/B/bea_res/Data/Tasks/$task/Basic/"

tsvListFile="tsvs.ls"
echo -n > $tsvListFile
for f in $(
         find $basedir -maxdepth 5 -mindepth 5 -iname \*.eyd -newermt 2008-01-01| 
          sort -t/ -k10nr # do newest first
         ); do

 echo $f
 outdir="$(dirname "$f")/txt"
 [ ! -d $outdir ] && mkdir -p $outdir

 tsv="$outdir/$(echo $f | perl -lne 'print "$+{id}.$+{date}.mix.1.data.tsv" if m:/(?<id>\d{5})/(?<date>\d{8})/Raw:')"
 echo "$tsv" >> $tsvListFile

 [ -r $tsv ] && echo "skipping $tsv" && continue

 ../../eyds/dataFromAnyEyd.pl $f > $tsv
done
