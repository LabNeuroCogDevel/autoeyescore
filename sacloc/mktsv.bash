#!/usr/bin/env bash

#
# find all eyd files
# convert to tsv if needed
#
converter=../../eyds/dataFromAnyEyd.pl
[ ! -x $converter ] && converter=../dataFromAnyEyd.pl
[ ! -x $converter ] && echo "cannot find $converter!" && exit 1

tsvListFile="tsvs.ls"
echo -n > $tsvListFile
for f in $(
         find /mnt/B/bea_res/Data/Tasks/SaccadeLocalizerBasic/ -maxdepth 5 -mindepth 5 -iname \*.eyd | 
          sort -t/ -k10nr # do newest first
         ); do

 echo $f
 outdir="$(dirname "$f")/txt"
 [ ! -d $outdir ] && mkdir -p $outdir

 tsv="$outdir/$(echo $f | perl -lne 'print "$+{id}.$+{date}.1.data.tsv" if m:/(?<id>\d{5})/(?<date>\d{8})/Raw:')"
 echo "$tsv" >> $tsvListFile

 [ -r "$tsv" ] && [ $(cat $tsv|wc -l) -gt 1 ] && echo "skipping $tsv" && continue

 set -x
 $converter $f > $tsv
 set +x
done
