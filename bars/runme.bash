#!/usr/bin/env bash

### Remove old score files
### only do this if scoring R scripts have changed has changed
### todo: check that they pass tests
echo "===== Moving old scoring files ===="
scorebase='/mnt/B/bea_res/Data/Tasks/BarsBehavioral/Basic'
old=old/$(date +%F); 
mkdir -p $old
ls -1d  $scorebase/*/*/Scored/txt/ |while read file; do
  mv "$file" $old/$(echo "$file" |perl -lne 'print "$1.$2" if m:(\d{5})/(\d{8}):;');
done

### create any new tsv files
echo "===== Looking for new runs ===="
$(dirname $0)/mktsv.bash


### rescore everyone
echo "===== Running scoring for everyone ===="
R CMD BATCH score.R

### check against manual scores
echo "===== comparing manual to automatic ===="
./compareToManual.pl
