#!/usr/bin/env bash

### Remove old score files
### only do this if scoring R scripts have changed has changed
### todo: check that they pass tests
echo "===== Moving old scoring files ===="
scorebase='/mnt/B/bea_res/Data/Tasks/BarsScan/Basic'
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

echo "===== %incorrect against scorers ===="
echo "score'soff	lat's off"
grep -v '^*' checkBars_trial.csv |perl -slane 'next  unless /20\d\d/; $i++; if($F[1] != $F[3]){$a++}elsif(abs($F[2]-$F[4])>50){ $o++ }END{print join("\t", map{$_/($i-1)}($a,$o))}'| tee results/accuracy_against_scorers-$(date +%F).txt

echo;echo;echo;
grep -v '^*' checkBars_trial.csv |cut -f 2,4 -d"	" |sort|uniq -c|sort -n | tee -a results/accuracy_against_scorers-$(date +%F).txt
