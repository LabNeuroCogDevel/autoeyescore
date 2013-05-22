#!/usr/bin/env bash

# 
# wrapper to run/resume scoring on everyone
#
#  [REQIRED]  -t <task>  see $0 -t list
#  [OPTIONAL] -C         compare scoring against manual
#  [OPTIONAL] -R         remove and redo all auto-scoring
# 
#  USAGE EXAMPLES
#    $0 -R -C -t scannerbars # rescore and compare scannerbars
# 
#    $0 -t bars              # update all unscored behav bars
# 
#    $0 -t list              # list aval. eye scoring experements 
# 
# 
#  PIPELINE   
# 
#   0. remove old if -R specified
#   1. mktsv.sh  [depends on ../eyds/dataFromAnyEyd.pl ]
#   2. score.R   [depends on ScoreRun.R ScoreEveryone.R]
#   3. compareToManual.pl if -C provided
#  
#   expect files like
#  
#   /mnt/B/bea_res/Data/Tasks/<task>/Basic/<subj>/<date>/
#      Scored/Run0<num>/fs*xls       # manually scored file
#      Raw/EyeData/*<runnum>.eyd     # raw eye tracking in ASL v6
# 
#  SEE ALSO
# 
#   getSacsbyDot.R                 for subj.date.run.trial
#                                  notation testing/viewing
#
#   library(debug);
#   mtrace(getSacs)                for debugging in R
#END

### print top of file as help
function printhelp {
 basename $0 | tr '[a-z]' '[A-Z]'
 sed -n "s:\$0:$0:g;s/# //p;/#END/q" $0
 echo;
 exit 1
}


### Process args
while [ -n "$1" ]; do
 arg=$1; shift;
 case $arg in
 
  -t) type=$1; shift;; # type; eg. scanner base
  -R) REDO=1        ;; # REDO everything; useful when code changes
  -C) COMPARE=1       ;; # COMPARE against manual
  *)  printhelp     ;;
 esac
done

### ARG COMPAREING
[ -z "$type" ] && printhelp

## list all types we can find
if [ "$type" = "list" ]; then
 echo "Aval. types:"
 find $(dirname $0) -name \*settings.R | xargs -n1 dirname | xargs -n1 basename |sed 's/^/  /'
 exit 0
fi

settingsfile=$(dirname $0)/$type/$type.settings.R
[ ! -f $settingsfile ] && echo "$type does not have a settings.R file!" && exit 1

scorebase=$(perl -lne 'print $1 if m/filebasedir[ \t]*<-[ \t]*(.*)/' $settingsfile|sed "s/['\"]//g")
[ ! -d "$scorebase" ] && echo "settings file points to nonexistant task base directory! ($scorebase)" && exit 1 
echo "Task in $scorebase"

# everythign is run from settings file, so go there
cd $(dirname $settingsfile)




## make old file name incase we need it
old=old/$(date +%F); 

# some days we have a lot of old files
i=1
oldprfx=$old
while [ -d $old ]; do 
 old=$old.$i
 let i++
done

### Remove old score files
### only do this if scoring R scripts have changed has changed
### todo: check that they pass tests
echo "===== Moving old files ===="
mkdir -p $old
mv results $old/

if [ -n "$REDO" ]; then
   mkdir -p tsv/$old
   ls -1d  $scorebase/*/*/Scored/txt/ |while read file; do
     mv "$file" tsv/$old/$(echo "$file" |perl -lne 'print "$1.$2" if m:(\d{5})/(\d{8}):;');
   done
   mv aux tsv/$old/
fi

### create any new tsv files
echo "===== Looking for new runs ===="
$(dirname $0)/mktsv.bash

### timestamp
mkdir results
echo "start: $(date)" | tee results/timing

### rescore everyone
echo "===== Running scoring for everyone ===="
R CMD BATCH ../score.R

if [ -n "$COMPARE" ]; then
   ### check against manual scores
   echo "===== comparing manual to automatic ===="
   ./compareToManual.pl

   echo "===== %incorrect against scorers ===="
   echo "scores_off	lats_off" | tee results/accuracy-overal.txt
   grep -v '^*' checkBars_trial.csv |perl -slane 'next  unless /20\d\d/; $i++; if($F[1] != $F[3]){$a++}elsif(abs($F[2]-$F[4])>50){ $o++ }END{print join("\t", map{$_/($i-1)}($a,$o))}'| tee -a results/accuracy-overall.txt

   echo;echo;echo;
   grep -v '^*' checkBars_trial.csv |cut -f 2,4 -d"	" |sort|uniq -c|sort -n | tee results/accuracy-breakdown.txt
fi;

## wrap up
echo "done: $(date)" | tee -a results/timing
rm Rplots.pdf
mv score.Rout results/
[ -n "$COMPARE" ] && mv checkBars_trial.csv results/
