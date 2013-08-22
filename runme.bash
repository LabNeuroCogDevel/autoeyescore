#!/usr/bin/env bash

# 
# wrapper to run/resume scoring on everyone
#
#  [REQIRED]  -t <task>  see $0 -t list
#  [OPTIONAL] -T         test ( -t not required )
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
#    $0 -T                   # run all tests
#    $0 -T bars              # run tests on bars
#    $0 -T -t bars           # same as above (NB order is important, '-t bars -T' does all)
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
 
  -t) type=$1; shift;;                          # type; eg. scanner base
  -T) TEST=1; [ -n "$1" ] && type=$1 && shift;; # type; eg. scanner base
  -R) REDO=1        ;;                          # REDO everything; useful when code changes
  -C) COMPARE=1     ;;                          # COMPARE against manual
  *)  printhelp     ;;
 esac
done

### ARG COMPAREING
[ -z "$type" -a -z "$TEST" ] && printhelp

basedir=$(cd $(dirname $0);pwd)

function list {
 find $basedir  -maxdepth 2 -name \*settings.R | xargs -n1 dirname | xargs -n1 basename
}
## list all types we can find
if [ "$type" = "list" ]; then
 echo "Aval. types:"
 list |sed 's/^/  /'
 exit 0
fi


## this is before TESTing portion because test can specify a single type. This makes sure that is valid.
## but if not type given for test, settingsfile is nonsense --  but it wont be used
settingsfile=$basedir/$type/$type.settings.R
[ ! -f $settingsfile -a -n "$type" ] && echo "$type does not have a settings.R file!" && exit 1

# export filebasedir and expectedTrialLengths
eval "$(perl -lne 'print if s/(filebasedir|expectedTrialLengths)[ \t]*<-[ \t]*/export $1=/ig' $settingsfile)"
# export expectedTrialLengths
# export filebasedir



## Run tests if told to
if [ -n "$TEST" ]; then
 [ -z "$type" ] && type="$(list)"
 for task in $type; do
   echo "===== TESTING $task ===="
   testdir=$basedir/$task/test
   [ ! -f "$testdir/trials.txt" ] && echo "	no tests available!" && continue
   cd $testdir
   
   # collect any parsed raw eye movements we dont already have
   $basedir/setTests.bash
   # run tests, test_file is set to chdir=T, making this a little ugly
   R CMD BATCH <( echo "library(testthat);wd<-getwd(); test_file('$basedir/scoreTests.R')")
   # show results
   cat *Rout
 done

 exit
fi

#filebasedir=$(perl -lne 'print $1 if m/filebasedir[ \t]*<-[ \t]*(.*)/' $settingsfile|sed "s/['\"]//g")
[ ! -d "$filebasedir" ] && echo "settings file points to nonexistant task base directory! ($filebasedir)" && exit 1 
echo "Task in $filebasedir"

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


#if [ -n "$REDO" ]; then
#  echo "===== TESTING BEFORE REDO ===="
#  $0 -T $type
#fi
### Remove old score files
### only do this if scoring R scripts have changed has changed
### todo: check that they pass tests
echo "===== Moving old files ===="
mkdir -p $old
mv results $old/

if [ -n "$REDO" ]; then
   echo "  ==== old TSVs too! ==="
   mkdir -p tsv/$old
   ls -1d  $filebasedir/*/*/Scored/txt/ |while read file; do
     oldscore=tsv/$old/$(echo "$file" |perl -lne 'print "$1.$2" if m:(\d{5})/(\d{8}):;');
     mkdir -p $oldscore
     # move sac and trial scoret txt files
     # but keep manual scored txt files
     mv "$file"/*{sac,trial}.txt $oldscore/
   done
   mv aux tsv/$old/
fi

### create any new tsv files
echo "===== Looking for new runs ===="
$basedir/mktsv.bash

### timestamp
mkdir results
echo "start: $(date)" | tee results/timing

### rescore everyone
echo "===== Running scoring for everyone ===="
R CMD BATCH ../score.R 
grep 'Execution halted' score.Rout  && echo "ERROR: could not finish!! see score.Rout" && exit 1

## if we have Rplots.pdf, remove it (ggsave leftover)
## if we dont, something went wrong, so print the end of the file
if [ -f Rplots.pdf ]; then 
 rm Rplots.pdf
else 
 tail score.Rout
fi


if [ -n "$COMPARE" ]; then
   ### check against manual scores
   echo "===== comparing manual to automatic ===="
   # need expectedTrialLengths and filebasedir exported
   if [ "$type" == "antistate"   ]; then
     echo "running compareToManual specially made for AntiState"
     $basedir/compareToManual_AntiState.pl
   else
     $basedir/compareToManual.pl
   fi

   echo "===== %incorrect against scorers ===="
   echo "scores_off	lats_off" | tee results/accuracy-overall.txt
   grep -v '^*' checkAgainstManual_trial.csv |perl -slane 'next  unless /20\d\d/; $i++; if($F[1] != $F[3]){$a++}elsif(abs($F[2]-$F[4])>50){ $o++ }END{print join("\t", map{$_/($i-1)}($a,$o))}'| tee -a results/accuracy-overall.txt

   echo;echo;echo;
   # plot
   grep -v '^*' checkAgainstManual_trial.csv |cut -f 2,4 -d"	" |sort|uniq -c|sort -k2r | grep -v 'NA' | tee results/accuracy-breakdown.txt
   R CMD BATCH ../compareToManual.R
   mv Rplots.pdf results/accuracy-breakdown.pdf
   git diff results/accuracy-overall.txt | cat  
fi;

## wrap up
echo "done: $(date)" | tee -a results/timing

mv score.Rout results/
# compare to auto scoring to scorers (results/checkAgainstManual_trial.csv), see what's changed (results/trailChanges.txt)
[ -n "$COMPARE" ] && mv checkAgainstManual_trial.csv results/ && $basedir/gitShowDiff.bash $type " " " " > results/trialChanges.txt
