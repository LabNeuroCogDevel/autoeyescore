#!/bin/bash
set -x

# single argument for step 1 or step 2 (correction script, re-score script)
step=$1
if [ $step != 1 ] && [ $step != 2 ]; then exit 1; fi # need step argument

# path and file names
path=/Volumes/Phillips/COG
correctPath=/Volumes/Phillips/COG/offsetDelayCorrect
pathScripts=$path/autoeyescore/dani_eyetools
eyescoreFunctions=$pathScripts/eyescoreFunctions.R
eyescoreScript=$pathScripts/eyescoreScript.R
correctScript=$pathScripts/offsetDriftCorrect/eyescoreScript_odc.R
logFile=$path/offsetDriftCorrect/.log

if [ ! -e $path/maxJobs ]; then exit 1; fi # need maxJobs for multicore

cd $path
tasks="MGSEncode AntiState"
correctionMethods="fixation scored"
for corMethod in $correctionMethods; do
  for task in $tasks; do
    ids=$( ls $path/$task )
    for id in $ids; do
      dates=$( ls $path/$task/$id )
      for date in $dates; do
        pathSession=$path/$task/$id/$date
        pathOut=$path/offsetDriftCorrect/$corMethod/$task/$id/$date
        mkdir -p $pathOut
        case $step in
          1) Rscript --vanilla --quiet $correctScript \
            path=\"$pathSession\" taskPath=\"$path\" task=\"$task\" \
            id=$id date=$date eyescoreFunctions=\"$eyescoreFunctions\" \
            correctionMethod=\"$corMethod\" >> $logFile 2>&1 & ;;
          2) Rscript --vanilla --quiet $eyescoreScript \
            path=\"$pathOut\" taskPath=\"$path\" task=\"$task\" \
            id=$id date=$date eyescoreFunctions=\"$eyescoreFunctions\" \
            >> $logFile 2>&1 & ;;
        esac
        set +x # set +x for suppressing clutter
        maxJobs=$( cat $path/maxJobs )
        while [ $( jobs | wc -l ) -ge $maxJobs ]; do sleep 1; done
        set -x
      done
    done
  done
done


