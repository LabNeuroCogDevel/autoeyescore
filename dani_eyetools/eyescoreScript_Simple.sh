#!/bin/bash


## DM DS WF  -- dani's scripts
# this is the head of eyescoreScript.R
# simplied to just handle the first step: score eyd files

# make sure Phillips (from skynet) exists
# mount -t smbfs //lncd@10.145.65.72/Phillips /Volumes/Phillips
#
#set -x

path=/Volumes/Phillips/COG
rcndata="/Users/lncd/rcn/bea_res/Data/Tasks/"
#pathScripts=$path/autoeyescore/dani
pathScripts=$(cd $(dirname $0); pwd)
maxjobfile=$path/maxJobs
eydparser="$pathScripts/../dataFromAnyEyd.pl"
eyescoreFunctions=$pathScripts/eyescoreFunctions.R
eyescoreScript=$pathScripts/eyescoreScript.R

# exiterr and trap functions
source $pathScripts/help.bash

for filetocheck in $maxjobfile $eydparser $eyescoreFunctions $eyescoreScript $rcndata; do
  exiterr "$filetocheck" "needed file/folder DNE, cannot run function"
done

logFile=$path/.log


cd $path
tasks="MGSEncode"
for task in $tasks; do
  ids=$( ls $path/$task )
  for id in $ids; do
    dates=$( ls $path/$task/$id )
    for date in $dates; do
      ## parse eyd files
      ## done by eydToRawTXT.sh

      
      ## run R script
      pathSession=$path/$task/$id/$date
      #echo $task $id $date
      Rscript --vanilla --quiet $eyescoreScript \
        path=\"$pathSession\" taskPath=\"$path\" task=\"$task\" \
        id=$id date=$date eyescoreFunctions=\"$eyescoreFunctions\" \
        >> $logFile 2>&1 &

      ## wait for jobs to finish if we have too many
      # set +x for suppressing clutter
      set +x
      maxJobs=$( cat $path/maxJobs )
      while [ $( jobs | wc -l ) -ge $maxJobs ]; do sleep 1; done
      set -x

    done
  done
done

