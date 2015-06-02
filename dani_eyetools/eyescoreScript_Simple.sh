#!/bin/bash


## DM DS WF  -- dani's scripts
# this is the head of eyescoreScript.R
# simplied to just handle the first step: score eyd files

# make sure Phillips (from skynet) exists
# mount -t smbfs //lncd@10.145.65.72/Phillips /Volumes/Phillips
#
set -e

path=/Volumes/Phillips/COG
#pathScripts=$path/autoeyescore/dani
pathScripts=$(cd $(dirname $0); pwd)
maxjobfile=$path/maxJobs

# exiterr and trap functions
source $pathScripts/help.bash
exiterr "$maxjobfile" "$maxjobfile DNE"


cd $path
tasks="MGSEncode"
for task in $tasks; do
  ids=$( ls $path/$task )
  for id in $ids; do
    dates=$( ls $path/$task/$id )
    for date in $dates; do
        $pathScripts/eyescore_lunadate.bash $id $date &

        maxJobs=$( cat $path/maxJobs )
        while [ $( jobs | wc -l ) -ge $maxJobs ]; do sleep 1; done
    done

  done
done

# wait for all to finish
wait
