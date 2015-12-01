#!/bin/bash

# find eyd files
# create cog dirs for subjects
# get txt eyd

# make sure Phillips (from skynet) exists
# mount -t smbfs //lncd@10.145.65.72/Phillips /Volumes/Phillips
# make sure bea_res is mounted (rcndata)
#
set -e

path=/Volumes/Phillips/COG
rcndata="/Users/lncd/rcn/bea_res/Data/Tasks/"
#pathScripts=$path/autoeyescore/dani
pathScripts=$(cd $(dirname $0); pwd)
maxjobfile=$path/maxJobs
eydparser="$pathScripts/../dataFromAnyEyd.pl"
eyescoreFunctions=$pathScripts/eyescoreFunctions.R
eyescoreScript=$pathScripts/eyescoreScript.R

# exiterr and trap
source $pathScripts/help.bash

for filetocheck in $maxjobfile $eydparser $eyescoreFunctions $eyescoreScript $rcndata; do
  exiterr "$filetocheck" "needed file/folder DNE, cannot run function"
done

task=MGSEncode
eydpath="$rcndata/MGSEncode/Basic/"
find $eydpath -iname "*.eyd" | while read eyd; do
   # check eyd's sanity, extract numbers
   # should match /{lunaid}/{date}/.*{runnum}.eyd
   ! [[ "$eyd" =~ /(1[0-9][0-9][0-9][0-9])/([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9])/.*([1-4]).eyd ]] && 
     echo "$eyd does not have luna and date!" >&2 && continue

   # from regexp above, extract our numbers
   id=${BASH_REMATCH[1]}
   date=${BASH_REMATCH[2]}
   rn=${BASH_REMATCH[3]}

   eydout=/Volumes/Phillips/COG/MGSEncode/$id/$date/${id}_${date}_${task}_run${rn}_raw.txt

   # check exists before running
   #[ -r "$eydout" ] && echo "already did $eydout!" && continue
   [ -r "$eydout" ] && continue

   # make sure we have a dir to save to
   [ ! -d $(dirname $eydout) ] && mkdir -p $(dirname $eydout) && date +%F > $(dirname $eydout)/createdFromB

   # run parser
   $eydparser $eyd > $eydout || echo "cannot write $eydout!" >&2


done
