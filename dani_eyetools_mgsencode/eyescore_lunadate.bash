#!/usr/bin/env bash

##
# score MGSEncode eye tracking for a subject and date
##
# ./eyescore_lunadate.bash 11217 20141203 
# REDO=1 ./eyescore_lunadate.bash 11217 20141203 
#
#
# main function is to run:
# Rscript --vanilla --quiet eyescoreScript.R \
#   taskPath=\"/Volumes/Phillips/COG\" task=\"MGSEncode\" \
#   eyescoreFunctions=\"eyescoreFunctions.R\" \
#   path=\"/Volumes/Phillips/COG/MGSEncode/$id/$date/\" 
#   id=$id date=$date \
#     2>&1 | tee -a $logFile | tee $scorelog 
#
#

## standard top
set -e
scriptdir=$(cd $(dirname $0);pwd)
#exiterr and trap
source $scriptdir/help.bash

## Check inputs
[ -z "$1" -o -z "$2" ] && exiterr "need subject and date"
id="$1"
date="$2"
scoredir=/Volumes/Phillips/COG/MGSEncode/$id/$date/
exiterr "$scoredir" "$id $date: score dir DNE ($scoredir)"

## Dani variables
path=/Volumes/Phillips/COG
logFile=$path/.log
eyescoreFunctions=$scriptdir/eyescoreFunctions.R
eyescoreScript=$scriptdir/eyescoreScript.R
for filetocheck in $eyescoreFunctions $eyescoreScript; do
  exiterr "$filetocheck" "needed file/folder DNE, cannot run function"
done



## parse eyd files
## done by eydToRawTXT.sh
## using ../dataFromAnyEyd.pl



## skip if it looks like we did this before 
nscore=$(ls $scoredir/*[1-4]_scored.txt 2>/dev/null|wc -l)
[ -z "$REDO" -a $nscore -ge 3 ] && 
 echo "$id $date: already have $nscore run_scored.txt; skipping" &&
 exit 0

## skip if we tried this before
skipfile=/Volumes/Phillips/COG/MGSEncode/$id/$date/scoreerr
[ -n "$REDO" -a -r "$skipfile" ] && rm "$skipfile"
[ -r $skipfile ] && 
  echo "$id $date: already failed to run; skipping (rm $skipfile)" && 
  exit 0

## score with R script
# keep record of command used to score
scorelog=$scoredir/scorelog
scorecmd=$scoredir/scorecmd
echo "scoring $id $date"
echo "  see $scorecmd"
echo "  output: $scoredir/*[1-4]_scored.txt"

cat > $scorecmd <<HEREDOC
Rscript --vanilla --quiet $eyescoreScript \
  path=\"$scoredir\" taskPath=\"$path\" task=\"MGSEncode\" \
  id=$id date=$date eyescoreFunctions=\"$eyescoreFunctions\" \
  2>&1 | tee -a $logFile | tee $scorelog 
HEREDOC

chmod +x $scorecmd
$scorecmd || echo "$id $date: failed to run $scorecmd"


## write a skip file if we failed to score all runs
nscore=$(ls /Volumes/Phillips/COG/MGSEncode/$id/$date/*[1-4]_scored.txt 2>/dev/null|wc -l)
[ $nscore -lt 3 ] && 
  date +%F > $skipfile

exit 0
