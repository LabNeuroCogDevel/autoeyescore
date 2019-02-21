#!/usr/bin/env bash
set -e
trap '[ $? -ne 0 ] && echo "exit with error!"' EXIT 

BEA_RES=/Volumes/L/bea_res
[ $(ls $BEA_RES | wc -l) -lt 1 ] && echo cannot find $BEA_RES && exit 1

function log {
 logfile=logfile.txt
 sed "s/^/[$(date +%F::%H:%M)] /" | tee -a $logfile
}
function checkandrm {
 outname="$1"
 removedir="$2"  # if second param, we want to remove dir too
 [ -z "$outname" ] && return 0
 if [ -r "$outname" ]; then 
   [ $(cat $outname 2>/dev/null |wc -l) -gt 0 ] && return 0
   echo "$outname removed because it is empty!" |log >&2
   rm $outname || echo "failed: removing $outname"
 fi
 [ -n "$removedir" ] && rmdir $(dirname $outname)

}
function extractEyd {
  eyd=$1;
  [ -z "$eyd" -o ! -r "$eyd" ] && echo "cannot find eyd '$eyd'" >&2 && return 1
  subj=$2; visit=$3;
  [ -z "$visit" ] && echo "extractEyd needs subj and visit" >&2 && return 1

  filen=$(basename $eyd .eyd| tr [A-Z] [a-z] )
  [[ ! $filen =~ mgs$ ]] && echo "$subj $visit. bad filename: $filen from $eyd" >&2 && return 1

  savedir=$(dirname $eyd)/txt/
  [ ! -d $savedir ] && mkdir -p $savedir 
  outname="$savedir/$subj.$visit.mgs.raw.txt"

  # run if we haven't
  [ ! -s $eyd ] && echo "# empty eyd file $eyd" >&2 && return 1
  if [ ! -r $outname -o $(cat $outname 2>/dev/null  | wc -l ) -lt 2 ]; then 

    if sed 1q $eyd |grep 'EYEDAT V1.20'; then
       octave -q <(echo "addpath('..'); read_eyd5('$eyd','$outname');")  || echo "$subj $visit: $eyd bad eyetracking data!" >&2
    else
      ../dataFromAnyEyd.pl $eyd > $outname  || echo "$subj $visit: $eyd bad eyetracking data!" >&2
    fi 
    checkandrm $outname "removdirtoo"
  fi

  [ -r $outname ] && echo $outname
  return 0

}


function realLog {
    savdir=$1
    out=$2
    [ -z "$2" -o -z "$1" -o ! -d "$savdir" ] && echo "realLog: bad inputs: savdir '$savdir' out '$out'" >&2 && return 1

    local logpath=$savdir/../..
    local logpatt='MGS*txt'
    local log=( $(find $logpath -maxdepth 2 -iname "$logpatt") )

    # skip if we dont have exactly one log file
    [ ${#log[@]} -ne 1 ] && 
      echo "[$s $v] not exactly one log (${#log[@]}) in $logpath $logpatt: '$log'" >&2 &&
      return 1

    # save log
    ./parseEP1es.pl $BEA_RES/Tasks/Behavorial/mgs-beakid/MGS.es $log > $out
}

function fakeLog {
    eyein="$1"
    out="$2"
    [ -z "$out" -o -z "$eyein" -o ! -r "$eyein" ] && echo "fakelog: bad inputs: eyein '$eyein' out '$out'" >&2 && return 1
    [ $(cat $eyein | wc -l) -lt 2 ]  && echo "bad eyefile" >&2 && return 1
    echo "#[fakelog] $eyein > $out" >&2
    ./fakeLog.pl $eyein > $out
}

function extractLog {
    eyein=$1 
    [ -z "$eyein" -o ! -r "$eyein" ] && echo "cannot find eyein '$eyein'" >&2 && return 1
    s=$2;v=$3;
    [ -z "$v" ] && echo "extractLog needs subj and visit" >&2 && return 1

    savdir="$(dirname $eyein)"
    # make sure we have raw eye data before trying to get log data
    out=$savdir/$s.$v.MGS.EPxdat.log
    [ ! -d $(dirname $out) ] && echo "[$s $v] no score dir for $out" >&2 && return 1

    # dont do if we have, comment to redo
    #[ -r $out -a $(cat $out |wc -l) -gt 1 ] && return 0

    if [ ! -s $out ]; then
       realLog $savdir $out || fakeLog $eyein $out
       checkandrm $out
    fi
}



####### MAIN

# given an eyd or we find all eyds
eydpat=$@
[ -z $eydpat ] &&  eydpat=($BEA_RES/Data/Tasks/MGS/Basic/*/*/[rR]aw/[eE]ye*/*.eyd)

# for everything we are working on
for eyd in ${eydpat[@]};do

  [[ ! $eyd =~ ([0-9]{5})/([0-9]{8}) ]] && echo "cannot find subj date from $eyd" >&2 && continue

   subj=${BASH_REMATCH[1]}
  visit=${BASH_REMATCH[2]}

  # parsed eye data
  eye=$(extractEyd $eyd $subj $visit|| echo "")
  [ -z "$eye" -o ! -r "$eye" ] && continue
  # some are corrupt, eg.
  #  /Users/lncd/rcn/bea_res/Data/Tasks/MGS/Basic/10480//20071016//Raw/EyeData/txt/
  # and wont return a file
  extractLog $eye $subj $visit || continue
  
done
