#!/usr/bin/env bash
set -e
trap 'e=$?; [ $e -ne 0 ] && echo "$0 exited in error"' EXIT

#
# cogemo sounds eye tracking files
# convert raw eyd to raw txt
#

 find /Volumes/L/bea_res/Data/Tasks/CogEmoSoundsBasic/*/*/Raw/EyeData/ -iname '*eyd' |
    perl -MFile::Basename -lne 'next unless m:/(\d{5})/(\d{8})/.*(\d).eyd$:; print dirname($_)."/txt/$1.$2.$3.data.txt ",$_'|
    while read out infile; do
       [ -s $out ] && continue
       echo $out
       d=$(dirname $out)
       [ ! -d $d ] && mkdir $d
       ./dataFromAnyEyd.pl $infile > $out
done

