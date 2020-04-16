#!/usr/bin/env bash
[ -z "$REDO" ] && REDO="" 
set -euo pipefail
trap 'e=$?; [ $e -ne 0 ] && echo "$0 exited in error"' EXIT
cd $(dirname $0)/audit/

#
# concat all drops
#  20200415WF  init  (from commands run 20200330)
[ $# -ne 1 ] && echo "give me a task from to run on" && exit 1
TASK="$1"
[ ! -d $TASK ] && echo "$TASK does not exist in $(pwd)" && exit 1

finalpdf=$(pwd)/${TASK}_drop.pdf
[ -z "$REDO" -a -r $finalpdf ] && echo "have $finalpdf; use REDO=1 $0 $@" && exit 0


[ -z "$REDO" ] && cmd="todo"  || cmd="redo"
#../eyeQC.R $TASK --pdf $cmd

#pdftk  /Volumes/Hera/Projects/autoeyescore/audit/$TASK/pdf/*/*-1.pdf cat output $finalpdf

for s in $(pwd)/$TASK/pdf/*/; do
   [ -r $s/drop.pdf -a -z "$REDO" ] && continue
   [ $(ls $s/*-1.pdf|wc -l) -eq 0 ] && continue
   pdftk $s/*-1.pdf cat output $s/drop.pdf
done
pdftk  /Volumes/Hera/Projects/autoeyescore/audit/$TASK/pdf/*/drop.pdf cat output $finalpdf
