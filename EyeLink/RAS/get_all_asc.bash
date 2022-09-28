#!/usr/bin/env bash
EDFDIR="$HOME/scratch/EyeLink_LinuxDevKit_1.11_x64_debs/usr/bin/EdfConverter"
edfconv(){ LD_LIBRARY_PATH=$EDFDIR java -jar $EDFDIR/edfconverter.jar "$@"; }

fetch_edf(){
   DRYRUNrsync=""
   [ -n "$DRYRUN" ] && DRYRUNrsync="-n"

  # currently working on all files at once
  # better to have single command to make predictable .asc.gz file
  origdir=/Volumes/SITS/Tasks/AnnualLabVisit/Pupil_Tasks/RAS/Data
  datadir=$(pwd)

  # only sync edfs we dont already have
  find $datadir/asc/ -iname '*asc.gz' | sed 's:.*/::; s/.asc.gz/.edf/;' | mkifdiff had_edf.txt
  rsync $DRYRUNrsync --size-only --exclude-from=had_edf.txt -vrhi $origdir/*.edf $datadir/edf/
}

# gzip, mv, and rm edf copy
addgz(){
  local f="$1"
  [ ! -r $f ] && echo "$f DNE" && return
  gzip $f
  gz=$f.gz
  mv $gz asc/
  test -r asc/$(basename $gz) && rm ${gz/.asc.gz/.edf} || echo "warning: issue w/ $_"
  return
}

edf_to_asc(){
  datadir=$(pwd)
  # convert all the new files
  mapfile -t all_edfs < <(find $datadir/edf/ -iname '*edf')
  [ ${#all_edfs[@]} -gt 0 ] && edfconv "${all_edfs[@]}"

  # gzip, mv, and rm edf copy
  for f in $datadir/edf/*asc; do
     addgz $f
  done
}

# if not sourced
if ! [[ "$(caller)" != "0 "* ]]; then
  set -euo pipefail
  trap 'e=$?; [ $e -ne 0 ] && echo "$0 exited in error $e"' EXIT
  [ -v DRYRUN ] && DRYRUN=echo || DRYRUN=""
  cd $(dirname $0)
  fetch_edf
  edf_to_asc
  exit $?
fi
