#!/usr/bin/env bash
set -xeuo pipefail

EDFDIR="$HOME/scratch/EyeLink_LinuxDevKit_1.11_x64_debs/usr/bin/EdfConverter"
edfconv(){ LD_LIBRARY_PATH=$EDFDIR java -jar $EDFDIR/edfconverter.jar "$@"; }

get_asc(){

  # currently working on all files at once
  # better to have single command to make predictable .asc.gz file
  origdir=/Volumes/NCTRC/SITS/Tasks/AnnualLabVisit/Pupil_Tasks/RAS/Data
  datadir=$(pwd)

  # only sync edfs we dont already have
  find $datadir/asc/ -iname '*asc.gz' | sed 's:.*/::; s/.asc.gz/.edf/;' > had_edf.txt
  rsync -n --size-only --exclude-from=had_edf.txt -vrhi $origdir/*.edf $datadir/edf/

  # convert all the new files
  mapfile -t all_edfs < <(find $datadir/edf/ -iname '*edf')
  [ ${#all_edfs[@]} -gt 0 ] && edfconv "${all_edfs[@]}"

  # gzip, mv, and rm edf copy
  for f in $datadir/edf/*asc; do
     [ ! -r $f ] && echo "$f DNE" && continue
     gzip $f
     gz=$f.gz
     mv $gz $datadir/asc/
     test -r $datadir/asc/$(basename $gz) && rm ${gz/.asc.gz/.edf} || echo "warning: issue w/ $_"
  done
}

# if not sourced
if ! [[ "$(caller)" != "0 "* ]]; then
  set -euo pipefail
  trap 'e=$?; [ $e -ne 0 ] && echo "$0 exited in error $e"' EXIT
  get_asc
  exit $?
fi
