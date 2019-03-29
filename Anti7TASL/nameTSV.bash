outbase="/Volumes/L/bea_res/Data/Tasks/Anti7TASL/"
basicdir="/Volumes/L/bea_res/Data/Temporary Raw Data/7T"
# give eyd file, print tsv
function nameTSV {
 echo $1 | perl -lne 'print "$+{id}.$+{date}.1.data.tsv" if m:/(?<id>\d{5})_(?<date>\d{8}):'
}

findeyd(){
  find "/Volumes/L/bea_res/Data/Temporary Raw Data/7T" -iname \*anti.eyd  -not -ipath '*dropped*' -print0
}

mkoutdir(){
  f="$1"
  ! [[ $f =~ ([0-9]{5})_([0-9]{8}) ]] && echo "'$f' does not have subj or date!?" >&2 && return 0
  subj=${BASH_REMATCH[1]}
  d8=${BASH_REMATCH[2]}
  echo "$outbase/$subj/$d8/Raw/EyeData/txt"
}

