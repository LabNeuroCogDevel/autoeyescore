basicdir="/Users/lncd/rcn/bea_res/Data/Tasks/BarsBehavioral/Basic/"
# give eyd file, print tsv
function nameTSV {
 echo $1 | perl -lne 'print "$+{id}.$+{date}.$+{run}.data.tsv" if m:/(?<date>\d{8})/.*/(?<id>\d{5})[^/]*bars(?<run>\d):'
}
