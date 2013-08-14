basicdir="/mnt/B/bea_res/Data/Tasks/Fix/Basic/"
[ ! -d $basicdir ] && basicdir=/Users/lncd/rcn/bea_res/Data/Tasks/Fix/Basic/
[ ! -d $basicdir ] && echo "no basic dir: $basicdir" && exit 1

# give eyd file, print tsv
function nameTSV {
 echo $1 | perl -lne 'print "$+{id}.$+{date}.1.data.tsv" if m:/(?<id>\d{5})/(?<date>\d{8})/Raw:'
}
