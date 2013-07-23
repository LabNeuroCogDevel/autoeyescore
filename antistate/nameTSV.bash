basicdir="/mnt/B/bea_res/Data/Tasks/AntiState/Basic/"
# give eyd file, print tsv
function nameTSV {
 echo $1 | perl -lne 'print "$+{id}.$+{date}.$+{run}.data.tsv" if m:/(?<id>\d{5})/(?<date>\d{8})/Raw/EyeData/.*run(?<run>\d):i'
}
