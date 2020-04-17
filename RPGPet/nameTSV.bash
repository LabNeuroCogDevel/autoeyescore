basicdir="/mnt/B/bea_res/Data/Tasks/RPG/mMR/"
# give eyd file, print tsv
function nameTSV {
 echo $f | perl -lne 'print "$+{id}.$+{date}.rpg.$+{run}.tsv" if m:/(?<id>\d{5})/(?<date>\d{8})/.*run(?<run>[1-6]):'
}
