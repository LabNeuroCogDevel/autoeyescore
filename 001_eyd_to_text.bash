#!/usr/bin/env bash
[ ! -d txt ] && mkdir txt 
if [[ $(hostname) =~ arnold ]]; then
   bea_res="$HOME/rcn/bea_res/"
   [ ! -r $bea_res ] && echo "run 000_mnt_oacres1.bash" && exit 1
else
   bea_res=/Volumes/L/bea_res/
fi

[ ! -s txt/eyd_version.txt ] &&
find $bea_res/Data/Tasks/ -iname '*eyd' |
  while read f; do 
     echo "$f $(sed -ne '/EYE\|Version/{s/.*\(V\|: \)//;p;q}' "$f"||echo NA)"
done | tee txt/eyd_version.txt

# convert all the 1.20
export bea_res
[ ! -s txt/eyd_to_tsv_120.m ] && 
 perl -salne '
   next unless / 1\.20/;
   next unless m:^(.*)/(Fix|Mix|VGS|Anti)/Basic/(\d{5})/(\d{8})/:;
   $new_file="$ENV{bea_res}/Data/Tasks/$2/Basic/$3/$4/Raw/EyeData/txt/$3.$4.1.data.tsv";
   $F[0] =~ s:$1:$ENV{bea_res}/Data/Tasks:;
   print "read_eyd5('"'"'$F[0]'"','"'$new_file'"'"');";
   ' txt/eyd_version.txt > txt/eyd_to_tsv_120.m

echo "NOT RUNNING octave. handled by mktsv.bash or mgs/00_eyd.bash"
echo "   octave txt/eyd_to_tsv_120.m"
