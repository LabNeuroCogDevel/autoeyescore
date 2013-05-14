#!/usr/bin/bash

# give me subject, date, run#, trial
# produce something like:
# scoreRun("/mnt/B/bea_res/Data/Tasks/BarsBehavioral/Basic/10717/20090727/Raw/EyeData/txt/10717.20090727.2.tsv",10717,2,"BarsBeh",58,writetopdf=FALSE,savedas="/mnt/B/bea_res/Data/Tasks/BarsBehavioral/Basic/10717/20090727/Scored/txt/10717.20090727.2.sac.txt",showplot=T)
#
# default directories
B="/mnt/B/bea_res/Data/Tasks/"; task="BarsBehavioral/Basic/"; settingsfile="../bars/bars.settings.R"
t_ttl=BarshBeh

# interpret input
while (( $#)); do
 case "$1" in
  -bb) shift; task=BarsBehavioral/Basic/;settingsfile="../bars/bars.settings.R"; t_ttl=BarsBeh ;;
  -sb) shift; task=BarsScanner/Basic/;settingsfile="../bars/bars.settings.R";    t_ttl=;;
  -s)  shift; subj=$1; shift ;;
  -d)  shift; date=$1; shift ;;
  -r)  shift;  run=$1; shift ;;
  -t)  shift;trial=$1; shift ;;
  #^[^-]) echo everything at once: $@; $@='';;
  *) echo "#hoping for: subj.date.run.trial; reading: $1";

     read subj date run trial <<READOC
     $(echo $1 | sed 's/[,\.]/ /g')
READOC

     break;;
 esac
done

# set input/output files
    subjdir="$B/$task/$subj/$date/"
   eyetrack="$subjdir/Raw/EyeData/txt/$subj.$date.$run.tsv"
  eyescored="$subjdir/Scored/txt/$subj.$date.$run.sac.tsv"
trailscored="$subjdir/Scored/txt/$subj.$date.$run.trial.txt"
manualscore="$(find $subjdir/Scored/*$run/ -iname f\*.xls)"
echo "#$t_ttl "
cat <<HEREDOC
source('$settingsfile')
source('../ScoreRun.R')
getSacs("$eyetrack",$subj,$run,"$t_ttl",onlyontrials=$trial,writetopdf=FALSE,savedas="$eyescored",showplot=T)
HEREDOC
echo "#$manualscore"
XLSperl -lane "print join(' ',@F) if \$F{C}==$trial; last if \$F{C} > $trial" $manualscore
echo "#$trailscored "
grep -P "^t|^$trial " "$trailscored"

