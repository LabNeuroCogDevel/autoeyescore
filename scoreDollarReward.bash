#!/usr/bin/env bash
set -ex
##
#
# score just one set of eyd files
#
# USAGE:
# 
#  ./scoreDollarReward.bash DollarReward/eyd/1940reward.eyd
#  ./scoreDollarReward.bash 1940
# 
# expects eyd file to be in DollarReward/eyd/
#
#end
##

function helpandexit { 
 sed -n 's/# //p;/#end/q;' $0 ; exit 1;
}

[ -z "$1" ] && helpandexit

eydfile=$1;
[ -r $eydfile ] || eydfile=DollarReward/eyd/${1}reward.eyd 
[ ! -r $eydfile ] && echo "can't find $1 eyd, tried $eydfile" && exit 1
 

## get to script directory (for later bash and R source)
scriptdir=$(cd $(dirname $0); pwd)
cd $scriptdir


echo "using $eydfile"
subj=$(basename $eydfile reward.eyd)
datadir=$scriptdir/DollarReward/txt
[ -d $datadir ] || mkdir $datadir


######
# convert eyd to data.csv
######

$scriptdir/dataFromAnyEyd.pl $eydfile > $datadir/$subj.data.csv

#
# score! and plot
#
Rscript --vanilla  --verbose <(echo "
 # load up all the source files
 
 source('DollarReward/DollarReward.R');
 source('ScoreRun.R');
 source('ScoreEveryone.R');
 
 # get the run files and score
 splitfile<-getFiles('$datadir/${subj}.data.csv');
 perRunStats <- scoreEveryone(splitfile,F,reuse=F);

 # plot
 sums     <- aggregate(. ~ subj, perRunStats[,c(1:8,match('subj',names(perRunStats))  )],sum)
 longsums <- melt(sums[,names(sums) != 'total'],id.vars='subj') 
 byrun    <- melt(perRunStats[,!grepl('lat|total|type',names(perRunStats))], id.vars=c('subj','date','run') )

 p.allbd<- ggplot(byrun) + geom_histogram(aes(x=variable,y=value,fill=variable),stat='identity') +
           facet_grid(.~run)+ggtitle('per run')+ theme(axis.text.x = element_text(angle = 90)) 
 p.subj <- ggplot( longsums ) + ggtitle('per subject breakdown of collected data') +
          geom_histogram(aes(x=subj,y=value,fill=variable),stat='identity')
 #p.drpd<- ggplot(perRunStats) + geom_histogram(aes(x=Dropped))


 # show each plot
  x11()
  cat('\npress anykey to plot');
  readLines(file('stdin'),1)
 for(p in list(p.allbd,p.subj)){
  print(p)
  cat('\npress anykey');
  readLines(file('stdin'),1)
 }
")
