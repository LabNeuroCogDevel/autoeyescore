#!/usr/bin/env bash

##
#
# score just one set of eyd files
#
# USAGE:
# 
#  ./scoreOne.bash -N
#  ./scoreOne.bash -D /mnt/B/bea_res/Data/Tasks/BarsBehavioral/Basic/11198/20130711
#  ./scoreOne.bash -p BarsBehavioral -s 11198 -d 20130711
# 
#  options:
#   -N [act on newest]
#   -D DIR [path to visit
#  OR
#   -p PARADIGM
#   -s SUBJECT
#   -d DATE
# 
# expect to find eyd files in PARADIGM/SUBJECT/DATE/
#
#
#
#end
##


[ -z "$1" ] && sed -n 's/# //p;/#end/q;' $0 && exit 1;

while [ -n "$1" ]; do
 opt=$1; shift;
 case "$opt" in
  -s) subject=$1;  shift;;
  -d) date=$1;     shift;;
  -p) paradigm=$1; shift;;
  -N) AUTO=1;;
  -D) usedir=$1; AUTO=1; shift;;
   *) sed -n 's/# //p;/#end/q;' $0; exit 1;;
 esac
done

## get to script directory (for later bash and R source)
cd $(dirname $0)

## where is the data?
TASKDIR="/mnt/B/bea_res/Data/Tasks/"
[ ! -r $TASKDIR ] && TASKDIR="/Users/lncd/rcn/bea_res/Data/Tasks/"
[ ! -r $TASKDIR ] && echo "cannot find where bea_res/Data/Tasks/ is!" && exit 1

# define defaults to be the most recently changed subject directory
if [ -n "$usedir" -a -d "$usedir" ]; then
 visitdir=$usedir
else
 echo "you provided a bad dir?";
 unset AUTO
 subjdir=$(ls -td $TASKDIR/{VGS,Bars*,Anti}/Basic/*/ | head -n1)
 visitdir=$(ls -td $subjdir/*/ | head -n1)
fi

subjdir=$(dirname $visitdir) # redoing incase use dir changes things
task=$(basename $(dirname $(dirname $subjdir)))
subj=$(basename $subjdir)
vis=$(basename $visitdir)
# make an array of these guys
declare -A default
default=([paradigm]="$task" [subject]="$subj" [date]="$vis")

echo "Most recently changed: ${default[@]}"

# if we don't want to do any work
# at least make sure the dir exits
if [ -n "$AUTO" ]; then
 dir="$TASKDIR/${default[paradigm]}/Basic/${default[subject]}/${default[date]}"
 [ ! -d $dir ] && echo "$dir DNE" &&  unset dir
fi

## itertively create path with user input
while [ -z "$dir" -o ! -d "$dir" ]; do
  dir="$TASKDIR/"
  for param in paradigm subject date; do

    # for each thing that's not defined, define it with user input
    if [ -z "${!param}" ];then
       default=${default[$param]}

       ext=""
       [ "$param" == "paradigm" ] && ext="/Basic"

       echo -n "$param [empty means $default]: "
       read readin

       if [ -n "$readin" ]; then
         printf -v "$param" "$readin$ext"
       else
         printf -v "$param" "$default$ext"
       fi
    fi

    # does that choice make sense?
    if [ ! -r "$dir/${!param}" ];then
      echo "$dir/${!param} Does Not Exist!"
      unset $param
      unset dir
      #echo "unset dir($dir) and $param(${!param})"
      echo "try again!"
      break
    fi

    # expand where we are looking
    dir="$dir/${!param}"
    default[$param]="${!param}"
    # first tasks/bars/
    # then tasks/bars/99999/
    # then tasks/bats/99999/20101231/
  done
done

echo "using $dir"
# convert from bea_res name to will's naming convention (stupid Will)
declare -A beaTowill=([VGS]='vgs' [BarsScan]='scannerbars' [BarsBehavioral]='bars' [Anti]='anti')
paradigm=${default[paradigm]}
paradigm=${paradigm%%/*}
willtask=${beaTowill[$paradigm]}

#
# convert to tsv
#
source $willtask/nameTSV.bash
for f in $dir/Raw/EyeData/*eyd; do
 outdir="$(dirname "$f")/txt"
 tsv="$outdir/$(nameTSV $f)"

 [ -r $tsv ] && continue
 [ ! -d $outdir ] && mkdir -p $outdir
 ../../eyds/dataFromAnyEyd.pl $f > $tsv
 # remove if cannot understand it's format
 [ $(wc -l $tsv|cut -f1 -d' ') -lt 10 ] && echo "Removed $tsv! $f looks bad" && rm $tsv
done


#
# score! and plot
#
Rscript --vanilla  --verbose <(echo "
 # load up all the source files
 source('$willtask/$willtask.settings.R');
 source('ScoreRun.R');
 source('ScoreEveryone.R');
 
 # get the run files and score
 print('looking in $dir/Raw/EyeData/txt/${default[subject]}.${default[date]}.*.data.tsv');
 splitfile<-getFiles('$dir/Raw/EyeData/txt/${default[subject]}.${default[date]}.*.data.tsv');
 perRunStats <- scoreEveryone(splitfile,F);

 # plot
 sums     <- aggregate(. ~ subj, perRunStats[,c(1:8,match('subj',names(perRunStats))  )],sum)
 longsums <- melt(sums[,names(sums) != 'total'],id.vars='subj') 
 p.all  <- ggplot(perRunStats, aes(x=as.factor(total),fill=type))+geom_histogram(position='dodge') +ggtitle('total seen')
 p.allbd<- ggplot( perRunStats ) + geom_histogram(aes(x=run,y=value,fill=variable,stat='identity')) +ggtitle('per run')
 p.subj <- ggplot( longsums ) + ggtitle('per subject breakdown of collected data') +
          geom_histogram(aes(x=subj,y=value,fill=variable,stat='identity'))
 #p.drpd<- ggplot(perRunStats) + geom_histogram(aes(x=Dropped))


 # show each plot
 x11()
 cat('\npress anykey to plot');
 readLines(file('stdin'),1)
 for(p in list(p.subj, p.all,p.subj, p.allbd)){
  print(p)
  cat('\npress anykey');
  readLines(file('stdin'),1)
 }
")
