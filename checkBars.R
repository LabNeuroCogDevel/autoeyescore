library(plyr)
source('scannerbars/scannerbars.settings.R')
source('ScoreRun.R')

filebasedir <- '/mnt/B/bea_res/Data/Tasks/BarsScan/Basic/'
if(!file.exists(filebasedir)) { filebasedir <- '/Users/lncd/rcn/bea_res/Data/Tasks/BarsScan/Basic/'}
if(!file.exists(filebasedir)) { error('cannot find good location for B') }

getSacDot <- function(dotnotation) {
 parts <- unlist(strsplit(dotnotation, '\\.'))
 parts <- as.numeric(parts);
 names(parts) <- c('subj','date','run','trial')
 # filebasedir come from *settings.R file
 dirbase  <- sprintf("%s/%s/%s",filebasedir,parts['subj'],parts['date'])
 eyetrack <- sprintf("%s/Raw/EyeData/txt/%s.%s.%s.data.tsv",dirbase,parts['subj'],parts['date'],parts['run'])
 saveto   <- sprintf("%s/Scored/txt/%s.%s.%s.sac.tsv",dirbase,parts['subj'],parts['date'],parts['run'])

 getSacs(eyetrack,parts['subj'],parts['run'],"BarsScan",onlyontrials=parts['trial'],savedas=saveto,writetopdf=F,showplot=T)
}


n<-read.table('scannerbars/results/checkBars_trial.csv',sep="\t",header=T)
#sampleDifferences <-function(auto,manual) {
#  sapply(sample(as.character(n[n$count_a==auto&n$count_m==manual,'trial']),10), function(x){a<-getSacDot(x);readline();dev.off();scoreSac(a)})
#}
#

showdiffs <-function(auto,manual,size=10) {
  d <- n[sample(which(n$count_a==auto&n$count_m==manual),size),]
  ddply(d,.(trial), function(x){
     print(x);
     print(x$trail)
     a<-getSacDot(as.character(x$trial));
     readline();
     dev.off();
     scoreSac(a);
   })
}

cat('

showdiffs(-1,1,10) 
    to see 10 instances  where auto says drop (-1), manual says correct (1)

')

