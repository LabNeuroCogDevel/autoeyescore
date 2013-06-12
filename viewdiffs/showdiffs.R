# expect defined: 
#settingsfile  # where are the xdat and pos settings (e.g. scannerbars.settings.R)
#taskdir       # where on B can we find subj/date/subj.date.run.data.tsv
#pertrialCSV   # where is the checkAgainstManual file: subj.date.run.trial acount mcount scorer

# should be in same dir as ScoreRun.R
# NB. NOT THE DIR THIS FILE IS IN

source(settingsfile)
source('ScoreRun.R')

# arnold (users/lncd/rnc/ vs reese (mnt/b)
Bdir <- '/mnt/B/'
if(!file.exists(Bdir)) { Bdir <- '/Users/lncd/rcn/' }
if(!file.exists(Bdir)) { stop('cannot find good location for B') }

filebasedir <- sprintf('%s/%s',Bdir,taskdir)
if(!file.exists(filebasedir)) { stop(sprintf('cannot find task directory: %s',filebasedir)) }

getSacDot <- function(dotnotation) {
 parts <- unlist(strsplit(dotnotation, '\\.'))
 parts <- as.numeric(parts);
 names(parts) <- c('subj','date','run','trial')
 # filebasedir come from *settings.R file
 dirbase  <- sprintf("%s/%s/%s",filebasedir,parts['subj'],parts['date'])
 eyetrack <- sprintf("%s/Raw/EyeData/txt/%s.%s.%s.data.tsv",dirbase,parts['subj'],parts['date'],parts['run'])
 saveto   <- sprintf("%s/Scored/txt/%s.%s.%s.sac.tsv",dirbase,parts['subj'],parts['date'],parts['run'])

 getSacs(eyetrack,parts['subj'],parts['run'],"BarsScan",onlyontrials=parts['trial'],savedas=saveto,writetopdf=F,showplot=T,rundate=parts['date'])
}


n.orig<-read.table(pertrialCSV,sep="\t",header=T)
# remove those that start with a *
n<-n.orig[!grepl('^\\*',n.orig$trial),]
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

oldwd<-getwd()
setwd(dirname(settingsfile))
source('../compareToManual.R')
setwd(oldwd)

cat(sprintf('

showdiffs(-1,1,10) 
    to see 10 instances  where auto says drop (-1), manual says correct (1)

getSacDot("%s")
    to see sac graph

scoreSac(getSacDot("%s"))
    to score and graph
',as.character(n$trial[1]),as.character(n$trial[1]) ) )

