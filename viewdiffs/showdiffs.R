# expect defined: 
#settingsfile  # where are the xdat and pos settings (e.g. scannerbars.settings.R)
#taskdir       # where on B can we find subj/date/subj.date.run.data.tsv
#pertrialCSV   # where is the checkAgainstManual file: subj.date.run.trial acount mcount scorer

# should be in same dir as ScoreRun.R
# NB. NOT THE DIR THIS FILE IS IN

source(settingsfile)
source('ScoreRun.R')
source('getSacsDot.R')


diffs.file <- file.path(dirname(pertrialCSV), 'trialChanges.txt')
if(file.exists(diffs.file) && file.info(diffs.file)$size >0){
 diffs.data <- read.table(diffs.file,sep="\t",header=F)
 names(diffs.data) <- c('trial', 'before','current','scorer','status')
}
if(file.exists(pertrialCSV )){
  ### format the diff between scorer and algor.

  n.orig<-read.table(pertrialCSV,sep="\t",header=T)
  # remove those that start with a *
  n<-n.orig[!grepl('^\\*',n.orig$trial),]
  
  n$run <- substr(n$trial,1,17)
  n <- ddply(n, .(run ), function(x){
         l<-length(which(x$count_a==-1));
         if(l<expectedTrialLengths-1)  {x} 
      })
  n.all <- n
  #sampleDifferences <-function(auto,manual) {
  #  sapply(sample(as.character(n[n$count_a==auto&n$count_m==manual,'trial']),10), function(x){a<-getSacDot(x);readline();dev.off();scoreSac(a)})
  #}
  
  source('viewdiffs/showdiffs.func.R')
  # show a plot scoring distributions
  oldwd<-getwd()
  setwd(dirname(settingsfile))
  source('../compareToManual.R')
  setwd(oldwd)
} else{
 warning(sprintf('%s doesnt exist, showdiffs function not loaded',pertrialCSV))
}

cat(sprintf('

showdiffs(-1,1,10) # same as showdiffs(auto=-1,manual=1,size=10)
    to see 10 instances  where auto says drop (-1), manual says correct (1)

 showdiffs(size=10) 
    to see 10 instances where manual and auto are different

 showdiffs(auto=1,xdat=133)
    to see where algorithm says 1 on xdats of 133 (shows 10 by default)

getSacDot("%s")
    to see sac graph

scoreSac(getSacDot("%s"))
    to score and graph

see also diff.data
',as.character(n$trial[1]),as.character(n$trial[1]) ) )

