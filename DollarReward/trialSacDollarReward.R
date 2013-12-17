source('DollarReward/DollarReward.settings.R') 
source('ScoreRun.R')
filebasedir<-'DollarReward/txt/'
trialSac <- function(subject,trial,showplot=T,funnybusiness='',showcmd=F) {
 subject <- as.character(subject)
 trial   <- as.character(trial)

 eyetrack <- sprintf("%s/%s.data.csv",filebasedir,subject)
 saveto   <- sprintf("%s//%s.0.0.sac.tsv",filebasedir,subject)

 # if we want subj.date.run.* # all trials
 if(grepl('\\*$',trial)){
  #trial=sprintf('1:%d',expectedTrialLengths)
  trial=1:expectedTrialLengths
  showplot=F
 }
 else { trial=as.numeric(trial) }

 getSacs(eyetrack,subject,0,"DollarReward",onlyontrials=trial,savedas=saveto,writetopdf=F,showplot=showplot,rundate=0,funnybusiness=funnybusiness)
}

