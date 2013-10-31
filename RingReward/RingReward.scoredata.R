source('RingReward/RingReward.settings.R')
source('ScoreRun.R')
source('ScoreEveryone.R')
splitfile<-getFiles('RingReward/txt/*.data.csv')
perRunStats <- scoreEveryone(splitfile,F,reuse=F)
perRunStats
