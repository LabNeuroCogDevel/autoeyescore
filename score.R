rm(list=ls());

# get what settings file we are using
# assume we are starting in the task's directory
settingsfile <- grep('settings.R',list.files(),value=T)
cat(sprintf('using %s!\n',settingsfile ) )

# source settings for run, and functions
source(settingsfile);
source('../ScoreRun.R');

# setup clusetesr
# export the whole environment
#library(foreach)
#library(snow)
#library(doSNOW)
#cl <- makeCluster(2, type = "SOCK")
#clusterExport(cl, ls())
#registerDoSNOW(cl)

# source wrapper functions
source('../ScoreEveryone.R')
# make sure we have a directory to save results in
dir.create('results')

# get files to act on
splitfile <- getFiles()  # sourced from task specific settings file

#score everyone in those files
plotFigs <- T
perRunStats <- scoreEveryone(splitfile,plotFigs)

#failed to run:
missingRuns <- setdiff(paste(splitfile$subj,splitfile$date,splitfile$run,sep='.'),paste(perRunStats$subj,perRunStats$date,perRunStats$run,sep='.'))
cat('failed to run:\n')
print(missingRuns)
