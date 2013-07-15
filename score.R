rm(list=ls());
settingsfile <- grep('settings.R',list.files(),value=T)
cat(sprintf('using %s!\n',settingsfile ) )
source(settingsfile);
source('../ScoreRun.R');
source('../ScoreEveryone.R')

# get files to act on
splitfile <- getFiles()  # sourced from task specific settings file

#score everyone in those files
plotFigs <- T
perRunStats <- scoreEveryone(splitfile,plotFigs)

#failed to run:
missingRuns <- setdiff(paste(splitfile$subj,splitfile$date,splitfile$run,sep='.'),paste(perRunStats$subj,perRunStats$date,perRunStats$run,sep='.'))
cat('failed to run:\n')
print(missingRuns)
