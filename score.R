rm(list=ls());
settingsfile <- grep('settings.R',list.files(),value=T)
cat(sprintf('using %s!\n',settingsfile ) )
source(settingsfile);
source('../ScoreRun.R');
source('../ScoreEveryone.R')
