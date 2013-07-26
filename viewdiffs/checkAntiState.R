library(plyr)

willname <- 'antistate'
bname    <- 'AntiState'

# path to settings file is relative, so lets try to be in the right place
if(any(grepl('viewdiff',getwd()))) { setwd('..'); cat('trying to move you to the correct directory\n') }
if(!any(grepl('score/?$',getwd()))) { stop('WRONG DIR: run me from the correct directory!') }

settingsfile<- sprintf('%s/%s.settings.R',willname,willname)
taskdir     <- sprintf('bea_res/Data/Tasks/%s/Basic/',bname)
pertrialCSV <- sprintf('%s/results/checkAgainstManual_trial.csv',willname)

source('viewdiffs/showdiffs.R')
