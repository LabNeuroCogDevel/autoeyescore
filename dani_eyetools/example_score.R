# debuging
library(debug)
options(error=recover) # prompt to browse after error

# GLOBALS
source('eyescoreFunctions.R')

task       <- "MGSEncode"
filePrefix <- "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_"
taskData   <- taskList$MGSEncode(path="/Volumes/Phillips/COG")
settings   <- settingsList$MGSEncode()

## scoring pipeline
# saccades
rawdata  <- read.table('/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_raw.txt',header=T)
preproc  <- preprocessEyd(rawdata,'/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_preproc.txt')
saccades <- getSaccades(preproc)
# match to task
runData  <- getRunData(rawdata,outputTable="/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_runData.txt")
scored   <- scoreRun(outputTable="/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_scored.txt")
scoredList <-list(run2=scored)
stats    <-  summaryData(list2data(scoredList), "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_stats.txt")


### PREPROC
# cap extreme values 
# remove artifacts (spikes, blinks) 
# interpolate removed time points 
# smooth time series

### GET SAC
# find all points where velocity exceeds minVel
# walk through in a while loop using an algorithm to get saccades
# 1) walk back from current index exceeding minVel until velocity < slowVel
#   this is START of saccade
# 2) walk forward from current minVel until velocity is beneath slowVel
#   last index before this is END of saccade
# 2a) if criteria are met for a merge, the end of the next saccade is found and
#   this is the new END of saccade
# 3) set index to first index exceeding minVel after END and continue

### runData
# add  "type"     "time"     "startInd" "xdatTime" to taskData 
