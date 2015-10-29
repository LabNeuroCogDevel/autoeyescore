# debuging
library(debug)
options(error=recover) # prompt to browse after error

# GLOBALS
source('eyescoreFunctions.R')
# 10125/20061021
# 11217/20141203

# make a file name 
mkfilen <- function(pf,sfx,ext=".txt") {
  sprintf('%s%s%s',pf,sfx,ext)
}

scoreMGSE <- function(lunaid,date,runno) {
  task       <- "MGSEncode"
  filePrefix <- sprintf("/Volumes/Phillips/COG/MGSEncode/%d/%d/%d_%d_MGSEncode_run%d_",lunaid,date,lunaid,date,runno)
  taskData   <- taskList$MGSEncode(path="/Volumes/Phillips/COG")
  settings   <- settingsList$MGSEncode()
  
  ## scoring pipeline
  # saccades
  rawdata  <- read.table( mkfilen(filePrefix,'raw') ,header=T)
  preproc  <- preprocessEyd(rawdata,mkfilen(filePrefix,'preproc'))
  saccades <- getSaccades(preproc)

  # match to task
  runData  <- getRunData(rawdata, outputTable=mkfilen(filePrefix,'runData') )

  scored   <- scoreRun(outputTable=mkfilen(filePrefix,'scored'))

  # files, eg.
  # "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_raw.txt"
  # "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_preproc.txt"
  # "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_runData.txt"
  # "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_scored.txt"
  # "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_stats.txt")
}

statMGSE <- function(lunaid,date) {
  # score all 3 runs
  scoredList <- lapply(1:3,function(rn){ scoreMGSE(lunaid,date,rn) })
  # rename fields
  names(scoredList) <-lapply(names(scoredList), function(rn){sprintf('run%d',rn)})

  outfile  <- sprintf("/Volumes/Phillips/COG/MGSEncode/%d/%d/%d_%d_MGSEncode_stats.txt",lunaid,date,lunaid,date)
  cat("saved to ",outfile,"\n")
  stats    <-  summaryData(list2data(scoredList), outfile )
}
