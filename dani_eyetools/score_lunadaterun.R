#
# scoreMGSE for a run and
# statMGSE  for all runs in a visit 
#

# GLOBALS
source('eyescoreFunctions.R')

# make a file name , .txt is only diff over paste0
mkfilen <- function(pf,sfx,ext=".txt") {
  sprintf('%s%s%s',pf,sfx,ext)
}

# get file names for each stage/step
filesMGS <- function(lunaid,date,runno) {
  fp <- sprintf("/Volumes/Phillips/COG/MGSEncode/%d/%d/%d_%d_MGSEncode_run%d_",lunaid,date,lunaid,date,runno)

  sapply(c("raw","preproc","runData","scored"),
         function(s){ mkfilen(fp,s)} )
  # files, eg.
  # "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_raw.txt"
  # "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_preproc.txt"
  # "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_runData.txt"
  # "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_scored.txt"
  # "/Volumes/Phillips/COG/MGSEncode/10125/20061021/10125_20061021_MGSEncode_run2_stats.txt")
}

scoreMGSE <- function(lunaid,date,runno) {
  # globals used by getRundata and scoredata
  task       <- "MGSEncode"
  filePrefix <- sprintf("/Volumes/Phillips/COG/MGSEncode/%d/%d/%d_%d_MGSEncode_run%d_",lunaid,date,lunaid,date,runno)
  taskData   <- taskList$MGSEncode(path="/Volumes/Phillips/COG")
  settings   <- settingsList$MGSEncode()
  
  # get where files should be
  f<-filesMGS(lunaid,date,runno)

  ## scoring pipeline
  # saccades
  rawdata  <- read.table( f[['raw']] ,header=T)
  preproc  <- preprocessEyd(rawdata,f[['preproc']])
  saccades <- getSaccades(preproc)

  # match to task
  runData  <- getRunData(rawdata, outputTable=f[['runData']] )

  scored   <- scoreRun(preproc,saccades,runData,outputTable=f[['scored']])
}

statMGSE <- function(lunaid,date) {
  # open file to report to
  conn <-file('fail.log',open="a") # append to fail log

  ## score all 3 runs
  tryCatch({
    # scoreMGSE for all 3 runs
    scoredList <- lapply(1:3,function(rn){ scoreMGSE(lunaid,date,rn) })

    # rename fields
    names(scoredList) <-lapply(names(scoredList), function(rn){sprintf('run%d',rn)})

    outfile  <- sprintf("/Volumes/Phillips/COG/MGSEncode/%d/%d/%d_%d_MGSEncode_stats.txt",lunaid,date,lunaid,date)
    # write results
    stats    <-  summaryData(list2data(scoredList), outfile )

    # print our save dir so we know it worked
    cat("saved to ",outfile,"\n")

  },error=function(e){

    # why did it fail?
    print(e)
    # probably an issue with the raw files, so lets make it easy to find those
    rawfiles  <- sprintf("/Volumes/Phillips/COG/MGSEncode/%d/%d/%d_%d_MGSEncode_run*_raw.txt\n",lunaid,date,lunaid,date)
    cat(sprintf("statMGSE(%d,%d) #FAILED %s",lunaid,date,rawfiles))

    # record what we'd need to do to run it again in the log file
    writeLines(sprintf("statMGSE(%d,%d) #%s FAILED %s",lunaid,date,date(),rawfiles), conn)
  })

  close(conn)
}
