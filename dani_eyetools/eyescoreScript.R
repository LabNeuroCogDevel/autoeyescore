# DS, WF - DS wrapper around functions
# get all raw.txt files matching "id" and "date"
# use eyescoreFunctions.R in order:
#  - read or make task_MGSEncode.txt
#      taskData <- taskList$MGSEncode(path="/Volumes/Phillips/COG")
#      settings <- settingsList$MGSEncode()
#  - preprocessEyd
#  - getSaccades
#  - getRunData
#  - scoreRun
#  - summaryData

#
#Run like
# Rscript --vanilla --quiet eyescoreScript.R \
#   taskPath=\"/Volumes/Phillips/COG\" task=\"MGSEncode\" \
#   eyescoreFunctions=\"eyescoreFunctions.R\" \
#   path=\"/Volumes/Phillips/COG/MGSEncode/$id/$date/\" 
#   id=$id date=$date \
#     2>&1 | tee -a $logFile | tee $scorelog 
#
#

# evaluate arguments
ept <- function(x) eval(parse(text=x), env=0)
args <- commandArgs(T)
for (arg in args) ept(arg)
# if required args are missing, stop
requiredArgs <- c("path", "taskPath", "task", "id", "date", 
                  "eyescoreFunctions")
for (arg in requiredArgs) if(!(arg %in% ls())) stop(arg, " is missing")

# write all output to log (commenting out because doing logging in bash script)
# tried to make sure that only errors will be present
  # some suppression of messages from warnings, startup, packages
#sink(logFile, append=T)

# load eyescore functions
source(eyescoreFunctions)

# load task data and scoring settings
taskData <- taskList[[task]](path=taskPath)
settings <- settingsList[[task]]()

# get file names and loop through runs
files <- basename(Sys.glob(file.path(path, "*raw.txt")))
scoredList <- list()
for (file in files){
  filePrefix <- gsub("_raw.txt", "", file)

  # all the try statements check if there is an error
  # if so, write it to the logfile, otherwise continue

  # convert from eyd to txt (done already), load txt
  rawdata <- try(
    #eyd2txt(id, date, task, run, 
    #        eydScript="~/src/autoeyescore/dataFromAnyEyd.pl", 
    #        srcPath="~/rcn/bea_res/Data/Tasks", 
    #        fileNaming=c("id", "date", "task", "Run"), fileDelim="_", 
    #        fileExt="raw.txt", txtPath=NULL, txtFile=NULL, overwrite=F),
    read.table(file.path(path, file), head=T),
    silent=T)
  if (class(rawdata) != "data.frame") { 
    cat(date(), "\n", filePrefix, "raw", "\n", raw, "\n\n")
    next
  } 

  # preprocess data
  preproc <- try(
    preprocessEyd(rawdata,outputTable=file.path(path, paste(filePrefix, "preproc.txt", 
                                                    sep="_"))),
    silent=T)
  if (class(preproc) != "data.frame") { 
    cat(date(), "\n", filePrefix, "preproc", "\n", preproc, "\n\n")
    next
  } 

  # get saccades
  saccades <- try(
    getSaccades(preproc),
    silent=T)
  if (class(saccades) != "data.frame") { 
    cat(date(), "\n", filePrefix, "saccades", "\n", saccades, "\n\n")
    next
  } 

  # get run data for scoring
  runData <- try(
    getRunData(rawdata,saccades,outputTable=file.path(path, paste(filePrefix, "runData.txt", 
                                                 sep="_"))),
    silent=T)
  if (class(runData) != "data.frame") { 
    cat(date(), "\n", filePrefix, "runData", "\n", runData, "\n\n")
    next
  } 
  
  # score saccades
  scored <- try(
    scoreRun(outputTable=file.path(path, paste(filePrefix, "scored.txt", 
                                               sep="_"))),
    silent=T)
  if (class(saccades) != "data.frame") {
    cat(date(), "\n", filePrefix, "scored", "\n", scored, "\n\n")
    next
  } else scoredList[[which(files %in% file)]] <- scored

  # get timing files
  #timings <- try(
  #  writeTimings(filePrefix, task, preproc, scored),
  #  silent=T)
  #if (!is.null(timings)) { 
  #  cat(date(), "\n", filePrefix, "timings", "\n", timings, "\n\n")
  #  next
  #} 
}

# run stats on all the scored runs
if (length(scoredList) > 0) {
  stats <- try(
    summaryData(list2data(scoredList),
                outputTable=file.path(path, paste(id, date, task, "stats.txt",
                                                  sep="_"))),
    silent=T)
  if (class(stats) != "data.frame") cat(date(), "\n", paste(id, date, task, 
                                        sep="_"), "stats", "\n", stats, "\n\n")
}

# close log file (commented out, as per above)
#sink()
