# evaluate arguments
ept <- function(x) eval(parse(text=x), env=0)
args <- commandArgs(T)
for (arg in args) ept(arg)
# if required args are missing, stop
requiredArgs <- c("path", "taskPath", "task", "id", "date", 
                  "eyescoreFunctions", "correctionMethod")
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
for (file in files){
  filePrefix <- gsub("_raw.txt", "", file)

  # convert from eyd to txt (done already), load txt
  rawTable <- file.path(path, file)
  raw <- try(
    read.table(rawTable, head=T),
    silent=T)
  if (class(raw) != "data.frame") { 
    cat(date(), "\n", filePrefix, "raw", "\n", raw, "\n\n")
    next
  } 

  # preprocess data
  preprocTable <- file.path(path, paste(filePrefix, "preproc.txt", sep="_"))
  preproc <- try(
    read.table(preprocTable, head=T),
    silent=T)
  if (class(preproc) != "data.frame") { 
    cat(date(), "\n", filePrefix, "preproc", "\n", preproc, "\n\n")
    next
  } 

  # get run data for scoring
  runDataTable <- file.path(path, paste(filePrefix, "runData.txt", sep="_"))
  runData <- try(
    getRunData(outputTable=runDataTable),
    silent=T)
  if (class(runData) != "data.frame") { 
    cat(date(), "\n", filePrefix, "runData", "\n", runData, "\n\n")
    next
  } 
  
  # score saccades
  scoredTable <- file.path(path, paste(filePrefix, "scored.txt", sep="_"))
  scored <- try(
    read.table(scoredTable, head=T),
    silent=T)
  if (class(saccades) != "data.frame") {
    cat(date(), "\n", filePrefix, "scored", "\n", scored, "\n\n")
    next
  }
  
  # model offset/drift
  pathOut <- file.path(taskPath, "offsetDriftCorrect", correctionMethod, 
                       task, id, date)
  correctionModel <- file.path(pathOut, paste(filePrefix, 
                                              "correctionModel.txt", sep="_"))
  newRawTable <- file.path(pathOut, paste(filePrefix, "raw.txt", sep="_"))
  newRaw <- try(
    offsetDriftCorrect(correctionMethod, settings$trialTypes[1], 
      outputModel=correctionModel, outputTable=newRawTable),
    silent=T)
  if (class(newRaw) != "data.frame") {
    cat(date(), "\n", filePrefix, "offsetDriftCorrect", "\n", 
        newRaw, "\n\n")
    next
  }
}