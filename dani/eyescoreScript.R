# evaluate arguments
ept <- function(x) eval(parse(text=x),env=0)
args <- commandArgs(T)
for(arg in args) ept(arg)
# if required args are missing, stop
requiredArgs <- c("path","task","id","date","eyescoreFunctions","logFile")
for(arg in requiredArgs) if(!(arg %in% ls())) stop(arg," is missing")

# load eyescore functions
source(eyescoreFunctions)
# convenience function for concatenating data frames stored as elements in a list
list2data <- function(l) { d<-cbind(l[[1]],listInd=1); if(length(l)>1) for(i in 2:length(l)) d<-rbind(d, cbind(l[[i]],listInd=i)); d }

# load task data
taskData <- taskList[[task]](path="/Volumes/Phillips/COG")
files <- basename(Sys.glob(file.path(path, "*raw.txt")))
scoredList <- list()
for(file in files){
  filePrefix <- gsub("_raw.txt", "", file)

  ## NOTE: all the try statements check if there is an error, and if so, write it to the logfile, otherwise continue

  # convert from eyd to txt (done already)
  #eyeData <- try(
  #  eyd2txt(id, date, task, run, eydScript="~/src/autoeyescore/dataFromAnyEyd.pl", srcPath="~/rcn/bea_res/Data/Tasks", fileNaming=c("id","date","task","Run"), fileDelim="_", fileExt="raw.txt", txtPath=NULL, txtFile=NULL, overwrite=F),
  #  silent=T)
  #if(class(eyeData)!="data.frame") { cat(paste(task, id, date, file, "eyeData_raw", eyeData), file=logFile, append=T); next } 

  # preprocess data
  eyeData <- try(
    preprocessEyd(read.table(file.path(path, file), head=T), outputTable=file.path(path, paste(filePrefix, "preproc.txt", sep="_"))),
    silent=T)
  if(class(eyeData)!="data.frame") { cat(paste(task, id, date, file, "eyeData_preproc", eyeData), file=logFile, append=T); next } 

  # get and process saccades
  saccades <- try(
    scoreSaccades(task, eyeData, getSaccades(eyeData), outputTable=file.path(path, paste(filePrefix, "saccades.txt", sep="_"))),
    silent=T)
  if(class(saccades)=="data.frame") scoredList[[which(files %in% file)]] <- saccades else { cat(paste(task, id, date, file, "saccades", saccades), file=logFile, append=T); next }

  # get timing files
  timings <- try(writeTimings(filePrefix, task, eyeData, saccades), silent=T)
  if(!is.null(timings)) { cat(paste(task, id, date, file, "timings", timings), file=logFile, append=T); next } 
}

# run stats on all the scored runs
if(length(scoredList)>0) stats <- summaryData(task, list2data(scoredList), outputTable=file.path(path, paste(id, date, task, "stats.txt", sep="_")))

