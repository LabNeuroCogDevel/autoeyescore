# set up
source("eyescoreFunctions.R")
#pathTarget <- "/Volumes/Phillips/COG"
pathTarget <- "/Volumes/Phillips/COG/checkParams"
tasks <- c("MGSEncode", "AntiState")

list2data <- function(l) { d<-l[[1]]; if(length(l)>1) for(i in 2:length(l)) d<-rbind(d, l[[i]]); d }

# settings to try
minVels <- c(4,8)
slowVels <- c(1,4)

cat("analysis started:", date())

# loop over tasks, ids, dates and runs
summaryList <- list()
for(task in tasks){ cat("\n\n\n", task)
  ids <- dir(file.path(pathTarget, task))
  for(id in ids){ cat("\n\n", task, id)
    dates <- dir(file.path(pathTarget, task, id))
    for(date in dates){ cat("\n", task, id, date)
      path <- file.path(pathTarget, task, id, date)
      files <- basename(Sys.glob(file.path(path, "*raw.txt")))

      # different params
      for(m in minVels){ for(s in slowVels){
        params <- paste("m",m,"s",s,sep="")

        scoredList <- list()
        for(file in files){
          filePrefix <- gsub("_raw.txt", "", file)

          # convert from eyd to txt (done already)
          #eyeData <- eyd2txt(id, date, task, run, eydScript="~/src/autoeyescore/dataFromAnyEyd.pl", srcPath="~/rcn/bea_res/Data/Tasks", fileNaming=c("id","date","task","Run"), fileDelim="_", fileExt="raw.txt", txtPath=NULL, txtFile=NULL, overwrite=F)

          # preprocess data - no differences here
          eyeData <- try( # using try statement because run might fail, so skipping
            preprocessEyd(read.table(file.path(path, file), head=T), outputTable=file.path(path, paste(filePrefix, "preproc.txt", sep="_"))),
            silent=T)

          # as long as eyeData is preprocessed, get and process saccades - here are the parameter iterations
          if(class(eyeData)=="data.frame"){ 
            saccades <- try( # using try statement because run might fail, so skipping
              scoreSaccades(task, eyeData, getSaccades(eyeData, minVel=m, slowVel=s), outputTable=file.path(path, paste(filePrefix, params, "saccades.txt", sep="_"))),
              silent=T)
            if(class(saccades)=="data.frame") scoredList[[length(scoredList)+1]] <- data.frame(saccades)
          }
        }

        # run stats on all the scored runs
        if(length(scoredList)>0){
          stats <- summaryData(task, list2data(scoredList), outputTable=file.path(path, paste(id, date, task, m, s, "stats.txt", sep="_")))
          summaryList[[length(summaryList)+1]] <- data.frame(stats, id, date, task, params)
        }
      }}
}}}

summaryList <- list2data(summaryList)
save(summaryList, file="summaryList.Rdata")

cat("analysis completed:", date(), "\n")
