
#######################
# convert from eyd to txt/tsv (will's perl script), then read into R
  # note, will's perl scripts and libraries need to be set up on computer
  # currently, only works on arnold, so defaults to this setup, but if setup on another computer can change eydScript/dataPath and it should work
  # default is consistent with will's naming, i like it different so manually specify most of the arguments myself anyways

eyd2txt <- function(id, date, task, run, eydScript="~/src/autoeyescore/dataFromAnyEyd.pl", srcPath="~/rcn/bea_res/Data/Tasks", fileNaming=c("id","date","run"), fileDelim=".", fileExt="data.tsv", txtPath=NULL, txtFile=NULL, overwrite=F){ #

  # location of eyd data
  eydPath <- file.path(srcPath, task, "Basic", id, date, "Raw")
  
  # if txtPath is specified, search for converted eyd file, otherwise search in eydPath
  filePattern <- paste("*", run, fileDelim, fileExt, sep="") # wildcard pattern used to find file
  if(!is.null(txtPath)) { txtPath<-file.path(txtPath, id, date, "behav", task); path<-txtPath } else path<-eydPath
  if(is.null(txtFile)) txtFile <- dir(path, pattern=filePattern, recursive=T)

  # if file already exists, can skip conversion, unless overwrite==T
  if(length(txtFile)!=1 | overwrite==T){

    # read eyd and check that only one file
    eydFile <- dir(eydPath, pattern=paste("*", run, ".eyd", sep=""), recursive=T)
    if(length(eydFile)==0) stop(paste("eyd file not found:", eydFile)) 
    if(length(eydFile)>1) stop(paste("too many eyd files found:", eydFile))   

    # if not specified, txtPath is in "txt" folder in eydPath
    if(is.null(txtPath)) txtPath <- file.path(dirname(eydFile), "txt")

    # create directory, read eyd, output as txt 
    dir.create(txtPath, showWarnings=F)
    if(is.null(txtFile)){
      filePrefix <- paste(sapply(fileNaming,
        # "run": "#"
        # "Run": "run#"
        function(i) if(i=="Run") paste("run", get(i), sep="") else get(i)
      ), collapse=fileDelim)
      txtFile <- paste(filePrefix, fileExt, sep=fileDelim)
    }
    # run command
    system(paste("./", eydScript, " ", eydFile, " > ", file.path(txtPath, txtFile), sep=""))
  }

  # read txt file, will be Nx4 table (XDAT, pupil_diam, horz_gaze_coord, vert_gaze_coord)
  return(read.table(txtFile, head=T))
}



#######################
# prepreprocess time series
  # cap extreme values
  # remove artifacts (spikes, blinks)
  # interpolate removed time points
  # smooth time series

preprocessEyd <- function(eyeData, outputTable=NULL, xmax=261, minSpikeVel=30, adjFilter=rep(1/5,5), adjThresh=0.4, smoothFilter=c(0.1,0.2,0.4,0.2,0.1), opts=list(capped=T, removeCapped=T, spikes=T, blink=T, blinkID="pupil", removeAdj=T, smooth=T)){

  suppressPackageStartupMessages( require(zoo) ) # for rollapply and na.approx time series function
                                                   # suppressing messages so less log clutter
  # quick utility function to fill in NAs at ends
  naFill <- function(x, avgPts=10){
    # NA stats
    r <- rle(is.na(x)); lr <- length(r$l); lx <- length(x)
    # first
    if(r$v[1]==T) x[1:r$l[1]] <- mean(x[r$l[1]+1:avgPts],na.rm=T)
    if(r$v[lr]==T) x[(lx-r$l[lr]+1):lx] <- mean(x[lx-r$l[lr]+1-1:avgPts],na.rm=T)
    return(x)
  }

  # x position
  xpos <- eyeData$horz

  # cap values >xmax
  if(opts$capped){ 
    cappedX <- xpos>xmax
    xpos[cappedX] <- xmax
    if(opts$removeCapped) capped <- cappedX|xpos==0 else capped <- logical(length(xpos))
  }

  # x velocity of change from sample to sample
  xvel <- c(0, diff(xpos))

  # record spikes
    # based on function from will which involves a quick direction reversal and a minimum velocity (default: 30 asl units) in both directions
    # need "zoo" library for rollapply
  if(opts$spikes) spikes <- c(F, rollapply(xvel, 2, function(x) { sign(prod(x))==-1 & abs(min(x))>minSpikeVel })) else spikes <- logical(length(xpos))

  # record blinks
  if(opts$blink){
    # option "pupil": record if pupil diameter=0; option "all": record if pupil diameter, horz and vert position are all 0 
    zeros <- switch(opts$blinkID,
      pupil = eyeData$pupil==0,
      all = eyeData$pupil==0 & eyeData$horz==0 & eyeData$vert==0
    )
  } else zeros <- logical(length(xpos))

  # if spikes or blinks, replace with NAs, then interpolate
    # need "zoo" library for na.approx
  replacePoints <- capped | spikes | zeros
  # interpolate points adjacent to removed samples, will uses 2, but then some extra code in case this may leave a stranded point
    # my solution - filter blink time series with moving average, and use thresholds between 0 and 1
    # default: >=0.4 with 5 point moving average filter (similar to will's 2 adjacent point threshold)
  if(opts$removeAdj) adjacent<-naFill(filter(replacePoints,adjFilter)>=adjThresh) else adjacent<-logical(length(xpos)) # after filter, need to replace NAs at end
  replacePoints <- replacePoints | adjacent
  if(length(which(replacePoints))>0) xpos[replacePoints] <- NA
  xpos <- naFill(na.approx(zoo(xpos), na.rm=F))

  # smooth and recalculate velocities
  if(opts$smooth) xpos <- naFill(filter(xpos, smoothFilter))
  xvel <- c(0, diff(xpos))
  # write table
  eyeData <- data.frame(eyeData, xpos, xvel, cappedX, capped, spikes, zeros, adjacent, replacePoints)
  if(!is.null(outputTable)) write.table(eyeData, file=outputTable, row.names=F, quote=F)

  # return preprocessed eye data
  return(eyeData)
}



#######################
# get saccades
  # find all points where velocity exceeds minVel and walk through in a while loop using an algorithm to get saccades
    # 1) walk backward from current index exceeding minVel until velocity is beneath slowVel - this is START of saccade
    # 2) walk forward from current minVel until velocity is beneath slowVel - last index before this is END of saccade
      # 2a) if criteria are met for a merge, the end of the next saccade is found and this is the new END of saccade
    # 3) set index to first index exceeding minVel after END and continue

getSaccades <- function(eyeData, outputTable=NULL, minVel=4, slowVel=1, minSacLength=4, minSacGap=3, replacedThr=0.7, opts=list(merge=T)){

  # if necessary variables from preprocessing (xvel, replacePoints) aren't present return error
    # will discards saccades if >70% are interpolated, which is why i use replacePoints here
  if("xvel" %in% names(eyeData) & "replacePoints" %in% names(eyeData)){
    xvel <- eyeData$xvel
    replacePoints <- eyeData$replacePoints
  } else stop("can't find xvel and replacePoints. have you preprocessed the eyd file?")

  # create empty data frame for saccade indices
    # start and end time of saccade, as well as how many indices were interpolated and how many merges there were
  saccades <- data.frame(start=numeric(), end=numeric(), replaced=numeric(), merged=numeric())

  # couple of convenience functions for loop
  # get start/stop indices
  getSacIndex <- function(range){
    for(i in range) if(abs(xvel[i])<slowVel | sign(xvel[range[1]])!=sign(xvel[i]) | is.na(xvel[i])) break
    return(i)
  }
  # update ind (point must exceed minVel and be after current ind)
  updateIndex <- function(i) return(which(abs(xvel)>minVel & 1:length(xvel)>i)[1])

  # current index in time series
  ind <- 0

  # start loop
  while(TRUE){
    # set index to first velocity exceeding minVel after current ind
    ind <- updateIndex(ind)
    # if no ind after end, break
    if(is.na(ind)) break # keep searching until there's no more saccades, then break
    # saccade starts once velocity drops below slowVel or switches direction
    start <- getSacIndex(ind:1)
    # saccade ends once velocity drops below slowVel or switches direction (-1 because using last index >slowVel)
    end <- getSacIndex(ind:length(xvel))-1
    # update ind to first after end
    ind <- updateIndex(end)
    # loop for merge: if next saccade is less than than minSacGap away, find end of next saccade and merge
    merged <- 0
    if(opts$merge) while(!is.na(ind)){
      # get next saccade start index
      startNext <- getSacIndex(ind:1)
      # if <minSacGap and the same direction, merge
      if(startNext-end<=minSacGap & sign(xvel[end])==sign(xvel[startNext])){
        end <- getSacIndex(ind:length(xvel))-1
        merged <- merged+1
      } else break
      # update ind
      ind <- updateIndex(end)
    }
    # saccade needs to be at least minSacLength and is discarded if >replacedThr proportion of its samples are interpolated
    replaced <- length(which(replacePoints[start:end]))
    if(end-start>=minSacLength-1 | replaced/(end-start+1)<=replacedThr) saccades <- rbind(saccades, data.frame(start=start, end=end, replaced=replaced, merged=merged))
  }

  # write table and return saccades
  if(!is.null(outputTable)) write.table(saccades, file=outputTable, quote=F)
  return(saccades)
}



########################
# scoring settings for all paradigms
  # currently have options for AntiState and MGSEncode
    # "xposTargetList", "expectedTrialCount", "trialTypes", "xdatList", "whichIndex", "targetDuration", "customTrialCode"
  # this step is generic so can add more

# quick convenience function to make a named list from a group of variables
makeList <- function(vars) { l<-list(); for(v in vars) l[[v]]<-get(v, envir=parent.frame()); return(l) }

# settings
settingsList <- list()

# AntiState settings
settingsList$AntiState <- function(){
  # possible position of targets
    # scale from xmax=640(px) to xmax=261(asl)
  xposTargetList <- c(7, 108, 214, 426, 532, 633)
  xmaxPx <- 640; xmaxAsl <- 261; xposTargetList <- round(xposTargetList*xmaxAsl/xmaxPx)
  # trial types
  trialTypes <- c("ps", "as")
  # expected number of trials
  expectedTrialCount <- rep(12, 2)
  # xdats to look for
  xdatList <- list(161:166, 171:176)
  # which index to return
    # "first": first index of xdat
    # "after": first index after last xdat
  whichIndex <- rep("first", 2)
  # target duration, period in which saccades are examined in asl units
  targetDuration <- rep(84, 2) # 1.4s, to be consistent with MGSEncode
  # custom code to be executed for specific trial types
    # for as, target position is reversed (have to look away)
  customTrialCode <- list(c(), c("xposTargetList <- rev(xposTargetList)"))
  # return settings in list form
  return(makeList(c("xposTargetList", "expectedTrialCount", "trialTypes", "xdatList", "whichIndex", "targetDuration", "customTrialCode")))
}

# MGSEncode settings
settingsList$MGSEncode <- function(){
  # possible position of targets
    # scale from xmax=640(px) to xmax=261(asl)
  xposTargetList <- c(7, 108, 214, 426, 532, 633)
  xmaxPx <- 640; xmaxAsl <- 261; xposTargetList <- round(xposTargetList*xmaxAsl/xmaxPx)
  # trial types
  trialTypes <- c("vgs", "mgs")
  # expected number of trials
  expectedTrialCount <- rep(20, 2)
  # xdats to look for
  xdatList <- list(c(20,30,40,50), rep(c(120,130,140,150),each=6)+rep(1:6,4))
  # which index to return
    # "first": first index of xdat
    # "after": first index after last xdat
  whichIndex <- c("first", "after")
  # target duration, period in which saccades are examined in asl units
  targetDuration <- rep(84, 2) # 1.4s target length, cue is 1.5 or 3s but will keep limited to target length
  # custom code to be executed for specific trial types
  customTrialCode <- list(
    # vgs (cue) xdat doesn't give position away, need to look to delay period which starts immediately after
    c("xposTarget <- xposTargetList[eyeData$XDAT[startInd+which(diff(eyeData$XDAT[startInd:dim(eyeData)[1]])!=0)[1]] %% 10]",
      "startInd <- startInd+84"), # for some reason, cue code starts 1.4s before cue), 
    c("startInd <- startInd-84") # for target code, using 1 after delay code ends, but turns out delay code also contains start code, which is 1.4s long
  )
  # return settings in list form
  return(makeList(c("xposTargetList", "expectedTrialCount", "trialTypes", "xdatList", "whichIndex", "targetDuration", "customTrialCode")))
}



########################
# scoreTrial - run within scoreSaccades

# quick convenience functions for converting asl units
asl2ms <- function(t) round(t*1000/60) # time, 60Hz
ms2asl <- function(t) round(t*60/1000) # time, 60Hz
asl2deg <- function(m) round(m/14.5,3) # magnitude, 14.5 is an approximation but so is the 3/6/9deg vs 106/106/101px

# score each trial, called within the scoreRun function below
scoreTrial <- function(startInd, minOnsetDelay=4, preTargetFix=10, blinkSample=6, sacMinMag=8, sacHeld=6, opts=list(blinkDrop=F, magCheck=F, heldCheck=T)){

  # run custom code before evaluating 
  custom <- customTrialCode[[type]]
  if(length(custom)>0) for(i in 1:length(custom)) eval(parse(text=custom[i]))

  # get xposTarget location
  if(!exists("xposTarget")){
    xposInd <- switch(whichIndex[type], first=startInd, after=startInd-1)
    xposTarget <- xposTargetList[eyeData$XDAT[xposInd] %% 10]
  }

  # an important issue encountered: what if there is no delay code following the cue code?
    # maybe this was a once off (10128, 2006xxxx, MGSEncode, run 3)
    # did the delay and target for the trial actually get deleted somehow??
    # if this happens, xposTarget will be empty, so just returing without scoring any saccades
  if(length(xposTarget)==0) return(scoring)

  # mean fixation value for preTargetFix samples before trial
  xposCenterFix <- mean(eyeData$xpos[startInd-preTargetFix:1])

  # rmList is a cleanup list so can use within function successfully
  rmList <- c("ind", "add", "locationExp", "locationAct", "sideExp", "sideAct", "directionExp", "directionAct")

  # begin scoring (will return revised saccades data frame)
  within(scoring, {

    # get location of first saccade (based on ending during trial period) - if none found, return
    ind <- which(end>startInd)
    if(length(ind)>0) ind <- ind[1] else { rm(list=rmList); return() }
    # trial start time
    trialStart[ind] <- startInd
    # scoring saccade now
    trialType[ind] <- trialTypes[type]
    # scoring saccade now
    trial[ind] <- t
    # saccade number in trial
    saccade[ind] <- 1
    add <- 0 # additional saccades
    # scoring saccade now
    scored[ind] <- T

    # set saccade expected/actual parameters for scoring
    locationExp <- xposTarget
    locationAct <- eyeData$xpos[end[ind]]
    sideExp <- sign(locationExp-xposCenter)
    sideAct <- sign(locationAct-xposCenter)
    directionExp <- sideExp
    directionAct <- sign(locationAct-eyeData$xpos[start[ind]])

    # 1) if no central fixation at start of trial, drop
    if(abs(xposCenterFix-xposCenter)>xposPadding){
      dropped[ind] <- T
      droppedReason[ind] <- "no_central_fix"
      rm(list=rmList); return()
    }

    # 2) if saccade is already in process during start of trial or start of saccade is <4 samples (67ms) from start of trial, discard as anticipatory 
    if(start[ind]-startInd<minOnsetDelay){
      dropped[ind] <- T
      droppedReason[ind] <- "anticipatory"
      rm(list=rmList); return()
    }

    # 3) if saccade doesn't end within trial window, discard 
    if(end[ind]-startInd>targetDuration[type]){
      dropped[ind] <- T
      droppedReason[ind] <- "missed"
      rm(list=rmList); return()
    }

    # 4) if there is a blink before/during the first saccade, note it (latency is unreliable), but don't drop
      # am not totally sure what will did but this seems the most reasonable to me
    if(length(which(eyeData$zeros[startInd:end[ind]]))>blinkSample){
      blinkStart[ind] <- T
      if(opts$blinkDrop){
        dropped[ind] <- T
        droppedReason[ind] <- "blink"
        rm(list=rmList); return()
      }
    }

    # 5) magnitude check
      # 10/20 units (px, not asl) depending on paradigm via will
      # based on new saccade criteria, 6 asl units = 6*(640px/261asl) = 14.7px is the minimum without this check
      # i don't think this is necessary, so included feature but defaults to not using (magCheck=F)
    if(opts$magCheck & abs(eyeData$xpos[end[ind]]-eyeData$xpos[start[ind]])<sacMinMag){
      dropped[ind] <- T
      droppedReason[ind] <- "low_magnitude"
      rm(list=rmList); return()
    }

    # 6) direction check - mark if incorrect (important for held check)
    if(directionExp!=directionAct) incorrect[ind] <- T

    # 7) held check
      # 100 ms / 6 samples, via will
      # this also appears unnecessary to me; there already has to be a minGap (default 67ms/4 samples) threshold otherwise the saccades get merged
      # it is possible to be <minGap if in opposite direction, so that should be accounted for
      # solution: only has to be held if next saccade is in opposite direction; will also make option in case this is still unnecessary
      # further, trial shouldn't be dropped if not held and incorrect (should just count as incorrect)
    if(opts$heldCheck & ind<length(start) & is.na(incorrect[ind]) & start[ind+1]-end[ind]<sacHeld){ # saccade held?
      dropped[ind] <- T
      droppedReason[ind] <- "not_held"
      rm(list=rmList); return()
    }

    # quick add: side+direction check
      # not sure if this will come up, but if direction and center fix are ok but the side is wrong, something is wrong so drop the trial
    if(sideExp!=sideAct & is.na(incorrect[ind])){
      dropped[ind] <- T
      droppedReason[ind] <- "impossible_side+direction_combo"
      rm(list=rmList); return()
    }

    # 8) if it's not already dropped or incorrect, it's correct!
    if(is.na(incorrect[ind])) correct[ind] <- T
    # within xposPadding range?
    if(abs(locationExp-locationAct)<xposPadding) correctPad[ind] <- T
    # get saccade stats
    accuracy[ind] <- asl2deg(locationAct-locationExp)
    latency[ind] <- asl2ms(start[ind]-startInd)

    # 9) score remaining saccades in trial, as long as there are saccades left in target period...
    while(ind+add<length(start) & end[ind+add+1]<startInd+targetDuration[type]){
      add <- add+1
      scored[ind+add] <- T
      saccade[ind+add] <- saccade[ind+add-1]+1
      trial[ind+add] <- t
      trialType[ind+add] <- trialTypes[type]
      # new saccade parameters
      directionExp <- sign(locationExp-locationAct) # based on location of previous saccade
      locationAct <- eyeData$xpos[end[ind+add]] # new saccade location
      sideAct <- sign(locationAct-xposCenter)
      directionAct <- sign(locationAct-eyeData$xpos[start[ind+add]])
      # magnitude/held checks, drop saccades but not whole trial at this point (only first saccade matters for that)
      if(opts$magCheck & abs(eyeData$xpos[end[ind+add]]-eyeData$xpos[start[ind+add]])<sacMinMag){
        dropped[ind+add] <- T
        droppedReason[ind+add] <- paste("low_magnitude")
      }
      if(opts$heldCheck & ind<length(start) & !is.na(incorrect[ind+add]) & start[ind+add+1]-end[ind+add]<sacHeld){ # saccade held?
        dropped[ind+add] <- T
        droppedReason[ind+add] <- paste("not_held")
      }

      # log accuracy and latency
      accuracy[ind+add] <- asl2deg(locationExp-locationAct)
      latency[ind+add] <- asl2ms(start[ind+add]-startInd)
      # if incorrect, was it corrected? ie; switch side 
      if(!is.na(incorrect[ind]) & sideExp==sideAct & directionExp==directionAct) corrected[ind] <- T
    }

    # clean up
    rm(list=rmList)
  })
}



########################
# scoreSaccades - score saccades in run

# score saccades
scoreSaccades <- function(task, eyeData, saccades, outputTable=NULL, xposCenter=261/2, fixCode=250, xposPadding=30, opts=list(fixCheck=T)){

  # get settings for all functions and attach
    # in case there was an error at a previous time that left a list attached, detach it (i do this each time i attach something to avoid warnings from previous times)
  while("settings" %in% search()) detach(settings); settings <- settingsList[[task]](); attach(settings)

  # add scoring columns to saccades
  scoring <- within(saccades, {trialStart<-NA; trialType<-NA; trial<-NA; saccade<-NA; scored<-NA; dropped<-NA; droppedReason<-NA; correct<-NA; correctPad<-NA; incorrect<-NA; corrected<-NA; blinkStart<-NA; accuracy<-NA; latency<-NA})

  ## check that overall fixation is accurate
  if(opts$fixCheck){
    xposCenterFix <- mean(eyeData$xpos[which(eyeData$XDAT %in% fixCode)], na.rm=T)
    if(xposCenterFix-xposCenter>xposPadding) stop(paste("mean of actual eye fixation across run (", xposCenterFix, ") differs from expected eye fixation (", xposCenter, ") by >", xposPadding, sep=""))
  }

  # run for each trial type
  for(type in 1:length(trialTypes)){
    # rle to get segments of T/F, then pull indices for T (start of xdat run)
    xdats <- rle(eyeData$XDAT %in% xdatList[[type]])
    startInd <- switch(whichIndex[type],
      first = c(1, 1+cumsum(xdats$l))[which(c(xdats$v, F))],
      after = 1+(cumsum(xdats$l))[which(xdats$v)]
    ) 
    # if incorrect number of trials, stop
    if(length(startInd) != expectedTrialCount[type]) stop(paste("number of ", trialTypes[type], " trials detected from xdats (", length(startInd), ") do not match expectedTrialCount (", expectedTrialCount[type], ")", sep=""))

    # loop through and process saccades for each trial
    for(t in 1:length(startInd)) { while("tempList" %in% search()) detach(tempList); tempList<-makeList(c("scoring","type","t","xposCenter","xposPadding")); attach(tempList); scoring <- scoreTrial(startInd[t]); detach(tempList) } # need to get those vars into nested function environment
  }
  detach(settings)

  # write table and return scored saccades
  if(!is.null(outputTable)) write.table(scoring, file=outputTable, quote=F)
  return(scoring)
}



#######################
# get individual summary data
#   can run on one or multiple runs

# convenience function for getting counts of different dropped reasons
strFreq <- function(strIn){ # input is string vector 
  strOut <- "" # output is single string with counts of unique inputs, for example: "anticipatory:3,not_held:1"
  ustr <- unique(strIn)
  for(u in ustr){
    str <- paste(u, length(which(strIn %in% u)), sep=":")
    if(nchar(strOut)==0) strOut <- str else strOut <- paste(strOut, str, sep=",")
  }
  return(strOut)
}

# convenience multi-intersect function; gets intersect of all indices in list
multiIntersect <- function(x){
  if(class(x)!="list" | length(x)<2) stop("incorrect input - either not a list or less than two sets of indices")
  ind <- x[[1]]; for(i in 2:length(x)) ind <- intersect(ind, x[[i]])
  return(ind)
}

# 
summaryData <- function(task, saccades, outputTable=NULL){

  while("settings" %in% search()) detach(settings); settings <- settingsList[[task]](); attach(settings)

  # sort through saccades, pull trial data
  summaryData <- with(saccades, {

    # run summary data
    summaryData <- data.frame(type=character(), count=numeric(), correct=numeric(), incorrect=numeric(), corrected=numeric(), dropped=numeric(), droppedReason=character(), percCorrect=numeric(), latency=numeric(), accuracy=numeric(), accuracyMost=numeric(), stringsAsFactors=F)

    # run for each trial type
    for(t in 1:length(trialTypes)){
      # 
      indT <- which(trialType==trialTypes[t])
      summaryData <- rbind(summaryData, data.frame(
        type = trialTypes[t],
        count = length(intersect(which(saccade==1),indT)),
        correct = length(intersect(which(correct),indT)),
        incorrect = length(intersect(which(incorrect),indT)),
        corrected = length(intersect(which(corrected),indT)),
        dropped = length(multiIntersect(list(which(saccade==1),which(dropped),indT))),
        droppedReason = strFreq(droppedReason[multiIntersect(list(which(saccade==1),which(dropped),indT))]),
        percCorrect = round(100 * length(intersect(which(correct),indT)) / (length(intersect(which(correct),indT))+length(intersect(which(incorrect),indT))), 1),
        latency = ifelse(length(intersect(which(correct),indT))>0, round(mean(latency[intersect(which(correct),indT)])), NA),
        accuracy = ifelse(length(intersect(which(correct),indT))>0, round(mean(abs(accuracy[intersect(which(correct),indT)])),2), NA),
        accuracyMost = ifelse(length(intersect(which(correct),indT))>0, round(mean(abs(sapply(intersect(which(correct),indT), function(i){
          j<-i; while(!is.na(scored[j+1])) j<-j+1; accuracy[i-1+which.min(abs(accuracy[i:j]))]}))), 2), NA),
        stringsAsFactors=F
      ))
      # fill in missing end trials
      diff <- expectedTrialCount[t]*max(listInd) - summaryData$count[t]
      if(diff>0) summaryData <- within(summaryData, { count[t]<-count[t]+diff; dropped[t]<-dropped[t]+diff; droppedReason[t]<-paste(droppedReason[t],paste("no_saccades_left",diff,sep=":"),sep=",") })
      if(summaryData$dropped[t]==0) summaryData$droppedReason[t] <- "NA" # need to have something in droppedReason column or else there will be wrong # of columns
    }

    # return summaryData
    summaryData
  })
  detach(settings)
  
  # write table and return scored saccades
  if(!is.null(outputTable)) write.table(summaryData, file=outputTable, row.names=F, quote=F)
  return(summaryData)
}



########################
# import task structure and timing into R

taskList <- list()

# MGSEncode - based on EPrime txt log output, with each run relabeled to 'mgsRun#.txt'
taskList$MGSEncode <- function(path="~/Dropbox/COG", prefix="mgsRun", output="task_MGSEncode.txt", useOld=T){

  # if already processed, no need to do it again, just read table
  if(file.exists(file.path(path, output)) & useOld) return(read.table(file.path(path, output), head=T))

  # basic task info
  runs <- 3
  trialCount <- 20
  durations <- data.frame(ShortCue=1.5, LongCue=3, ShortDelay=1.5, LongDelay=9, Target=1.5, Fix=1.5)
  cues <- array(c("ShortCue", "LongCue")) # arrays help with apply code below
  delays <- array(c("ShortDelay", "LongDelay"))
  targets <- c(7, 108, 214, 426, 532, 633)

  # data frame for stimulus data
  stimData <- data.frame(run=numeric(0), cueTime=numeric(0), cueLength=numeric(0), delayTime=numeric(0), delayLength=numeric(0), targetTime=numeric(0), targetPos=numeric(0))

  # loop through runs to pull task data
  for(run in 1:runs) {

    # read run txt file
    txt <- readLines(file.path(path, paste(prefix, run, ".txt", sep="")))

    # rle function to get trialCount * 2 (pairs of trial/fixation), returns values (used for trial) and lengths (used for fixation)
    allProcs <- rle(txt[grep("Proc", txt)])
    if(length(allProcs[[1]])!=trialCount*2) stop("wrong number of trials in file, or else file format is incorrect")
    # trial info, contains cue length, delay length and target position
    trialProcs <- allProcs$values[seq(1, trialCount*2, 2)]
    # fixation times
    fixTimes <- allProcs$lengths[seq(2, trialCount*2, 2)] * durations$Fix

    # track absolute times through loop
    absoluteTime <- 0

    # loop through trials
    for(t in 1:trialCount) {
      # each trial starts with 1.5s fixation
      tempTime <- absoluteTime + durations$Fix
      # cue either short (1.5s) or long (3s)
      cueTime <- tempTime
      cueLength <- durations[[cues[apply(cues, 1, grepl, trialProcs[t])]]]
      tempTime <- tempTime + cueLength
      # delay either short (1.5s) or long (9s)
      delayTime <- tempTime
      delayLength <- durations[[delays[apply(delays, 1, grepl, trialProcs[t])]]]
      tempTime <- tempTime + delayLength
      # delay either short (1.5s) or long (9s)
      targetTime <- tempTime
      targetPos <- which(targets %in% strsplit(trialProcs[t], "Delay")[[1]][2])
      # update absolute time and add trial data to data frame
      absoluteTime <- tempTime + durations$Target + fixTimes[t]

      # trial info, contains cue length, delay length and target position, and all stimulus times
      stimData <- rbind(stimData, data.frame(run, cueTime, cueLength, delayTime, delayLength, targetTime, targetPos))
    }
  }

  # write table and return stimData
  write.table(stimData, file=file.path(path, output), row.names=F, quote=F)
  return(stimData)
}

# AntiState - based on stimulus lists in excel file
taskList$AntiState <- function(path="~/Dropbox/COG", file="Anti_Mix_Design_Lists_FINAL_minusfix_fixblockrecord_FINMOD.xls", output="task_AntiState.txt", useOld=T){

  # if already processed, no need to do it again, just read table
  if(file.exists(file.path(path, output)) & useOld) return(read.table(file.path(path, output), head=T))

  require(gdata) # for reading xls files

  # basic task info - 24 run scripts, 1-12 AV/VA (anti block then vgs block or vice versa)
  scripts <- paste("List", rep(1:12,2), rep(c("AV","VA"), each=12), sep="")
  types <- c("ps", "as") # prosaccade, antisaccade
  trialCount <- 24 # 12 each as/ps
  targets <- c(7, 108, 214, 426, 532, 633)
  rng <- list(x=2:245, y=c(54:48)) # location of table in sheet to get data
  varNames <- c("FRAME", "Start Tm", "End Tm", "Event", "Location") # easier to specify than read from sheet, formatting issues

  # data frame for stimulus data
  stimData <- data.frame(script=numeric(0), time=numeric(0), type=character(0), targetPos=numeric(0))

  # loop through runs to pull task data
    # note, this will take some time, due to read.xls which is slow and used 24 times; maybe about a minute or two
  for(s in scripts) {

    print(s)

    # read script xls sheet, remove all columns before "FRAME" (skips first line and helps format columns appropriately)
    xls <- read.xls(file.path(path, file), sheet=s, pattern="FRAME")
    # trial indices (use target and subtract 2 to get start of cue; this was the easiest way since cue is duplicated for each trial)
    trialIndex <- grep("TARG", xls$Event)-2
    # stop if wrong number of trials (note: this is from the excel file, should be impossible!)
    if(length(trialIndex)!=trialCount) stop("wrong number of trials in file, or else file format is incorrect")

    # trial info, contains cue length, delay length and target position
    stimData <- rbind(stimData, data.frame(
      script = rep(s, trialCount),
      time = xls$Start.Tm[trialIndex],
      type = if(length(grep("AV",s))==0) rep(types, each=trialCount/2) else rep(rev(types), each=trialCount/2),
      targetPos = match(xls$Location[!is.na(xls$Location) & (xls$Location!=0)], targets) # need the !=0 because for some reason some NAs became 0 here
    ))
  }

  # write table and return stimData
  write.table(stimData, file=file.path(path, output), row.names=F, quote=F)
  return(stimData)
}



###################
# write timing files
  # write files separately for each run, can figure out combining in a way that is consistent with the fmri data
  # for each trial type: correct, incorrect/corrected, incorrect/not corrected, dropped, unscored saccades, blinks, maybe capped?
    # custom code for mgs to get delay periods (also mark delay period saccades so they don't end up in unscored regressor), also separate by cue and delay length
    # custom code for anti to subtract 1.5s from each time so start is prep period

## NOTE: should have a task-specific "timings settings" function, just like for the saccades

writeTimings <- function(filePrefix, task, eyeData, saccades, outcomes=c("correct","incorrect","corrected","dropped"), nullRegs=c("unscored","blinks","capped"), outputTable=NULL, opts=list()){

  # attach settings and saccades
  while("settings" %in% search()) detach(settings); settings <- settingsList[[task]](); attach(settings)
  while("saccades" %in% search()) detach(saccades); attach(saccades)

  # create timings folder
  outPath <- file.path(path, "timings", filePrefix)
  suppressWarnings( dir.create(outPath, recursive=T) ) # suppressing warning message if directory already exists

  # get run/script info for run, attach
  filePrefix <- strsplit(filePrefix, "_")[[1]]
  r <- gsub("run", "", filePrefix[4])
  if(task=="AntiState"){ s<-filePrefix[5]; if(nchar(s)==4 & substr(s,1,1)=="0") s<-gsub("0","",s) }
  runData <- taskData[switch(task, MGSEncode=grep(r,taskData$run), AntiState=grep(s,taskData$script)),]
  while("runData" %in% search()) detach(runData); attach(runData)

  # start of run
  runStartInd <- switch(task,
    MGSEncode = trialStart[which(saccade==1)[1]] - 90, # starts 1.5s (90 samples) before first cue
    AntiState = trialStart[which(saccade==1)[1]] - 90 - ms2asl(1000*(time[1])) # variable time to first trial; also, index is target, subtract 1.5s (90 samples) to get cue (will model as 4.5s block)
  )

  # run for each trial type and outcome
  for(type in 1:length(trialTypes)){
    stimTimes <- runData[[switch(task, MGSEncode=switch(type,"1"="cueTime","2"="delayTime"), AntiState="time")]] # stim times column name different for different tasks
    indType <- intersect(which(trialType==trialTypes[type]), which(saccade==1)) # 1st saccade for each trial of type
    for(outcome in outcomes){
      # get timings
      indTimings <- intersect(indType, switch(outcome,
        correct = which(!is.na(correct)),
        incorrect = intersect(which(!is.na(incorrect)), which(is.na(corrected))),
        corrected = which(!is.na(corrected)),
        dropped = which(!is.na(dropped))
      ))
      # convert to seconds
      if(length(indTimings)==0) timings <- "*" else timings <- asl2ms(trialStart[indTimings]-runStartInd)/1000
      # if dropped, make sure any missing trials (due to no remaining saccades) are counted as dropped
      if(outcome=="dropped"){
        scoredTrialCount <- max(trial, na.rm=T); diff <- expectedTrialCount[type]-scoredTrialCount
        if(diff>0){
          endDropped <- stimTimes[(scoredTrialCount+1):expectedTrialCount[type]]
          if(class(timings)!="character") timings <- c(timings, endDropped) else timings <- endDropped
        }
      }
      # write file
      cat(timings, "\n", file=file.path(outPath, paste(trialTypes[type], outcome, sep="_")))

      # custom code to get delay period and separate by cue and delay length (could be in separate function, but keeping here now)
      if(task=="MGSEncode"){
        cueLengths <- ifelse(cueLength==1.5, "short", "long")
        delayLengths <- ifelse(delayLength==1.5, "short", "long")
        if(class(timings)!="character"){
          indMatch <- sapply(timings, function(t) which.min(abs(stimTimes-t)))
          if(type==1){
            for(c in c("short","long")){
              ind <- which(cueLengths[indMatch]==c); if(length(ind)==0) timings1<-"*" else timings1<-timings[ind]
              cat(timings1, "\n", file=file.path(outPath, paste(trialTypes[type], outcome, "cue", c, sep="_")))
            }
          }else if(type==2){
            timingsDelay <- timings-delayTime[indMatch]
            cat(timingsDelay, "\n", file=file.path(outPath, paste("delay", outcome, sep="_"))) # write delay timings
            for(c in c("short","long")) for(d in c("short","long")){
              ind <- which(cueLengths[indMatch]==c & delayLengths[indMatch]==d)
              if(length(ind)==0){ timings2<-"*"; timings3<-"*" } else { timings2<-timings[ind]; timings3<-timingsDelay[ind] }
              cat(timings2, "\n", file=file.path(outPath, paste(trialTypes[type], outcome, "cue", c, "delay", d, sep="_")))
              cat(timings3, "\n", file=file.path(outPath, paste("delay", outcome, "cue", c, "delay", d, sep="_")))
            }
          }
        }else{
          if(type==1) for(c in c("short","long")) cat(timings, "\n", file=file.path(outPath, paste(trialTypes[type], outcome, "cue", c, sep="_")))
          if(type==2){ cat(timings, "\n", file=file.path(outPath, paste("delay", outcome, sep="_"))) # write delay timings
            for(c in c("short","long")) for(d in c("short","long")){
              cat(timings, "\n", file=file.path(outPath, paste(trialTypes[type], outcome, "cue", c, "delay", d, sep="_")))
              cat(timings, "\n", file=file.path(outPath, paste("delay", outcome, "cue", c, "delay", d, sep="_")))
            }
          }
        }
      }
    }
  }

  # if MGSEncode, mark saccades as scored during delay, so they don't count in unscored
  if(task=="MGSEncode") for(t in 1:expectedTrialCount[2]) {
    delaySacs <- intersect(which(saccade==1), which(trial %in% t))
    if(length(delaySacs)==2) if(diff(delaySacs)>1) scored[(delaySacs[1]+1):(delaySacs[2]-1)] <- T
  }

  # null regressors
  for(nullReg in nullRegs){
    # get timings
    indTimings <- switch(nullReg,
      unscored=start[which(is.na(scored))],
      blinks={ temp<-which(eyeData$zeros); temp[c(1,1+which(diff(temp)>1))] },
      capped={ temp<-which(eyeData$capped); temp[c(1,1+which(diff(temp)>1))] } # should make this compatible with blockDM but whatever
    )
    timings <- asl2ms(indTimings-runStartInd)/1000
    if(length(timings)>0 & min(timings)<0) timings <- timings[-which(timings<0)] # can be saccades, blinks etc... before run starts
    if(length(timings)==0) timings <- "*"
    # write file
    cat(timings, "\n", file=file.path(outPath, paste(nullReg, sep="_")))
  }

  detach(settings); detach(saccades); detach(runData)
  return(NULL)
}


## FUNCTION, GET RUN INPUTS, PUT TOGETHER FINAL TIMINGS






# 6) get eprime lists to check against, log discrepancies
#   - trial count, types, timing


# 7) get index for start of scanning, writing timing files
#


# ..
# this should be in a bash file that accepts arguments for other functions
# for(arg in commandArgs()) eval(parse(text=arg))
# need to have id, date, task, etc...

