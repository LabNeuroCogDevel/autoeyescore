# =============================================================================
# convert from eyd to txt/tsv (will's perl script), then read into R
# =============================================================================
# note, will's perl scripts and libraries need to be set up on computer
# currently, only works on arnold, so defaults to this setup, but if setup on
#   another computer can change eydScript/dataPath and it should work
# default is consistent with will's naming, i like it different so manually
#   specify most of the arguments myself anyways
library(dplyr) # for rbind_all

eyd2txt <- function(id, date, task, run,
  eydScript = "~/src/autoeyescore/dataFromAnyEyd.pl",
  srcPath = "~/rcn/bea_res/Data/Tasks",
  fileNaming = c("id", "date", "run"), fileDelim = ".", fileExt = "data.tsv",
  txtPath = NULL, txtFile = NULL, overwrite = F) {

  # location of eyd data
  eydPath <- file.path(srcPath, task, "Basic", id, date, "Raw")
  
  # if txtPath is specified, search for converted eyd file
  #   otherwise search in eydPath
  filePattern <- paste("*", run, fileDelim, fileExt, sep="") # wildcard pattern
  if (!is.null(txtPath)) {
    txtPath <- file.path(txtPath, id, date, "behav", task)
    path <- txtPath
  } else path <- eydPath
  if (is.null(txtFile)) txtFile <- dir(path, pattern=filePattern, recursive=T)

  # if file already exists, can skip conversion, unless overwrite==T
  if (length(txtFile) != 1 | overwrite == T) {

    # read eyd and check that only one file
    eydFile <- dir(eydPath, pattern=paste("*", run, ".eyd", sep=""),
                   recursive=T)
    if (length(eydFile) == 0) stop(paste("eyd file not found:", eydFile)) 
    if (length(eydFile) >= 2) stop(paste("too many eyd files found:", eydFile))   

    # if not specified, txtPath is in "txt" folder in eydPath
    if (is.null(txtPath)) txtPath <- file.path(dirname(eydFile), "txt")

    # create directory, read eyd, output as txt 
    dir.create(txtPath, showWarnings=F)
    if (is.null(txtFile)) {
      filePrefix <- paste(sapply(fileNaming,
        # "run": "#", "Run": "run#"
        function(i) if (i == "Run") paste("run", get(i), sep="") else get(i)
      ), collapse=fileDelim)
      txtFile <- paste(filePrefix, fileExt, sep=fileDelim)
    }
    # run command
    system(paste("./", eydScript, " ", eydFile, " > ",
      file.path(txtPath, txtFile), sep=""))
  }

  # read txt file, will be Nx4 table
    # XDAT, pupil_diam, horz_gaze_coord, vert_gaze_coord
  return(read.table(txtFile, head=T))
}



# =============================================================================
# prepreprocess time series
# =============================================================================
# cap extreme values
# remove artifacts (spikes, blinks)
# interpolate removed time points
# smooth time series

preprocessEyd <- function(rawdata, outputTable=NULL, xmax=261, minSpikeVel=30,
  adjFilter=rep(1/5,5), adjThresh=0.4, smoothFilter=c(0.1,0.2,0.4,0.2,0.1),
  opts=list(capped=T, removeCapped=T, spikes=T, blink=T, blinkID="pupil", 
            removeAdj=T, smooth=T)) {

  # if rawdata data not present, stop
  #check <- checkVars("raw"); if (!is.null(check)) stop(check)
  if(is.null(rawdata) || length(rawdata)==0L) stop("no or empty raw data!")

  # for rollapply and na.approx time series function
  # suppressing messages so less log clutter
  suppressPackageStartupMessages( require(zoo) )

  # quick utility function to fill in NAs at ends
  naFill <- function(x, avgPts=10) {
    r <- rle(is.na(x)); lr <- length(r$l); lx <- length(x)
    if (r$v[1] == T) x[1:r$l[1]] <- mean(x[r$l[1]+1:avgPts], na.rm=T)
    if (r$v[lr] == T) x[(lx-r$l[lr]+1):lx] <- mean(x[lx-r$l[lr]+1-1:avgPts],
                                                   na.rm=T)
    x
  }

  # x position
  xpos <- rawdata$horz

  # cap values >xmax
  if (opts$capped) { 
    cappedX <- xpos > xmax
    xpos[cappedX] <- xmax
    if (opts$removeCapped) {
      capped <- cappedX | xpos <= 0
    } else capped <- logical(length(xpos))
  }

  # x velocity of change from sample to sample
  xvel <- c(0, diff(xpos))

  # record spikes
    # based on function from will which involves a quick direction reversal
    #   and a minimum velocity (default: 30 asl units) in both directions
    # need "zoo" library for rollapply
  if (opts$spikes) {
    spikes <- c(F, rollapply(xvel, 2, function(x) {
      sign(prod(x)) == -1 & abs(min(x)) > minSpikeVel }))
  } else {
    spikes <- logical(length(xpos))
  }

  # record blinks
  if (opts$blink) {
    # option "pupil": blink if pupil diameter=0
    # option "all":   blink if pupil diameter, horz and vert position are all 0 
    zeros <- switch(opts$blinkID,
      pupil = rawdata$pupil == 0,
      all = rawdata$pupil == 0 & rawdata$horz == 0 & rawdata$vert == 0
    )
  } else {
    zeros <- logical(length(xpos))
  }

  # if capped, spikes, or blinks, replace with NAs, then interpolate
  replacePoints <- capped | spikes | zeros
  # interpolate points adjacent to removed samples
    # will uses 2, but then some extra code in case of stranded points
    # my solution - filter blink time series with moving average
      # use thresholds between 0 and 1
      # default: >=0.4 with 5 point moving average filter
      # similar to will's 2 adjacent point threshold
  if (opts$removeAdj) {
    # naFill to replace NAs at end after filter
    adjacent <- naFill(filter(replacePoints, adjFilter) >= adjThresh)
  } else {
    adjacent <- logical(length(xpos))
  }
  replacePoints <- replacePoints | adjacent
  # interpolate
  if (length(which(replacePoints)) > 0) xpos[replacePoints] <- NA
  # need "zoo" library for na.approx, naFill to replace NAs at end
  xpos <- naFill(na.approx(zoo(xpos), na.rm=F))

  # smooth and recalculate velocities
  if (opts$smooth) xpos <- naFill(filter(xpos, smoothFilter))
  xvel <- c(0, diff(xpos))

  # write table
  preproc <- data.frame(rawdata, xpos, xvel, cappedX, capped, spikes, zeros,
                        adjacent, replacePoints)
  if (!is.null(outputTable)) write.table(preproc, file=outputTable,
                                         row.names=F, quote=F)

  # return preprocessed eye data
  preproc
}



# =============================================================================
# get saccades
# =============================================================================
# find all points where velocity exceeds minVel
# walk through in a while loop using an algorithm to get saccades
# 1) walk back from current index exceeding minVel until velocity < slowVel
#   this is START of saccade
# 2) walk forward from current minVel until velocity is beneath slowVel
#   last index before this is END of saccade
# 2a) if criteria are met for a merge, the end of the next saccade is found and
#   this is the new END of saccade
# 3) set index to first index exceeding minVel after END and continue

getSaccades <- function(preproc,outputTable=NULL, minVel=4, slowVel=1, minSacLength=4,
  minSacGap=3, replacedThr=0.7, opts=list(merge=T)) {

  # if preprocessed data not present, stop
  #check <- checkVars("preproc"); if (!is.null(check)) stop(check)
  checkEmptyVar(preproc,"missing scoredSacs")

  # pulling out variables needed from preproc
  xvel <- preproc$xvel; replacePoints <- preproc$replacePoints

  # create empty data frame for saccade indices
    # start and end time of saccade
    # how many indices were interpolated
    # how many merges there were
  saccades <- data.frame(start=numeric(), end=numeric(), replaced=numeric(),
                         merged=numeric())

  # couple of convenience functions for loop
  # get start/stop indices
  getSacIndex <- function(range) {
    for (i in range) {
      if (abs(xvel[i]) < slowVel | 
            sign(xvel[range[1]]) != sign(xvel[i]) |
            is.na(xvel[i])) break
    }
    i
  }
  # update ind (point must exceed minVel and be after current ind)
  updateIndex <- function(i) which(abs(xvel) > minVel & 1:length(xvel) > i)[1]

  # current index in time series
  ind <- 0

  # start loop
  while (TRUE) {
    # set index to first velocity exceeding minVel after current ind
    ind <- updateIndex(ind)
    # keep searching until no more saccades (if no ind after end, break)
    if (is.na(ind)) break
    # saccade starts once velocity drops below slowVel or switches direction
    start <- getSacIndex(ind:1)
    # saccade ends once velocity drops below slowVel or switches direction
      # -1 because using last index >slowVel
    end <- getSacIndex(ind:length(xvel)) - 1
    # update ind to first after end
    ind <- updateIndex(end)
    # loop for merge
      # if next saccade is less than than minSacGap away
      # find end of next saccade and merge
    merged <- 0
    if (opts$merge) while (!is.na(ind)) {
      # get next saccade start index
      startNext <- getSacIndex(ind:1)
      # if <minSacGap and the same direction, merge
      if (startNext - end <= minSacGap &
            sign(xvel[end]) == sign(xvel[startNext])) {
        end <- getSacIndex(ind:length(xvel)) - 1
        merged <- merged + 1
      } else break
      # update ind
      ind <- updateIndex(end)
    }
    # saccade needs to be at least minSacLength and
    #   discarded if >replacedThr proportion of its samples are interpolated
    replaced <- length(which(replacePoints[start:end]))
    if (end - start >= minSacLength - 1 | 
          replaced / (end - start + 1) <= replacedThr) {
      saccades <- rbind(saccades, data.frame(start, end, replaced, merged))
    }
  }

  # write table and return saccades
  if (!is.null(outputTable)) write.table(saccades, file=outputTable, quote=F)
  saccades
}



# =============================================================================
# scoring settings for all paradigms
# =============================================================================
# variables
#   xposTargetList
#   expectedTrialCount
#   trialTypes
#   xdatList
#   whichIndex
#   targetDuration
#   customTrialCode
# currently have options for AntiState and MGSEncode
# this step is generic so can add more

# settings
settingsList <- list()

# AntiState settings
settingsList$AntiState <- function() {
  # possible position of targets
  # scale from xmax=640(px) to xmax=261(asl)
  xposTargetList <- c(7, 108, 214, 426, 532, 633)
  xmaxPx <- 640
  xmaxAsl <- 261
  xposTargetList <- round(xposTargetList * xmaxAsl / xmaxPx)
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
  # fixation code
  fixCode <- 250
  # custom code to be executed for specific trial types
    # for as, target position is reversed (have to look away)
  customTrialCode <- list(c(), 
    c("settings$xposTargetList <- rev(settings$xposTargetList)"))
  # return settings in list form
  makeList(c("xposTargetList", "expectedTrialCount", "trialTypes", "xdatList", 
             "whichIndex", "targetDuration", "fixCode", "customTrialCode"))
}

# MGSEncode settings
settingsList$MGSEncode <- function(){
  # possible position of targets
  # scale from xmax=640(px) to xmax=261(asl)
  xposTargetList <- c(7, 108, 214, 426, 532, 633)
  xmaxPx <- 640
  xmaxAsl <- 261
  xposTargetList <- round(xposTargetList * xmaxAsl / xmaxPx)
  # trial types
  trialTypes <- c("vgs", "mgs")
  # expected number of trials
  expectedTrialCount <- rep(20, 2)
  # xdats to look for
  xdatList <- list(2:5 * 10, rep(12:15 * 10, each=6) + rep(1:6, 4))
  # which index to return
    # "first": first index of xdat
    # "after": first index after last xdat
  whichIndex <- c("first", "after")
  # target duration, period in which saccades are examined in asl units
  # 1.4s target length, cue is 1.5 or 3s but will keep limited to target length
  targetDuration <- rep(84, 2)
  # fixation codes
  # should just be 250, but actual fix blocks have three codes
  fixCode <- c(60, 160, 250)
  # custom code to be executed for specific trial types
  customTrialCode <- list(
    # vgs (cue) xdat doesn't give position away, need to look to delay period
    #   which starts immediately after
    c(paste("xposTarget <- settings$xposTargetList[preproc$XDAT[startInd +", 
      "which(diff(preproc$XDAT[startInd:dim(preproc)[1]]) != 0)[1]] %% 10]")),
    c()
  )
  # return settings in list form
  makeList(c("xposTargetList", "expectedTrialCount", "trialTypes", "xdatList",
             "whichIndex", "targetDuration", "fixCode", "customTrialCode"))
}



# =============================================================================
# scoreTrial - run inside scoreRun
# =============================================================================
# defined in main environment (when eyescoreFunctions.R is sourced)
# environment reassigned in scoreRun to give access to its variables

scoreTrial <- function(startInd, minOnsetDelay=4, preTargetFix=9, blinkSample=6,
  sacMinMag=8, sacHeld=6, opts=list(blinkDrop=F, magCheck=F, heldCheck=T)) {

  # run custom code before evaluating 
  custom <- settings$customTrialCode[[type]]
  if (length(custom) > 0) {
    for (i in 1:length(custom)) eval(parse(text=custom[i]))
  }
    
  # get xposTarget location
  if (!exists("xposTarget")){
    xposInd <- switch(settings$whichIndex[type],
                      first = startInd, after = startInd - 1)
    xposTarget <- settings$xposTargetList[preproc$XDAT[xposInd] %% 10]
  }

  # an important issue encountered
    # what if there is no delay code following the cue code?
    # maybe this was a once off (10128, 2006xxxx, MGSEncode, run 3)
    # did the delay and target for the trial actually get deleted somehow??
    # if this happens, xposTarget will be empty, so just returing without
    #   scoring any saccades
  if (length(xposTarget) == 0) return(scoring)

  # mean fixation value for preTargetFix samples before trial
  #if(startInd<0 ) browser()
  xposCenterFix <- mean(preproc$xpos[startInd - preTargetFix:1])

  # rmList is a cleanup list so can use within function successfully
  rmList <- c("ind", "add", "locationExp", "locationAct", "sideExp", "sideAct", 
              "directionExp", "directionAct")

  # begin scoring (will return revised saccades data frame)
  within(scoring, {

    # get location of first saccade (based on ending during trial period)
      # if none found, return
    ind <- which(end > startInd)
    if (length(ind) > 0) ind <- ind[1] else { rm(list=rmList); return() }
    # trial start time
    trialStart[ind] <- startInd
    # scoring saccade now
    trialType[ind] <- settings$trialTypes[type]
    # scoring saccade now
    trial[ind] <- t
    # saccade number in trial
    saccade[ind] <- 1
    add <- 0 # additional saccades
    # scoring saccade now
    scored[ind] <- T

    # set saccade expected/actual parameters for scoring
    locationExp <- xposTarget
    locationAct <- preproc$xpos[end[ind]]
    sideExp <- sign(locationExp - xposCenter)
    sideAct <- sign(locationAct - xposCenter)
    directionExp <- sideExp
    directionAct <- sign(locationAct - preproc$xpos[start[ind]])

    # 1) if no central fixation at start of trial, drop
    if (abs(xposCenterFix - xposCenter) > xposPadding) {
      dropped[ind] <- T
      droppedReason[ind] <- "no_central_fix"
      rm(list=rmList); return()
    }

    # 2) if saccade is already in process during start of trial or start of
    # saccade is <4 samples (67ms) from start of trial, discard as anticipatory 
    if (start[ind] - startInd < minOnsetDelay) {
      dropped[ind] <- T
      droppedReason[ind] <- "anticipatory"
      rm(list=rmList); return()
    }

    # 3) if saccade doesn't end within trial window, discard 
    if (end[ind] - startInd > settings$targetDuration[type]) {
      dropped[ind] <- T
      droppedReason[ind] <- "missed"
      rm(list=rmList); return()
    }

    # 4) if there is a blink before/during the first saccade, note it
      # latency is unreliable), but don't drop
      # not exactly sure what will did but this seems the most reasonable to me
    #if(startInd<0 ) browser()

    if (length(which(preproc$zeros[startInd:end[ind]])) > blinkSample) {
      blinkStart[ind] <- T
      if(opts$blinkDrop){
        dropped[ind] <- T
        droppedReason[ind] <- "blink"
        rm(list=rmList); return()
      }
    }

    # 5) magnitude check
      # from will, 10/20 units (px, not asl) depending on paradigm
      # based on new saccade criteria
      # min without check: 6 asl units = 6*(640px/261asl) = 14.7px
      # don't think this is necessary, included feature but defaults to off
        # magCheck=F
    if (opts$magCheck & 
          abs(preproc$xpos[end[ind]] - preproc$xpos[start[ind]]) < sacMinMag) {
      dropped[ind] <- T
      droppedReason[ind] <- "low_magnitude"
      rm(list=rmList); return()
    }

    # 6) direction check - mark if incorrect (important for held check)
    if (directionExp != directionAct) incorrect[ind] <- T

    # 7) held check
      # 100 ms / 6 samples, via will
      # this also appears unnecessary to me
        # there already has to be a minGap (default 67ms/4 samples) threshold
        #   otherwise the saccades get merged
        # it is possible to be <minGap if in opposite direction
        #   so that should be accounted for
      # solution: only has to be held if next saccade is in opposite direction
        # will also make option in case this is still unnecessary
      # further, trial shouldn't be dropped if not held and incorrect
        # should just count as incorrect
    if (opts$heldCheck & 
          ind < length(start) & 
          is.na(incorrect[ind]) & 
          start[ind+1] - end[ind] < sacHeld) { # saccade held?
      dropped[ind] <- T
      droppedReason[ind] <- "not_held"
      rm(list=rmList); return()
    }

    # quick add: side+direction check
      # not sure if this will come up
      # if direction and center fix are ok but the side is wrong,
      #   something is wrong so drop the trial
    if (sideExp != sideAct & is.na(incorrect[ind])) {
      dropped[ind] <- T
      droppedReason[ind] <- "impossible_side+direction_combo"
      rm(list=rmList); return()
    }

    # 8) if it's not already dropped or incorrect, it's correct!
    if (is.na(incorrect[ind])) correct[ind] <- T
    # within xposPadding range?
    if (abs(locationExp - locationAct) < xposPadding) correctPad[ind] <- T
    # get saccade stats
    accuracy[ind] <- asl2deg(locationAct - locationExp)
    latency[ind] <- asl2ms(start[ind] - startInd)

    # 9) score remaining saccades in trial
      # as long as there are saccades left in target period...
    while (ind + add < length(start) &
             end[ind+add+1] < startInd + settings$targetDuration[type]) {
      add <- add + 1
      scored[ind+add] <- T
      saccade[ind+add] <- saccade[ind+add-1] + 1
      trial[ind+add] <- t
      trialType[ind+add] <- settings$trialTypes[type]
      # new saccade parameters
      # based on location of previous saccade
      directionExp <- sign(locationExp - locationAct)
      # new saccade location
      locationAct <- preproc$xpos[end[ind+add]]
      sideAct <- sign(locationAct - xposCenter)
      directionAct <- sign(locationAct - preproc$xpos[start[ind+add]])
      # magnitude/held checks, drop saccades but not whole trial at this point
        # only first saccade matters for that
      if (opts$magCheck & 
            abs(preproc$xpos[end[ind+add]] - 
                  preproc$xpos[start[ind+add]]) < sacMinMag) {
        dropped[ind+add] <- T
        droppedReason[ind+add] <- paste("low_magnitude")
      }
      if (opts$heldCheck & 
            ind < length(start) & 
            !is.na(incorrect[ind+add]) & 
            start[ind+add+1] - end[ind+add] < sacHeld) { # saccade held?
        dropped[ind+add] <- T
        droppedReason[ind+add] <- paste("not_held")
      }

      # log accuracy and latency
      accuracy[ind+add] <- asl2deg(locationExp - locationAct)
      latency[ind+add] <- asl2ms(start[ind+add] - startInd)
      # if incorrect, was it corrected? ie; switch side 
      if (!is.na(incorrect[ind]) & 
            sideExp == sideAct & 
            directionExp == directionAct) corrected[ind] <- T
    }

    # clean up
    rm(list=rmList)
  })
}



# =============================================================================
# scoreRun - score saccades in run
# =============================================================================

scoreRun <- function(outputTable=NULL, xposCenter=261/2, xposPadding=30,
  opts=list(fixCheck=T)) {

  # if preprocessed data not present, stop
  check <- checkVars(c("preproc", "saccades", "runData", "settings"))
  if (!is.null(check)) stop(check)

  # need to switch scoreTrial environment so can see variables in this function
  environment(scoreTrial) <- environment()

  # add scoring columns to saccades
  scoring <- within(saccades, { trialStart<-NA; trialType<-NA; trial<-NA;
    saccade<-NA; scored<-NA; dropped<-NA; droppedReason<-NA; correct<-NA;
    correctPad<-NA; incorrect<-NA; corrected<-NA; blinkStart<-NA; accuracy<-NA;
    latency<-NA })

  ## check that overall fixation is accurate
  if (opts$fixCheck) {
    xposCenterFixAll <- mean(preproc$xpos[
      which(preproc$XDAT %in% settings$fixCode)], na.rm=T)
    if (xposCenterFixAll - xposCenter > xposPadding) stop(
      paste("mean of actual eye fixation across run (", xposCenterFixAll,
            ") differs from expected eye fixation (", xposCenter, ") by >",
            xposPadding, sep=""))
  }

  # run for each trial type and trial
  for (type in 1:length(settings$trialTypes)) {
    for (t in 1:length(which(runData$type == settings$trialTypes[type]))) {
      startind <- runData$startInd[runData$type == settings$trialTypes[type]][t]
      #if(startind < 0) browser()
      if(startind <0 ) {warning(sprintf("SKIPPING: startind/time does not makes sense (%d) in type %d trial %d",startind,type,t)); next}
      scoring <- scoreTrial(runData$startInd[runData$type == 
                                               settings$trialTypes[type]][t])
    }
  }

  # write table and return scored saccades
  if (!is.null(outputTable)) write.table(scoring, file=outputTable, quote=F)
  return(scoring)
}



# =============================================================================
# getRunData - gets data for stimuli in run
# =============================================================================

getRunData <- function(rawdata,outputTable=NULL, opts=list()) {

  # GLOBALS
  # if necessary variables are not present, stop
  check <- checkVars(c("settings", "task", "filePrefix", "taskData"))
  if (!is.null(check)) stop(check)

  # locals
  checkEmptyVar(rawdata,"getRunData: missing rawdata")
  #checkEmptyVar(saccades,"getRunData: missing saccades")

  # short variable name to save space, since we use it a few times
  # using 1st index, assuming same for rest
  N <- settings$expectedTrialCount[1]

  # get run/script info for run, select run data from task data frame
  filePrefix <- strsplit(filePrefix, "_")[[1]]
  r <- gsub("run", "", filePrefix[4])
  if (task == "AntiState") {
    s <- filePrefix[5]
    if (nchar(s) == 4 & substr(s, 1, 1) == "0") s <- gsub("0", "", s)
  }
  # format runData so has trial number, trial type, time (s)
    # will add index to this after merging below
  runData <- switch(task,
    MGSEncode = taskData[grep(r, taskData$run), ],
    AntiState = taskData[grep(s, taskData$script), ]
  )
  # duplicating so we have for both trial types (already duplicated in anti)
  if (task == "MGSEncode") {
    runData <- rbind(runData, runData)
    runData$type <- rep(settings$trialTypes, each=N)
    runData$time <- c(runData[["cueTime"]][1:N], runData[["targetTime"]][1:N])
  } else if (task == "AntiState") {
    runData$time <- runData[["targetTime"]]
  }

  ###
  # get start index and start times from xdat codes, add to runData
  parseXdatsByType <- function(type) {

    # rle to get segments of T/F, then pull indices for T (start of xdat run)
    xdats <- rle(rawdata$XDAT %in% settings$xdatList[[type]])
    startInd <- switch(settings$whichIndex[type],
      first = c(1, 1 + cumsum(xdats$l))[which(c(xdats$v, F))],
      after = 1 + (cumsum(xdats$l))[which(xdats$v)]
    )

    # codes in MGSEncode occur at wrong times, need to fix
    if (task == "MGSEncode") {
      startInd <- switch(type, "1" = startInd + 84, "2" = startInd - 84)
    }
    
    # if incorrect number of trials, stop
      # note: no longer need to error out, will match to expected trials
    #if(length(startInd) != settings$expectedTrialCount[type]) stop(
    #  paste("number of ", settings$trialTypes[type],
    #  " trials detected from xdats (", length(startInd),
    #  ") do not match expectedTrialCount (", settings$expectedTrialCount[type], 
    #  ")", sep=""))

    # index of start of run to get times from xdats
    if (type == 1) {
      # assigning runStartInd to global environment so can use for other fns
      assign("runStartInd", switch(task,
        # starts 1.5s (90 samples) before first cue
        MGSEncode = startInd[1] - 90,
        # variable time to first trial (will model as 4.5s block)
        AntiState = startInd[1] - ms2asl(1000 * runData$time[1])
      ), env=globalenv())
    }
    
    # if we started recording before the task, set time=0 to first xdat sent
    # (subtract away non task eye tracking)
    # N.B we are only looking at cue xdats, trial starts 1.5s before them
    xdatTime <- asl2ms(startInd - runStartInd) / 1000

    # match up xdat index/times to taskData times
    # may be off due to xdat code errors (not sure of the reasons for this)
    # if too many, remove inappropriate indices
    # if too few, put in NAs for missing trials
    #N.xdat <- length(xdatTime)
    #diff <- N - N.xdat
    excludeInd <- findbadxdatidx(xdatTime,runData,settings$trialTypes[type])
    # ignore negative in scoreRun
    #browser()
    #excludeInd <- unique(c(excludeInd,which(xdatTime<0)))

    if(length(excludeInd)>0L){
      xdatTime <- xdatTime[-excludeInd]
      startInd <- startInd[-excludeInd]
    }

    ### now find missing
    # redo matching trials (based on what we've potentially excluded)
    trialMatch <- trialMatches(xdatTime,runData,settings$trialTypes[type] )

    # interpet missing
    matchingTrials <- which(1:N %in% trialMatch$rel) 
    missingTrials <- setdiff(1:N,matchingTrials) #which(!(1:N %in% trialMatch))
    if(length(missingTrials)>0L){
       interpolateTime <- predict(lm(xdatTime ~ matchingTrials),
                                  data.frame(matchingTrials = missingTrials))
       #
       tempTime <- numeric(N)
       tempTime[matchingTrials] <- xdatTime
       tempTime[missingTrials] <- interpolateTime
       xdatTime <- round(tempTime, 3)
       #
       tempInd <- numeric(N)
       tempInd[matchingTrials] <- startInd
       tempInd[missingTrials] <- ms2asl(1000 * interpolateTime)
       startInd <- tempInd
    }

    data.frame(startInd, xdatTime) #,listInd=type)
  }
  
  # do for each type (1=vgs,2=mgs)
  sistxc <- lapply(1:length(settings$trialTypes),parseXdatsByType )

  # merge into datafram
  #runData <- data.frame(runData, list2data(sistxc, opts=list(listInd=F)))
  runData <- data.frame(runData, rbind_all(sistxc))


  # write table and return run data
  if (!is.null(outputTable)) write.table(runData, file=outputTable, 
                                         row.names=F, quote=F)
  runData
}



# =============================================================================
# offset/drift correction - 
# =============================================================================
# two options:
# 1) correct from fixation
  # use trial start times (from runData)
  # get median xpos during fixation for each trial
# 2) correct from vgs accuracy
  # get accuracy (not absolute value) for correct vgs trials
# in both cases
  # fit linear model (accuracy vs. time)
  # remove high influence points
  # predict for all indices
  # return correction

offsetDriftCorrect <- function(correctionMethod, trialType, fixDuration=90,
  fixMode="median", xmid=261/2, outputModel=NULL, outputTable=NULL, 
  opts=list()) {

  # if necessary variables are not present, stop
  check <- checkVars(c("runData"))
  if (!is.null(check)) stop(check)

  # get indices of trial starts
  startInd <- with(runData, startInd[which(type == trialType)])
  n <- length(startInd)
  # start index at start of run so intercept is meaningful 
  runInd <- startInd - runStartInd
  
  # from correct trial accuracy
  if (correctionMethod == "scored") {
    if (!is.null(checkVars("scored"))) stop("scored") # var check
    ind <- intersect(which(scored$trialType == trialType),
                     which(scored$correct == T))
    runInd <- runInd[scored$trial[ind]]
    xerr <- deg2asl(scored$accuracy[ind])

  # from fixation
  } else if (correctionMethod == "fixation") {
    if (!is.null(checkVars("preproc"))) stop("preproc") # var check
    xerr <- sapply(startInd, function(i) {
      xpos <- preproc$xpos[(i-fixDuration):(i-1)]
      switch(fixMode,
        mean = mean(xpos, na.rm=T),
        median = median(xpos, na.rm=T)
      )
    }) - xmid

  # if correction method not one of the above
  } else stop(paste("correction methods value of", correctionMethod,
                    "is invalid, must be either scored or fixation"))

  # correction model, refit model without high influence points
  correctionModel <- lm(xerr ~ runInd, subset = 
                          which(cooks.distance(lm(xerr ~ runInd)) <= 4/n))

  # if filename provided, print model summary to file
  if (!is.null(outputModel)) {
    sink(outputModel); print(summary(correctionModel)); sink()
  }
  
  # apply correction to raw data
  newRaw <- rawdata
  newRaw$correction <- round(predict(correctionModel, 
                          data.frame(runInd = 1:dim(rawdata)[1] - runStartInd)), 1)
  newRaw$horz_gaze_coord <- rawdata$horz - newRaw$correction
  newRaw$orig_horz_gaze_coord <- rawdata$horz
  
  # write table and return
  if (!is.null(outputTable)) write.table(newRaw, file=outputTable, 
                                         row.names=F, quote=F)  
  newRaw
}



# =============================================================================
# get individual summary data
# =============================================================================
# can run on one or multiple runs

summaryData <- function(scoredSacs, outputTable=NULL) {

  # if preprocessed data not present, stop
  check <- checkVars(c("settings"))
  if (!is.null(check)) stop(check)
  #checkEmptyVar(scoredSacs,"missing scoredSacs")


  # sort through saccades, pull trial data
  summaryData <- list2data(with(scoredSacs, {

    lapply(1:length(settings$trialTypes), function(t) {
      indT <- which(trialType == settings$trialTypes[t])
      summaryData <- data.frame(
        type = settings$trialTypes[t],
        count = length(intersect(which(saccade == 1), indT)),
        correct = length(intersect(which(correct), indT)),
        incorrect = length(intersect(which(incorrect), indT)),
        corrected = length(intersect(which(corrected), indT)),
        dropped = length(multiIntersect(
          list(which(saccade == 1), which(dropped), indT))),
        droppedReason = strFreq(droppedReason[multiIntersect(
          list(which(saccade == 1), which(dropped), indT))]),
        percCorrect = round(100 * length(intersect(which(correct), indT)) /
          (length(intersect(which(correct), indT)) + 
          length(intersect(which(incorrect), indT))), 1),
        latency = ifelse(length(intersect(which(correct), indT)) > 0, 
          round(mean(latency[intersect(which(correct), indT)])), NA),
        accuracy = ifelse(length(intersect(which(correct), indT)) > 0, 
          round(mean(abs(accuracy[intersect(which(correct), indT)])), 2), NA),
        accuracyMost = ifelse(length(intersect(which(correct), indT)) > 0,
          round(mean(abs(sapply(intersect(which(correct), indT), function(i) {
            j <- i; while (!is.na(scored[j + 1])) j <- j + 1
            accuracy[i - 1 + which.min(abs(accuracy[i:j]))]
          }))), 2), NA),
        stringsAsFactors=F
      )

      # fill in missing end trials
      diff <- ( settings$expectedTrialCount[t] * max(listInd) - 
        summaryData$count )
      if (diff > 0) summaryData <- within(summaryData, {
        count <- count + diff
        dropped <- dropped + diff
        droppedReason <- paste(droppedReason, 
          paste("no_saccades_left", diff, sep=":"), sep=",")
      })
      # need to have something in droppedReason column or else there will be
      #   wrong # of columns
      if (summaryData$dropped == 0) summaryData$droppedReason <- "NA"
      summaryData
    })
  }))
  
  # write table and return scored saccades
  if (!is.null(outputTable)) write.table(summaryData, file=outputTable, 
                                         row.names=F, quote=F)
  summaryData
}



# =============================================================================
# import task structure and timing into R
# =============================================================================

taskList <- list()

# MGSEncode
  # based on EPrime txt log output, with each run relabeled to 'mgsRun#.txt'
taskList$MGSEncode <- function(path="~/Dropbox/COG", prefix="mgsRun",
                               output="task_MGSEncode.txt", useOld=T) {

  # if already processed, no need to do it again, just read table
  if (file.exists(file.path(path, output)) & 
        useOld) return(read.table(file.path(path, output), head=T))

  # basic task info
  runs <- 3
  trialCount <- 20
  total <- runs * trialCount
  durations <- data.frame(ShortCue=1.5, LongCue=3, ShortDelay=1.5, LongDelay=9, 
                          Target=1.5, Fix=1.5)
  cues <- array(c("ShortCue", "LongCue")) # arrays help with apply code below
  delays <- array(c("ShortDelay", "LongDelay"))
  targets <- c(7, 108, 214, 426, 532, 633)

  # data frame for stimulus data
  stimData <- data.frame(run=numeric(total), cueTime=numeric(total), 
    cueLength=numeric(total), delayTime=numeric(total),
    delayLength=numeric(total), targetTime=numeric(total), 
    targetPos=numeric(total))

  # loop through runs to pull task data
  for (run in 1:runs) {

    # read run txt file
    txt <- readLines(file.path(path, paste(prefix, run, ".txt", sep="")))

    # rle function to get trialCount * 2 (pairs of trial/fixation)
    # returns values (used for trial) and lengths (used for fixation)
    allProcs <- rle(txt[grep("Proc", txt)])
    if (length(allProcs[[1]]) != trialCount*2) {
      stop("wrong number of trials in file, or else file format is incorrect")
    }
    # trial info, contains cue length, delay length and target position
    trialProcs <- allProcs$values[seq(1, trialCount*2, 2)]
    # fixation times
    fixTimes <- allProcs$lengths[seq(2, trialCount*2, 2)] * durations$Fix

    # track absolute times through loop
    absoluteTime <- 0

    # loop through trials
    for (t in 1:trialCount) {
      # each trial starts with 1.5s fixation
      tempTime <- absoluteTime + durations$Fix
      # cue either short (1.5s) or long (3s)
      cueTime <- tempTime
      cueLength <- durations[[cues[apply(cues, 1, grepl, trialProcs[t])]]]
      tempTime <- tempTime + cueLength
      # delay either short (1.5s) or long (9s)
      delayTime <- tempTime
      delayLength <- durations[[delays[
        apply(delays, 1, grepl, trialProcs[t])]]]
      tempTime <- tempTime + delayLength
      # delay either short (1.5s) or long (9s)
      targetTime <- tempTime
      targetPos <- which(targets %in% strsplit(trialProcs[t], "Delay")[[1]][2])
      # update absolute time and add trial data to data frame
      absoluteTime <- tempTime + durations$Target + fixTimes[t]

      # trial info, contains
        # cue length, delay length and target position, and all stimulus times
      stimData[t + (run-1) * trialCount, ] <- data.frame(
        run, cueTime, cueLength, delayTime, delayLength, targetTime, targetPos)
    }
  }

  # write table and return stimData
  write.table(stimData, file=file.path(path, output), row.names=F, quote=F)
  stimData
}

# AntiState
  # based on stimulus lists in excel file
taskList$AntiState <- function(path="~/Dropbox/COG",
  file="Anti_Mix_Design_Lists_FINAL_minusfix_fixblockrecord_FINMOD.xls", 
  output="task_AntiState.txt", useOld=T) {

  # if already processed, no need to do it again, just read table
  if (file.exists(file.path(path, output)) & 
        useOld) return(read.table(file.path(path, output), head=T))

  require(gdata) # for reading xls files

  # basic task info
    # 24 run scripts, 1-12 AV/VA (anti block then vgs block or vice versa)
  scripts <- paste("List", rep(1:12, 2), rep(c("AV","VA"), each=12), sep="")
  types <- c("ps", "as") # prosaccade, antisaccade
  trialCount <- 24 # 12 each as/ps
  total <- length(scripts) * trialCount
  targets <- c(7, 108, 214, 426, 532, 633)
  rng <- list(x=2:245, y=c(54:48)) # location of table in sheet to get data
  # easier to specify variable names than to read from sheet, formatting issues
  varNames <- c("FRAME", "Start Tm", "End Tm", "Event", "Location")

  # data frame for stimulus data
  stimData <- data.frame(script=numeric(total), time=numeric(total), 
                         type=character(total), targetPos=numeric(total))

  # loop through runs to pull task data
    # note, this will take some time, due to read.xls which is slow and used 24
    #   times; maybe about a minute or two
  for (s in 1:length(scripts)) {

    print(scripts[s])

    # read script xls sheet
      # pattern = remove all columns before "FRAME"
      # skips first line and helps format columns appropriately
    xls <- read.xls(file.path(path, file), sheet=scripts[s], pattern="FRAME")
    # trial indices (for target, will subtract 2 to get start of cue)
    trialIndex <- grep("TARG", xls$Event)
    # stop if wrong number of trials
      # note: this is from the excel file, should be impossible! - sanity check
    if (length(trialIndex) != trialCount) {
      stop("wrong number of trials in file, or else file format is incorrect")
    }
    
    # trial info, contains cue length, delay length and target position
    stimData[1:trialCount + (s - 1) * trialCount, ] <- data.frame(
      script = rep(scripts[s], trialCount),
      # cue starts 2 units (3s) before target
      cueTime = xls$Start.Tm[trialIndex - 2], 
      targetTime = xls$Start.Tm[trialIndex],
      type = if (length(grep("AV", scripts[s])) == 0) {
          rep(types, each=trialCount/2)
        } else rep(rev(types), each=trialCount/2),
      # need the !=0 because for some reason some NAs became 0 here
      targetPos = match(xls$Location[!is.na(xls$Location) &
                                       (xls$Location != 0)], targets)
    )
  }

  # write table and return stimData
  write.table(stimData, file=file.path(path, output), row.names=F, quote=F)
  stimData
}



# ==============================================================================
# write timing files
# ==============================================================================
# write files separately for each run
  # will combine runs at a later step before glm
# for each trial type
  # correct, incorrect (not corrected), corrected, dropped
  # unscored saccades, blinks, maybe capped?
  # custom code for mgs to get delay periods
    # mark delay period saccades so not in unscored regressor
    # separate by cue and delay length
  # custom code for anti to subtract 1.5s from each time so start is prep period

## NOTE: should have a task-specific "timings settings" function, just like for the saccades
## DOUBLE NOTE: 

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
  if (task == "AntiState") { s <- filePrefix[5]; if (nchar(s) == 4 & substr(s, 1, 1) == "0") s <- gsub("0", "", s) }
  runData <- taskData[switch(task, MGSEncode = grep(r, taskData$run), AntiState = grep(s, taskData$script)), ]
  while("runData" %in% search()) detach(runData); attach(runData)

  # start of run
  runStartInd <- switch(task,
    MGSEncode = trialStart[which(saccade==1)[1]] - 90, # starts 1.5s (90 samples) before first cue
    AntiState = trialStart[which(saccade==1)[1]] - ms2asl(1000*(time[1])) # variable time to first trial (will model as 4.5s block)
  )

  # run for each trial type and outcome
  for(type in 1:length(trialTypes)){
    stimTimes <- runData[[switch(task, MGSEncode=switch(type,"1"="cueTime","2"="delayTime"), AntiState="cueTime")]] # stim times column name different for different tasks
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
            timingsDelay <- timings-delayLength[indMatch]
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
    tr <- 1.5; volumes <- switch(task, MGSEncode=229, AntiState=244); maxTime <- tr*(volumes-1)
    if(length(timings)>0 & max(timings)>maxTime) timings <- timings[-which(timings>maxTime)] # ... after run ends
    if(length(timings)==0) timings <- "*"
    # write file
    cat(timings, "\n", file=file.path(outPath, paste(nullReg, sep="_")))
  }

  detach(settings); detach(saccades); detach(runData)
  return(NULL)
}



# ==============================================================================
# convenience functions
# ==============================================================================

# check that variables are present, otherwise 
checkVars <- function(vars) {
  for (v in vars) if (!exists(v)) return(paste(v, "not found"))
  NULL
}

# concatenate data frames stored as elements in a list
list2data <- function(l, opts=list(listInd=T)) {
  if (opts$listInd) d <- cbind(l[[1]], listInd=1) else d <- l[[1]]
  if (length(l) > 1) for (i in 2:length(l)) {
    if (opts$listInd) {
      d <- rbind(d, cbind(l[[i]], listInd=i)) 
    } else d <- rbind(d, l[[i]])
  }
  d
}

# make a named list from a group of variables
makeList <- function(vars) {
  l <- list()
  for (v in vars) l[[v]] <- get(v, envir=parent.frame())
  l
}

# convert asl units
# time, 60Hz
asl2ms <- function(t) round(t * 1000 / 60)
ms2asl <- function(t) round(t * 60 / 1000)
# magnitude, 14.5 is an approximation but so is the 3/6/9deg vs 106/106/101px
asl2deg <- function(m) round(m / 14.5, 2)
deg2asl <- function(m) round(m * 14.5, 0)

# get counts of different dropped reasons
strFreq <- function(strIn) { # input is string vector
  # output is single string with counts of unique inputs
    # for example: "anticipatory:3,not_held:1"
  strOut <- ""
  ustr <- unique(strIn)
  for (u in ustr) {
    str <- paste(u, length(which(strIn %in% u)), sep=":")
    if (nchar(strOut) == 0) {
      strOut <- str 
    } else strOut <- paste(strOut, str, sep=",")
  }
  strOut
}

# multi-intersect function; gets intersect of all indices in list
multiIntersect <- function(x) {
  if (class(x) != "list" | length(x) < 2) stop(
    "incorrect input - either not a list or less than two sets of indices")
  ind <- x[[1]]
  for(i in 2:length(x)) ind <- intersect(ind, x[[i]])
  ind
}

checkEmptyVar <- function(var,stopstr="missing or empty variable") {
 if(is.null(var) || length(var) == 0L) stop(stopstr) 
}

# report indices to remove
# need xdatTime, runData[type,time,trial], and type (=1,2)
# uses global settings to get "mgs" or "vgs" from type (1,2)
trialMatches <- function(xdatTime,runData,strtype) {
   # what part of runData has this type
   tidx <- runData$type == strtype

   trialMatch<-list()
   # get the expected times for just this trial type
   trialMatch$typetrialTimes <- runData$time[tidx]
   # match trials to the time for both relative (just this type)  and full (all types)
   trialMatch$rel <- sapply(xdatTime, function(x) {which.min(abs(x - trialMatch$typetrialTimes))})
   # update to match the part of the dataframe the indexes originally came from
   trialMatch$full <- which(tidx)[trialMatch$rel]

   return(trialMatch)
}
findbadxdatidx <- function(xdatTime,runData,strtype){
   trialMatch <- trialMatches(xdatTime,runData,strtype)
   r <- rle(trialMatch$full)
   uniqueCount <- r$l

   # return which indexes to exclude
   excludeInd <- unlist(lapply(
     which(uniqueCount > 1),
     function(ind) {
      trial <- r$v[ind] # trial is trial of all types (e.g. 30 instead of 10 if type==2)

      # DS magic to get the repeated xdat indexes
      xdatInd <- c(1, 1 + cumsum(uniqueCount))[ind]
      xdatInd <- seq(xdatInd, xdatInd + uniqueCount[ind] - 1)

      # ERROR HERE, runData missing trial column (20151026 WF)
      #xdatInd[-which.min(abs(xdatTime[xdatInd] - 
      #          runData$time[which(runData$trial %in% trial)]))]

      # excludeInd should NOT have the closests of the duplicat trial starts
      closest <- which.min( abs( xdatTime[xdatInd] - runData$time[trial] ) )
      if(length(closest)==0L) return(xdatInd)
      return(xdatInd[-closest])
    }))
}

### useless function, trying to extract what type is used for
typeinfo <- function(type,task,xdats) {
    startInd <- switch(settings$whichIndex[type],
      first = c(1, 1 + cumsum(xdats$l))[which(c(xdats$v, F))],
      after = 1 + (cumsum(xdats$l))[which(xdats$v)]
    )
    # index of start of run to get times from xdats
    if (type == 1) {
      # assigning runStartInd to global environment so can use for other fns
      assign("runStartInd", switch(task,
        # starts 1.5s (90 samples) before first cue
        MGSEncode = startInd[1] - 90,
        # variable time to first trial (will model as 4.5s block)
        AntiState = startInd[1] - ms2asl(1000 * runData$time[1])
      ), env=globalenv())
    }

    strtype <- settings$xdatList[[type]]
  }
