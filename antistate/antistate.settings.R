#### Settings ######

# XDAT CODES
startcodes   <- c(60,70)
targetcodes  <- c(160:180)
stopcodes    <- c(250)

# number of trials
expectedTrialLengths  <- 24

#minium distance to be considered a saccade
sac.minmag   <-  20      # min abs of x position change -- set very low, inc to 20 at LR request :)

## xpos ##
# saccade left thresholds
sac.right.large  <- 7 
sac.right.middle <- 108
sac.right.small  <- 214

# saccade right thresholds
sac.left.small <- 426
sac.left.middle <- 532
sac.left.large <- 633

# acceleration -- needs to be this many pixels per sample before considering saccade
lat.minvel   <- 4      # ASLcoordx/60Hz 

# if there are spikes in fixation, bad tracking, drop
# 99 means don't work about them
maxSamplesFromBaseline <- 99

# padding to give to expected positions
sac.padding     <- 30

# the useful saccades probably already occur
# time in seconds
sac.majorRegionEnd <- .75
  
sac.thresholds <- c(
      c  (sac.right.large,
      	  sac.right.middle,
          sac.right.small  ) ,

      c(  sac.left.small,
      	  sac.left.middle,
          sac.left.large )
)/(800/261) # convert from PC resolution (800x600 -> ASL 261x240)

names(sac.thresholds)<-c(1:6)



## FUNCTIONS
# where are the files?
filebasedir <- '/mnt/B/bea_res/Data/Tasks/AntiState/Basic/'
getFiles <- function(filesFrom=sprintf('%s/*/*/Raw/EyeData/txt/*.data.tsv',filebasedir)) {
 #/mnt/B/bea_res/Data/Tasks/Anti/Basic/11146/20130313/Raw/EyeData/txt/11146.20130313.anti.1.tsv
 files     <- Sys.glob(filesFrom)
 splitfile <- strsplit(basename(files),'\\.')
 splitfile <- as.data.frame(t(sapply(splitfile,rbind)))[,-5]
 names(splitfile)  <- c('subj','date','run')
 splitfile$subj    <- as.character(splitfile$subj)
 splitfile$date    <- as.character(splitfile$date)
 splitfile$type    <- 'AntiState'
 #should always be 1?
 splitfile$run     <- as.numeric(as.character(splitfile$run))
 splitfile$file    <- files
 splitfile$id      <- paste( splitfile$date,splitfile$run, sep=".")  
 splitfile$savedas <- paste(dirname(dirname(dirname(dirname(files)))), '/Scored/txt/',
                            paste(splitfile$subj,splitfile$date,splitfile$run,'sac.txt',sep="."),
                            sep="" )
 return(splitfile)
}

# is this xdat an antisaccde xdat?
xdatIsAS <- function(xdat){
 # these are all prosacs
 return(as.numeric(xdat) > 170)

}

# what is the threshold for left/right position
getExpPos <- function(sac.thresholds,xdatCode){
    ## xdatCode is 16x or 17x where x is xpos of dot: 1 (far right) to 6  (far left)
    # we only want to look at last (3rd) digit (threshIDX)
    threshIDX <- as.numeric(substr(xdatCode,3,3))

    # but if we are on an antisac trial, we need to reverse the expectation
    # so 1->6, 2->5, ..., 6->1
    if(xdatIsAS(xdatCode)) {
      exppos <- sac.thresholds[7-threshIDX ] 
    } else {
      exppos <- sac.thresholds[ threshIDX ] 
    }

    cat(sprintf('xdat: %.0f, expos: %.0f, AS: %s\n',xdatCode,exppos,xdatIsAS(xdatCode))) 
    return(exppos)
}

trialIsType <- function(xdat){
 if(xdatIsAS(xdat) ) { return('AS') }
 return('PS')
}
