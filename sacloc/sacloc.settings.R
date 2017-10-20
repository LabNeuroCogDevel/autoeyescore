#### Settings ######

# XDAT CODES
startcodes   <- c(3,6,8)*10
targetcodes  <- c(1:4)+130
stopcodes    <- c(250)

CAREABOUTSTARTCODE <- T

# number of trials
expectedTrialLengths  <- 42

#minium distance to be considered a saccade
sac.minmag   <-  10      # min abs of x position change -- set very low, inc to 20 at LR request :)

## xpos ##
# saccade left thresholds
sac.left.large  <- 2 
sac.left.small  <- 87 

# saccade right thresholds
sac.right.small <- 172
sac.right.large <- 258

# acceleration -- needs to be this many pixels per sample before considering saccade
lat.minvel   <- 4      # ASLcoordx/60Hz 

# if there are spikes in fixation, bad tracking, drop
# 99 means don't worry about it
maxSamplesFromBaseline <- 99

# padding to give to expected positions
sac.padding     <- 30

# the useful saccades probably already occur
sac.majorRegionEnd <- .75 
  
sac.thresholds <- c(
      c  (sac.right.large,
          sac.right.small  ) ,

      c(  sac.left.small,
          sac.left.large )
)

names(sac.thresholds)<-c(1:4)



## FUNCTIONS
# where are the files?
filebasedir <- '/mnt/B/bea_res/Data/Tasks/SaccadeLocalizerBasic/'
getFiles <- function(filesFrom=sprintf('%s/*/*/Raw/EyeData/txt/*.data.tsv',filebasedir)) {
 #/mnt/B/bea_res/Data/Tasks/SaccadeLocalizerBasic/11146/20130313/Raw/EyeData/txt/11146.20130313.data.1.tsv
 files     <- Sys.glob(filesFrom)
 splitfile <- strsplit(basename(files),'\\.')
 splitfile <- as.data.frame(t(sapply(splitfile,rbind)))[,-5]
 names(splitfile)  <- c('subj','date','run')
 splitfile$subj    <- as.character(splitfile$subj)
 splitfile$date    <- as.character(splitfile$date)
 splitfile$type    <- 'Anti'
 #should always be 1?
 splitfile$run     <- as.numeric(as.character(splitfile$run))
 splitfile$file    <- files
 splitfile$id      <- paste( splitfile$date,splitfile$run, sep=".")  
 splitfile$savedas <- paste(dirname(dirname(dirname(dirname(files)))), '/Scored/txt/',
                            paste(splitfile$subj,splitfile$date,splitfile$run,'sac.txt',sep="."),
                            sep="" )
 return(splitfile)
}
getExpPos <- function(sac.thresholds,xdatCode){
    xdats <- sepcodes(xdatCode)
    tt <- trialIsType(xdatCode)
    side <- xdats$target - 130 # xdatCode is index for thesholds (1 -> right short, 2-> right long, 3->left short, 4->left long)

    if(tt == "PS")  return(rev(sac.thresholds)[side])
    if(tt == "AS")  return(sac.thresholds[side])
    if(tt == "FX")  return(261/2)

}

# if start code was 30 and and target was 131 =  30131
# 30131%%1000 == 131
# floor(30131/1000) == 30
sepcodes<-function(modxdat) {
 list( 'target' = modxdat%%1000,
       'start'  = floor(modxdat/1000))
}


# is this xdat an antisaccde xdat?
xdatIsAS <- function(xdat){
 startcode = sepcodes(xdat)$start
 if(startcode == 60) return(TRUE)
 return(FALSE)
}
trialIsType <- function(xdat){
 startcode = sepcodes(xdat)$start
 if(startcode == 60) return('AS')
 if(startcode == 80) return('PS')
 if(startcode == 30) return('FX')
 warning(xdat,' is not in expected 60 (AS) 80 (PS) 30 (FIX)')
}
