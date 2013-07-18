#### Settings ######

# XDAT CODES
startcodes   <- c(2,4,5,6)+20
targetcodes  <- c(1:4)+120
stopcodes    <- c(250)

# number of trials
expectedTrialLengths  <- 48

#minium distance to be considered a saccade
sac.minmag   <-  20      # min abs of x position change -- set very low, inc to 20 at LR request :)

## xpos ##
# saccade left thresholds
sac.right.large  <- 2 
sac.right.small  <- 87 

# saccade right thresholds
sac.left.small <- 172
sac.left.large <- 258

# if there are spikes in fixation, bad tracking, drop
# 99 means don't work about them
maxSamplesFromBaseline <- 99

# padding to give to expected positions
sac.padding     <- 30

# the useful saccades probably already occur
# time in seconds
sac.majorRegionEnd <- .5
  
sac.thresholds <- c(
      c  (sac.right.large,
          sac.right.small  ) ,

      c(  sac.left.small,
          sac.left.large )
)

names(sac.thresholds)<-c(1:4)



## FUNCTIONS
# where are the files?
filebasedir <- '/mnt/B/bea_res/Data/Tasks/VGS/Basic/'
getFiles <- function(filesFrom=sprintf('%s/*/*/Raw/EyeData/txt/*.data.tsv',filebasedir)) {
 #/mnt/B/bea_res/Data/Tasks/Anti/Basic/11146/20130313/Raw/EyeData/txt/11146.20130313.anti.1.tsv
 files     <- Sys.glob(filesFrom)
 splitfile <- strsplit(basename(files),'\\.')
 splitfile <- as.data.frame(t(sapply(splitfile,rbind)))[,-5]
 names(splitfile)  <- c('subj','date','run')
 splitfile$subj    <- as.character(splitfile$subj)
 splitfile$date    <- as.character(splitfile$date)
 splitfile$type    <- 'VGS'
 #should always be 1?
 splitfile$run     <- as.numeric(as.character(splitfile$run))
 splitfile$file    <- files
 splitfile$id      <- paste( splitfile$date,splitfile$run, sep=".")  
 splitfile$savedas <- paste(dirname(dirname(dirname(dirname(files)))), '/Scored/txt/',
                            paste(splitfile$subj,splitfile$date,splitfile$run,'sac.txt',sep="."),
                            sep="" )
 return(splitfile)
}
# what is the threshold for left/right position
getExpPos <- function(sac.thresholds,xdatCode){
    # xdatCode is index for thesholds (1 -> right short, 2-> right long, 3->left short, 4->left long)
    return(sac.thresholds[ xdatCode - 130 ] )
}

# is this xdat an antisaccde xdat?
xdatIsAS <- function(xdat){
 # these are all prosacs
 return(FALSE)
}
