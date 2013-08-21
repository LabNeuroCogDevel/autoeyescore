#### Settings ######

# XDAT CODES
startcodes   <- c(7,8,9)*10
targetcodes  <- c(170:200)
stopcodes    <- c(0,250)

# number of trials
expectedTrialLengths  <- 60 

#minium distance to be considered a saccade
sac.minmag   <-  10      # min abs of x position change -- set very low, inc to 20 at LR request :)

## xpos ##
# saccade left thresholds
sac.left.large  <- 2 
sac.left.mid    <- 44
sac.left.small  <- 87 

# saccade right thresholds
sac.right.small <- 172
sac.right.mid   <- 216
sac.right.large <- 258

# acceleration -- needs to be this many pixels per sample before considering saccade
lat.minvel   <- 4      # ASLcoordx/60Hz 

# if there are spikes in fixation, bad tracking, drop
maxSamplesFromBaseline <- 2

# padding to give to expected positions
sac.padding     <- 30

# the useful saccades probably already occur
sac.majorRegionEnd <- .75 
  
sac.thresholds <- c(
      c  (sac.right.large,
          sac.right.mid,
          sac.right.small  ) , # + sac.padding

      c(  sac.left.small,
          sac.left.mid,
          sac.left.large ) #- sac.padding
)


names(sac.thresholds)<-1:4

#filename <- paste(sep="",  'txt/',  paste(collapse=".",c(splitfile[i,c('subj','date','run')],'tsv') ) )
#type     <- splitfile$type[i]
#rundate  <- splitfile$date[i]
#run      <- splitfile$run[i]
#subj     <- splitfile$subj[i]
#id       <- paste( rundate,run, sep=".")  
#
## from ScoreRun.R
## saverootdir  <- 'eyeData'
## outputdir <- paste(saverootdir, subj,runtype,sep="/")
## file=paste(outputdir,"/",subj,"-",runtype,".txt",sep="")
#savedas  <- paste(  paste('eyeData',subj,id,sep="/"), "/", subj, "-", type,'.txt', sep="")


## FUNCTIONS
# where are the files?
filebasedir <- 'DollarReward/txt'
getFiles <- function(filesFrom=sprintf('*.data.tsv',filebasedir)) {
 #/mnt/B/bea_res/Data/Tasks/Anti/Basic/11146/20130313/Raw/EyeData/txt/11146.20130313.anti.1.tsv
 files     <- Sys.glob(filesFrom)
 #/mnt/B/bea_res/Data/Tasks/Anti/Basic/11146/20130313/Raw/EyeData/txt/11146.20130313.anti.1.tsv
 # subjs are the number part of the filename
 splitfile <- data.frame(subj=gsub('[^0-9]','',files)) 
 splitfile$type    <- 'DollarReward'
 # date and run are need elsewhere
 splitfile$subj    <- as.character(splitfile$subj)  # was factor
 splitfile$date    <- 0
 splitfile$run     <- 0
 splitfile$file    <- files
 splitfile$id      <- splitfile$subj
 splitfile$savedas <- sprintf('%s/%s.sac.txt',filebasedir,splitfile$subj)
 return(splitfile)
}

# what is the threshold for left/right position
# ... the expected final position of a correct saccade 
getExpPos <- function(sac.thresholds,xdatCode){
    # xdatCode is index for thesholds (1 -> right short, 2-> right long, 3->left short, 4->left long)
    # targetcodes are in pairs of 4, first 2 are right, second 2 are down. But this is always anti
    #  -- find which index the targetcode matches in the list, find which number in the pair of four it is
    #  use that (i^th) index of threshold
    threshIDX <- as.numeric(substr(xdatCode,3,3))
    exppos <- sac.thresholds[ threshIDX ] 
}

# is this xdat an antisaccde xdat?
xdatIsAS <- function(xdat){
 return(TRUE)
}
