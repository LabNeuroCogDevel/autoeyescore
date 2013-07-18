#### Settings ######

# XDAT CODES
startcodes   <- c(1:10,20)*10
targetcodes  <- c(111:150,201:204)
stopcodes    <- c(0,250)

# number of trials
expectedTrialLengths  <- 42

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

# if there are spikes in fixation, bad tracking, drop
maxSamplesFromBaseline <- 2

# padding to give to expected positions
sac.padding     <- 30

# the useful saccades probably already occur
sac.majorRegionEnd <- .75 
  
sac.thresholds <- c(
      c  (sac.right.large,
          #sac.left.mid,
          sac.right.small  ) , # + sac.padding

      c(  sac.left.small,
          #sac.right.mid,
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
filebasedir <- '/mnt/B/bea_res/Data/Tasks/BarsScan/Basic/'
getFiles <- function(filesFrom=sprintf('%s/*/*/Raw/EyeData/txt/*.data.tsv',filebasedir)) {
 #/mnt/B/bea_res/Data/Tasks/Anti/Basic/11146/20130313/Raw/EyeData/txt/11146.20130313.anti.1.tsv
 files     <- Sys.glob(filesFrom)
 #files <-c(
 # '/mnt/B/bea_res/Data/Tasks/BarsScan/Basic/10590/20080815/Raw/EyeData/txt/10590.20080815.1.data.tsv',
 # '/mnt/B/bea_res/Data/Tasks/BarsScan/Basic/11162/20130521/Raw/EyeData/txt/11162.20130521.3.data.tsv',
 # '/mnt/B/bea_res/Data/Tasks/BarsScan/Basic/11162/20130521/Raw/EyeData/txt/11162.20130521.4.data.tsv'
 #)
 splitfile <- strsplit(basename(files),'\\.')
 splitfile <- as.data.frame(t(sapply(splitfile,rbind)))[,-4]
 names(splitfile)  <- c('subj','date','run')
 splitfile$type    <- 'BarsScan'
 splitfile$subj    <- as.character(splitfile$subj)
 splitfile$date    <- as.character(splitfile$date)
# splitfile$type    <- as.character(splitfile$type)
 # run should be 1 to 4 but might have _corrected, so only take the first bit
 splitfile$run     <- as.numeric(substr(as.character(splitfile$run),1,1))
 splitfile$file    <- files
 splitfile$id      <- paste( splitfile$date,splitfile$run, sep=".")  
 splitfile$savedas <- paste(dirname(dirname(dirname(dirname(files)))), '/Scored/txt/',
                            paste(splitfile$subj,splitfile$date,splitfile$run,'sac.txt',sep="."),
                            sep="" )
 return(splitfile)
}

# what is the threshold for left/right position
# ... the expected final position of a correct saccade 
getExpPos <- function(sac.thresholds,xdatCode){
    # xdatCode is index for thesholds (1 -> right short, 2-> right long, 3->left short, 4->left long)
    # targetcodes are in pairs of 4, first 2 are right, second 2 are down. But this is always anti
    #  -- find which index the targetcode matches in the list, find which number in the pair of four it is
    #  use that (i^th) index of threshold
    sacidx <- which(targetcodes %in% xdatCode) %% 4 
    if(sacidx == 0) sacidx=4
    return(sac.thresholds[sacidx])
}

# is this xdat an antisaccde xdat?
xdatIsAS <- function(xdat){
 return(TRUE)
}
