#### Settings ######

# XDAT CODES
startcodes   <- c(22,26,32,36,42,46) #rep(c(2,6),3)+ rep(c(20,30,40),2)
targetcodes  <- c(121:124,131:134,141:144)
stopcodes    <- c(0,250)

# number of trials
expectedTrialLengths  <- 48 # 3 cues * 2 delays * 4 sides * 2 repeats each

#minium distance to be considered a saccade
sac.minmag   <-  10      # min abs of x position change -- set very low, inc to 20 at LR request :)

## xpos ##
# saccade left thresholds

# acceleration -- needs to be this many pixels per sample before considering saccade
lat.minvel   <- 4      # ASLcoordx/60Hz 

# if there are spikes in fixation, bad tracking, drop
maxSamplesFromBaseline <- 2

# padding to give to expected positions
sac.padding     <- 30

# the useful saccades probably already occur
sac.majorRegionEnd <- .75 
  
# from left to right
# 40, 180, 460, 600 | max x on display is 640 | on eye tracker max x is 261 (?)
sac.thresholds <- round(c(40,180,460,600)/640 * 261)
names(sac.thresholds)<-1:4


## FUNCTIONS
# where are the files?
filebasedir <- '/mnt/B/bea_res/Data/Tasks/Mix/Basic/'
filebasedir <- '/Users/lncd/rcn/bea_res/Data/Tasks/Mix/Basic/'
getFiles <- function(filesFrom=sprintf('%s/*/*/Raw/EyeData/txt/*.data.tsv',filebasedir)) {
 files     <- Sys.glob(filesFrom)
 splitfile <- strsplit(basename(files),'\\.')
 splitfile <- as.data.frame(t(sapply(splitfile,rbind)))[,-4]
 names(splitfile)  <- c('subj','date')
 splitfile$type    <- 'Mix'
 splitfile$subj    <- as.character(splitfile$subj)
 splitfile$date    <- as.character(splitfile$date)
# splitfile$type    <- as.character(splitfile$type)
 splitfile$run     <- 1
 splitfile$file    <- files
 splitfile$id      <- paste( splitfile$date,splitfile$run, sep=".")  
 splitfile$savedas <- paste(dirname(dirname(dirname(dirname(files)))), '/Scored/txt/',
                            paste(splitfile$subj,splitfile$date,splitfile$run,'sac.txt',sep="."),
                            sep="" )
 return(splitfile)
}


# is this xdat an antisaccde xdat?
# 121-124 is PS, 131-4 is AS, 141-4 is FIX
trialIsType <- function(xdat){
 dec <- floor(xdat/10)
 if( dec == 12 ) return('PS')
 if( dec == 13 ) return('AS')
 if( dec == 14 ) return('FX')

 warning('xdat ', xdat, ' is not understood to be PS AS or FX!')
 return('??')
}

# what is the threshold for left/right position
# ... the expected final position of a correct saccade 
# xdatCode is from target
getExpPos <- function(sac.thresholds,xdatCode){
    tt <- trialIsType(xdatCode)
    side <- xdatCode%%10
    if(! side %in% 1:4) {
      warning("xdat ",xdatCode," cannot be prased into a side within 1:4(", side,") -- return middle");
      tt <- "FX"
    }

    if(tt == "PS")  return(sac.thresholds[side])
    if(tt == "AS")  return(rev(sac.thresholds)[side])
    if(tt == "FX")  return(261/2)

}

