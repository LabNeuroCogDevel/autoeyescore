#library(zoo)
#library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
# use DNA alignment algo Needleman-Wunsch for xdat alignment
# source("http://bioconductor.org/biocLite.R")
# biocLite('Biostrings')
library(Biostrings)
#source('eyescoreFunctions.R')

## TODO: 
#  ?? drop trial where saccade during dot flash

## USAGE:
#
# setMGSGlobals() sets "settings", called when file is sourced
# SCORE_ALL()  scores all files that it can find and saves scored sac to willout/

##### 
# 0. have a raw eye tracking file (parsed from eyd) -- see 00_eyd.bash
# 1. preprocess the eye data (e.g remove blinks, smooth)
# 2. get start/stop index of all saccades
# 3. get task timing and event info (runData) 
# 4. match saccades to tasks and score


# experimenting:
## see how well all subjs sacs match to target
# p <- montezdisplay()
#
##  score one run and plot
# a <- MGSscore.will(10129,20140103)
# plotAllSacs(norm.lat(a$d %>% mutate(subj=1,date=1)),onlycor=F,allines=T)
#
## single trial
# extractTrialWrap(a,trial=27,showplot=T,sipad=30) 


source('score_lunadaterun.R')
setMGSGlobals() # set Task and settings, used by scoreRun

# narm without having to load zoo, will loose factor labels
narm <- function(x) { x<-x[-which(is.na(x)) ]; ifelse(length(x)==0L,NA,x) }
# absmin
minabs <-function(x) { r<-x[which.min(abs(x))]; ifelse(length(r)==0L,NA,r) }

#
# score everyone
#
SCORE_ALL <- function(   globpat='~/rcn/bea_res/Data/Tasks/MGS/Basic/*/*/', # where subjects are
                      pathsvidx=c(10,11)) {

 # get all subject/visit directories and break into list of vector: c(subj,visit)
 subjdate <- lapply( strsplit(Sys.glob(globpat),'/'), FUN='[', pathsvidx )
 # path is strings, make numbers
 subjdate <- lapply(subjdate,as.numeric)

 # for each item in the subjdate date list
 for(s_d in subjdate) {
    writeScoreVisit(s_d[[1]],s_d[[2]])
 }
}

writeScoreVisit <- function(lunaid,visitdate,redo=F) {

  # skip if we have already scored this visit
  # redundant call to filesMGS :(
  f<-filesMGS(lunaid,visitdate)
  if(!file.exists(f[['raw']])){
    print(sprintf("skipping %d %d do not have raw %s",lunaid,visitdate,f[['raw']]))
    return()
  }
  if(file.exists(f[['willout']]) && !redo){ 
    print(sprintf("skipping %d %d have %s",lunaid,visitdate,f[['willout']]))
    return()
  }

  tryCatch({
     #print(sprintf('trying %d.%d',lunaid,visitdate))
     allinfo <- MGSscore.will(lunaid,visitdate) 
     d<-allinfo$d
     d$subj=lunaid
     d$date=visitdate

     write.table(d,row.names=F,quote=F,file=allinfo$files[["willout"]])
     print(sprintf("wrote %s",allinfo$files[["willout"]]))

 }, error=function(e){cat(sprintf("failed %d %d: ",lunaid,visitdate));print(e)})
}


###
# montez wants to see what all the (correct) sacs look like 
# when time=0 (per trial) is the start of the first saccade
# -- useful to quality check our scoring (all sacs should go to target)
montezdisplay <- function() {
 d<-readallMGS()
 dnorm <- norm.lat(d)
 p<- plotAllSacs(dnorm) 
 print(p)
 return(p)
}


## stats
statsPerTrial <- function(d) {
  # need subj and date to group by
  if(!"subj" %in% names(d)) d$subj <- 0
  if(!"date" %in% names(d)) d$date <- 0
  
  d %>% 
   filter(!is.na(trial)) %>% 
   group_by(trial,subj,date) %>% 
   summarise(nsac=max(sacno,na.rm=T),
             score=HMUC[first(which(sacno==1))],
             lat=first(narm(latency)),
             prcsn=minabs(accuracy)
             ) 
}

statsPerRun <-function(d) {

   statsPerTrial(d)          %>%
   group_by(subj,date,score) %>% 
   summarise(n=n(),lat.m=mean(lat),lat.sd=sd(lat),prcsn.m=mean(prcsn),prcsn.sd=sd(prcsn),nsac=mean(nsac) )
}

# read all the scored files into one big dataframe
readallMGS <-function(globpat='willout/*MGS.txt') {
 d<- rbind_all( lapply( Sys.glob(globpat),FUN=read.table,header=T ) )
}

# normalize time to first saccade onset=0
# only grab max 1.4 seconds after
# for DM compare
norm.lat <-  function(d,...) {
  plyr::ddply(d, c('trial','subj','date',...), function(x){ 
    i<-first(which(x$sacno==1))
    x %>% 
      mutate(  time=time-x$time[i], 
             tscore=first(narm(as.character(unique(HMUC))) ) ) %>% 
      filter(time>=0,time<=1.4,tscore=='cor')
  }) 
}


# plot all the saccades in dataframe d, output of (or merge of many outputs form) allRunSacs
plotAllSacs <-function(d,allines=F,showtarget=T,onlycor=T){
 if(onlycor) d<-d %>% filter(HMUC=='cor' )
 if('file' %in% names(d)) d$trial <- paste(d$trial,d$file)
 p <- ggplot(d )+
      aes(x=time,y=horz_gaze_coord,group=trial,color=as.factor(targetPos)) + 
      geom_smooth(aes(group=NULL)) +
      facet_wrap(~targetPos) +
      scale_y_continuous(limits=c(0,262))

 # individual lines
 if(allines) p<-p+geom_line(color='yellow',aes(color=NULL))

 # targets
 npos <- length(settings$xposTargetList)
 if(showtarget) p<-p+
           geom_hline(data=data.frame(targetPos=settings$xposTargetList ,px=settings$xposTargetList ),
                      aes(yintercept=px) )

 return(p + theme_bw() )
}
 

# MGSscore.will(lunaid=10125,date=20061021,runno=1,matchrun=2)
# load a subj's raw data, preproc saccades, scoreRun
# returns all steps in a list
MGSscore.will <- function(lunaid=10129,date=20140103,matchrun=1) {
  task <- "MGS"
  f<-filesMGS(lunaid,date)
  rawdata <- read.table(f[['raw']],header=T)
  ## danni's functions
  # smooth data
  eyedata <- preprocessEyd(rawdata)
  # get start and end index of saccades
  saccades<- getSaccades(eyedata)

  # align raw data to expected task xdats
  tdr     <- getGnrcRun(f[['log']]) 
  aligned <- alignToExpected(rawdata, tdr)
  # check that our alignment is good
  if( any(aligned$etyperaw!=aligned$etype) ) warning("some event types are mismatched!")

  # dani's version of runData (from aligned)
  drunData <- willtodani(aligned)
  # check 
  #  head(runData%>%arrange(xdatTime)%>%select(type,startInd,time,xdatTime,targetPos))
  #  source('willEye.R'); drunData <- willtodani(aligned);head(drunData)
  
  # score the run
  #scored   <- scoreRun(eyedata,saccades,drunData,outputTable=f[['scored']])
  scored   <- scoreRun(eyedata,saccades,drunData,outputTable=NULL)
  
  # everything we've done
  allinfo <- list(
              files  = f,
              scored =scored,
              runData=drunData,
              aligned=aligned,
              preproc=eyedata,
              rawdata=rawdata,
              files  =f)

  # combined scored saccades with task info
  # go get trials score
  allinfo$d<-allRunSacs(allinfo)

  return(allinfo)
}

# score a run (all trials) using extractTrial
# a (input) is output of MGSscore.will
allRunSacs <-function(a,nruns=32) {
 d <- rbind_all(lapply(1:nruns, FUN=function(t){ extractTrial(a$preproc,a$runData,a$scored,strial=t)} ))
}

# extract trial for list returned by MGSscore.will
# add arguments: e.g. showplot=T
extractTrialWrap <- function(a,trial=1,...) {
  extractTrial(a$preproc,a$runData,a$scored,strial=trial,...) 
}

### extractTrial
# ---- output
# raw eye data + sac id,score, and latency
# ---- input
# eyedata   preprocessed eye data (actual positions of eye)
# saccades  start/stop index of a saccade
# runData   tells us when a trial starts (only look 1.4 seconds in) | NOT orig. rundata, need trial
# scored    saccades and how they were scored
#
# tdur is duration of the trial in samples
# looking at only 60*2 (120 samples), length of mgs event, after for vgs doesn't matter?
#
# use _sipad_ and _eipad_ to view before and after a trial, used with showplot=T [NOT for actually scoring]
# adjust tdur to extend the view window when sipad and eipad are > 120
###
extractTrial <- function(eyedata,runData,scored,strial=1,ttype='mgs',tdur=120,showplot=F,sipad=0,eipad=0) {

  # index of this trial in the run
  tidx <- which(runData$trial==strial&runData$type==ttype)
  # die if we have wrong number of trials
  if(length(tidx) != 1L ) stop("wrong number of trials ",length(tidx) , ". matching trial ", strial," for type ", ttype,"!")

  # set start and stop idx
  si <- runData$startInd[tidx] - sipad
  ei <- si + tdur              + eipad


  # subset to just the sacs we care about
  trialscore <- scored %>% 
           filter( si <= end &   # does't end    before we start  looking
                   ei >= start ) # doesn't start after  we finish looking 

  ## subset the eyedata
  # get eye data that matches the saccades (can be more events then in the trial window alone) 
  trialeye    <- eyedata %>% 
     mutate(idx =  1:nrow(eyedata) ) %>%
     filter( idx >= min(c(si,trialscore$start)), 
             idx <= max(c(ei,trialscore$end)) )

  # merged saccades and preproc-ed raw data
  tscore   <- addsactoeye(trialeye,trialscore)

  # set time to trial onset
  tscore$time <- (tscore$idx - runData$startInd[tidx])/60

  # give a numeric score "how many until correct"
  tscore$HMUC <- HMUCscore(tscore) 
  
  # target position in runData, also used in accuracy

  targetpos <- runData$targetPos[tidx]

  #give tscore target pos and type for aggrigating later
  tscore$targetPos <- targetpos
  tscore$ttype     <- ttype
  tscore$trial     <- strial

  ## plot
  if(showplot){
   print(plotscored(tscore,targetpos) )
  }

  return(tscore)
}

# plot extracted trial. output of  extractTrial 
plotscored <- function(tscore,targetpos=NULL) {

   # get accuracy
   accpos    <- unique(tscore$accuracy) + targetpos

   #targetPos 7=1,108=2,214=3,426=4,532=5,633=6  
   # screen x size: 640

   p<- ggplot(tscore) + 
    aes(x=time,y=horz_gaze_coord,shape=as.factor(XDAT)) +
    geom_point() +
    geom_line(data=tscore[!is.na(tscore$sacno),],aes(group=sacno,color=HMUC) ) +
    #scale_color_discrete(c('red','green','orange','blue')) +
    scale_color_manual(limits=c("drop","cor", "incor","ercor"),
                       values=c('red','green','orange','blue'),
                       na.value="gray") +
    # plot latency 
    geom_vline(data=data.frame( x= (unique(tscore$latency)/1000 ) ),
              aes(xintercept=x),
              color='yellow' ) +
    # plot accuracy
    geom_hline(data=data.frame( y= accpos ), aes(yintercept=y), color='red', linetype='dotted' ) +
    scale_y_continuous(limits=c(0,245)) +
    theme_bw() +
    ggtitle(sprintf("trail %d: %s",first(tscore$trial),first(narm(as.character(unique(tscore$HMUC))) )) )
 
   # add targetpos if we have it
   if(!is.null(targetpos)) {
     # asl    x size is 240
     aslpos <- c(7,108,214,426,532,633) * 261/640 #240/640
     #aslpos <- rev(c(7,108,214,426,532,633)) * 240/640
     trgloc <- aslpos[ targetpos ]
     p<-p+geom_hline(data=data.frame( y= trgloc ), aes(yintercept=y), color='purple') 
   }
   return(p)
}

# take will's aligned dataframe (each row is xdat)
# turn into dani's dataframe
#e.g.
#  startInd time xdatTime targetPos trial type
#      567   9.5    9.433       245     1  mgs
#     1377  23.0   22.933       245     2  mgs
#     2187  36.5   36.433       188     3  mgs
willtodani <- function(adf,taskstarttime=0,samplerate=60) {
 # TIMING:
 #    -> XDAT 42/43
 #     CueMGS (1.925s)
 #    -> XDAT 121/
 #     TargetMGS (.075s)
 #     WMdelay (Sort/long, 2.5s) 
 #     MGSExecute (2s)
 #    -> XDAT 250
 #     feedback (2s)
 

 b<- adf %>% 
   # build need vars
   mutate(
     # MGS happens 2seconds before the first end
     timeadj   = ifelse(etypetask=='end', -2, 0),
     # dani has this so the start cue is 1.4
     time      = time     + timeadj - taskstarttime,
     # N.B. startInd (adjusted) is not startind (unadjusted)
     startInd  = startind + timeadj*samplerate,
     # xdattime, we use the adjusted startInd and take out the first actual index 
     xdatTime  = round( (startInd - first(startind))/samplerate, 3) ,
     # targetpos is in screen resolution, we need it in asl
     targetPos = xposEP2ASL(targetPos)
   ) %>%
   # only want start and end, no FIXes
   # we added      to get to cue onset from start
   #    subtracted to get MGSexcute onset)
   filter(etypetask =='end' ) %>% 
   # grab only want we need
   select(startInd,time,xdatTime,targetPos,trial=rtrial) #,xdattask,etypetask)

 # make vgs targetpos that of mgs
 # this could fail if vgs doesn't always prepceed mgs
 # it would mean something is wrong
 #b$targetPos[b$targetPos==0] <- b$targetPos[b$targetPos!=0]

 # add type is mgs, same for all
 b$type <- 'mgs'
 return(b)
}

addsactoeye <- function(eyedata,sacidx) {
 # add index if we haven't
 if(!"idx" %in% names(eyedata)) eyedata$idx <- 1:nrow(eyedata)

 # what kind of sacidx we have (score or unscored)
 # determines what columns we want
 cols <- c('replaced','merged')
 scorednames <- c('corrected','incorrect','correct','dropped','latency','accuracy')
 if( all(scorednames %in% names(sacidx)) ) cols <- c(cols,scorednames)

 # merge sacidxs into the eyedata data frame
 eyedata[,c('sacno',cols)]<- NA

 # end if no scored sacs
 if(nrow(sacidx)==0L) return(eyedata)

 ni <- 0
 for(i in 1:nrow(sacidx)){ 
    ni<-ni+1
    idxrange <- sacidx$start[i]:sacidx$end[i]
    rawidxsofsac<- eyedata$idx %in% idxrange
    repidx <- rep(i,length(which(rawidxsofsac) ))
    eyedata[rawidxsofsac,cols] <- sacidx[ repidx , cols]
    eyedata$sacno[rawidxsofsac] <- ni
 }
 
 eyedata
}

# align rawdata to expected task xdats 
# -- will likely catch misnamed runs
# output dataframe:
# tdridx       index key, should be consecutive (rowid)
# etyperaw     event type (start,delay,end) according to rawdata
# trialraw     number of cue's seen ("trial number" in raw data)
# xdatraw      xdat in raw eye data
# nsamp        number of raw data samples of the xdat
# startind     index raw xdat starts
# netype       number of xdats in this group (should be 1 unless LPT or TX error)
# event        eprime event name (e.g. SCLDstartEM, targetFIX)
# xdattask     xdat that should have been sent by eprime (should always agree with xdatraw)
# time         expected time xdat would be sent by eprime
# trialtask    number start-delay-end pair in task
# etypetask    start/delay/end from task (should always agree with etyperaw)
# run          1-3
# rtrial       "real" trial count where fix start-delay-end are ignored 
#
alignToExpected <- function(rawdata,tdr) {
  # try to align raw xdats to expected task xdats
  a <- doalign(rawdata,tdr[,xdat]) 

  # collapse raw data into xdat index data frame
  xraw   <- getxdats(rawdata)

  # add a task index
  #  -- should NA extra start and end
  i <-  rangeidx(a)
  Ndif <- nrow(xraw) - length(i) 
  if(Ndif>0) i <- c(i, rep(NA,Ndif) )

  if(Ndif<0) stop('something when wrong in the alignment: returns more indexs than have xdats for!')

  xraw$tdridx <- i

  # make a pretty data frame
  merge(xraw,tdr,by.x='tdridx',by.y='row',suffixes=c('raw','task'))
}

# get the generic run info from a file
# function will fail if in wrong directory :(
# /Users/lncd/rcn/bea_res/Data/Tasks/MGS/Basic/10312/20090828/Raw/EyeData/txt/10312.20090828.MGS.EPxdat.log
#  0	StartMGSShort	42	600
getGnrcRun <- function(filen) {
  r <- setNames(read.table(filen), c('time','event','xdat','targetPos') )
  # set "real" trial number by cumlitive counting cues that are not 60 (exclude fix)
  r$rtrial <- cumsum(r$xdat <100 & r$xdat!=60)
  r$row    <- 1:nrow(r) 
  r$etype  <- cut(r$xdat,breaks=c(-Inf,100,200,Inf), labels=c('start','delay','end') )
  return(setDT(r))
}

 

getxdats <- function(rawdata) {
 # only want one run
 # need taskData,run
 #taskdata <- taskData %>% filter(run==run) 

 xr<-rle(rawdata$XDAT)

 xdats <- 
   # 0. create data frame from runlength encoded info
   data.frame(
      xdat    = xr$v,
      startind= c(1,1+cumsum(xr$l[-length(xr$l)])),
      l       = xr$l
   ) %>%
   # name events and trials
   mutate( 
     etype = xdattype(xdat),
     trial = 1+cumsum(ifelse(etype=="start",1,0)) ) %>%
   # group by event and trial 
   group_by(etype,trial) %>% 
   # so we can collapse repeats
   summarise( 
      xdat    = xdat[which.max(l)],
      nsamp   = sum(l),
      startind= min(startind), 
      netype  = n() ) %>% 
   ungroup %>% arrange(startind)

# library(ggplot2)
# ggplot(xdats) + geom_histogram(aes(x=as.factor(ls),fill=etype),position='dodge')
# library(data.table)
# dcast(setDT(xdats), trail ~  etype, value.var=c('n','xdat','ls','startind'))
}


xdattype <- function(xdat) {
 ifelse(xdat<100,       'start' , 
        ifelse(xdat>220,'end',
                        'delay' ) )
}

# "How Many saccades Until Correct"
# -1 is dropped
#  0 is incorrect
#  1 is correct
#  2 is corrected incorrect
# "drop"=-1,"incor"=0, "cor"=1,"ercor"=2,"badscore"=3)
HMUCscore <- function(score) {
  #                       -1          0         1          2
  m<-as.matrix(score[,c('dropped','incorrect','correct','corrected')])
 
  # look for the truth in each column
  # if only column 1 = drop
  #                2 = incorrect
  #                3 = correct
  #                4 = corrected
  # if there is more than one columnt give 5
  # THEN subtract 2 to get to the "how many until correct" score number
  s<- as.factor(unlist(
   lapply(
     apply(FUN=which,MARGIN=1,m),
     FUN=function(x){ 
           switch(as.character(length(x)),
             "0"=NA,
             "1"=x, 
             "2"=4,  # assume it's corrected and incorrect, set to same as just corrected
              NA
   )  }))  - 2 )

  # if we were all NA, apply returned empty, so give list of all NAs
  if(length(s)==0L) s <- rep(NA,nrow(score))
  
  
  levels(s) <- list( "drop"=-1,"incor"=0, "cor"=1,"ercor"=2,"badscore"=3)
  s
}

######## ALIGNMENT

doalign <- function(rawdata,runxdats) {
 rawxdat <- rle(rawdata$XDAT)$v
 #taskList <- read.table('runOrderXdat/all_xdats.txt')
 #runxdats <- setDT(taskList)[run==2,xdat]
 a<-pairwiseAlignment( pattern=num2bstr(runxdats), subject=num2bstr(rawxdat) )
 #print(  cbind( bstr2num(a@subject) , runxdats )  )
}



## return vector of indexs i such that subject(i) = expected task
rangeidx <-function(a,expectscore=200) {
 if(a@score<expectscore) {warning('score: ',round(a@score,2),' is low; matching to wrong run?')}
 s <- bstr2num(a@subject)
 p <- bstr2num(a@pattern)

 # use NaN instead of NA so we can remove later
 s.nan <- as.numeric(s>0)
 s.nan[is.na(s.nan)] <- NaN

 p.na  <- as.numeric(p>0)    # 1 or NA
 p.inc <- cumsum( is.na(p) ) # 0 0 0 0 1 2 3 3 3 3 

 # index from start of pattern (should be same length as subject matches)
 # make NaNs where subject gaps
 # decrement where pattern is nan
 # put NA where pattern gaps
 idx.nan  <- 1:length(p) * s.nan - p.inc*p.na

 # remove where subject gaps: subject(idx) can never match
 idx <- idx.nan[!is.nan(idx.nan)]
 # see 
 #print(cbind(p,s,idx.nan))

 # subject probably stated with junk xdats, NA those
 idx.fpad <- c(rep(NA,a@subject@range@start-1), idx)

 # do we need to add NAs to end
 # needs to be done elsewhere?
 #Ndif <- a@pattern@range@width idx.fpad 
 # idx <- c(idx.fpad,c(rep(NA,Ndif))
 #browser()

 return(idx.fpad)

}

# take in an alignment, give back indexes of subject matching pattern
# N.B assumes no gaps in pattern
#          "ATCTCGGGGGATAT"
# pattern: [4] TCGGGGGATAT 
# subject: [1] TC----GAT-T 
# returns: 4  5 10 11 12 14

rangeidx.old <- function(a) {
 ###
 prange<-a@pattern@range
 srange<-a@subject@indel@unlistData
 pstart <- prange@start - 1
 widths <- cumsum(c(0,srange@width))
 if(length(srange)>0L) {
  gaps <- unlist(
   sapply( 1:length(srange),
    function(i){  pstart  + srange@start[i] + widths[i] + c(0: (srange@width[i]-1) ) })
  )
 } else{
  gaps <- 0
 }
 c(rep(NA,1,a@subject@range@start-1), setdiff( prange@start +c(1:prange@width-1), gaps))
}

######  string <-> number
# number to bstring
num2bstr <- function(nums) {
  BString( rawToChar(as.raw( nums  )) )
}
# convert bstring back to numbers, replace - with NA
# - is missing in pattern, 2d in raw, 45 in numeric
# 45 is not an xdat used (but could occur in very weird case)
bstr2num <-function(str) {
  n <- as.numeric(charToRaw(as.character(str)))
  n[n==45] <- NA 
  n
}
