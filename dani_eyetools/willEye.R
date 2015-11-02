#library(zoo)
library(dplyr)
library(tidyr)
library(data.table)
# use DNA alignment algo Needleman-Wunsch for xdat alignment
# source("http://bioconductor.org/biocLite.R")
# biocLite('Biostrings')
library(Biostrings)
#source('eyescoreFunctions.R')



# 10133,20071204
# willeyeam(lunaid=10125,date=20061021,runno=1,matchrun=2)
source('score_lunadaterun.R')

# load a subj's raw data, preproc saccades, scoreRun
# returns all steps in a list
willeyeam <- function(lunaid=10125,date=20061021,runno=1,matchrun=1) {
  f<-filesMGS(lunaid,date,runno)
  rawdata <- read.table(f[['raw']],header=T)
  ## danni's functions
  # smooth data
  eyedata <- preprocessEyd(rawdata)
  # get start and end index of saccades
  saccades<- getSaccades(eyedata)

  # align raw data to expected task xdats
  tdr     <- getGnrcRun(matchrun) 
  aligned <- alignToExpected(rawdata, tdr)
  # check that our alignment is good
  if( any(aligned$etyperaw!=aligned$etype) ) warning("some event types are mismatched!")

  # dani's version of runData (from aligned)
  drunData <- willtodani(aligned)
  # check 
  #  head(runData%>%arrange(xdatTime)%>%select(type,startInd,time,xdatTime,targetPos))
  #  source('willEye.R'); drunData <- willtodani(aligned);head(drunData)
  
  # score the run
  scored   <- scoreRun(eyedata,saccades,drunData,outputTable=f[['scored']])
  
  # everything we've done
  allinfo <- list(
              scored =scored,
              runData=drunData,
              aligned=aligned,
              preproc=eyedata,
              rawdata=rawdata,
              files  =f)

  return(allinfo)
}

# eyedata   preprocessed eye data (actual positions of eye)
# saccades  start/stop index of a saccade
# runData   tells us when a trial starts (only look 1.4 seconds in) | NOT orig. rundata, need trial
# scored    saccades and how they were scored
#
# tdur is duration of the trial in samples
# looking at only 60*1.4 (84 samples), length of mgs event, after for vgs doesn't matter?
extractTrial <- function(eyedata,runData,scored,strial=1,ttype='mgs',tdur=84,showplot=F) {

  # index of this trial in the run
  tidx <- which(runData$trial==strial&runData$type==ttype)
  # die if we have wrong number of trials
  if(length(tidx) != 1L ) stop("wrong number of trials ",length(tidx) , "matching trial ", strial," for ", ttype,"!")

  # set start and stop idx
  si <- runData$startInd[tidx]
  ei <- si + tdur 


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
  tscore$time <- (tscore$idx - si)/60

  # give a numeric score "how many until correct"
  tscore$HMUC <- HMUCscore(tscore) 

  # get accuracy
  #targetPos 7=1,108=2,214=3,426=4,532=5,633=6  
  # screen x size: 640
  # asl    x size is 240
  aslpos <- c(7,108,214,426,532,633) * 240/640
  #aslpos <- rev(c(7,108,214,426,532,633)) * 240/640
  targetpos <- aslpos[ runData$targetPos[tidx] ]
  accpos    <- unique(tscore$accuracy) + targetpos

  ## plot
  if(showplot){
   p<- ggplot(tscore) + 
    aes(x=time,y=horz_gaze_coord) +
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
    geom_hline(data=data.frame( y= accpos     ), aes(yintercept=y), color='red', linetype='dotted' ) +
    geom_hline(data=data.frame( y= targetpos  ), aes(yintercept=y), color='purple') +
    scale_y_continuous(limits=c(0,240)) +
    theme_bw()
 
   print(p)
  }
}

# take will's aligned dataframe (each row is xdat)
# turn into dani's dataframe
# Dani - taskData 
#  run           1-3
#  cueTime       cuexdat + 1.4
#  cueLength     delayxdattime - cuexdattime - 1.4 (long=3,short=1.5)
#  targetTime    delayxdattime 
#  targetPos     mod(delayxdat,10) (pos: 7=1,108=2,214=3,426=4,532=5,633=6)  
#  delayTime     delayxdattime
#  delayLength   endxdattime - delayxdattime - 1.4 (short=1.5 or long=9)
#  + runData
#  type          vgs or mgs
#  time          targettime if mgs, cuetime if vgs
#  startInd      where xdat is
#  xdatTime      time according to xdat index
## import to have type and startInd
# taskstarttime removes the 6s wait of discacq
# also included trial, because it's useful when plotting
willtodani <- function(adf,taskstarttime=6.0,samplerate=60) {
 # TIMING:
 #  start (.1)                           | start  XDAT [2-5]0 | 
 #  Cross+gap (1.4)   + s/l cue (3|1.5)  | target XDAT 1[2-5][1-6] |
 #  s/l delay (1.4|9) + MGSExecute (1.4) | end    XDAT 250
 # we want to start at s/l cue, and MGSExecute
 # so start + 1.4 (cross+gap)
 # and end  - 1.4 (MGSexecute)
 b<- adf %>% 
   # build need vars
   mutate(
     # xdats 
     # so target xdat 
     type      = ifelse(etypetask=='start','vgs','mgs'),
     # xdat sent after cue "+", + dur is (1.2 + .2)
     # xdat sent after MGSExecute (end), dur to comp for is 1.4
     timeadj   = ifelse(type=='vgs', + 1.4, -1.4),
     # dani has this so the start cue is 1.4
     time      = time     + timeadj - taskstarttime,
     # N.B. startInd (adjusted) is not startind (unadjusted)
     startInd  = startind + timeadj*samplerate,
     # xdattime, we use the adjusted startInd and take out the first actual index 
     # .1 for startbuffer before any task
     xdatTime  = round( (startInd - first(startind))/samplerate, 3)  + .1 
   ) %>%
   # only want start and end, no FIXes
   # we added      to get to cue onset from start
   #    subtracted to get MGSexcute onset)
   filter(etypetask %in% c('start','end'), 
         #!grepl('FIX',event), # target>0 does the same thing
         # but also kills FIX ends (which don't say FIX in the event name)
         target>0 ) %>% 
   # grab only want we need
   select(type,startInd,time,xdatTime,targetPos=target,trial=rtrial) #,xdattask,etypetask)

 # make vgs targetpos that of mgs
 # this could fail if vgs doesn't always prepceed mgs
 # it would mean something is wrong
 #b$targetPos[b$targetPos==0] <- b$targetPos[b$targetPos!=0]

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

# checkAlignment <- function(a) {
#   a
# }


getGnrcRun <- function(runno) {
  r <- setDT(read.table('runOrderXdat/all_xdats.txt'))[run==runno]
  # set "real" trial number by cumlitive counting cues that are not 60 (exclude fix)
  r$rtrial <- cumsum(r$xdat <100 & r$xdat!=60)
  r$row    <- 1:nrow(r) 
  # add targetpos: only mod10 not 0 is target e.g 152 -> 2;  50 250 60 -> 0
  #                so get max for all of the trial, set as target for whole trial
  merge(r,
        r %>% group_by(trial) %>% summarise(target=max( xdat%%10 ) ), by='trial')
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
rangeidx <-function(a) {
 if(a@score<1200) {warning('score: ',round(a@score,2),' is low; matching to wrong run?')}
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
