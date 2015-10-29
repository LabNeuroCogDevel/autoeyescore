#library(zoo)
library(dplyr)
library(tidyr)
library(data.table)
# use DNA alignment algo Needleman-Wunsch for xdat alignment
# source("http://bioconductor.org/biocLite.R")
# biocLite('Biostrings')
library(Biostrings)
#source('eyescoreFunctions.R')

### trying to fix these errors:
#  24  Error in -excludeInd                                                            
#  4   Error in `$<-.data.frame`(`*tmp*`, "type", value = c("vgs", "vgs", "vgs",  
#  27  Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) 
#  24  Error in model.frame.default(formula = xdatTime ~ matchingTrials, drop.unused.levels = TRUE) 
#  67  Error in rbind(deparse.level, ...) 
#  4   Error in read.table(file.path(path, file), head = T) 
#  48  Error in scan(file, what, nmax, sep, dec, quote, skip, nlines, na.strings,  
#  8   Error in ts(x)

getGnrcRun <- function(runno) {
  r <- setDT(read.table('runOrderXdat/all_xdats.txt'))[run==runno]
  # set "real" trial number by cumlitive counting cues that are not 60 (exclude fix)
  r$rtrial <- cumsum(run2$xdat <100 & run2$xdat!=60)
}

xdattype <- function(xdat) {
 ifelse(xdat<100,       'cue' , 
        ifelse(xdat>220,'stop',
                        'mem' ) )
}

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
 

XdatsToTask <- function(rawdata,taskData,run) {
 # only want one run
 taskdata <- taskData %>% filter(run==run) 
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
   event = xdattype(xdat),
   trial = 1+cumsum(ifelse(event=="cue",1,0)) ) %>%
 # group by event and trial 
 group_by(event,trial) %>% 
 # so we can collapse repeats
 summarise( 
    xdat    = xdat[which.max(l)],
    ls      = sum(l),
    startind= min(startind), 
    n       = n() ) %>% 
 ungroup %>% arrange(startind)

# library(ggplot2)
# ggplot(xdats) + geom_histogram(aes(x=as.factor(ls),fill=event),position='dodge')
# library(data.table)
# dcast(setDT(xdats), trail ~  event, value.var=c('n','xdat','ls','startind'))
}
doalign <- function(rawdata,runxdats) {
 rawxdat <- rle(rawdata$XDAT)$v
 #taskList <- read.table('runOrderXdat/all_xdats.txt')
 #runxdats <- setDT(taskList)[run==2,xdat]
 a<-pairwiseAlignment( pattern=num2bstr(runxdats), subject=num2bstr(rawxdat) )
 #print(  cbind( bstr2num(a@subject) , runxdats )  )
}

# take in an alignment, give back indexes of subject matching pattern
# N.B assumes no gaps in pattern
#          "ATCTCGGGGGATAT"
# pattern: [4] TCGGGGGATAT 
# subject: [1] TC----GAT-T 
# returns: 4  5 10 11 12 14
rangeidx <-function(a) {
 prange<-a@pattern@range
 srange<-a@subject@indel@unlistData
 #browser()
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
 browser()
 c(rep(NA,1,a@subject@range@start-1), setdiff( prange@start +c(1:prange@width-1), gaps))

}

