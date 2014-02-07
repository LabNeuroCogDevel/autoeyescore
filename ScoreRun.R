#TO USE:
# Score an eyd turned tsv file
#  ** NEED TO SOURCE a settings file before running this!!!
#     e.g. anti/anti.settings.R 
#
# TESTING:
#   funnybusiness ='ignoreblinks,dontcutspikes,preblinkok,useextremefilter'
#     blinkbeforeok: dont drop because there is a blink maxBlinkLengthBeforeFirstSac before first sac
#     ignoreblinks:  dont do anything with blinks
#     dontcutspikes: dont take out large isolated changes in eye postion
#     preblinkok:    dont drop because there is a blink before trial starts
#     useextremefilter: 
#
#TODO:
#NOTES: 
# accuracy is if the saccade is held
#    which is if there is no saccade in the next 100ms
# * add 'held' T/F column
# * need to check sac.minmag, suggest increasing
# * do not need sac.maxlength
# * should not merged saccades that go opposite ways -- see 004 ANTI 35
# * graph dropped trials still. can see how bad it is
# * take a meassure of polyfit's estimation, drop if off in critical time
#
#  getSacs  -- generate saccades for a run
#  scoreSac  -- score sacades 
#
#  trial, xdat, lat, fstCorrect, ErrCorr, AS, Count 
#    fstCorrect boolean, True if first saccade is the correct direction
#    ErrCorr boolean, True if there is a corretive saccade (not just to baseline)
#    AS boolean, prosaccade or antisaccade? True if AS
#    Count -- number of trials to get to a correct response
#       0 -- never correct
#       1 -- first saccade is correct
#       2 -- corrected incorrect saccade (might have been 3rd, 4th, etc, saccade)

### To avoid any conflicts with other R installs (e.g. on arnold)
###    use our own packages
library(devtools)
dev_mode(TRUE, path = "Rforsoring")

#
library(KernSmooth)
library(ggplot2)
library(gridExtra)
library(plyr)
library(zoo) # for na.approx, rollapply

#### Settings ######
saverootdir  <- 'aux'

#ASL max is 261x240
xmax         <- 274
ymax         <- 250
screen.x.mid <- 261/2    # xpos of eye, not x axis! -- this is where we expect the center fixation to be
sampleHz     <- 60

## experimental design
sac.time          <- 1.45 # how long is the target up before we see the fixation cross again? -- why isn't this in each paraigms setting

sac.trackingtresh <- 0    # what percent of a sac has to have actual samples (tacked) to be counted, set to 0 to ignore
# this is used for a rule that is stupid
#sac.firstmincoverage  <- .6 # first saccade has to be this much tracked or trial is dropped


## latancy properties
#  how fast the eye has to move before considering the movement a saccade (also heuristic overcompesating approximation)
#lat.minvel   <- 4      # ASLcoordx/60Hz , per paradigm
lat.fastest  <- 67/1000  # fastest latency allowed is 200ms

## saccade properties ##
#minium distance to be considered a saccade
#sac.minmag   <-  10      # NOW DEFINED IN each tasks settings file (eg. vgs.settings.R -- vgs is 20, everywhere else is 10)
                               
# target position thresholds
# beyond threshold == saccad to expected region


########## this was 50 --- samples are 1/60, so 3 are 50ms but approx are made at finer resolution. want to exclude 2 or less samples (33ms) so round up the half way btwn pt.
sac.minlen   <-  42/1000 # saccades less than 50ms are merged/ignored

sac.mingap   <-  50/1000 # saccades must be at mimumum this time appart, merged otherwise
sac.held     <- 100/1000 # if a sac is held -- it is accurate
sac.slowvel  <- 1        # same units as lat.minvel ASLXcord/60Hz
#sac.maxlen   <- 250/1000 # maximum length of a sac should be 250 ms # NOTE. account for merged

blink.mintimedrop  <- 100/1000
blink.trim.samples <- 2

### Plot settings
plot.endtime <- 1.75
#plot.endtime <- 4
maxBlinkLengthBeforeFirstSac <- .3

# Green:   means correct direction to the correct place
# Blue:    good dir, over or under shot position
# Orange:  wrong dir, but moved as much as should have if the direction was correct
# Red:     wrong direction, wrong magnitude
#
# TRUE FALSE is correct direction, wrong magintued of saccade -- NA's sneak in, so inluce a bunch of 'black's
positionColors<-c('TRUE TRUE'='green', 'TRUE FALSE'='blue', 'FALSE FALSE'='red','FALSE TRUE'='orange', 'black','black','black','black','black')
  
xScaleTime <- sort(c(seq(0,plot.endtime,by=.25), lat.fastest, sac.time ) )
# remove the tick that is close to sac.time -- should do this algorithmicly with diff
xScaleTime <- xScaleTime[-which(xScaleTime==1.5)]
# p is always the same
# but x limit should probably be done programticly/abstracted (170-> variable name)
p    <- ggplot() + theme_bw() + theme(legend.position="none")  + #,text=element_text(size=10,family="mono") ) +
        scale_x_continuous(limits=c(0,plot.endtime*sampleHz), breaks=xScaleTime*sampleHz, labels=xScaleTime)


## FUNCTIONS

#
#### PLOT 
#
ggplotXpos <- function(est,d,trgt,sac.df,base.val,delt.x.scale,slowpnt.x.scale,ptitle,blinkends=c()) {
  
  ybreaks= sort( c(122,sac.thresholds ) )
  g    <- p +
          scale_y_continuous(breaks=ybreaks,labels=ybreaks,limits=c(0,263) )

  if(length(est$x)>0) {
     g <- g + geom_point(data=data.frame(x=est$x,y=est$y),aes(x=x,y=y),alpha=I(.5))
  }

     g <- g + geom_point(data=d[trgt,],x=1:length(trgt), aes(y=xpos,color=as.factor(xdat),size=dil))  +
          scale_color_manual(values=c('green','red','blue','black')) + # should only ever see green -- otherwise more than one XDAT
          scale_size_continuous(limits=c(1,100),range=c(.3,5)) + # limit dilation scale to 2 std of mean (for random subj)
          geom_vline(xintercept=sampleHz*lat.fastest,color=I('red')) +
          geom_vline(xintercept=sampleHz*sac.time,color=I('red'),linetype=I('dotdash'))

  # show velocity changes and estimated base (center focus) value
  if(length(delt.x.scale)>0) {
     g <- g +
          geom_vline(xintercept=delt.x.scale,color=I('blue'),alpha=I(.3)) +
          geom_vline(xintercept=slowpnt.x.scale,color=I('yellow'),alpha=I(.3)) +
          geom_hline(yintercept=base.val,color=I('green'),alpha=I(.5))+
          geom_hline(yintercept=screen.x.mid,color=I('darkgreen'),alpha=I(.5))
   }

  # outline "good" position (with padding) in purple
  g <- g +
       geom_hline(yintercept=c(sac.thres-sac.padding,sac.thres+sac.padding),
                  color=I('purple'),alpha=I(.2)) +
       ggtitle(ptitle) + ylab('left to right')+xlab('')
  
  # put colored boxes around saccades
  if(!is.null(sac.df) && dim(sac.df)[1]>=1){
     g <- g +
           geom_rect(data=sac.df, aes(xmin=onset*sampleHz,xmax=end*sampleHz,
                                      ymin=-Inf,    ymax=Inf,
                     fill=as.factor(paste(cordir,corpos)), alpha=intime&gtMinLen  )) + 
           scale_fill_manual(values=positionColors) +
           scale_alpha_manual(values=c('TRUE'=.5,'FALSE'=.2))
  } 

  # put in blinkends if we have any
  if(!is.null(blinkends) && length(blinkends)>0) {
   g<- g + geom_vline(xintercept=blinkends,color=I('green'),linetype=I('dotdash'))
  }

  return(g)
} 

## deriviatives
ggplotDrv <- function(fst,scnd,slowpnt.x.scale,delt.x.scale) {
  
  drv  <- p + theme(legend.position="none" ) +
           geom_point(data=data.frame(x=scnd$x,y=scnd$y),aes(x=x,y=y),alpha=I(.3),color=I('green')) +
           geom_point(data=data.frame(x=fst$x, y=fst$y), aes(x=x,y=y),alpha=I(.6),color=I('blue')) +
           geom_hline(yintercept=c(-lat.minvel, lat.minvel),color=I('purple'),alpha=I(.2)) +
           geom_vline(xintercept=slowpnt.x.scale,color=I('yellow'),alpha=I(.7)) +
           geom_vline(xintercept=delt.x.scale,color=I('blue'),alpha=I(.7)) + xlab('seconds') +ylab('') +
           scale_y_continuous(limits=c(-10,10))
  
  
  return(drv)
} 

savePlots <- function(sac.df,g,drv,filename,writetopdf) {

  
  # only write to pdf if we are told to
  if(writetopdf) {
     outputdir<-dirname(filename)
     if(!file.exists(outputdir)) { dir.create(outputdir,recursive=T) }

     cat('write to', filename,"\n")
     pdf(filename,height=6, width=15)
  }
  else {
   print('not saving')
   x11()
  }
  
  # if we have saccades, show a table
  if(length(sac.df)>0){
      print (
        grid.arrange( nrow=3,heights=c(.5,1,.4),
                tableGrob( sac.df[,-c(1:3)],gpar.coretext = gpar(fontsize=6),gpar.coltext=gpar(fontsize=8)),  # exclude index values
                 g,
                 drv
           ) 
      )
  }
  # otherwise just show the eye data
  else{
     print(g)
  }
  
  if(writetopdf) {
   dev.off()
  }
  #grid.newpage()
}



### DROP A TRIAL
dropTrialSacs <- function(subj,runtype,trl,xdatCode,reason,allsacs,showplot=F,savedas=NULL,writetopdf=F,run=0,rundate=0) {
   cat(sprintf('DROP: %s.%s.%s.%s %s\n',subj,rundate, run, trl,reason))

   ## write what is dropped
   outputdir=saverootdir;
   if(!file.exists(outputdir)) { dir.create(outputdir,recursive=T) }
   for(trial in trl) {
      cat(file=sprintf('%s/%s.%s.%s.%s.dropped.txt',outputdir,subj,rundate,run,trial),
          sprintf("%s\n",reason))
   }

   if(showplot==T){
      ptitle <- paste(subj,runtype, trl,xdatCode, paste('DROPPED!', reason) )
      g    <- ggplotXpos(est=NULL,d,trgt,sac.df=NULL,base.val=NULL,delt.x.scale=NULL,slowpnt.x.scale=NULL,ptitle)
      #g    <- g + theme(legend.position="top") , causes error

      # set and make if ness. output directory for plotsj
      if(is.null(savedas)){ 
        outputdir <- paste(saverootdir, subj,paste(run,runtype,'DROPPED',sep=".") ,sep="/")
      } else {
        outputdir <- paste(dirname(savedas),'img',sep='/')
      }
      if(!file.exists(outputdir)) { dir.create(outputdir,recursive=T) }
      
      filename<-paste(sprintf("%02d",trl),xdatCode,sep="-")
      filename<-paste(outputdir,'/', filename,'.pdf', sep="")
      if(!file.exists(outputdir)) { dir.create(outputdir,recursive=T) }

      savePlots(sac.df=NULL,g,drv=NULL,filename,writetopdf)
   }

   # return empty data frame -- no sacs

   droppedTrial <- data.frame(
         subj=subj,run=run,trial=trl,
         onset=NA,slowed=0,end=0,startpos=0,endpos=0,corside=F,cordir=F,corpos=F, # combined=F,
         held=F,gtMinLen=F, intime=F,p.tracked=0, xdat=xdatCode, maxpos=NA,minpos=NA,
         crossFix=F,MaxMinX=F,gtMinMag=F, startatFix=F, distance=NA, reason=reason
         )

  if(all(is.null(allsacs)) || all(is.na(allsacs)) || dim(allsacs)[1] < 1 ) {
    allsacs <- droppedTrial
   } else {
    allsacs <- rbind( allsacs, droppedTrial)
   }
   return(allsacs)
}

parseRawForTargets <- function(eydfile, funnybusiness=''){
  # reset d
  d <<-data.frame(xdat=NA,dil=NA,xpos=NA,ypos=NA)

  cat('using ', eydfile ,'\n')

  ### load data
  #load xdat, eye position and dialation data for parsed eydfile
  readeydsuccess <- tryCatch( { d        <<- read.table( eydfile, sep="\t",header=T) },error=function(e){cat('error! cant read input\n')})

  if(is.null(readeydsuccess)) {
    #allsacs <- dropTrialSacs(subj,runtype,1:expectedTrialLengths,0,'no data in eyd!',
    #                     allsacs,showplot=F,run=run,rundate=rundate)
    #return(allsacs) 
    return('no data in eyd!') 
  }


  if(dim(d)[2] != 4) {
    #allsacs <- dropTrialSacs(subj,runtype,1:expectedTrialLengths,0,sprintf('eyd data does not make sense to me %dx%d',dim(d)[1],dim(d)[2]),
    #                     allsacs,showplot=F,run=run,rundate=rundate)
    #return(allsacs) 
    return('eyd data does not make sense!')
  }


  names(d) <<- c("xdat","dil","xpos","ypos")

  ## fix bars zero'ed xdats
  zeroxdat        <- rle(d$xdat==0)
  
  if(zeroxdat$values[1] == T) { 
    minzeroxdatlen <-2 
  } else {
    minzeroxdatlen <-1
  }

  if(length(zeroxdat$lengths)>minzeroxdatlen) {
     xdatswitchidx   <- cumsum(zeroxdat$lengths)
     zeroxdatidx     <- which(zeroxdat$values==TRUE)

     #if the very first xdat is 0, we're in trouble, so ingore that one
     if(zeroxdatidx[1] == 1) {
        zeroxdatidx <- zeroxdatidx[-1]
     }

     #replacementXdat <- d$xdat[xdatswitchidx[zeroxdatidx-1]]
     #needReplaced    <- d$xdat[xdatswitchidx[zeroxdatidx]:(xdatswitchidx[zeroxdatidx]+zeroxdat$lengths[zeroxdatidx])]
     zerostartend    <- cbind(  end=xdatswitchidx[zeroxdatidx], 
                              start=(xdatswitchidx[zeroxdatidx]-zeroxdat$lengths[zeroxdatidx]+1)
                             )

     zeroIdxinD      <- apply(zerostartend,1,function(x){x['start']:x['end']})
     shouldbeXdat    <- d$xdat[xdatswitchidx[zeroxdatidx-1]]

     if(length(shouldbeXdat) != length(zeroIdxinD) ) warning('zero xdats and replacement not the same length!! something very funny is going on')
     for( i in 1:length(shouldbeXdat)){ d$xdat[zeroIdxinD[[i]]] <<- shouldbeXdat[i]}
  }

  
  ##### Remove bad data #####
  # both x and y are out of range, or there is no dilation measure
  # TODO!!
  # if there is x and ypos and dil != 0
  # maybe we should keep?

  # see scannerbars 10711.20121108.1.17 -- good xpos, no good dil or ypos
  
  if(any(grepl('useextremefilter',funnybusiness))){
   badidxs=d$xpos>xmax|d$ypos>ymax|d$dil==0
  }else{
   # allow for some over/under shooting (xmax+50->xmax, -50 -> 0)
   allowedovershoot <- 0
   d$xpos[d$xpos>xmax&d$xpos<xmax+allowedovershoot] <<- xmax
   d$xpos[d$xpos<0&d$xpos>-allowedovershoot] <<- 0

   badidxs=d$xpos>xmax|(d$xpos==0&d$ypos==0&d$dil==0)
  }
  d[which(badidxs),c('dil','xpos','ypos')] <<- c(NA,NA,NA)
  if(any(na.omit(d$dil)<1)) { d$dil[which(d$dil<1)]<<-1 }# so we can see something! 
  # which is need to not die on scanbars 10656.20090410.2
  
   # remove repeats
  xdats             <- rle(d$xdat)            # run length encode
  xdats$cs          <- cumsum(xdats$lengths)  # index in d where xdat happends
  
  #### Split eye positions by target code
  # use only targetcodes that are preceded by startcodes and followed by end codes
  # this should let us ingore  catch trials (no target)
  tarCodePos       <- which(xdats$values %in% targetcodes)
  goodTargPos      <- tarCodePos[  xdats$values[tarCodePos  - 1]  %in%  startcodes ]
  goodTargPos      <- goodTargPos[ xdats$values[goodTargPos + 1]  %in%  stopcodes  ]
  
  # x by 2 matrix of target onset and offset indicies
  targetIdxs       <- cbind(xdats$cs[goodTargPos-1],xdats$cs[goodTargPos])


  if(length(goodTargPos) <= 0) {
    #allsacs <- dropTrialSacs(subj,runtype,runontrials,0,'no understandable start/stop xdats!',allsacs,showplot=F,run=run,rundate=rundate)
    #return() 
    return('no understandable start/stop xdats!')
  }

  if(! length(goodTargPos) %in% expectedTrialLengths ) {
    cat('WARNING: unexpected num of trials: ', length(goodTargPos),'\n')
  }

  return(targetIdxs)
}

## need what trial to work on
## assume "d" and targetIdxs are populated
## export b.orig for plotting
## return b.approx
interoplateSamples <- function(trl, funnybusiness=''){
    # seems okay because:
    #print(head(d$xdat[ targetIdxs[trl,1]:targetIdxs[trl,2] ]))
    
    # maybe not the best place, but we should check targetIdx length
    # targetIdxs has start and stop, diff is length
    if(is.na(trl)){ stop('why is trl na?!') }
    numTargetCodes<-diff(targetIdxs[trl,])
    ##TODO: THIS 85 SHOULD NOT BE HARDCODED
    # it is the min number of samples accepted for a target code  
    # # replaced with 85 with 55 while looking at anti
    if(numTargetCodes < 55 ) cat(sprintf('WARNING: have only %d samples of the target codes\n',numTargetCodes))


    
    # grab the eye data for this trials traget code
    #trgt <-targetIdxs[trl,1]:targetIdxs[trl,2] 
    #print(trl)
    trgt <<-(targetIdxs[trl,1]+1):targetIdxs[trl,2]  # +1 to skip the last cue xdat code

    # test for tracking before target onset
    # we want to drop (b/c lat will be useless) if they blinked for more than .2s within .3s of target onset
    # FIX FIX FIX
    pretargetIDX <- trgt[1]-.3*sampleHz + 1:(.3*sampleHz)
    if (any(pretargetIDX<1)) {
     #allsacs <- dropTrialSacs(subj,runtype,trl,xdatCode,'weird! no eye position data before trial onset! cannot tell if pretrial blink',allsacs,showplot=F,run=run,rundate=rundate)
     #next
     return('no eye position data before trial onset! pretrial blink?')
    }

    pretargetD   <- d[pretargetIDX, ] 
    if(
      length(which(is.na(pretargetD$xpos))) > .2*sampleHz &&
      !any(grepl('preblinkok',funnybusiness))
    ){
     #allsacs <- dropTrialSacs(subj,runtype,trl,xdatCode,'blink before target onset',allsacs,showplot=showplot,run=run,rundate=rundate)
     #next
     return('blink before target onset')
    }


    ## get eye tracking for relevant portion
    b.orig     <<- data.frame(time=1:length(trgt),x=d$xpos[trgt]) 

    # fill in the NAs with approximations
    # using first method that came to mind (old/bad)
    b.nona<- na.omit(b.orig)
    #b.all <-b.nona
    #names(b.all) <- c('x','y')
    # plot which points are used by changing the shape
    #b.all$used <- T


    ###### BLINK
    # scary blink fliter -- remove 2 samples on each side of an NA seq that is longer than 10 samples (1/6 sec)
    naSeq <- rle(is.na(b.orig$x))
    SeqStartIdx <- cumsum(naSeq$lengths) - naSeq$lengths + 1
    naStartIdx <- which( naSeq$values == T & naSeq$lengths > 10 ) 

    nastarts <- SeqStartIdx[naStartIdx]-blink.trim.samples
    nastarts[nastarts<1] <- 1

    nastops  <-  SeqStartIdx[naStartIdx]+ naSeq$lengths[naStartIdx]+blink.trim.samples 
    nastops[nastops>length(b.orig$x)] <- length(b.orig$x)

    start10nas <- unname(unlist(alply(cbind(nastarts,nastops),1, function(x) { x[1]:x[2]} )))
    b.cutblink <- b.orig
    b.cutblink$x[start10nas] <- NA
    #b.cuttblink$used[start10nas] <- F


    b.approx <- b.cutblink;

    ###### Remove crazy points that can be nothing other than tracking errors
    # eg, xpos like: 10 10 *200* 10 10; see behbars: 10827.20100617.1.34
    if(!any(grepl('dontcutspikes',funnybusiness))){
      e<-length(b.approx$x)
      xposΔ <- c(0,diff(b.approx$x));
      # change is different on both sides (spike) and is greater than threshold
      spikypointsIdx <- which(rollapply(xposΔ,2,function(x){sign(prod(x,na.rm=T))==-1 & abs(min(x,na.rm=T))>30}))

      b.approx$x[spikypointsIdx ] <- NA
      #b.approx$used[start10nas] <- F
    }

    

    ###### TRIM
    # drop all NAs at the end (b/c they can't be estimtated)
    naX   <- which(is.na(b.cutblink$x))

    lastNA <- ifelse(length(naX)==0, 0 , lastNA <- naX[length(naX)] )



    # trim last 
    while(length(b.approx$x) == lastNA && length(b.approx$x)>0) {
     # find all the NAs at the end
     NAreps   <- tail(rle(diff(naX))$lengths,n=1) 

     if( length(NAreps)>=1 & !is.na(NAreps[1])) {
       dropidxs <- seq( lastNA - NAreps,lastNA) 
     } else { 
       dropidxs <-  lastNA 
     }

     b.approx <- b.approx[-dropidxs,]

     naX   <- which(is.na(b.approx$x))
     lastNA<-ifelse(length(naX)==0,  0, naX[length(naX)] )
    }

    naX     <- which(is.na(b.approx$x))
    firstNA <-ifelse(length(naX)==0,  0, naX[1] )
    # trim first
    while(firstNA == 1) {
     # find all the NAs at the end
     NAreps   <- rle(diff(naX))$lengths[1]

     if(length(NAreps)>=1 & !is.na(NAreps[1]) ) { 
       dropidxs <- seq( firstNA , NAreps+1)
     } else { 
       dropidxs <- firstNA               
     }
     b.approx <- b.approx[-dropidxs,]
     naX   <- which(is.na(b.approx$x))
     firstNA <-ifelse(length(naX)==0,  0, naX[1] )
    }

    ###### CHECKS
    # if we take out the NAs and there is nothing left!
    if(length(b.approx$x)<=1){
     #allsacs <- dropTrialSacs(subj,runtype,trl,xdatCode,'no data',allsacs,showplot=showplot,run=run,rundate=rundate)
     #next 
     return('no data left after removing blinks')
    }



    ####### Estimate away NAs
    # replace NAs so we can do polyfit
    b.approx <<- b.approx
    b.all <- b.approx
    b.all$x <- na.approx(b.approx$x,x=b.approx$time) 
    
    names(b.all) <- c('x','y') # bad code elsewhere wants x and y


    return(b.all)
}

# returns 'allsacs' a list of all saccades in each trial
getSacs <- function(eydfile, subj, run, runtype,rundate=0,onlyontrials=NULL,writetopdf=F,savedas=NULL, showplot=F, funnybusiness=""){
  
  # this inputs might not come in as numbers
  subj   <-as.numeric(subj)
  run    <-as.numeric(run)
  rundate<-as.numeric(rundate)

  # setup dataframe to hold sacades from all trials
  allsacs     <- data.frame()

  ## updates "d" in global scope
  # returns a fail reason or targetIdxs
  targetIdxs <<- parseRawForTargets(eydfile,funnybusiness)

  if(is.character(targetIdxs)) {
    allsacs <- dropTrialSacs(subj,runtype,1:expectedTrialLengths,0,samples,
                         allsacs,showplot=F,run=run,rundate=rundate)
    return(allsacs) 
  }

  
  # if onlyontrials is specified, we only want to look at those/that trial
  # but we want to keep onlyontrials null/not null for write.csv check later
  if( is.null(onlyontrials) ) { runontrials <- 1:dim(targetIdxs)[1] }
  else                        { runontrials <- onlyontrials }

  for(trl in runontrials ) {
    #print(c(trl,length(runontrials)))
    #trl  <- 8
    #print(trl)
    


    nrow(targetIdxs)
    if(trl>nrow(targetIdxs)){
     #cat('BAD TRIAL NUMBER ', trl, ' only have ', nrow(targetIdxs), ' trials')
     allsacs <- dropTrialSacs(subj,runtype,trl,0,'no trial in data',allsacs,showplot=F,run=run,rundate=rundate)
     next
    }
    # target code xdat is a little past where target index starts
    xdatCode <- d$xdat[ targetIdxs[trl,1] + 1 ]
    # the code is after the startcode but before stopcode (between targetidx[,1] and [,2] 
    # before was taking the 15th to make sure past any repated start xdat??
    # now only using 1 ahead

    # getExpPos comes from *.settings.R -- so does sac.thresholds (shouldn't have specified this, but thats how it is now :(  )
    # where the eye should look (opposite of dot if AntiSac, loc of dot if pro. sac)
    ## export to global scopefor plotting
    sac.thres <<- getExpPos(sac.thresholds,xdatCode)

    #expected  mag and direction of saccade
    sac.expmag <<- sac.thres - screen.x.mid
    sac.expdir <<- sign(sac.expmag)
    sac.expmag <<- abs(sac.expmag)


    # have "d" and "targetIdxs" in global scope
    # will export b.orig and b.approx too
    # returns "b" (time,xpos) with blinks removed
    #   or reason for falure
    b.all <- interoplateSamples(trl,funnybusiness)
    if(is.character(b.all)){
     failreason <- b.all
     allsacs <- dropTrialSacs(subj,runtype,trl,xdatCode,failreason,allsacs,showplot=showplot,run=run,rundate=rundate)
     next
    }



    # sanity check -- do we really only have target Idxs?
    obsvTrgCode <- unique(d$xdat[trgt])
    if( length(obsvTrgCode) != 1 || ! obsvTrgCode %in% targetcodes ){
     cat(subj,runtype,trl,xdatCode, 'funky trial codes! ',obsvTrgCode ,"\n")
    }

    # check tracking coverage for  "samples of interest"
    SOI.expect <- (sac.majorRegionEnd - lat.fastest)*sampleHz
    SOI.actual <- length(which(b.all$x/sampleHz < sac.majorRegionEnd & b.all$x/sampleHz > lat.fastest) )
    #print(c(SOI.actual,SOI.expect))
    #print(b.all)
    if(SOI.actual < SOI.expect*.30 && !any(grepl('soipercent',funnybusiness))) {
     allsacs <- dropTrialSacs(subj,runtype,trl,xdatCode,'< 30% tracking for samples of interest',allsacs,showplot=showplot,run=run,rundate=rundate)
     next 
    }

    
    #TODO: determine baseline and sac.* iteratively
    #      error out if cannot be done
    #base.idx <- which(est$y < sac.right.small & est$y > sac.left.small & abs(fst$y) < lat.minvel )
    #base.val <- mean(est$y[base.idx])
    base.val <- mean(d[ c(-5:2) + targetIdxs[trl,1], 'xpos' ],na.rm=T)

    if(is.nan(base.val) || abs(base.val-screen.x.mid )>50) {
     allsacs <- dropTrialSacs(subj,runtype,trl,xdatCode,
                   sprintf('average fixpos(%f) is off from baseline(%f)!',base.val,screen.x.mid ),
                   allsacs,run=run,showplot=showplot,rundate=rundate)
     next;
    }

    # is there big movement before the start?
    # don't do this for fix or anti (maxSamp... = 99, bars scanbars=2)
    averageChangeBeforePhysOnset <- abs(na.omit(d$xpos[trgt[1:(lat.fastest*sampleHz) ]  ])- base.val)
    numtoofarfrombaseline <- length(which(averageChangeBeforePhysOnset > sac.minmag  ) )
    if(numtoofarfrombaseline> maxSamplesFromBaseline) {
     allsacs <-  dropTrialSacs(subj,runtype,trl,xdatCode,
                     sprintf('%.0f samples > %.0f px (%.2f max) from %.2f (base)',
                           numtoofarfrombaseline,
                           max(averageChangeBeforePhysOnset),
                           sac.minmag,base.val),
                     allsacs,run=run,showplot=showplot,rundate=rundate)
     next
    }

    # is tracking smooth?
    xposStdDevMax <- 40
    averageChange <- sd(abs(diff(na.omit(d$xpos[trgt[3:(sac.majorRegionEnd*sampleHz) ]  ]))))
    if(is.na(averageChange) || (averageChange > xposStdDevMax && !any(grepl('ignorexpossd',funnybusiness)) )   ) {
     allsacs <-  dropTrialSacs(subj,runtype,trl,xdatCode,
                     sprintf('poor tracking: sd of xpos Δ=%f',averageChange),
                     allsacs,run=run,showplot=showplot,rundate=rundate)
     next
    }

   # target codes didn't match, we ate the whole file
   # or way too many
   # 200 might be too low of a threshold
   if (   length(b.all$x ) > 200 ) {
     dropTrialSacs(subj,runtype,trl,xdatCode,paste('too many samples in trial', dim(b.all$x)[1]),allsacs,showplot=showplot,run=run,rundate=rundate) 
     next 
    }
    
    
    # fit to local polynomial (with guassain kernel)
    # NOTE: using bandwidth>1
    #browser()
    est  <- locpoly(b.all$x,b.all$y,bandwidth=1,drv=0)
    fst  <- locpoly(b.all$x,b.all$y,bandwidth=1,drv=1)
    scnd <- locpoly(b.all$x,b.all$y,bandwidth=1,drv=2)
    

    # catch movement before actual onset 
    # fst$x is in samples, we want the y value before the sample capturing closest time a sac can be made
    # avoid points up to the first sample to all things to settle
    # fst$x starts at 1, 1.9 is enough time to settle down from initial
    maxBeforeOnset <- max(abs( fst$y[fst$x< lat.fastest*sampleHz & fst$x> 1.9 ] ),na.rm=T)
    # sac.slowvel is probably 1px/60Hz
    # lat.minvel  is probably 4px/60Hz
    if( (is.nan(maxBeforeOnset)     || 
         abs(maxBeforeOnset) == Inf || 
         maxBeforeOnset >lat.minvel   ) && !any(grepl('highvelstartok',funnybusiness))
    ){
     allsacs <- dropTrialSacs(subj,runtype,trl,xdatCode,
                 sprintf('moving (%.3f px/60Hz) before target onset',maxBeforeOnset),
                 allsacs,showplot=showplot,run=run,rundate=rundate)
     next
    }


    # run length encode when the change in sacad postion is faster
    # than the min velocity for a saccade
    # ** First sacade attribute

    rlePastDrv <- rle(abs(fst$y)>lat.minvel & !is.na(fst$y))
    delt.x <- cumsum( rlePastDrv$lengths )

    # when does the direction change?
    #nsamp <- length(fst$x)
    #idxDirChange <-  which( c(F, sign(fst$y[1:(nsamp-1)]) !=  sign(fst$y[2:nsamp]) )  )
    
    # where they are moving, but less than requried for a saccade
    slowpoints<-rle(abs(fst$y)< sac.slowvel)
    slowpntIdx <- cumsum(slowpoints$lengths)

    
    # stages of motion
    # where has the subject started to move, made an actual saccade, slowed down, and stopped moving
    startUp     <- slowpntIdx[slowpoints$values==T]
    speedingUp  <- delt.x[rlePastDrv$values==F]
    slowingDown <- delt.x[rlePastDrv$values==T]
    slowedDown  <- slowpntIdx[slowpoints$values==F]
    
    
    # how do those match up
    sac.df <- data.frame(
               # saccade started
               onsetIdx  = speedingUp,
               # find matching slowDowns
               slowedIdx = sapply(speedingUp, function(x){ slowingDown[which(x<slowingDown)[1]] }),
               # find the first place they slow down after speeding up
               endIdx    =  sapply(speedingUp, function(x){ slowedDown[which(x<slowedDown)[1]] })
    )



    # remove saccades that haven't finished (end usually doesn't matter)
    # could probably do the same thing by which(df$onset < sac.time)
    noEndIdx <- which(is.na(sac.df$endIdx+sac.df$slowedIdx))
    if(length(noEndIdx)>0) { sac.df <- sac.df[ -noEndIdx, ] }
    
    nsacs      <- dim(sac.df)[1]

    ###### DROP TRIAL CONDITIONS
    # no saccades, also will trap no enough data
    if( nsacs<1  ){
     allsacs <- dropTrialSacs(subj,runtype,trl,xdatCode,'no saccades (getSacs)',allsacs,run=run,showplot=showplot,rundate=rundate)
     next
    }


    # actual time from target cue
    # est$x is in 60Hz samples, but est indexes are not!
    sac.df$onset  = est$x[sac.df$onsetIdx]/sampleHz
    sac.df$slowed = est$x[sac.df$slowedIdx]/sampleHz
    sac.df$end    = est$x[sac.df$endIdx]/sampleHz
    

    # remove sacs that are too far away
    sac.df    =  sac.df[which(sac.df$onset < sac.time), ]
    nsacs      <- dim(sac.df)[1]
    # test again for no sacs
    if( nsacs<1  ){
     allsacs <- dropTrialSacs(subj,runtype,trl,xdatCode,sprintf('no saccades within sac.time(%.3f)',sac.time),
                          allsacs,run=run,showplot=showplot,rundate=rundate)
     next
    }

    ####
    #### BLINKS GO TO START of SAC
    ####
    NArle <- rle(is.na(b.approx$x))
    NArlecs <- cumsum(NArle$lengths)/sampleHz
    actualblinkidx     <- NArle$values==T & NArle$lengths/sampleHz > blink.mintimedrop 
    eyeidx.blinkstart  <- NArlecs[which(actualblinkidx)-1]*sampleHz
    blinkends <- NArlecs[ actualblinkidx ]


    if(length(blinkends)==0){blinkends <- Inf}

    # if a sac starts with a blink, add that blink to the start
    # NB!!! onsetidx, min and max are now incorrect!!!
    if(!any(grepl('ignoreblinks',funnybusiness))){
       sac.df$onset <- sapply(sac.df$onset,
               function(x){ 
                    a=x-blinkends
                    bidx=which(a<2/sampleHz&a>0)
                    newstart=NArlecs[which(actualblinkidx)-1][bidx]
                    #TODO: newstart should be null if this change doesn't change x position
                    if(length(newstart)>0 && x-newstart < .5 ){ 
                     newstart + blink.trim.samples/sampleHz 
                    }else{
                     x
                    } 
               })
    }

    
    #####
    ##### OVERLAPS
    #####

    # for blink then overlap, see
    # scannerbars.10128.20080925.3
    
    # merge sacs that are too close together
    ## look for overlap, set end info of overlapping to first
    ## remove overlap
    ## may have trouble with many overlapping
    overlapSac <-  c(F, sac.df$onset[2:nsacs] - sac.df$end[1:(nsacs-1)] < 0)
    if(nsacs<2) { overlapSac <- F}
    # TODO: don't merge saccades that go opposite ways? 
    #      ... & sign($endpos-$startpos) == sign($endpos-$startpos)
    #      ... end[2:nsacs] = start[1:(nsacs-1)]

    #sac.df$combined <- overlapSac
    
    if(any(overlapSac) ) {
      overlapSac <- which(overlapSac)
      # merge into one
      #needReplaced <-c('slowedIdx','endIdx','slowed','end','combined')
      #sac.df[overlapSac-1,needReplaced] <- sac.df[overlapSac, needReplaced]
      #sac.df<-sac.df[-overlapSac,]
      #nsacs<-dim(sac.df)[1]

      # move end of first to beginning of second
      endAttr   <- c('endIdx','end')
      startAttr <- c('onsetIdx','onset')
      mid      <- sac.df[overlapSac,startAttr]
      sac.df[overlapSac-1, endAttr  ] <- mid
      sac.df$slowed[overlapSac-1] <- NA
      sac.df[overlapSac,   startAttr] <- mid
    }
    
    
    # position
    sac.df$startpos  = est$y[sac.df$onsetIdx]
    sac.df$endpos    = est$y[sac.df$endIdx]

    sac.df <- cbind( sac.df,
            ddply(sac.df,.(onsetIdx, endIdx),function(x){
             a=est$y[x$onsetIdx:x$endIdx]; 
             c(maxpos=max(a),minpos=min(a))}
            )[,c('maxpos','minpos')]
   ) 
    # T/F saccade attributes
    # direction, position, length(time), and before next fixation
    if(nsacs>1){
       sac.df$held      = c(sac.df$onset[2:nsacs] - sac.df$end[1:(nsacs-1)] >= sac.held, T) 
     } else {
       sac.df$held      = 1
     }

    #browser()
    sac.df$cordir    = sign(sac.df$endpos - sac.df$startpos) == sac.expdir
    sac.df$corpos    = abs(sac.df$endpos - sac.thres) <= sac.padding
    sac.df$corside   = sign(sac.df$endpos - base.val ) == sign(sac.thres - base.val)
    sac.df$gtMinLen  = sac.df$end - sac.df$onset > sac.minlen
    sac.df$intime    = sac.df$onset < sac.time & sac.df$end > lat.fastest
    # first clause of intime (within xdat, isn't needed)
    sac.df$distance  = sac.df$endpos - sac.df$startpos 
    sac.df$crossFix  = as.numeric(sac.df$startpos < base.val) - as.numeric(sac.df$endpos < base.val)
    # want to test against both the base.val (where we think center is) and screen.x.mid (where center fix should be)
    #sac.df$MaxMinX   = as.numeric(sac.df$minpos < min(base.val,screen.x.mid)) - as.numeric(sac.df$maxpos < max(base.val,screen.x.mid))
    sac.df$MaxMinX   = sign(sac.df$minpos - base.val) != sign(sac.df$maxpos - base.val)
    sac.df$gtMinMag  = abs(sac.df$distance) > sac.minmag
    sac.df$startatFix= sac.df$startpos > base.val -10 & sac.df$startpos < base.val + 10
    #  0 if sac did not cross sides
    # -1 moved to the left
    #  1 moved to the right

    
    #sac.df$p.tracked = length(which( !is.na(d$xpos[trgt][sac.df$onsetIdx:sac.df$endIdx]) ) )/(sac.df$endIdx - sac.df$onsetIdx +1)
    sac.df$p.tracked  = apply(sac.df,1,
                         function(x){  
                          idxs <- round(fst$x[ x['onsetIdx']]) : round(fst$x [ x['endIdx'] ])
                          length(which(  !is.na(d$xpos[trgt][idxs]) ))/length(idxs)   
                         })
    #print(dput(sac.df))

    #sac.df$ltMaxLen  = sac.df$end - sac.df$onset < sac.maxlen # don't care how long it is
    
    
    # scale to original
    delt.x.scale    <- est$x[delt.x] # * length(trgt)/length(fst$x))
    slowpnt.x.scale <- est$x[slowpntIdx]

    slowcnt <- length(which(slowpntIdx<sac.majorRegionEnd*sampleHz))
    # normal seems to be around 7
    # 2 would be perfect (e.g. start going up, slow once at top)
    if(slowcnt > 8) { cat(subj,run,trl,"WARNING: unusual number of velc. changes",slowcnt ," poor tracking?\n") }
    
    ##TODO?? not a saccade (we care about) if motion is back to baseline
                          



    # BLINK DROP
    # drop if there is a long blink that ends before any good sacade begins
    # TODO: rename variables to someting saine
    firstgoodsacidx <-  sac.df$intime & sac.df$gtMinLen & sac.df$gtMinMag & sac.df$p.tracked>sac.trackingtresh
    firstsacstart <- sac.df[firstgoodsacidx, ]$onset[1]
    if(is.na(firstsacstart)){firstsacstart <- -Inf}



    if(any(blinkends < firstsacstart ) & !any(grepl('ignoreblinks',funnybusiness))){
     # quick way to see if blink is held
     # if xpos is more than 5 px from any other
     unheldblinks <- unlist(lapply(which(blinkends<firstsacstart),
       function(x){
        # before and after blink ( with a two samples give for blink junk)
        samples <- round(sac.held*sampleHz)
        if(x<samples) { 
          origIdxesBeforeBlink <- c() 
        } else {
          origIdxesBeforeBlink <- eyeidx.blinkstart[x]+c(-samples:-1)
        }
        origIdxesAfterBlink <- blinkends[x]*sampleHz+c(1:samples)
        position <- b.orig[c(origIdxesBeforeBlink,origIdxesAfterBlink) ,'x']
        cat(blinkends[x] - eyeidx.blinkstart[x]/sampleHz,"\n")
        max(position)-min(position) > sac.minmag |         # positions of start and stop are far away
         blinkends[x] - eyeidx.blinkstart[x]/sampleHz > .5 # sac is too long
        # should let scannerbars 10701.20110323.2.30 pass, doesn't
     }))



     
     # this checks that there isn't an immediate acceleration after the blink
     # ... but the blink may be part of an acceleration and the start might be before
     # so we should check the first derv
     #unheldblinks <- any( sapply(blinkends[blinkends<firstsacstart], function(x){abs(x*sampleHz-startUp)<10}) )
     #unheldblinks <- any(sapply(blinkends[blinkends<firstsacstart],
     #                      function(x){
     ##                       abs(fst$y[x]*sampleHz-startUp)<10
     #                        max(fst$y[which(fst$x - x*sampleHz > 0)[1:20]]) > sac.slowvel
     #                   }) )
     if(is.na(unheldblinks) || any(unheldblinks)) {
        allsacs <-  dropTrialSacs(subj,runtype,trl,xdatCode,'blink ends before any saccades',allsacs,run=run,showplot=showplot,rundate=rundate)
        next
     }

     ## after the blink (or loss of tracking) the blink is held
     ## move back the onset of a sac to the start of the closest blink onset
     #firstsacstart[firstsacstart<]
     # if actualbinkidx has a 1, this will fail, but hopefully that is a preblink and is cut anyway



    }

    ## unheld blink before first saccade
    ## added 20131217 - WF
    # * we'll check that a blink doesn't start some threshold before the first saccade
    # * see bars getSacDot('10872.20131129.1.55') and .54 
    #    - these are correct saccades, but cant be distinquished from a blink
    if(length(eyeidx.blinkstart)>0 
       && nrow(sac.df) > 0
       && length(eyeidx.blinkstart)>0
       && !any(grepl('blinkbeforeok',funnybusiness))
       && !any(grepl('ignoreblinks',funnybusiness))){

      firstsacstart <- sac.df$onset[1]
      firstblinkbeforesac <- firstsacstart - eyeidx.blinkstart[1]/sampleHz;
      if( firstblinkbeforesac  > maxBlinkLengthBeforeFirstSac){
        allsacs <-  dropTrialSacs(subj,runtype,trl,xdatCode,
                      sprintf('blink starts %.3f before any saccade',firstblinkbeforesac),
                      allsacs,run=run,showplot=showplot,rundate=rundate)
        next
      }
    }

    

    
    #cat('mean', base.val , '\n')

    # output dir to save images/write dropped
    if(is.null(savedas)){ 
       #outputdir <- paste(saverootdir,paste(subj,rundate,run,trl,sep=".") ,sep="/")
       #outputdir <- sprintf("%s/%d.%d.%d.%d",saverootdir,subj,rundate,run,trl)
       outputdir <- saverootdir
    } else {
       outputdir <- dirname(savedas)
    }
 
    ## PLOT -- only if we are told to
    if(showplot | writetopdf) {
       ptitle <- paste(subj,runtype, trl,xdatCode)
       g    <- ggplotXpos(est,d,trgt,sac.df,base.val,delt.x.scale,slowpnt.x.scale,ptitle,blinkends=c(eyeidx.blinkstart,blinkends*sampleHz))
       drv <- ggplotDrv(fst,scnd,slowpnt.x.scale,delt.x.scale)

       # write out plot
       #filename<-paste(sprintf("%02d",trl),xdatCode,sep="-")
       #filename<-paste(outputdir,'/img/', filename,'.pdf', sep="")
       filename<-sprintf('%s/img/%d.%d.%d.%02d-%d.pdf',outputdir,subj,rundate,run,trl,xdatCode)

       savePlots(sac.df,g,drv,filename,writetopdf)
    }

    allsacs <- rbind(allsacs,
                   data.frame(
                     subj=subj,run=run,trial=trl,
                     sac.df[,-c(1:3)], xdat=xdatCode,reason=NA
                   )
                )
  }


  # only write to file if we have processed all
  # which means we didn't specify onlyontrials when calling the function
  if(is.null(onlyontrials) ) {
   if(is.null(savedas)) { savedas <- paste(outputdir,"/",subj,"-",runtype,".txt",sep="") }
   if(!file.exists(dirname(savedas))) { dir.create(dirname(savedas),recursive=T) }
   print(paste('writting to',savedas))
   # save, but don't save the reason -- that's for the trial output
   # super cludgy way to pass around failure reason (first non-saccade of the trial)
   #grep('reason',names(allsacs))
   write.table(allsacs,file=savedas, sep="\t",row.names=F)
   print('wrote')
  }

  return(allsacs)
} 

scoreSingleTrial<-function(x,funnybusiness='') { # x is good sacs for that trial
     failreason <-NA
     if(!is.na(x$reason) && x$reason[1]!=''){
       failreason <- as.character(x$reason[1])
     }else if(dim(x)[1]<1 || is.na(x$onset) ){
        failreason <- 'no saccades' 

     # if the first sacc is too soon
     # is something the scoring function should do!? -- dropTrialSacs is easier to run from here
     } else if(x$onset[1]<lat.fastest && !any(grepl('nothingistoofast',funnybusiness)) ){
        failreason <- '1st good sac too soon' 

     # drop if the first good sac has poor tracking
     ### in BarsBeh this drops 1141 trials, of those 552 disagree with scorer, and those that agree are mostly wrong
     ### so probably a bad rule :)
     #} else if( length(x$p.tracked)>1 && x$p.tracked[1] < sac.firstmincoverage && !any(grepl('ignorefirstsactrack',funnybusiness)) ){
     #   failreason <- sprintf('first good sac has poor tracking ( %.2f < %.2f obvervedsamples/expected)',
     #                 x$p.tracked[1],sac.firstmincoverage )

     ## drop if first sac is not close to baseline (use same value as used to drop trials that start too far from baseline)
     ##  means the first sac doesn't inform the initial movement, so this trial is bogus
     } else if(abs(x$startpos[1] - screen.x.mid) > 50 && !any(grepl('ignorefirstsacstart',funnybusiness)) ) { 
       failreason <- 'start pos too far from center fix' 

     # do we have a good sac that explains how we got where we are from baseline
     # if not, drop trial
     #}else if(any(which(diff(c(0,goodsacsIdx))>1))) { 

     # # if the first good saccade is not the first saccade
     # # check that the start of the first good sac is 
     # # not all that much different from the start of the very first sac
     # # -- earlier invocation check this for all sacs, but it doesn't much matter after the first
     # goodsacidxdiffidx <- which(diff(c(0,goodsacsIdx))>1) 
     # if( goodsacidxdiffidx[1] == 1 ) {
     #  goodsacsIdx_afterbad <- goodsacsIdx[ goodsacidxdiffidx ]
     #  #good<-allsacs[ goodsacsIdx_afterbad    , ]
     #  #bad <-allsacs[ goodsacsIdx_afterbad -1 , ]
     #  good<-allsacs[ goodsacsIdx_afterbad[1]    , ]
     #  bad <-allsacs[ 1 , ]
     #  if(any(abs(good$startpos - bad$startpos)> 25 & good$cordir != bad$cordir    )){
     #    #DROP TRIAL, a good saccade is preced by a bad sac that moves the position by a lot
     #    # AND they do not go in the same direction
     #    failreason <- sprintf('first good sac at %f too far from first bad sac start position %f', good$startpos, bad$startpos)
     #    goodsacs <- NULL
     #  }
     # }

     ### made it past first round
     ### now take out all the bad saccades
     }else {
       # now select only the good saccades
       goodsacsIdx <- which(with(x,{intime&gtMinLen&p.tracked>sac.trackingtresh}))
       if(length(goodsacsIdx)<1){
        failreason <- 'no good saccades'
       } else {
        goodsacs <- x[goodsacsIdx, ]
        x <- goodsacs
       }
     }

     # if we want to drop
     if(!is.na(failreason)   ){
      if(is.na(x$reason[1]) || x$reason==''){
       cat('dropped at trial level:', x$subj[1],x$trial[1], failreason,'\n');
      }

      #return( dropTrialScore(x$trial[1],failreason, xdat=x$xdat[1], AS=xdatIsAS(x$xdat[1]) ) )
      return( dropTrialScore(x$trial[1],failreason, xdat=x$xdat[1], AS=trialIsType(x$xdat[1]) ) )

     ### EVERYTHING IS GOOD SO FAR
     } else {
       # break into trials, do we have 
       #  1) a first correct movement?
       #  2) correct movement after incorrect?
       #  3) an xdat that says Anti Saccade?
       return(data.frame( 
         trial=x$trial[1],
         xdat=x$xdat[1],
         lat=round(x$onset[1]*1000),
         fstCorrect=x$cordir[1], #!x$cordir[1]&any(x$cordir & (x$MaxMinX | x$startatFix) ),
         #ErrCorr=!x$cordir[1]&any(x$cordir &  x$corside  ),
         # this way error corrected is calcuted such that it is corrected if min and max
         # are on opposite side of the center, we asssume a correction's been made
         #   this could be complicated by the subject being off baseline (max 50px)
         #ErrCorr=!x$cordir[1]&length(x$cordir)>1&(max(x$maxpos-screen.x.mid)*min(x$minpos - screen.x.mid))<0,
         # this takes baseline into account instead of absolute 1/2screen
         # will still fail if there is a sac with no tracking dropped
         # before the good saccades get counted
         #  -- or if any saccade ends on the correct side of base.val (not screen.x.mid)  -- even if its the incorrect sac
         ErrCorr=!x$cordir[1]&length(x$cordir)>1&(any(x$MaxMinX[-1])||any(x$corside)),
         #AS=xdatIsAS(mean(x$xdat)),
         AS=trialIsType(mean(x$xdat)),
         Desc=''
       ) )
     }
}
# score a saccades per trial (or feed just one trial)
dropTrialScore <- function(trial=0,desc='no saccades',xdat=NA, AS=NA){
   if(trial==0){count=NA}
   else {count=-1}
   data.frame(trial=trial,
         xdat=xdat,
         lat=NA,
         fstCorrect=F,ErrCorr=F,
         AS=AS,
         Count=count,
         Desc=desc
     )
}

# reset Count for fix types
setScoreForFix <- function(scoredSac) {
   a<-scoredSac
   #a$Count[a$Desc=="no saccades"]<-1;
   a$Count[grepl("no saccades",a$Desc)] <- 1;
   a$Count[a$Desc==""]                  <- 0;
   a$Count[a$Desc=="no good saccades"]  <- 1;
   a$Count[grepl("no saccades within sac.time", a$Desc) ] <- 1;
   return(a) 
}

scoreSac <- function(allsacs,EPcorrect=NULL,funnybusiness=''){


  # select only those saccades we will count
  #goodsacs <- subset(allsacs, subset=intime&gtMinLen&p.tracked>sac.trackingtresh&!(crossFix!=0&!corside) )
  #goodsacs <- subset(allsacs, subset=intime&gtMinLen&p.tracked>sac.trackingtresh)
  if(is.null(allsacs) || dim(allsacs)[1]<1){
      cat('null or no saccades given to scoreSac!')
      dropTrialScore(0)
  }
  

  #if(is.null(goodsacs) || nrow(goodsacs)<1 ||dim(goodsacs)[1] < 1) { 
  #   failreason <- 'no good saccades found' 
  #}

  cor.ErrCor.AS <- ddply(allsacs, .(trial), scoreSingleTrial, funnybusiness=funnybusiness)


  #names(cor.ErrCor.AS) <- c('trial','xdat','lat','fstCorrect','ErrCorr','AS')
  # turn these back into logicals
  cor.ErrCor.AS[,c('fstCorrect','ErrCorr')]<-sapply(cor.ErrCor.AS[,c('fstCorrect','ErrCorr')],as.logical)

  # these get turned into factor, but back as character
  cor.ErrCor.AS$AS<-as.character(cor.ErrCor.AS$AS)
  cor.ErrCor.AS$Desc<-as.character(cor.ErrCor.AS$Desc)

  # set count "num sacs til correct movement"  (-1 drop, 0 error, 1 correct, 2 error corrected)
  cor.ErrCor.AS$Count[is.na(cor.ErrCor.AS$lat )] <- -1
  cor.ErrCor.AS$Count[!is.na(cor.ErrCor.AS$lat )] <- 0
  cor.ErrCor.AS$Count[ which(cor.ErrCor.AS$fstCorrect == T ) ] <- 1
  cor.ErrCor.AS$Count[ which(cor.ErrCor.AS$ErrCorr    == T ) ] <- 2
  # force order with Desc last
  cor.ErrCor.AS <- cor.ErrCor.AS[c('trial','xdat','lat','fstCorrect','ErrCorr','AS','Count','Desc')]
  
  # drop trials that disagree with log, if we have a log
  if(!is.null(EPcorrect) && length(EPcorrect) != nrow(cor.ErrCor.AS) ) {
   EPcorrect<- NULL;
   cat(sprintf('WARNING: not using EP log file, log has %d entries but run is %d\n',length(EPcorrect), nrow(cor.ErrCor.AS)))
  }
  if(!is.null(EPcorrect) ) {
   # eprime is 1 or NA. change NA to 0
   EPcorrect[is.na(EPcorrect)] <- 0;
   cor.ErrCor.AS$EPlog <- EPcorrect

   # drop any disagreement
   logDisagree <- which( (cor.ErrCor.AS$Count==1 & EPcorrect!=1) | (cor.ErrCor.AS$Count!=1 & EPcorrect==1) )
   if(length(logDisagree) > 0L ) {
     cor.ErrCor.AS$Desc[logDisagree] <- sprintf("Disagree with log file (org. cnt:%d) %s", cor.ErrCor.AS$Count[logDisagree] , cor.ErrCor.AS$Desc[logDisagree])
     cor.ErrCor.AS$Count[logDisagree] <- -1
     # undo anything that might have been set before dropping trial
     cor.ErrCor.AS$lat[logDisagree] <- NA
     cor.ErrCor.AS$fstCorrect[logDisagree] <- F
     cor.ErrCor.AS$ErrCorr[logDisagree] <- F
   }

  } else {
   # we could modify all other runs to indicate there was no EP log
   # but that's a lot of space
   #cor.ErrCor.AS$EPlog <- NA
  }

  # reinterpert count for fixation trials
  fixsacs <- which(grepl('FIX',cor.ErrCor.AS$AS))
  cor.ErrCor.AS[fixsacs,] <- setScoreForFix(cor.ErrCor.AS[fixsacs,])

  #fstCorrect&ErrCor
  #write.table(file=paste(  paste('eyeData',subj,id,sep="/"), "/", subj, "-", type,'.pertrial.txt', sep=""),cor.ErrCor.AS,row.names=F,quote=F)
  return(cor.ErrCor.AS)
}

# nerver used, also have dropScore in scoreEveryone that looks similiar
#dropRun <- function(){
#  data.frame('PSCor'=NA,'PSCorErr'=NA,'PSErr'=NA,'ASCor'=NA,'ASErrCor'=NA,'ASErr'=NA, 'Dropped'=expectedTrialLengths,'total'=expectedTrialLengths)
#
#}
scoreRun <-function(cor.ErrCor.AS,seentrials) {
  if(is.null(cor.ErrCor.AS) || length(cor.ErrCor.AS) <1){
   cat('scoreRun given bad cor.ErrCor.AS dataframe\n')
   return(dropRun())
  }
  ## dropped trials
  dropped <- which(cor.ErrCor.AS$Count == -1)
  dropped <- c(dropped, setdiff( seentrials,  cor.ErrCor.AS$trial) )
  #TODO: are dropped pro or anti saccades
  
  ## Pro Saccade
  PS <- subset(cor.ErrCor.AS, subset=grepl('PS',AS)&Count %in% 0:2)
  PS.cor    <- which(PS$fstCorrect)
  PS.ErrCor <- which(PS$ErrCorr)
  PS.Err    <- which(!( PS$fstCorrect | PS$ErrCorr ))

  ## Anti Saccade
  AS <- subset(cor.ErrCor.AS, subset=grepl('AS',AS)&Count %in% 0:2)
  AS.cor    <- which(AS$fstCorrect)
  AS.ErrCor <- which(AS$ErrCorr)
  AS.Err    <- which(!( AS$fstCorrect | AS$ErrCorr ))

  ## Other
  OS <- subset(cor.ErrCor.AS, subset=!grepl('[AP]S',AS)&Count %in% 0:2)
  OS.cor    <- which(OS$Count==1)
  OS.ErrCor <- which(OS$Count==2)
  OS.Err    <- which(OS$Count==0)

  lats <-as.data.frame(t(
   c(
     sapply(list(AScor.lat=AS.cor,ASErrCor.lat=AS.ErrCor,ASErr.lat=AS.Err),function(x){mean(AS$lat[x])}), 
     sapply(list(PScor.lat=PS.cor,PSErrCor.lat=PS.ErrCor,PSErr.lat=PS.Err),function(x){mean(PS$lat[x])}),
     sapply(list(OScor.lat=OS.cor,OSErrCor.lat=OS.ErrCor,OSErr.lat=OS.Err),function(x){mean(OS$lat[x])})
   )))

  #simple stat
  stats <- list('PSCor'=PS.cor,'PSCorErr'=PS.ErrCor,'PSErr'=PS.Err,'ASCor'=AS.cor,'ASErrCor'=AS.ErrCor,'ASErr'=AS.Err,'OSCor'=OS.cor,'OSErrCor'=OS.ErrCor,'OSErr'=OS.Err, 'Dropped'=dropped)
  lengths <- lapply(stats,length)
  #print(lengths)
  #print(sum(lengths))
  lengths$total <- sum(unlist(lengths))

  r <- cbind(as.data.frame(lengths), lats) 
  return(r)
}

#library(debug)
#mtrace(getSacs)
#mtrace(dropTrialSacs)
