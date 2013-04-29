# TODO:
#
# Score an eyd turned tsv file
#  ** NEED TO SOURCE a settings file before running this!!!
#     e.g. anti/anti.settings.R 
#
# accuracy is if the saccade is held
#    which is if there is no saccade in the next 100ms
# * add 'held' T/F column
# * need to check sac.minmag, suggest increasing
# * do not need sac.maxlength
# * should not merged saccades that go opposite ways -- see 004 ANTI 35
# * graph dropped trials still. can see how bad it is
# * take a meassure of polyfit's estimation, drop if off in critical time
#
library(KernSmooth)
library(ggplot2)
library(gridExtra)

#### Settings ######
saverootdir  <- 'eyeData'

#ASL max is 261x240
xmax         <- 274
ymax         <- 250
screen.x.mid <- 261/2    # xpos of eye, not x axis! -- this is where we expect the center fixation to be

## experimental design
sac.time     <- 1.45 # how long is the target up before we see the fixation cross again?

## latancy properties
#  how fast the eye has to move before considering the movement a saccade (also heuristic overcompesating approximation)
lat.minvel   <- 5        # ASLcoordx/60Hz 
lat.fastest  <- 67/1000  # fastest latency allowed is 200ms

## saccade properties ##
                               
# target position thresholds
# beyond threshold == saccad to expected region

#minium distance to be considered a saccade
sac.minmag   <- 50       # min abs of x position change, was 20 -- then changed to 15, now 50
#sac.maxlen   <- 250/1000 # maximum length of a sac should be 250 ms # NOTE. account for merged
sac.minlen   <-  50/1000 # saccades less than 50ms are merged/ignored
sac.mingap   <-  50/1000 # saccades must be at mimumum this time appart, merged otherwise
sac.held    <- 100/1000  # if a sac is held -- it is accurate


### Plot settings
# Green:   means correct direction to the correct place
# Blue:    good dir, over or under shot position
# Orange:  wrong dir, but moved as much as should have if the direction was correct
# Red:     wrong direction, wrong magnitude
#
# TRUE FALSE is correct direction, wrong magintued of saccade -- NA's sneak in, so inluce a bunch of 'black's
positionColors<-c('TRUE TRUE'='green', 'TRUE FALSE'='blue', 'FALSE FALSE'='red','FALSE TRUE'='orange', 'black','black','black','black','black')
  
xScaleTime <- sort(c(seq(0,3,by=.25), lat.fastest, sac.time ) )
# remove the tick that is close to sac.time -- should do this algorithmicly with diff
xScaleTime <- xScaleTime[-which(xScaleTime==1.5)]
# p is always the same
# but x limit should probably be done programticly/abstracted (170-> variable name)
p    <- ggplot() + theme_bw() + theme(legend.position="none" ) +
        scale_x_continuous(limits=c(0,170), breaks=xScaleTime*60, labels=xScaleTime)


## FUNCTIONS

#
#### PLOT 
#
ggplotXpos <- function(est,d,trgt,sac.df,base.val,delt.x.scale,slowpnt.x.scale,ptitle) {
  
  ybreaks= sort( c(122,sac.thresholds ) )
  g    <- p +
          scale_y_continuous(breaks=ybreaks,labels=ybreaks,limits=c(0,263) )

  if(length(est$x)>0) {
     g <- g + geom_point(data=data.frame(x=est$x,y=est$y),aes(x=x,y=y),alpha=I(.5))
  }

     g <- g + geom_point(data=d[trgt,],x=1:length(trgt), aes(y=xpos,color=as.factor(xdat)))  +
          scale_color_manual(values=c('green','red','blue','black')) + # should only ever see green
          geom_vline(xintercept=60*lat.fastest,color=I('red')) +
          geom_vline(xintercept=60*sac.time,color=I('red'),linetype=I('dotdash'))

  # show velocity changes and estimated base (center focus) value
  if(length(delt.x.scale)>0) {
     g <- g +
          geom_vline(xintercept=delt.x.scale,color=I('blue'),alpha=I(.3)) +
          geom_vline(xintercept=slowpnt.x.scale,color=I('yellow'),alpha=I(.3)) +
          geom_hline(yintercept=base.val,color=I('green'),alpha=I(.5))
   }

     # outline "good" position (with padding) in purple
     g <- g +
          geom_hline(yintercept=c(sac.thres-sac.padding,sac.thres+sac.padding),
                     color=I('purple'),alpha=I(.2)) +
          ggtitle(ptitle) + ylab('left to right')+xlab('')
  
  # put colored boxes around saccades
  if(!is.null(sac.df) && dim(sac.df)[1]>=1){
     g <- g +
           geom_rect(data=sac.df, aes(xmin=onset*60,xmax=end*60,
                                      ymin=-Inf,    ymax=Inf,
                     fill=as.factor(paste(cordir,corpos)), alpha=intime&gtMinLen  )) + 
           scale_fill_manual(values=positionColors) +
           scale_alpha_manual(values=c('TRUE'=.5,'FALSE'=.2))
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
     #cat('write to', outputdir, filename,"\n")
     outputdir<-dirname(filename)
     if(!file.exists(outputdir)) { dir.create(outputdir,recursive=T) }

     pdf(filename,height=6, width=15)
  }
  
  # if we have saccades, show a table
  if(length(sac.df)>0){
      grid.arrange( nrow=3,heights=c(.5,1,.4),
                tableGrob( sac.df[,-c(1:3)]),  # exclude index values
                 g,
                 drv
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
dropTrial <- function(subj,runtype,trl,xdatCode,reason,plotData=F,savedas=NULL) {
   cat(subj, runtype, trl ,'|', reason,'| DROPPED!\n')

   if(plotData==T){
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

      savePlots(sac.df=NULL,g,drv=NULL,filename,writetopdf=T)
   }

   # return empty data frame -- no sacs

   data.frame(
         subj=subj,run=runtype,trial=trl,
         onset=0,slowed=0,end=0,startpos=0,endpos=0,cordir=F,corpos=F, # combined=F,
         held=F,gtMinLen=F, intime=F,p.tracked=0, xdat=xdatCode
         )

}
# returns 'allsacs' a list of all saccades in each trial
scoreRun <- function(eydfile, subj, run, runtype,onlyontrials=NULL,writetopdf=T,savedas=NULL, showplot=F){
  cat('using ', eydfile ,'\n')

  ### load data
  #load xdat, eye position and dialation data for parsed eydfile
  d        <- read.table( eydfile, sep="\t",header=T)
  names(d) <- c("xdat","dil","xpos","ypos")

  ## fix bars zero'ed xdats
  zeroxdat        <- rle(d$xdat==0)
  if(length(zeroxdat$lengths)>1) {
     xdatswitchidx   <- cumsum(zeroxdat$lengths)
     zeroxdatidx     <- which(zeroxdat$values==TRUE)

     replacementXdat <- d$xdat[xdatswitchidx[zeroxdatidx-1]]
     #needReplaced    <- d$xdat[xdatswitchidx[zeroxdatidx]:(xdatswitchidx[zeroxdatidx]+zeroxdat$lengths[zeroxdatidx])]
     zerostartend    <- cbind(  end=xdatswitchidx[zeroxdatidx], 
                              start=(xdatswitchidx[zeroxdatidx]-zeroxdat$lengths[zeroxdatidx]+1)
                             )

     zeroIdxinD      <- apply(zerostartend,1,function(x){x['start']:x['end']})
     shouldbeXdat    <- d$xdat[xdatswitchidx[zeroxdatidx-1]]

     if(length(shouldbeXdat) != length(zeroIdxinD) ) warning('zero xdats and replacement not the same length!! something very funny is going on')
     for( i in 1:length(shouldbeXdat)){ d$xdat[zeroIdxinD[[i]]] <- shouldbeXdat[i]}
  }

  
  ##### Remove bad data #####
  # both x and y are out of range, or there is no dilation measure
  # TODO!!
  # if there is x and ypos and dil != 0
  # maybe we should keep?
  badidxs=d$xpos>xmax|d$ypos>ymax|d$dil==0
  d[badidxs,c('dil','xpos','ypos')] = c(NA,NA,NA)
  
   # remove repeats
  xdats             <- rle(d$xdat)            # run length encode
  xdats$cs          <- cumsum(xdats$lengths)  # index in d where xdat happends
  
  #### Split eye positions by target code
  # use only targetcodes that are preceded by startcodes and followed by end codes
  tarCodePos       <- which(xdats$values %in% targetcodes)
  goodTargPos      <- tarCodePos[  xdats$values[tarCodePos  - 1]  %in%  startcodes ]
  goodTargPos      <- goodTargPos[ xdats$values[goodTargPos + 1]  %in%  stopcodes  ]
  
  if(length(goodTargPos) <= 0) {
    cat(subj,runtype,'WARNING: no understandable start/stop xdats!\n')
    return() 
  }
  if(! length(goodTargPos) %in% expectedTrialLengths ) {
    cat(subj,runtype,'WARNING: unexpected num of trials', length(goodTargPos),'\n')
  }
  # x by 2 matrix of target onset and offset indicies
  targetIdxs       <- cbind(xdats$cs[goodTargPos-1],xdats$cs[goodTargPos])
  

  # setup dataframe to hold sacades from all trials
  allsacs     <- data.frame()
  
  # if onlyontrials is specified, we only want to look at those/that trial
  # but we want to keep onlyontrials null/not null for write.csv check later
  if( is.null(onlyontrials) ) { runontrials <- 1:dim(targetIdxs)[1] }
  else                        { runontrials <- onlyontrials }

  for(trl in runontrials ) {
    #print(c(trl,length(runontrials)))
    #trl  <- 8
    
    # target code xdat is a little past where target index starts
    xdatCode <- d$xdat[ targetIdxs[trl,1] + 1 ]
    # the code is after the startcode but before stopcode (between targetidx[,1] and [,2] 
    # before was taking the 15th to make sure past any repated start xdat??
    # now only using 1 ahead

    # seems okay because:
    #print(head(d$xdat[ targetIdxs[trl,1]:targetIdxs[trl,2] ]))
    
    # maybe not the best place, but we should check targetIdx length
    numTargetCodes<-diff(targetIdxs[trl,])
    ##TODO: THIS 85 SHOULD NOT BE HARDCODED
    # it is the min number of samples accepted for a target code  
    if(numTargetCodes < 85) cat(sprintf('WARNING: have only %d target codes\n',numTargetCodes))

    # this particular threshold
    #sac.thres <<- sac.thresholds[ xdatCode%%10 ]
    # xdatCode is index for thesholds (1 -> right short, 2-> right long, 3->left short, 4->left long)
    # targetcodes are in pairs of 4, first 2 are right, second 2 are down. But this is always anti
    #  -- find which index the targetcode matches in the list, find which number in the pair of four it is
    #  use that (i^th) index of threshold
    sacidx <- which(targetcodes %in% xdatCode) %% 4 
    if(sacidx == 0) sacidx=4
    sac.thres <<- sac.thresholds[sacidx]
    
    
    #expected  mag and direction of saccade
    sac.expmag <<- sac.thres - screen.x.mid
    sac.expdir <<- sign(sac.expmag)
    sac.expmag <<- abs(sac.expmag)

    
    # grab the eye data for thsi trials traget code
    #trgt <-targetIdxs[trl,1]:targetIdxs[trl,2] 
    print(trl)
    trgt <-(targetIdxs[trl,1]+1):targetIdxs[trl,2]  # +1 to skip the last cue xdat code
    b    <-na.omit( data.frame(time=1:length(trgt),x=d$xpos[trgt]) )
    # fill in the NAs with approximations
    naX   <- which(is.na(d$xpos[trgt]))
    b.all <-b
    names(b.all) <- c('x','y')
    
    # if we take out the NAs and there is nothing left!
    if(length(b.all$y)<=0){
     allsacs <- rbind( allsacs, dropTrial(subj,runtype,trl,xdatCode,'no data',plot=F))
     next 
    }

    # sanity check -- do we really only have target Idxs?
    obsvTrgCode <- unique(d$xdat[trgt])
    if( length(obsvTrgCode) != 1 || ! obsvTrgCode %in% targetcodes ){
     cat(subj,runtype,trl,xdatCode, 'funky trial codes! ',obsvTrgCode ,"\n")
    }

    # check tracking coverage for  "samples of interest"
    SOI.expect <- (sac.majorRegionEnd - lat.fastest)*60
    SOI.actual <- length(which(b.all$x/60 < sac.majorRegionEnd & b.all$x/60 > lat.fastest) )
    #print(c(SOI.actual,SOI.expect))
    #print(b.all)
    if(SOI.actual < SOI.expect*.30) {
     allsacs <- rbind( allsacs, dropTrial(subj,runtype,trl,xdatCode,'< 30% tracking for samples of interest'))
     next 
    }

    # is trackign smooth?
    averageChange <- sd(abs(diff(na.omit(d$xpos[trgt[3:(sac.majorRegionEnd*60) ]  ]))))
    if(averageChange > 15) {
        cat(subj,run,trl,"WARNING: sd of xpos delta in samples of interset is high: ",averageChange,"\n")
    }

    # replace NAs so we can do polyfit
    if(length(naX)>0){
      esta  <-approx(b.all$x,b.all$y, naX)
      if(any(is.na(esta$y))){
        esta <- data.frame(x=c(),y=c())
        print(paste(sep=" ",subj,runtype, trl, xdatCode, 'estimation errored, has NAs')  )
      }
    
      # merge est approx with original
      # keep original so approx can easily be plotted
      b.all <- rbind(b.all,as.data.frame(esta))
    
      # check bind didn't introduce repeated x values
      if (   length(which( rle( sort( b.all$x ) )$lengths >1 ) ) != 0 ) {
         print(paste(sep=" ",subj,runtype, trl, xdatCode,'multiple X values after approximation!')  )
       }
    
    }
   # target codes didn't match, we ate the whole file
   # or way too many
   # 200 might be too low of a threshold
   if (   length(b.all$x ) > 200 ) {
     allsacs <- rbind( allsacs, dropTrial(subj,runtype,trl,xdatCode,paste('too many samples in trial', dim(b.all$x)[1]) ) )
     next 
    }
    
    
    
    # fit to local polynomial (with guassain kernel)
    # NOTE: using bandwidth>1
    #browser()
    est  <- locpoly(b.all$x,b.all$y,bandwidth=1,drv=0)
    fst  <- locpoly(b.all$x,b.all$y,bandwidth=1,drv=1)
    scnd <- locpoly(b.all$x,b.all$y,bandwidth=1,drv=2)
    
    # run length encode when the change in sacad postion is faster
    # than the min velocity for a saccade
    # ** First sacade attribute
    rlePastDrv <- rle(abs(fst$y)>lat.minvel)
    delt.x <- cumsum( rlePastDrv$lengths )

    # when does the direction change?
    #nsamp <- length(fst$x)
    #idxDirChange <-  which( c(F, sign(fst$y[1:(nsamp-1)]) !=  sign(fst$y[2:nsamp]) )  )
    
    # where they are moving, but less than requried for a saccade
    slowpoints<-rle(abs(fst$y)<1)
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

    if(nsacs<1){
     droppedTrial <- dropTrial(subj,runtype,trl,xdatCode,'no saccades')
     if(dim(allsacs)[1] > 1 ) {
       allsacs <- rbind( allsacs, droppedTrial)
      } else {
       allsacs <- droppedTrial
      }

     next 
    }

    ##print(b.all$x)
    #print(est$x
    
    # actual time from target cue
    # est$x is in 60Hz samples, but est indexes are not!
    sac.df$onset = est$x[sac.df$onsetIdx]/60
    sac.df$slowed = est$x[sac.df$slowedIdx]/60
    sac.df$end    = est$x[sac.df$endIdx]/60
    
    
    
    ## TODO:
    # see 007 ANTI 48
    #  dont overlap if sign of drv is different!
    
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
    
    if(any(overlapSac)) {
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
    sac.df$gtMinLen  = sac.df$end - sac.df$onset > sac.minlen
    sac.df$intime    = sac.df$onset < sac.time & sac.df$end > lat.fastest

    
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

    slowcnt <- length(which(slowpntIdx<sac.majorRegionEnd*60))
    # normal seems to be around 7
    # 2 would be perfect (e.g. start going up, slow once at top)
    if(slowcnt > 8) { cat(subj,run,trl,"WARNING: unusual number of velc. changes",slowcnt ," poor tracking?\n") }
    
    ##TODO?? not a saccade (we care about) if motion is back to baseline
                          
    #TODO: determine baseline and sac.* iteratively
    #      error out if cannot be done
    base.idx <- which(est$y < sac.right.small & est$y > sac.left.small & abs(fst$y) < lat.minvel )
    base.val <- mean(est$y[base.idx])
    
    #cat('mean', base.val , '\n')
 
    ## PLOT -- only if we are told to

    if(showplot) {
       ptitle <- paste(subj,runtype, trl,xdatCode)
       g    <- ggplotXpos(est,d,trgt,sac.df,base.val,delt.x.scale,slowpnt.x.scale,ptitle)
       drv <- ggplotDrv(fst,scnd,slowpnt.x.scale,delt.x.scale)

       # write out plot
       if(is.null(savedas)){ 
         outputdir <- paste(saverootdir, subj,paste(run,runtype,'DROPPED',sep=".") ,sep="/")
       } else {
         outputdir <- paste(dirname(savedas),'img',sep='/')
       }
       filename<-paste(sprintf("%02d",trl),xdatCode,sep="-")
       filename<-paste(outputdir,'/', filename,'.pdf', sep="")

       savePlots(sac.df,g,drv,filename,writetopdf)
    }

    allsacs <- rbind(allsacs,
                   data.frame(
                     subj=subj,run=runtype,trial=trl,
                     sac.df[,-c(1:3)], xdat=xdatCode
                   )
                )
  }


  # only write to file if we have processed all
  # which means we didn't specify onlyontrials when calling the function
  if(is.null(onlyontrials) ) {
   if(is.null(savedas)) { savedas <- paste(outputdir,"/",subj,"-",runtype,".txt",sep="") }
   if(!file.exists(dirname(savedas))) { dir.create(dirname(savedas),recursive=T) }
   print(paste('writting to',savedas))
   write.table(allsacs,file=savedas, sep="\t",row.names=F)
   print('wrote')
  }

  return(allsacs)
} 
#mtrace(scoreRun)
#mtrace(dropTrial)
