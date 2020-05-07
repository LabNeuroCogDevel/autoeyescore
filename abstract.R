# 20200503WF - attempt to extract from complex mess I've made
# using parquet/arrow format for quick reads (maybe)
library(arrow)
ds <- read_parquet("./anti.parquet")



studysettings <- function(...){
  defaults <- list(
   ## TASK SPECIFIC
   # where is the middle of screen (fix location)
   lat.fastest=67/1000, # seconds, fastests possible saccade
   screen.x.mid=261/2,  # ASL coordinate positions
   # number of trials
   expectedTrialLengths=48,
   #minium distance to be considered a saccade
   sac.minmag=10,      # min abs of x position change -- set very low, inc to 20 at LR request :)
   maxsamples=200,     # more than this means we took more than we should have
   # acceleration -- needs to be this many pixels per sample before considering saccade
   lat.minvel=4,      # ASLcoordx/60Hz 
   # if there are spikes in fixation, bad tracking, drop
   # 99 means don't worry about it
   maxSamplesFromBaseline=99,
   sac.padding=30,  # padding to give to expected positions
   sac.majorRegionEnd=.75,  # the useful saccades probably already occur
   sampleHz = 60,
   xposStdDevMax = 40, # how unsmooth can interopolation be?
   sac.time=1.45,      # how long is the target up before we see the fixation cross again?
   blink.mintimedrop=100/1000,
   sac.minlen=42/1000, # saccades less than 50ms are merged/ignored 
   sac.held=100/1000, # if a sac is held -- it is accurate 
   sac.trackingtresh=0,  # what percent of a sac has to have actual samples (tacked) to be counted, set to 0 to ignore

   ### Score settings
   soipercent=F,     # samples of interest percent missing (30%)
   ignorexpossd=F,   # poor tracking: sd of xpos Δ= 40
   highvelstartok=F , # uses maxBeforeOnset
   ignoreblinks=F,
   blinkbeforeok=F,
   dontcutspikes=F)

  update(defaults,list(...))
}

drop_interp <- function(interp_df, base.val, opt) {
  if(is.nan(base.val) || abs(base.val-screen.x.mid )>50) {
   return(sprintf('average fixpos(%f) is off from baseline(%f)!',base.val,screen.x.mid ))
  }
  
  # is there big movement before the start?
  # don't do this for fix or anti (maxSamp... = 99, bars scanbars=2)
  # 20200506 - previously used on uninterpoltaed data
  onset_samples <- interp_df$xpos[1:(opts$lat.fastest*opts$sampleHz)]
  averageChangeBeforePhysOnset <- abs(na.omit(onset_samples- base.val))
  numtoofarfrombaseline <- length(which(averageChangeBeforePhysOnset > opts$sac.minmag  ) )
  if(numtoofarfrombaseline> maxSamplesFromBaseline) {
     return(sprintf('%.0f samples > %.0f px (%.2f max) from %.2f (base)'))
  }
  
  # is tracking smooth?
  # 20200506 - previously used on uninterpoltaed data
  averageChange <- sd(abs(diff(na.omit(interp_df$xpos[3:max(opts$sac.majorRegionEnd*opts$lsampleHz, len(interp_df$xpos))]))))
  if(is.na(averageChange) || (averageChange > opts$xposStdDevMax && !opts$ignorexpossd) ) {
      return(sprintf('poor tracking: sd of xpos Δ=%f',averageChange))
  }
  
  # target codes didn't match, we ate the whole file
  # or way too many
  # 200 might be too low of a threshold
  if (length(interp_df$xpos ) > opts$maxsamples ) {
     return(paste('too many samples in trial', dim(interp_df$xpos)[1]))
  }

  return(NA)
}

#  base.val <- mean(d[ c(-5:2) + targetIdxs[trl,1], 'xpos' ],na.rm=T)
saccades <- function(interp_df, opts){
    # sac.thres for anti like  2, 87, 172 or 258 (left to right)
    # likely from e.g.  getExpPos(sac.thresholds,xdatCode)
    # 20200503 - init - copy of ScoreRun.R:getSacs

    
    #TODO: determine baseline and sac.* iteratively
    #      error out if cannot be done
    #base.idx <- which(est$y < sac.right.small & est$y > sac.left.small & abs(fst$y) < lat.minvel )
    #base.val <- mean(est$y[base.idx])
    
    
    # fit to local polynomial (with guassain kernel)
    # NOTE: using bandwidth>1
    #browser()
    est  <- locpoly(interp_df$time, interp_df$xpos, bandwidth=1,drv=0)
    fst  <- locpoly(interp_df$time, interp_df$xpos, bandwidth=1,drv=1)
    scnd <- locpoly(interp_df$time, interp_df$xpos, bandwidth=1,drv=2)
    

    # catch movement before actual onset 
    # fst$x is in samples, we want the y value before the sample capturing closest time a sac can be made
    # avoid points up to the first sample to all things to settle
    # fst$x starts at 1, 1.9 is enough time to settle down from initial
    maxBeforeOnset <- max(abs( fst$y[fst$x< opts$lat.fastest*opts$sampleHz & fst$x> 1.9 ] ),na.rm=T)
    # sac.slowvel is probably 1px/60Hz
    # lat.minvel  is probably 4px/60Hz
    if(!opts$highvelstartok &&
        (is.nan(maxBeforeOnset)     || 
         abs(maxBeforeOnset) == Inf || 
         maxBeforeOnset > opts$lat.minvel)){
     return(sprintf('moving (%.3f px/60Hz) before target onset',maxBeforeOnset))
    }


    # run length encode when the change in sacad postion is faster
    # than the min velocity for a saccade
    # ** First sacade attribute

    rlePastDrv <- rle(abs(fst$y)>opts$lat.minvel & !is.na(fst$y))
    delt.x <- cumsum(rlePastDrv$lengths)

    # when does the direction change?
    #nsamp <- length(fst$x)
    #idxDirChange <-  which( c(F, sign(fst$y[1:(nsamp-1)]) !=  sign(fst$y[2:nsamp]) )  )
    
    # where they are moving, but less than requried for a saccade
    slowpoints <-rle(abs(fst$y)< sac.slowvel)
    slowpntIdx <-cumsum(slowpoints$lengths)

    
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
    if(nsacs<1){return('no saccades (getSacs)')}


    # actual time from target cue
    # est$x is in 60Hz samples, but est indexes are not!
    sac.df$onset  = est$x[sac.df$onsetIdx]/opts$sampleHz
    sac.df$slowed = est$x[sac.df$slowedIdx]/opts$sampleHz
    sac.df$end    = est$x[sac.df$endIdx]/opts$sampleHz
    

    ####
    #### BLINKS GO TO START of SAC
    ####
    NArle <- rle(is.na(b.approx$x))
    NArlecs <- cumsum(NArle$lengths)/opts$sampleHz
    actualblinkidx     <- NArle$values==T & NArle$lengths/opts$sampleHz > opts$blink.mintimedrop 
    eyeidx.blinkstart  <- NArlecs[which(actualblinkidx)-1]*sampleHz
    blinkends <- NArlecs[ actualblinkidx ]


    if(length(blinkends)==0){blinkends <- Inf}

    # if a sac starts with a blink, add that blink to the start
    # NB!!! onsetidx, min and max are now incorrect!!!
    if(!opts$ignoreblinks){
       sac.df$onset <- sapply(sac.df$onset,
               function(x){ 
                    a=x-blinkends
                    bidx=which(a<2/opts$sampleHz&a>0)
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
       sac.df$held      = c(sac.df$onset[2:nsacs] - sac.df$end[1:(nsacs-1)] >= opts$sac.held, T) 
     } else {
       sac.df$held      = 1
     }

    #browser()
    sac.df$gtMinLen  = sac.df$end - sac.df$onset > opts$sac.minlen
    sac.df$intime    = sac.df$onset < opts$sac.time & sac.df$end > opts$lat.fastest
    # first clause of intime (within xdat, isn't needed)
    sac.df$distance  = sac.df$endpos - sac.df$startpos 
    
    #sac.df$p.tracked = length(which( !is.na(d$xpos[trgt][sac.df$onsetIdx:sac.df$endIdx]) ) )/(sac.df$endIdx - sac.df$onsetIdx +1)
    sac.df$p.tracked  = apply(sac.df,1,
                         function(x){  
                          idxs <- round(fst$x[ x['onsetIdx']]) : round(fst$x [ x['endIdx'] ])
                          length(which(  !is.na(d$xpos[trgt][idxs]) ))/length(idxs)   
                         })
    #print(dput(sac.df))
    #sac.df$ltMaxLen  = sac.df$end - sac.df$onset < sac.maxlen # don't care how long it is
    
    
    slowcnt <- length(which(slowpntIdx<opts$sac.majorRegionEnd*opts$sampleHz))
    # normal seems to be around 7
    # 2 would be perfect (e.g. start going up, slow once at top)
    if(slowcnt > 8) { cat(subj,run,trl,"WARNING: unusual number of velc. changes",slowcnt ," poor tracking?\n") }
    
    ##TODO?? not a saccade (we care about) if motion is back to baseline
                          
    return(sac.df)
}

drop_blink <- function(ts, sac.df, opts) {
    # BLINK DROP
    # drop if there is a long blink that ends before any good sacade begins
    # TODO: rename variables to someting saine
    firstgoodsacidx <-  sac.df$intime & sac.df$gtMinLen & sac.df$gtMinMag & sac.df$p.tracked>opts$sac.trackingtresh
    firstsacstart <- sac.df[firstgoodsacidx, ]$onset[1]
    if(is.na(firstsacstart)){firstsacstart <- -Inf}


    if(any(blinkends < firstsacstart ) & !opts$ignoreblinks){
     # quick way to see if blink is held
     # if xpos is more than 5 px from any other
     unheldblinks <- unlist(lapply(which(blinkends<firstsacstart),
       function(x){
        # before and after blink ( with a two samples give for blink junk)
        samples <- round(opts$sac.held*opts$sampleHz)
        if(x<samples) { 
          origIdxesBeforeBlink <- c() 
        } else {
          origIdxesBeforeBlink <- eyeidx.blinkstart[x]+c(-samples:-1)
        }
        origIdxesAfterBlink <- blinkends[x]*opts$sampleHz+c(1:samples)
        # 20200506 change from real data to interoplated 
        #position <- b.orig[c(origIdxesBeforeBlink,origIdxesAfterBlink) ,'x']
        posittion <- ts[c(origIdxesBeforeBlink,origIdxesAfterBlink)]
        cat(blinkends[x] - eyeidx.blinkstart[x]/opts$sampleHz,"\n")
        max(position)-min(position) > opts$sac.minmag |         # positions of start and stop are far away
         blinkends[x] - eyeidx.blinkstart[x]/opts$sampleHz > .5 # sac is too long
        # should let scannerbars 10701.20110323.2.30 pass, doesn't
      }))

      # this checks that there isn't an immediate acceleration after the blink
      if(is.na(unheldblinks) || any(unheldblinks)) {
         return('blink ends before any saccades')
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
       && !opts$blinkbeforeok
       && !opts$ignoreblinks){

      firstsacstart <- sac.df$onset[1]
      firstblinkbeforesac <- firstsacstart - eyeidx.blinkstart[1]/opts$sampleHz
      if( firstblinkbeforesac  > maxBlinkLengthBeforeFirstSac){
         return('blink starts %.3f before any saccade')
      }
    }
    return(NA)
}

interoplateSamples <- function(ts, opts){
    ## get eye tracking for relevant portion
    b.orig <- data.frame(time=1:length(ts),x=ts) 


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


    b.approx <- b.cutblink

    ###### Remove crazy points that can be nothing other than tracking errors
    # eg, xpos like: 10 10 *200* 10 10; see behbars: 10827.20100617.1.34
    if(!opts$dontcutspikes){
      e<-length(b.approx$x)
      xposΔ <- c(0,diff(b.approx$x));
      # change is different on both sides (spike) and is greater than threshold
      spikypointsIdx <- which(rollapply(xposΔ,2,function(x){sign(prod(x,na.rm=T))==-1 & abs(min(x,na.rm=T))>30}))
      b.approx$x[spikypointsIdx ] <- NA
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
    if(length(b.approx$x)<=1) return('no data left after removing blinks')


    ####### Estimate away NAs
    # replace NAs so we can do polyfit
    b.approx <<- b.approx
    b.all <- b.approx
    b.all$x <- na.approx(b.approx$x,x=b.approx$time) 
    
    names(b.all) <- c('time','xpos')
    return(b.all)
}

scoreSacs <- function(sac.df, base.val) {
    # test again for no sacs
    # remove sacs that are too far away
    sac.df    =  sac.df[which(sac.df$onset < opts$sac.time), ]
    nsacs      <- dim(sac.df)[1]
    if( nsacs<1  ){
     return('no saccades within sac.time(%.3f)', opts$sac.time),
    }

  
    #expected mag and direction of saccade
    sac.expmag <- sac.thres - screen.x.mid
    sac.expdir <- sign(sac.expmag)
    sac.expmag <- abs(sac.expmag) # never used? 20200507

    sac.df$crossFix  = as.numeric(sac.df$startpos < base.val) - as.numeric(sac.df$endpos < base.val)
    # want to test against both the base.val (where we think center is) and screen.x.mid (where center fix should be)
    #sac.df$MaxMinX   = as.numeric(sac.df$minpos < min(base.val,screen.x.mid)) - as.numeric(sac.df$maxpos < max(base.val,screen.x.mid))
    sac.df$MaxMinX   = sign(sac.df$minpos - base.val) != sign(sac.df$maxpos - base.val)
    sac.df$gtMinMag  = abs(sac.df$distance) > opts$sac.minmag
    sac.df$startatFix= sac.df$startpos > base.val -10 & sac.df$startpos < base.val + 10
    #  0 if sac did not cross sides
    # -1 moved to the left
    #  1 moved to the right

    # SCORE
    sac.df$cordir    = sign(sac.df$endpos - sac.df$startpos) == sac.expdir
    sac.df$corpos    = abs(sac.df$endpos - sac.thres) <= sac.padding
    sac.df$corside   = sign(sac.df$endpos - base.val ) == sign(sac.thres - base.val)

    return(sac.df)
}



segment <- function(ts, opts){
    interp_df <- interoplateSamples(ts, opts)
    if(is.character(interp_df)) return(c(failreason=interp_df))

    sac.df <- saccades(interp_df, opts)
    if(is.character(sac.df)) return(c(failreason=sac.df))

    # TODO: partition into trials
    # get baseval


    # check tracking coverage for  "samples of interest"
    SOI.expect <- with(opts, (sac.majorRegionEnd - lat.fastest)*sampleHz)
    SOI.actual <- length(which(interp_df$time/opts$sampleHz < opts$sac.majorRegionEnd &
                               interp_df$time/opts$sampleHz > opts$lat.fastest) )
    #print(c(SOI.actual,SOI.expect))
    #print(b.all)
    if(SOI.actual < SOI.expect*.30 && !opts$soipercent) return('< 30% tracking for samples of interest')

    # sac.thres for anti like  2, 87, 172 or 258 (left to right)
    # likely from e.g.  getExpPos(sac.thresholds,xdatCode)

    # TODO
    start <- NULL 
    end <- NULL
    base.val <- mean(ts[c(-5:2) + start],na.rm=T)
    dropreason <- drop_interp(interp_df[start:end], base.val, opts)
    if(!is.null(dropreason)) return(dropreason)

    # TODO: adjust sac.df indexes to match provided ts?
    dropreason <- drop_blink(ts, sac.df, opts)
    if(!is.null(dropreason)) return(dropreason)

    scoreSacs(sac.df, base.val)
}
