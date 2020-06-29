# 20200503WF - attempt to extract from complex mess I've made
# using parquet/arrow format for quick reads (maybe)

# library(arrow)
# ds <- read_parquet("./anti.parquet")

# encase return value with success/failure structure
# helpful when we need to bail out of a function because something broke
# 20200629 - is this better than `stop` and tryCatch?
r <- function(value=NULL, reason=NULL)
    list(value=value,success=!is.null(value), reason=reason) 
f <- function(reason) r(reason=reason) 

#
# data pipeline:
#  opts <- studysettings(): task and algo settings/options. passed everwhere. 
#  eyedf <- {read,clean}_data(): all raw data
#  b.approx, interp_df/b.nona <- interp_samples(): blink exended and sike removed, nan's removed
#  -- trial  <- trial_sacs() --
#  blinks <- find_blinks(): blink index info
#  sac_df <- find_saccades():  where eye movement velocity excites expected
#         <- sac_pos():     use expected position to "score" saccades



# make a list of variables and carry over those names
namedList <-function(...) { X_<-list(...); names(X_) <- unlist(substitute(...())); return(X_)}


studysettings <- function(...){
  defaults <- list(

   ##LIKELY TO CHANGE (task)
   # number of trials
   expectedTrialLengths=c(48),
   startcodes=c(32,34,35,36),
   targetcodes=c(131,132,133,134),
   stopcodes=c(250,254,255),
   sac.time=1.45,           # how long is the target up before we see the fixation cross again?
   sac.majorRegionEnd=.75,  # the useful saccades probably already occur
   ## XDAT functions -> trial info
   # use xdat to get trial info
   # what is the threshold for left/right position
   # xdatCode is index for thesholds (1 -> right short, 2-> right long, 3->left short, 4->left long)
   getExpPos=function(xdat) return(c(258, 172, 87, 2)[ xdat - 130 ] ),
   # is this xdat an antisaccde xdat?
   xdatIsAS=function(xdat) return(TRUE),
   trialIsType=function(xdat) return('AS'),

      
   ## equipment - ASL eyetracker circa ~2000
   # where is the middle of screen (fix location)
   xmax=274,
   ymax=250,
   screen.x.mid=261/2,  # ASL coordinate positions
   sampleHz = 60,

   ## algo parameters
   lat.fastest=67/1000, # seconds, fastests possible saccade
   #minium distance to be considered a saccade
   sac.minmag=10,      # min abs of x position change -- set very low, inc to 20 at LR request :)
   maxsamples=200,     # more than this means we took more than we should have
   # acceleration -- needs to be this many pixels per sample before considering saccade
   lat.minvel=4,      # ASLcoordx/60Hz 
   sac.slowvel=1,     # ASLXcord/60Hz
   # if there are spikes in fixation, bad tracking, drop
   # 99 means don't worry about it
   maxSamplesFromBaseline=99,
   sac.padding=30,  # padding to give to expected positions
   xposStdDevMax = 40, # how unsmooth can interopolation be?
   blink.mintimedrop=100/1000,
   sac.minlen=42/1000, # saccades less than 50ms are merged/ignored 
   sac.held=100/1000, # if a sac is held -- it is accurate 
   sac.trackingtresh=0,  # what percent of a sac has to have actual samples (tacked) to be counted, set to 0 to ignore
   blink.trim.samples=2,

   ### Score settings
   soipercent=F,     # samples of interest percent missing (30%)
   ignorexpossd=F,   # poor tracking: sd of xpos Δ= 40
   highvelstartok=F, # uses maxBeforeOnset
   ignoreblinks=F,
   blinkbeforeok=F,
   dontcutspikes=F,
   useextremefilter=F # any kill sammple if any dil, xpos, ypos are bad
  )

  changes <- list(...)
  if(length(changes)>0L) return(modifyList(defaults, changes))
  return(defaults)
}

drop_interp <- function(interp_xpos, base.val, opts) {
  # okay fixation position?
  if(is.nan(base.val) || abs(base.val - opts$screen.x.mid)>50) {
   return(sprintf('average fixpos(%f) is off from baseline(%f)!', base.val, opts$screen.x.mid ))
  }
  xtime <- 1:length(interp_xpos)

  # check tracking coverage for  "samples of interest"
  SOI.expect <- with(opts, (sac.majorRegionEnd - lat.fastest)*sampleHz)
  SOI.actual <- length(which(xtime/opts$sampleHz < opts$sac.majorRegionEnd &
                             xtime/opts$sampleHz > opts$lat.fastest) )
  if(SOI.actual < SOI.expect*.30 && !opts$soipercent) return('< 30% tracking for samples of interest')
  
  # is there big movement before the start?
  # don't do this for fix or anti (maxSamp... = 99, bars scanbars=2)
  # 20200506 - previously used on uninterpoltaed data
  onset_samples <- interp_xpos[1:(opts$lat.fastest*opts$sampleHz)]
  averageChangeBeforePhysOnset <- abs(na.omit(onset_samples- base.val))
  numtoofarfrombaseline <- length(which(averageChangeBeforePhysOnset > opts$sac.minmag  ) )
  if(numtoofarfrombaseline> maxSamplesFromBaseline) {
     return(sprintf('%.0f samples > %.0f px (%.2f max) from %.2f (base)',
                           numtoofarfrombaseline,
                           max(averageChangeBeforePhysOnset),
                           sac.minmag,base.val))
  }
  
  # is tracking smooth?
  # 20200506 - previously used on uninterpoltaed data
  averageChange <- sd(abs(diff(na.omit(interp_xpos[3:max(opts$sac.majorRegionEnd*opts$sampleHz, length(interp_xpos))]))))
  if(is.na(averageChange) || (averageChange > opts$xposStdDevMax && !opts$ignorexpossd) ) {
      return(sprintf('poor tracking: sd of xpos Δ=%f',averageChange))
  }
  
  # target codes didn't match, we ate the whole file
  # or way too many
  # 200 might be too low of a threshold
  if (length(interp_xpos ) > opts$maxsamples ) {
     return(paste('too many samples in trial', dim(interp_xpos)[1]))
  }

  return(NA)
}

no_early_move <- function(ts, opts){
    # opts$lat.fastest*opts$sampleHz
    if(length(ts) > opts$lat.fastest*opts$sampleHz+1) stop('no_early_move given too long a timesies!')

    fst  <- KernSmooth::locpoly(1:length(ts), ts, bandwidth=1, drv=1)
    # catch movement before actual onset 
    # fst$x is in samples, we want the y value before the sample capturing closest time a sac can be made
    # avoid points up to the first sample to all things to settle
    # fst$x starts at 1, 1.9 is enough time to settle down from initial
    maxBeforeOnset <- max(abs(fst$y[fst$x< opts$lat.fastest*opts$sampleHz & fst$x> 1.9]),na.rm=T)
    # sac.slowvel is probably 1px/60Hz
    # lat.minvel  is probably 4px/60Hz
    if(!opts$highvelstartok &&
        (is.nan(maxBeforeOnset)     || 
         abs(maxBeforeOnset) == Inf || 
         maxBeforeOnset > opts$lat.minvel)){
     return(sprintf('moving (%.3f px/60Hz) before target onset', maxBeforeOnset))
    }
    return(NA)
}

find_blinks <- function(x, opts){
    # find blink indexes (in actual sample space)
    # x from b.approx$x
    NArle <- rle(is.na(x))
    NArlecs <- cumsum(NArle$lengths)/opts$sampleHz
    actualblinkidx     <- NArle$values==T & NArle$lengths/opts$sampleHz > opts$blink.mintimedrop 
    eyeidx.blinkstart  <- NArlecs[which(actualblinkidx)-1]*opts$sampleHz
    blinkends <- NArlecs[actualblinkidx]
    if(length(blinkends)==0) blinkends <- Inf
    # all the things we'll use later
    namedList(NArlecs, actualblinkidx, eyeidx.blinkstart, blinkends)
}

onset_blink <-function(onsets, blinks, opts) {
    # if a sac starts with a blink, add that blink to the start
    # NB!!! onsetidx, min and max are now incorrect!!!
    if(opts$ignoreblinks) return(onsets)
    sapply(sac.df$onset,
       function(x){ 
            a=x-blinks$blinkends
            bidx=which(a<2/opts$sampleHz & a>0)
            newstart=blinks$NArlecs[which(blinks$actualblinkidx)-1][bidx]
            #TODO: newstart should be null if this change doesn't change x position
            if(length(newstart)>0 && x-newstart < .5 ){ 
             newstart + opts$blink.trim.samples/opts$sampleHz 
            }else{
             x
            } 
      })

}

find_saccades <- function(interp_xpos, blinks, opts, xtime=1:length(interp_xpos)){
    # 20200503 - init - copy of ScoreRun.R:getSacs

    # fit to local polynomial (with guassain kernel)
    #browser()
    est  <- KernSmooth::locpoly(xtime, interp_xpos, bandwidth=1, drv=0)
    fst  <- KernSmooth::locpoly(xtime, interp_xpos, bandwidth=1, drv=1)
    

    # run length encode when the change in sacad postion is faster
    # than the min velocity for a saccade
    # ** First sacade attribute

    rlePastDrv <- rle(abs(fst$y)>opts$lat.minvel & !is.na(fst$y))
    delt.x <- cumsum(rlePastDrv$lengths)

    # when does the direction change?
    #nsamp <- length(fst$x)
    #idxDirChange <-  which( c(F, sign(fst$y[1:(nsamp-1)]) !=  sign(fst$y[2:nsamp]) )  )
    
    # where they are moving, but less than requried for a saccade
    slowpoints <-rle(abs(fst$y)< opts$sac.slowvel)
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
      endIdx    =  sapply(speedingUp, function(x){ slowedDown[which(x<slowedDown)[1]] }))



    # remove saccades that haven't finished (end usually doesn't matter)
    # could probably do the same thing by which(df$onset < sac.time)
    noEndIdx <- which(is.na(sac.df$endIdx+sac.df$slowedIdx))
    if(length(noEndIdx)>0) { sac.df <- sac.df[ -noEndIdx, ] }
    nsacs      <- dim(sac.df)[1]


    # actual time from target cue
    # est$x is in 60Hz samples, but est indexes are not!
    sac.df$onset  = est$x[sac.df$onsetIdx]/opts$sampleHz
    sac.df$slowed = est$x[sac.df$slowedIdx]/opts$sampleHz
    sac.df$end    = est$x[sac.df$endIdx]/opts$sampleHz
    

    # onsets start at start of blink if saccade over a blink
    sac.df$onset <- onset_blink(sac.df$onsets, blinks, opts)
    
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
            plyr::ddply(sac.df,c('onsetIdx', 'endIdx'),function(x){
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
    
    
    slowcnt <- length(which(slowpntIdx<opts$sac.majorRegionEnd*opts$sampleHz))
    # normal seems to be around 7
    # 2 would be perfect (e.g. start going up, slow once at top)
    if(slowcnt > 8) { cat("WARNING: unusual number of velc. changes",slowcnt ," poor tracking?\n") }
    
    ##TODO?? not a saccade (we care about) if motion is back to baseline

    # 20200508 - reset onsetIdx and endIdx to be relative to actual samplerate
    sac.df$onsetIdx <- round(fst$x[sac.df$onsetIdx])
    sac.df$endIdx   <- round(fst$x[sac.df$endIdx])
    return(sac.df)
}

drop_blink <- function(ts, sac.df, blinks, opts) {


    ###### DROP TRIAL CONDITIONS
    # no saccades, also will trap no enough data
    if(nrow(sac.df)<1L){return('no saccades (drop_blink)')}

    # BLINK DROP
    # drop if there is a long blink that ends before any good sacade begins
    # TODO: rename variables to someting saine
    firstgoodsacidx <-  sac.df$intime & sac.df$gtMinLen & sac.df$gtMinMag & sac.df$p.tracked>opts$sac.trackingtresh
    firstsacstart <- sac.df[firstgoodsacidx, ]$onset[1]
    if(is.na(firstsacstart)){firstsacstart <- -Inf}


    if(any(blinks$blinkends < firstsacstart ) & !opts$ignoreblinks){
     # quick way to see if blink is held
     # if xpos is more than 5 px from any other
     unheldblinks <- unlist(lapply(which(blinks$blinkends<firstsacstart),
       function(x){
        # before and after blink ( with a two samples give for blink junk)
        samples <- round(opts$sac.held*opts$sampleHz)
        if(x<samples) { 
          origIdxesBeforeBlink <- c() 
        } else {
          origIdxesBeforeBlink <- blinks$eyeidx.blinkstart[x]+c(-samples:-1)
        }
        origIdxesAfterBlink <- blinks$blinkends[x]*opts$sampleHz+c(1:samples)
        # 20200506 change from real data to interoplated 
        #position <- b.orig[c(origIdxesBeforeBlink,origIdxesAfterBlink) ,'x']
        posittion <- ts[c(origIdxesBeforeBlink,origIdxesAfterBlink)]
        cat(blinks$blinkends[x] - blinks$eyeidx.blinkstart[x]/opts$sampleHz,"\n")
        max(position)-min(position) > opts$sac.minmag |         # positions of start and stop are far away
         blinks$blinkends[x] - blinks$eyeidx.blinkstart[x]/opts$sampleHz > .5 # sac is too long
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
    if(length(blinks$eyeidx.blinkstart)>0 
       && nrow(sac.df) > 0
       && length(blinks$eyeidx.blinkstart)>0
       && !opts$blinkbeforeok
       && !opts$ignoreblinks){

      firstsacstart <- sac.df$onset[1]
      firstblinkbeforesac <- firstsacstart - blinks$eyeidx.blinkstart[1]/opts$sampleHz
      if( firstblinkbeforesac  > maxBlinkLengthBeforeFirstSac){
         return('blink starts %.3f before any saccade', firstblinkbeforesac)
      }
    }
    return(NA)
}

# remove spikes at single time points
# see rmspikes(b.approx$x, opts$sac.padding)
# 20200629 thres was hardcoded at 30 (sac.padding)
# was:
#  which(zoo::rollapply(c(0,diff(ts)),2,function(x){sign(prod(x,na.rm=T))==-1 & abs(min(x,na.rm=T))>30}))
rmspikes <- function(ts, thres) {
      delta <- c(0, diff(ts))
      # change is different on both sides (spike) and is greater than threshold
      spikypointsIdx <- which(zoo::rollapply(delta, 2, function(x){
          # remove warnings about no comparison in min
          if(all(is.na(x))) return(F)
          # return diff on each side, gt thres
          sign(prod(x, na.rm=T)) == -1 &
          abs(  min(x, na.rm=T))  > thres
      }))
      ts[spikypointsIdx ] <- NA
      return(ts)
}

interp_samples <- function(ts, opts){
    ## get eye tracking for relevant portion
    b.orig <- data.frame(time=1:length(ts),x=ts) 


    ###### BLINK
    # scary blink fliter -- remove 2 samples on each side of an NA seq that is longer than 10 samples (1/6 sec)
    naSeq <- rle(is.na(b.orig$x))
    SeqStartIdx <- cumsum(naSeq$lengths) - naSeq$lengths + 1
    naStartIdx <- which( naSeq$values == T & naSeq$lengths > 10 ) 

    nastarts <- SeqStartIdx[naStartIdx]-opts$blink.trim.samples
    nastarts[nastarts<1] <- 1

    nastops  <-  SeqStartIdx[naStartIdx]+ naSeq$lengths[naStartIdx]+opts$blink.trim.samples 
    nastops[nastops>length(b.orig$x)] <- length(b.orig$x)

    start10nas <- unname(unlist(plyr::alply(cbind(nastarts,nastops),1, function(x) { x[1]:x[2]} )))
    b.cutblink <- b.orig
    b.cutblink$x[start10nas] <- NA
    #b.cuttblink$used[start10nas] <- F


    b.approx <- b.cutblink

    ###### Remove crazy points that can be nothing other than tracking errors
    # eg, xpos like: 10 10 *200* 10 10; see behbars: 10827.20100617.1.34
    if(!opts$dontcutspikes){
        b.approx$x <- rmspikes(b.approx$x, opts$sac.padding)
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
    if(length(b.approx$x)<=1) stop('no data left after removing blinks')


    ####### Estimate away NAs
    # replace NAs so we can do polyfit
    names(b.approx) <- c('time','xpos')
    b.nona <- b.approx
    b.nona$xpos <- zoo::na.approx(b.approx$xpos,x=b.approx$time) 
    
    return(namedList(b.approx, b.nona))
}

sac_pos <- function(sac.df) {
    # use position to inform extracted saccades
    # used soley by trail_pos
    # here to make it "easier" to read through code (maybe)

    # quick check. need to have gotten a few extra columns
    stopif(!c("onset","sac.time", "base.val", "sac.thres") %in%names(sac.df))

    # previously not stored in df. so extract here
    base.val <- sac.df$base.val[1]
    sac.thres <- sac.df$sac.thres[1]

    #expected mag and direction of saccade
    sac.expdir <- sign(sac.df$sac.expmag)

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
    sac.df$cordir  = sign(sac.df$endpos - sac.df$startpos) == sac.expdir
    sac.df$corpos  = abs(sac.df$endpos - sac.thres) <= opts$sac.padding
    sac.df$corside = sign(sac.df$endpos - base.val ) == sign(sac.thres - base.val)

    return(sac.df)
}


tracked_withinsac <- function(sac_df, rawts) {
  mapply(function(a,b) length(which(!is.na(rawts[a:b])))/(b-a+1), sac_df$onsetIdx, sac_df$endIdx)
}

plot_data <- function(eyedf, interp_df, sac.df, base.val, sac.exp, opts, tmin=1, tmax=nrow(eyedf)) {
    require(ggplot2)
    d <- eyedf
    d$time <- 1:nrow(eyedf)
    d <- merge(d, interp_df, by="time", suffixes = c("",".interp"))
    d <- d[tmin:tmax, ]
    d$type <- xdat_to_type(d$xdat, opts)
    print(head(d$type))
    ggplot(d) + aes(x=time, y=xpos) +
     geom_point(aes(y=xpos.interp), color="black") +
     geom_point(aes(size=dil, color=type)) +
     geom_hline(yintercept=base.val, color="green") +
     cowplot::theme_cowplot()
}

segment <- function(data_file, opts=studysettings()){
    # data_file <- './10997.20200221.1.data.tsv'
    
    eyedf <- read_eye(data_file) # this maybe replaced by arrow?

    eyedf <- clean_xdat(eyedf, opts) # bad data samples to NA
    tidxdf <- trial_indexs(eyedf, opts) # trial, start, target, stop, xdat
    interps <- interp_samples(eyedf$xpos, opts) #  remove samples around blinks, smooth

    tinfo <- partition_trials(tidxdf$target, tidxdf$stop, eyedf$xpos) 


    # TODO: do for each trial
    tidxs <- tinfo[[1]]$trgidxs
    bval  <- tinfo[[1]]$baseline
    xdat <- tidxdf$xdat[1]
    sac_df <- trial_sacs(eyedf$xpos[tidxs], interps$b.nona$xpos[tidxs], interps$b.approx$xpos[tidxs],
                     xdat, bval, opts)
    score_trial(sac_df, opts)
}

partition_trials <- function(targets, stops, xpos) {
  # how to partition trials - need baseline (from raw ts) and range of where to work
  # take raw input and target/stop indexs
  
  baselines <- sapply(FUN=function(i)  mean(xpos[c(-5:2) + i],na.rm=T), targets) 
  trgrange  <- mapply(FUN=function(a,b) c(a:(b-1)), targets, stops, SIMPLIFY=F) 
  # put these together
  lapply(1:length(baselines), function(i) list(baseline=baselines[i], trgidxs=trgrange[[i]]))
}

trial_sacs <- function(xdat, eye_xpos, t_interp_xpos, t_approx, base.val, opts){
    # get blinks, saccades, tracking
    # check for resaons to drop

    # parse data
    blinks <- find_blinks(t_approx, opts)
    sac_df <- find_saccades(t_interp_xpos, blinks, opts)
    sac_df$p.tracked <- tracked_withinsac(sac_df, eye_xpos)

    # sac.thres for anti like  reverse of 2, 87, 172 or 258 (left to right)
    sac.thres <- opts$getExpPos(xdat) 
    sac_df$sac.expmag <- sac.thres - opts$screen.x.mid
    sac_df$sac.thres <- sac.thres
    sac_df$base.val  <- base.val

    # drop?
    sac_df$Desc <- should_drop(eye_xpos, t_interp_xpos sac_df, blinks, opts)

    # use columns to setup useful info from positions
    sac_df <- sac_pos(sac_df)

    return(sac_df)
}

should_drop <- function(eye_xpos, t_interp_xpos sac_df, blinks, opts) {
    # should we drop?
    dropreason <- drop_interp(t_interp_xpos, x$base.val[1], opts)
    if(!is.na(dropreason)) return(dropreason)
    dropreason <- no_early_move(eye_xpos[1:(opts$lat.fastest*opts$sampleHz)], opts)
    if(!is.na(dropreason)) return(dropreason)
    dropreason <- drop_blink(ts, sac_df, blinks, opts)
    if(!is.na(dropreason)) return(dropreason)

    if(dim(sac_df)[1]<1 || is.na(sac_df$onset)) return('no saccades')
    # if the first sacc is too soon
    # is something the scoring function should do!? -- dropTrialSacs is easier to run from here
    if(sac_df$onset[1]<lat.fastest && !opts$nothingistoofast ) return('1st good sac too soon')
    if(abs(sac_df$startpos[1] - opts$screen.x.mid) > 50 && !opts$ignorefirstsacstart)
      return('start pos too far from center fix')
    goodsacsIdx <- with(sac_df,{intime & gtMinLen & p.tracked > sac.trackingtresh})
    if(length(goodsacsIdx)<1) return('no good saccades')

    if(!any(sac_df$onset < opts$sac.time)) {
        return(sprintf('no saccades within sac.time(%.3f)', opts$sac.time))

    return(NA)
}

read_eye <- function(data_file, verbose=T){
  # read tsv file created by dataFromAnyEyd.pl
  # empty but with correct columns
  default <- data.frame(xdat=NA,dil=NA,xpos=NA,ypos=NA)

  if(verbose) cat('using ', data_file ,'\n')

  ### load data
  #load xdat, eye position and dialation data for parsed data_file
  d <- tryCatch(read.table(data_file, sep="\t",header=T),
                             error=function(e){
                                 cat(sprintf("# error! cant read input '%s': %s\n", data_file, e))
                                 return(default)})

  if(dim(d)[2] != 4) return(default)
  names(d) <- c("xdat","dil","xpos","ypos")
  return(d)
}

clean_xdat <- function(d, opts) {
  # replace strings of zeros with prevous xdat
  # zero out values that are too hight (blinks)
  # input is dataframe with xdat xpos ypos dil 
  # see read_eye
  # opts for xmax, ymax, and useextremefilter

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

     if(length(shouldbeXdat) != length(zeroIdxinD) )
         warning('zero xdats and replacement not the same length!! something very funny is going on')
     for( i in 1:length(shouldbeXdat)){ d$xdat[zeroIdxinD[[i]]] <- shouldbeXdat[i]}
  }

  
  ##### Remove bad data #####
  # both x and y are out of range, or there is no dilation measure
  # TODO!!
  # if there is x and ypos and dil != 0
  # maybe we should keep?

  # see scannerbars 10711.20121108.1.17 -- good xpos, no good dil or ypos
  
  if(opts$useextremefilter){
   badidxs=d$xpos>opts$xmax|d$ypos>opts$ymax|d$dil==0
  }else{
   # allow for some over/under shooting (xmax+50->xmax, -50 -> 0)
   allowedovershoot <- 0
   d$xpos[d$xpos>opts$xmax&d$xpos<opts$xmax+allowedovershoot] <- opts$xmax
   d$xpos[d$xpos<0&d$xpos>-allowedovershoot] <- 0

   badidxs=d$xpos>opts$xmax|(d$xpos==0&d$ypos==0&d$dil==0)
  }
  d[which(badidxs),c('dil','xpos','ypos')] <- c(NA,NA,NA)
  if(any(na.omit(d$dil)<1)) { d$dil[which(d$dil<1)]<-1 }# so we can see something! 
  # which is need to not die on scanbars 10656.20090410.2
  return(d)
}

get_targets <- function(d, opts) {
  # deperacatd?

  # remove repeats
  xdats             <- rle(d$xdat)            # run length encode
  xdats$cs          <- cumsum(xdats$lengths)  # index in d where xdat happends
  
  #### Split eye positions by target code
  # use only targetcodes that are preceded by startcodes and followed by end codes
  # this should let us ingore  catch trials (no target)
  tarCodePos       <- which(xdats$values %in% opts$targetcodes)
  goodTargPos.start <- tarCodePos[  xdats$values[tarCodePos  - 1]  %in%  opts$startcodes ]
  goodTargPos      <- goodTargPos.start[ xdats$values[goodTargPos.start + 1]  %in%  opts$stopcodes  ]
  
  # x by 2 matrix of target onset and offset indicies
  targetIdxs       <- cbind(xdats$cs[goodTargPos-1],xdats$cs[goodTargPos])


  if(length(goodTargPos) <= 0) {
    #allsacs <- dropTrialSacs(subj,runtype,runontrials,0,'no understandable start/stop xdats!',allsacs,showplot=F,run=run,rundate=rundate)
    #return() 
    stop('no understandable start/stop xdats!')
  }

  if(! length(goodTargPos) %in% opts$expectedTrialLengths ) {
    cat('WARNING: unexpected num of trials: ',
        length(goodTargPos), '!%in% ',
        paste(collaspe=" or ", opts$expectedTrialLengths),
        '\n')
  }

  return(targetIdxs)
}

xdat_to_type <- function(xdat, opts) {
  type <- 2*(xdat %in% opts$startcodes) +
          4*(xdat %in% opts$targetcodes) +
          8*(xdat %in% opts$stopcodes)
  #0=unknown, 2=start, 4=target, 8=stop
  type <- factor(type, c(0,2,4,8), c('unknown','start','target','stop'))
}

trial_indexs <- function(d, opts) {
  # return: index xdat type trial xdat(target)
  #
  # 20200629 - replaces get_targets ?
  xdats             <- rle(d$xdat)            # run length encode
  xdats$cs          <- cumsum(xdats$lengths)  # index in d where xdat happends

  # remove repeats
  trials      <- rle(d$xdat)
  if(!any(trials$values %in% opts$targetcodes)) stop('no targetcodes in data!')
  # make dataframe
  trials      <- data.frame(index=trials$lengths,
                            xdat=trials$values)
  # get index of change. start at 1st.
  trials$index<- c(1, head(cumsum(trials$index), -1)) + 1

  # set types
  trials$type <- xdat_to_type(trials$xdat, opts)
  # on recording stop
  # if last xdat is 254 and there is a stop before it
  # remove last
  if(trials$xdat[nrow(trials)]==254 & trials$type[nrow(trials)-1]=='stop')
      trials <- trials[-nrow(trials),]

  trials$trial<- cumsum(trials$type == 'start')
  # remove any unknown
  trials <- trials[!is.na(trials$type), ]
  trials <- trials[trials$type!='unknown',]

  # transform to wide
  # merge with target xdat
  # or return an error message
  tryCatch({
      wide <- tidyr::spread(trials[,c("index","type","trial")], type, index)
      merge(wide, trials[trials$type=='target',c('trial','xdat')], by='trial')
     }, error=function(e)
      stop("cannot create start,target,stop pairing: %s", e$message))
}

score_trial <- function(x, opts, failreason=NA) {
     if (opts$trialIsType(x$xdat[1]) == 'FX')  {
       return(count_as_score_fix(x$Desc))
     } 

     # now select only the good saccades
     goodsacsIdx <- which(with(x,{intime & gtMinLen & p.tracked > opts$sac.trackingtresh}))
     goodsacs <- x[goodsacsIdx, ]
     x <- goodsacs

     # if we want to drop
     if(!is.na(failreason)){
       cat('dropped at trial level:', x$subj[1],x$trial[1], failreason,'\n');
       y <- x[1,]
       y[1,] <-NA
       y$trial <- x$trial[1]
       y$xdat <- x$xdat[1]
       x <- y
       #return( dropTrialScore(x$trial[1],failreason, xdat=x$xdat[1], AS=xdatIsAS(x$xdat[1]) ) )
     } else {
         # from NA to ''
         failreason=''
     }
     ### EVERYTHING IS GOOD SO FAR
     # break into trials, do we have 
     #  1) a first correct movement?
     #  2) correct movement after incorrect?
     #  3) an xdat that says Anti Saccade?
     data.frame( 
        trial=x$trial[1],
        xdat=x$xdat[1],
        lat=round(x$onset[1]*1000),
        fstCorrect=x$cordir[1],
        ErrCorr=!x$cordir[1] &
                 length(x$cordir)>1 &
                 (any(x$MaxMinX[-1]) || any(x$corside)),
        AS=opts$trialIsType(x$xdat[1]),
        Desc=failreason)
}

count_as_score <- function(lat, frstCor, ErrCorr){
  # get a "count" score for saccades
  # default to dropped
  count <- rep(-1, length(lat))
  # set count "num sacs til correct movement" 
  # -1 drop, 0 error, 1 correct, 2 error corrected
  #count[is.na(lat )]  <- -1
  count[!is.na(lat )] <- 0
  count[fstCorrect]   <- 1
  count[ErrCorr   ]   <- 2
  return(count)
}

count_as_score_fix <- function(desc){
  # score fixation -- if no saccades drop reason. consider it good
  count <- rep(-1, length(desc))
  count[grepl("no saccades",desc)] <- 1;
  count[desc==""]                  <- 0;
  count[desc=="no good saccades"]  <- 1;
  count[grepl("no saccades within sac.time", desc) ] <- 1;
  return(count)
}
