# 20200503WF - attempt to extract from complex mess I've made
# using parquet/arrow format for quick reads (maybe)
library(arrow)
ds <- read_parquet("./anti.parquet")




getSacs <- function(ts, sac.thres, screen.x.mid=??){
    # sac.thres for anti like  2, 87, 172 or 258 (left to right)
    # 20200503 - init - copy of ScoreRun.R:getSacs
  
    # 20171020 -- sacloc task has some information in the startcode
    # startcode:30 |+ target:131 => 30131
    # if( exists("CAREABOUTSTARTCODE") && CAREABOUTSTARTCODE ) {

    # getExpPos comes from *.settings.R -- so does sac.thresholds (shouldn't have specified this, but thats how it is now :(  )
    # where the eye should look (opposite of dot if AntiSac, loc of dot if pro. sac)
    ## export to global scopefor plotting

    #expected  mag and direction of saccade
    sac.expmag <- sac.thres - screen.x.mid
    sac.expdir <- sign(sac.expmag)
    sac.expmag <- abs(sac.expmag)


    # have "d" and "targetIdxs" in global scope
    # will export b.orig and b.approx too
    # returns "b" (time,xpos) with blinks removed
    #   or reason for falure
    b.all <- interoplateSamples(trl,funnybusiness)
    if(is.character(b.all)){
     failreason <- b.all
     allsacs <- dropTrialSacs(subj,runtype,trl,xdatCode,failreason,allsacs,showplot=showplot,run=run,rundate=rundate,savedas=pdffname, writetopdf=writetopdf)
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
     allsacs <- dropTrialSacs(subj,runtype,trl,xdatCode,'< 30% tracking for samples of interest',allsacs,showplot=showplot,run=run,rundate=rundate,savedas=pdffname, writetopdf=writetopdf)
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
                   allsacs,run=run,showplot=showplot,rundate=rundate,savedas=pdffname, writetopdf=writetopdf)
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
                     allsacs,run=run,showplot=showplot,rundate=rundate,savedas=pdffname, writetopdf=writetopdf)
     next
    }

    # is tracking smooth?
    xposStdDevMax <- 40
    averageChange <- sd(abs(diff(na.omit(d$xpos[trgt[3:(sac.majorRegionEnd*sampleHz) ]  ]))))
    if(is.na(averageChange) || (averageChange > xposStdDevMax && !any(grepl('ignorexpossd',funnybusiness)) )   ) {
     allsacs <-  dropTrialSacs(subj,runtype,trl,xdatCode,
                     sprintf('poor tracking: sd of xpos Δ=%f',averageChange),
                     allsacs,run=run,showplot=showplot,rundate=rundate,savedas=pdffname, writetopdf=writetopdf)
     next
    }

   # target codes didn't match, we ate the whole file
   # or way too many
   # 200 might be too low of a threshold
   if (   length(b.all$x ) > 200 ) {
     dropTrialSacs(subj,runtype,trl,xdatCode,paste('too many samples in trial', dim(b.all$x)[1]),allsacs,showplot=showplot,run=run,rundate=rundate,savedas=pdffname, writetopdf=writetopdf) 
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
                 allsacs,showplot=showplot,run=run,rundate=rundate,savedas=pdffname, writetopdf=writetopdf)
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
     allsacs <- dropTrialSacs(subj,runtype,trl,xdatCode,'no saccades (getSacs)',allsacs,run=run,showplot=showplot,rundate=rundate,savedas=pdffname, writetopdf=writetopdf)
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
                          allsacs,run=run,showplot=showplot,rundate=rundate,savedas=pdffname, writetopdf=writetopdf)
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
        allsacs <-  dropTrialSacs(subj,runtype,trl,xdatCode,'blink ends before any saccades',allsacs,run=run,showplot=showplot,rundate=rundate,savedas=pdffname, writetopdf=writetopdf)
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
                      allsacs,run=run,showplot=showplot,rundate=rundate,savedas=pdffname, writetopdf=writetopdf)
        next
      }
    }

    #browser()
    ## done cacluating things - plot and save? 

    
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
       if(is.null(pdffname))
         pdffname<-sprintf('%s/img/%d.%d.%d.%02d-%d.pdf',outputdir,subj,rundate,run,trl,xdatCode)

       savePlots(sac.df,g,drv,pdffname,writetopdf)
    }

    # return everying with subject info
    allsacs <- rbind(allsacs,
                   data.frame(
                     subj=subj,run=run,trial=trl,
                     sac.df[,-c(1:3)], xdat=xdatCode,reason=NA
                   )
                )
  }
} 
