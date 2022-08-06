library(tidyr)
library(dplyr)
options(dplyr.summarise.inform = FALSE) # okay with overwritting group_by with another
# IRanges is in Bioconductor:
# install.packages("BiocManager"); BiocManager::install("IRanges")
# also need
#  install.packages("eyelinker") # used to read asc file


# #./220682rr01.asc
# # 9526649   641.7   720.5  1891.0   691.5   652.4  2264.0   127.0 .....
# d <- read.table(na.strings=".", text=system(intern=T, glue::glue("perl -F'\\t' -slane 'print if /^[0-9]/ and $#F == 8' {asc_fname}")))
# #names(d) <- c("time", "gx1", "gy1", "V4","gx2","gy2","V7", "input", "hdata")
# names(d) <- c("time", "xpl", "ypl", "psl","xpr","ypr","psr", "input", "hdata")
# # looking at matlab data to compare
# # m = load("./ras.mat")
# # for f =fieldnames(m.ras(1).FSAMPLE)'; fprintf("%s %s\n", f{1}, sprintf("%.2f,",m.ras(1).FSAMPLE.(f{1})(:,90000))); end


extract_events<-function(eyets){
  # event intervals (4 per trial, long format)
  # from GK 2022-04-15 (encoded in eprime3 task):
  #   Message Trial Event TrialType         DotPosition1+2
  #   TRIALID   2       0 Reward            1406    R-27
  #   EventID   2       1 Fixation Cross
  #   EventID   2       2 Ring ($ or #)
  #   EventID   2       3 Fixation Cross
  #   EventID   2       4 Yellow Dot
  #   EventID   2       5 End of Trial (Yellow Dot Offset)
  #   TRIAL OK
  events <- eyets$msg %>%
      # remove task administrator notes (InputId Awake Droop Intrv) and end of TRIAL OK
      # could maybe just grab TRIALID|EventID
      filter(!grepl("^InputID|OK$",text)) %>%
      # text encodes all the event information. pull it into separate columns
      separate(text, c("msg", "trial","event","ttypenum", "desc"), extra="merge") %>%
      # only TRIALID has rewtype, dotpos, & L/R-#
      # na.locf will forward fill the missing 'desc' info into EventID rows
      mutate(desc=zoo::na.locf(desc)) %>%
      # with desc filled down from TRIALID into all EventIDs, we can break it apart
      separate(desc, c("rewtype","dotpos", "tdir", "tnum")) %>%
      # trialid and fist event id are same timpeont
      # and then all msg are EventID so we dont need the column
      filter(msg != "TRIALID") %>%
      select(-msg) %>%
      # create interval
      mutate(event_endtime=lead(time, default=max(time))-1) %>%
             # iranges cant live within the tibble/dataframe :(
             #erange=IRanges::IRanges(start=time,end=endtime))
      # text field seperate treated as chars. but most are nums
      mutate_each(as.numeric, trial, event, ttypenum, dotpos, tnum)
}

merge_sacs <- function(events, saccs){
    #saccs <- eyets$sacc
    # identify sacs that overlap with when the dot is displayed
    # create long dataframe combines sac info for dot event (repeated if multiple sacs per event)
    event_ranges <- with(events, IRanges::IRanges(start=time,end=event_endtime))
    sac_ranges <-with(saccs, IRanges::IRanges(start=stime,end=etime))
    overlaps <- IRanges::findOverlaps(sac_ranges, event_ranges)
    sac_during_event <- cbind( saccs[overlaps@from,], events[overlaps@to,] )
}


## scoring
score<-function(val, desc="") return(list(score=val, desc=desc))
score_trial <- function(lat, dur, sacdist, dotpos, screen_x_res=1920){
  # trial score a value -1 to 2. can think of as number of saccades until correct
  # -1 dropped trial,  0 = error, 1 = correct, 2 = error corrected
  # returning list of [1] score value [2] drop explination/description
  # use vectors of saccade
  #  latencies, duration, and distance
  # with single value
  #  dotpos - correct saccade is away from this position
  #  screen_x_res - how wide the screen is (default to 1920)
  #
  # drop if was already saccading or moved before could be reacting to dot
  if(min(lat) < 60) return(score(-1, "early saccade"))
  # maybe we care about saccade duration
  # ignore saccads that are short (sacdist. TODO and dur?)
  # could merge close together short saccades?
  consider_idx<- abs(sacdist)>20
  if(!any(consider_idx)) return(score(-1, "no saccades"))
  if(any(is.na(consider_idx))) return(score(-1, "NA values in sac distance!?"))
  if(is.na(dotpos)) return(score(-1, "dot position unknown!?"))

  # expect to look opposite dot position
  expect_dir <- -1* sign(dotpos[1]-screen_x_res/2)

  # TODO: drop if not starting close to center

  if(expect_dir == sign(sacdist[consider_idx][1])) return(score(1))

  # all saccades of any real length are the wrong direction? incorrect
  if(all(expect_dir != sign(sacdist[consider_idx]))) return(score(0))

  # first saccade is the wrong way. but there are some that go the correct direction
  # TODO: should maybe check that we didn't just go back to center (range(exp))
  return(score(2))
}

score_file<-function(asc_fname='example/220682rr01.asc.gz', dot_event=4){
    dat<- eyelinker::read.asc(asc_fname)
    events <- extract_events(dat)
    #dat$info$screen.x # 1920

    # TODO: is dot event encoded as 4? it has the fewest negative saccades
    dot_events <- events %>% filter(event==dot_event) 
    sac_during_dot_all <- merge_sacs(dot_events, dat$sacc)
    sacs_during_dot <- sac_during_dot_all %>%
      select(trial, dot_onset=time, sac_onset=stime, rewtype,
             dotpos, tdir, sxp, exp, dur) %>%
      mutate(lat=sac_onset-dot_onset, sacdist=exp-sxp)

    scored_run <- sacs_during_dot %>%
      group_by(trial, dotpos, rewtype) %>%
      summarise(lat=lat[1],
                nsac=n(),
                leftmost=min(c(sxp,exp)),
                rightmost=max(c(sxp,exp)),
              score_desc=as.data.frame(score_trial(lat,dur,sacdist,dotpos))) %>%
      unpack(score_desc)
}

run_stats <- function(scored_run){
    run_score %>%
        group_by(score) %>%
        summarise(n=n(), lat_m=mean(lat,na.rm=T), lat_sd=sd(lat,na.rm=T))
}



ascfile_score_stats <-function(){
    run_stats(score_file(asc_fname='example/220682rr01.asc.gz'))
}

# instead of a row per range. get a row per sample
# repeat each row for each sample in the range and make a new time column
rep_samples <- function(events) {
    events_rep <- events %>%
        rename(stime=time) %>%
        mutate(nsamples=event_endtime-stime +1) %>%
        filter(nsamples>0) %>%
        uncount(nsamples) %>% 
        group_by(stime) %>%
        mutate(time=stime -1 + 1:n()) %>%
        ungroup
}

# create simulated TTL trigger (one byte 0-255 to encode event state)
# care about reward type, event in trial, and dotposition
#  in old scorer settings for RingReward:
# /Volumes/Hera/Projects/autoeyescore/RingReward/RingReward.settings.R
#  startcodes   <- c(2,4,6,8)*10
#  targetcodes  <- c(160:190)
#  stopcodes    <- c(0,250)
#  expectedtarget 
#       c(sac.right.large, sac.right.mid, sac.right.small  )
#         sac.left.small, sac.left.mid, sac.left.large )
#       [ as.numeric(substr(xdatCode,3,3)) ] 
#  if     (xdat<170) { return('ASNue') }
#  else if(xdat<190) { return('ASRew') }
#  else if(xdat<200) { return('ASPun') }
xdat <- function(rewtype, dotpos, eventnum, target_num=4, x.screen.res=1920) {

 rewtype_int <- ifelse(rewtype=="Reward", 80, 60)
 # count dot position 1 to 6 right to left
 dot_int <- as.numeric(cut(dotpos/x.screen.res,
            #  0.05  0.13  0.27  0.73 0.87 0.95
     breaks=c(0 , .12 , .25 , .4  , .8 , .9 , 1)))

 # after target, trial ended send stop code
 case_when(
   eventnum > target_num ~ 250,
   eventnum<target_num ~ rewtype_int,
   eventnum == target_num ~ 100 + rewtype_int + dot_int,
   TRUE ~ 0)
}

# have 245Hz = sample every 4ms. 
# old data is 60Hz.        10ms
approx60Hz<-function(ts, time,hz=60, ...){
  # ... useful for method="constant" for xdat
  #NB. ts expected in milliseconds
  xout <- seq(min(time), max(time), by=1000/hz)
  stats::approx(x=time,y=ts, xout=xout, ...)$y
}

trial_from_xdat <-function(xdat) cumsum(c(T,diff(xdat)!=0)&xdat<100)
asc_as_asl<-function(asc_fname){
    #asc_fname='example/220682rr01.asc.gz'
    dat<- eyelinker::read.asc(asc_fname)
    gaze <- list(x=dat$info$screen.x, y=dat$info$screen.y)
    origHz <- dat$info$sample.rate
    
    # time is in ms
    # 4    == dat$raw$time %>% head %>% diff %>% unique
    # .004 == 1/origHz

    events_rep <- dat %>%
        extract_events %>%
        mutate(XDAT=xdat(rewtype, dotpos, event)) %>%
        rep_samples
    raw_events <- merge(dat$raw, events_rep, by="time") %>% arrange(time)
        
    # rescale to asl grid: 262x240. y doesn't matter other than it cant be super large
    # gaze and screen are the same. that doesn't have to be true?
    # minus one on the EyeLink side b/c 0 is valid position
    to_asl_x = 261/(gaze$x-1)
    to_asl_y = 240/(gaze$y-1)

    # set names and resample grid
    asl_coord <- raw_events %>%
        mutate(horz_gaze_coord=xpr*to_asl_x,
               vert_gaze_coord=ypl*to_asl_y) %>%
        select(time, XDAT, pupil_diam=ypr, horz_gaze_coord, vert_gaze_coord)
    
    # probably an xts+zoo way to do this elegantly.
    # but cant figure it out. so same approx call on each column
    asl_60hz <- with(asl_coord, data.frame(
              time            = approx60Hz(time, time) %>% round, # lazy. should use $x
              XDAT            = approx60Hz(XDAT, time, method="constant"),
              pupil_diam      = approx60Hz(pupil_diam, time),
              missing         = approx60Hz(1*!is.na(horz_gaze_coord), time, method="constant") < 1,
              # NA constant interpolation didn't work as expected? (20220429)
              #missing         = is.na(approx60Hz(horz_gaze_coord, time, method="constant")),
              horz_gaze_coord = approx60Hz(horz_gaze_coord, time),
              vert_gaze_coord = approx60Hz(vert_gaze_coord, time)))

    ## blinks do not overlap with missing raw data!
    ## -- 20220429 probably because I'm mixing L/R eye blinks and (prev) the worse of the eyes (left)
    ## removing those regions just makes it harder to see whats happening
    # # set blink samples to 0.
    # # reuse eyelinks blink segment across both eyes
    # # might cause a problem later (pick eye=="L" or "R")
    # asl_60hz_blink <- dat$blinks %>%
    #     select(time=stime,dur) %>%
    #     mutate(isblink=TRUE) %>%
    #     uncount(dur) %>%
    #     left_join(asl_60hz,., by="time")

    # asl <- asl_60hz_blink %>%
    #     mutate_at(vars(pupil_diam,horz_gaze_coord, vert_gaze_coord),
    #               ~ifelse(is.na(isblink),., 0)) %>%
    #     select(-time, -isblink)

    asl_rmmissing <- asl_60hz %>%
        mutate(across(c(pupil_diam, horz_gaze_coord, vert_gaze_coord),
                      ~ifelse(missing,0, .x)))

    asl <- asl_rmmissing %>% select(-time, -missing)
    return(asl)

    ## dead code below. plot for resample visual QC 
    theme_set(cowplot::theme_cowplot())
    
    # what does the raw eyetrack look like
    plot_raw <- raw_events %>% filter(event==4) %>%
        group_by(trial) %>%
        mutate(time=time-min(time))
    ggplot(plot_raw) + aes(y=xpr, x=time, color=trial) +
        geom_point() +
        geom_hline(aes(NULL), yintercept = unique(raw_events$dotpos),color="red") +
        ggtitle("xgaze @ event=4")

    # resample overlayed on original. randomly hardcoded trials
    plot_resample <- rbind(
        asl %>% mutate(time=min(asl_coord$time)+1000/60*(0:(nrow(asl)-1)),
                       res="60Hz", trial=trial_from_xdat(XDAT)),
        asl_coord %>% mutate(res="250Hz", trial=trial_from_xdat(XDAT))) %>%
        group_by(res,trial) %>% mutate(time=time-min(time))

    ggplot(plot_resample %>%
           filter(trial %in% c(2,5,20), time<9000)) +
        aes(x=time, y=horz_gaze_coord, shape=as.factor(XDAT), linetype=as.factor(XDAT),
            color=res, group=paste(XDAT,res)) +
        geom_point() + geom_path() +
        facet_grid(~trial) +
        #facet_grid(res~trial) +
        ggtitle("SR as ASL: interpolation check on random trials")
}


old_scorer <- function(asl){
   source("../ScoreRun.R")
   sacs <- getSacs(asl, subj=1, run=1, runtype=AS,rundate=0, save_scored=F)
}

restrict_range <- function(d, t_start, t_end){
   d %>% filter(stime <= t_end,
                etime >= t_start) %>%
      mutate(across(c(etime, stime), ~ .x - t_start))
}


inspect_trial <- function(dat, only_trial, desc="") {
    #asc_fname='example/220682rr01.asc.gz'
    #dat<- eyelinker::read.asc(asc_fname)
    if("character" %in% class(dat)) dat <- eyelinker::read.asc(dat)
    t_events <- extract_events(dat) %>% filter(trial==only_trial)
    t_start <- min(t_events$time)
    t_end <- max(t_events$event_endtime)

    t_raw <- filter(dat$raw, time>=t_start, time<= t_end) %>%
        mutate(time=time-t_start)

    t_b <- restrict_range(dat$blinks, t_start, t_end)
    t_s <- restrict_range(dat$sacc, t_start, t_end)

    t_event_plot <- t_events %>%
        mutate(across(c(time,event_endtime), ~.x-t_start),
               event=factor(event,levels=1:5,
                            labels=c("fix","ring","fix2","dot","end")))
    
    ggplot(t_raw) +
        aes(xmin=stime, xmax=etime)+
        geom_rect(data=t_b, aes(fill=eye, ymin=-100, ymax=-200),
                  alpha=.5, color="red")+
        geom_rect(data=t_s, aes(fill=eye, ymin=0, ymax=-100),
                  alpha=.5, color="blue")+
        geom_vline(data=t_event_plot,
                   aes(xintercept=time, color=event))+
        geom_hline(data=t_event_plot[1,],
                   aes(yintercept=dotpos),color="yellow", size=3)+
        geom_hline(yintercept=1920/2,color="green",linetype=2)+
        geom_point(aes(x=time, y=xpr, xmin=NULL, xmax=NULL), color="blue",alpha=.5)+
        geom_point(aes(x=time, y=xpl, xmin=NULL, xmax=NULL), color="red",alpha=.5)+
        coord_cartesian(ylim=c(-200,1920),xlim=c(0,t_end-t_start))+
        ggtitle(sprintf("trial %d %s", only_trial, desc))
}

local_tests<-function(){
    library(testthat)
    test_that("scores",{
        # dot pos = 96 259 514 | 1406 1661 1824  
        # score_trial <- function(lat, dur, sacdist, dotpos, screen_x_res=1920){
        Rdot <- 1824
        Ldot <- 96
        expect_equal(1, score_trial(lat=300,dur=100,sacdist=-100, dotpos=Rdot)$score)
        expect_equal(1, score_trial(lat=300,dur=100,sacdist=100, dotpos=Ldot)$score)

        expect_equal(0, score_trial(lat=300,dur=100,sacdist=100, dotpos=Rdot)$score)
        expect_equal(0, score_trial(lat=300,dur=100,sacdist=-100, dotpos=Ldot)$score)

        durs<-c(100,100)
        dists<-c(100,-100)
        expect_equal(2, score_trial(lat=300,dur=durs,sacdist=dists, dotpos=Rdot)$score)
        expect_equal(2, score_trial(lat=300,dur=durs,sacdist=-dists, dotpos=Ldot)$score)
        # TODO: untested and unhandled
        # correct direction but started in the wrong place
    })

    test_that("merge ranges",{
        # overlap early, within, within again, goes over
        events_t <- data.frame(time       =c(10,100,     200),
                             event_endtime=c(15,150,     250), e=c("A","B","C"))
        sacs_t <- data.frame(        stime=c(1, 105, 120,245),
                                     etime=c(15,109, 130,300), s=c("W","X","Y","Z"))
        merge_t <- merge_sacs(events_t, sacs_t)
        pairs <- with(merge_t, paste0(e,s))
        expect_equal(c("AW","BX","BY", "CZ"), pairs)
    })

    test_that("rep event range",{
        events_t <- data.frame(time       =c(10,100,     200),
                               event_endtime=c(15,150,     250), lab=c("A","B","C"))
        r <- rep_samples(events_t)
        r_stat <- r %>% group_by(lab, event_endtime, stime) %>%
            summarise(mx=max(time), mn=min(time), n=n())

        expect_true(all(r_stat$mx == r_stat$event_endtime))
        expect_true(all(r_stat$mn == r_stat$stime))
        expect_equal(6, with(r_stat, n[lab=="A"]))
        expect_equal(51, with(r_stat, n[lab=="B"]))
        expect_equal(51, with(r_stat, n[lab=="C"]))
    })
    
    test_that("approx 60Hz",{
      timeeg <- seq(1,500,1000/250)
      expect_equal(approx60Hz(timeeg,timeeg), seq(1,500,by=1000/60))
    })

    test_that("xdats",{
        expect_equal(80, xdat("Reward", 1824, 1))
        expect_equal(186, xdat("Reward", 1824, 4))

        expect_equal(60, xdat("Neut",96, 1))
        expect_equal(161, xdat("Neut",96, 4))
        expect_equal(250, xdat("Neut",96, 5))

        expect_equal(c(250,80), xdat(c("Neut","Reward"),96, c(5,1)))
    })
    
   ranges <- data.frame(stime=c(0,5,15),
                        etime=c(2,8,20))
   rr_res       <- restrict_range(ranges, 4, 6)
   rr_res_early <- restrict_range(ranges, 6, 8)
   rr_res_late  <- restrict_range(ranges, 4, 6)
   test_that("restrict range",{
      expect_equal(1, nrow(rr_res))
      expect_equal(1, rr_res$stime)
      expect_equal(4, rr_res$etime)

      expect_equal(1, nrow(rr_res_early))
      expect_equal(-1, rr_res_early$stime)

      expect_equal(1, nrow(rr_res_late))
   })
}
