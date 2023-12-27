#
#

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


into_asl_tests<-function(){
    library(testthat)
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
}
