library(tidyr)
library(dplyr)
options(dplyr.summarise.inform = FALSE) # okay with overwritting group_by with another
# IRanges is in Bioconductor:
# install.packages("BiocManager"); BiocManager::install("IRanges")
# also need
#  install.packages("eyelinker") # used to read asc file

source('eprime3_dollarrward.R') # extract_EP3_DR_events, score_file_ep3_dr
source('./vgs.R')               # extract_vgs_events,    score_file_vgs

# main wrapper:
#   ascfile_score_stats
# needs task specific extention of score_file_generic (e.g. score_file_func=score_file_ep3_dr)



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

# what side of the screen are we looking at?
#  screen_x_res - how wide the screen is (default to 1920)
find_expect_dir <- function(dotpos, screen_x_res=1920) -1* sign(dotpot-screen_x_res/2)

# wrapper for score and reason. unlisted into dataframe columns later
score<-function(val, desc="") return(list(score=val, desc=desc))

score_trial <- function(lat, dur, sacdist, dotpos, find_expect_dir=find_expect_dir){
  # trial score a value -1 to 2. can think of as number of saccades until correct
  # -1 dropped trial,  0 = error, 1 = correct, 2 = error corrected
  # returning list of [1] score value [2] drop explination/description
  # use vectors of saccade
  #  latencies, duration, and distance
  # find_expect_dir is a function that uses first dotpos to set side of correct saccade
  #  for anti/dollar reward: correct saccade is away from this position
  #
  # drop if was already saccading or moved before could be reacting to dot
  if(min(lat) < 60) return(score(-1, "early saccade"))
  # maybe we care about saccade duration
  # ignore saccads that are short (sacdist. TODO and dur?)
  # could merge close together short saccades?
  consider_idx<- abs(sacdist)>20
  if(!any(consider_idx)) return(score(-1, "no saccades"))
  if(any(is.na(consider_idx))) return(score(-1, "NA values in sac distance!?"))
  if(any(is.na(dotpos))) return(score(-1, "dot position unknown!?"))

  # expect to look opposite dot position
  expect_dir <- find_expect_dir(dotpos[1])

  # TODO: drop if not starting close to center

  if(expect_dir == sign(sacdist[consider_idx][1])) return(score(1))

  # all saccades of any real length are the wrong direction? incorrect
  if(all(expect_dir != sign(sacdist[consider_idx]))) return(score(0))

  # first saccade is the wrong way. but there are some that go the correct direction
  # TODO: should maybe check that we didn't just go back to center (range(exp))
  return(score(2))
}

# asc_fname="./example/sub-wf_sendmsg/ses-01/20231226_VGSEye/sub_wf_sendmsg_ses_01_task_VGS_run_1_20232326152333.asc.gz"
# expanded in eprime3_dollarrward.R:score_file_ep3_dr or vgs.R:score_file_vgs
# based on eprime3_dollarrward.R
score_file_generic <-function(asc_fname='example/220682rr01.asc.gz',
                     dot_event=4,
                     extract_events=extract_EP3_DR_events,
                     groups=c("rewtype","tdir"),
                     find_expect_dir=find_expect_dir){
    dat<- eyelinker::read.asc(asc_fname)
    events <- extract_events(dat)
    #dat$info$screen.x # 1920

    # TODO: is dot event encoded as 4? it has the fewest negative saccades
    dot_events <- events %>% filter(event==dot_event) 
    sac_during_dot_all <- merge_sacs(dot_events, dat$sacc)
    sacs_during_dot <- sac_during_dot_all %>%
      select(trial, dot_onset=time, sac_onset=stime,
             dotpos, !!groups,
             sxp, exp, dur) %>%
      mutate(lat=sac_onset-dot_onset, sacdist=exp-sxp)

    scored_run <- sacs_during_dot %>%
      group_by(trial, dotpos, !!!rlang::syms(groups)) %>%
      summarise(lat=lat[1],
                nsac=n(),
                leftmost=min(c(sxp,exp)),
                rightmost=max(c(sxp,exp)),
              score_desc=as.data.frame(score_trial(lat,dur,sacdist,dotpos, find_expect_dir))) %>%
      unpack(score_desc)
}

run_stats <- function(scored_run){
    run_score %>%
        group_by(score) %>%
        summarise(n=n(), lat_m=mean(lat,na.rm=T), lat_sd=sd(lat,na.rm=T))
}



#### MAIN WRAPPER
# score_file_func likley one of eprime3_dollarrward.R:score_file_ep3_dr or vgs.R:score_file_vgs
ascfile_score_stats <-function(asc_fname, score_file_func){
    run_stats(score_file_func(asc_fname))
}

# instead of a row per range. get a row per sample
# repeat each row for each sample in the range and make a new time column

eyelink_functions_test<-function(){
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
}
