library(ggplot2)
source('eyelink_functions.R') # vgs:extract_vgs_events

restrict_range <- function(d, t_start, t_end){
   d %>% filter(stime <= t_end,
                etime >= t_start) %>%
      mutate(across(c(etime, stime), ~ .x - t_start))
}


# extract_events from eyelink_functions.R
inspect_trial <- function(dat, only_trial=1, trial_desc="",
                          extract_events=extract_vgs_events,
                          events=NULL) {
    #asc_fname='example/220682rr01.asc.gz'
    #dat<- eyelinker::read.asc(asc_fname)
    if("character" %in% class(dat)) dat <- eyelinker::read.asc(dat)
    t_events <- extract_events(dat) %>% filter(trial == only_trial)
    t_start <- min(t_events$time)
    t_end <- max(t_events$event_endtime)

    t_raw <- filter(dat$raw, time>=t_start, time<= t_end) %>%
        mutate(time=time-t_start)

    t_b <- restrict_range(dat$blinks, t_start, t_end)
    t_s <- restrict_range(dat$sacc, t_start, t_end)


    events <- unique(t_events$event)
    t_event_plot <- t_events %>%
        mutate(across(c(time,event_endtime), ~.x-t_start),
               #event=factor(event, levels=seq_along(events), labels=events))
               event=as.factor(event))

    #if(!'eye' %in% names(t_raw)) t_raw$eye <- 'mono'
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
        ggtitle(sprintf("trial %d %s", only_trial, trial_desc)) + theme_bw()
}

inspect_trial_test <- function(){
  asc_fname <- "example/sub-wf_sendmsg/ses-01/20231226_MGSEye/sub_wf_sendcmd_ses_01_task_MGS_run_1_20233426153455.asc.gz"
  inspect_trial(asc_fname, 3, extract_events=extract_mgs_events)
}
