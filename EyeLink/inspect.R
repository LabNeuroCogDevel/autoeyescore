library(ggplot2)
source('eyelink_functions.R') # vgs:extract_vgs_events

restrict_range <- function(d, t_start, t_end){
   d %>% filter(stime <= t_end,
                etime >= t_start) %>%
      mutate(across(c(etime, stime), ~ .x - t_start))
}

paste00 <- function(...) paste0(collapse=",",...)

# extract_events from eyelink_functions.R
inspect_trial <- function(dat, only_trial=1, trial_desc="",
                          extract_events=extract_vgs_events,
                          only_events=NULL, score_fnc=NULL, max_x=NULL,
                          score_df=NULL) {
    #asc_fname='example/220682rr01.asc.gz'
    #dat<- eyelinker::read.asc(asc_fname)
    fname <- ""
    if("character" %in% class(dat)){
       fname <- basename(dat)
       dat <- eyelinker::read.asc(dat)
    }
    t_events <- extract_events(dat) %>% filter(trial %in% only_trial)
    if(!is.null(only_events)) t_events <- t_events %>% filter(event %in% only_events)
    t_start <- min(t_events$time)
    t_end <- max(t_events$event_endtime)

    t_raw <- filter(dat$raw, time>=t_start, time<= t_end) %>%
        mutate(time=time-t_start)

    # pass in scored df, or score right now?
    this_score<-NULL
    if(is.null(score_df) & !is.null(score_fnc))  score_df <- score_fnc(dat)
    if(!is.null(score_df)) this_score <- score_df %>% filter(trial %in% only_trial)

    if("info" %in% names(dat)) max_x <- dat$info$screen.x # likely 1024 or  1920 

    t_b <- restrict_range(dat$blinks, t_start, t_end)
    t_s <- restrict_range(dat$sacc, t_start, t_end)


    events <- unique(t_events$event)
    t_event_plot <- t_events %>%
        mutate(across(c(time,event_endtime), ~.x-t_start),
               #event=factor(event, levels=seq_along(events), labels=events))
               event=as.factor(event))

    t_event_plot$dotpos_adj <- (t_event_plot$dotpos+1)/2 * max_x
    #if(!'eye' %in% names(t_raw)) t_raw$eye <- 'mono'
    p <- ggplot(t_raw) +
        aes(xmin=stime, xmax=etime)+
        geom_rect(data=t_b, aes(fill=eye, ymin=-100, ymax=-200),
                  alpha=.5, color="red")+
        geom_rect(data=t_s, aes(fill=eye, ymin=0, ymax=-100),
                  alpha=.5, color="blue")+
        geom_vline(data=t_event_plot,
                   aes(xintercept=time, color=event))+
        geom_line(data=t_event_plot,
                  aes(x=time, xmin=NULL, xmax=NULL, y=dotpos_adj),color="yellow", size=3)+
        geom_hline(yintercept=max_x/2,color="green",linetype=2)+
        coord_cartesian(ylim=c(-200,max_x),xlim=c(0,t_end-t_start))+
        ggtitle(paste0(collapse=" ","trial", paste00(only_trial), trial_desc)) + theme_bw()

     if("xpr" %in% names(t_raw)){
      p <- p +
        geom_point(aes(x=time, y=xpr, xmin=NULL, xmax=NULL), color="blue",alpha=.5)+
        geom_point(aes(x=time, y=xpl, xmin=NULL, xmax=NULL), color="red",alpha=.5)
     } else {
      p <- p +
        geom_point(aes(x=time, y=xp, xmin=NULL, xmax=NULL), color="blue",alpha=.5)
     }

     if(!is.null(this_score)) {
        new_title <- paste0(collapse=" ",
                            '', paste00(only_trial), paste00(trial_desc),
                            's:', paste00(this_score$score),
                            'l:', paste00(this_score$lat),
                            "; ", ifelse(trial_desc=="",fname,trial_desc))
        p <- p + ggtitle(new_title)
     }
     return(p)
}

source('dollarreward.R')
inspect_DR <- function(dat, ...){
  inspect_trial(dat,
                score_fnc=score_file_antiDR,
                extract_events=extract_dollar_events,
                trial_desc=" ",
                ...)
}

inspect_DR_run <- function(asc, save_name=NULL, ...){

  dat <- eyelinker::read.asc(asc)
  score_df <- score_file_antiDR(dat)
  trials <- as.numeric(stringr::str_extract(dat$msg$text[grepl("dot", dat$msg$text)], "^\\d+"))
  plts <- lapply(trials, \(trl)
                 inspect_trial(dat,
                               score_df=score_df,
                               extract_events=extract_dollar_events,
                               only_events="dot",
                               trial_desc=" ", only_trial=trl) +
                 theme(legend.position="none",
                       axis.text.x=element_blank(),
                       axis.text.y=element_blank(),
                       axis.ticks.x=element_blank(),
                       axis.ticks.y=element_blank())+
                 labs(y=NULL,x=NULL))
  p <-  do.call(cowplot::plot_grid,plts)

  if(!is.null(save_name)) {
     ggsave(p, save_name, ...)
  }
  invisible(p)
}

inspect_trial_test <- function(){
  asc_fname <- "example/sub-wf_sendmsg/ses-01/20231226_MGSEye/sub_wf_sendcmd_ses_01_task_MGS_run_1_20233426153455.asc.gz"
  inspect_trial(asc_fname, 3, extract_events=extract_mgs_events)
}
