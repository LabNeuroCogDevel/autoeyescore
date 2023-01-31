#!/usr/bin/env Rscript
# 20210924 - extract from vpx.org (20210813)

## * Packages
suppressPackageStartupMessages({
 source('RingReward/RingReward.settings.R')
 source('ScoreRun.R')
 library(dplyr)
 library(tidyr)
 library(glue)
 library(stringr)
})


## * Read
# arrington has a line per sample
# but lines that start with 12 are "triggers"
# using 'grep | cut' to extract messages and merge back
read_arr <- function(f) {
   id <- gsub(".*sub-([^_]*).*run-([^_]).*", "\\1 \\2", f)
   eye_samps <- read.table(text=system(glue("grep ^10 {f}|cut -f2-14"), intern=T),
                           sep="\t",
      col.names=c("TotalTime", "DeltaTime",
                  "X_Gaze", "Y_Gaze",
                  "X_CorrectedGaze", "Y_CorrectedGaze",
                  "Region", "PupilWidth", "PupilHeight",
                  "Quality", "Fixation", "PupilDiameter", "Count"))
   msg <- read.table(text=system(glue('grep ^12 {f}|cut -f2-3'), intern=T),
                     sep="\t", col.names=c("TotalTime","msg")) 
  
   d <- bind_rows(eye_samps, msg) %>%
    arrange(TotalTime) %>%
    fill(msg, .direction='down') %>%
    filter(!is.na(Count)) %>%
    mutate(id=id)
}
  
## * Task Events

# extract trigger messages specific to the memory guided saccade task
# originally as mgs task. re purposing for dollar reward
msg2dollarreward <- function(d) d %>%
   separate(msg, c('trial','part','ttype','dotpos'),sep=" ", extra='merge', remove=F) %>%
   mutate_at(vars(trial, dotpos),as.numeric) %>%
   mutate(part=ifelse(is.na(part),'iti', part)) %>%
   # if iti doesn't carry trial, use value from dot (which is after iti)
   fill(trial, .direction="up")
  
## ** Median gaze location
# b/c each sample is so noisy, look at median position during the event
med_norm <- function(d, norm_from=c("cue")) {
  # norm_from could be c("iti","ring","cue")
  t_med <- d %>%
    #filter(! part %in% c('dot')) %>%
    filter(part %in% norm_from) %>%
    group_by(trial) %>% summarise(x_med = median(X_CorrectedGaze,na.rm=T))
  d %>% left_join(t_med, by="trial") %>% mutate(x_norm = X_CorrectedGaze - x_med)
}

# 
## ** left vs right
# dot pos is -1=left to 1=right.
dr_lrx <- function(d) d %>%
   filter(part=='dot') %>% group_by(id,trial) %>%
   mutate(t=TotalTime-first(TotalTime),
          color=ifelse(dotpos<0, 'left', 'right')) %>% 
   select(trial, t, color, dotpos, matches('X_')) 

## * Viewpoint to ASL
# autoscoring expects a 261 point grid. but Viewpoint is 0-1
# but because of drift and poor tracking quality, we are often well beyond the 0-1 bounds
vp2asl <- function(vp) {
   asasl <- vp %>%
      rename(pupil_diam=PupilDiameter) %>%
      mutate(horz_gaze_coord=X_CorrectedGaze*263,
             vert_gaze_coord=Y_CorrectedGaze*263,
             dot=as.numeric(as.factor(dotpos)), # 1:6
             ttypenum=ifelse(ttype=='rew',80,60)) %>%  
      group_by(id,trial) %>% mutate(iscatch=!any(part=='dot')) %>% ungroup %>%
      mutate(XDAT=case_when(
                            part=='iti' ~ 250,
                            # always cue if catch
                            iscatch ~ case_when(ttype=='neu' ~ 20,
                                                ttype=='rew' ~ 40,
                                                TRUE ~ 255),
                            TRUE ~ case_when(part=='ring' ~ 250, # no trigger sent for ring
                                             part=='cue' ~ ttypenum,
                                             part=='dot' ~ 100+ttypenum+dot,
                                             TRUE ~ 0))) %>%
      arrange(id,TotalTime)
}

## ** Pariticipant and visit in filename

info_from_fname <- function(eyetxt) {
   # "sub-96_ses-01_task-DR_run-1
   basename(eyetxt) %>%
      str_match(pattern="sub-(.*)_task-(.*)_run-(.*)") %>%
      as.vector %>%
      `names<-`(c('all','subj','task','run')) 
}

## * SCORE
# need to tweak settings to allow for noisier gaze position samples
score_arrington <- function(eyetxt, ...) {

   nameinfo <- info_from_fname(eyetxt)

   # eyetxt <- "arrington/example/sub-WF_ses-01_task-DR_run-1"
   # ... can be 'show_plot=T'
   fixpos.maxdrift <<- 100 # default 50
   #sac.minmag <<- 30 #default 10
   #lat.minvel <<- 8 # default 4

   sac.minmag   <<- 40      # min abs of x position change -- set very low, inc to 20 at LR request :)
   lat.minvel   <<- 10      # ASLcoordx/60Hz 
   sac.slowvel  <<- 5       # lower bound for movement to plot but not score

   d <- read_arr(eyetxt) %>%
      msg2dollarreward %>%
      med_norm %>%
      vp2asl

   print(head(d))
   d %>%
      select(XDAT, pupil_diam, horz_gaze_coord, vert_gaze_coord) %>%
      getSacs(subj=nameinfo['subj'],
              run=nameinfo['run'],
              nameinfo['task'],
              save_scored=F, ...) %>%
      scoreSac
}


## * USING
arrington_example <- function() {
  runs96y7 <- Sys.glob("arrington/example/sub-*_ses-*_task-DR_run-1") %>%
     lapply(score_arrington) %>%
     bind_rows
  print(runs96y7)
}

## ** Single example
run_one <- function() {
  d <- score_arrington("/Volumes/L/bea_res/Data/Tasks/DollarReward2/MR/11907_20230117/sub-11907_ses-01_task-DR_run-1.txt",
                       onlyontrials=2, showplot = TRUE)
  while (dev.off()) print("closing")
}
