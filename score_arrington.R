#!/usr/bin/env Rscript
# 20210924 - extract from vpx.org (20210813)

suppressPackageStartupMessages({
 source('/Volumes/Hera/Projects/autoeyescore/RingReward/RingReward.settings.R')
 source('/Volumes/Hera/Projects/autoeyescore/ScoreRun.R')
 library(dplyr)
 library(tidyr)
 library(glue)
 library(stringr)
})


read_arr <- function(f) {
   id <- gsub('.*sub-([^_]*).*run-([^_]).*','\\1 \\2',f)
   eye_samps <- read.table(text=system(glue('grep ^10 {f}|cut -f2-14'),intern=T),sep="\t",
      col.names=c("TotalTime","DeltaTime",
		  "X_Gaze","Y_Gaze",
		  "X_CorrectedGaze","Y_CorrectedGaze",
		  "Region","PupilWidth","PupilHeight",
		  "Quality","Fixation","PupilDiameter","Count"))
   msg <- read.table(text=system(glue('grep ^12 {f}|cut -f2-3'),intern=T),sep="\t", col.names=c("TotalTime","msg")) 
  
   d <- bind_rows(eye_samps, msg) %>%
    arrange(TotalTime) %>%
    fill(msg, .direction='down') %>%
    filter(!is.na(Count)) %>%
    mutate(id=id)
}
  
msg2dollarreward <- function(d) d %>%
   separate(msg, c('trial','part','ttype','dotpos'),sep=" ", extra='merge', remove=F) %>%
   mutate_at(vars(trial, dotpos),as.numeric) %>%
   mutate(part=ifelse(is.na(part),'iti', part))
  
dr_lrx <- function(d) d %>%
   filter(part=='dot') %>% group_by(id,trial) %>%
   mutate(t=TotalTime-first(TotalTime),
          color=ifelse(dotpos<0, 'left', 'right')) %>% 
   select(trial, t, color, dotpos, matches('X_')) 

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

score_arrington <- function(eyetxt, ...) {
   # ... can be 'show_plot=T'
   fixpos.maxdrift <- 100 # default 50
   sac.minmag <- 30 #default 10
   lat.minvel <- 8 # default 4
   # sub-xxx_run-y
   nameinfo <- basename(eyetxt) %>%
      # "sub-96_ses-01_task-DR_run-1
      str_match(pattern="sub-(.*)_task-(.*)_run-(.*)") %>%
      as.vector %>%
      `names<-`(c('all','subj','task','run')) 
   print(nameinfo)

   d <- read_arr(eyetxt) %>%
      msg2dollarreward %>%
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


# USING
runs96y7 <- Sys.glob("096/y7/sub-*run-[1-4]") %>% 
   lapply(score_arrington) %>%
   bind_rows

print(runs96y7)
