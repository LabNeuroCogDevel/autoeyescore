# 20230331 - copied from /Volumes/Hera/Projects/Habit/mr/dollarreward/eye_score.R
#ocwd <- getwd()
#setwd("/Volumes/Hera/Projects/autoeyescore/")
source('score_arrington.R')
#setwd(ocwd)

#eg_file <- '/Volumes/L/bea_res/Data/Tasks/DollarReward2/MR/11883_20220913/sub-11883_ses-01_task-DR_run-1.txt'
#plot_run('/Volumes/L/bea_res/Data/Tasks/DollarReward2/MR/11883_20220913/sub-11883_ses-01_task-DR_run-2.txt')


dr_eye_files <- function() 
 Sys.glob('DR_MR_files/11734_20221111/sub*_task-DR_run-*')
 #Sys.glob('/Volumes/L/bea_res/Data/Tasks/DollarReward2/MR/1*_2*/sub*_task-DR_run-*')

score_dr <- function(eg_file) {
  eye_df <- read_arr(eg_file) %>% msg2dollarreward %>% med_norm
  d_score <- score_arrington(eg_file)
}

wrap_desc <- function(txt) sapply(txt, \(x) paste(collapse="\n",strwrap(x, width=24)))

# 20230402 -- using to identify ideal settings for avotech/arrington at prisma2
plot_run <- function(eg_file) {

  # autoscore the file. will need to adjust parameters
  d_score <- score_arrington(eg_file)

  # get raw eye data so we can figure out where the dot is
  eye_df <- read_arr(eg_file) %>% msg2dollarreward %>% med_norm

  # limit to just rew cue and dot (saccade cue)
  #  remove catch trials, make relative time
  dot_df <- eye_df %>%
      filter(!is.na(trial))  %>%
      group_by(trial) %>%
      filter(part %in% c('cue','dot')) %>%
      mutate(t=(TotalTime-min(TotalTime)),
             hasdot=any(part == 'dot')) %>%
      # remove catch trials
      filter(hasdot) %>% ungroup() %>%
      # reset  trial count -- should already be numeric but "1" "10" "2"
      # can't use order b/c expect many repeats
      mutate(trial=as.numeric(as.factor(as.numeric(trial))))
  
  # only need the position of the dot once per trial
  dot_pos <- dot_df %>% filter(part=='dot') %>%
      group_by(trial) %>%
      summarise(dotpos=first(dotpos))

  ggplot(dot_df) +
      aes(x=t, y=x_norm, color=part) +
      geom_hline(aes(yintercept=0), linetype=1, alpha=.5) +
      geom_point(size=.3) +
      geom_vline(data=d_score, aes(xintercept=lat/1000+1.5, color=as.factor(Count))) +
      geom_hline(data=dot_pos, aes(yintercept=-1*sign(dotpos))) +
      geom_text(data=d_score, aes(x=0,y=.9, color=NULL, label=wrap_desc(Desc)), vjust=1, hjust=0, size=2) +
      facet_wrap(~trial) + lims(y=c(-2,2)) + cowplot::theme_cowplot() +
      ggtitle(basename(eg_file))

}
