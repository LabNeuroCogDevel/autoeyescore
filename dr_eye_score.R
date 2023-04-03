# 20230331 - copied from /Volumes/Hera/Projects/Habit/mr/dollarreward/eye_score.R
#ocwd <- getwd()
#setwd("/Volumes/Hera/Projects/autoeyescore/")
source('score_arrington.R')
library(ggplot2)
#setwd(ocwd)

#eg_file <- '/Volumes/L/bea_res/Data/Tasks/DollarReward2/MR/11883_20220913/sub-11883_ses-01_task-DR_run-1.txt'
#plot_run('/Volumes/L/bea_res/Data/Tasks/DollarReward2/MR/11883_20220913/sub-11883_ses-01_task-DR_run-2.txt')

library(tidyr)
DATELOOKUP <- read.table('scans.txt', sep="\t",
                         col.names=c("ld8", "age", "sex", "scan", "study", "Vx", "session")) %>%
    separate(ld8,c("id","d8"))
subjid <- function(fname) stringr::str_extract(fname,"(?<=sub-)[0-9]{5}")
fname2info <-function(fname) {
    fnameid <- subjid(fname)
    matchi <- grep(fnameid, DATELOOKUP$id)[1]
    d_id <- DATELOOKUP[matchi,]
    return(with(d_id, glue::glue("{id}_{d8} {round(age)} yo{sex}")))
}

dr_eye_files <- function() 
 Sys.glob('DR_MR_files/1*_2*/sub*_task-DR_run-*')
 #Sys.glob('/Volumes/L/bea_res/Data/Tasks/DollarReward2/MR/1*_2*/sub*_task-DR_run-*')

score_dr <- function(eg_file) {
  eye_df <- read_arr(eg_file) %>% msg2dollarreward %>% med_norm
  d_score <- score_arrington(eg_file)
}

wrap_desc <- function(txt) sapply(txt, \(x) paste(collapse="\n",strwrap(x, width=24)))

# 20230402 -- using to identify ideal settings for avotech/arrington at prisma2
plot_run <- function(eg_file) {
    
  fname_info <- fname2info(eg_file)

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
  dot_pos <- dot_df %>%
      filter(part=='dot') %>% group_by(trial) %>%
      summarise(dotpos=first(dotpos),
                sd=sd(x_norm),
                n=length(which(abs(x_norm)<=2)),
                mint=min(t), maxt=max(t),
                fail=sd>abs(dotpos))
   
  # for plotting so we can see if noisey or just off the plot
  cap_at <- function(xs, lim=2) ifelse(abs(xs)>lim, sign(xs)*lim, xs)

  ggplot(dot_df %>% mutate(x_norm=cap_at(x_norm))) +
      aes(x=t, y=x_norm, color=part) +
      geom_hline(aes(yintercept=0), linetype=1, alpha=.5) +
      geom_point(size=.3) +
      geom_vline(data=d_score, aes(xintercept=lat/1000+1.5, color=as.factor(Count))) +
      geom_hline(data=dot_pos, aes(yintercept=-1*sign(dotpos))) +
      # data quality vs actual position
      geom_ribbon(data=rbind(dot_pos %>% mutate(t=mint),
                             dot_pos %>% mutate(t=maxt)),
                  aes(ymin=-sd, ymax=sd, y=NULL, color=NULL),
                  fill='gray', alpha=.4) +
      # actual ideal look location
      # visualizing to check should be outside of sd?
      geom_hline(data=dot_pos,
                 aes(yintercept=-dotpos), color='blue',linetype=2) +
      # give us actual sd and count in range
      geom_label(data=dot_pos,
                 aes(x=mint, y=-1*sign(dotpos), color=NULL,
                     label=paste0("SD: ",round(sd,1), " n:", n), 
                     vjust=ifelse(sign(dotpos)==1, 1, 0)),
                 hjust=0, alpha=.7) +
      # scoring notes
      geom_text(data=d_score, aes(x=0,y=.9, color=NULL, label=wrap_desc(Desc)), vjust=1, hjust=0, size=2) +
      facet_wrap(~trial) +
      lims(y=c(-2,2)) +
      cowplot::theme_cowplot() +
      ggtitle(paste0(fname_info, " ", basename(eg_file)))

}

# collapse all scores into one giant file
mkpdf <- function(pdf_fname="all_dollar_reward.pdf") {
    pdf(pdf_fname, width=15, height = 8)
    all_dr <- data.frame(fname=dr_eye_files()) %>% mutate(id=subjid(fname)) %>%
        merge(DATELOOKUP, by='id') %>%
        arrange(d8)
    for(f in all_dr$fname) print(plot_run(f))
    dev.off()
}


# 
# sub−11883_ses−01_task−DR_run−1.txt | low sd for many. few scored well
# sub−11883_ses−01_task−DR_run−2.txt | high sd but low noise. good traces
# sub−11884_ses−01_task−DR_run−1.txt | low sd, low noise. not scored well
# sub−11885_ses−01_task−DR_run−1.txt | very noisy 
# sub−11889_ses−01_task−DR_run−1.txt | low noise, high sd (why?)
# sub−11896_ses−01_task−DR_run−1.txt | beautiful traces! saccades not captured but sored well
# sub−11897_ses−01_task−DR_run−1.txt | ditto -- but all run shav ea score!!!! (albeit not correct)
# sub−11900_ses−01_task−DR_run−1.txt | bad tracking
# sub−11907_ses−01_task−DR_run−2.txt | good tracking. many scored but most incorrectly
# sub−11912_ses−01_task−DR_run−2.txt | good tracking for most trials
# sub−11913_ses−01_task−DR_run−2.txt | good tracking
# sub−11914_ses−01_task−DR_run−2.txt | bad tracking
# sub−11915_ses−01_task−DR_run−1.txt | maybe not scorable. too noisy. but low sd?
# sub−11919_ses−01_task−DR_run−1.txt | too noisy

# sub−11919
# is off from middle(130.5) by 736.42 (>max 100.0)!
# -- lots of blinks .. need to adjust blink threshold?

# sub−11915_ses−01_task−DR_run−2.txt
# moving (17.165 px/60Hz) before target onset

