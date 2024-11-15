#!/usr/bin/env Rscript
# score dollar reward task eye tracking asc files (converted edf)
# sources eyelink_functions.R
# 202410xxVD/WF
# 20241114WF - save full filename in df,
#              rename func to score_all_antiDR, add script action,


extract_dollar_events <- function(eyets){
  #asc_fname="./example/sub-wf_sendmsg/ses-01/20231226_VGSEye/sub_wf_sendmsg_ses_01_task_VGS_run_1_20232326152333.asc.gz"
  # eyets <- eyelinker::read.asc(asc_fname)
  # eyets$msg
  #  block   time text
  #   <dbl>  <dbl> <chr>
  #       1 441420 !MODE RECORD CR 500 2 1 LR
  #       1 441436 TRIALID 3
  #       1 441437 vgs_cue_4.0_0.43
  #       1 445398 gap_4.0_0.43
  #       1 445595 dot
  #       1 446595 blank_4.0_0.43
  #       1 446621 TRIAL OK
  events <- eyets$msg %>%
      # remove task administrator notes (InputId Awake Droop Intrv) and end of TRIAL OK
      # could maybe just grab TRIALID|EventID
      filter(!grepl("^!|InputID|OK$",text)) %>%
      mutate(
          trial=stringr::str_extract(text,"(?<=TRIALID )\\d+"),
          # original data has dot instead of mgstarget. probably okay to remove this
          # currently not used anyway
          # TODO: CHANGE ME HERE
          text=gsub("^ring", "0_ring", text)) %>%
      # only TRIALID has trial number
      # na.locf will forward fill the missing 'desc' info into EventID rows
      # and then we dont need trial id rows anymore
      mutate(trial=zoo::na.locf(trial, na.rm= FALSE)) %>%
      filter(!grepl('TRIALID',text)) %>%
      # text encodes all the event information. pull it into separate columns
      separate(text, c("trialjunk", "event", "reward","dotpos"), extra="merge",sep="_") %>%
      # create interval
      mutate(event_endtime=lead(time, default=max(time))-1) %>%
             # iranges cant live within the tibble/dataframe :(
             #erange=IRanges::IRanges(start=time,end=endtime))
      # text field seperate treated as chars. but most are nums
      mutate(across(c(trial, dotpos), as.numeric),
             across(c(reward,dotpos), function(x) zoo::na.locf (x, na.rm = FALSE)))
}

anti_expect_dir <- function(dotpos) -1*sign(dotpos)

# wraps generic score function.
source('eyelink_functions.R')
score_file_antiDR <- function(asc_fname, ...)
    score_file_generic(
        asc_fname,
        extract_events = extract_dollar_events,
        groups=c("reward"),
        dot_event="dot",
        find_expect_dir=anti_expect_dir,
        ...) %>% mutate(file=ifelse(is.character(asc_fname),asc_fname, NA))

score_all_antiDR <- function(){
    asc_files <- Sys.glob("../../subj_info/sub-1*/ses-*/*_DollarReward/sub_*.asc*")
    # TODO: remove head when ready to run infull
    all_scored_list <- lapply(asc_files, score_file_antiDR)
    # final giant dataframe
    # TODO: is id stored within? otherwise add as column in lapply
    all_scored_dataframe <- dplyr::bind_rows(all_scored_list)
}
if (sys.nframe() == 0){
   args <- commandArgs(trailingOnly = FALSE)
   all_scored <- lapply(args[1:length(args)], score_file_anti) %>% bind_rows
   write.table(all_scored, file=stdout(), sep="\t", quote=F, row.names=F)
}
