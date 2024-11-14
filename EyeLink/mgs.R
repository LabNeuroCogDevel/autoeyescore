extract_mgs_events <- function(eyets){
  events <- eyets$msg %>%
      # remove task administrator notes (InputId Awake Droop Intrv) and end of TRIAL OK
      # could maybe just grab TRIALID|EventID
      filter(!grepl("^!|InputID|OK$",text)) %>%
      mutate(
          trial=stringr::str_extract(text,"(?<=TRIALID )\\d+"),
          # original data has dot instead of mgstarget. probably okay to remove this
          # currently not used anyway
          text=gsub("mgs_","mgs",text),
          text=gsub("dot", "mgstarget", text)) %>%
      # only TRIALID has trial number
      # na.locf will forward fill the missing 'desc' info into EventID rows
      # and then we dont need trial id rows anymore
      mutate(trial=zoo::na.locf(trial)) %>%
      filter(!grepl('TRIALID',text)) %>%
      # text encodes all the event information. pull it into separate columns
      separate(text, c("event", "dlydur","dotpos"), extra="merge",sep="_") %>%
      # create interval
      mutate(event_endtime=lead(time, default=max(time))-1) %>%
             # iranges cant live within the tibble/dataframe :(
             #erange=IRanges::IRanges(start=time,end=endtime))
      # text field seperate treated as chars. but most are nums
      mutate(across(c(trial, dlydur, dotpos), as.numeric),
             across(c(dlydur,dotpos),zoo::na.locf))
}

mgs_expect_dir <- function(dotpos) sign(dotpos)
score_file_mgs <- function(asc_fname, ...)
    score_file_generic(
        asc_fname,
        extract_events = extract_mgs_events,
        groups=c("dlydur"),
        dot_event="mgsexec",
        find_expect_dir=mgs_expect_dir,
        ...)

score_file_mgs_test <- function(){
    asc_fname <- 'example/sub-wf_sendmsg/ses-01/20231226_MGSEye/sub_wf_sendcmd_ses_01_task_MGS_run_1_20233426153455.asc.gz'
    s <- score_file_mgs(asc_fname)
}
