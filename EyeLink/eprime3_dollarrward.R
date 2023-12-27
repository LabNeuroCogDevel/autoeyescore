# #./220682rr01.asc
# # 9526649   641.7   720.5  1891.0   691.5   652.4  2264.0   127.0 .....
# d <- read.table(na.strings=".", text=system(intern=T, glue::glue("perl -F'\\t' -slane 'print if /^[0-9]/ and $#F == 8' {asc_fname}")))
# #names(d) <- c("time", "gx1", "gy1", "V4","gx2","gy2","V7", "input", "hdata")
# names(d) <- c("time", "xpl", "ypl", "psl","xpr","ypr","psr", "input", "hdata")
# # looking at matlab data to compare
# # m = load("./ras.mat")
# # for f =fieldnames(m.ras(1).FSAMPLE)'; fprintf("%s %s\n", f{1}, sprintf("%.2f,",m.ras(1).FSAMPLE.(f{1})(:,90000))); end


# eprime3 dollar reward
extract_EP3_DR_events<-function(eyets){
  # event intervals (4 per trial, long format)
  # from GK 2022-04-15 (encoded in eprime3 task):
  #   Message Trial Event TrialType         DotPosition1+2
  #   TRIALID   2       0 Reward            1406    R-27
  #   EventID   2       1 Fixation Cross
  #   EventID   2       2 Ring ($ or #)
  #   EventID   2       3 Fixation Cross
  #   EventID   2       4 Yellow Dot
  #   EventID   2       5 End of Trial (Yellow Dot Offset)
  #   TRIAL OK
  events <- eyets$msg %>%
      # remove task administrator notes (InputId Awake Droop Intrv) and end of TRIAL OK
      # could maybe just grab TRIALID|EventID
      filter(!grepl("^InputID|OK$",text)) %>%
      # text encodes all the event information. pull it into separate columns
      separate(text, c("msg", "trial","event","ttypenum", "desc"), extra="merge") %>%
      # only TRIALID has rewtype, dotpos, & L/R-#
      # na.locf will forward fill the missing 'desc' info into EventID rows
      mutate(desc=zoo::na.locf(desc)) %>%
      # with desc filled down from TRIALID into all EventIDs, we can break it apart
      separate(desc, c("rewtype","dotpos", "tdir", "tnum")) %>%
      # trialid and fist event id are same timpeont
      # and then all msg are EventID so we dont need the column
      filter(msg != "TRIALID") %>%
      select(-msg) %>%
      # create interval
      mutate(event_endtime=lead(time, default=max(time))-1) %>%
             # iranges cant live within the tibble/dataframe :(
             #erange=IRanges::IRanges(start=time,end=endtime))
      # text field seperate treated as chars. but most are nums
      mutate_each(as.numeric, trial, event, ttypenum, dotpos, tnum)
}

score_file_ep3_dr <- function(asc_fname, ...)
    score_file_generic(
        asc_fname='example/220682rr01.asc.gz',
        dot_event=4,
        extract_events=extract_EP3_DR_events,
        groups=c("rewtype","tdir"),
        ...)
