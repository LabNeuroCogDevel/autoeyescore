library(dplyr)
source('../eyelink_functions.R')
get_id <- function(s) stringr::str_extract(s,'\\d{4,6}..\\d{2}')

# 20220928: 3* ids are CARRS. 2* are SITS (?)
asc_files <- Sys.glob('asc/3*.asc.gz')
scores_el <- lapply(asc_files, function(f) score_file(f) %>% mutate(vid=get_id(f)))
d_eyelink <- scores_el %>% bind_rows()

source("../../ScoreRun.R")
source("../../RingReward/RingReward.settings.R")
# disable some checks to get values
#fb <- 'highvelstartok,nothingistoofast' # funnybusiness = fb 

asl_score <- function(asc)
    asc_as_asl(asc) %>%
        getSacs(subj='1', run=1, runtype='AS',
                rundate=0, save_scored=F) %>%
        scoreSac() %>%
        # not sure why subj is lost after scoreSac
        # but want as vid for merging anyway
        mutate(vid=get_id(asc))

## play with single file
# d_asc <-  asc_as_asl(asc_files[[1]])
# getSacs(d_asc, onlyontrials=10,showplot=T,save_score=F, funnybusiness=fb,subj='',run=1,runtype='AS') %>% scoreSac()
# d_a <- asl_score(asc_files[[1]])
# sort(d_a$Count) %>% rle %>% with(data.frame(v=values,n=lengths))
# head(d_a$Desc)
# d$lat %>% summary


scores_asl <- lapply(asc_files,
                    function(f) tryCatch(asl_score(f), error=function(e)NULL))


d_asl <- scores_asl %>% bind_rows()

d_both <- merge(d_asl %>% select(vid,trial,lat,Count,Desc),
                d_eyelink %>% select(vid,trial,lat,Count=score,Desc=desc),
                by=c("vid","trial"),
                suffixes=c(".asl",".el"))
write.csv(d_both,"scoreCARRS_asl_and_et.csv",row.names=F)
#d_both %>% filter(Count.el==Count.asl,Count.asl>=0) %>% with(summary(lat.el - lat.asl))
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  -938.00   -4.00   -1.00  -12.92    3.00  158.00 

#d_both %>% group_by(Count.el,Count.asl) %>% tally %>% spread(Count.asl,n)
# Count.el  `-1`   `0`   `1`   `2`
#    <dbl> <int> <int> <int> <int>
#       -1   591    18   665   208
#        0    37    45    19    NA
#        1   483     3  7772    50
#        2   145    49    69  1345


score_rat <- d_both %>%
    group_by(Count.el, Count.asl) %>%
    summarise(n=n(), latdiff=mean(lat.asl-lat.el,na.rm=T)) %>%
    left_join(d_both %>% group_by(Count.el) %>% summarise(n.el=n())) %>%
    mutate(rat.el=n/n.el)

p_score_rat <- ggplot(score_rat) +
    aes(x=Count.el, y=Count.asl, fill=rat.el,
        label=glue::glue("n={n}\nΔlat={round(latdiff)}")) +
    geom_raster() +
    geom_label() +
    scale_fill_gradient(low="white", high="red") +
    ggtitle("Algo agreement: ASL vs EyeLink (lat: asl-el)")
ggsave(p_score_rat, file="score_rat_CARRS.png")
