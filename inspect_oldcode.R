library(plyr)
library(dplyr)
source('abstract.R')
source('ScoreRun.R')
source('anti/anti.settings.R')
sacs_old <- getSacs('./10997.20200221.1.data.tsv', '10997', 1, runtype,rundate=0,onlyontrials=c(1),showplot=F)

opts <- studysettings()
eyedf <- read_eye(data_file) # this maybe replaced by arrow?
eyedf <- clean_xdat(eyedf, opts) # bad data samples to NA

# new
tidxdf <- trial_indexs(eyedf, opts) # trial, start, target, stop, xdat
 #  remove samples around blinks, smooth
interps <- interp_samples(eyedf$xpos, opts)
interp_df <- interps$b.nona
tinfo <- partition_trials(tidxdf$target, tidxdf$stop, eyedf$xpos) 
ti1 <- tinfo[[1]]
t_interp_df <- interp_df[ti1$trgidxs, ]
t_approx <- interps$b.approx$xpos[ti1$trgidxs]
blinks <- find_blinks(t_approx, opts)
sacs_new <- find_saccades(t_interp_df$xpos, blinks, opts)

# old
d <- eyedf
targetIdxs <- get_targets(eyedf, opts)
b.approx <- NA
b.all <- interoplateSamples(trl=1)

# check interpolate is same (60 samples)
nrow(b.approx) == targetIdxs[1,2] - targetIdxs[1,1] 
nrow(b.approx) == nrow(t_interp_df)
all(b.all$y == t_interp_df$xpos)
all(b.all$x == (t_interp_df$time - t_interp_df$time[1]+1)) # t_interp_df time is absolute


# check pulled saccades (before dropped, percent tracked, scored)
n <- intersect(names(sacs_new),names(sacs_old))
length(n) == 11
all(sacs_old[,n] == sacs_new[,n])

# check we get coverage correct
sacs_new$p.tracked <- tracked_withinsac(sacs_new, eyedf$xpos[tinfo[[1]]$trgidxs])
all(sacs_new$p.tracked == sacs_old$p.tracked) # all 1 -- full coverage

# add scored
sacs_new_scored <- score_sacs(sacs_new, sac.expmag, ti1$baseline)


### for real, all together
sacs_new_all <- score_trial(eyedf$xpos[ti1$trgidxs], interp_df$xpos[ti1$trgidxs], interps$b.approx$xpos[ti1$trgidxs], ti1$baseline, opts)
all(sacs_new_all == sacs_new_scored)

dropreason <- drop_interp(interp_df[ti1$trgidxs,], ti1$baseline, opts)
dropreason <- no_early_move(eyedf$xpos[1:(opts$lat.fastest*opts$sampleHz)], opts)
