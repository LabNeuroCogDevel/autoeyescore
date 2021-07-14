library(plyr)
library(dplyr)
source('abstract.R')
source('ScoreRun.R')
source('anti/anti.settings.R')
sacs_old <- getSacs('./10997.20200221.1.data.tsv', '10997', 1, runtype,rundate=0,onlyontrials=c(1),showplot=F)

data_file <- './10997.20200221.1.data.tsv'
opts <- studysettings()
eyedf <- read_eye(data_file) # this maybe replaced by arrow?
eyedf <- clean_xdat(eyedf, opts) # bad data samples to NA

# new
tidxdf <- trial_indexs(eyedf, opts) # trial, start, target, stop, xdat
 #  remove samples around blinks, smooth
interps <- interp_samples(eyedf$xpos, opts)
interp_df <- interps$b.nona
tinfo <- partition_trials(tidxdf, eyedf$xpos) 
ti1 <- tinfo[[1]]
t_interp_df <- interp_df[ti1$trgidxs, ]
t_approx <- interps$b.approx$xpos[ti1$trgidxs]
blinks <- find_blinks(t_approx, opts)
accel_info <- get_accels(t_interp_df$xpos, opts)
sacs_new <- find_saccades(accel_info, blinks, opts)

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


### for real, all together
ti <- ti1$trgidxs # idx from 132 to 191

# check out drop reasons
dropreason <- drop_interp(interp_df[ti,], ti1$baseline, opts) # TODO: sampling rate?
dropreason <- no_early_move(eyedf$xpos[ti[1] + 0:(opts$lat.fastest*opts$sampleHz)], opts)

sacs_new_all <- trial_sacs(ti1, eyedf$xpos[ti], interp_df$xpos[ti], interps$b.approx$xpos[ti], opts)
trial1_score <- score_trial(sacs_new_all, opts)
