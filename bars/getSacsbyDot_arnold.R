source('bars.settings.R')
source('../ScoreRun.R')

getSacDot <- function(dotnotation) {
 parts <- unlist(strsplit(dotnotation, '\\.'))
 parts <- as.numeric(parts);
 names(parts) <- c('subj','date','run','trial')
 dirbase  <- sprintf("/Users/lncd/rcn/bea_res/Data/Tasks//BarsBehavioral/Basic/%s/%s",parts['subj'],parts['date'])
 eyetrack <- sprintf("%s/Raw/EyeData/txt/%s.%s.%s.tsv",dirbase,parts['subj'],parts['date'],parts['run'])
 saveto   <- sprintf("%s/Scored/txt/%s.%s.%s.sac.tsv",dirbase,parts['subj'],parts['date'],parts['run'])

 getSacs(eyetrack,parts['subj'],parts['run'],"BarshBeh",onlyontrials=parts['trial'],savedas=saveto,writetopdf=F,showplot=T)
}
