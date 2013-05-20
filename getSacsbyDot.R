getSacDot <- function(dotnotation) {
 parts <- unlist(strsplit(dotnotation, '\\.'))
 parts <- as.numeric(parts);
 names(parts) <- c('subj','date','run','trial')
 # filebasedir come from *settings.R file
 dirbase  <- sprintf("%s/%s/%s",filebasedir,parts['subj'],parts['date'])
 eyetrack <- sprintf("%s/Raw/EyeData/txt/%s.%s.%s.data.tsv",dirbase,parts['subj'],parts['date'],parts['run'])
 saveto   <- sprintf("%s/Scored/txt/%s.%s.%s.sac.tsv",dirbase,parts['subj'],parts['date'],parts['run'])

 getSacs(eyetrack,parts['subj'],parts['run'],"BarsScan",onlyontrials=parts['trial'],savedas=saveto,writetopdf=F,showplot=T)
}
