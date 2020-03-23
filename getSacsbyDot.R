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

# 20200323 - more flexible getSacDot for manually inspecting AntiPet
getSacDot... <- function(dotnotation, ...) {
 parts <- unlist(strsplit(dotnotation, '\\.'))
 parts <- as.numeric(parts);
 names(parts) <- c("subj", "date", "run")
 # filebasedir come from *settings.R file
 fname <- paste(sep=".", parts['subj'], parts['date'], parts['run'], "data.tsv")
 eyetrack <- file.path(filebasedir, parts["subj"], parts["date"], "Raw/EyeData/txt", fname)
 tname <- basename(dirname(filebasedir))
 # what to send to getSacs
 s_args <- list(eyetrack, parts["subj"], parts["run"], tname, ...)
 # if we have a 4th dot, it's a trial
 if (length(parts) ==4) s_args <- c(s_args, list(onlyontrials=parts[4]))
 do.call(getSacs, s_args)
}
