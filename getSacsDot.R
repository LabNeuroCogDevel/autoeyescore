source('ScoreRun.R')
# arnold (users/lncd/rnc/ vs reese (mnt/b)
taskdir <- gsub('.*/Data/Tasks','',filebasedir)
Bdir <- '/mnt/B/'
if(!file.exists(Bdir)) { Bdir <- '/Users/lncd/rcn/' }
if(!file.exists(Bdir)) { stop('cannot find good location for B') }

# rewrite filebasedir based on Bdir so it function works on arnold as well as reese
filebasedir <- sprintf('%s/bea_res/Data/Tasks/%s',Bdir,taskdir)

# get bname (bea_res name) of the paradigm/task
bname <- gsub('.*/Data/Tasks/', '',
               gsub('/Basic.*', '', filebasedir) 
             )
if(!file.exists(filebasedir)) { stop(sprintf('cannot find task directory: %s',filebasedir)) }

getSacDot <- function(dotnotation, showplot=T,funnybusiness='',showcmd=F) {
 parts <- unlist(strsplit(dotnotation, '\\.'))
 parts <- as.numeric(parts);
 names(parts) <- c('subj','date','run','trial')
 # filebasedir come from *settings.R file
 dirbase  <- sprintf("%s/%s/%s",filebasedir,parts['subj'],parts['date'])
 eyetrack <- sprintf("%s/Raw/EyeData/txt/%s.%s.%s.data.tsv",dirbase,parts['subj'],parts['date'],parts['run'])
 saveto   <- sprintf("%s/Scored/txt/%s.%s.%s.sac.tsv",dirbase,parts['subj'],parts['date'],parts['run'])

 # if we want subj.date.run.* # all trials
 if(grepl('\\*$',dotnotation)){
  #trial=sprintf('1:%d',expectedTrialLengths)
  trial=1:expectedTrialLengths
  parts['trial']=sprintf('1:%d',expectedTrialLengths);
  showplot=F
 }else{
  trial=parts['trial'] 
 }
 if(showcmd) {cat(sprintf("getSacs('%s','%s','%s','%s',onlyontrials='%s',savedas='%s',writetopdf=F,showplot=%s,rundate='%s')\n",eyetrack,parts['subj'],parts['run'],bname,parts['trial'],saveto,showplot,parts['date']))  }

 getSacs(eyetrack,parts['subj'],parts['run'],bname,onlyontrials=trial,savedas=saveto,writetopdf=F,showplot=showplot,rundate=parts['date'],funnybusiness=funnybusiness)
}

getRunDot <- function(dotnotation, showplot=F,funnybusiness='',showcmd=F) {
 parts <- unlist(strsplit(dotnotation, '\\.'))
 parts <- as.numeric(parts);
 names(parts) <- c('subj','date','run')

 dirbase  <- sprintf("%s/%s/%s",filebasedir,parts['subj'],parts['date'])
 eyetrack <- sprintf("%s/Raw/EyeData/txt/%s.%s.%s.data.tsv",dirbase,parts['subj'],parts['date'],parts['run'])

 # maybe we can use eprime?
 EPfile    <- sub('data.tsv$','eplog.txt',eyetrack)
 print(EPfile)
 EPcorrect <- tryCatch({ 
     correct <- read.table(file=EPfile,sep="\t",header=T)$Correct
     if(!is.null(correct)){cat('found and using eprime log file\n')}
     correct
    },error=function(e){ cat('couldnt open eplog!\n'); NULL })

 if(showplot==T){
    for(trial in 1:expectedTrialLengths){
       print(trial)
       getSacs(eyetrack,
               parts['subj'],
               parts['run'],
               bname,
               onlyontrials=trial,
               writetopdf=F,
               showplot=showplot,
               rundate=parts['date'],
               funnybusiness=funnybusiness)
       cat('push any key when ready for next')
       readline()
    }
 }
 
 allsacs <- getSacs(eyetrack,parts['subj'],parts['run'],bname,onlyontrials=1:expectedTrialLengths,writetopdf=F,showplot=F,rundate=parts['date'],funnybusiness=funnybusiness)
 scoreSac(allsacs, EPcorrect=EPcorrect)
}

