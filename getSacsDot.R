# install.packages(c('colorspace','digest','zoo','plyr','KernSmooth','ggplot2'))
source('ScoreRun.R')
# make sure we loaded the settings file
if(!exists('filebasedir')) { 
  stop('need to source a settings file first!\nsource("anti/anti.settings.R")') 
} else  { 
  cat('using ', filebasedir, "\n")
}
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

taskname <- tolower( strsplit(taskdir,'/')[[1]][2] )

getSacDot <- function(dotnotation, showplot=T,funnybusiness='',showcmd=F) {
 parts <- unlist(strsplit(dotnotation, '\\.'))
 parts <- as.numeric(parts);
 names(parts) <- c('subj','date','run','trial')
 # filebasedir come from *settings.R file
 dirbase  <- sprintf("%s/%s/%s",filebasedir,parts['subj'],parts['date'])


 inputdir <- 'Raw/EyeData/txt'
 outputdir <- 'Scored/txt'
 eyetrack <- sprintf("%s/%s/%s.%s.%s.data.tsv",dirbase,inputdir,parts['subj'],parts['date'],parts['run'])
 saveto   <- sprintf("%s/%s/%s.%s.%s.sac.tsv",dirbase,outputdir,parts['subj'],parts['date'],parts['run'])

 # maybe we have taskname in the file?
 if(!file.exists(eyetrack)) eyetrack<-sprintf("%s/%s/%s.%s.%s.%s.data.tsv",dirbase,inputdir, parts['subj'],parts['date'],taskname,parts['run'])
 # 20190625 - RPG/mMR does not match rest of studies
 if(!file.exists(eyetrack)){
    eyetrack<-sprintf("%s/%s/%s.%s.%s.fake.tsv",dirbase,'txt', parts['subj'],parts['date'],parts['run'])
    saveto   <- sprintf("%s/%s/%s.%s.%s.sac.tsv",dirbase,'txt/scored_fake',parts['subj'],parts['date'],parts['run'])
 }


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

getRunDot <- function(dotnotation, showplot=F,funnybusiness='',showcmd=F,auditor=NULL, cleanup=T) {
 if(is.null(auditor)) {
  cat('What are your initials? ')
  auditor<-readline()
 }

 parts <- unlist(strsplit(dotnotation, '\\.'))
 parts <- as.numeric(parts);
 names(parts) <- c('subj','date','run')

 dirbase  <- sprintf("%s/%s/%s", filebasedir, parts['subj'], parts['date'])
 eyetrack <- sprintf("%s/Raw/EyeData/txt/%s.%s.%s.data.tsv",dirbase,parts['subj'],parts['date'],parts['run'])
 # for mix, maybe also anti?
 if(!file.exists(eyetrack)) eyetrack <- sprintf("%s/Raw/EyeData/txt/%s.%s.%s.%s.data.tsv",dirbase,parts['subj'],parts['date'],taskname,parts['run'])

 # maybe we can use eprime?
 EPfile    <- sub('data.tsv$','eplog.txt',eyetrack)
 print(EPfile)
 EPcorrect <- tryCatch({ 
     correct <- read.table(file=EPfile,sep="\t",header=T)$Correct
     if(!is.null(correct)){cat('found and using eprime log file\n')}
     correct
    },error=function(e){ cat('couldnt open eplog!\n'); NULL })

 if (showplot==T) {
    # where to save
    tname <- basename(gsub("Basic/", "", filebasedir))
    auditpath <- file.path("audit", tname)
    if (!dir.exists(auditpath)) dir.create(auditpath)
    auditfile <- file.path(auditpath, paste0(dotnotation, ".", auditor, ".audit.txt"))

    # if first pass
    trials <- 1:expectedTrialLengths
    auditdf <- NULL

    # try to resume from semi completed
    if (file.exists(auditfile)) {
       auditdf <- read.table(auditfile)
       trials <- setdiff(trials, auditdf$trail)
       if (length(trials)==0L) {
          warnings("already finished! to redo, rm ", auditfile)
       } else {
          cat("resuming from ", trials[1])
       }
    }

    for(trial in trials){
       ## TODO, open audit file, redo
       print(trial)
       sacs<-getSacs(eyetrack,
               parts['subj'],
               parts['run'],
               bname,
               onlyontrials=trial,
               writetopdf=F,
               showplot=showplot,
               rundate=parts['date'],
               funnybusiness=funnybusiness)
       score<-scoreSac(sacs)
       print(score)

       ## ask questions
       cat('Scored Correctly?[enter for yes]')
       good<-readline()
       if(good==""){
        good=1;
        shouldbe=score$Count;
        reason="";
       } else {
         good=0;
         cat("what should the score be\n(-1 drop,0 incorect, 1 correct, 2 error corr, NA = donno)\n")
         shouldbe<-readline()
         cat("whats wrong?")
         reason<-readline()
       }

       # cleanup plots when we are done
       if(showplot & cleanup) dev.off()

      # show all of these
      thisrun<-data.frame(subj=parts['subj'],visit=parts['date'], run=parts['run'], 
                           trail=trial,correctly=good,reason=reason,
                           scoreas=score$Count, shoudlbe=shouldbe, auditor=auditor)
       # append or create
       if( is.null(auditdf) ) {
        print('replacying null')
        auditdf <- thisrun
       } else {
        print('adding')
        auditdf <- rbind(auditdf,thisrun)
       }
       write.table(file=auditfile,auditdf)
    }
 }
 
 allsacs <- getSacs(eyetrack,parts['subj'],parts['run'],bname,onlyontrials=1:expectedTrialLengths,writetopdf=F,showplot=F,rundate=parts['date'],funnybusiness=funnybusiness)
 scoreSac(allsacs, EPcorrect=EPcorrect)
}

cat("use getSacDot('lunaid.yyyymmdd.r#.t#') or getRunDot('lunaid.yyyymmdd.r#',showplot=F)\n")

