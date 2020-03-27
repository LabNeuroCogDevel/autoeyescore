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
if(!file.exists(Bdir)) { Bdir <- "/Volumes/L/" }
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

dot2runinfo <- function(dotnotation) {
 # expected in scope:
 #  filebasedir, taskname
 
 parts <- unlist(strsplit(dotnotation, '\\.'))
 parts <- as.numeric(parts);
 names(parts) <- c('subj','date','run')
 if(length(parts) > 3) names(parts)[4] <- 'trial'

 dirbase  <- sprintf("%s/%s/%s", filebasedir, parts['subj'], parts['date'])
 inputdir <- 'Raw/EyeData/txt'
 outputdir <- 'Scored/txt'
 eyetrack <- sprintf("%s/%s/%s.%s.%s.data.tsv",dirbase,inputdir,parts['subj'],parts['date'],parts['run'])
 saveto   <- sprintf("%s/%s/%s.%s.%s.sac.tsv",dirbase,outputdir,parts['subj'],parts['date'],parts['run'])


 # taskname might be in eye file (mix, maybe also anti?)
 if(!file.exists(eyetrack))
     eyetrack <- sprintf("%s/Raw/EyeData/txt/%s.%s.%s.%s.data.tsv",dirbase,parts['subj'],parts['date'],taskname,parts['run'])

 # 20190625 - RPG/mMR does not match rest of studies
 if(!file.exists(eyetrack)){
    eyetrack<-sprintf("%s/%s/%s.%s.%s.fake.tsv",dirbase,'txt', parts['subj'],parts['date'],parts['run'])
    saveto   <- sprintf("%s/%s/%s.%s.%s.sac.tsv",dirbase,'txt/scored_fake',parts['subj'],parts['date'],parts['run'])
 }

 # maybe we can use eprime?
 EPfile    <- sub('data.tsv$','eplog.txt',eyetrack)
 EPcorrect <- tryCatch({ 
     correct <- read.table(file=EPfile,sep="\t",header=T)$Correct
     if(!is.null(correct)){cat('found and using eprime log file\n')}
     correct
    },error=function(e){ cat('couldnt open eplog!\n'); NULL })

 return(list(eyetrack=eyetrack, saveto=saveto, parts=parts, EPcorrect=EPcorrect))
}

getSacDot <- function(dotnotation, showplot=T,funnybusiness='',showcmd=F) {
 di <- dot2runinfo(dotnotation)

 # if we want subj.date.run.* # all trials
 if(grepl('\\*$',dotnotation || length(di$parts) <= 3)){
  #trial=sprintf('1:%d',expectedTrialLengths)
  trial=1:expectedTrialLengths
  showplot=F
 }else{
  trial=di$parts['trial'] 
 }
 if(showcmd) {cat(sprintf("getSacsI('%s',onlyontrials='%s',savedas='%s',writetopdf=F,showplot=%s)\n",
                          dotnotation,trial,di$saveto,showplot))  }

 getSacsI(dotnotation,onlyontrials=trial,savedas=di$saveto,writetopdf=F,showplot=showplot,funnybusiness=funnybusiness)
}


getSacI <- function(info, ...) {
    # wrapper for getSac using dot2runinfo's output
    # onlyontrials=trial,savedas=saveto,writetopdf=F,showplot=showplot,rundate=parts['date'],funnybusiness=funnybusiness

    # if given dotnotation, convert to info
    if(is.character(info)) info <- dot2runinfo(info)
    # cat('getSacI: info and ', paste(collapse=",", sep=", ", ...), "\n")
    getSacs(info$eyetrack, info$parts['subj'], info$parts['run'], bname, rundate=info$parts['date'], ...)
}
mkpdf <- function(dot, pdffname, ...) {
    di <- dot2runinfo(dot)
    # cat('mkpdf: ', dot, pdffname, paste(sep=", ", ...), "\n")
    sacs <- getSacI(di, onlyontrials=di$part['trial'], writetopdf=T, showplot=T,pdffname=pdffname, ...)
}
pdfrun <- function(dot, outdir, ...) {
    # cat('pdfrun given ', dot, outdir, paste(sep=", ", ... ))
    for(i in 1:expectedTrialLengths){
        trl <- sprintf("%s.%02d",dot,i)
        sacs <- getSacI(dot, onlyontrials=i, ...)
        sc <- scoreSac(sacs)
        outname <- file.path(outdir, sprintf('%s:%d.pdf', trl, sc$Count[1]))
        mkpdf(trl, outname, ...)
    }
}

getRunDot <- function(dotnotation, showplot=F,funnybusiness='',showcmd=F,auditor=NULL, cleanup=T, pdfdir=NULL) {
 # "audit trials in run"
 if(is.null(auditor)) {
  cat('What are your initials? ')
  auditor <- readLines("stdin", n=1)
 }

 i <- dot2runinfo(dotnotation)
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

    savedas<-NULL
    writetopdf <- F
    for(trial in trials){
       ## TODO, open audit file, redo
       print(trial)
       sacs<-getSacsI(i,
               onlyontrials=trial,
               showplot=showplot, writetopdf=writetopdf,savedas=savedas,
               funnybusiness=funnybusiness)
       score <- scoreSacs(sacs)
       print(score)

       ## ask questions
       cat('Scored Correctly?[enter for yes]')
       good <- readLines("stdin", n=1)
       if(good==""){
        good=1;
        shouldbe=score$Count;
        reason="";
       } else {
         good=0;
         cat("what should the score be\n(-1 drop,0 incorect, 1 correct, 2 error corr, NA = donno)\n")
         shouldbe <- readLines("stdin", n=1)
         cat("whats wrong?")
         reason <- readLines("stdin", n=1)
       }

       # cleanup plots when we are done
       if(showplot & cleanup) dev.off()

      # show all of these
      thisrun<-data.frame(subj=parts['subj'],visit=parts['date'], run=parts['run'], 
                           trail=trial,correctly=good,reason=reason,
                           scoreas=score$Count, shoudlbe=shouldbe, auditor=auditor)
      # append or create
      if( is.null(auditdf) ) {
       # print('replacing null')
       auditdf <- thisrun
      } else {
       # print('adding')
       auditdf <- rbind(auditdf,thisrun)
      }
      write.table(file=auditfile,auditdf)
    }
 }
 
 allsacs <- getSacs(eyetrack,parts['subj'],parts['run'],bname,onlyontrials=1:expectedTrialLengths,writetopdf=F,showplot=F,rundate=parts['date'],funnybusiness=funnybusiness)
 scoreSac(allsacs, EPcorrect=i$EPcorrect)
}

cat("use getSacDot('lunaid.yyyymmdd.r#.t#') or getRunDot('lunaid.yyyymmdd.r#',showplot=F)\n")

