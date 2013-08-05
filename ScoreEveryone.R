#
# Score Everyone (in txt/*.tsv)
# or just one person
#
# --- score 11129 and don't plot
# Rscript ScoreEveryone.R txt/11129.anti.1.tsv F
#
# if run on one person, summary stats/graphs are not made

library(plyr)
library(foreach)
library(reshape2)
library(ggplot2)
# do this after sourcing the approp. settings file and Run scoring functions
#  source('*.settings.R')
#  source('ScoreRun.R')


#args<-commandArgs(TRUE)
#if(length(args)>0) {
# files    <- args[1]
#} else {
# files    <- Sys.glob('txt/*.tsv')
#}
#plotTrial=F
#if(length(args)>1) {
# plotTrial=T
#}

#files    <- Sys.glob('txt/*.tsv')
plotTrial=F


# show subjects that have insufficent run numer
# MIX is not named well
#r<-rle(sort(as.numeric(splitfile$subj)))
#r$lengths[ which(r$lengths!=4) ]

#r<-rle(sort(paste(splitfile$subj, splitfile$type)))

dropScore <- function(subj,rundate,run,type,reason) {
   cat(subj,run,type, reason,"\n")
   output<-list('PSCor'=NaN,'PSCorErr'=NaN,'PSErr'=NaN,'ASCor'=NaN,'ASErrCor'=NaN,'ASErr'=NaN, 'Dropped'=NaN,
                'AScor.lat'=NaN,'ASErrCor.lat'=NaN,'ASErr.lat'=NaN,'PScor.lat'=NaN,'PSErrCor.lat'=NaN,'PSErr.lat'=NaN)
   output$total <- 0
   #output$xdat  <- NaN # how'd this get in there?
   output$subj  <- subj
   output$date  <- rundate
   output$type  <- type
   output$run   <- run
   return(output)
}

getsubj <- function(i,reuse=T){
  filename <- splitfile$file[i]
  type     <- splitfile$type[i]
  rundate  <- splitfile$date[i]
  run      <- splitfile$run[i]
  subj     <- splitfile$subj[i]
  id       <- splitfile$id[i]  
  savedas  <- splitfile$savedas[i]
  pertrialoutput <- sub('.sac.txt$','.trial.txt',splitfile$savedas[i])

  # from ScoreRun.R
  # saverootdir  <- 'eyeData'
  # outputdir <- paste(saverootdir, subj,runtype,sep="/")
  # file=paste(outputdir,"/",subj,"-",runtype,".txt",sep="")
  #savedas  <- paste(  paste('eyeData',subj,id,sep="/"), "/", subj, "-", type,'.txt', sep="")
  #print('looking for')
  #print(savedas)

  #print(filename)
  #print(savedas)
  if(file.exists(savedas) && reuse) {
    print(paste(i,'reading',savedas))
    readfilesuccess <-
     tryCatch({ 
         allsacs <- read.table(sep="\t",header=T, file=savedas)
        },error=function(e){
            cat('error! cant read input\n')
            NULL
        })

  } else{

    cat(sprintf('%d: running: getSacs("%s",%s,%s,"%s",rundate=%s,writetopdf=%s,savedas="%s")\n',
                i,filename, subj,run, type,rundate,plotTrial,savedas))

     allsacs <- tryCatch({ 
        getSacs( filename, subj,run, type,rundate=rundate,writetopdf=plotTrial,savedas=savedas) 
        #sprintf('getSacs(%s,%s,%d,%d,rundate=%s,writetopdf=F,savedas=%s)')
        },error=function(e){
            cat(sprintf('getSacs failed on %s.%s.%d\n',subj,rundate,run))
            #dropTrial(subj,runtype,1:expectedTrialLengths,0,'no data in eyd!',
            #             NA,showplot=F,run=run,rundate=rundate)
            NULL
        })

  }
  
  if(length(allsacs)<1|is.null(allsacs)) {
   output<-dropScore(subj,rundate,run,type,'no info')
   return(as.data.frame(output))
  }

  # SCORE SACCADES
  # get a line per trial dataframe
  
  cor.ErrCor.AS<-tryCatch({ 
      scoreSac(allsacs)
     },error=function(e){
         cat(sprintf('scoreSac failed on %s.%s.%d\n',subj,rundate,run))
     })

  #if(!scorable){
  # output<-dropScore(subj,run,type,sprintf('scoreSac failed on %d.%d.%d\n',subj,rundate,run))
  # return(as.data.frame(output))
  #}

  if(!file.exists(dirname(pertrialoutput))) { dir.create(dirname(pertrialoutput),recursive=T)}
  write.table(file=pertrialoutput,cor.ErrCor.AS,row.names=F,quote=F,sep="\t")
  
  r <- scoreRun(cor.ErrCor.AS, 1:expectedTrialLengths  ) #was ( , unique(allsacs$trial))
  r$subj  <- subj
  r$date  <- rundate 
  r$run   <- run
  r$type  <- type


  # save summary as a one line file in subject directory
  summaryoutput <- sub('.trial.txt$','.summary.txt',pertrialoutput)

  if(!file.exists(dirname(summaryoutput))) { dir.create(dirname(summaryoutput),recursive=T)}
  write.table(file=summaryoutput,r,row.names=F,quote=F,sep="\t")
  #print(c('wrote',subj))

  return(r)


  #TODO: need to find stats for
  #ASCORR ASERRCORR ASERRUNCORR ASERRCORR|ASERROR ASDROPPS CORR PSERR PSDROP
  #ASCORR_PREC	PSCORR_PREC	ASCORRcorr_PREC	PSCORRcorr_PREC	ASERRcorr_PREC	ASCORR_ABSPREC	PSCORR_ABSPREC	ASCORRcorr_ABSPREC	PSCORRcorr_ABSPREC	ASERRcorr_ABSPREC	ASCORR_MOSTPREC	PSCORR_MOSTPREC
   
  
  #break
}

scoreEveryone <- function(splitfile,plotfigs=T,saveoutput=T,reuse=T){
   #mtrace('getsubj')
   # TODO: wrap this in an tryCatch so no errors at the end
   perRunStats <- foreach(i=1:nrow(splitfile),.combine=rbind ) %dopar% getsubj(i,reuse=reuse)
   # this will have written a .sac.txt and .trial.txt for every subject

   #perRunStats <- getsubj(1)
   #for( i in 2:nrow(splitfile) ){
   # print(i)
   # s <-getsubj(i)
   # perRunStats <- rbind(perRunStats,s )
   #}

   # only do final summaries if we actually grabbed everyone
   if(plotfigs ) {
      #print(perRunStats)
      p.all<-ggplot(perRunStats, aes(x=as.factor(total),fill=type))+geom_histogram(position='dodge')


      sums<-aggregate(. ~ subj, perRunStats[,c(1:8,match('subj',names(perRunStats))  )],sum)
      longsums <- melt(sums[,names(sums) != 'total'],id.vars="subj")
      p.subj<- ggplot( longsums ) + ggtitle('per subject breakdown of collected data')
      p.subj<- p.subj +geom_histogram(aes(x=subj,y=value,fill=variable,stat='identity'))

      if(saveoutput){
       write.table(sums,file='results/sums.tsv',sep="\t",quote=F,row.names=F)
       ggsave(p.all,file='results/totalsHist.png')
       ggsave(p.subj,file='results/perSubjHist.png')
       png('results/droppedHist.png')
      } else {
       print(p.all)
       x11()
       print(p.subj)
       x11()
      }

      hist(perRunStats$Dropped)

      if(saveoutput){ dev.off() }
   }
   return(perRunStats)
}

