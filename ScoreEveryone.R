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
# do this after sourcing the approp. settings file and Run scoring functions
#  source('*.settings.R')
#  source('ScoreRun.R')


args<-commandArgs(TRUE)
if(length(args)>0) {
 files    <- args[1]
} else {
 files    <- Sys.glob('txt/*.tsv')
}
plotTrial=F
if(length(args)>1) {
 plotTrial=T
}

# get files to act on
splitfile <- getFiles()  # sourced from task specific settings file

# show subjects that have insufficent run numer
# MIX is not named well
#r<-rle(sort(as.numeric(splitfile$subj)))
#r$lengths[ which(r$lengths!=4) ]

#r<-rle(sort(paste(splitfile$subj, splitfile$type)))


getsubj <- function(i){
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
  if(file.exists(savedas) ) {
    print(paste('reading',savedas))
    allsacs <- read.table(sep="\t",header=T, file=savedas)
  } else{
    cat(sprintf('running: getSacs("%s",%s,%s,"%s",rundate=%s,writetopdf=%s,savedas="%s")\n',filename, subj,run, type,rundate,plotTrial,savedas))
    allsacs <- getSacs( filename, subj,run, type,rundate=rundate,writetopdf=plotTrial,savedas=savedas) 
  }
  
  if(length(allsacs)<1) {
   cat(subj,run,type, "no info\n")
   output<-list('PSCor'=NaN,'PSCorErr'=NaN,'PSErr'=NaN,'ASCor'=NaN,'ASErrCor'=NaN,'ASErr'=NaN, 'Dropped'=NaN,
                'AScor.lat'=NaN,'ASErrCor.lat'=NaN,'ASErr.lat'=NaN,'PScor.lat'=NaN,'PSErrCor.lat'=NaN,'PSErr.lat'=NaN)
   output$total <- 0
   output$xdat  <- NaN
   output$subj  <- subj
   output$run   <- run
   output$date  <- rundate
   output$type  <- type
   return(as.data.frame(output))
  }

  # TODO: score sac goes here
  cor.ErrCor.AS <- scoreSac(allsacs)
  write.table(file=pertrialoutput,cor.ErrCor.AS,row.names=F,quote=F,sep="\t")
  
  r <- scoreRun(cor.ErrCor.AS, expectedTrialLengths  ) #was ( , unique(allsacs$trial))
  r$subj  <- subj
  r$date  <- rundate 
  r$run   <- run
  r$type  <- type


  # save summary as a one line file in subject directory
  summaryoutput <- sub('.trial.txt$','.summary.txt',pertrialoutput)
  dir.create(dirname(summaryoutput),recursive=T)
  write.table(file=summaryoutput,r,row.names=F,quote=F,sep="\t")
  #print(c('wrote',subj))

  return(r)


  #TODO: need to find stats for
  #ASCORR ASERRCORR ASERRUNCORR ASERRCORR|ASERROR ASDROPPS CORR PSERR PSDROP
  #ASCORR_PREC	PSCORR_PREC	ASCORRcorr_PREC	PSCORRcorr_PREC	ASERRcorr_PREC	ASCORR_ABSPREC	PSCORR_ABSPREC	ASCORRcorr_ABSPREC	PSCORRcorr_ABSPREC	ASERRcorr_ABSPREC	ASCORR_MOSTPREC	PSCORR_MOSTPREC
   
  
  #break
}
perRunStats <- foreach(i=1:nrow(splitfile),.combine=rbind ) %do% getsubj(i)
#perRunStats <- getsubj(1)
#for( i in 2:nrow(splitfile) ){
# print(i)
# s <-getsubj(i)
# perRunStats <- rbind(perRunStats,s )
#}

# only do final summaries if we actually grabbed everyone
if(length(args) == 0 ) {
   #print(perRunStats)
   p<-ggplot(perRunStats, aes(x=as.factor(total),fill=type))+geom_histogram(position='dodge')
   ggsave(p,file='results/totalsHist.png')

   png('results/droppedHist.png')
   hist(perRunStats$Dropped)
   dev.off()

   sums<-aggregate(. ~ subj, perRunStats[,c(1:8,match('subj',names(perRunStats))  )],sum)
   # need to includ run in perrunstats[] >> #sums<-aggregate(. ~ subj+run, perRunStats[,c(1:8,match('subj',names(perRunStats))  )],sum)
   write.table(sums,file='results/sums.tsv',sep="\t",quote=F,row.names=F)
   p <- ggplot( melt(sums[,names(sums)!='total']  ,id.vars='subj') ) + ggtitle('per subject breakdown of collected data')
   p <- p+geom_histogram(aes(x=subj,y=value,fill=variable,stat='identity'))
   ggsave(p,file='results/perSubjHist.png')
}
