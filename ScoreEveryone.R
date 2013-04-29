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
# do this after sourcing *.settings.R
#source('ScoreRun.R')


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

  if(file.exists(savedas) ) {
    print(paste('reading',savedas))
    allsacs <- read.table(sep="\t",header=T, file=savedas)
  } else{
    print(sprintf('running: scoreRun(%s,%s,%s,%s,%s,writetopdf=%s,savedas=%s)',filename, subj,run, type,plotTrial,savedas))
    allsacs <- scoreRun( filename, subj,run, type,writetopdf=plotTrial,savedas=savedas) 
  }
  
  if(length(allsacs)<1) {
   cat(subj,run,type, "no info\n")
   output<-list('PSCor'=NaN,'PSCorErr'=NaN,'PSErr'=NaN,'ASCor'=NaN,'ASErrCor'=NaN,'ASErr'=NaN, 'Dropped'=NaN,
                'AScor.lat'=NaN,'ASErrCor.lat'=NaN,'ASErr.lat'=NaN,'PScor.lat'=NaN,'PSErrCor.lat'=NaN,'PSErr.lat'=NaN)
   output$total <- 0
   output$xdat  <- NaN
   output$subj  <- subj
   output$run   <- run
   output$type  <- type
   return(as.data.frame(output))
  }
  # select only those saccades we will count
  goodsacs <- subset(allsacs, subset=intime&gtMinLen&p.tracked>0 )
  # break into trials, do we have a first correct movement, correct movement after incorrect, an xdat that says Anti Saccade?
  cor.ErrCor.AS <- ddply(goodsacs, .(trial), function(x) { c(x$xdat[1], round(x$onset[1]*1000), x$cordir[1]==TRUE, !x$cordir[1]&any(x$cordir), xdatIsAS(mean(x$xdat))) } )
  names(cor.ErrCor.AS) <- c('trial','xdat','lat','fstCorrect','ErrCorr','AS')
  cor.ErrCor.AS[,c('fstCorrect','ErrCorr','AS')]<-sapply(cor.ErrCor.AS[,c('fstCorrect','ErrCorr','AS')],as.logical)
  cor.ErrCor.AS$Count <- 0
  cor.ErrCor.AS$Count[ which(cor.ErrCor.AS$fstCorrect == T ) ] <- 1
  cor.ErrCor.AS$Count[ which(cor.ErrCor.AS$ErrCorr    == T ) ] <- 2
  #fstCorrect&ErrCor
  #write.table(file=paste(  paste('eyeData',subj,id,sep="/"), "/", subj, "-", type,'.pertrial.txt', sep=""),cor.ErrCor.AS,row.names=F,quote=F)
  write.table(file=pertrialoutput,cor.ErrCor.AS,row.names=F,quote=F)
  
  ## dropped trials
  dropped <- setdiff( unique(allsacs$trial),  cor.ErrCor.AS$trial)
  #TODO: are dropped pro or anti saccades
  
  ## Pro Saccade
  PS <- subset(cor.ErrCor.AS, subset=!AS)
  PS.cor    <- which(PS$fstCorrect)
  PS.ErrCor <- which(PS$ErrCorr)
  PS.Err    <- which(!( PS$fstCorrect | PS$ErrCorr))

  ## Anti Saccade
  AS <- subset(cor.ErrCor.AS, subset=AS)
  AS.cor    <- which(AS$fstCorrect)
  AS.ErrCor <- which(AS$ErrCorr)
  AS.Err    <- which(!( AS$fstCorrect | AS$ErrCorr))

  lats <-as.data.frame(t(
   c(
     sapply(list(AScor.lat=AS.cor,ASErrCor.lat=AS.ErrCor,ASErr.lat=AS.Err),function(x){mean(AS$lat[x])}), 
     sapply(list(PScor.lat=PS.cor,PSErrCor.lat=PS.ErrCor,PSErr.lat=PS.Err),function(x){mean(PS$lat[x])})
   )))

  #simple stat
  stats <- list('PSCor'=PS.cor,'PSCorErr'=PS.ErrCor,'PSErr'=PS.Err,'ASCor'=AS.cor,'ASErrCor'=AS.ErrCor,'ASErr'=AS.Err, 'Dropped'=dropped)
  lengths <- lapply(stats,length)
  #print(lengths)
  #print(sum(lengths))
  lengths$total <- sum(unlist(lengths))

  r <- cbind(as.data.frame(lengths), lats) 
  r$subj  <- subj
  r$run   <- run
  r$type  <- type



  # save summary as a one line file in subject directory
  summaryoutput <- sub('.trial.txt$','.summary.txt',pertrialoutput)
  write.table(file=summaryoutput,r,row.names=F,quote=F)

  return(r)


  #TODO: need to find stats for
  #ASCORR ASERRCORR ASERRUNCORR ASERRCORR|ASERROR ASDROPPS CORR PSERR PSDROP
  #ASCORR_PREC	PSCORR_PREC	ASCORRcorr_PREC	PSCORRcorr_PREC	ASERRcorr_PREC	ASCORR_ABSPREC	PSCORR_ABSPREC	ASCORRcorr_ABSPREC	PSCORRcorr_ABSPREC	ASERRcorr_ABSPREC	ASCORR_MOSTPREC	PSCORR_MOSTPREC
   
  
  #break
}
perRunStats <- foreach(i=1:nrow(splitfile),.combine=rbind ) %do% getsubj(i)

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
