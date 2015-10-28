# works on output of parseEP1es.pl
readxdats <- function(f.in) {
  d<-setNames(read.table(f.in),c('level','event','dur','xdat'))
  d$dur[d$dur < 0|is.na(d$dur) ] <- 0
  d$time<-cumsum(d$dur)/1000
  xdat <- d[!is.na(d$xdat),c('event','xdat','time')] 
  xdat$trial <- cumsum(xdat$xdat < 100)
  xdat$etype <- cut(xdat$xdat,breaks=c(0,100,200,260),labels=c('start','delay','end'))

  f.out <- sprintf("%s_xdats.txt",gsub(".txt","",f.in) )
  write.table(file=f.out,col.names=F,row.names=F,quote=F, xdat )
  xdat
  # dcast.data.table(setDT(xdat),trial~etype,value.var='xdat')
}


infs <- Sys.glob('runOrderXdat/[1-3].txt')
x <- lapply(infs,readxdats)
all <- do.call(rbind,x)
all$run <- cumsum(all$trial==1&all$etype=='start')
write.table(all,file="runOrderXdat/all_xdats.txt")

