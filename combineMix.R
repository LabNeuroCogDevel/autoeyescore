#!/usr/bin/env Rscript

# read all manual scored txt files (parsed from xls by perl `./getman_mix.pl`
# make giant csv

# read in one file:
# set filename (subj,visit,type) into data frame, remove drop
#
# NB strsplit expects filename to be like 
#    mix/subj.visit.type.manual.txt
#     1   2    3     4    5      6
extFN <- function(f) { 
  info <- strsplit(f,'[/.]')[[1]][2:4]
  d <- setNames( read.table(f,sep='\t',header=F,skip=1), c('trial','score','lat') )
  d[,c('subj','date','sac.type')] <- rep(info,each=nrow(d))
  d[d$score!=-1,]
}

# get all anti and vgs files
fs <- c(Sys.glob('mix/*anti.manual.txt'),Sys.glob('mix/*vgs.manual.txt'))

# read in each, put into one big df
alldata <- do.call(rbind, lapply(fs, extFN) )

# write table with a reasonable order of both rows and columns
write.csv(alldata[order(d$subj,d$date,d$trial),
                  c('subj','date','trial','sac.type','score','lat')],
          file='mix/allmix.csv',row.names=F,quote=F)
