library(plyr)
library(ggplot2)
for(task in c('anti','bars','scannerbars')) {
   b           <- read.table(sprintf('%s/results/checkAgainstManual_trial.csv',task),header=T)
   badidxs<-grep('\\*',b$trial)
   if(length(badidxs)>0) {
    b.good      <- b[-badidxs,] 
   } else {
    b.good <-b
   }
   
   b.good$year <- substr(b.good$trial,7,10)
   
   scorer      <- ddply(b.good,.(scorer, year), 
                   function(x){
                     cbind(total=length(x$count_m),
                       accuracy=length(which(x$count_a==x$count_m))/length(x$count_a) )
                   } )
   
   p <- ggplot(scorer,aes(x=year,y=accuracy,size=total))  +
       geom_point()  +
       facet_wrap(~scorer) + #, scales='free_x') +
       theme(axis.text.x = element_text(angle = 90, hjust = 1))+
       scale_y_continuous(limits=c(.5,1)) +
       ggtitle(sprintf('%s: scorer compared to automatic',task)) + ylab('% agreement')
   ggsave(p,file=sprintf('%s_byscorer.pdf',task))
}
