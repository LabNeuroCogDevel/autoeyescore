library(plyr)
library(ggplot2)
# expect to be run from task subdir, and that results/accuracy-breakdown.txt exists in there
a<-read.table('results/accuracy-breakdown.txt',header=T)
# "count" refers to how many movements until the correct is made ( count per trial)
# a=automatic, m=manual
#names(a)<-c('occurances','count_a','count_m')
names(a)[1]<-'occurances'
# group by what will be the facet (auto score) and divied by all in that facet to get a percent
compared<-ddply(a,.(count_a),function(x){x$p <- x$occurances/sum(x$occurances);x})
# mask broken bits
compared<-compared[!is.na(compared$count_a),]

# rename numbers to 
l <- c('Drop','Error','Cor','ErrorCor')
compared$count_a<-factor(compared$count_a, levels=c(-1:2), labels=l)                             
compared$count_m<-factor(compared$count_m, levels=c(-1:2), labels=l)                             

compared$scoresMatch <- compared$count_m==compared$count_a;
compared.plot<-ggplot(compared,aes(x=count_m,y=p,label=occurances,fill=scoresMatch))+
                 geom_bar(stat="identity")+
                 facet_wrap(~count_a)+
                 ggtitle(sprintf('%s: per trial classification by automic score',basename(getwd())))+
                 theme_bw()+
                 geom_text()+
                 ylab('% automatic agrees with manual')+xlab('manually scored as')

print(compared.plot)
#ggsave(file='results/accuracy-breakdown.pdf',compared.plot)
