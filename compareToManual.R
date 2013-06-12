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
compared.plot<-ggplot(compared,aes(x=count_m,y=p,label=occurances))+geom_bar(stat="identity")+facet_wrap(~count_a)+ggtitle('Percents in auto-score classification')+theme_bw()+geom_text()
#ggsave(file='results/accuracy-breakdown.pdf',p)
print(compared.plot)
