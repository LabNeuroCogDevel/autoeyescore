# make function to show how scorer and algorithm disagree
showdiffs <-function(auto=NA,manual=NA,size=10,xdat=NA) {
 savetxt<-sprintf('%s.%s.csv',Sys.Date(),Sys.time());
 if(!is.na(auto) || !is.na(manual)) {
   viewBOOL <- n$count_a!=n$count_m
 }else{
   viewBOOL <- rep(T, length(n$count_a))
 }
 if(!is.na(auto)  ) { 
    viewBOOL <- viewBOOL&n$count_a==auto 
    savetxt<-sprintf('a%d.%s',auto,savetxt)
  }

 if(!is.na(manual)) { 
    viewBOOL <- viewBOOL&n$count_m==manual  
    savetxt<-sprintf('m%d.%s',manual,savetxt)
  }

 if(!is.na(xdat)  ) { 
    viewBOOL <- viewBOOL&n$xdat==xdat  
    savetxt<-sprintf('x%d.%s',xdat,savetxt)
  }

 # grab samples and remove from n
 sampleidx<-sample(which(viewBOOL),size)
 trials <- n[sampleidx,]
 n<-n[-sampleidx,]

 results <- ddply(trials,.(trial), function(x){
    print(x);
    print(x$trail)
    a<-getSacDot(as.character(x$trial));
    print(scoreSac(a));
    cat('which is correct,  manual(m) or algorithm(a)?')
    correct <- readline();
    dev.off();
    cbind(x,whosCorrect=correct)
  })

  write.csv(results,file=savetxt)
  results
}
