###> source('fix/fix.settings.R')
###> source('getSacsDot.R')
###sacs <-  getSacDot('11151.20130408.1.*',showplot=F,funnybusiness="ignorefirstsactrack,nothingistoofast",showcmd=T)
scoreFix <- function(sacs) {
	a<-scoreSac(sacs);
	a$Count[a$Desc=="no saccades"]<-1;
	a$Count[a$Desc==""]<-0;
	a$Count[a$Desc=="no good saccades"]<-1;
	a$Count[grepl("no saccades within sac.time", a$Desc) ]<-1;
	return(a) 
}

### other score functin outputs a dataframe with the following columns: "trial" "xdat" "lat" "fstCorrect" "ErrCorr" "AS" "Count""Desc" 
