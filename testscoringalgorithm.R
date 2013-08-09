library(testthat) # may need to install.packages('testthat'); # see ?testthat

print("hello world")

ttt <- read.table('trialstotest.csv', header=T,sep=',')


for(i in 1:dim(ttt)[1]) {
	settingsfile <- sprintf('%s/%s.settings.R',ttt$Paradigm[i],ttt$Paradigm[i])
	source(settingsfile)
	source('getSacsDot.R')
	saccades <- getSacDot(as.character(ttt$Trial[i]),showplot=F)
	scored.trial <- scoreSac(saccades)
	print(as.numeric(scored.trial[7]))
	print(ttt$ExpectedScore[i])
	test_that(ttt$Description[i], { expect_equal(as.numeric(scored.trial[7]), ttt$ExpectedScore[i]) } )
}
