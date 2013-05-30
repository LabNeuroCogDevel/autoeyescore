#TO USE:
#  test_file('scoreTests.R')
#
# put expected cases in trials.txt
# and use setTests.bash to pull in the tsvs
# ** should be run in task/test/
#
# this is wrapped by readme.bash at root level
#
# wd should be the working directory where trials.txt is. needed b/c test_file does chdir=T
setwd(wd)
library(testthat)

source(  sprintf( '../%s',  grep('settings.R',list.files('..'),value=T)   ) );
source('../../ScoreRun.R')

testTrials<-read.table('trials.txt',header=T,sep="\t",
  colClasses=c('character','integer','integer','integer','integer','integer','logical','logical','integer','character') )

maxlatdiff <- 10


for (i in 1:dim(testTrials)[1] ) {
   trial <- testTrials[i,]
   with(trial, {
     filename <- sprintf("input/%d.%d.%d.data.tsv",subj,date,run)
     cmd<-sprintf("getSacs('%s', %d,%d,'%s',%d,onlyontrials=%d,showplot=T,writetopdf=F) ", filename, subj,run,Task,date,trial)
     capture.output({
        print(sprintf(cmd))
        #print(str(score))
        sacs  <- getSacs( filename, subj,run,Task,date, onlyontrials=trial,showplot=F,writetopdf=T) 
        score <- scoreSac(sacs) 
      })

      context(sprintf("\n%s\n%d@%d %d.%d: %s\n",cmd,subj,date,run,trial,reason))
      test_that( "count"  , { expect_equal( score$Count        , Count        ) }  )
      test_that( "first"  , { expect_equal( score$fstCorrect   , fstCorrect   ) }  )
      test_that( "errcorr", { expect_equal( score$ErrCorr      , ErrCorr      ) }  )
      test_that( "lat"    , { expect_true (all(is.na(c(lat,score$lat))) || abs(score$lat-lat)< maxlatdiff  ) }  )
   })
}
