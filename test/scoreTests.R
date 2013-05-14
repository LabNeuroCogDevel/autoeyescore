#TO USE:
#  test_file('scoreTests.R')
#
# put expected cases in task_subj_date_run_trial.list
# and use setTests.bash to pull in the tsvs
#
library(testthat)

source('../bars/bars.settings.R')
source('../ScoreRun.R')

testTrials<-read.table('task_subj_date_run_trial.list',header=T,sep="\t")
maxlatdiff <- 10


for (i in 1:dim(testTrials)[1] ) {
   trial <- testTrials[i,]
   with(trial, {
     filename <- sprintf("input/%d.%d.%d.tsv",subj,date,run)
     cmd<-sprintf("getSacs('%s', %d,%d,'%s',onlyontrials=%d,showplot=T,writetopdf=F) ", filename, subj,run,Task,trial)
     #capture.output({
        print(sprintf(cmd))
        #print(str(score))
        sacs  <- getSacs( filename, subj,run,Task,onlyontrials=trial,showplot=F,writetopdf=F) 
        score <- scoreSac(sacs) 
     # })

      context(sprintf("\n%s\n%d@%d %d.%d: %s\n",cmd,subj,date,run,trial,reason))
      test_that( "count"  , { expect_equal( score$Count        , Count        ) }  )
      test_that( "first"  , { expect_equal( score$fstCorrect   , fstCorrect   ) }  )
      test_that( "errcorr", { expect_equal( score$ErrCorr      , ErrCorr      ) }  )
      test_that( "lat"    , { expect_true (all(is.na(c(lat,score$lat))) || abs(score$lat-lat)< maxlatdiff  ) }  )
   })
}
