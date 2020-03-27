#!/usr/bin/env Rscript
# run at ../.. with test_check('.')
require(testthat)
context("dot notation")
source('ScoreRun.R')
source('getSacsDot.R')

test_that("getfiles",{
    source('AntiPet/AntiPet.settings.R')
    i <- dot2runinfo('11677.20200110.1.1')
    expect_true(file.exists(i$eyetrack))
    expect_equivalent(i$parts, c(11677,20200110,1,1) )
})

test_that("scoreSacI",{
    source('AntiPet/AntiPet.settings.R')
    i <- dot2runinfo('11677.20200110.1.1')
    sacs <- getSacI(i, onlyontrials=1:expectedTrialLengths)
    expect_equal(unique(sacs$trial), 1:expectedTrialLengths )
})

test_that("pdf",{
    source('AntiPet/AntiPet.settings.R')
    pdf <- paste0(tempfile(),'.rmme.pdf')
    unlink(pdf)
    on.exit(unlink(pdf),add=TRUE)
    mkpdf('11677.20200110.1.1', savedas=pdf)
    expect_true(file.exists(pdf))
    expect_true(file.info(pdf)$size > 10)
})
