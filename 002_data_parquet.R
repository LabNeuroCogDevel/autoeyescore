#!/usr/bin/env Rscript
# 20200503 - read all input files and save to as parquet output
library(data.table)
library(arrow)
Fs <- 60 # sampleing rate
readit <- function(f, trunc_end=TRUE) {
    # datatable quick read of parsed eyd file
    # if trunc_end remove all repeating XDATS at end
    dt <- fread(f)
    if(ncol(dt) < 4) return(NULL)
    # remove all leading zeros
    startzeros_end <- Position(function(a) a != 0, d$XDAT)-1
    if(is.finite(startzeros_end)) dt <- dt[-c(1:startzeros_end), ]

    # for storage, remove all but the last 10 (?) of last code
    # this could be a bad idea if last code is still part of a task
    r <- rev(dt$XDAT)
    i <- Position(function(a) a!=r[1], r) - 10
    if(is.finite(i) && i>0 && trunc_end) dt <- dt[ -c(nrow(dt) - i:0),]

    # dont care if nothing recorded
    if(ncol(dt) < 4) return(NULL)
    # file name should end like '10022.20031229.1'
    dt[, c("id","date","run") :=
             lapply(tstrsplit(stringr::str_extract(f, "\\d{5}.\\d{8}.\\d+"), "\\."), as.numeric)]
    dt[, t := round(1:nrow(dt)/Fs,3)]
    return(dt)
}

cat("start glob ", Sys.time(), "\n")
root <- '/Volumes/L/bea_res/Data/Tasks/Anti/'
g <- Sys.glob(sprintf('%s/Basic/1*/2*/Raw/EyeData/txt/*.tsv', root))
cat("start read ", length(g), " files ", Sys.time(), "\n")
d <- rbindlist(lapply(g,readit), fill=TRUE)
cat("write out ", nrow(d), " rows ", Sys.time(), "\n")
write_parquet(d, "anti.parquet")
cat("done", Sys.time(), "\n")

