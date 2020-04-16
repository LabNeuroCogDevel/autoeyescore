#!/usr/bin/env Rscript

# 20200323WF - init
#  developed for AntiPet eyetracking quality checking
setwd("/Volumes/Hera/Projects/autoeyescore/")


o <- docopt::docopt(
"eye quality check

Usage:
  eyeQC.R tasks
  eyeQC.R <task> --pdf ( redo | todo | done | <dotnotation> )
  eyeQC.R <task> <intials> ( todo | done | reverse | random | <dotnotation> ) [ --droponly ]
  eyeQC.R (-h | --help)

Options:
  -h --help     Show this screen.
  -d            luna.date.trial dot notation.
")

# just list avaiable tasks
if (o$tasks) {
  cat(paste(collapse="\n", sapply(Sys.glob("*/*.settings.R"), dirname)), "\n")
  quit()
}

src <- sprintf("%s/%s.settings.R", o$task, o$task)
if (!file.exists(src))
   stop("no settings file for task ", o$task, " like '", src, "'. see eyeQC.R tasks")
source(src)
source("ScoreRun.R")
source("getSacsDot.R")


outdir <- file.path('audit', o$task)
# all files
if(!is.null(o$dotnotation)) {
    di <- dot2runinfo(o$dotnotation)
    allinfo <- getFiles(di$eyetrack)
} else {
    cat("# generating list of all raw eye files\n")
    allinfo <- getFiles()
}

# pull out parts we care about
allfiles <- allinfo$file
dots <-  allinfo$id

cat("# generating list of all completed files\n")
if(o$pdf) {
    have <- lapply(Sys.glob(paste0(outdir,"/pdf/*/")), basename)
} else if(is.null(o$dotnotation)) {
    have <- lapply(Sys.glob(paste0(outdir,"/*_", o$intials, ".txt")),
                   function(f) gsub("_*", "", basename(f)))
} else {
    have <- list()
}
need <- setdiff(dots, have)
# just show what we have todo and quit
if (o$todo) {
   cat(paste(collapse="\n", need), "\n")
   quit()
}
if (o$done) {
   cat(paste(collapse="\n", have), "\n")
   quit()
}

# how to organize
if (o$reverse) need <- rev(need)
if (o$random) need <- need[sample(1:length(need))]


for (dot in need) {
    if(o$pdf) {
        pdfdir <- file.path(outdir,'pdf', dot)
        print(sprintf('pdfrun(%s,%s)', dot,pdfdir))
        pdfrun(dot, pdfdir)
        next
    }
   getRunDot(dot, showplot=T, auditor=o$intials, pdfdir=pdfdir)
   cat("====== finished ", dot, "\n\n\n")
   readline("Want to stop? Push Ctrl-C.\nEnter to continue\n")
}
