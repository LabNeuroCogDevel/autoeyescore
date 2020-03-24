#!/usr/bin/env Rscript

# 20200323WF - init
#  developed for AntiPet eyetracking quality checking
setwd("/Volumes/Hera/Projects/autoeyescore/")


o <- docopt::docopt(
"eye quality check

Usage:
  eyeQC.R tasks
  eyeQC.R <task> <auditor> [todo | done | --reverse | --random | <dotnotation> ]
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

# if we only want to do one
if (length(o$dotnotation)>0L) {
   getRunDot(o$dotnotation, showplot=T)
   quit()
}

# all files
allfiles <- Sys.glob(sprintf("%s/*/*/Raw/EyeData/txt/*.*.*.data.tsv",
                             filebasedir))
dots <- gsub(".data.tsv", "", basename(allfiles))
have <- lapply(Sys.glob(paste0("audit/AntiPet/*_", o$auditor, ".txt")),
               function(f) gsub("_*", "", basename(f)))
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
if (o$reverse) need <- rev(need)
if (o$random) need <- need[sample(1:length(need))]

for (dot in need) {
   getRunDot(dot, showplot=T, auditor=o$auditor)
   cat("====== finished ", dot, "\n\n\n")
   readline("Want to stop? Push Ctrl-C.\nEnter to continue\n")
}
