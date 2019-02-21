#!/usr/bin/env bash
#./00_eyd.bash 
Rscript <(cat <<EOF
 source('willEye.R')
 setMGSGlobals() # sets "settings", called when file is sourced
 SCORE_ALL(globpat="/Volumes/L/bea_res/Data/Tasks/MGS/Basic/*/*/",pathsvidx=c(9,10))
EOF
)
