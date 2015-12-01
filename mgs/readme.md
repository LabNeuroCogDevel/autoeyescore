## extract log for everyone (need to know random order of sequences)

`00_eyd.bash` uses `parseEP1es.pl`

## score everyone
using `willEye.R`
```
R
 source('willEye.R')
 setMGSGlobals() # sets "settings", called when file is sourced
 SCORE_ALL()     # scores all files that it can find and saves scored sac to willout/
```
