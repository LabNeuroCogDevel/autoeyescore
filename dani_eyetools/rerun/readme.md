# Using

```R
source('score_lunadaterun.R')
#eg
source('rerun/excludeError.R')
```
# Deriving

these files are the outputs of looking through errors reported by `/Volumes/Phillips/COG/MGSEncode/*/*/scorelog`

```bash
grep Error /Volumes/Phillips/COG/MGSEncode/*/*/scorelog|cut -f 2 -d: | sort | uniq -c
  24  Error in -excludeInd                                                            
  4   Error in `$<-.data.frame`(`*tmp*`, "type", value = c("vgs", "vgs", "vgs",  
  27  Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) 
  24  Error in model.frame.default(formula = xdatTime ~ matchingTrials, drop.unused.levels = TRUE) 
  67  Error in rbind(deparse.level, ...) 
  4   Error in read.table(file.path(path, file), head = T) 
  48  Error in scan(file, what, nmax, sep, dec, quote, skip, nlines, na.strings,  
  8   Error in ts(x)
```

```bash
grep -l 'Error in -excludeInd' /Volumes/Phillips/COG/MGSEncode/*/*/scorelog|
  cut -f1 -d:|
  cut -f 6,7 -d/|
  tr / , |
  while read p; do
    echo "statMGSE($p)";
  done |
  tee rerun/excludeError.R
```
