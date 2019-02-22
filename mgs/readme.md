## extract log for everyone (need to know random order of sequences)

`00_eyd.bash` uses 
 - extract eyedata: `dataFromAnyEyd.pl` for asl model 6000 or `read_eyd5.m` for 5000
 - extract log:  `parseEP1es.pl` or `fakeLog.pl` if that fails

## score everyone
`runme.bash` scores mgs and fix events using  `willEye.R`
 -  `MGSscore.will(lunaid,date)`
    - `filesMGS(lunaid,date)` to find files
    -  `preprocessEyd(rawdata)`,`getSaccades(rawdata)`, `getGnrcRun(logfile)`  + `alignToExpected()`, `willtodani` + `scoreRun`
 - `score_mgs_fix` on the output of `MGSscore.will` (uses raw, saccades, and alignment)
