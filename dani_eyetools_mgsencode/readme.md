# Run
```
source('score_lunadaterun.R')
statMGSE(10129,20090825)
```

# Data
- Phillips/Cog
- bea_res

# Pipeline

see 
* `example_score.R` for a verbose outline
* `score_lunadaterun.R` and `rerun/readme.md` for more generalized


## Initial
this is a re-establishing pipeline from `arnold:~/src/autoeyescore/dani_eyetools`.
intial re-run done with `eyescoreScript_Simple.sh` looping over `eyescore_lunadate.bash` executing `eyescoreScript.R`

## Fixes
* some raw eye files need to be corrected with `correctRaw.bash`.
* errors were parsed and rerun (leaving old scorelog files behind), see `rerun/readme.md`



# Steps
## raw eye movements: binary to text
 `eydToRawTXT.sh` to find all eyd files on bea_res, parse to text and save in newly created dirs on Philips (DONE)
```bash
find /Volumes/Phillips/COG/MGSEncode -name createdFromB                 
 /Volumes/Phillips/COG/MGSEncode/10152/20150312/createdFromB
 /Volumes/Phillips/COG/MGSEncode/10173/20150527/createdFromB
 /Volumes/Phillips/COG/MGSEncode/10174/20150411/createdFromB
 /Volumes/Phillips/COG/MGSEncode/10184/20150512/createdFromB
 /Volumes/Phillips/COG/MGSEncode/10185/20150413/createdFromB
 /Volumes/Phillips/COG/MGSEncode/10192/20150511/createdFromB
 /Volumes/Phillips/COG/MGSEncode/10279/20150207/createdFromB
 /Volumes/Phillips/COG/MGSEncode/10311/20150120/createdFromB
 /Volumes/Phillips/COG/MGSEncode/10344/20150512/createdFromB
 /Volumes/Phillips/COG/MGSEncode/10359/20150511/createdFromB
 /Volumes/Phillips/COG/MGSEncode/10385/20150505/createdFromB
 /Volumes/Phillips/COG/MGSEncode/10816/20150212/createdFromB
 /Volumes/Phillips/COG/MGSEncode/10895/20150226/createdFromB
 /Volumes/Phillips/COG/MGSEncode/11111/20150511/createdFromB
 /Volumes/Phillips/COG/MGSEncode/11214/20150506/createdFromB
 /Volumes/Phillips/COG/MGSEncode/11216/20150513/createdFromB

```

## scoring eye movements
`eyescoreScript_Simple.sh` mod of `eyescoreScript.sh`

loops through `eyescore_lunadate.bash`, creates `scorelog`, `scorecmd` and `scoreerr` (if any errors)

see:
```
ls  /Volumes/Phillips/COG/MGSEncode/*/*/scoreerr|wc -l # 73 visits are not scored`

# why
ls  /Volumes/Phillips/COG/MGSEncode/*/*/scoreerr|sed s/err/log/|xargs head
```

## imaging
 `preprocessMprage` and `preprocessFunctional` -- part of cron scripts, not in this scope
