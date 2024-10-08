#!/bin/bash
set -x

path=/Volumes/Phillips/COG
pathScripts=$path/autoeyescore/dani
eyescoreFunctions=$pathScripts/eyescoreFunctions.R
eyescoreScript=$pathScripts/eyescoreScript.R
logFile=$path/.log

if [ ! -e $path/maxJobs ]; then exit 1; fi # need maxJobs for multicore

cd $path
tasks="MGSEncode AntiState"
for task in $tasks; do
  ids=$( ls $path/$task )
  for id in $ids; do
    dates=$( ls $path/$task/$id )
    for date in $dates; do
      pathSession=$path/$task/$id/$date
      #echo $task $id $date
      Rscript --vanilla --quiet $eyescoreScript \
        path=\"$pathSession\" taskPath=\"$path\" task=\"$task\" \
        id=$id date=$date eyescoreFunctions=\"$eyescoreFunctions\" \
        >> $logFile 2>&1 &
      set +x # set +x for suppressing clutter
      maxJobs=$( cat $path/maxJobs )
      while [ $( jobs | wc -l ) -ge $maxJobs ]; do sleep 1; done
      set -x
    done
  done
done

# totals
  # MGSEncode: 456 sessions, 1343 runs
  # AntiState: 464 sessions, 1826 runs
# errors (across both tasks):
  # numbers of columns of arguments do not match (session level) -- 74
  # number of XXX trials detected from xdats (XXX) do not match expectedTrialCount (XXX) -- 102
  # missing value where TRUE/FALSE needed (xposCenterFix) -- 59
  # mean of actual eye fixation across run (XXX) differs from expected eye fixation (XXX) by >XX -- 49
  # Error in ts(x) : 'ts' object must have one or more observations -- 11

# quick loop to hide unused timing files
for task in $tasks; do
  case $task in
    "MGSEncode") hideFiles=$( echo "capped" $( for cond in vgs delay mgs; do
      for out in correct incorrect corrected dropped; do echo ${cond}_${out}
      done; done ) ) ;;
    "AntiState") hideFiles="capped" ;;
  esac
  for hideFile in $hideFiles; do 
    set +x # turning off output for this section because too much clutter
    for file in $( ls $path/$task/*/*/timings/*/$hideFile ); do
      mv $file $( dirname $file )/.$( basename $file )
    done
    set -x
  done
done

# behavior spreadsheets
header="id date type count correct incorrect corrected dropped droppedReason \
percCorrect latency accuracy accuracyMost"
for task in $tasks; do
  case $task in
    "MGSEncode") types="vgs mgs" ;;
    "AntiState") types="as ps" ;;
  esac
  for type in $types; do
    echo $header > $path/${task}_${type}.txt
    set +x # too much output clutter in this section
    for file in $( ls $path/$task/*/*/*stats.txt ); do
      id=$( echo $file | cut -d/ -f6 )
      date=$( echo $file | cut -d/ -f7 )
      echo $id $date $( grep "$type " $file ) >> $path/${task}_${type}.txt
    done
    set -x #
  done
done

# fmri spreadsheets
  # cross check fmri and behavior, create timing files for glms
ids=$( basename $( ls -d $path/1* ))
mkdir -p $path/GLM
for task in $tasks; do
  mkdir -p $path/GLM/$task
  case $task in
    "MGSEncode") runs=3; runID="MS" ;;
    "AntiState") runs=4; runID="AS" ;;
  esac
  runString=$( for run in $( seq $runs ); do echo run${run}; done )
  runStringBeh=$( for run in $( seq $runs ); do echo beh${run}; done )
  header="id date $runString $runStringBeh"
  echo $header > $path/${task}_fmri.txt
  for id in $ids; do
    dates=$( ls $path/$id )
    for date in $dates; do
      set +x # declutter
      sessLogic=""
      sessLogicBeh=""
      for run in $( seq $runs ); do
        runLogic=$( ls $path/$id/$date | grep ${runID}${run} | wc -w )
        sessLogic=$( echo $sessLogic $runLogic )
        runLogicBeh=$( ls $path/$task/$id/$date/timings | grep run${run} | wc -w )
        sessLogicBeh=$( echo $sessLogicBeh $runLogicBeh )
        if [ $runLogic == 1 ] && [ $runLogicBeh == 1 ]; then
          mkdir -p $path/GLM/$task/$id/$date/timings
          for file in $( ls $path/$task/$id/$date/timings/*run${run}* ); do
            cat $path/$task/$id/$date/timings/*run${run}*/$file >> $path/GLM/$task/$id/$date/timings/$file
          done
        fi
      done
      set -x
      echo "$id $date $sessLogic $sessLogicBeh" >> $path/${task}_fmri.txt
    done
  done
done

# 3dDeconvolve - regular and beta series
glmScript=$pathScripts/eyescoreGlms.sh # needs path, task, id, date, model (glm, beta_series), mask
models="glm beta_series"
writeScript="TRUE"; runGlm="TRUE"; runReml="FALSE" # defaults
mask=$HOME/standard/mni_icbm152_nlin_asym_09c/mni_icbm152_t1_tal_nlin_asym_09c_brain_3mm.nii
for task in $tasks; do
  for model in $models; do
    ids=$( ls $path/GLM/$task )
    for id in $ids; do
      dates=$( ls $path/GLM/$task/$id )
      for date in $dates; do
        pathModel=$path/GLM/$task/$id/$date/$model
        mkdir -p $pathModel
        cd $pathModel
        if [ -e "glm_out+tlrc.HEAD" ]; then continue; fi
        if [ $writeScript == "TRUE" ]; then sh $glmScript $path $task $id $date $model $mask > .writeScript.log 2>&1; fi
        if [ $runGlm == "TRUE" ] && [ -e "glm_out.cmd" ]; then sh glm_out.cmd > .runGlm.log 2>&1 & fi ## runs glm
        if [ $runReml == "TRUE" ] && [ -e "glm_out.REML_cmd" ]; then sh glm_out.REML_cmd > .runReml.log 2>&1 & fi ## runs reml glm
        set +x; maxJobs=$( cat $path/maxJobs ); while [ $( jobs | wc -l ) -ge $maxJobs ]; do sleep 1; done; set -x # set +x for suppressing clutter
      done
    done
  done
done

## STILL TO DO
  ## BEHAVIOR GROUP RESULTS
  ## FMRI GROUP MAPS/CONJUNCTIONS - ALL AND LINEAR AGE (NO LMER YET, TOO LONG)
    ## ANTI: PScorr, AScorr, ASinc_cor
    ## MGS: CUE, DELAY, TARGET
    ## BOTH: PS/VGScorr, AScorr, CUE/DELAYcorr
  ## ROI GROUP RESULTS
  ## DTI TBSS FOR SUBJS
  ## 

###################
###### NOTES ######
###################

## NOTE: HAVE NOT ACTUALLY MADE BELOW CHANGES

# AntiState
  # 10133/20081217 - 4th run redone, hiding incomplete file
    # file=10133_20081217_AntiState_run4_07AV_NOTCOMPLETED_raw.txt; mv $file .${file}; echo "4th run redone, hid incomplete file" > note
  # 10133/20090114 - empty
  # 10138/20070901 - script not given in file names
  # 10152/20090730 - missing run 2
  # 10183/20051210 - missing run 4
  # 10184/20100925 - missing run 4
  # 10184/20101120 - has 5 runs, same code for runs 3 and 5
    # temporary assumption that 3 was incomplete, will confirm
    # a="10184_20101120_AntiState_run"; b="_08AV_raw.txt"; mv ${a}3${b} .${a}3${b}; mv ${a}4${b} ${a}3${b}; mv ${a}5${b} ${a}4${b}; echo "3rd and 5th run use same script, assuming 3 incomplete, hiding 3 and renaming 5 as 4 and 4 as 3" > note
  # 10186/20090108 - 2nd run error, redid script in 5th run, renaming
    # file1=10186_20090108_AntiState_run2_05VAerror_raw.txt; file2=10186_20090108_AntiState_run5_05AVcorrect_raw.txt; mv $file1 .${file1}; mv $file2 $file1; echo "2nd run error, redid as 5th run, renaming as 2 (but will need to account for fmri order differences later" > note
  # 10202/20090106 - missing run 4
  # 10218/20060220 - missing run 4
  # 10218/20080628 - missing run 4
  # 10359/20131001 - script not given in file names
  # 10361/20061111 - run1 and run2 have copies with different scripts! uh oh. need to investigate further.
  # 10451/20100111 - missing runs 2-4
  # 10463/20070927 - missing run 4
  # 10463/20071024 - empty
  # 10476/20071210 - missing run 4
  # 10479/20071110 - script not given in file names
  # 10479/20091031 - empty
  # 10479/20091109 - missing runs 3-4
  # 10480/20091031 - empty
  # 10488/20071109 - missing run 4
  # 10542/20080331 - script not given in file names
  # 11052/20130924 - missing run 2
  # 11090/20130116 - missing run 4

# MGSEncode
  # 10133/20061102 - missing run 3
  # 10160/20080328 - missing run 3
  # 10173/20051128 - missing run 3
  # 10177/20051117 - missing runs 2-3
  # 10183/20090214 - missing run 3
  # 10184/20101120 - missing run 3
  # 10186/20051201 - missing runs 2-3
  # 10216/20070331 - missing runs 2-3
  # 10218/20070630 - missing runs 2-3
  # 10232/20060327 - missing runs 2-3
  # 10252/20060504 - missing run 3
  # 10288/20080813 - missing run 3
  # 10329/20090625 - missing runs 2-3
  # 10365/20101218 - missing runs 2-3
  # 10370/20061104 - missing run 3
  # 10370/20071117 - has a run6 in folder, hiding
    # file=10370_20071117_MGSEncode_run6_raw.txt; mv $file .${file}; echo "for some reason, there is a run 6, hiding" > note
  # 10470/20121010 - missing run 3
  # 10477/20080215 - has a run5 and run7 in folder, hiding
    # files=$( echo 10477_20080215_MGSEncode_run{5,7}_raw.txt ); for file in $files; do mv $file .${file}; done; echo "for some reason, there is a run 5 and 7, hiding" > note
  # 10479/20071110 - has a run5 in folder, hiding
    # file=10479_20071110_MGSEncode_run5_raw.txt; mv $file .${file}; echo "for some reason, there is a run 5, hiding" > note
  # 10480/20071026 - missing run 3
  # 10492/20071126 - missing run 3
  # 10822/20110803 - missing run 3
  # 11183/20131114 - empty
