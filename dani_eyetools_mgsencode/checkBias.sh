## looking for x-position bias in saccade data
  ## compare number of left/right accuracy errors
  ## average
path=/Volumes/Phillips/COG
tasks="MGSEncode AntiState"
for task in $tasks; do
  case $task in "MGSEncode") type="vgs" ;; "AntiState") type="ps" ;; esac
  outFile=$path/${task}_bias_check.txt
  echo "id date accAll accLeft accRight accAvg" > $outFile
  files=$( ls $path/$task/*/*/*saccades* )
  for file in $files; do
    id=$( echo $file | cut -d/ -f6 )
    date=$( echo $file | cut -d/ -f7 )
    acc=$( awk -v type=$type '$18 ~ type' $file | awk '$16 ~ 1' | awk '{print $7}' ) # gets accuracies for first saccade in all vgs trials
    accAll=$( echo $acc | wc -w )
    accLeft=$( echo $acc | grep -o "-" | wc -l )
    accRight=$(( accAll - accLeft ))
    accAvg=$( Rscript -e 'x<-commandArgs(trail=T); x<-x[-which(x=="NA")]; cat(round(median(as.numeric(x)),3))' $acc )
    echo $id $date $accAll $accLeft $accRight $accAvg >> $outFile
  done
done

