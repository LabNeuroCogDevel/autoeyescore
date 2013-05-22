 for scored in /mnt/B/bea_res/Data/Tasks/BarsScan/Basic/*/*/Scored; do for i in {1..4}; do f=$scored/Run0$i/fs_*xls; [ ! -r $f ] && echo "$f"; done; done > findmissing_output.txt
