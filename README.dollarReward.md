To score, in a terminal window, run the following:

    cd ~/autoeyescore/
    ./scoreDollarReward.bash DollarReward/eyd/1940reward.eyd

it does not matter where the eyd file is, however if they are in ``DollarReward/eyd/``, an abbreviated command can be run

   ./scoreDollarReward.bash 1940

In either case, the output is saved to ``DollarReward/txt``, where each run has 4 files.
  
    60samples/sec raw data: 1940.data.csv
    saccades found:         1940.sac.txt
    scored trial:           1940.trial.txt
    run summary:            1940.summary.txt

trials are codes are given below. These can be thought number of saccades until a correct movement

     -1  drop
     0   error
     1   correct
     2   error correct

To inspect (show a graph) for the saccades in a trial, run ``R``. Inside ``R``, load (via source) the function to inspect trials (``trialSac``), and look at the subject+trial of interest.

    R
    #----------------

    source('DollarReward/trialSacDollarReward.R')

    # inspect trial 32 of subj 1940
    trialSac(1940, 32)

    # to see how this trial was scored
    # Notice: showplot=F means no plot will be given
    scoreSac( trialSac(1940,32,showplot=F) )

    # to see all trials
    trialSac(1940,'*')
    
    # to see score of all trials
    scoreSac( trialSac(1940,'*') )

Note:

Dollar Reward is a special task unlike the others here (as it does not have a ``*.settings.R`` file, it will not be see by ``runme.bash``)

