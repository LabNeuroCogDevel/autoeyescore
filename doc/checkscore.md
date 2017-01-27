# Check Scoring

{{out.gif}}
see this [bookmarklet](http://slbkbs.org/jsgif/) to add controls to the gif
## Overview

One might have two reasons to check the automated scoring

*  **Single Trial:** The scoring is suspect on a participant (because the results are strange, or the participant/tracking showed unusual behavior) 

*  **All:** We want to explore general automatic scoring deficits

In either case, the scripts and environment are in a [git repository](https///github.com/LabNeuroCogDevel/autoeyescore/tree/master/viewdiffs) cloned on ''lncd@arnold:~/src/autoeyescore'' 

For windows users this means:
 1.  connecting to arnold with mobaXterm and 
 2.  entering the directory: ''cd  src/autoeyescore''

Once in the scripts directory (''src/autoeyescore''), launch R and source the ''type/type.settings.R'' file you want to use and ''getSacDot.R''.  ''getSacDot'' and ''getRunDot'' are now available and will run within the chosen task. 
 1.  type ''/sw/bin/R'' to launch the R environment ((''fink'' installed R not the manually intalled version ''/Library/Frameworks/R.framework/Resources/bin/R''))
 2.  ''source('getSacsDot.R')'' to load functions to be used with the Anti task

### Outline of Commands

	:::bash
	ssh lncd@arnold.wpic.upmc.edu  # on windows, this is double clicking Arnold from the server left side bar
	cd ~/src/autoeyescore          # script directory
	/sw/bin/R                      # launch R environment for scripts -- fink version not installed version
	
	source('anti/anti.settings.R') # load xdats, file locations, and other settings
	source('getSacsDot.R')         # put useful functions in environment ( getSacDot(),getRunDot() )
	# look at the whole run (with annotate drop reasons)
	getRunDot('10193.20091121.1',showplot=F)  # show scoring for whole run, showplot=T for graph per trial
	# inspect one trial, show a graph
	getSacDot('10193.20091121.1.25')  # sac graph scoring for subj.date.run.trial  


## Single Trial

After sourcing the correct ''check*R'' file as illustrated above, ''getSacDot'' and ''scoreSac'' can be used to check the scoring of a single trial. 

 * ''getSacDot'' will look at the eye tracking at a specific trial and pull out saccades 
 * ''scoreSac''  will score those extracted saccades
These can be run at the same time or idividually

	
	scoreSac(getSacDot('11167.20130517.1.1'))
	# is the same as
	sacs <- getSacDot('11167.20130517.1.1')
	scoreSac(sacs)

### The Graph

The graph produced has the following indicators

{{ :howto:eyedata:saccade.png?300 }}

#### Rectangles

rectangles surround movements registered as a saccade ( meet acceleration threshold)
##### Opacity

 | solid         | meets all requirements            |
 | ---------------------------------------------------
 | translucent   | does not meet some requirement    |
##### Color

 | blue   | correct movement               |
 | -----------------------------------------
 | green  | correct and accurate movement  |
 | red    | incorrect movement             |
 | orange | incorrect and exactly opposite |

#### Lines

##### Horizontal eye position graph
 | horiz. pink | between pink lines is considered accurate |
 | ---------------------------------------------------------
 | horiz. green| center of screen (fixation pt) |           
 | horiz. lime | actual fixation before target onset |      
 | vert. solid red | presaccade if before line |            
 | vert. dot red | end of trial, before stop code |         
##### Acceleration graph

 | vert. blue   | acceleration above sac threshold |                                                        
 | -------------------------------------------------                                                        
 | vert. yellow | acceleration above movement start ((sacades start here if they go over other threshold)) |
## All

Because we want to look for systematic errors it makes sense to view many trials in sequence. We can pick what kind of errors to explore by finding trials where automatic and manual scores disagree. ''showdiffs'' allows this by classifying trail scores loosely as minimum number of saccades until a correct movement is made.

### Scored Categories

 | score | event           | 
 | ----- | -----           | 
 | -1    | drop            | 
 | 0     | incorrect       | 
 | 1     | correct         | 
 | 2     | corrected error | 
### Running

If we want to view **10** instances where the scoring disagreement is **automatic says drop** and **manual says correct** we would run

	
	showdiffs(-1,1,10) 

# Add tests

 1.  add to ''TASK/test/trials.txt''
 2.  check test: ''./runme.sh -T TASK''
 3.  push to git
