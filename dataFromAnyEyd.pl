#!/usr/bin/env perl

########
# pull data from any eyd file (v 602)
#
# e.g. check trials:
#
# ./dataFromAnyEyd.pl ~/remotes/B/bea_res/Data/Tasks/Anti/Basic/11082/20121127/Raw/EyeData/11082_anti.eyd |cut -f 1|uniq -c| perl -ne 'chomp; print $_ if /\d+ \d+/; print "\n" if /250$/'|cat -n
#
# prints:
# trial#   #samples XDAT    #samples XDAT     #samples 250

use strict; use warnings; 
#use lib '/home/foranw/src/eyds/EyeTracking-EYD/lib/';
use EyeTracking::EYD;
use File::Basename;
use Number::Range;
#use Data::Dumper;


# what are the xdat code ranges
my $startCodes  = Number::Range->new("50,100,200");
my $targetCodes = Number::Range->new("110..160,201..210");
my $stopCodes   = Number::Range->new("250");

# how many different orders do we have
my $orderRange  = Number::Range->new("1..4");

#my $file="$filePrefix/10128/20080925/Raw/EyeData/10128_run1.eyd";
#my @files = glob("$BEARES/Data/Tasks/CogEmoSoundsBasic/*/*/Raw/E*/*eyd");
my @files=@ARGV;

die "give me somethign to work with!" if $#ARGV<0;

## Get a list of all the files
for my $file (@files) {
  
  print "#ERROR: $file: file DNE\n" and next if ! -e $file;
 
  # get the order file, or die
  # usually #.eyd
  # but occasionally: 10585_scannerbars1_correct.eyd

  print "** $file\n" if $#ARGV>0;
  # read in eyd file
  #my $eyd = EyeTracking::EYD->new($file,$id,"test/$id.err");
  my $eyd = EyeTracking::EYD->new($file,'',".err");
  $eyd->{printerrors}=1;
  $eyd->{verbose}=0;
  $eyd->read();
  $eyd->writeerror("ERROR: no data found (empty file?)") and next if ! $eyd->{data};
  $eyd->writeerror("ERROR: insufficant data ($#{$eyd->{data}} rows)") and next if $#{$eyd->{data}} < 1000;
  
  # save raw data (just xdat, puple, and gaze)
  #$eyd->printEyeData("test/$id.data.tsv");
  $eyd->printEyeData();
  
  # set up codes
  #$eyd->trialPositions($startCodes,$targetCodes,$stopCodes );
  #$eyd->writeerror("ERROR: no codes found!") and next if ! $eyd->{codes};
  
  # get expected
  #$eyd->barsEprimeTrialOrder($order);

  # compare to expected
  #my $score = $eyd->checkAlignment();
  #print "\talignment score: $score\n";

  undef $eyd;
}
