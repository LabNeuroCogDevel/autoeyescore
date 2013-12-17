#!/usr/bin/env perl

use strict;
use warnings;
#use Getopt::Std;
#use v5.14;

######
# 
# Make the txt log file into something R can handle
#
#####
#my @keys = qw/trial location masterlist.Cycle Running Correct Procedure Level iXEtePos masterlist.Sample Latency masterlist Score/;
my @keys = qw/trial location Correct Procedure iXEtePos masterlist.Sample Latency masterlist Score/;
if( !$ARGV[0] ) { die "useage $0 EPlogfile.txt\n";}
my $logfile = $ARGV[0];
#my $logfile = ($ARGV[0] || "/mnt/B/bea_res/Data/Tasks/BarsBehavioral/Basic/10872/20131129/Raw/EPrime/Behavioral Value Bars with scoring - v. 1-10872-1.txt");
#my $logfile =  "/mnt/B/bea_res/Data/Tasks/BarsBehavioral/Basic/10872/20131129/Raw/EPrime/Behavioral Value Bars with scoring - v. 1-10872-1.txt";
my $logfh;
open $logfh, $logfile or die "cannot open $logfile";
my %rec;
$rec{$_}="" for @keys;
my $s=0; # should we be pulling values (started?)

my $t=0; # what trial are we on

#header
print join("\t", @keys),"\n";
while(<$logfh>){
 chomp while chomp; 

 # done logging if at the end of the frame
 if(m/LogFrame End/){

  # only print and increment if we are in master list and is a reward type (not fix)
  if($rec{'Procedure'} !~ m/fix|done/i && $rec{'Running'} eq 'masterlist'){
    $rec{'trial'}=++$t;
    print join("\t",@rec{@keys}), "\n" 
   }
   # clear out the hash
   $rec{$_}="" for @keys;
 }

 # key:value into perl hash
 if(m/\W*(.*):\W*([a-zA-Z0-9]+)/ and $s){
   $rec{$1}="$2"; 
   #print "\t\t$1 -- $2\n"
  }

 # should start logging on next line
 $s=1 if m/LogFrame Start/;
}


