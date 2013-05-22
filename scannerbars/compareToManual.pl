#!/usr/bin/env perl

use strict;
use warnings;

use Data::Dumper;
use File::Find;
use File::Basename;
#use File::Find::Rule;
use Spreadsheet::ParseExcel;
#use Getopt::Std;
#use v5.14;

######
# 
# find all fs*xls files and check %correct against auto
#
#####

open my $OUTFH, '>checkBars_trial.csv' or die 'cannot open output\n';
my $trialsperrun = 42;
my $basedir ='/mnt/B/bea_res/Data/Tasks/BarsScan/Basic/';


my $p = Spreadsheet::ParseExcel->new();

# print header
my $header=join("\t",qw/trial count_a lat_a count_m lat_m scorer/)."\n";
print $header;
print $OUTFH $header;

my @scoreSheets;
sub wanted {push @scoreSheets, $File::Find::name if  m:fs.*xls:i};
find( {wanted => \&wanted,no_chdir=>1,follow=>1}, $basedir );
#@scoreSheets=qw(/mnt/B/bea_res/Data/Tasks/BarsBehavioral/Basic/10156/20110810/Scored/Run02/fs_10156_bars2.xls
#                );
#                #/mnt/B/bea_res/Data/Tasks/BarsBehavioral/Basic/10847/20100920/Scored/Run01/fs_10847_bars1.xls

for my $xlsfn (@scoreSheets){
  ################################
  # manual scoring parse
  ################################
  next if $xlsfn =~ /^$/;
  my $run=0;
  $run = $1 if $xlsfn =~ m:Run0?(\d):;
  

  my $xlsp = $p->parse($xlsfn);
  if(!$xlsp ) { print STDERR "$xlsfn is empty?\n"; next }
  my @worksheets = $xlsp->worksheets();
  if($#worksheets <=0 ) { print STDERR "$xlsfn is empty?\n"; next }
  my $xls    = $worksheets[0];

  my $scorer = $xls->get_cell(1,0)->unformatted();
  $scorer =~ s/scorer:? ?//i;
  $scorer = uc($scorer);
  $scorer =~ s/[^A-Z]//gi;
  $scorer="JP" if $scorer =~ /justin/i;
  $scorer="unknown" if $scorer =~ /^$/;


  my ($rowstart,$rowend) = $xls->row_range();
  my @trialsacs; # array of trials with an array of hashes lat dlat dacc
  my @manualT;   # array of trials with hash for countToCorrect and fist lat
  for my $row (5..$rowend){

   my $tmp = $xls->get_cell( $row, 2 );
   my $trial = $tmp? $tmp->value(): 0;
   last if $trial =~ /^$|-1/;

   $tmp = $xls->get_cell( $row, 6 );
   my $lat   = $tmp? $tmp->value(): -1;

   $tmp = $xls->get_cell( $row, 7 );
   my $dlat  = $tmp? $tmp->value(): -1;

   $tmp = $xls->get_cell( $row, 9 );
   my $dacc  = $tmp? $tmp->value(): -1;
   #print "$row\t$trial\t$lat\t$dlat\t$dacc\n";
   push @{$trialsacs[$trial]}, {dlat=>$dlat,lat=>$lat,dacc=>$dacc} if $dlat > 0;
  }

  for my $trial (1..$#trialsacs){
   my @dlats=map {$_->{dlat}} @{$trialsacs[$trial]};
   my $countToCorrect;
   if   ( scalar(grep(/4/,@dlats) ) > 0 ){$countToCorrect= 2 }
   elsif( scalar(grep(/2/,@dlats) ) > 0 ){$countToCorrect= 0 } 
   elsif( scalar(grep(/1/,@dlats) ) > 0 ){$countToCorrect= 1 }
   else                                  {$countToCorrect=-1 }

   #print "$trial: ";#, join(" ", map {$_->{dlat}} @{$trialsacs[$trial]}), "\n";
   #print $countToCorrect, "\n";
   my $lat=-1;
   $lat = @{$trialsacs[$trial]}[0]->{lat} if(@{$trialsacs[$trial]}>0);
   $manualT[$trial]={count=>$countToCorrect ,lat=>$lat }
  }



  ################################
  # get auto scoring
  ################################
  my $autofnpath = dirname(dirname($xlsfn)) ."/txt/*$run.trial.txt";
  my @autofn = glob( $autofnpath );

  # initialze
  # if no score sheet can be found, first column will start with *
  # this happens because eyds aren't where they are expected to be
  $xlsfn =~ m:/(\d{5})/(\d{8})/:;
  my $runname= "*$1.$2.$run";


  my @autoT; # store counttocorrect and lat per [T]rial
  # if there is a score sheet
  if($#autofn>=0){
     $runname=basename($autofn[0],'.trial.txt');
     #print $autofn[0],"\n";
     open my $autoFH, $autofn[0];
     while(<$autoFH>) {
       next if $.==1;
       my ($trial,$xdat,$lat,$fstCorrect,$ErrCorr,$AS,$Count)=split/\W+/;
       $autoT[$trial] = {count=>$Count ,lat=>$lat }
     }
     #print STDERR "$trial has ", $.-1, "entries\n" if $. != 61;
     close $autoFH;
  }
 
  for my $t (1..$trialsperrun){
   # account for drop trials 
   if(!$autoT[$t])   { $autoT[$t]   = { count => -1, lat=>-1}; }
   if(!$manualT[$t]) { $manualT[$t] = { count => -1, lat=>-1}; }
   # only print if count is of or lat is more than 10ms off
   #if(
   #   $autoT[$t]->{count} != $manualT[$t]->{count} ||
   #   abs($autoT[$t]->{lat} - $manualT[$t]->{lat}) > 30 
   #) {
     my $output=join("\t","$runname.$t", @{$autoT[$t]}{qw/count lat/},@{$manualT[$t]}{qw/count lat/},$scorer)."\n";
     print $OUTFH $output;
     print $output;
   #}
  }
}
close $OUTFH;

