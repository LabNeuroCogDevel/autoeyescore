#!/usr/bin/env perl

use strict;
use warnings;

# ./compareToManual.pl subj*xls subj*trial.txt

use Data::Dumper;
use File::Find;
use File::Basename;
use File::Path;
#use File::Find::Rule;
use Spreadsheet::ParseExcel;
#use Getopt::Std;
#use v5.14;




my $trialsperrun = 28;

my $p = Spreadsheet::ParseExcel->new();

# print header

my @scoreSheets;
push @scoreSheets,$ARGV[0]; 
for my $xlsfn (@scoreSheets){
  ################################
  # manual scoring parse
  ################################
  next if $xlsfn =~ /^$/;
  next if $xlsfn =~ /bak/;
  next if  $xlsfn =~ /fs_10165_anti.xls/; # hung
  print "looking at: $xlsfn\n" if $ENV{DEBUG};
  my ($subj,$rundate,$run,$scorer)=(0)x4;
  $subj    = $1 if $xlsfn =~ m:/(\d{5})/:;
  $rundate = $1 if $xlsfn =~ m:/(\d{8})/:;

  my $hasrundir = $xlsfn =~ m:/Run:;
  if($hasrundir) {
   $run     = $1 if $xlsfn =~ m:Run0?(\d):;
  }else{
   #$run     = $1 if $xlsfn =~ m:(\d).xls.*:;
   $run=1;
  }

  my $basename = dirname($xlsfn);
  $basename = dirname($basename) if $hasrundir;

  # for anti this is in subj/date dir b/c there is no rundir
  # for bars and scannerbars this is in subj/date/run#/
  
  my @trialsacs; # array of trials with an array of hashes lat dlat dacc
  my @manualT;   # array of trials with hash for countToCorrect and fist lat

  my $xlsp = $p->parse($xlsfn);
  if(!$xlsp ) { print STDERR "$xlsfn is empty?\n"; next }
  my @worksheets = $xlsp->worksheets();
  if($#worksheets <=0 ) { print STDERR "$xlsfn is empty?\n"; next }
  my $xls    = $worksheets[0];

  $scorer = $xls->get_cell(1,0)->unformatted();
  $scorer =~ s/scorer:? ?//i;
  $scorer = uc($scorer);
  $scorer =~ s/[^A-Z]//gi;
  $scorer="JP" if $scorer =~ /justin/i;
  $scorer="unknown" if $scorer =~ /^$/;


  my ($rowstart,$rowend) = $xls->row_range();
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

  # initialze
  # if no score sheet can be found, first column will start with *
  # this happens because eyds aren't where they are expected to be
  $xlsfn =~ m:/(\d{5})/(\d{8})/:;
  my $runname= "*$1.$2.$run";


  my @autoT; # store counttocorrect and lat per [T]rial
  # if there is a score sheet
  $runname=$ARGV[1];
  #print $autofn[0],"\n";
  open my $autoFH, $ARGV[1];
  while(<$autoFH>) {
    next if $.==1;
    chomp;
    my ($trial,$xdat,$lat,$fstCorrect,$ErrCorr,$AS,$Count,$desc)=split/\t/;
    $autoT[$trial] = {count=>$Count ,lat=>$lat,xdat=>$xdat, desc=>$desc };
    print "line:${_}parsed:$trial $xdat $lat $fstCorrect $ErrCorr $AS $Count $desc\n" if $ENV{DEBUG};
  }
  #print STDERR "$trial has ", $.-1, "entries\n" if $. != 61;
  close $autoFH;
 
   my $header=join("\t",qw/trial count_a lat_a count_m lat_m scorer xdat reason/)."\n";
   print $header;
  for my $t (1..$trialsperrun){
   # account for drop trials 
   if(!$autoT[$t])   { $autoT[$t]   = { count => -1, lat=>-1}; }
   if(!$manualT[$t]) { $manualT[$t] = { count => -1, lat=>-1}; }
   # only print if count is of or lat is more than 10ms off
   #if(
   #   $autoT[$t]->{count} != $manualT[$t]->{count} ||
   #   abs($autoT[$t]->{lat} - $manualT[$t]->{lat}) > 30 
   #) {
     my $output=join("\t",$t, @{$autoT[$t]}{qw/count lat/},@{$manualT[$t]}{qw/count lat/},$scorer,@{$autoT[$t]}{qw/xdat desc/})."\n";
     print $output;
     #print $output;
   #}
  }
}

