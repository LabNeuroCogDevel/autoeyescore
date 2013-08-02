#!/usr/bin/env perl

## NORMAL ULSAGE
# need to export
# export expectedTrialLengths=48
# export filebasedir='/mnt/B/bea_res/Data/Tasks/Anti/Basic/'
# compareToManual.pl 
#
## DEGUBING
# export DEBUG=1
# export expectedTrialLengths=48
# compareToManual.pl  /mnt/B/bea_res/Data/Tasks/Anti/Basic/10166/20050804/Scored/fs_10166_anti.xls

use strict;
use warnings;

use Data::Dumper;
use File::Find;
use File::Basename;
use File::Path;
#use File::Find::Rule;
use Spreadsheet::ParseExcel;
#use Getopt::Std;
#use v5.14;

######
# 
# find all fs*xls files and check %correct against auto
#
# write manual scoring to text file (from xls) if it doesn't already exist
#  as "/txt/$subj.$rundate.$run.manual.txt"
#
#####



open my $OUTFH, '>checkAgainstManual_trial.csv' or die 'cannot open output\n';
if(!$ENV{expectedTrialLengths}){
 print STDERR "need to know number of trials and basedirectory\n";
 print STDERR "\n\nexport expectedTrialLengths=42; export filebasedir=/mnt/B/bea_res/Data/Tasks/BarsScan/Basic/\n\n";
 exit 1;
}
my $trialsperrun = $ENV{expectedTrialLengths};
my $basedir =$ENV{filebasedir};

my $p = Spreadsheet::ParseExcel->new();

# print header
my $header=join("\t",qw/trial count_a lat_a count_m lat_m scorer xdat/)."\n";
#print $header;
print $OUTFH $header;

my @scoreSheets;
if(@ARGV) { 
 @scoreSheets=@ARGV; 
} else {
  sub wanted {push @scoreSheets, $File::Find::name if  m:fs.*xls:i};
  find( {wanted => \&wanted,no_chdir=>1,follow=>1}, $basedir );
  #@scoreSheets=qw(/mnt/B/bea_res/Data/Tasks/BarsBehavioral/Basic/10156/20110810/Scored/Run02/fs_10156_bars2.xls
  #                );
  #                #/mnt/B/bea_res/Data/Tasks/BarsBehavioral/Basic/10847/20100920/Scored/Run01/fs_10847_bars1.xls
}

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
  my $xlstxtfn = "$basename/txt/$subj.$rundate.$run.manual.txt";
  
  my @trialsacs; # array of trials with an array of hashes lat dlat dacc
  my @manualT;   # array of trials with hash for countToCorrect and fist lat

  if(! -e $xlstxtfn) {

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

     mkpath(dirname($xlstxtfn)) if( ! -d dirname($xlstxtfn) );
     # write to txt file for faster processing
     if($#manualT>0 and open my $txtfh, '>', $xlstxtfn){
        print "writting $xlstxtfn\n";
        print $txtfh "$scorer\n";
        print $txtfh join("\t",$_,@{$manualT[$_]}{qw/count lat/})."\n" for (1..$#manualT);
        close $txtfh;
     } else {
      print STDERR "$xlstxtfn not written!(have $#manualT trials in $xlsfn)\n";
     }

  }else{
     open my $txtfh, '<', $xlstxtfn;
     $scorer = <$txtfh>;
     chomp($scorer);
     while(<$txtfh>){
      chomp;
      #read trial count lat into manualT
      my ($trial, $count, $lat) = split(/\t/);
      $manualT[$trial] ={count=>$count ,lat=>$lat };

     }
     close $txtfh;
  }


  ################################
  # get auto scoring
  ################################
  my $autofnpath = "$basename/txt/*$run.trial.txt";
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
       $autoT[$trial] = {count=>$Count ,lat=>$lat,xdat=>$xdat }
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
     my $output=join("\t","$runname.$t", @{$autoT[$t]}{qw/count lat/},@{$manualT[$t]}{qw/count lat/},$scorer,${$autoT[$t]}{xdat})."\n";
     print $OUTFH $output;
     #print $output;
   #}
  }
}
close $OUTFH;

