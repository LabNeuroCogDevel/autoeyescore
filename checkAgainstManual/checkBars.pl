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
my @scoreSheets;
sub wanted {push @scoreSheets, $File::Find::name if  m:fs.*xls:i};
find( {wanted => \&wanted,no_chdir=>1,follow=>1}, '/mnt/B/bea_res/Data/Tasks/BarsBehavioral/Basic/' );

my $p = Spreadsheet::ParseExcel->new();

# print header
print join(",",qw(subject.date.trial PCdif autoPC manPC a_drop m_drop scorer conf)),"\n";

#@scoreSheets=qw(/mnt/B/bea_res/Data/Tasks/BarsBehavioral/Basic/10156/20110810/Scored/Run02/fs_10156_bars2.xls
#                /mnt/B/bea_res/Data/Tasks/BarsBehavioral/Basic/10847/20100920/Scored/Run01/fs_10847_bars1.xls
#                );
for my $xlsfn (@scoreSheets){
  next if $xlsfn =~ /^$/;
  my $run = $1 if $xlsfn =~ m:Run0(\d):;
  

  my $xls    = ($p->parse($xlsfn)->worksheets())[0];

  my $manPC      = sprintf('%.2f',$xls->get_cell(5,12)->unformatted());

  my $confidence = $xls->get_cell(0,0)->unformatted();
  $confidence    = $1 if $confidence  =~ /(\d.*) of 5?/;
  $confidence    =~ s/,/;/g;
  

  my $scorer = $xls->get_cell(1,0)->unformatted();
  $scorer =~ s/scorer:? ?//i;
  my $dropped    = $xls->get_cell(1,6)->unformatted();
  $dropped=0 if !$dropped;

  # get auto scoring
  my $autofnpath = dirname(dirname($xlsfn)) ."/txt/*$run.trial.txt";
  my @autofn = glob( $autofnpath );

  # initialze
  my $trial  = basename($autofnpath).".$run";
  my $autoTotalRuns = 0;
  my $autoPC = 0;

  # if there is a score sheet
  if($#autofn>=0){
     $trial=basename($autofn[0],'.trial.txt');
     open my $autoFH, $autofn[0];
     my %a=(TRUE=>0,FALSE=>0);
     while(<$autoFH>) {
       my @F=split/\W+/;
       $a{$F[3]}+=1;
     }
     #print STDERR "$trial has ", $.-1, "entries\n" if $. != 61;
     close $autoFH;

     $autoTotalRuns=$a{TRUE}+$a{FALSE};
     $autoPC=sprintf("%.2f", $a{TRUE}/($autoTotalRuns)*100);
  }

  print join(",",$trial, sprintf('%.2f',$autoPC - $manPC), $autoPC,$manPC,60 - $autoTotalRuns,$dropped,$scorer, $confidence), "\n";
}

