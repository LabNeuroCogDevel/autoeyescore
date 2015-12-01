#!/usr/bin/env perl
use strict; use warnings; use feature qw/say switch/;
use Data::Dumper;

#####
# create fake log file given an raw eye data file using the xdats
# 0. pull out start/target/stop info from raw eyd
# 1. use xdat and count to get position and duration
# 2. print timing and info
#    assume start time is 1.925
#    long dur target xdats are a trial that takes 13.5s
#    short dur target xdats                        8.5s
#
####
# check output like
# vimdiff \
#   <(sed 's/MGSTarget[RL][12]/MGSTarget/;s/Long\|Short//;' ~/rcn/bea_res/Data/Tasks/MGS/Basic//10129/20140103/Raw/EyeData/txt/10129.20140103.MGS.EPxdat.log) \
#   <(./fakeLog.pl  ~/rcn/bea_res/Data/Tasks/MGS/Basic//10129/20140103/Raw/EyeData/txt/10129.20140103.mgs.raw.txt)
#
#####
#
# TIMING:
#    -> XDAT 42/43
#     CueMGS (1.925s)
#    -> XDAT 121/
#     TargetMGS (.075s)
#     WMdelay (Sort/long, 2.5s, 7.5s) 
#     MGSExecute (2s)
#    -> XDAT 250
#     feedback (2s)

my @a=(); #$a[1]{start=>{onset=>,xdat=>,count=>, target=>...}
my $n=-1;

# keep time
my $srate=1/60;
my $time=0;

$_=<>; # read header line
while(<>){
  my ($xdat, $rest)  = split/\s+/;
  # increase n if we have a start xdat and we are just starting or we have a previous stop
  $n++ if $xdat < 100 && ($n<0 || $a[$n]->{stop});
  # skip beginning until we have start code
  next unless $n>=0;

  $time+=$srate;

  my $type;
  given($xdat){
    $type = 'start'  when ([42,43]);
    $type = 'target' when ([121..124]);
    $type = 'stop'   when ([250]);
    default { $type=''  } #$type='nonsense' }
  }

  next if $type eq '';


  $a[$n]->{$type}->{onset}= $time if !$a[$n]->{$type}->{onset};
  $a[$n]->{$type}->{xdat}= $xdat;
  $a[$n]->{$type}->{count}++;
}

#say Dumper(@a);
$time=1.925;
for my $t (@a) {
  my $pos;
  my $targ=$t->{target};

  # xdats' speak to position
  #xdat pos
  #121 40
  #122 160
  #123 460
  #124 600
  given($targ->{xdat}){
    $pos=40  when (121);
    $pos=160 when (122);
    $pos=460 when (123);
    $pos=600 when (124);
    default {$pos=0}
  }
  my $dur=($targ->{count})*$srate;

  # end after target + delay   + MGSencode 
  #            .075s + 7.5|2.5 + 2s
  my $endin=($dur > 5 ? 7.5 : 2.5) + 2.075;
  # long xdat is 43, short is 42
  my $startxdat=$dur > 5 ? 43 : 42;
  #se what we have
  #my $dtype =  $dur > 5 ? 'long' : 'short';
  #say join("\t", @{$targ}{qw/xdat onset/}, $dtype, sprintf("%.2f",$dur) ,$pos)

  # expected output is like
  # time    event          xdat    target
  #1.925   MGSTargetR2     124     600
  #11.5    EndTrial        250     600
  #13.5    StartMGSLong    43      600
  say join("\t",$time-1.925,'StartMGS',$startxdat, $pos);
  say join("\t",$time,'MGSTarget',$targ->{xdat}, $pos);
  say join("\t",$time+$endin,'EndTrial',250, $pos);

  # if mgs dur is > 5, it was long and took a total of 13.5s til the next one
  # if mgs     is not,        short                     8.5s
  $time+=$dur > 5 ? 13.5 : 8.5;


}

