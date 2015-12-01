#!/usr/bin/env perl

# WF 20151105
### ADAPTED from MGSEncode
# diff: reads ep as function, now also reads log
#
# use to extract dur and xdat from e.g.
#   for f in eptaskfiles/*es; do 
#      echo ./parseEP1es.pl "$f" \> taskData/$(echo $f|sed 's/.*\([1-3]\).es/\1/' ).txt;
#   done
#
# later to be read by R: parseEP1es.R
#
# - first pass
#    use Levels "spreasheet" to get order of top level events
#    read in each object and record name,dur, and xdat
# - second pass
#   go through object array and make hash, keyed by object name
#   go through levels, recursively opening objects and print xdat and duration
#
#
use strict; use warnings; use feature 'say';
use Data::Dumper;


# read in eprime file
# needs .es file as input
sub readEP {
  my $file=shift;
  open my $EPFH, '<',$file or die "cannot open file $file";

  my $i=0;
  my @a=();
  my @taskorder=();

  while(<$EPFH>){
   chomp while chomp;
   s/^\s+//;
   if(m/\[Object(\d+)\]/){
      $i=$1;
      next;
   }
   m/([.()\w]+)=([^]*)/ or next;
   my ($f,$v) = ($1,$2);
  
   $v=~s/^"//;
   $v=~s/"$//;
  
   # "Spreadsheet" of task order stored in lines like
   #Levels(34).ValueString="5\t\tFix\t \t"
   #Levels(1).ValueString="4\t\tMGSL1Short\t40\t2500\t"
   push @taskorder, [ (split/\\t/,$v)[0,2,3,4] ]  if $f =~ m/Levels.*Value/;
  
   # grab useful event info, put into array "a"
   next unless $f =~ /^(Name|Code|_ItemList|Duration)/;
   $a[$i]->{$f} = $v;
   $a[$i]->{xdat} = 10+$1 if $v=~m/EventStrobe\+(\d+)/;
  }
  
  ## rewrite data structure
  my %h;
  for $a (@a) {
   $h{ $a->{Name} } = [ @$a{qw/xdat Duration _ItemList/} ]
  }
  
  return {h=>{%h},taskorder=>[@taskorder]};
}

## recursively decent into itemlists stored in global h
sub lookup {
 our %h;
 our $time;
 my ($itemlist,$level,@rest) = @_;
 for my $i (split/;/,$itemlist){
  my $dur = $h{$i}[1] || 0;
  $time+= $h{$i}[1] || 0 ;
  # print everything for every line
  #say join("\t",$time/1000,$level,$i,map {$_||0} @{$h{$i}}[1,0],@rest);
  # print selection for only xdats
  say join("\t",$time/1000,$i,map {$_||0} $h{$i}[0],@rest) if @{$h{$i}}[0];
  lookup($h{$i}[2],$level+1,@rest) if $h{$i}[2];
 }
}


# if the EP event list is not random
# we can read right off the task list
# this is not the case for MGS, but is for MGSEncode
# here for postarity
sub selfcontained {
   my @taskorder=$@;
   for my $t (@taskorder) {
     #Levels(1).ValueString="4\t\tMGSL1Short\t40\t2500\t"
     my ($reps,$name,$pos,$dur) = @$t;
     lookup($name,1,$pos,$dur) for (1..$reps);
   }
}


sub readLog {
  my $file=shift;
  open my $logfh, '<',$file or die "cannot open log file $file";
  my @tasklist=();
  my %temp=();
  while(<$logfh>){
    chomp while chomp;
    if(m/Frame End/ and $temp{Procedure}){
       push @tasklist, [@temp{qw/Procedure Location WMDelay/}];
       %temp=();
    }
    next unless /^\s*(WMDelay|Location|Procedure):\s*([^]+)/;
    $temp{$1}=$2;
  }
  return @tasklist;
}

######## MAIN #######

my $finfo =readEP($ARGV[0]);
our %h = %{$finfo->{h}};
our $time = 0;

my @tasklist = readLog($ARGV[1]);
for my $t (@tasklist) {
  my @a=@$t;
  # send everyhting
  #lookup($a[0],1, @a[1..$#a]);
  # send postition only
  lookup($a[0],1, $a[1]);
}
