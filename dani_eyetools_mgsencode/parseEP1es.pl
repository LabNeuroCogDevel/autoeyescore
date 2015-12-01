#!/usr/bin/env perl
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

my $i=0;
my @a=();
my @taskorder=();

while(<>){
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
 push @taskorder, [ (split/\\t/,$v)[0,2] ]  if $f =~ m/Levels.*Value/;

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

sub lookup {
 my ($itemlist,$level) = @_;
 for my $i (split/;/,$itemlist){
  lookup($h{$i}[2],$level+1) if $h{$i}[2];
  say join("\t",$level,$i,map {$_||"NA"} @{$h{$i}}[1,0]);
 }
}


### parse
lookup('disacq',1); # first 6 seconds before going into MgsErExpt
for my $t (@taskorder) {
  lookup($t->[1],1) for (1..$t->[0]);
}

