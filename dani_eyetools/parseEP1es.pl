#!/usr/bin/env perl
# use to extract dur and xdat from e.g.
# /Users/lncd/rcn/bea_res/Personal/Will/Will please convert to E-Prime 2.0/MGS Encode - v. 1.es
# for f in ~/rcn/bea_res/Personal/Will/Will\ please\ convert\ to\ E-Prime\ 2.0/*MGS*es; do echo ./parseEP1es.pl "$f" \> taskData/$(echo $f|sed 's/.*\([1-3]\).es/\1/' ).txt;done
# later to be read by R
use strict; use warnings; use feature 'say';
use Data::Dumper;

my $i=0;
my @a=();

while(<>){
 chomp while chomp;
 s/^\s+//;
 if(m/\[Object(\d+)\]/){
    $i=$1;
    next;
 }
 m/(\w+)=([^]*)/ or next;
 my ($f,$v) = ($1,$2);
 next unless $f =~ /^(Name|Code|_ItemList|Duration)/;
 $v=~s/^"//;
 $v=~s/"$//;
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
lookup($h{SessionProc}[2],1)

