#!/usr/bin/env bash
#
# #### $0 ######
#  print ok meh or BAD for all changes to checkAgainstManual_trial.csv from two points in git history
# 
# usage:
#   $0 $paradim $prev[=HEAD~1] $current[=HEAD]
# e.g
#   $0 antistate  # same as antistate HEAD~1 HEAD
#   $0 vgs HEAD~2 HEAD~1 |grep BAD
#  
cd $(dirname $0)

[ -z "$1" -o ! -d "$1" ] && sed -n "s:\$0:$0:g;s/^# //p;/END/q" $0 && exit 1

prev=HEAD~1
current=HEAD
[ -n "$1" ] && paradigm=$1
[ -n "$2" ] && prev=$2
[ -n "$3" ] && current=$3
# look at difference in checkAgainstManual
# if change matches scorer, say good
# if change doesn't AND previous was good, say BAD
# if change doesn't help or hurt, write meh
git diff -U0 --minimal $prev $current $paradigm/results/checkAgainstManual_trial.csv | perl -slane 'next unless /^[+-]\d/; $F[0]=~s/^([+-])//; $a{$F[0]}->{$1}=$F[1]; $a{$F[0]}->{score}=$F[3];END{for (keys %a) { print join("\t",$_,$a{$_}->{"-"}, $a{$_}->{"+"},$a{$_}->{score},$a{$_}->{score}==$a{$_}->{"+"}?"ok":($a{$_}->{score}==$a{$_}->{"-"}?"BAD":"meh") ) if $a{$_}->{"-"} ne $a{$_}->{"+"} } }'
