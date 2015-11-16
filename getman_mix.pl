#!/usr/bin/env perl
use Spreadsheet::ParseExcel;
use File::Basename;
use File::Path;
my $p = Spreadsheet::ParseExcel->new();
for my $xlsfn (@ARGV) {
 
  print "looking at: $xlsfn\n" if $ENV{DEBUG};
  my ($subj,$rundate,$run,$scorer)=(0)x4;
  $subj    = $1 if $xlsfn =~ m:/(\d{5})/:;
  $rundate = $1 if $xlsfn =~ m:/(\d{8})/:;
  $run     = $1 if $xlsfn =~ m/(fix|anti|vgs)/;

  my $xlstxtfn = "mix/$subj.$rundate.$run.manual.txt";
  print "making/checking $xlstxtfn\n";
  
  my @trialsacs; # array of trials with an array of hashes lat dlat dacc
  my @manualT;   # array of trials with hash for countToCorrect and fist lat

  next unless ! -e $xlstxtfn;

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
}
