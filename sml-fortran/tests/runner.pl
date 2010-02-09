#!/usr/bin/perl

opendir(DIR,".") || die "Can't opendir .: $!";
@files = grep { /\.f90$/ } readdir(DIR);
closedir(DIR);

foreach $f (@files) {
    $outfile = $f;
    $outfile =~ s/\.f90$/\.out/;
    $dotfile = $f;
    $dotfile =~ s/\.f90$/\.dot/;
    print "\n===========================================================\n";
    print "Processing $f...\n";
    system("RunOFP $f $outfile");
    print "Running sml-fortran...\n";
    system("../src/driver -i $outfile -o $dotfile");
}

