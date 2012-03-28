#!/usr/bin/perl
#############################################################################
##
## fortran type kind probe.  intended for automatic generation of supported
## kind sets for compilers.
##
## matt@galois.com // march 2012
##
#############################################################################

##
## make stderr go to /dev/null
##
open STDERR, ">/dev/null";

# upper bound on kind values to try
$maxkinds = 32;

# compiler to invoke.  TODO: make this a parameter
$compiler = "gfortran";

##
## TODO: find perl routines that let you set up a sandbox for doing
##       this kind of thing and then cleaning up afterwards.
##

##
## given a type name, generate a kind tester with an empty kind string,
## yielding a program that will print the kind value that is the
## default for the type.  return this value.
##
sub probe_default_kind {
  my $typename = shift(@_);
  &generate_kind_test($typename, "");
  system("$compiler -o testfile testfile.f03");
  $kindval = int(`./testfile`);
  unlink("testfile");
  unlink("testfile.f03");
  return $kindval;
}

##
## given a type name, probe a sequence of kind values to see which
## are legal.  the legality is tested by attempting to compile the
## code to an object file and testing if the compile fails, indicating
## that the kind is not legal.  this test assumes that failure can
## be detected by looking at the error code set by the compiler.
##
sub probe_kind_range {
  my $typename = shift(@_);

  @validkinds = ();

  for ($k=0; $k<$maxkinds; $k++) {
    &generate_kind_test($typename,"(kind=$k)");
    system("$compiler -c testfile.f03");

    if ($? == 0) {
      push(@validkinds, $k);
    }
    unlink("testfile.f03");
    unlink("testfile.o");
  }

  return @validkinds;
}

##
## given a typename and a kind parameter (full string, not just the
## number), generate a test that can be used to test if that type and
## kind combination is legal.  if the kind string is empty, the
## generate code will print the default kind value for the given type.
##
sub generate_kind_test {
  my ($typename,$kindname) = @_;

  open(OUTFILE,">testfile.f03");

  $type_string = $typename.$kindname;

  print OUTFILE<<ENDER
program testfile
  $type_string :: i
  print *, kind(i)
end program testfile
ENDER
;
  close(OUTFILE);
}

#############################################################################
## main program body
#############################################################################

@typeset = ("real","integer","complex","character","logical");

foreach $t (@typeset) {
    $defkind = &probe_default_kind($t);

    # sort kinds in numerically descending order.
    @validkinds = sort {$b <=> $a} (&probe_kind_range($t));

    print "$t = ".$t."_".$defkind."\n";
    for ($i = 0; $i < $#validkinds; $i++) {
	for ($j = $i+1; $j <= $#validkinds; $j++) {
	    $ikindtype = $t."_".$validkinds[$i];
	    $jkindtype = $t."_".$validkinds[$j];
	    print "$ikindtype -> $jkindtype : error\n";
	}
    }
}
