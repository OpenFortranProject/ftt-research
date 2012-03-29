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
## get command line options
##

use File::Temp qw / tempdir /;
use Getopt::Long;

# options:
#
# -c [compiler] : set compiler
# -m [maxkinds] : specify upper bound for kinds to test
# -a            : dump aliases only, ignoring warnings for explicit kind maps

my %args = ();

GetOptions("c:s"=>\$args{compiler},
	   "m:i"=>\$args{maxkinds},
	   "onlyalias"=>\$args{onlyalias},
           "help"=>\$args{help});

if (defined $args{help}) {
    print <<ENDER

usage: probe_kinds.pl -c [compiler] -m [maxkinds] -onlyalias

  -c compiler : specify Fortran compiler to probe. (DEFAULT: gfortran)
  -m maxkinds : specify maximum kind value to probe.  (DEFAULT: 32)
  -onlyalias : flag to only print type aliases that map intrinsic
               types to their explicitly kind-annotated equivalent.

ENDER
	       ;
    exit(1);
}

##
## make stderr go to /dev/null
##
open STDERR, ">/dev/null";

##
## create a temp directory, and go into it.  set cleanup flag so that when the
## script exits, the directory and its contents are all cleaned up.
##
$tempdir = tempdir( CLEANUP => 1 ) || die "Error creating temporary work directory.";
chdir($tempdir) || die "Error changing to temporary location.";

# upper bound on kind values to try
$maxkinds = 32;
if (defined $args{maxkinds}) {
    $maxkinds = $args{maxkinds};
}

# compiler to invoke.
$compiler = "gfortran";
if (defined $args{compiler}) {
    $compiler = $args{compiler};
}

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

# all the types we care about
@typeset = ("real","integer","complex","character","logical");

# define some maps that indicate which mappings are considered
# erroneous or warnable, regardless of kind. prec_drops is used to
# deal with legal maps that are warnable due to precision loss.
$error_maps{"character"} = \@typeset;
$error_maps{"logical"} = \@typeset;
$error_maps{"real"} = ["logical","character"];
$error_maps{"integer"} = ["logical","character"];
$error_maps{"complex"} = ["logical","character"];

$warn_maps{"real"} = ["integer"];
$warn_maps{"complex"} = ["real","integer"];
$warn_maps{"integer"} = ["real","complex"];

$prec_drop{"real"} = ["complex"];

# first, dump the aliases to map intrinsic types to a specific
# kinded type
foreach $t (@typeset) {
    $defkind = &probe_default_kind($t);
    print "$t = ".$t."_".$defkind."\n";
}

# if they want only aliases, bail out now
if (defined $args{onlyalias}) {
    exit(0);
}

# within each type, make sure high kind to low kind conversions
# are considered legal but warnable.
foreach $t (@typeset) {
    # sort kinds in numerically descending order.
    @validkinds = sort {$b <=> $a} (&probe_kind_range($t));

    # store the set of valid kinds for later
    $vkinds{$t} = \@validkinds;

    for ($i = 0; $i < $#validkinds; $i++) {
	for ($j = $i+1; $j <= $#validkinds; $j++) {
	    $ikindtype = $t."_".$validkinds[$i];
	    $jkindtype = $t."_".$validkinds[$j];
	    print "$ikindtype -> $jkindtype : error\n";
	}
    }
}

# now spin through the warn and error maps
foreach $t (@typeset) {
    foreach $er (@{$error_maps{$t}}) {
	if ($t eq $er) {
	    next;
	}
	@src_vkinds = @{$vkinds{$t}};
	@dest_vkinds = @{$vkinds{$er}};
	foreach $sk (@src_vkinds) {
	    foreach $dk (@dest_vkinds) {
		print $t."_".$sk." -> ".$er."_".$dk." : error\n";
	    }
	}
    }

    foreach $wr (@{$warn_maps{$t}}) {
	if ($t eq $wr) {
	    next;
	}
	@src_vkinds = @{$vkinds{$t}};
	@dest_vkinds = @{$vkinds{$wr}};
	foreach $sk (@src_vkinds) {
	    foreach $dk (@dest_vkinds) {
		print $t."_".$sk." -> ".$wr."_".$dk." : warn\n";
	    }
	}
    }

    foreach $pr (@{$prec_drop{$t}}) {
	@src_vkinds = @{$vkinds{$t}};
	@dest_vkinds = @{$vkinds{$pr}};
	foreach $sk (@src_vkinds) {
	    foreach $dk (@dest_vkinds) {
		if ($dk < $sk) {
		    print $t."_".$sk." -> ".$pr."_".$dk." : warn\n";
		}
	    }
	}	
    }
}
