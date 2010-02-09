sml-fortran
===========

For parser, see:

  http://fortran-parser.sourceforge.net/

sml-fortran is currently developed usign SML/NJ 110.65.

  http://www.smlnj.org/

To build as standalone:

  cd src/
  make

For x86 linux, tweak the makefile to change "darwin" to "linux" in the
heap2asm command.

To build interactively:

  cd src/
  sml
  - CM.make "sources.cm";

To run, first generate a text file with OFP output:

  RunOFP foo.f90 > foo.out

With standalone:

  src/driver foo.out foo.dot

In interactive,

  cd src/
  sml
  - CM.make "sources.cm";
  - val (p,ctxt) = Driver.setup "../foo.out";
  - val (p,ctxt) = Driver.doone p ctxt;
  - val (p,ctxt) = Driver.doone p ctxt;
  - val (p,ctxt) = Driver.doone p ctxt;
...


