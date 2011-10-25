This directory contains a set of ROSE-based checkers for analyzing
Fortran codes for various properties for performance and correctness.

= To create a checker perform the following steps =

1. Run the script to generate the checker skeleton

  ROSE_HEAD/projects/compass/src/compass_scripts/gen_checker.sh checker name

Note, it is actually best to separate the name into two words; it will be
converted to camelcase, e.g., checkerName.

2. Edit the generated Makefile and uncomment and fix the following macro
variables:

   a. ROSE_INSTALL = path to your ROSE install directory


3. Copy the following files ...
4. TODO: add a script to setup test harness for new checkers.

= Testing =

== Test Harness ==
Each checker should have a tests directory, with the following layout:
{{{
  tests
    * cases
    * expected
}}}

The `cases` directory contains input for the checker and the `expected`
directory contains the expected output for the checker.  For example, if the
test case is named `test1.f90` then the following files should exist:
  * `tests/cases/test1.f90`
  * `tests/expected/test1.f90.out`

When the test is run the output will be put in `tests/actual/test1.f90.out`
and diffed against `tests/expected/test1.f90.out`, if the files differ the
test is considered a failure.  This makes testing easy but brittle against
changes to the output.

== Running tests ==

Currently there is no support for runing an individual test.  To run the
tests for a checker, go into the checker directory and run `run-tests.sh`.
This will look in `tests/cases` and invoke the checker on each file it finds
there and report the resutlts.
