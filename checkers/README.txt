This directory contains a set of ROSE-based checkers for analyzing
Fortran codes for various properties for performance and correctness.

To create a checker perform the following steps:

1. Run the script to generate the checker skeleton

  ROSE_HEAD/projects/compass/src/compass_scripts/gen_checker.sh checker name

Note, it is actually best to separate the name into two words; it will be
converted to camelback, e.g., checkerName.

2. Edit the generated Makefile and uncomment and fix the following macro
variables:

   a. ROSE_INSTALL = path to your ROSE install directory


3. Copy the following files ...

