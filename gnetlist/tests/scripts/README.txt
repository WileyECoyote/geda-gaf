# $Id$
#

This directory contains a test scheme for the "netlist" scripts.
If a bug report is filed, please add a test to show the bug has been
fixed.

Seems to me, tests in the gEDA-gaf suite are way too complex. At least
for me, Wiley. Some don't have the necessary schematics, and even ones
that do, don't work. Who knows how to fix? Where is the documentation?
Since I don't know how to create such complex schemes, and I'm not sure
that I even want to, I just created a simple scheme to run some tests
for scripts that I modified in this suite. It's very simple. Even the
the Makefile.am is comprehensible:

tests:
	@exec $(srcdir)/${RUN_TESTS} ${SAMPLES}

The variable SAMPLES is the list of schematics to be used in the test
and the RUN_TESTS variable is the bash script this perform the tests.
Both variables are members of EXTRA_DIST. Essentially, the list of sch
files is passed as arguments to the run_test.sh script. If the scripts
exit with code 0, we passed. If the scripts exit with any value other
0, we did not pass.



