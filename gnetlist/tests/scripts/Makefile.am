
BUILDDIR=$(top_builddir)/gnetlist/tests/scripts
SRCDIR=$(srcdir)
GNETLIST=$(top_builddir)/gnetlist/src/gnetlist/scripts

RUN_TESTS= run_tests.sh
SAMPLES=inverter.sch usg_driver.sch

check_SCRIPTS= ${RUN_TESTS}

TESTS= ${RUN_TESTS}

EXTRA_DIST= ${RUN_TESTS} ${SAMPLES} README.txt

tests:
	@exec $(srcdir)/${RUN_TESTS} ${SAMPLES}

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
