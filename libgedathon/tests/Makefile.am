
BUILDDIR=$(top_builddir)/libgedathon/tests
SRCDIR=$(srcdir)

RUN_TESTS= run_tests.sh

check_SCRIPTS= ${RUN_TESTS}

#TESTS_ENVIRONMENT = cd $(srcdir) &&

TESTS= ${RUN_TESTS}

EXTRA_DIST= ${RUN_TESTS} lpfilter.sch README.txt

tests:
	@exec $(srcdir)/${RUN_TESTS}

tester:
	mv $(srcdir)/tmp/lpfilter.sch $(builddir)/lpfilter.sch
	rm -f  $(BUILDDIR)/tmp/sym/*
	rm -rf $(BUILDDIR)/tmp/sym
	rm -r  $(BUILDDIR)/tmp/*
	rm -r  $(BUILDDIR)/tmp

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
