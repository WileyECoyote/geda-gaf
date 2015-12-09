
BUILDDIR=$(top_builddir)/libgedathon/tests
SRCDIR=$(srcdir)

RUN_TESTS= run_tests.sh

check_SCRIPTS= ${RUN_TESTS}

TESTS= ${RUN_TESTS}

EXTRA_DIST= ${RUN_TESTS} lpfilter.sch README.txt

check-am: update-gsymcheck

tests:
	@exec $(srcdir)/${RUN_TESTS}

tester:
	mv $(srcdir)/tmp/lpfilter.sch $(BUILDDIR)/lpfilter.sch
	rm -rf $(BUILDDIR)/tmp

update-gsymcheck:
	(cd $(top_builddir)/gsymcheck/src && $(MAKE) $(AM_MAKEFLAGS) all);

uninstall-hook:
	@(test -d geda && rm -rf geda) || : ;

.PHONY: update-gsymcheck

clean-local:
	test -d tmp && rm -rf tmp || : ;

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
