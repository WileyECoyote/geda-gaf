
BUILDDIR      = $(top_builddir)/libgedathon/tests

SRCDIR        = $(srcdir)

RUN_TESTS     = run_tests.sh

check_SCRIPTS = ${RUN_TESTS}

TESTS         = ${RUN_TESTS}

EXTRA_DIST    = ${RUN_TESTS} lpfilter.sch README.txt

check-am: update-dependents

tests:
	@exec $(srcdir)/${RUN_TESTS}

regen:
	mv $(srcdir)/tmp/lpfilter.sch $(BUILDDIR)/lpfilter.sch
	rm -rf $(BUILDDIR)/tmp

DEPENDS=\
	libgeda/src \
	libgedathon/src \
	gsymcheck/src

update-dependents:
	@for d in $(DEPENDS); do \
	  (cd $(top_builddir)/$$d && $(MAKE) $(AM_MAKEFLAGS) all); \
	done

uninstall-hook:
	@(test -d geda && rm -rf geda) || : ;

.PHONY: regen update-dependents

clean-local:
	test -d tmp && rm -rf tmp || : ;

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
