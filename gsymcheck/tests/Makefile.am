## Process this file with automake to produce Makefile.in
AUTOMAKE_OPTIONS = $(GEDA_AUTO_MAKE_OPTIONS) $(GEDA_AUTOMAKE_TEST_OPTIONS)

EXTRA_DIST = runtest.sh

include output/Makefile_insert
include symbols/Makefile_insert

check_SCRIPTS = tests

BUILDDIR=$(builddir)
SRCDIR=$(srcdir)

tests:
	@fail=0 ; \
	pass=0 ; \
	for file in $(SRCDIR)/symbols/*.sym; do \
	  base=`basename $$file`; \
	  GEDADATARC=$(builddir)/../etc \
	  $(SHELL) $(SRCDIR)/runtest.sh $$file $(BUILDDIR) $(SRCDIR); \
          if [ $$? -ne 0 ]; then \
	     echo "$$base FAILED!"; \
             fail=`expr $$fail + 1`; \
	   else \
	     echo "$$base passed."; \
             pass=`expr $$pass + 1`; \
	   fi; \
	done ; \
	echo "passed: $$pass" ; \
	echo "failed: $$fail" ; \
	exit $$fail

MOSTLYCLEANFILES     = *.log *~ new_*
CLEANFILES           = *.log *~ new_*
DISTCLEANFILES       = *.log *~ new_* core FILE
MAINTAINERCLEANFILES = *.log *~ Makefile.in
