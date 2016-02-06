## Process this file with automake to produce Makefile.in

EXTRA_DIST = 1217.geda bottom.sch bottom.sym gnetlistrc.hierarchy gschemrc \
	     gschlasrc hierarchy.geda middle.sch middle.sym rock.sch rock.sym \
	     top.sch pnp-hierarchy.sym

check_SCRIPTS = tests

BUILDDIR=$(top_builddir)/gnetlist/tests/hierarchy
SRCDIR=$(srcdir)
GNETLIST=$(top_builddir)/gnetlist/src/gnetlist

tests:

# pre-clean
	@rm -f $(BUILDDIR)/new_*
# make and run the tests
	@cp -f $(SRCDIR)/gnetlistrc.hierarchy $(BUILDDIR)/gnetlistrc
	( TESTDIR=$(SRCDIR) \
	  GEDADATARC=$(top_builddir)/gnetlist/lib \
	  SCMDIR=${top_builddir}/gnetlist/scheme \
	  SYMDIR=$(top_srcdir)/symbols \
	    $(GNETLIST) \
	      -L $(top_srcdir)/libgeda/scheme \
	      -L $(top_builddir)/libgeda/scheme \
	      -o $(BUILDDIR)/new_hierarchy.geda \
	      -g geda $(SRCDIR)/top.sch )
	diff $(SRCDIR)/hierarchy.geda $(BUILDDIR)/new_hierarchy.geda;
	@if test $$? -ne 0 ; then \
		echo "FAILED: gnetlist/tests/hierarchy" ; \
		cat $(BUILDDIR)/failed.log ; \
		exit 1 ; \
	else \
		echo "PASSED: gnetlist/tests/hierarchy" ; \
	fi
	@rm -f $(BUILDDIR)/gnetlistrc
	@rm -rf $(BUILDDIR)/logs

MOSTLYCLEANFILES     = new_* *.log *~ gnetlistrc pnp-test.sym
CLEANFILES           = new_* *.log *~ gnetlistrc pnp-test.sym
DISTCLEANFILES       = new_* *.log *~ gnetlistrc pnp-test.sym
MAINTAINERCLEANFILES = new_* *.log *~ gnetlistrc pnp-test.sym Makefile.in
