AUTOMAKE_OPTIONS = $(GEDA_AUTO_MAKE_OPTIONS) $(GEDA_AUTOMAKE_TEST_OPTIONS)

EXTRA_DIST = gafrc runtest.sh IMAGE_DIFF LOG_DIFF

include Makefile_insert
include doc/Makefile_insert
include golden/Makefile_insert
include sort/Makefile_insert
include src/Makefile_insert
include sym/Makefile_insert

check_SCRIPTS = tests

BUILDDIR=$(builddir)
SRCDIR=$(srcdir)

tests:
	$(SRCDIR)/runtest.sh $(BUILDDIR) $(SRCDIR)

MOSTLYCLEANFILES     = *.log doc/*~ sort/*~ src/*~ sym/*~ *~
CLEANFILES           = *.log doc/*~ sort/*~ src/*~ sym/*~ *~
DISTCLEANFILES       = *.log doc/*~ sort/*~ src/*~ sym/*~ *~ core FILE
MAINTAINERCLEANFILES = *.log doc/*~ sort/*~ src/*~ sym/*~ *~ Makefile.in
