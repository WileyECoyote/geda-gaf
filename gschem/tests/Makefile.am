## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## This file is processed by automake to produce Makefile.in, after which
## The configure script generates the actual Makefile from the Makefile.in.

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

regen:
	$(SRCDIR)/runtest.sh --regen $(BUILDDIR) $(SRCDIR)

.PHONY: regen

MOSTLYCLEANFILES     = *.log doc/*~ sort/*~ src/*~ sym/*~ *~
CLEANFILES           = *.log doc/*~ sort/*~ src/*~ sym/*~ *~
DISTCLEANFILES       = *.log doc/*~ sort/*~ src/*~ sym/*~ *~ core FILE
MAINTAINERCLEANFILES = *.log doc/*~ sort/*~ src/*~ sym/*~ *~ Makefile.in
