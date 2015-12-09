## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## This file is processed by automake to produce Makefile.in, after which
## The configure script generates the actual Makefile from the Makefile.in.

AUTOMAKE_OPTIONS = $(GEDA_AUTO_MAKE_OPTIONS) $(GEDA_AUTOMAKE_TEST_OPTIONS)

EXTRA_DIST       = gTAG-geda.net runtest.sh

check_SCRIPTS    = tests

SRCDIR           = $(srcdir)

tests:
	@$(SRCDIR)/runtest.sh "gTAG" $(SRCDIR)

MOSTLYCLEANFILES     = *~
CLEANFILES           = *~
DISTCLEANFILES       = *~ core FILE
MAINTAINERCLEANFILES = *~ Makefile.in
