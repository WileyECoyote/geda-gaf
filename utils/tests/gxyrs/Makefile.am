## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
##
## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = $(GEDA_AUTO_MAKE_OPTIONS) $(GEDA_AUTOMAKE_TEST_OPTIONS)

SUBDIRS= inputs outputs

AM_TESTS_ENVIRONMENT= PERL=${PERL} top_srcdir=${srcdir}/../..

check-local:
	$(AM_TESTS_ENVIRONMENT) srcdir=$(srcdir) $(SHELL) $(srcdir)/run_tests.sh

EXTRA_DIST= run_tests.sh tests.list README.txt

MOSTLYCLEANFILES     = *.log *.ps core FILE *~
CLEANFILES           = *.log *.ps core FILE *~
DISTCLEANFILES       = *.log *.ps core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in
