## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## This file is processed by automake to produce Makefile.in, after which
## The configure script generates the actual Makefile from the Makefile.in.

AUTOMAKE_OPTIONS = $(GEDA_AUTO_MAKE_OPTIONS) $(GEDA_AUTOMAKE_TEST_OPTIONS)

SUBDIRS       = model sym

exampledir    = $(docdir)/examples/RF_Amp/

SRCDIR        = $(srcdir)

example_DATA  = 5980-2396E.pdf gafrc MSA-2643.cir MSA-2643.sch \
                Q1.cir Q1.sch Q2.cir Q2.sch README Simulation.cmd \
                bom/MSA-2643-bom.csv

EXTRA_DATA    = tests/MSA-2643-bom.csv tests/MSA-2643-geda.net

RUN_TESTS     = tests/runtest.sh

EXTRA_DIST    = $(example_DATA) $(RUN_TESTS) $(EXTRA_DATA)

check_SCRIPTS = tests

tests:
	@$(SRCDIR)/${RUN_TESTS} "MSA-2643" $(SRCDIR)

clean-local:
	test -d gEDA && rm -rf gEDA || : ;

.PHONY: tests

MOSTLYCLEANFILES     = *.log tests/*~ *~
CLEANFILES           = *.log tests/*~ *~ model/*~ sym/*~
DISTCLEANFILES       = *.log tests/*~ *~ model/*~ sym/*~
MAINTAINERCLEANFILES = *.log tests/*~ *~ model/*~ sym/*~ core Makefile.in

