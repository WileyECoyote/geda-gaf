## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## This file is processed by automake to produce Makefile.in, after which
## The configure script generates the actual Makefile from the Makefile.in.

SUBDIRS       = models sym

exampledir    = $(docdir)/examples/MSA-2643/

SRCDIR        = $(srcdir)

example_DATA  = 5980-2396E.pdf gafrc MSA-2643.cir MSA-2643.sch \
                Q1.cir Q1.sch Q2.cir Q2.sch README Simulation.cmd \
                bom/MSA-2643-bom.csv

EXTRA_DATA    = tests/MSA-2643-bom.csv tests/MSA-2643-geda.net \
                bom/MSA-2643-bom.csv

EXTRA_DIST    = $(example_DATA) $(RUN_TESTS) $(EXTRA_DATA)

RUN_TESTS     = tests/runtest.sh

check_SCRIPTS = $(SRCDIR)/${RUN_TESTS}

TESTS         = ${RUN_TESTS}

regen:
	@$(SRCDIR)/${RUN_TESTS} --regen

clean-local:
	test -d gEDA && rm -rf gEDA || : ;

.PHONY: regen

MOSTLYCLEANFILES     = *.log tests/*~ *~
CLEANFILES           = *.log tests/*~ *~ models/*~ sym/*~
DISTCLEANFILES       = *.log tests/*~ *~ models/*~ sym/*~
MAINTAINERCLEANFILES = *.log tests/*~ *~ models/*~ sym/*~ core Makefile.in

