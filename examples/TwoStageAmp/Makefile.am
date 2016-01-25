## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## This file is processed by automake to produce Makefile.in, after which
## The configure script generates the actual Makefile from the Makefile.in.

SRCDIR        = $(srcdir)

exampledir    = $(docdir)/examples/TwoStageAmp/

nobase_example_DATA = \
		attribs \
		gafrc \
		Simulation.cmd \
		TwoStageAmp.sch \
		spice.netlist \
		models/2N3904.mod \
		bom/TwoStageAmp-bom.csv \
		sym/transistor.sym

EXTRA_DATA    = tests/TwoStageAmp-bom.csv tests/TwoStageAmp-geda.net \
                bom/TwoStageAmp-bom.csv

EXTRA_DIST    = $(nobase_example_DATA) $(RUN_TESTS) $(EXTRA_DATA)

RUN_TESTS     = tests/runtest.sh

check_SCRIPTS = $(SRCDIR)/${RUN_TESTS}

TESTS         = ${RUN_TESTS}

regen:
	@$(SRCDIR)/${RUN_TESTS} --regen

clean-local:
	test -d gEDA && rm -rf gEDA || : ;

.PHONY: regen

MOSTLYCLEANFILES     = *.log tests/*~ *~
CLEANFILES           = *.log tests/*~ *~ model/*~ sym/*~
DISTCLEANFILES       = *.log tests/*~ *~ model/*~ sym/*~
MAINTAINERCLEANFILES = *.log tests/*~ *~ model/*~ sym/*~ core Makefile.in

