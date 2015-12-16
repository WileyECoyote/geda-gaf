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
		model/2N3904.mod \
		bom/TwoStageAmp-bom.csv \
		sym/transistor.sym

EXTRA_DATA    = tests/TwoStageAmp-bom.csv tests/TwoStageAmp-geda.net

RUN_TESTS     = tests/runtest.sh

EXTRA_DIST    = $(nobase_example_DATA) $(RUN_TESTS) $(EXTRA_DATA)

check_SCRIPTS = tests

tests:
	@$(SRCDIR)/${RUN_TESTS} "TwoStageAmp" $(SRCDIR)

clean-local:
	test -d gEDA && rm -rf gEDA || : ;

.PHONY: tests

MOSTLYCLEANFILES     = *.log tests/*~ *~
CLEANFILES           = *.log tests/*~ *~ model/*~ sym/*~
DISTCLEANFILES       = *.log tests/*~ *~ model/*~ sym/*~
MAINTAINERCLEANFILES = *.log tests/*~ *~ model/*~ sym/*~ core Makefile.in

