## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## This file is processed by automake to produce Makefile.in, after which
## The configure script generates the actual Makefile from the Makefile.in.

exampledir    = $(docdir)/examples/CascodeAmp/

SRCDIR        = $(srcdir)

example_DATA  = gafrc \
				CascodeAmp.cir \
				CascodeAmp.sch \
                jfet_bias_iv.cir \
				Simulation.cmd \
                bom/CascodeAmp-bom.csv \
				img/2n5245_iv.png \
				models/2N5245.mod \
				models/bjt.lib \
				sym/njfet.sym \
				sym/npn-ebc.sym \
				sym/pnp-ebc.sym

EXTRA_DATA    = tests/CascodeAmp-bom.csv tests/CascodeAmp-geda.net

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

