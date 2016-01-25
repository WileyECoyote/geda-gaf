## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## This file is processed by automake to produce Makefile.in, after which
## The configure script generates the actual Makefile from the Makefile.in.

exampledir   = $(docdir)/examples/lightning_detector/

SRCDIR       = $(srcdir)

example_DATA = \
		gafrc \
		lightning_detector.net \
		lightning_detector.pcb \
		lightning_detector.sch \
		revision-history.txt \
		schbom schbom.readme \
		bom/lightning_detector.sch.bom  \
		bom/lightning_detector-bom.csv \
		sym/2N4401.sym \
		sym/2N4403.sym

EXTRA_DIST    = $(example_DATA) \
		bom/lightning_detector-bom.csv \
		tests/lightning_detector-bom.csv \
		tests/lightning_detector-geda.net \
		tests/runtest.sh

RUN_TESTS     = tests/runtest.sh

check_SCRIPTS = $(SRCDIR)/${RUN_TESTS}

TESTS         = ${RUN_TESTS}

regen:
	@$(SRCDIR)/${RUN_TESTS} --regen

clean-local:
	test -d gEDA && rm -rf gEDA || : ;

.PHONY: regen

MOSTLYCLEANFILES     = *.log tests/*~ *~
CLEANFILES           = *.log tests/*~ *~ sym/*~
DISTCLEANFILES       = *.log tests/*~ *~ sym/*~
MAINTAINERCLEANFILES = *.log tests/*~ *~ sym/*~ core Makefile.in

