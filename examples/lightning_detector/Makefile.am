exampledir = $(docdir)/examples/lightning_detector/

SRCDIR     = $(srcdir)

nobase_example_DATA = \
		bom bom.readme gafrc \
		lightning_detector.net \
		lightning_detector.pcb \
		lightning_detector.sch \
		lightning_detector.sch.bom  \
		revision-history.txt \
		sym/2N4401.sym sym/2N4403.sym

EXTRA_DIST    = $(nobase_example_DATA) \
		tests/lightning_detector-geda.net \
		tests/runtest.sh

RUN_TESTS     =	tests/runtest.sh

check_SCRIPTS = tests

tests:
	@$(SRCDIR)/${RUN_TESTS} "lightning_detector" $(SRCDIR)

.PHONY: tests

MOSTLYCLEANFILES     = *.log *~
CLEANFILES           = *.log *~
DISTCLEANFILES       = *.log *~ core FILE
MAINTAINERCLEANFILES = *.log *~ Makefile.in

