
SRCDIR        = $(srcdir)

EXTRA_DIST    = runtest.sh attribs2 powersupply.bom

RUN_TESTS     = ./runtest.sh

check_SCRIPTS = $(SRCDIR)/${RUN_TESTS}

TESTS         = ${RUN_TESTS}

regen:
	@$(SRCDIR)/${RUN_TESTS} --regen

clean-local:
	test -d gEDA && rm -rf gEDA || : ;

.PHONY: regen

MOSTLYCLEANFILES     = *.log *~
CLEANFILES           = *.log *~
DISTCLEANFILES       = *.log *~ core FILE
MAINTAINERCLEANFILES = *.log *~ Makefile.in
