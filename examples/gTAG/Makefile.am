## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## This file is processed by automake to produce Makefile.in, after which
## The configure script generates the actual Makefile from the Makefile.in.

exampledir       = $(docdir)/examples/gTAG/

SRCDIR           = $(srcdir)

example_DATA     =  attribs \
		   crdist.sh \
		   gafrc \
		   gnetlistrc \
		   gTAG.sch \
		   gTAG-geda.net \
		   gTAG-pcb.net \
		   README \
		   bom/gTAG.bom \
		   bom/gTAG-bom.csv \
		   ps/gTAG.ps \
		   ps/gTAG-consio.ps \
		   ps/gTAG-jtagio.ps \
		   ps/gTAG-psu.ps \
		   ps/gTAG-ucont.ps \
		   sch/gTAG-consio.sch \
		   sch/gTAG-jtagio.sch \
		   sch/gTAG-psu.sch \
		   sch/gTAG-ucont.sch \
		   sht/copyleft.sym \
		   sht/gTAG-consio.sym \
		   sht/gTAG-jtagio.sym \
		   sht/gTAG-psu.sym \
		   sht/gTAG-ucont.sym \
		   sym/24C64-gt.sym \
		   sym/7414-gt.sym \
		   sym/CY7C64603-52NC-gt.sym \
		   sym/MAX882-gt.sym \
		   sym/SN75240PW-gt.sym

EXTRA_DATA    = tests/gTAG-bom.csv tests/gTAG-geda.net bom/gTAG-bom.csv

EXTRA_DIST    = ChangeLog-1.0 $(example_DATA) $(RUN_TESTS) $(EXTRA_DATA)

RUN_TESTS     = tests/runtest.sh

check_SCRIPTS = $(SRCDIR)/${RUN_TESTS}

TESTS         = ${RUN_TESTS}

regen:
	@$(SRCDIR)/${RUN_TESTS} --regen

clean-local:
	test -d gEDA && rm -rf gEDA || : ;

.PHONY: regen

MOSTLYCLEANFILES     = *.log tests/*~ *~
CLEANFILES           = *.log tests/*~ *~ sch/*~ sht/*~ sym/*~
DISTCLEANFILES       = *.log tests/*~ *~ sch/*~ sht/*~ sym/*~
MAINTAINERCLEANFILES = *.log tests/*~ *~ sch/*~ sht/*~ sym/*~ core Makefile.in

