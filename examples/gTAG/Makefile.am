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
		   gTAG-drc2.txt \
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

EXTRA_DATA    = tests/gTAG-bom.csv tests/gTAG-geda.net

RUN_TESTS     = tests/runtest.sh

EXTRA_DIST    = ChangeLog-1.0 $(example_DATA) $(RUN_TESTS) $(EXTRA_DATA)

check_SCRIPTS = tests

tests:
	@$(SRCDIR)/${RUN_TESTS} "gTAG" $(SRCDIR)

clean-local:
	test -d gEDA && rm -rf gEDA || : ;

.PHONY: tests

MOSTLYCLEANFILES     = *.log tests/*~ *~
CLEANFILES           = *.log tests/*~ *~ sch/*~ sht/*~ sym/*~
DISTCLEANFILES       = *.log tests/*~ *~ sch/*~ sht/*~ sym/*~
MAINTAINERCLEANFILES = *.log tests/*~ *~ sch/*~ sht/*~ sym/*~ core Makefile.in

