
SUFFIXES = .sh

bin_SCRIPTS = 	bompp \
		bom_xref \
		annotate \
		unannotate \
		sch2eaglepos

EXTRA_DIST = $(bin_SCRIPTS)

CLEANFILES = *.log *~  $(bin_SCRIPTS)

MOSTLYCLEANFILES = $(CONFIG_CLEAN) $(bin_SCRIPTS)

DISTCLEANFILES = *.log core FILE *~  $(bin_SCRIPTS)

MAINTAINERCLEANFILES = *.log *~ Makefile.in  $(bin_SCRIPTS)
