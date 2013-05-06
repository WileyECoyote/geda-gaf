bin_SCRIPTS = gschemdoc

EXTRA_DIST = 

gschemdoc: gschemdoc.sh
	sed -e 's:[@]DOCDIR@:$(docdir):' < $(srcdir)/gschemdoc.sh > tmp_gschemdoc
	chmod a+x tmp_gschemdoc
	mv -f tmp_gschemdoc gschemdoc

dist_bin_SCRIPTS   = gschemdoc.sh

MOSTLYCLEANFILES =	*.log *~
CLEANFILES = 		*.log *~
DISTCLEANFILES = 	*.log core FILE *~ gschemdoc
MAINTAINERCLEANFILES = 	*.log *~ Makefile.in gschemdoc
