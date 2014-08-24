## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
##
## Process this file with automake to produce Makefile.in

bin_SCRIPTS = gschemdoc

EXTRA_DIST = 

gschemdoc: gschemdoc.sh
	sed -e 's:[@]DOCDIR@:$(docdir):' < $(srcdir)/gschemdoc.sh > tmp_gschemdoc
	chmod a+x tmp_gschemdoc
	mv -f tmp_gschemdoc gschemdoc

dist_bin_SCRIPTS   = gschemdoc.sh

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~ gschemdoc
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in gschemdoc

