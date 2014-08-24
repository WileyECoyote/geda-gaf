## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
##
## Process this file with automake to produce Makefile.in

bin_SCRIPTS = gxyrs

rcdatadir   = @GEDADATADIR@/perl/lib
rcdata_DATA = gxyrs.pm

dist_bin_SCRIPTS  = gxyrs.pl \
					gxyrs.pm

all: gxyrs

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

##  This hardcodes the path to the
##  system libraries into geda_filter prior to installation.
gxyrs: $(srcdir)/gxyrs.pl
	sed -e "s#GEDADATADIR#$(GEDADATADIR)#" $(srcdir)/gxyrs.pl | sed -e "s#GEDARCDIR#$(GEDARCDIR)#" > tmp$@
	chmod a+x tmp$@
	mv -f tmp$@ $@

MOSTLYCLEANFILES     = *.log *.ps core FILE *~
CLEANFILES           = *.log *.ps core FILE *~
DISTCLEANFILES       = *.log *.ps core FILE *~ gxyrs
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ gxyrs Makefile.in

