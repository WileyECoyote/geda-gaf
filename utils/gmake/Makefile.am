## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
##
## Process this file with automake to produce Makefile.in

SUFFIXES = .sh

bin_SCRIPTS    = gmake-caps-sym \
				 gmake-set-resistors \
				 gmake-transistors-sym

distcheck:

dist_bin_SCRIPTS   = gmake-caps-sym.sh \
					 gmake-set-resistors.sh \
					 gmake-transistors-sym.sh

MOSTLYCLEANFILES     = *.log core FILE *~ $(bin_SCRIPTS) $(CONFIG_CLEAN)
CLEANFILES           = *.log core FILE *~ $(bin_SCRIPTS)
DISTCLEANFILES       = *.log core FILE *~ $(bin_SCRIPTS)
MAINTAINERCLEANFILES = *.log core FILE *~ $(bin_SCRIPTS) Makefile.in

