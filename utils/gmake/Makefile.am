SUFFIXES = .sh

bin_SCRIPTS    = gmake-caps-sym \
		 gmake-set-resistors \
		 gmake-transistors-sym

distcheck:

dist_bin_SCRIPTS   = gmake-caps-sym.sh \
		     gmake-set-resistors.sh \
		     gmake-transistors-sym.sh

CLEANFILES           = *.log *~  $(bin_SCRIPTS)
MOSTLYCLEANFILES     = $(CONFIG_CLEAN) $(bin_SCRIPTS)
DISTCLEANFILES       = *.log core FILE *~  $(bin_SCRIPTS)
MAINTAINERCLEANFILES = *.log *~ Makefile.in  $(bin_SCRIPTS)
