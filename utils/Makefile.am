## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
##
## Process this file with automake to produce Makefile.in

SUBDIRS = docs examples include gmake gsch2pdf gschemdoc gschlas gxyrs \
	      etc scripts src tests

EXTRA_DIST = ChangeLog ChangeLog.tragesym-1.0 ChangeLog.gsch2pcb-1.0 \
	         ChangeLog-1.0 AUTHORS

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- .) > $@
endif HAVE_GIT_REPO

doxygen:

# call 'make doxygen' for utils
if ENABLE_DOXYGEN
	(cd docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in ChangeLog
