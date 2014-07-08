## Process this file with automake to produce Makefile.in
SUBDIRS = docs include info scripts src module tests

pkgconfigdir    = $(libdir)/pkgconfig
pkgconfig_DATA  = libgedathon.pc
pkgconfig_DATA += geda.pc

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- .) > $@
endif HAVE_GIT_REPO

doxygen:

# call 'make doxygen' for libgedathon
if ENABLE_DOXYGEN
	(cd docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~ geda.pc libgedathon.pc
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in libgedathon.pc ChangeLog
