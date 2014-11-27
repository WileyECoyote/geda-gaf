
SUBDIRS = po docs include src

EXTRA_DIST = README ChangeLog ChangeLog-1.0

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libgedadraw.pc

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- .) > $@
endif HAVE_GIT_REPO

doxygen:

# call 'make doxygen' for libgedadraw
if ENABLE_DOXYGEN
	(cd docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

MOSTLYCLEANFILES     = *.log core FILE ./*~
CLEANFILES           = *.log core FILE ./*~
DISTCLEANFILES       = *.log core FILE ./*~ libgedadraw.pc
MAINTAINERCLEANFILES = *.log core FILE ./*~ Makefile.in libgedadraw.pc ChangeLog


