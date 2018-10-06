## Process this file with automake to produce Makefile.in
SUBDIRS = po docs include info scripts src module pymodules tests

EXTRA_DIST = README ChangeLog ChangeLog-1.0 po/domain.mak.in

pkgconfigdir    = $(libdir)/pkgconfig
pkgconfig_DATA  = libgedathon.pc
pkgconfig_DATA += geda.pc

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && \
	    $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	    echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO

doxygen:

# call 'make doxygen' for libgedathon
if ENABLE_DOXYGEN
	(cd docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

update-gmo:
	$(MAKE) -C po update-gmo || exit 1;

update-po:
	$(MAKE) -C po update-po || exit 1;

maintainer-clean-local:
	-rm -rf po/*.sed po/*.header po/*.sin po/*.template po/Rules-quot

MOSTLYCLEANFILES     = *.log core FILE po/*~ *~
CLEANFILES           = *.log core FILE po/*~ *~
DISTCLEANFILES       = *.log core FILE po/*~ *~ geda.pc libgedathon.pc po/domain.mak
MAINTAINERCLEANFILES = *.log core FILE po/*~ *~ Makefile.in libgedathon.pc ChangeLog

