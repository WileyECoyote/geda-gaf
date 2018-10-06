
SUBDIRS = po docs include etc src tests

EXTRA_DIST = BUGS README ChangeLog ChangeLog-1.0 po/domain.mak.in

pkgconfigdir         = $(libdir)/pkgconfig
pkgconfig_DATA       = libgedacolor.pc

libgedacolor-pc-install: libgedacolor.pc
		    $(mkinstalldirs) $(DESTDIR)$(pkgconfigdir)
		    $(INSTALL_DATA) libgedacolor.pc $(DESTDIR)$(pkgconfigdir)

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && \
	    $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	    echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO

doxygen:

# call 'make doxygen' for libgedacolor
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

MOSTLYCLEANFILES     = *.log core FILE po/*~ *~
CLEANFILES           = *.log core FILE po/*~ *~
DISTCLEANFILES       = *.log core FILE po/*~ *~ libgedacolor.pc po/domain.mak
MAINTAINERCLEANFILES = *.log core FILE po/*~ *~ Makefile.in libgedacolor.pc ChangeLog
