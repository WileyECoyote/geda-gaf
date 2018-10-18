
SUBDIRS = po src include etc design docs data

EXTRA_DIST = BUGS NOTES README ChangeLog ChangeLog-1.0 ToDos

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && \
	    $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	    echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO

doxygen:

# call 'make doxygen' for gattrib
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
DISTCLEANFILES       = *.log core FILE po/*~ *~
MAINTAINERCLEANFILES = *.log core FILE po/*~ *~ Makefile.in ChangeLog
