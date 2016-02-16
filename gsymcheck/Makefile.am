
SUBDIRS = po src include etc docs tests

EXTRA_DIST = BUGS ChangeLog ChangeLog-1.0

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- .) > $@
endif HAVE_GIT_REPO

doxygen:

# call 'make doxygen' for gsymcheck
if ENABLE_DOXYGEN
	(cd docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

update-po:
	$(MAKE) -C po update-po || exit 1;

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in ChangeLog
