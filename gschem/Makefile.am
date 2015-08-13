# The order subdirectories are processed is important!
#
#        1) src/ must be processed before the po/ folder
#        2) data/ should be processed after the po/ folder because data
#           contains geda-gschem.desktop which needs geda-gschem.LINGUAS
#
SUBDIRS = bitmap docs examples include etc scheme scripts src \
	  po data tests

EXTRA_DIST = BUGS ChangeLog ChangeLog-1.0 icon-theme-installer

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- .) > $@
endif HAVE_GIT_REPO

doxygen:

# call 'make doxygen' for gschem
if ENABLE_DOXYGEN
	(cd docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

maintainer-clean-local:
	-rm -rf po/*.sed po/*.header po/*.sin po/*.template po/Rules-quot

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in ChangeLog
