
SUBDIRS = po src include docs

EXTRA_DIST = ChangeLog-1.0

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- .) > $@
endif HAVE_GIT_REPO

update-po:
	$(MAKE) -C po update-po || exit 1;

MOSTLYCLEANFILES      = *.log core FILE *~
CLEANFILES             = *.log core FILE *~
DISTCLEANFILES        = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in ChangeLog
