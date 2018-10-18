
SUBDIRS = po src include docs

EXTRA_DIST = ChangeLog-1.0

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && \
	    $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	    echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO

update-gmo:
	$(MAKE) -C po update-gmo || exit 1;

update-po:
	$(MAKE) -C po update-po || exit 1;

MOSTLYCLEANFILES     = *.log core FILE po/*~ *~
CLEANFILES           = *.log core FILE po/*~ *~
DISTCLEANFILES       = *.log core FILE po/*~ *~
MAINTAINERCLEANFILES = *.log core FILE po/*~ *~ Makefile.in ChangeLog
