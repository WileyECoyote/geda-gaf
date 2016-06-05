## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## This file is processed by automake to produce Makefile.in, after which
## The configure script generates the actual Makefile from the Makefile.in.

SUBDIRS = gTAG lightning_detector MSA-2643 TwoStageAmp

exampledir = $(docdir)/examples/

example_DATA = README

EXTRA_DIST = ChangeLog ChangeLog-1.0 $(example_DATA)

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && \
	    $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- . || \
	    echo "WARNING: ChangeLog information not available from git" >&2 ; \
	) > $@
endif HAVE_GIT_REPO

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in ChangeLog
