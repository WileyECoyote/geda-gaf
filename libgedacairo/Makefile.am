SUBDIRS = include src

EXTRA_DIST = README ChangeLog


pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libgedacairo.pc

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- .) > $@
endif HAVE_GIT_REPO
