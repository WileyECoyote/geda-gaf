
symbol_libs =@symbol_dirs@

gafrcddir = $(GEDADATADIR)/gafrc.d
dist_gafrcd_DATA = geda-clib.scm

datasymdir = $(GEDADATADIR)/sym
nobase_dist_datasym_DATA = \
	vhdl/verilog2vhdl.sh vhdl/script.sed

EXTRA_DIST = ChangeLog ChangeLog-1.0 \
	     radio/README AUTHORS

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- .) > $@
endif HAVE_GIT_REPO

# Copies symbols into distribution
dist-hook:
	@for lib in $(symbol_libs); do \
	  $(MKDIR_P) $(distdir)/$$lib && \
	  list=`(ls $(srcdir)/$$lib) | grep '.[Ss][Yy][Mm]$$'`; \
	  for file in $$list; do \
	    cp -p $(srcdir)/$$lib/$$file $(distdir)/$$lib || exit $$?; \
	  done; \
	done;

# Installs symbols

install-data-local:
	@$(NORMAL_INSTALL)
	@for lib in $(symbol_libs); do \
	  d="$(DESTDIR)$(datasymdir)/$$lib"; \
	  $(MKDIR_P) "$$d"; \
	  list=`(ls $(srcdir)/$$lib) | grep '.[Ss][Yy][Mm]$$'`; \
	  if test -n "$$list"; then \
	    (cd $(srcdir)/$$lib && $(INSTALL_DATA) $$list "$$d") || exit $$?; \
	  fi; \
	done

# Uninstalls symbols
uninstall-local:
	@$(NORMAL_UNINSTALL)
	@for lib in $(symbo_libs); do \
	  d="$(DESTDIR)$(datasymdir)/$$lib"; \
	  list=`(ls $(srcdir)/$$lib) | grep '\.[Ss][Yy][Mm]$$'`; \
	  if test -n "$$list"; then \
	    echo " ( cd '$$d' && rm -f" $$list ")"; \
	    (cd "$$d" && rm -f $$list) || exit $$?; \
	  fi; \
	done

# Since there are no makefile in the symbol subfolders, each must be include here
# with ./*/*~
MOSTLYCLEANFILES = *.log *~ ./*/*~
CLEANFILES = *.log *~ ./*/*~
DISTCLEANFILES = *.log core FILE *~ ./*/*~
MAINTAINERCLEANFILES = *.log *~ ./*/*~ Makefile.in ChangeLog
