
rcuserdir = ~/.gEDA
rcdatadir = $(GEDARCDIR)

rcdata_DATA = user-gschemrc system-gschemrc gschem-gtkrc \
	      gschem-colormap-lightbg gschem-colormap-darkbg \
	      gschem-colormap-bw gschem-colormap-custom

system-gschemrc: $(top_builddir)/configure system-gschemrc.scm
	@date_ver=$(DATE_VERSION); \
	sed -e"s:[@]DATE_VERSION@:$$date_ver:" \
	  < $(srcdir)/system-gschemrc.scm > system-gschemrc.new; \
	if diff system-gschemrc system-gschemrc.new > /dev/null 2>&1; then \
	  rm system-gschemrc.new; \
	  echo "system-gschemrc is unchanged"; \
	else \
	  echo "Recreating system-gschemrc"; \
	  mv system-gschemrc.new system-gschemrc; \
	fi

user-gschemrc: $(top_builddir)/configure user-gschemrc.scm
	@date_ver=$(DATE_VERSION); \
	sed -e"s:[@]DATE_VERSION@:$$date_ver:" \
	  < $(srcdir)/user-gschemrc.scm > user-gschemrc.new; \
	if diff user-gschemrc user-gschemrc.new > /dev/null 2>&1; then \
	  rm user-gschemrc.new; \
	  echo "user-gschemrc is unchanged"; \
	else \
	  echo "Recreating user-gschemrc"; \
	  mv user-gschemrc.new user-gschemrc; \
	fi

EXTRA_DIST = system-gschemrc.scm user-gschemrc.scm gschem-gtkrc \
             gschem-colormap-lightbg gschem-colormap-darkbg \
             gschem-colormap-bw gschem-colormap-custom \
             

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~ system-gschemrc user-gschemrc
MAINTAINERCLEANFILES = *.log core FILE *~ system-gschemrc user-gschemrc system-gnetlistrc system-gsymbolrc  Makefile.in

