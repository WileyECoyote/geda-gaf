## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## This file is processed by automake to produce Makefile.in, after which
## The configure script generates the actual Makefile from the Makefile.in.

rcuserdir = ~/.gEDA
rcdatadir = $(GEDARCDIR)

rcdata_DATA = user-gschemrc system-gschemrc gschem-gtkrc

system-gschemrc: $(top_builddir)/configure system-gschemrc.scm
	@date_ver=$(DATE_VERSION); \
	sed -e"s:[@]DATE_VERSION@:$$date_ver:" \
	  < $(srcdir)/system-gschemrc.scm > system-gschemrc.new; \
	if diff system-gschemrc system-gschemrc.new > /dev/null 2>&1; then \
	  rm system-gschemrc.new; \
	  echo "system-gschemrc is unchanged"; \
	else \
	  echo "Recreating system-gschemrc"; \
	  mv -f system-gschemrc.new system-gschemrc; \
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
	  mv -f user-gschemrc.new user-gschemrc; \
	fi

EXTRA_DIST = system-gschemrc.scm user-gschemrc.scm gschem-gtkrc

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~ system-gschemrc user-gschemrc
MAINTAINERCLEANFILES = *.log core FILE *~ system-gschemrc user-gschemrc Makefile.in

