## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## This file is processed by automake to produce Makefile.in, after which
## The configure script generates the actual Makefile from the Makefile.in.

templatedir = $(GEDATEMPLATEDIR)

template_DATA = schematic.sch symbol.sym

schematic.sch: $(top_builddir)/configure schematic.sch.in
	@date_ver=$(DATE_VERSION); \
	sed -e"s:[@]DATE_VERSION@:$$date_ver:" \
	  < $(srcdir)/schematic.sch.in > schematic.sch;

symbol.sym: $(top_builddir)/configure symbol.sym.in
	@date_ver=$(DATE_VERSION); \
	sed -e"s:[@]DATE_VERSION@:$$date_ver:" \
	  < $(srcdir)/symbol.sym.in > symbol.sym;

EXTRA_DIST = schematic.sch.in symbol.sym.in

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~ schematic.sch symbol.sym
MAINTAINERCLEANFILES = *.log core FILE *~ schematic.sch symbol.sym Makefile.in
