## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
##
## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = $(GEDA_AUTO_MAKE_OPTIONS)

INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES 	= $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

bin_PROGRAMS	= gsch2pcb

AM_CFLAGS	= $(GEDA_INCLUDES) $(GCC_CFLAGS) $(MINGW_CFLAGS) \
		  $(GLIB_CFLAGS)

gsch2pcb_SOURCES   = gsch2pcb.c
gsch2pcb_CPPFLAGS  = -DPCBDATADIR=\"$(PCBDATADIR)\" \
		     -DPCBLIBPATH=\"$(PCBLIBPATH)\"
gsch2pcb_CFLAGS    = $(AM_CFLAGS)
gsch2pcb_LDFLAGS   = $(GLIB_LIBS)
gsch2pcb_LDADD     = $(top_builddir)/libgeda/src/libgeda.la

doxygen:
# Change to the docs directory and 'make doxygen' for geda utilities
if ENABLE_DOXYGEN
	(cd ../docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch. To generate'
	@echo 'Doxygen documentation for geda utilities, run configure with the'
	@echo '--enable-doxygen switch in the top of the source tree and try again'
endif

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~ $(BUILT_SOURCES)
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
