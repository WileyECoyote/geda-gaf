## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
##
## Process this file with automake to produce Makefile.in

INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES  = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

rcdatadir 	     = $(GEDARCDIR)

bin_PROGRAMS 	 = gsch2pdf

rcdata_DATA 	 = system-gsch2pdfrc

local_HEADERs    = junction.h \
				   rc-config.h \
				   print-settings.h

EXTRA_DIST       = $(rcdata_DATA) $(local_HEADERs)

gsch2pdf_SOURCES = gsch2pdf.c \
				   junction.c \
				   rc-config.c \
				   parsecmd.c \
				   print-settings.c

AM_CFLAGS	     = $(GEDA_INCLUDES) -I$(includedir)

gsch2pdf_CPPFLAGS    = -DLOCALEDIR=\"$(localedir)\"

gsch2pdf_CFLAGS      = $(AM_CFLAGS)         $(GCC_CFLAGS)  $(GLIB_CFLAGS) \
					   $(MINGW_CFLAGS)      $(GUILE_CFLAGS) \
			 	       $(PANGOCAIRO_CFLAGS)

gsch2pdf_LDFLAGS     = $(GLIB_LIBS) $(GUILE_LIBS) $(PANGOCAIRO_LIBS)

gsch2pdf_LDADD       = $(top_builddir)/libgeda/src/libgeda.la @LIBINTL@

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
