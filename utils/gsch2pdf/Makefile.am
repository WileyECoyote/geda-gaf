## Process this file with automake to produce Makefile.in
AUTOMAKE_OPTIONS = 1.7 subdir-objects

INC_GEDA = -I$(top_srcdir) -I$(abs_top_builddir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES  = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

rcdatadir 	 = $(GEDARCDIR)

bin_PROGRAMS 	 = gsch2pdf

rcdata_DATA 	 = system-gsch2pdfrc

gsch2pdf_SOURCES = main.c \
		   junction.c \
		   rc-config.c \
		   print-settings.c

AM_CFLAGS	     = $(GEDA_INCLUDES) -I$(includedir)

gsch2pdf_CPPFLAGS    =

gsch2pdf_CFLAGS      = $(AM_CFLAGS)         $(GCC_CFLAGS)  $(GLIB_CFLAGS) \
		       $(MINGW_CFLAGS)      $(GUILE_CFLAGS) \
	 	       $(GDK_PIXBUF_CFLAGS) $(PANGOCAIRO_CFLAGS)

gsch2pdf_LDFLAGS     = $(GLIB_LIBS) $(GUILE_LIBS) $(PANGOCAIRO_LIBS) $(GDK_PIXBUF_LIBS)

gsch2pdf_LDADD       = $(top_builddir)/libgeda/src/libgeda.la

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in

