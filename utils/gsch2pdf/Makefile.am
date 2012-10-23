## Process this file with automake to produce Makefile.in

INC_GEDA = -I$(top_srcdir) -I$(abs_top_builddir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

rcdatadir = $(GEDARCDIR)

bin_PROGRAMS = gsch2pdf

rcdata_DATA = system-gsch2pdfrc

# don't forget all *.h files */
gsch2pdf_SOURCES = \
	main.c \
	junction.c \
	rc-config.c \
	print-settings.c

gsch2pdf_CPPFLAGS = $(GEDA_INCLUDES)

gsch2pdf_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLIGS) $(GLIB_CFLAGS) \
	$(GUILE_CFLAGS) $(PANGOCAIRO_CFLAGS) $(GDK_PIXBUF_CFLAGS)

gsch2pdf_LDFLAGS = $(GLIB_LIBS) $(GUILE_LIBS) $(PANGOCAIRO_LIBS) $(GDK_PIXBUF_LIBS)

gsch2pdf_LDADD = $(top_builddir)/libgeda/src/libgeda.la

MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in

