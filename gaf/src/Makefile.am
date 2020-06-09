## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## This file is processed by automake to produce Makefile.in, after which
## The configure script generates the actual Makefile from the Makefile.in.

INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include
INC_LIBCAIRO = -I$(top_srcdir)/libgedacairo/include
INC_LIBCOLOR = -I$(top_srcdir)/libgedacolor/include

GEDA_INCLUDES = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA) $(INC_LIBCAIRO) \
                $(INC_LIBCOLOR)

bin_PROGRAMS  = gaf

BUILT_SOURCES = shell.x

EXTRA_DIST    = $(BUILT_SOURCES)

gaf_SOURCES   = gaf.c \
				gaf_getopt.c \
				config.c \
				export.c \
				path.c \
				shell.c

AM_CFLAGS     = $(GEDA_INCLUDES) -I$(includedir)

gaf_CPPFLAGS  =

gaf_CFLAGS    = $(AM_CFLAGS) $(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) \
				$(GTK_CFLAGS) $(CAIRO_CFLAGS) $(CAIRO_PNG_CFLAGS) \
				$(CAIRO_PDF_CLAGS) $(CAIRO_PS_CFLAGS) $(CAIRO_SVG_CFLAGS)

gaf_LDFLAGS   = $(GUILE_LIBS) $(GTK_LIBS) $(CAIRO_LIBS) $(CAIRO_PNG_LIBS) \
				$(CAIRO_PDF_CLAGS) $(CAIRO_PS_LIBS) $(CAIRO_SVG_LIBS)

gaf_LDADD     = $(top_builddir)/libgedacairo/src/libgedacairo.la \
				$(top_builddir)/libgedacolor/src/libgedacolor.la \
				$(top_builddir)/libgeda/src/libgeda.la \
				@LIBINTL@

DEFS		  = -DLOCALEDIR=\"$(localedir)\"  @DEFS@

# This is used to generate boilerplate for defining Scheme functions
# in C.
snarf_cpp_opts = $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) \
		 $(gaf_CPPFLAGS) $(AM_CFLAGS) $(gaf_CFLAGS) $(CPPFLAGS)
.c.x:
	@CPP="$(CPP)" $(GUILE_SNARF) -o $@ $< $(snarf_cpp_opts)

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~  $(BUILT_SOURCES)
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~  Makefile.in

