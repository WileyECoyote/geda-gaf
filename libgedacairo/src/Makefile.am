## Process this file with automake to produce Makefile.in

AUTOMAKE_OPTIONS = $(GEDA_AUTO_MAKE_OPTIONS)

INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

lib_LTLIBRARIES = libgedacairo.la

libgedacairo_la_SOURCES = edacairo.c \
			  edapangorenderer.c \
			  edarenderer.c

AM_CPPFLAGS      = $(GEDA_INCLUDES) -I$(includedir)

AM_CFLAGS  = $(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) \
	     $(GDK_CFLAGS) $(CAIRO_CFLAGS) $(PANGO_CFLAGS) $(GDK_PIXBUF_CFLAGS)

libgedacairo_la_CFLAGS = $(GCC_CFLAGS) $(AM_CFLAGS) $(GLIB_CFLAGS) $(GTK_CFLAGS)

AM_LDFLAGS = -version-info \
             $(LIBGEDACAIRO_SHLIB_VERSION) $(WINDOWS_LIBTOOL_FLAGS) \
	     $(MINGW_LDFLAGS) $(GUILE_LIBS) $(GDK_LIBS) $(GLIB_LIBS) \
	     $(CAIRO_LIBS) $(PANGO_LIBS)  $(GDK_PIXBUF_LIBS)

LIBTOOL=@LIBTOOL@ --silent

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
