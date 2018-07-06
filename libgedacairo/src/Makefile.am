## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## Process this file with automake to produce Makefile.in

#AUTOMAKE_OPTIONS = 1.6 subdir-objects

INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include

LOG_DOMAIN      = -DG_LOG_DOMAIN=\"libgedacairo\"

GEDA_INCLUDES   = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

AM_CPPFLAGS = $(LOG_DOMAIN) $(GEDA_INCLUDES) -I$(includedir)

AM_CFLAGS   = $(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) \
	          $(GDK_CFLAGS) $(CAIRO_CFLAGS) $(PANGO_CFLAGS) $(GDK_PIXBUF_CFLAGS)

AM_LDFLAGS = $(WINDOWS_LIBTOOL_FLAGS) -version-info $(LIBGEDACAIRO_SHLIB_VERSION) \
			 $(MINGW_LDFLAGS) $(GUILE_LIBS) $(GLIB_LIBS) $(CAIRO_LIBS) \
			 $(PANGO_LIBS)    $(GDK_LIBS)   $(GDK_PIXBUF_LIBS)

lib_LTLIBRARIES = libgedacairo.la

libgedacairo_la_SOURCES = \
				edacairo.c \
				edapangorenderer.c \
				edarenderer.c

libgedacairo_la_CFLAGS = $(GCC_CFLAGS) $(AM_CFLAGS) $(GLIB_CFLAGS) $(GTK_CFLAGS)

libgedacairo_la_LDFLAGS       = $(AM_LDFLAGS) $(LDFLAGS)

libgedacairo_la_DEPENDENCIES  = $(top_builddir)/libgeda/src/libgeda.la

libgedacairo_la_LIBADD        = $(top_builddir)/libgeda/src/libgeda.la

LIBTOOL=@LIBTOOL@ --silent

doxygen:
# Change to the docs directory and 'make doxygen' for libgedacairo
if ENABLE_DOXYGEN
	(cd ../docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch. To generate'
	@echo 'Doxygen documentation for libgedacairo, run configure with the'
	@echo '--enable-doxygen switch in the top of the source tree and try again'
endif

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
