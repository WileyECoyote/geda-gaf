## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## Process this file with automake to produce Makefile.in
## `pkg-config --cflags freetype2`

#AUTOMAKE_OPTIONS             = 1.6 subdir-objects

#DEBUGGING                    = --debug --verbose

INC_GEDA                     = -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL                    = -I$(srcdir)/../include
INC_LIBGEDA                  = -I$(top_srcdir)/libgeda/include
INC_LIBCOLOR                 = -I$(top_srcdir)/libgedacolor/include

LOG_DOMAIN                   = -DG_LOG_DOMAIN=\"libgedadraw\"

GEDA_INCLUDES                = $(LOG_DOMAIN) $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA) $(INC_LIBCOLOR)

AM_CPPFLAGS                  = $(DEBUGGING) $(MINGW_CFLAGS) $(GLIB_CFLAGS)  $(GTK_CFLAGS) \
	                           $(GDK_CFLAGS)   $(CAIRO_CFLAGS) $(PANGO_CFLAGS) $(GDK_PIXBUF_CFLAGS)

AM_LDFLAGS                   = $(DEBUGGING) $(WINDOWS_LIBTOOL_FLAGS) -version-info $(LIBGEDADRAW_SHLIB_VERSION) \
								$(GLIB_LIBS) $(X11_LIBS) $(CAIRO_LIBS) $(MINGW_LDFLAGS) $(GDK_PIXBUF_LIBS)

lib_LTLIBRARIES              = libgedadraw.la

libgedadraw_la_SOURCES       =

if OS_IS_LINUX
include x11/Makefile_insert
endif

if OS_IS_MINGW
include win32/Makefile_insert
else
	@echo 'MINGW is not the OS'
endif

libgedadraw_la_CPPFLAGS      = $(DEBUGGING) $(GEDA_INCLUDES) $(AM_CPPFLAGS)

libgedadraw_la_LDFLAGS       = $(DEBUGGING) $(LDFLAGS) $(AM_LDFLAGS)

libgedadraw_la_DEPENDENCIES  = $(top_builddir)/libgeda/src/libgeda.la

libgedadraw_la_LIBADD        = $(top_builddir)/libgeda/src/libgeda.la

LIBTOOL                      =@LIBTOOL@ --silent

doxygen:
# Change to the docs directory and 'make doxygen' for libgedadraw
if ENABLE_DOXYGEN
	(cd ../docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch. To generate'
	@echo 'Doxygen documentation for libgedadraw, run configure with the'
	@echo '--enable-doxygen switch in the top of the source tree and try again'
endif

MOSTLYCLEANFILES           = *.log core FILE x11/*~ *~
CLEANFILES                 = *.log core FILE x11/*~ *~
DISTCLEANFILES             = *.log core FILE x11/*~ *~
MAINTAINERCLEANFILES       = *.log core FILE x11/*~ *~ Makefile.in

# Unfortunately, in order to test libtool thoroughly, we need access
# to its private directory.
objdir = `sed -n -e 's/^objdir=\"\(.*\)\"$$/\1/p' ./libtool`
