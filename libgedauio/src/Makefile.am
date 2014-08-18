## Process this file with automake to produce Makefile.in
#

#DEBUGGING                   = --debug --verbose

AUTOMAKE_OPTIONS             = $(GEDA_AUTO_MAKE_OPTIONS)

INC_GEDA                     = -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL                    = -I$(srcdir)/../include
INC_LIBGEDA                  = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES                = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

AM_CFLAGS                    = $(GCC_CFLAGS) $(MINGW_CFLAGS) $(GLIB_CFLAGS)  $(GTK_CFLAGS) \
	             	           $(GDK_CFLAGS) $(CAIRO_CFLAGS) $(PANGO_CFLAGS) $(GDK_PIXBUF_CFLAGS)

AM_LDFLAGS                   = -version-info $(LIBGEDAUIO_SHLIB_VERSION) $(GLIB_LIBS)     $(GTK_LIBS) \
					           $(WINDOWS_LIBTOOL_FLAGS)    $(MINGW_LDFLAGS) $(GDK_PIXBUF_LIBS)

lib_LTLIBRARIES              = libgedauio.la

libgedauio_extra_sources     = functions/geda-marshal.list

libgedauio_la_SOURCES        =

include functions/Makefile_insert
include dialogs/Makefile_insert
include widgets/Makefile_insert

libgedauio_la_CFLAGS         = $(DEBUGGING) $(GEDA_INCLUDES) $(AM_CFLAGS)

libgedauio_la_CPPFLAGS       =

libgedauio_la_LDFLAGS        = $(DEBUGGING) $(LDFLAGS) $(AM_LDFLAGS)

libgedauio_la_DEPENDENCIES   = $(top_builddir)/libgeda/src/libgeda.la

libgedauio_la_LIBADD         = $(top_builddir)/libgeda/src/libgeda.la

LIBTOOL                      = @LIBTOOL@ --silent

doxygen:
# Change to the docs directory and 'make doxygen' for libgedauio
if ENABLE_DOXYGEN
	(cd ../docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch. To generate'
	@echo 'Doxygen documentation for libgedauio, run configure with the'
	@echo '--enable-doxygen switch in the top of the source tree and try again'
endif

MOSTLYCLEANFILES           = *.log core FILE dialogs/*~ widgets/*~ *~
CLEANFILES                 = *.log core FILE dialogs/*~ widgets/*~ *~
DISTCLEANFILES             = *.log core FILE dialogs/*~ widgets/*~ *~
MAINTAINERCLEANFILES       = *.log core FILE dialogs/*~ widgets/*~ *~ Makefile.in

# Unfortunately, in order to test libtool thoroughly, we need access
# to its private directory.
objdir = `sed -n -e 's/^objdir=\"\(.*\)\"$$/\1/p' ./libtool`
