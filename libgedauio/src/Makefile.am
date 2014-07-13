## Process this file with automake to produce Makefile.in
#

#DEBUGGING                 = --debug --verbose

AUTOMAKE_OPTIONS           = $(GEDA_AUTO_MAKE_OPTIONS)

INC_GEDA                   = -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL                  = -I$(srcdir)/../include

GEDA_INCLUDES              = $(INC_GEDA) $(INC_LOCAL)

AM_CFLAGS                  = $(GCC_CFLAGS) $(MINGW_CFLAGS) $(GLIB_CFLAGS)  $(GTK_CFLAGS) \
	             	     $(GDK_CFLAGS) $(CAIRO_CFLAGS) $(PANGO_CFLAGS) $(GDK_PIXBUF_CFLAGS)

AM_LDFLAGS                 = -version-info $(LIBGEDAUIO_SHLIB_VERSION) $(GLIB_LIBS)  $(GTK_LIBS) \
		           	$(MINGW_LDFLAGS) $(WINDOWS_LIBTOOL_FLAGS) $(GDK_PIXBUF_LIBS)

lib_LTLIBRARIES            = libgedauio.la

libgedauio_la_SOURCES      =

include dialogs/Makefile_insert
include widgets/Makefile_insert

libgedauio_la_CFLAGS       = $(DEBUGGING) $(GEDA_INCLUDES) $(AM_CFLAGS)

libgedauio_la_CPPFLAGS     =

libgedauio_la_LDFLAGS      = $(DEBUGGING) $(LDFLAGS) $(AM_LDFLAGS)

libgedauio_la_DEPENDENCIES =

LIBTOOL                    = @LIBTOOL@ --silent

MOSTLYCLEANFILES           = *.log core FILE dialogs/*~ widgets/*~ *~
CLEANFILES                 = *.log core FILE dialogs/*~ widgets/*~ *~
DISTCLEANFILES             = *.log core FILE dialogs/*~ widgets/*~ *~
MAINTAINERCLEANFILES       = *.log core FILE dialogs/*~ widgets/*~ *~ Makefile.in

# Unfortunately, in order to test libtool thoroughly, we need access
# to its private directory.
objdir = `sed -n -e 's/^objdir=\"\(.*\)\"$$/\1/p' ./libtool`
