## $Id$ indent-tabs-mode: t; tab-width: 4
## Process this file with automake to produce Makefile.in
# Currently not using the CAIRO_CFLAGS or PANGO_CFLAGS but libgedathon is still
# in infancy, leaving for now. GDK_CFLAGS added due to inclusion of GDK_PIXBUF
# _CFLAGS, which are include because libgedathon includes headers for libgeda,
# whose proto-type include GdkPixbuf.

#REF: (geda-python.m4).PYTHON_CFLAGS  = -Wstrict-prototypes -pthread -fno-strict-aliasing -fwrapv -fPIC
#REF: (geda-python.m4).PYTHON_LDFLAGS = -shared -Bsymbolic-functions
#REF: (geda-python.m4).PYTHON_LIBS    = -lpython$PYTHON_VERSION

#DEBUGGING               = --debug --verbose

INC_GEDA                 = -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL                = -I$(srcdir)/../include
INC_LIBGEDA              = -I$(top_srcdir)/libgeda/include
INC_LIBCOLOR             = -I$(top_srcdir)/libgedacolor/include

LOCALE_DIR               = -DLOCALEDIR=\"$(localedir)\"

LOG_DOMAIN               = -DG_LOG_DOMAIN=\"libgedathon\"

GEDA_INCLUDES            = $(LOG_DOMAIN) $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA) $(INC_LIBCOLOR)

PY_INCLUDES              = $(PYTHON_CFLAGS) $(PYTHON_IFLAGS)

PY_LIBFLAGS              = $(PYTHON_LDFLAGS) $(PYTHON_LIBS) -export-symbols-regex initialize

AM_CFLAGS                = $(GCC_CFLAGS) $(MINGW_CFLAGS) $(GLIB_CFLAGS) \
						   $(GDK_CFLAGS) $(CAIRO_CFLAGS) $(PANGO_CFLAGS) $(GDK_PIXBUF_CFLAGS)

AM_LDFLAGS               = $(WINDOWS_LIBTOOL_FLAGS) -version-info $(LIBGEDATHON_SHLIB_VERSION) \
						   $(GLIB_LIBS) $(MINGW_LDFLAGS) $(GDK_PIXBUF_LIBS)

lib_LTLIBRARIES          = libgedathon.la

libgedathon_la_SOURCES   = update_objects.c geda_capsule.c libgedathon.c

libgedathon_la_CFLAGS    = $(DEBUGGING) $(LOCALE_DIR) $(GEDA_INCLUDES) $(AM_CFLAGS) $(PY_INCLUDES)

#libgedathon_la_CPPFLAGS  = $(PYTHON_CFLAGS) $(PYTHON_IFLAGS)

libgedathon_la_LDFLAGS   = $(DEBUGGING) $(PY_LIBFLAGS) $(AM_LDFLAGS) $(LDFLAGS)

libgedathon_la_DEPENDENCIES = $(top_builddir)/libgeda/src/libgeda.la

libgedathon_la_LIBADD       = $(top_builddir)/libgeda/src/libgeda.la

LIBTOOL                     = @LIBTOOL@ --silent

doxygen:
# Change to the docs directory and 'make doxygen' for libgedathon
if ENABLE_DOXYGEN
	(cd ../docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch. To generate'
	@echo 'Doxygen documentation for libgedathon, run configure with the'
	@echo '--enable-doxygen switch in the top of the source tree and try again'
endif

MOSTLYCLEANFILES         = *.log core FILE *~
CLEANFILES               = *.log core FILE *~
DISTCLEANFILES           = *.log core FILE *~
MAINTAINERCLEANFILES     = *.log core FILE *~ Makefile.in

# Unfortunately, in order to test libtool thoroughly, we need access
# to its private directory.
objdir = `sed -n -e 's/^objdir=\"\(.*\)\"$$/\1/p' ./libtool`
