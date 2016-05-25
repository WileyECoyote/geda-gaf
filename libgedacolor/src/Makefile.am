## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## Process this file with automake to produce Makefile.in

# Build a libtool library for installation in libdir.
INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES    = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

LOG_DOMAIN      = -DG_LOG_DOMAIN=\"libgedacolor\"

lib_LTLIBRARIES  = libgedacolor.la

libgedacolor_la_SOURCES =

# Include partial makefiles from subdirectories
include globals/Makefile_insert
include guile/Makefile_insert
include get/Makefile_insert
include key/Makefile_insert
include struct/Makefile_insert
include utility/Makefile_insert
include x11/Makefile_insert
include Makefile_insert

AM_CFLAGS                 = $(GEDA_INCLUDES) -I$(includedir)

libgedacolor_la_CPPFLAGS  = $(LOG_DOMAIN) -DLOCALEDIR=\"$(localedir)\" $(DATADIR_DEFS)

libgedacolor_la_CFLAGS    = $(AM_CFLAGS)    $(GCC_CFLAGS) $(GUILE_CFLAGS) \
			   $(GLIB_CFLAGS)  $(GTK_CFLAGS) $(GDK_PIXBUF_CFLAGS) \
			   $(MINGW_CFLAGS)

libgedacolor_la_LDFLAGS   = -version-info $(LIBGEDACOLOR_SHLIB_VERSION) \
			   $(GLIB_LIBS) $(GTK_LIBS) $(GUILE_LIBS) $(GDK_PIXBUF_LIBS) \
			   $(WINDOWS_LIBTOOL_FLAGS) $(MINGW_LDFLAGS)

libgedacolor_la_LIBADD    = $(top_builddir)/libgeda/src/libgeda.la

LIBTOOL=@LIBTOOL@ --silent

doxygen:
# Change to the docs directory and 'make doxygen' for libgedacolor
if ENABLE_DOXYGEN
	(cd ../docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch. To generate'
	@echo 'Doxygen documentation for libgedacolor, run configure with --enable-doxygen'
	@echo 'in the top of the source tree and then try again'
endif

CLEAN_DIRS = get/*~ globals/*~ guile/*~ key/*~ \ struct/*~ utility/*~ x11/*~

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~ $(CLEAN_DIRS) $(BUILT_SOURCES)
DISTCLEANFILES       = *.log core FILE *~ $(CLEAN_DIRS)
MAINTAINERCLEANFILES = *.log core FILE *~ $(CLEAN_DIRS) Makefile.in

get/clean:
			@rm -rf get/*~
globals/clean:
			@rm -rf globals/*~
guile/clean:
			@rm -rf guile/*~
key/clean:
			@rm -rf key/*~
struct/clean:
			@rm -rf struct/*~
utility/clean:
			@rm -rf utility/*~
x11/clean:
			@rm -rf x11/*~

# Unfortunately, in order to test libtool thoroughly, we need access
# to its private directory.
objdir = `sed -n -e 's/^objdir=\"\(.*\)\"$$/\1/p' ./libtool`
