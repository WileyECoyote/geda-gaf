## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## Process this file with automake to produce Makefile.in

#AUTOMAKE_OPTIONS = 1.6 subdir-objects

# Build a libtool library for installation in libdir.
INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include/libgeda
INC_LIBCOLOR = -I$(top_srcdir)/libgedacolor/include

GEDA_INCLUDES   = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA) $(INC_LIBCOLOR)

LOG_DOMAIN      = -DG_LOG_DOMAIN=\"libgeda\"

lib_LTLIBRARIES = libgeda.la

BUILT_SOURCES   = \
		scheme/scheme_smob.x \
		scheme/scheme_toplevel.x \
		scheme/scheme_make.x \
		scheme/scheme_object.x \
		scheme/scheme_complex.x \
		scheme/scheme_page.x \
		scheme/scheme_attrib.x \
		scheme/scheme_os.x \
		scheme/scheme_config.x \
		scheme/scheme_closure.x \
		scheme/scheme_deprecated.x \
		scheme/scheme_log.x \
		scheme/scheme_version.x

libgeda_la_SOURCES =

# Include partial makefiles from subdirectories
include file/Makefile_insert
include geda/Makefile_insert
include guile/Makefile_insert
include iface/Makefile_insert
include math/Makefile_insert
include object/Makefile_insert
include scheme/Makefile_insert
include struct/Makefile_insert
include utility/Makefile_insert
include Makefile_insert

# Probably not the correct way but all's fair in Love, War and Distchecks
EXTRA_DIST = $(BUILT_SOURCES)

AM_CFLAGS            = $(LOG_DOMAIN) $(GEDA_INCLUDES) -I$(includedir)

libgeda_la_CPPFLAGS  = -DLOCALEDIR=\"$(localedir)\"

libgeda_la_CFLAGS    = $(AM_CFLAGS)   $(GUILE_CFLAGS) $(GCC_CFLAGS)   \
					   $(GLIB_CFLAGS) $(MINGW_CFLAGS) $(GDK_PIXBUF_CFLAGS)

libgeda_la_LDFLAGS   = $(WINDOWS_LIBTOOL_FLAGS) -version-info $(LIBGEDA_SHLIB_VERSION) \
					   $(MINGW_LDFLAGS) $(GUILE_LIBS) \
					   $(GLIB_LIBS) $(GIO_LIBS) $(GDK_PIXBUF_LIBS)

if LOCAL_LIBINTL

libgeda_la_LIBADD    = $(top_srcdir)/intl/libintl.a

endif

LIBTOOL=@LIBTOOL@ --silent

# This is used to generate boilerplate for defining edascm functions in C.
SUFFIXES = .x
snarf_cpp_opts       = $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
			$(libgeda_la_CPPFLAGS) $(libgeda_la_CFLAGS) $(CPPFLAGS)
.c.x:
	@CPP="$(CPP)" $(GUILE_SNARF) -o $@ $< $(snarf_cpp_opts)

doxygen:
# Change to the docs directory and 'make doxygen' for libgeda
if ENABLE_DOXYGEN
	(cd ../docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch. To generate'
	@echo 'Doxygen documentation for libgeda, run configure with --enable-doxygen'
	@echo 'in the top of the source tree and then try again'
endif

CLEAN_DIRS = edascm/*~ file/*~ geda/*~ guile/*~ iface/*~ math/*~ object/*~ \
             scheme/*~ struct/*~ utility/*~

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~ $(CLEAN_DIRS) $(BUILT_SOURCES)
DISTCLEANFILES       = *.log core FILE *~ $(CLEAN_DIRS)
MAINTAINERCLEANFILES = *.log core FILE *~ $(CLEAN_DIRS) Makefile.in

edascm/clean:
			@rm -rf edascm/*~
file/clean:
			@rm -rf file/*~
geda/clean:
			@rm -rf geda/*~
guile/clean:
			@rm -rf guile/*~
iface/clean:
			@rm -rf iface/*~
math/clean:
			@rm -rf math/*~
object/clean:
			@rm -rf object/*~
scheme/clean:
			@rm -rf scheme/*~
struct/clean:
			@rm -rf struct/*~
utility/clean:
			@rm -rf utility/*~

# Unfortunately, in order to test libtool thoroughly, we need access
# to its private directory.
objdir = `sed -n -e 's/^objdir=\"\(.*\)\"$$/\1/p' ./libtool`
