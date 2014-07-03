## Process this file with automake to produce Makefile.in
AUTOMAKE_OPTIONS = $(GEDA_AUTO_MAKE_OPTIONS)

# Build a libtool library for installation in libdir.
INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include/libgeda

GEDA_INCLUDES = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

lib_LTLIBRARIES = libgeda.la

BUILT_SOURCES = \
	scheme/scheme_init.x \
	scheme/scheme_smob.x \
	scheme/scheme_toplevel.x \
	scheme/scheme_object.x \
	scheme/scheme_complex.x \
	scheme/scheme_color.x \
	scheme/scheme_page.x \
	scheme/scheme_attrib.x \
	scheme/scheme_os.x \
	scheme/scheme_config.x \
	scheme/scheme_closure.x \
	scheme/scheme_deprecated.x

libgeda_la_SOURCES =

# Include partial makefiles from subdirectories
include scheme/Makefile_insert
include file/Makefile_insert
include geda/Makefile_insert
include guile/Makefile_insert
include iface/Makefile_insert
include math/Makefile_insert
include object/Makefile_insert
include struct/Makefile_insert
include utility/Makefile_insert
include Makefile_insert

# Probably not the correct way but all's fair in Love, War and Distchecks
EXTRA_DIST = $(BUILT_SOURCES)

AM_CFLAGS            = $(GEDA_INCLUDES) -I$(includedir)

libgeda_la_CPPFLAGS  = -DLOCALEDIR=\"$(localedir)\" $(DATADIR_DEFS)

libgeda_la_CFLAGS    = $(AM_CFLAGS)   $(GUILE_CFLAGS) $(GCC_CFLAGS)   \
	               $(GLIB_CFLAGS) $(MINGW_CFLAGS) $(GDK_PIXBUF_CFLAGS)

libgeda_la_LDFLAGS   = -version-info $(LIBGEDA_SHLIB_VERSION) \
		       $(WINDOWS_LIBTOOL_FLAGS) $(MINGW_LDFLAGS) $(GUILE_LIBS) \
		       $(GLIB_LIBS) $(GDK_PIXBUF_LIBS)

LIBTOOL=@LIBTOOL@ --silent

# This is used to generate boilerplate for defining Scheme functions in C.
SUFFIXES = .x
snarf_cpp_opts       = $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) \
	               $(libgeda_la_CPPFLAGS) $(libgeda_la_CFLAGS)
.c.x:
	@CPP="$(CPP)" $(GUILE_SNARF) -o $@ $< $(snarf_cpp_opts)

MOSTLYCLEANFILES     = *.log core FILE *~ scheme/*~ file/*~ geda/*~ guile/*~ iface/*~ math/*~ object/*~ struct/*~ utility/*~
CLEANFILES           = *.log core FILE *~ scheme/*~ file/*~ geda/*~ guile/*~ iface/*~ math/*~ object/*~ struct/*~ utility/*~ $(BUILT_SOURCES)
DISTCLEANFILES       = *.log core FILE *~ scheme/*~ file/*~ geda/*~ guile/*~ iface/*~ math/*~ object/*~ struct/*~ utility/*~
MAINTAINERCLEANFILES = *.log core FILE *~ scheme/*~ file/*~ geda/*~ guile/*~ iface/*~ math/*~ object/*~ struct/*~ utility/*~ Makefile.in

# Unfortunately, in order to test libtool thoroughly, we need access
# to its private directory.
objdir = `sed -n -e 's/^objdir=\"\(.*\)\"$$/\1/p' ./libtool`

