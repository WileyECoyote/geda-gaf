## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## This file is processed by automake to produce Makefile.in, after which
## The configure script generates the actual Makefile from the Makefile.in.

#AUTOMAKE_OPTIONS = 1.6 subdir-objects

# This is used for debugging the local build not the source
#DEBUGGING = --debug --verbose

INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL = -I$(srcdir)/../include
INC_LIBUIO = -I$(top_srcdir)/libgedauio/include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include
INC_LIBCAIRO = -I$(top_srcdir)/libgedacairo/include
INC_LIBCOLOR = -I$(top_srcdir)/libgedacolor/include
INC_LIBDRAW  = -I$(top_srcdir)/libgedadraw/include

GEDA_INCLUDES = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBUIO) $(INC_LIBGEDA) \
		        $(INC_LIBCAIRO) $(INC_LIBCOLOR) $(INC_LIBDRAW)

bin_PROGRAMS  = gschem

gschem_CXXFLAGS=$(AM_CXXFLAGS)

BUILT_SOURCES  = \
		guile/g_attrib.x \
		guile/g_hook.x   \
		guile/g_keys.x   \
		guile/g_util.x   \
		guile/g_window.x \
		guile/g_select.x

gschem_SOURCES = globals.c

# Include partial makefiles from subdirectories
include base/Makefile_insert
include guile/Makefile_insert
include iface/Makefile_insert
include math/Makefile_insert
include object/Makefile_insert
include x/Makefile_insert

# Extra files not mentioned as sources to include in the source tarball
MAKE_INPUTS	 = base/makefile.in guile/makefile.in iface/makefile.in \
		   math/makefile.in object/makefile.in x/makefile.in

EXTRA_DIST       = $(BUILT_SOURCES)

AM_CFLAGS        = $(DEBUGGING) $(GEDA_INCLUDES)

gschem_CFLAGS    = $(AM_CFLAGS)      $(GCC_CFLAGS)   $(LIBSTROKE_CFLAGS) \
				   $(GLIB_CFLAGS)    $(GTK_CFLAGS)   $(GUILE_CFLAGS) \
				   $(GTHREAD_CFLAGS) $(MINGW_CFLAGS) $(gschem_CXXFLAGS)

gschem_CPPFLAGS  = $(GEDA_INCLUDES) $(AM_CXXFLAGS) $(GLIB_CFLAGS) $(GTK_CFLAGS)

gschem_LDFLAGS   = $(GLIB_LIBS)     $(GTK_LIBS)      $(LIBSTROKE_LDFLAGS) \
				   $(GTHREAD_LIBS)  $(GUILE_LIBS)    $(MINGW_GUI_LDFLAGS) \
				   $(X11_LIBS)      $(DEBUGGING)     $(STACK_PROTECTOR)

gschem_LDADD     = $(top_builddir)/libgedadraw/src/libgedadraw.la \
				   $(top_builddir)/libgedauio/src/libgedauio.la \
				   $(top_builddir)/libgedacairo/src/libgedacairo.la \
				   $(top_builddir)/libgedacolor/src/libgedacolor.la \
				   $(top_builddir)/libgeda/src/libgeda.la \
				   @LIBINTL@

LIBTOOL          = @LIBTOOL@ --silent

# This is used to generate boilerplate for defining Scheme functions in C.
SUFFIXES = .x

snarf_cpp_opts   = $(DEFS) $(INCLUDES) $(AM_CPPFLAGS) $(gschem_CFLAGS) \
		   $(gschem_CPPFLAGS)  $(gschem_CFLAGS) $(CPPFLAGS)

.c.x:
	     @CPP="$(CPP)" $(GUILE_SNARF) -o $@ $< $(snarf_cpp_opts)

DEFS             = -DLOCALEDIR=\"$(localedir)\"  @DEFS@

doxygen:
# Change to the docs directory and 'make doxygen' for gschem
if ENABLE_DOXYGEN
	(cd ../docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch. To generate'
	@echo 'Doxygen documentation for gschem, run configure with --enable-doxygen'
	@echo 'in the top of the source tree and then try again'
endif

MOSTLYCLEANFILES     = *.log core FILE *~ ./*/*~
CLEANFILES           = *.log core FILE *~ ./*/*~ $(BUILT_SOURCES)
DISTCLEANFILES       = *.log core FILE *~ ./*/*~
MAINTAINERCLEANFILES = *.log core FILE *~ ./*/*~ Makefile.in
