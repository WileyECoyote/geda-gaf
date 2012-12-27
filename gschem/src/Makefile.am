
AUTOMAKE_OPTIONS = nostdinc 1.7 subdir-objects

INC_GEDA = -I$(top_srcdir) -I$(abs_top_builddir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include
INC_LIBCAIRO = -I$(top_srcdir)/libgedacairo/include

GEDA_INCLUDES = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA) $(INC_LIBCAIRO)

bin_PROGRAMS = gschem

gschem_CXXFLAGS=$(AM_CXXFLAGS)

BUILT_SOURCES = \
	g_attrib.x \
	g_hook.x \
	g_keys.x \
	g_util.x \
	g_window.x \
	g_select.x

gschem_SOURCES =

include Makefile_insert

# Include partial makefiles from subdirectories (for now, we only got 1)
include widgets/Makefile_insert

# Extra files not mentioned as sources to include in the source tarball
EXTRA_DIST =	\
	widgets/makefile.in

gschem_CPPFLAGS = $(GEDA_INCLUDES)
gschem_CFLAGS = $(GCC_CFLAGS) $(LIBSTROKE_CFLAGS) $(AM_CFLAGS)\
		$(GLIB_CFLAGS) $(GTK_CFLAGS) $(GUILE_CFLAGS) \
		$(GTHREAD_CFLAGS) $(MINGW_CFLAGS) $(gschem_CXXFLAGS)

gschem_LDFLAGS = $(GLIB_LIBS) $(GTK_LIBS) $(LIBSTROKE_LDFLAGS) \
		 $(GTHREAD_LIBS) $(GUILE_LIBS) $(MINGW_GUI_LDFLAGS)

gschem_LDADD = $(top_builddir)/libgedacairo/src/libgedacairo.la \
	       $(top_builddir)/libgeda/src/libgeda.la \
	       @LIBINTL@

# This is used to generate boilerplate for defining Scheme functions in C.
SUFFIXES = .x
snarf_cpp_opts = $(DEFS) $(INCLUDES) $(AM_CPPFLAGS) $(AM_CFLAGS) \
		 $(gschem_CPPFLAGS) $(gschem_CFLAGS)

.c.x:
	CPP="$(CPP)" $(GUILE_SNARF) -o $@ $< $(snarf_cpp_opts)

localedir = @datadir@/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~ $(BUILT_SOURCES)
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in

