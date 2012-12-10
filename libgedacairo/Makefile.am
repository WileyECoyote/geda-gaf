INC_GEDA = -I$(top_srcdir) -I$(abs_top_builddir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

lib_LTLIBRARIES = libgedacairo.la

libgedacairo_la_SOURCES = \
	edacairo.c \
	edapangorenderer.c \
	edarenderer.c
AUTOMAKE_OPTIONS = nostdinc
libgedacairoincludedir = $(includedir)/libgedacairo

libgedacairoinclude_HEADERS = \
	libgedacairo.h \
	edacairo.h \
	edarenderer.h

noinst_HEADERS = \
	edapangorenderer.h

EXTRA_DIST = README ChangeLog

AM_CPPFLAGS = $(GEDA_INCLUDES)

AM_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) \
	    $(GDK_CFLAGS) $(CAIRO_CFLAGS) $(PANGO_CFLAGS) $(GDK_PIXBUF_CFLAGS)

libgedacairo_la_CFLAGS = $(GCC_CFLAGS) $(AM_CFLAGS) $(GLIB_CFLAGS) $(GTK_CFLAGS)
 
AM_LDFLAGS = -version-info $(LIBGEDACAIRO_SHLIB_VERSION) \
	$(WINDOWS_LIBTOOL_FLAGS) $(MINGW_LDFLAGS) $(GUILE_LIBS) \
	$(GDK_LIBS) $(GLIB_LIBS) $(CAIRO_LIBS) $(PANGO_LIBS) $(GDK_PIXBUF_LIBS)
	
LIBTOOL=@LIBTOOL@ --silent

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libgedacairo.pc

if HAVE_GIT_REPO
ChangeLog: $(top_builddir)/stamp-git
	(cd $(srcdir) && $(GIT) log --pretty=medium $(CHANGELOG_BASE).. -- .) > $@
endif HAVE_GIT_REPO
