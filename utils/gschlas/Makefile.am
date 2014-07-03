## Process this file with automake to produce Makefile.in
AUTOMAKE_OPTIONS = $(GEDA_AUTO_MAKE_OPTIONS)

INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES  = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

bin_PROGRAMS = gschlas

gschlas_SOURCES = \
		g_rc.c \
		g_register.c \
		globals.c \
		gschlas.c \
		i_vars.c \
		parsecmd.c \
		s_util.c

AM_CFLAGS            = $(GEDA_INCLUDES) -I$(includedir)
gschlas_CPPFLAGS     =
gschlas_CFLAGS	     = $(AM_CFLAGS)    $(GCC_CFLAGS)        $(GLIB_CFLAGS) \
	               $(GUILE_CFLAGS) $(GDK_PIXBUF_CFLAGS) $(MINGW_CFLAGS)
gschlas_LDFLAGS	     = $(GLIB_LIBS) $(GUILE_LIBS) $(GDK_PIXBUF_LIBS)
gschlas_LDADD 	     = $(top_builddir)/libgeda/src/libgeda.la

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in

