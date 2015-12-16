## Process this file with automake to produce Makefile.in
## This Makefile.am created for gattrib by WEH 10.26.2012
AUTOMAKE_OPTIONS = 1.6 subdir-objects

GTKSHEET_LOCAL   =  -I$(srcdir)/../../include/gtksheet

AM_LDFLAGS = -version-info $(LIBGTKSHEET_SO_VERSION)    \
	     -export-dynamic                            \
	     -rpath $(libdir)                           \
	     $(no_undefined)                            \
	     $(GTK_LIBS)

noinst_LIBRARIES = libgtksheet.a

libgtksheet_a_SOURCES = \
						gtkdataformat.c    \
						gtkdatatextview.c  \
						gtkitementry.c	   \
						gtksheet.c         \
						gtksheetcolumn.c   \
						gtksheet-marshal.c \
						gtksheettypebuiltins.c

gtksheet_extra_sources = gtksheet-marshal.list

#libgtksheet_a_CPPFLAGS = $(GTKSHEET_LOCAL) -gtoggle
libgtksheet_a_CPPFLAGS = $(GTKSHEET_LOCAL)

libgtksheet_a_CFLAGS   = $(GLIB_CFLAGS) $(GTK_CFLAGS)

# $(GCC_CFLAGS) $(AM_CFLAGS) $(GTHREAD_CFLAGS) $(MINGW_CFLAGS)

EXTRA_LIBRARIES = libgtksheet.a

MOSTLYCLEANFILES     = *.log *.ps core FILE *~
CLEANFILES           = *.log *.ps core FILE *~ *.a
DISTCLEANFILES       = *.log *.ps core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in

