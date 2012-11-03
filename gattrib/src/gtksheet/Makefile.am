## Process this file with automake to produce Makefile.in
## This Makefile.am created for gattrib by WEH 10.26.2012

INC_LOCAL =  -I$(srcdir)/../../include

noinst_LIBRARIES = libgtksheet.a

libgtksheet_a_SOURCES = gtkitementry.c	\
		        gtksheet.c \
			gtksheet-marshal.c \
			gtksheettypebuiltins.c \
			gtksheetwidget.c


libgtksheet_a_CPPFLAGS = $(INC_LOCAL) -gtoggle

libgtksheet_a_CFLAGS = $(GLIB_CFLAGS) $(GTK_CFLAGS)

# $(GCC_CFLAGS) $(AM_CFLAGS) $(GTHREAD_CFLAGS) $(MINGW_CFLAGS)

EXTRA_LIBRARIES = libgtksheet.a

MOSTLYCLEANFILES = *.log *.ps core FILE *~ #*# 
CLEANFILES = *.a *.log *.ps core FILE *~ #*#
DISTCLEANFILES = *.log core FILE *~ #*#
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in #*#
