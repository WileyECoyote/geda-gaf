## Process this file with automake to produce Makefile.in
AUTOMAKE_OPTIONS = 1.7 subdir-objects

LIB_GTKSHEET = gtksheet

SUBDIRS = $(LIB_GTKSHEET)

INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

bin_PROGRAMS = gattrib

gattrib_SOURCES = \
		globals.c \
		listsort.c \
		parsecmd.c \
		f_export.c \
		g_util.c \
		gattrib_dialog.c \
		g_register.c \
		g_rc.c \
		i_vars.c \
		i_basic.c \
		x_toolbars.c \
		x_window.c \
		x_find.c \
		x_gtksheet.c \
		x_dialog.c \
		x_fileselect.c \
		x_menus.c \
		s_rename.c \
		s_misc.c \
		s_string_list.c \
		s_sheet_data.c \
		s_table.c \
		s_object.c \
		s_attrib.c \
		s_toplevel.c \
		s_properties.c \
		gattrib.c

AM_CFLAGS        = $(GEDA_INCLUDES) -I$(includedir)

gattrib_CPPFLAGS =

gattrib_CFLAGS 	 = $(GCC_CFLAGS)     $(GLIB_CFLAGS) $(GTK_CFLAGS) \
		   $(GTHREAD_CFLAGS) $(GUILE_CFLAGS) \
		   $(MINGW_CFLAGS)   $(AM_CFLAGS)

gattrib_LDFLAGS  = $(GLIB_LIBS)  $(GTK_LIBS) $(GTHREAD_LIBS) \
		   $(GUILE_LIBS) $(MINGW_GUI_LDFLAGS)

gattrib_LDADD	 = $(LIB_GTKSHEET)/libgtksheet.a \
		   $(top_builddir)/libgeda/src/libgeda.la

MOSTLYCLEANFILES     = *.log *.ps core FILE *~ #*#
CLEANFILES           = *.log *.ps core FILE *~ #*#
DISTCLEANFILES       = *.log core FILE *~ #*#
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in #*#

