## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## Process this file with automake to produce Makefile.in
#AUTOMAKE_OPTIONS = 1.6 subdir-objects

LIB_GTKSHEET = gtksheet

SUBDIRS = $(LIB_GTKSHEET)

INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL = -I$(srcdir)/../include
INC_LIBUIO = -I$(top_srcdir)/libgedauio/include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include
INC_LIBCOLOR = -I$(top_srcdir)/libgedacolor/include

GEDA_INCLUDES = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBUIO) $(INC_LIBGEDA) $(INC_LIBCOLOR)

bin_PROGRAMS   = gattrib

BUILT_SOURCES   = $(top_builddir)/version.h

gattrib_SOURCES = $(BUILT_SOURCES) \
		globals.c \
		listsort.c \
		parsecmd.c \
		f_export.c \
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

gattrib_CFLAGS   = $(AM_CFLAGS) $(GCC_CFLAGS) $(GLIB_CFLAGS) $(GTK_CFLAGS) \
				   $(GTHREAD_CFLAGS) $(GUILE_CFLAGS) $(MINGW_CFLAGS)

gattrib_LDFLAGS  = $(GLIB_LIBS)  $(GTK_LIBS) $(GTHREAD_LIBS) \
		   $(GUILE_LIBS) $(MINGW_GUI_LDFLAGS) "-Wl,--copy-dt-needed-entries"

gattrib_LDADD	 = $(LIB_GTKSHEET)/libgtksheet.a \
				   $(top_builddir)/libgedauio/src/libgedauio.la \
				   $(top_builddir)/libgeda/src/libgeda.la \
				   @LIBINTL@

DEFS             = -DLOCALEDIR=\"$(localedir)\"  @DEFS@

version.h:
	@if test ! -f $(top_srcdir)/version.h; then \
	cd $(top_srcdir) \
	$(MAKE) version.h || exit 1; \
	fi

doxygen:
# Change to the docs directory and 'make doxygen' for gattrib
if ENABLE_DOXYGEN
	(cd ../docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch. To generate'
	@echo 'Doxygen documentation for gattrib, run configure with --enable-doxygen'
	@echo 'in the top of the source tree and then try again'
endif

MOSTLYCLEANFILES     = *.log *.ps core FILE *~
CLEANFILES           = *.log *.ps core FILE *~
DISTCLEANFILES       = *.log *.ps core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in

