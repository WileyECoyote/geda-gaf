AUTOMAKE_OPTIONS = nostdinc

INC_GEDA = -I$(top_srcdir) -I$(abs_top_builddir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

bin_PROGRAMS = gschem

gschem_CXXFLAGS=$(AM_CXXFLAGS)

gschem_SOURCES = \
	a_pan.c \
	a_zoom.c \
	g_attrib.c \
	g_funcs.c \
	g_hook.c \
	g_keys.c \
	g_rc.c \
	g_register.c \
	g_select.c \
	g_util.c \
	g_window.c \
	globals.c \
	gschem.c \
	gschem_accel_label.c \
	gschem_action.c \
	gschem_cairo.c \
	gschem_dialog.c \
	gschem_pango.c \
	gschem_toplevel.c \
	i_basic.c \
	i_callbacks.c \
	i_vars.c \
	m_basic.c \
	o_arc.c \
	o_attrib.c \
	o_basic.c \
	o_box.c \
	o_buffer.c \
	o_bus.c \
	o_circle.c \
	o_complex.c \
	o_copy.c \
	o_cue.c \
	o_delete.c \
	o_find.c \
	o_grips.c \
	o_line.c \
	o_misc.c \
	o_move.c \
	o_net.c \
	o_path.c \
	o_picture.c \
	o_pin.c \
	o_place.c \
	o_select.c \
	o_slot.c \
	o_text.c \
	o_undo.c \
	parsecmd.c \
	s_stretch.c \
	x_attribedit.c \
	x_autonumber.c \
	x_basic.c \
	x_clipboard.c \
	x_color.c \
	x_compselect.c \
	x_dialog.c \
	x_event.c \
	x_fileselect.c \
	x_grid.c \
	x_image.c \
	x_log.c \
	x_menus.c \
	x_misc.c \
	x_multiattrib.c \
	x_pagesel.c \
	x_preview.c \
	x_print.c \
	x_rc.c \
	x_script.c \
        x_settings.c \
        x_settings_dialog.c \
	x_stroke.c \
	x_window.c

gschem_CPPFLAGS = $(GEDA_INCLUDES)
gschem_CFLAGS = $(GCC_CFLAGS) $(LIBSTROKE_CFLAGS) $(AM_CFLAGS)\
		$(GLIB_CFLAGS) $(GTK_CFLAGS) $(GUILE_CFLAGS) \
		$(GTHREAD_CFLAGS) $(MINGW_CFLAGS) $(gschem_CXXFLAGS)
gschem_LDFLAGS = $(LIBSTROKE_LDFLAGS) $(GLIB_LIBS) $(GTK_LIBS) \
		 $(GTHREAD_LIBS) $(GUILE_LIBS) $(MINGW_GUI_LDFLAGS)
gschem_LDADD = $(top_builddir)/libgeda/src/libgeda.la @LIBINTL@

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

