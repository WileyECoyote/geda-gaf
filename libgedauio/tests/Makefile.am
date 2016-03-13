EXTRA_DIST = README

check_dialogs = \
	test_file_chooser

check_functions = \
	test_accumulate

check_widgets = \
	test_accel_label

check_PROGRAMS = \
	$(check_dialogs) \
	$(check_functions) \
	$(check_widgets)

TESTS = $(check_PROGRAMS)

tests: $(check_PROGRAMS)

AM_CPPFLAGS = -DLOCALEDIR=\"$(localedir)\"  $(DATADIR_DEFS) \
	-I$(srcdir)/../include -I$(srcdir)/../include/libgedauio \
	-I$(srcdir)/../include -I$(srcdir)/../include/libgeda \
	-I$(top_srcdir) -I$(top_srcdir)/include

AM_CFLAGS = -DWOW \
	$(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) \
	$(GDK_PIXBUF_CFLAGS)

AM_LDFLAGS = \
	$(WINDOWS_LIBTOOL_FLAGS) $(MINGW_LDFLAGS) $(GUILE_LIBS) \
	$(GLIB_LIBS) $(GDK_PIXBUF_LIBS) \
	$(top_builddir)/libgedauio/src/libgedauio.la \
	$(top_builddir)/libgeda/src/libgeda.la \
	@LIBINTL@

