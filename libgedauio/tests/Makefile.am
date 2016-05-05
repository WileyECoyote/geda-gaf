test_file_chooser_SOURCES = 	\
	prop-editor.c		\
	test_file_chooser.c

check_dialogs = \
	test_file_chooser

check_functions = \
	test_accumulate

check_widgets = \
	test_accel_label \
	test_bulb \
	test_combobox \
	test_combobox_text \
	test_completion \
	test_entry \
	test_font_button \
	test_font_dialog \
	test_handlebox \
	test_image_menu_item

check_PROGRAMS = \
	$(check_functions) \
	$(check_widgets)

TESTS = $(check_PROGRAMS)

tests: $(check_PROGRAMS)

noinst_PROGRAMS = \
		test_file_chooser

EXTRA_DIST = \
	prop-editor.h \
	file-chooser-test-dir/empty \
	file-chooser-test-dir/text.txt

AM_CPPFLAGS = -DLOCALEDIR=\"$(localedir)\"  $(DATADIR_DEFS) \
	-I$(srcdir)/../include -I$(srcdir)/../include/libgedauio \
	-I$(srcdir)/../include -I$(srcdir)/../include/libgeda \
	-I$(top_srcdir) -I$(top_srcdir)/include

AM_CFLAGS = -DWOW \
	$(GCC_CFLAGS) $(GTK_CFLAGS) $(MINGW_CFLAGS) $(GLIB_CFLAGS) $(GDK_PIXBUF_CFLAGS)

AM_LDFLAGS = \
	$(WINDOWS_LIBTOOL_FLAGS) $(MINGW_LDFLAGS) \
	$(GLIB_LIBS) $(GTK_LIBS) $(GDK_PIXBUF_LIBS) \
	$(top_builddir)/libgedauio/src/libgedauio.la \
	$(top_builddir)/libgeda/src/libgeda.la \
	@LIBINTL@

