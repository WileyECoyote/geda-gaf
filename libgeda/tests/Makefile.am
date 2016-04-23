EXTRA_DIST = README test-suite.h

check_file = \
	test_file

check_geda = \
	test_arc \
	test_box \
	test_bus \
	test_circle \
	test_complex \
	test_line \
	test_list \
	test_net \
	test_path  \
	test_picture \
	test_pin \
	test_text

check_object = \
	test_arc_object \
	test_box_object \
	test_attrib_object

check_utility = \
	test_utility

check_PROGRAMS = \
	$(check_file) \
	$(check_geda) \
	$(check_object) \
	$(check_utility)

TESTS = $(check_PROGRAMS)

tests: $(check_PROGRAMS)

AM_CPPFLAGS = -DLOCALEDIR=\"$(localedir)\"  $(DATADIR_DEFS) \
	-I$(srcdir)/../include -I$(srcdir)/../include/libgeda \
	-I$(top_srcdir) -I$(top_srcdir)/include \
	-I$(top_srcdir)/libgedacolor/include

AM_CFLAGS = -DWOW \
	$(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) \
	$(GDK_PIXBUF_CFLAGS)

AM_LDFLAGS = \
	$(WINDOWS_LIBTOOL_FLAGS) $(MINGW_LDFLAGS) $(GUILE_LIBS) \
	$(GLIB_LIBS) $(GDK_PIXBUF_LIBS) \
	$(top_builddir)/libgedacairo/src/libgedacairo.la \
	$(top_builddir)/libgeda/src/libgeda.la \
	@LIBINTL@
