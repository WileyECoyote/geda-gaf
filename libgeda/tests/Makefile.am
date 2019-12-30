TEST_FILES = data/ATMega32-DIP_test.sym \
             data/read_only.sch

EXTRA_DIST = README test_parsecmd.h test-suite.h $(TEST_FILES)

check_file = \
	test_file

check_geda = \
	test_object \
	test_arc \
	test_box \
	test_bus \
	test_circle \
	test_complex \
	test_keyfile \
	test_line \
	test_list \
	test_net \
	test_path  \
	test_picture \
	test_pin \
	test_text \
	test_toplevel

check_math = \
	test_math \
	test_arc_math

check_object = \
	test_arc_object \
	test_attrib_object \
	test_box_object \
	test_bus_object \
	test_circle_object \
	test_color_object \
	test_complex_object \
	test_line_object \
	test_object_list \
	test_net_object \
	test_picture_object

check_utility = \
	test_utility

check_PROGRAMS = \
	$(check_file) \
	$(check_geda) \
	$(check_math) \
	$(check_object) \
	$(check_utility)

TESTS = $(check_PROGRAMS)

tests: $(check_PROGRAMS)

AM_CPPFLAGS = -DLOCALEDIR=\"$(localedir)\"  \
	-I$(srcdir)/../include -I$(srcdir)/../include/libgeda \
	-I$(top_srcdir) -I$(top_srcdir)/include \
	-I$(top_srcdir)/libgedacolor/include

AM_CFLAGS = -DWOW -D _GNU_SOURCE \
	$(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) \
	$(GDK_PIXBUF_CFLAGS)

AM_LDFLAGS = \
	$(MINGW_LDFLAGS) $(GUILE_LIBS) \
	$(GLIB_LIBS) $(GDK_PIXBUF_LIBS) \
	$(top_builddir)/libgedacairo/src/libgedacairo.la \
	$(top_builddir)/libgeda/src/libgeda.la @LIBINTL@

