
check_PROGRAMS = \
	test_arc \
	test_box \
	test_bus \
	test_circle \
	test_complex \
	test_line \
	test_net \
	test_path  \
	test_picture \
	test_pin

TESTS = $(check_PROGRAMS)

tests: $(check_PROGRAMS)

AM_CPPFLAGS = -DLOCALEDIR=\"$(localedir)\"  $(DATADIR_DEFS) \
	-I$(srcdir)/../include -I$(srcdir)/../include/libgeda \
	-I$(top_srcdir) -I$(top_srcdir)/include

AM_CFLAGS = -DWOW \
	$(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) \
	$(GDK_PIXBUF_CFLAGS)

AM_LDFLAGS = \
	$(WINDOWS_LIBTOOL_FLAGS) $(MINGW_LDFLAGS) $(GUILE_LIBS) \
	$(GLIB_LIBS) $(GDK_PIXBUF_LIBS) \
	$(top_builddir)/libgedacairo/src/libgedacairo.la \
	$(top_builddir)/libgeda/src/libgeda.la \
	@LIBINTL@
