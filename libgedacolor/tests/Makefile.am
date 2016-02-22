
check_PROGRAMS = \
		test_color

TESTS = $(check_PROGRAMS)

tests: $(check_PROGRAMS)

check-am: update-dependents

AM_CPPFLAGS = -DLOCALEDIR=\"$(localedir)\"  $(DATADIR_DEFS) \
	-I$(srcdir)/../include -I$(srcdir)/../include/libgedacolor \
	-I$(top_srcdir) -I$(top_srcdir)/include

AM_CFLAGS = -DWOW \
	$(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GLIB_CFLAGS) \
	$(GDK_PIXBUF_CFLAGS)

AM_LDFLAGS = $(WINDOWS_LIBTOOL_FLAGS) $(MINGW_LDFLAGS) $(GUILE_LIBS) \
	$(GLIB_LIBS) $(GDK_PIXBUF_LIBS) $(MINGW_GUI_LDFLAGS) \
	@LIBINTL@

test_color_LDADD  = $(top_builddir)/libgedacolor/src/libgedacolor.la @LIBINTL@

DEPENDS=\
	libgeda/src \
	libgedathon/src

update-dependents:
	@for d in $(DEPENDS); do \
	  (cd $(top_builddir)/$$d && $(MAKE) $(AM_MAKEFLAGS) all); \
	done

.PHONY: update-dependents

MOSTLYCLEANFILES     = *.log *~
CLEANFILES           = *.log *~
DISTCLEANFILES       = *.log *~
MAINTAINERCLEANFILES = *.log *~ Makefile.in
