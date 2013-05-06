## Process this file with automake to produce Makefile.in
AUTOMAKE_OPTIONS = 1.7 subdir-objects

SUBDIRS = po

INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include
INC_LIBCAIRO = -I$(top_srcdir)/libgedacairo/include

GEDA_INCLUDES = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA) $(INC_LIBCAIRO)

bin_PROGRAMS = gaf

BUILT_SOURCES = shell.x

EXTRA_DIST = $(BUILT_SOURCES) $(html_man_files) gaf.1.in

gaf_SOURCES = \
	builtins.h \
	gettext.h \
	gaf.c \
	config.c \
	export.c \
	shell.c

AM_CFLAGS     = $(GEDA_INCLUDES) -I$(includedir)

gaf_CPPFLAGS  =

gaf_CFLAGS    = $(GCC_CFLAGS) $(MINGW_CFLAGS) $(GUILE_CFLAGS) $(GTK_CFLAGS) \
	        $(GDK_PIXBUF_CFLAGS) $(CAIRO_CFLAGS) $(CAIRO_PNG_CFLAGS) \
		$(CAIRO_PDF_CLAGS) $(CAIRO_PS_CFLAGS) $(CAIRO_SVG_CFLAGS) \
		$(AM_CFLAGS)

gaf_LDFLAGS   = $(GUILE_LIBS) $(GTK_LIBS) $(GDK_PIXBUF_LIBS) $(CAIRO_LIBS) \
	        $(CAIRO_PNG_LIBS) $(CAIRO_PDF_CLAGS) $(CAIRO_PS_LIBS) $(CAIRO_SVG_LIBS)
gaf_LDADD     = $(top_builddir)/libgedacairo/src/libgedacairo.la \
	        $(top_builddir)/libgeda/src/libgeda.la \
	          @LIBINTL@

localedir     = @datadir@/locale
DEFS          = -DLOCALEDIR=\"$(localedir)\" @DEFS@

# This is used to generate boilerplate for defining Scheme functions
# in C.
snarf_cpp_opts = $(DEFS) $(DEFAULT_INCLUDES) $(INCLUDES) $(AM_CPPFLAGS) \
		 $(gaf_CPPFLAGS) $(AM_CFLAGS) $(gaf_CFLAGS)
.c.x:
	CPP="$(CPP)" $(GUILE_SNARF) -o $@ $< $(snarf_cpp_opts)

.1.in.1:
	d=`LANG=C date -d $(DATE_VERSION) "+%B %-d, %Y"` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" < $< > $@

# Rules for converting man->html
dist_man_MANS = gaf.1
html_man_files = gaf.html

docsmandir=$(docdir)/man
docsman_DATA=$(html_man_files)
if ENABLE_GROFF_HTML
.1.html:
	rm -rf $@.tmp && \
	LC_NUMERIC=C $(GROFF) -man -T html  $< >$@.tmp && \
	mv -f $@.tmp $@
else
.1.html:
	@echo "No working groff available: cannot create $@"
	@! :
endif

SUFFIXES = .x .1.in .1 .html

CLEANFILES = $(BUILT_SOURCES)
