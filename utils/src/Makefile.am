## Process this file with automake to produce Makefile.in
AUTOMAKE_OPTIONS = $(GEDA_AUTO_MAKE_OPTIONS)

INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES 	= $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

bin_PROGRAMS	= gmk_sym smash_megafile convert_sym sarlacc_schem olib \
	          gsch2pcb grenum

AM_CFLAGS		= $(GEDA_INCLUDES) $(GCC_CFLAGS) $(MINGW_CFLAGS) \
			  $(GLIB_CFLAGS)   $(GUILE_CFLAGS)

gmk_sym_SOURCES 	= gmk_sym.c char_width.c
smash_megafile_SOURCES 	= smash_megafile.c
grenum_SOURCES		= grenum.c grenum.h

olib_SOURCES		= olib.l geda_sym_format.h
olib_LDFLAGS		= -lm

convert_sym_SOURCES	= convert_sym.c char_width.c
convert_sym_LDFLAGS	= -lm

gsch2pcb_SOURCES	= gsch2pcb.c
gsch2pcb_CPPFLAGS 	= -DPCBDATADIR=\"$(PCBDATADIR)\" \
			  -DPCBLIBPATH=\"$(PCBLIBPATH)\"
gsch2pcb_CFLAGS 	= $(AM_CFLAGS) $(GDK_PIXBUF_CFLAGS)
gsch2pcb_LDFLAGS 	= $(GLIB_LIBS)
gsch2pcb_LDADD   	= $(top_builddir)/libgeda/src/libgeda.la

sarlacc_schem_SOURCES 	= sarlacc_schem.c
sarlacc_schem_CPPFLAGS  = $(GDK_PIXBUF_CFLAGS)
sarlacc_schem_LDFLAGS 	= $(GLIB_LIBS) $(GUILE_LIBS) $(GDK_PIXBUF_LIBS)
sarlacc_schem_LDADD   	= $(top_builddir)/libgeda/src/libgeda.la

MOSTLYCLEANFILES	= *.log core FILE *~
CLEANFILES		= *.log core FILE *~ $(BUILT_SOURCES)
DISTCLEANFILES		= *.log core FILE *~
MAINTAINERCLEANFILES	= *.log core FILE *~ Makefile.in
