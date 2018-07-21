## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
##
## Process this file with automake to produce Makefile.in

INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include
INC_LIBCOLOR = -I$(top_srcdir)/libgedacolor/include

GEDA_INCLUDES    = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA) $(INC_LIBCOLOR)

bin_PROGRAMS     = gmk_sym smash_megafile convert_sym sarlacc_schem olib \
				   grenum

AM_CFLAGS        = -DLOCALEDIR=\"$(localedir)\" \
				   $(GEDA_INCLUDES) $(GCC_CFLAGS) $(MINGW_CFLAGS) \
				   $(GLIB_CFLAGS)   $(GUILE_CFLAGS)

LDADD                    = @LIBINTL@

gmk_sym_SOURCES          = gmk_sym.c char_width.c
grenum_SOURCES           = grenum.c grenum.h
smash_megafile_SOURCES   = smash_megafile.c

olib_CFLAGS              = -std=gnu99 -DLOCALEDIR=\"$(localedir)\" $(GEDA_INCLUDES)
olib_SOURCES             = olib.l geda_sym_format.h
olib_LDFLAGS             = -lm

convert_sym_SOURCES      = convert_sym.c char_width.c
convert_sym_LDFLAGS      = -lm

sarlacc_schem_SOURCES    = sarlacc_schem.c
sarlacc_schem_CPPFLAGS   =
sarlacc_schem_LDFLAGS    = $(GLIB_LIBS) $(GUILE_LIBS)
sarlacc_schem_LDADD      = $(top_builddir)/libgeda/src/libgeda.la $(LDADD)

doxygen:
# Change to the docs directory and 'make doxygen' for geda utilities
if ENABLE_DOXYGEN
	(cd ../docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch. To generate'
	@echo 'Doxygen documentation for geda utilities, run configure with the'
	@echo '--enable-doxygen switch in the top of the source tree and try again'
endif

MOSTLYCLEANFILES      = *.log core FILE *~
CLEANFILES            = *.log core FILE *~
DISTCLEANFILES        = *.log core FILE *~
MAINTAINERCLEANFILES  = *.log core FILE *~ Makefile.in
