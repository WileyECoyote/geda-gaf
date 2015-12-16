## $Id$ indent-tabs-mode: t; c-basic-offset: 4; tab-width: 4
## This file is processed by automake to produce Makefile.in, after which
## The configure script generates the actual Makefile from the Makefile.in.

#AUTOMAKE_OPTIONS = 1.6 subdir-objects

INC_GEDA = -I$(top_srcdir) -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

bin_PROGRAMS  = gsymcheck

gsymcheck_SOURCES = 	g_rc.c \
			g_register.c \
			globals.c \
			gsymcheck.c \
			i_vars.c \
			parsecmd.c \
			s_check.c \
			s_log.c \
			s_symstruct.c

AM_CFLAGS            = $(GEDA_INCLUDES) -I$(includedir)

gsymcheck_CPPFLAGS   =

gsymcheck_CFLAGS     = $(AM_CFLAGS)    $(GCC_CFLAGS)   $(GLIB_CFLAGS) \
	               $(GUILE_CFLAGS) $(MINGW_CFLAGS) $(GDK_PIXBUF_CFLAGS)
gsymcheck_LDFLAGS    = $(LIBSTROKE_LDFLAGS) $(GLIB_LIBS) $(GTK_LIBS) \
		       $(GTHREAD_LIBS) $(GUILE_LIBS) $(MINGW_GUI_LDFLAGS)
gsymcheck_LDADD      = $(top_builddir)/libgeda/src/libgeda.la @LIBINTL@

doxygen:
# Change to the docs directory and 'make doxygen' for gsymcheck
if ENABLE_DOXYGEN
	(cd ../docs; $(MAKE) doxygen) || exit 1;
else
	@echo 'configure was not run with the --enable-doxygen switch. To generate'
	@echo 'Doxygen documentation for gsymcheck, run configure with --enable-doxygen'
	@echo 'in the top of the source tree and then try again'
endif

MOSTLYCLEANFILES     = *.log *.ps core FILE *~
CLEANFILES           = *.log *.ps core FILE *~
DISTCLEANFILES       = *.log      core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in
