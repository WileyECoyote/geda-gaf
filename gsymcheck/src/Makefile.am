## Process this file with automake to produce Makefile.in

INC_GEDA = -I$(top_srcdir) -I$(abs_top_builddir)/include -I$(top_srcdir)/intl
INC_LOCAL =  -I$(srcdir)/../include
INC_LIBGEDA = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

bin_PROGRAMS = gsymcheck 

# don't forget all *.h files */
gsymcheck_SOURCES = \
	g_rc.c \
	g_register.c \
	globals.c \
	gsymcheck.c \
	i_vars.c \
	parsecmd.c \
	s_check.c \
	s_log.c \
	s_symstruct.c

gsymcheck_CPPFLAGS = $(GEDA_INCLUDES)

gsymcheck_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLIGS) $(GLIB_CFLAGS) \
	           $(GUILE_CFLAGS) $(GDK_PIXBUF_CFLAGS)
gsymcheck_LDFLAGS = $(LIBSTROKE_LDFLAGS) $(GLIB_LIBS) $(GTK_LIBS) \
		 $(GTHREAD_LIBS) $(GUILE_LIBS) $(MINGW_GUI_LDFLAGS)
gsymcheck_LDADD = $(top_builddir)/libgeda/src/libgeda.la @LIBINTL@

MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in

