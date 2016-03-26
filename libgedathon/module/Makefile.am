## Process this file with automake to produce Makefile.in
#
# Remarks: WEH: the -module flag means the file name does not begin with 'lib'
#REF: (geda-python.m4).PYTHON_CFLAGS  = -Wstrict-prototypes -pthread -fno-strict-aliasing -fwrapv -fPIC
#REF: (geda-python.m4).PYTHON_LDFLAGS = -shared -Bsymbolic-functions
#REF: (geda-python.m4).PYTHON_LIBS    = -lpython$PYTHON_VERSION
#DEBUGGING           = --debug --verbose

LIB_OBJECTS          = objects
SUBDIRS              = $(LIB_OBJECTS)

INC_GEDA             = -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL            = -I$(srcdir)/../include
INC_LIBGEDA          = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES        = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

SUFFIXES             = .py

moduledir            = $(PYTHON_MODDIR)/geda

module_PYTHON        = __init__.py

PY_LIBFLAGS          = $(PYTHON_LDFLAGS) -module -avoid-version -export-symbols-regex initgeda

AM_CFLAGS            = $(DEBUGGING) $(GEDA_INCLUDES) $(GDK_PIXBUF_CFLAGS)

AM_LDFLAGS           = $(DEBUGGING) -version-info $(LIBGEDATHON_SHLIB_VERSION)

pyexecsave           = $(pyexecdir)

pyexecdir            = $(moduledir)

pyexec_LTLIBRARIES   = geda.la

geda_la_SOURCES      = geda.c constants.c functions.c

geda_la_CFLAGS       = $(GCC_CFLAGS) $(AM_CFLAGS) $(GLIB_CFLAGS)

geda_la_CPPFLAGS     = $(PYTHON_CFLAGS) $(PYTHON_IFLAGS)

geda_la_LDFLAGS      = $(AM_LDFLAGS) $(PY_LIBFLAGS) $(GLIB_LIBS) $(LDFLAGS) $(PYTHON_LIBS)

geda_la_LIBADD       = $(LIB_OBJECTS)/libobjects.la

LIBTOOL              = @LIBTOOL@ --silent

install-exec-hook:
		    @rm -f $(moduledir)/$(pyexec_LTLIBRARIES)

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
