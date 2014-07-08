## Process this file with automake to produce Makefile.in
#
# Remarks: WEH: the -module flag means the file name does not begin with 'lib'
#REF: (geda-python.m4).PYTHON_CFLAGS  = -Wstrict-prototypes -pthread -fno-strict-aliasing -fwrapv -fPIC
#REF: (geda-python.m4).PYTHON_LDFLAGS = -shared -Bsymbolic-functions
#REF: (geda-python.m4).PYTHON_LIBS    = -lpython$PYTHON_VERSION
#DEBUGGING           = --debug --verbose

AUTOMAKE_OPTIONS         = $(GEDA_AUTO_MAKE_OPTIONS)

INC_GEDA                 = -I$(top_srcdir)/include -I$(top_srcdir)/intl
INC_LOCAL                = -I$(srcdir)/../../include
INC_LIBGEDA              = -I$(top_srcdir)/libgeda/include

GEDA_INCLUDES            = $(INC_GEDA) $(INC_LOCAL) $(INC_LIBGEDA)

PY_LIBFLAGS              = $(PYTHON_LDFLAGS) #-export-dynamic

AM_LDFLAGS               = $(no_undefined) $(PY_LIBFLAGS) $(LDFLAGS)

noinst_LTLIBRARIES       = libobjects.la

libobjects_la_SOURCES    = geda_py_page.c \
			   geda_py_object.c \
			   geda_py_arc.c \
			   geda_py_box.c \
			   geda_py_bus.c \
			   geda_py_circle.c \
			   geda_py_complex.c \
			   geda_py_line.c \
			   geda_py_net.c \
			   geda_py_path.c \
			   geda_py_picture.c \
			   geda_py_pin.c \
			   geda_py_text.c

libobjects_la_CFLAGS     = $(GCC_CFLAGS) $(AM_CFLAGS) $(GLIB_CFLAGS)

libobjects_la_CPPFLAGS   = $(GEDA_INCLUDES) $(PYTHON_CFLAGS) $(PYTHON_IFLAGS)

libobjects_la_LDFLAGS    = $(AM_LDFLAGS) $(GLIB_LIBS) $(PYTHON_LIBS)

#EXTRA_LIBRARIES         = libobjects.la
#LIBTOOL                 = @LIBTOOL@ --silent

MOSTLYCLEANFILES         = *.log *.ps core FILE *~
CLEANFILES               = *.a *.log *.ps core FILE *~
DISTCLEANFILES           = *.log core FILE *~
MAINTAINERCLEANFILES     = *.log *.ps core FILE *~ Makefile.in
