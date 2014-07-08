## Process this file with automake to produce Makefile.in

libgedathonincludedir      = $(includedir)/libgedathon

libgedathoninclude_HEADERS =    geda_capsule.h    \
				geda_module.h     \
				geda_py_colors.h  \
				geda_py_docs.h    \
				geda_py_object.h  \
				geda_py_page.h    \
				geda_py_struct.h  \
				geda_python.h     \
				libgedathon.h

noinst_HEADERS = functions.h

EXTRA_DIST = $(noinst_HEADERS)

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
