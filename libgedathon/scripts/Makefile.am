## $Id$
##
## Process this file with automake to produce Makefile.in
SUFFIXES = .py

PYdir	             = $(TARGET_DIR)

## ftest.py gtest.py
PY_SCRIPTS 	    = capacitor.py dual-opamp.py lpbf.py resistor.py \
		      gsym-attribute.py gsym-set-font-size.py \
		      gsym-set-visibility.py geda-snap-grid.py

EXTRA_DIST	    = $(PY_SCRIPTS)

#enble this for distcheck
distcheck:
dist_bin_SCRIPTS     = $(PY_SCRIPTS)

MOSTLYCLEANFILES     = *.log core FILE *~
CLEANFILES           = *.log core FILE *~
DISTCLEANFILES       = *.log core FILE *~
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in
